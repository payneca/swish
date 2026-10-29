;;; Copyright 2026 Chris Payne
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;; Distributed Erlang-style messaging.
;;
;; Remote processes are represented by rpid records from (swish internal).
;; (swish erlang) calls the hooks we install for any operation on an rpid,
;; so send, link, monitor, and kill work transparently on remote processes.
;; We intern rpids and remote monitors so that eq? and equal? hold for the
;; same remote identity.
;;
;; Each pair of nodes shares one connection. A frame is a 4-byte big-endian
;; length followed by a fasl-encoded token vector and a fasl-encoded body.
;; Processes and monitors in the body are fasl externals whose tokens
;; identify them by node, creation, and id.

#!chezscheme
(library (swish dist)
  (export
   dist:connect
   dist:disconnect
   dist:listen-port
   dist:node
   dist:nodes
   dist:spawn
   dist:spawn&link
   dist:start&link
   dist:stop
   )
  (import
   (chezscheme)
   (swish digest)
   (swish erlang)
   (swish gen-server)
   (swish internal)
   (swish io)
   (swish osi)
   (swish supervisor)
   (swish watcher)
   )

  ($import-internal
   make-rpid rpid? rpid-node rpid-creation rpid-id
   make-mon mon? mon-origin mon-target
   make-dist-hooks set-dist-hooks! get-dist-core
   dist-core-remote-link dist-core-remote-unlink dist-core-remote-monitor
   dist-core-remote-demonitor dist-core-remote-exit dist-core-remote-down
   dist-core-node-down dist-core-local-process?
   fault-condition? fault-condition-reason)

  (define $remote-link (dist-core-remote-link (get-dist-core)))
  (define $remote-unlink (dist-core-remote-unlink (get-dist-core)))
  (define $remote-monitor (dist-core-remote-monitor (get-dist-core)))
  (define $remote-demonitor (dist-core-remote-demonitor (get-dist-core)))
  (define $remote-exit (dist-core-remote-exit (get-dist-core)))
  (define $remote-down (dist-core-remote-down (get-dist-core)))
  (define $node-down (dist-core-node-down (get-dist-core)))
  (define local-process? (dist-core-local-process? (get-dist-core)))

  (define protocol-version 1)
  (define max-frame-size (* 64 1024 1024))
  (define handshake-timeout 10000)

  ;; Node state. We access these tables with interrupts disabled.
  (define my-node #f)
  (define my-creation #f)
  (define node-conns (make-eq-hashtable))           ; node -> connection
  (define exports (make-eqv-hashtable))             ; id -> process or weak
  (define rpids (make-hashtable equal-hash equal?)) ; #(node creation id) -> weak
  (define foreign-mons (make-hashtable equal-hash equal?)) ; key -> weak
  (define local-mons (make-eqv-hashtable))          ; id -> weak
  (define mon-keys (make-weak-eq-hashtable))        ; mon -> #(node creation id)
  (define next-mon-id 0)
  (define weak-inserts 0)

  (define (weak-ref ht key)
    (let ([wp (hashtable-ref ht key #f)])
      (and wp
           (let ([x (car wp)])
             (and (not (bwp-object? x)) x)))))

  (define (weak-set! ht key x)
    (hashtable-set! ht key (weak-cons x #f))
    (set! weak-inserts (fx+ weak-inserts 1))
    (when (fx= weak-inserts 4096)
      (set! weak-inserts 0)
      (for-each sweep! (list exports rpids foreign-mons local-mons))))

  (define (sweep! ht)
    (let-values ([(keys vals) (hashtable-entries ht)])
      (vector-for-each
       (lambda (key x)
         (when (and (weak-pair? x) (bwp-object? (car x)))
           (hashtable-delete! ht key)))
       keys vals)))

  (define (reset-tables!)
    (for-each hashtable-clear!
      (list node-conns exports rpids foreign-mons local-mons mon-keys)))

  (define (node-conn node)
    (with-interrupts-disabled (eq-hashtable-ref node-conns node #f)))

  (define (intern-rpid node creation id)
    (let ([key (vector node creation id)])
      (or (weak-ref rpids key)
          (let ([r (make-rpid node creation id)])
            (weak-set! rpids key r)
            r))))

  ;; We hold exported processes strongly while they are alive so that they
  ;; are not collected while remote nodes can reach them. Once they die, we
  ;; hold them weakly so that tokens still resolve to the same process while
  ;; local references remain.
  (define (lookup-export id)
    (let ([x (hashtable-ref exports id #f)])
      (if (weak-pair? x)
          (let ([p (car x)])
            (and (not (bwp-object? p)) p))
          x)))

  (define (my-key? node creation)
    (and (eq? node my-node) (eqv? creation my-creation)))

  ;; Tokens

  (define (export! p)
    (let ([id (process-id p)])
      (unless (hashtable-contains? exports id)
        (hashtable-set! exports id p)
        ;; net-kernel monitors p so we can drop it from exports when it dies
        (let ([nk (whereis 'net-kernel)])
          (when nk
            (send nk `#(export ,p)))))))

  (define (pid->token p)
    (cond
     [(rpid? p) (vector 'p (rpid-node p) (rpid-creation p) (rpid-id p))]
     [else
      (export! p)
      (vector 'p my-node my-creation (process-id p))]))

  (define (mon->token m)
    (let ([key (or (eq-hashtable-ref mon-keys m #f)
                   (let ([id next-mon-id])
                     (set! next-mon-id (+ id 1))
                     (let ([key (vector my-node my-creation id)])
                       (eq-hashtable-set! mon-keys m key)
                       (weak-set! local-mons id m)
                       key)))])
      (vector 'm key (pid->token (mon-origin m)) (pid->token (mon-target m)))))

  (define (object->token x)
    (with-interrupts-disabled
     (if (mon? x)
         (mon->token x)
         (pid->token x))))

  (define (token->object t)
    (with-interrupts-disabled
     (match t
       [#(p ,node ,creation ,id)
        (guard (and (symbol? node) (integer? creation) (integer? id)))
        (or (and (my-key? node creation) (lookup-export id))
            (intern-rpid node creation id))]
       [#(m ,(key <= #(,node ,creation ,id)) ,origin ,target)
        (guard (and (symbol? node) (integer? creation) (integer? id)))
        (let ([mine? (my-key? node creation)])
          (or (if mine?
                  (weak-ref local-mons id)
                  (weak-ref foreign-mons key))
              (let ([m (make-mon (token->object origin) (token->object target))])
                (eq-hashtable-set! mon-keys m key)
                (if mine?
                    (weak-set! local-mons id m)
                    (weak-set! foreign-mons key m))
                m)))]
       [,_ (throw `#(bad-token ,t))])))

  ;; Serialization

  (define (external? x) (or (process? x) (mon? x)))

  (define (encode obj)
    (let-values ([(op get) (open-bytevector-output-port)])
      (let ([tokens '()])
        (fasl-write obj op
          (lambda (x)
            (and (external? x)
                 (begin
                   (set! tokens (cons (object->token x) tokens))
                   #t))))
        (let ([body (get)])
          (fasl-write (list->vector (reverse tokens)) op)
          (let ([head (get)])
            (let* ([n (+ (bytevector-length head) (bytevector-length body))]
                   [frame (make-bytevector (+ 4 n))])
              (bytevector-u32-set! frame 0 n (endianness big))
              (bytevector-copy! head 0 frame 4 (bytevector-length head))
              (bytevector-copy! body 0 frame (+ 4 (bytevector-length head))
                (bytevector-length body))
              frame))))))

  (define (decode payload)
    (let* ([ip (open-bytevector-input-port payload)]
           [tokens (fasl-read ip 'load)])
      (unless (vector? tokens)
        (throw `#(bad-frame ,tokens)))
      (fasl-read ip 'load (vector-map token->object tokens))))

  (define (serializable? x)
    (let-values ([(op get) (open-bytevector-output-port)])
      (guard (c [else #f])
        (fasl-write x op external?)
        #t)))

  (define (wire-reason r)
    (let ([r (if (fault-condition? r) (fault-condition-reason r) r)])
      (cond
       [(serializable? r) r]
       [(condition? r)
        `#(unserializable-reason
           ,(call-with-string-output-port
             (lambda (op) (display-condition r op))))]
       [else
        `#(unserializable-reason
           ,(parameterize ([print-graph #t]) (format "~s" r)))])))

  (define (encode-ctl obj)
    (encode
     (match obj
       [#(exit ,kind ,to ,from ,reason) `#(exit ,kind ,to ,from ,(wire-reason reason))]
       [#(down ,m ,reason) `#(down ,m ,(wire-reason reason))]
       [,_ obj])))

  ;; Hooks called by (swish erlang)

  (define (unreachable node)
    (if (eq? node my-node) 'noproc 'noconnection))

  (define (hook-send dest msg)
    (cond
     [(rpid? dest)
      (let ([conn (node-conn (rpid-node dest))])
        (when conn
          (send conn `#(out ,(encode `#(send ,dest ,msg))))))]
     [(and (vector? dest)
           (fx= (vector-length dest) 2)
           (symbol? (vector-ref dest 0))
           (symbol? (vector-ref dest 1)))
      (let ([name (vector-ref dest 0)] [node (vector-ref dest 1)])
        (if (eq? node my-node)
            (send name msg)
            (let ([conn (node-conn node)])
              (when conn
                (send conn `#(out ,(encode `#(send ,name ,msg))))))))]
     [else (bad-arg 'send dest)]))

  (define (hook-link from to)
    (let ([conn (node-conn (rpid-node to))])
      (if conn
          (send conn `#(ctl #(link ,from ,to)))
          ($remote-exit 'link from to (unreachable (rpid-node to))))))

  (define (hook-unlink from to)
    (let ([conn (node-conn (rpid-node to))])
      (when conn
        (send conn `#(ctl #(unlink ,from ,to))))))

  (define (hook-monitor m)
    (let* ([node (rpid-node (mon-target m))]
           [conn (node-conn node)])
      (if conn
          (send conn `#(ctl #(monitor ,m)))
          ($remote-down m (unreachable node)))))

  (define (hook-demonitor m)
    (let ([conn (node-conn (rpid-node (mon-target m)))])
      (when conn
        (send conn `#(ctl #(demonitor ,m))))))

  ;; May be called with interrupts disabled from @kill, so we only enqueue.
  (define (hook-exit kind target from reason)
    (case kind
      [(link kill)
       (let ([conn (node-conn (rpid-node target))])
         (when conn
           (send conn `#(ctl #(exit ,kind ,target ,from ,reason)))))]
      [(down)
       (let ([conn (node-conn (rpid-node (mon-origin target)))])
         (when conn
           (send conn `#(ctl #(down ,target ,reason)))))]))

  (define hooks
    (make-dist-hooks hook-send hook-link hook-unlink hook-monitor
      hook-demonitor hook-exit))

  ;; Incoming requests, handled by the connection's reader process

  (define (deliver to x)
    (cond
     [(local-process? to) (send to x)]
     [(symbol? to)
      (let ([p (whereis to)])
        (when p
          (send p x)))]
     [else (void)]))

  (define (dispatch msg writer)
    (match msg
      [#(send ,to ,x) (deliver to x)]
      [#(link ,from ,to)
       (let ([r (if (local-process? to) ($remote-link to from) 'noproc)])
         (unless (eq? r #t)
           (send writer `#(ctl #(exit link ,from ,to ,r)))))]
      [#(unlink ,from ,to)
       (when (local-process? to)
         ($remote-unlink to from))]
      [#(exit ,kind ,to ,from ,reason)
       (when (and (memq kind '(link kill)) (local-process? to))
         ($remote-exit kind to from reason))]
      [#(monitor ,m)
       (let* ([to (mon-target m)]
              [r (if (local-process? to) ($remote-monitor m) 'noproc)])
         (unless (eq? r #t)
           (send writer `#(ctl #(down ,m ,r)))))]
      [#(demonitor ,m)
       (when (local-process? (mon-target m))
         ($remote-demonitor m))]
      [#(down ,m ,reason)
       (when (local-process? (mon-origin m))
         ($remote-down m reason))]
      [#(spawn ,from ,ref ,module ,proc ,args ,link?)
       (let ([reply
              (match (try (resolve-procedure module proc))
                [`(catch ,reason) `#(error ,reason)]
                [,f
                 ;; The new process does not run until we yield, so we can
                 ;; link and reply before it can exit.
                 (let ([pid (spawn (lambda () (apply f args)))])
                   (when link?
                     ($remote-link pid from))
                   `#(ok ,pid))])])
         (send writer `#(ctl #(spawn-reply ,from ,ref ,link? ,reply))))]
      [#(spawn-reply ,to ,ref ,link? ,reply)
       (match reply
         [#(ok ,pid)
          ;; Link here rather than in the caller so that we do not miss an
          ;; exit signal that arrives before the caller sees the reply.
          (when link?
            (let ([r (if (local-process? to) ($remote-link to pid) 'noproc)])
              (unless (eq? r #t)
                (send writer `#(ctl #(exit link ,pid ,to ,r))))))]
         [,_ (void)])
       (when (local-process? to)
         (send to `#(dist-spawn-reply ,ref ,reply)))]))

  ;; Remote spawn resolves procedures by name so that no closures cross
  ;; the wire. module is a library name or #f for interaction-environment.
  (define (resolve-procedure module proc)
    (let ([env (if module
                   (match (try (environment module))
                     [`(catch ,_) (throw `#(bad-module ,module))]
                     [,env env])
                   (interaction-environment))])
      (match (try (top-level-value proc env))
        [`(catch ,_) (throw `#(undefined-procedure ,module ,proc))]
        [,f (guard (procedure? f)) f]
        [,_ (throw `#(not-a-procedure ,module ,proc))])))

  (define (do-spawn who node spec link?)
    (unless (symbol? node)
      (bad-arg who node))
    (match spec
      [#(,module ,proc ,args)
       (guard (and (or (not module) (list? module)) (symbol? proc) (list? args)))
       (cond
        [(eq? node (dist:node))
         (let ([f (resolve-procedure module proc)])
           ((if link? spawn&link spawn) (lambda () (apply f args))))]
        [(node-conn node) =>
         (lambda (conn)
           (let ([ref (osi_make_uuid)]
                 [m (monitor conn)])
             (send conn
               `#(out ,(encode `#(spawn ,self ,ref ,module ,proc ,args ,link?))))
             (receive
              [#(dist-spawn-reply ,@ref ,reply)
               (demonitor&flush m)
               (match reply
                 [#(ok ,pid) pid]
                 [#(error ,reason) (throw reason)])]
              [`(DOWN ,@m ,_ ,_) (throw `#(noconnection ,node))])))]
        [else (throw `#(noconnection ,node))])]
      [,_ (bad-arg who spec)]))

  (define (dist:spawn node spec) (do-spawn 'dist:spawn node spec #f))

  (define (dist:spawn&link node spec) (do-spawn 'dist:spawn&link node spec #t))

  ;; Carriers move frames between nodes.

  (define-record-type carrier
    (nongenerative)
    (sealed #t)
    (fields
     (immutable read)                   ; thunk -> payload without length
     (immutable write)                  ; (write frame)
     (immutable close)))                ; thunk

  (define (read-message c) (decode ((carrier-read c))))
  (define (write-message c obj) ((carrier-write c) (encode obj)))

  (define (tcp-carrier ip op)
    (make-carrier
     (lambda ()
       (let* ([head (get-bytevector-exactly-n ip 4)]
              [n (bytevector-u32-ref head 0 (endianness big))])
         (when (> n max-frame-size)
           (throw `#(frame-too-large ,n)))
         (get-bytevector-exactly-n ip n)))
     (lambda (frame)
       (put-bytevector op frame)
       (flush-output-port op))
     (lambda ()
       (force-close-output-port op)
       (close-port ip))))

  (define (parse-address address)
    (define (host+port s)
      (let lp ([i (- (string-length s) 1)])
        (cond
         [(< i 0) (throw `#(bad-address ,address))]
         [(char=? (string-ref s i) #\:)
          (let ([port (string->number (substring s (+ i 1) (string-length s)))])
            (unless (and (fixnum? port) (fx< 0 port 65536))
              (throw `#(bad-address ,address)))
            (values
             (let ([host (substring s 0 i)])
               ;; allow [::1]:port
               (if (and (> (string-length host) 1)
                        (char=? (string-ref host 0) #\[))
                   (substring host 1 (- (string-length host) 1))
                   host))
             port))]
         [else (lp (- i 1))])))
    (unless (string? address)
      (bad-arg 'dist:connect address))
    (let-values ([(host port) (host+port address)])
      `#(tcp ,host ,port)))

  (define (open-carrier address)
    (match (parse-address address)
      [#(tcp ,host ,port)
       (let-values ([(ip op) (connect-tcp host port)])
         (tcp-nodelay op #t)
         (tcp-carrier ip op))]))

  ;; HMAC-SHA1 (RFC 2104); the default digest provider has no HMAC support.
  (define (sha1 . bvs)
    (let ([d (open-digest 'sha1)])
      (on-exit (close-digest d)
        (for-each
         (lambda (bv)
           (unless (fxzero? (bytevector-length bv))
             (hash! d bv)))
         bvs)
        (get-hash d))))

  (define (hmac-sha1 key msg)
    (let* ([key (if (> (bytevector-length key) 64) (sha1 key) key)]
           [k (make-bytevector 64 0)])
      (bytevector-copy! key 0 k 0 (bytevector-length key))
      (let ([pad (lambda (b)
                   (let ([bv (make-bytevector 64)])
                     (do ([i 0 (fx+ i 1)]) ((fx= i 64) bv)
                       (bytevector-u8-set! bv i
                         (fxlogxor b (bytevector-u8-ref k i))))))])
        (sha1 (pad #x5c) (sha1 (pad #x36) msg)))))

  (define (make-proof cookie challenge node)
    (hmac-sha1 cookie
      (call-with-bytevector-output-port
       (lambda (op)
         (put-bytevector op challenge)
         (put-bytevector op (string->utf8 (symbol->string node)))))))

  ;; Both sides send hello, then each proves knowledge of the cookie by
  ;; signing the other's challenge.
  (define (handshake c cookie)
    (define challenge (osi_make_uuid))
    (write-message c `#(hello ,protocol-version ,my-node ,my-creation ,challenge))
    (match (read-message c)
      [#(hello ,version ,node ,creation ,peer-challenge)
       (guard (and (eqv? version protocol-version) (symbol? node)
                   (integer? creation) (bytevector? peer-challenge)))
       (write-message c `#(proof ,(make-proof cookie peer-challenge my-node)))
       (match (read-message c)
         [#(proof ,proof)
          (unless (equal? proof (make-proof cookie challenge node))
            (throw 'bad-cookie))
          (when (eq? node my-node)
            (throw 'same-node))
          (vector node creation)]
         [,other (throw `#(bad-handshake ,other))])]
      [,other (throw `#(bad-handshake ,other))]))

  ;; Connection process: performs the handshake, registers with net-kernel,
  ;; then writes frames sent to it while a linked reader dispatches incoming
  ;; frames. Once running, it traps exits so it can close the carrier.
  (define (connection mode cookie)
    (define c
      (match mode
        [#(accept ,c) c]
        [#(connect ,address) (open-carrier address)]))
    (define watchdog
      (spawn&link
       (lambda ()
         (receive
          (after handshake-timeout
            ((carrier-close c))
            (raise 'handshake-timeout))))))
    (match (try (handshake c cookie))
      [`(catch ,reason ,e)
       ((carrier-close c))
       (raise e)]
      [#(,node ,creation)
       (unlink watchdog)
       (kill watchdog 'kill)
       (match (gen-server:call 'net-kernel `#(up ,node ,creation ,self) 'infinity)
         [ok (run-connection c)]
         [duplicate
          ((carrier-close c))
          (raise 'normal)])]))

  (define (run-connection c)
    (define writer self)
    (process-trap-exit #t)
    (on-exit ((carrier-close c))
      (let ([reader
             (spawn&link
              (lambda ()
                (let lp ()
                  (dispatch (read-message c) writer)
                  (lp))))])
        (let lp ()
          (receive
           [#(out ,frame)
            ((carrier-write c) frame)
            (lp)]
           [#(ctl ,obj)
            ((carrier-write c) (encode-ctl obj))
            (lp)]
           [`(EXIT ,pid ,reason ,e)
            (kill reader 'kill)
            (raise e)]
           [,_ (lp)])))))

  ;; net-kernel

  (define-state-tuple <net-kernel>
    listener                            ; tcp listener
    cookie                              ; bytevector
    conns                               ; ((pid . node) ...)
    pending                             ; ((pid . from) ...) for dist:connect
    next-id                             ; for connection child names
    )

  (define (init node cookie port address)
    (process-trap-exit #t)
    (let ([listener (listen-tcp address port self)])
      (with-interrupts-disabled
       (reset-tables!)
       (set! my-node node)
       (set! my-creation (erlang:now))
       (set-dist-hooks! hooks))
      `#(ok ,(<net-kernel> make
               [listener listener]
               [cookie cookie]
               [conns '()]
               [pending '()]
               [next-id 1]))))

  (define (terminate reason state)
    (close-tcp-listener ($state listener))
    (with-interrupts-disabled
     (let ([nodes (hashtable-keys node-conns)])
       (set-dist-hooks! #f)
       (reset-tables!)
       (set! my-node #f)
       (set! my-creation #f)
       (vector-for-each (lambda (node) ($node-down node 'noconnection)) nodes)))
    'ok)

  (define (start-connection mode state)
    (let* ([id ($state next-id)]
           [name (string->symbol (format "conn-~a" id))]
           [cookie ($state cookie)])
      (match (watcher:start-child 'dist-conn-sup name 1000
               (lambda ()
                 `#(ok ,(spawn (lambda () (connection mode cookie))))))
        [#(ok ,pid)
         (monitor pid)
         `#(,pid ,($state copy [next-id (+ id 1)]))]
        [#(error ,reason) (throw reason)])))

  (define (handle-call msg from state)
    (match msg
      [#(connect ,address)
       (match (start-connection `#(connect ,address) state)
         [#(,pid ,state)
          `#(no-reply ,($state copy* [pending (cons (cons pid from) pending)]))])]
      [#(up ,node ,creation ,pid)
       (let ([waiter (assq pid ($state pending))])
         (when waiter
           (gen-server:reply (cdr waiter) `#(ok ,node)))
         (let ([state ($state copy* [pending (remq waiter pending)])])
           (cond
            [(node-conn node) `#(reply duplicate ,state)]
            [else
             (with-interrupts-disabled
              (eq-hashtable-set! node-conns node pid))
             `#(reply ok
                 ,($state copy* [conns (cons (cons pid node) conns)]))])))]
      [listen-port
       `#(reply ,(listener-port-number ($state listener)) ,state)]
      [#(disconnect ,node)
       (let ([conn (node-conn node)])
         (when conn
           (kill conn 'shutdown))
         `#(reply ,(and conn #t) ,state))]))

  (define (handle-cast msg state) (match msg))

  (define (handle-info msg state)
    (match msg
      [#(accept-tcp ,_ ,ip ,op)
       (match (try (start-connection `#(accept ,(tcp-carrier ip op)) state))
         [`(catch ,reason)
          (force-close-output-port op)
          (close-port ip)
          `#(no-reply ,state)]
         [#(,_ ,state) `#(no-reply ,state)])]
      [#(accept-tcp-failed ,_ ,_ ,_)
       `#(stop ,msg ,state)]
      [#(export ,pid)
       (monitor pid)
       `#(no-reply ,state)]
      [`(DOWN ,_ ,pid ,reason)
       (cond
        [(assq pid ($state conns)) =>
         (lambda (entry)
           (let ([node (cdr entry)])
             (with-interrupts-disabled
              (when (eq? (eq-hashtable-ref node-conns node #f) pid)
                (eq-hashtable-delete! node-conns node)
                ($node-down node 'noconnection))))
           `#(no-reply ,($state copy* [conns (remq entry conns)])))]
        [(assq pid ($state pending)) =>
         (lambda (waiter)
           (gen-server:reply (cdr waiter) `#(error ,reason))
           `#(no-reply ,($state copy* [pending (remq waiter pending)])))]
        [else
         (with-interrupts-disabled
          (let ([id (process-id pid)])
            (when (eq? (hashtable-ref exports id #f) pid)
              (weak-set! exports id pid))))
         `#(no-reply ,state)])]
      [`(EXIT ,_ ,_) `#(no-reply ,state)]))

  ;; API

  (define (->cookie who cookie)
    (cond
     [(string? cookie) (string->utf8 cookie)]
     [(bytevector? cookie) cookie]
     [else (bad-arg who cookie)]))

  (define (full-node-name who name)
    (unless (symbol? name)
      (bad-arg who name))
    (let ([s (symbol->string name)])
      (if (memv #\@ (string->list s))
          name
          (string->symbol (format "~a@~a" s (osi_get_hostname))))))

  (define dist:start&link
    (case-lambda
     [(name cookie) (dist:start&link name cookie 0 "0.0.0.0")]
     [(name cookie port) (dist:start&link name cookie port "0.0.0.0")]
     [(name cookie port address)
      (let ([node (full-node-name 'dist:start&link name)]
            [cookie (->cookie 'dist:start&link cookie)])
        (arg-check 'dist:start&link
          [port fixnum? (lambda (p) (fx<= 0 p 65535))]
          [address string?])
        (supervisor:start&link 'dist-sup 'one-for-all 0 1
          `(#(dist-conn-sup
              ,(lambda () (watcher:start&link 'dist-conn-sup))
              permanent infinity supervisor)
            #(net-kernel
              ,(lambda ()
                 (gen-server:start&link 'net-kernel node cookie port address))
              permanent 1000 worker))))]))

  (define (dist:stop)
    (let ([sup (whereis 'dist-sup)])
      (when sup
        (let ([m (monitor sup)])
          (kill sup 'shutdown)
          (receive [`(DOWN ,@m ,_ ,_) 'ok]))))
    'ok)

  (define dist:connect
    (case-lambda
     [(address) (dist:connect address 30000)]
     [(address timeout)
      (parse-address address)
      (gen-server:call 'net-kernel `#(connect ,address) timeout)]))

  (define (dist:disconnect node)
    (arg-check 'dist:disconnect [node symbol?])
    (gen-server:call 'net-kernel `#(disconnect ,node)))

  (define (dist:listen-port)
    (gen-server:call 'net-kernel 'listen-port))

  (define dist:node
    (case-lambda
     [() (or my-node 'nonode@nohost)]
     [(p)
      (cond
       [(rpid? p) (rpid-node p)]
       [(process? p) (dist:node)]
       [else (bad-arg 'dist:node p)])]))

  (define (dist:nodes)
    (sort
     (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
     (with-interrupts-disabled (vector->list (hashtable-keys node-conns)))))
  )
