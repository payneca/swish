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

#!chezscheme
(import
 (chezscheme)
 (swish dist)
 (swish erlang)
 (swish gen-server)
 (swish io)
 (swish mat)
 (swish osi)
 (swish script-testing)
 (swish string-utils)
 (swish testing)
 )

(define cookie "dist-test-cookie")

;; Code we send to each child node. The evaluator lets the parent run
;; expressions on the child via ordinary dist messages.
(define child-prelude
  '((define (dist-test-evaluator)
      (receive
       [#(eval ,from ,expr)
        (spawn
         (lambda ()
           (send from
             `#(result ,expr
                 ,(match (try (eval expr))
                    [`(catch ,r)
                     `#(error
                        ,(if (condition? r)
                             (call-with-string-output-port
                              (lambda (op) (display-condition r op)))
                             r))]
                    [,v `#(ok ,v)])))))])
      (dist-test-evaluator))))

(define-tuple <node> name os-pid to-stdin)

;; Start a child swish OS process running a node named name.
(define (start-node name node-cookie)
  (define me self)
  (define (drain ip tag os-pid)
    (spawn
     (lambda ()
       (let ([ip (binary->utf8 ip)])
         (let lp ()
           (let ([line (get-line ip)])
             (unless (eof-object? line)
               (when tag
                 (send me `#(,tag ,os-pid ,line)))
               (lp))))))))
  (let-values ([(to-stdin from-stdout from-stderr os-pid)
                (spawn-os-process swish-exe '("-q") me)])
    (let ([to-stdin (binary->utf8 to-stdin)])
      (drain from-stdout 'stdout os-pid)
      (drain from-stderr #f os-pid)
      (for-each
       (lambda (x) (write x to-stdin) (newline to-stdin))
       `(,@child-prelude
         (dist:start&link ',name ,node-cookie 0 "127.0.0.1")
         (register 'evaluator (spawn dist-test-evaluator))
         (begin
           (printf "dist-port:~a\n" (dist:listen-port))
           (flush-output-port))))
      (flush-output-port to-stdin)
      (let ([port
             (let lp ()
               (receive (after 20000 (throw `#(timeout-starting-node ,name)))
                 [#(stdout ,@os-pid ,line)
                  (match (pregexp-match "dist-port:(\\d+)" line)
                    [(,_ ,port) (string->number port)]
                    [#f (lp)])]))])
        (values
         (<node> make [name name] [os-pid os-pid] [to-stdin to-stdin])
         (format "127.0.0.1:~a" port))))))

(define (stop-node node)
  (osi_kill* (<node> os-pid node) 9))

(define-syntax with-node
  (syntax-rules ()
    [(_ (node address name node-cookie) e1 e2 ...)
     (let-values ([(node address) (start-node name node-cookie)])
       (on-exit (stop-node node)
         e1 e2 ...))]))

(define (rpc node-name expr)
  (send `#(evaluator ,node-name) `#(eval ,self ,expr))
  (receive (after 10000 (throw `#(rpc-timeout ,node-name ,expr)))
    [#(result ,@expr #(ok ,v)) v]
    [#(result ,@expr #(error ,r)) (throw `#(rpc-error ,r))]))

(define (start-local-node)
  (start-silent-event-mgr)
  (match (dist:start&link 'a@test cookie 0 "127.0.0.1")
    [#(ok ,pid) pid]))

(define (connected address)
  (match (dist:connect address)
    [#(ok ,node) node]))

(isolate-mat no-dist ()
  (assert (eq? (dist:node) 'nonode@nohost))
  (assert (eq? (dist:node self) 'nonode@nohost))
  (assert (null? (dist:nodes)))
  (match (catch (dist:node 'x))
    [#(EXIT #(bad-arg dist:node x)) 'ok])
  (match (catch (send '#(foo b@test) 'x))
    [#(EXIT #(bad-arg send #(foo b@test))) 'ok]))

(isolate-mat connect ()
  (start-local-node)
  (assert (eq? (dist:node) 'a@test))
  (assert (eq? (dist:node self) 'a@test))
  (with-node (b address 'b@test cookie)
    (assert (eq? (connected address) 'b@test))
    (assert (equal? (dist:nodes) '(b@test)))
    (assert (equal? (rpc 'b@test '(dist:nodes)) '(a@test)))
    (assert (equal? (rpc 'b@test '(dist:node)) 'b@test))
    ;; connecting again reuses the connection
    (assert (eq? (connected address) 'b@test))
    (assert (equal? (dist:nodes) '(b@test)))
    ;; disconnect
    (assert (eq? #t (dist:disconnect 'b@test)))
    (let lp ([n 0])
      (unless (null? (dist:nodes))
        (when (> n 100) (throw 'still-connected))
        (sleep-ms 10)
        (lp (+ n 1))))
    (assert (eq? #f (dist:disconnect 'b@test)))
    (assert (eq? (connected address) 'b@test))
    (assert (equal? (rpc 'b@test '(dist:nodes)) '(a@test)))))

(isolate-mat bad-cookie ()
  (start-local-node)
  (with-node (b address 'b@test "wrong-cookie")
    (match (dist:connect address)
      [#(error bad-cookie) 'ok])
    (assert (null? (dist:nodes))))
  (match (dist:connect "127.0.0.1:1")
    [#(error ,_) 'ok])
  (match (catch (dist:connect "no-port"))
    [#(EXIT #(bad-address "no-port")) 'ok]))

(isolate-mat send-remote-pid ()
  (start-local-node)
  (with-node (b address 'b@test cookie)
    (connected address)
    (let ([p (rpc 'b@test
               '(spawn
                 (lambda ()
                   (let lp ()
                     (receive
                      [#(echo ,from ,x)
                       (send from `#(echoed ,self ,x))
                       (lp)])))))])
      (assert (process? p))
      (assert (eq? (dist:node p) 'b@test))
      (assert (equal? (format "~s" p)
                (format "#<process b@test:~a>" (process-id p))))
      ;; remote pids are interned
      (assert (eq? p (rpc 'b@test `(begin ',p))))
      ;; local pids round-trip
      (assert (eq? self (rpc 'b@test `(begin ',self))))
      (let ([data `(1 "two" 3.0 #(four) ,(string->utf8 "five") ,self)])
        (send p `#(echo ,self ,data))
        (receive (after 5000 (throw '#(timeout 1)))
          [#(echoed ,@p ,@data) 'ok]))
      ;; send to a registered name on the remote node
      (rpc 'b@test `(register 'echo ',p))
      (send '#(echo b@test) `#(echo ,self hello))
      (receive (after 5000 (throw '#(timeout 2)))
        [#(echoed ,@p hello) 'ok])
      ;; send to unknown remote names is silently dropped
      (send '#(no-such-name b@test) 'dropped)
      ;; #(name node) for the local node uses the local registry
      (register 'me self)
      (send '#(me a@test) 'local)
      (receive (after 5000 (throw '#(timeout 3)))
        [local 'ok])
      ;; messages that cannot be serialized raise in the sender
      (match (catch (send p (lambda () 'no-closures)))
        [#(EXIT ,_) 'ok]))))

(isolate-mat remote-monitor ()
  (start-local-node)
  (with-node (b address 'b@test cookie)
    (connected address)
    (let* ([spawn-waiter
            (lambda ()
              (rpc 'b@test
                '(spawn
                  (lambda ()
                    (receive
                     [#(exit ,reason) (raise reason)]
                     [normal 'ok])))))])
      ;; normal exit
      (let* ([p (spawn-waiter)] [m (monitor p)])
        (send p 'normal)
        (receive (after 5000 (throw '#(timeout 4)))
          [`(DOWN ,@m ,@p normal) 'ok]))
      ;; abnormal exit
      (let* ([p (spawn-waiter)] [m (monitor p)])
        (send p '#(exit crash))
        (receive (after 5000 (throw '#(timeout 5)))
          [`(DOWN ,@m ,@p crash) 'ok]))
      ;; error conditions are converted to strings
      (let* ([p (rpc 'b@test '(spawn (lambda () (receive [go (car 'x)]))))]
             [m (monitor p)])
        (send p 'go)
        (receive (after 5000 (throw '#(timeout 6)))
          [`(DOWN ,@m ,@p #(unserializable-reason ,s))
           (assert (string? s))]))
      ;; kill from this node
      (let* ([p (spawn-waiter)] [m (monitor p)])
        (kill p 'boom)
        (receive (after 5000 (throw '#(timeout 7)))
          [`(DOWN ,@m ,@p boom) 'ok]))
      (let* ([p (spawn-waiter)] [m (monitor p)])
        (kill p 'kill)
        (receive (after 5000 (throw '#(timeout 8)))
          [`(DOWN ,@m ,@p killed) 'ok]))
      ;; kill with normal is ignored by a process not trapping exits
      (let* ([p (spawn-waiter)] [m (monitor p)])
        (kill p 'normal)
        (receive (after 100 'ok)
          [`(DOWN ,@m ,@p ,r) (throw `#(unexpected ,r))])
        (send p 'normal)
        (receive (after 5000 (throw '#(timeout 9)))
          [`(DOWN ,@m ,@p normal) 'ok]))
      ;; demonitor
      (let* ([p (spawn-waiter)] [m (monitor p)] [m2 (monitor p)])
        (demonitor&flush m)
        (send p '#(exit after-demonitor))
        (receive (after 5000 (throw '#(timeout 10)))
          [`(DOWN ,@m2 ,@p after-demonitor) 'ok])
        (receive (after 100 'ok)
          [`(DOWN ,@m ,_ ,_) (throw 'demonitor-failed)]))
      ;; monitor a remote process that is already dead
      (let* ([p (spawn-waiter)] [m (monitor p)])
        (send p 'normal)
        (receive (after 5000 (throw '#(timeout 11)))
          [`(DOWN ,@m ,@p normal) 'ok])
        (let ([m (monitor p)])
          (receive (after 5000 (throw '#(timeout 12)))
            [`(DOWN ,@m ,@p ,reason)
             (assert (memq reason '(normal noproc)))])))
      ;; a remote process monitors a local process
      (let ([local (spawn (lambda () (receive [,reason (raise reason)])))])
        (rpc 'b@test
          `(let ([me ',self])
             (spawn
              (lambda ()
                (let ([m (monitor ',local)])
                  (send me 'monitoring)
                  (receive
                   [`(DOWN ,@m ,_ ,reason) (send me `#(remote-saw ,reason))]))))))
        (receive (after 5000 (throw '#(timeout 13))) [monitoring 'ok])
        (send local 'local-crash)
        (receive (after 5000 (throw '#(timeout 14)))
          [#(remote-saw local-crash) 'ok])))))

(isolate-mat remote-link ()
  (start-local-node)
  (with-node (b address 'b@test cookie)
    (connected address)
    (let* ([spawn-waiter
            (lambda ()
              (rpc 'b@test
                '(spawn
                  (lambda ()
                    (receive
                     [#(exit ,reason) (raise reason)]
                     [normal 'ok])))))])
      ;; trap-exit: abnormal and normal exits arrive as EXIT messages
      (process-trap-exit #t)
      (let ([p (spawn-waiter)])
        (link p)
        (send p '#(exit oops))
        (receive (after 5000 (throw '#(timeout 15)))
          [`(EXIT ,@p oops) 'ok]))
      (let ([p (spawn-waiter)])
        (link p)
        (send p 'normal)
        (receive (after 5000 (throw '#(timeout 16)))
          [`(EXIT ,@p normal) 'ok]))
      ;; unlink
      (let* ([p (spawn-waiter)] [m (monitor p)])
        (link p)
        (unlink p)
        (send p '#(exit unlinked))
        (receive (after 5000 (throw '#(timeout 17)))
          [`(DOWN ,@m ,@p unlinked) 'ok])
        (receive (after 100 'ok)
          [`(EXIT ,@p ,_) (throw 'unlink-failed)]))
      ;; link to a dead remote process
      (let* ([p (spawn-waiter)] [m (monitor p)])
        (send p '#(exit dead))
        (receive (after 5000 (throw '#(timeout 18)))
          [`(DOWN ,@m ,@p dead) 'ok])
        (link p)
        (receive (after 5000 (throw '#(timeout 19)))
          [`(EXIT ,@p ,reason) (assert (memq reason '(dead noproc)))]))
      (process-trap-exit #f)
      ;; a local process that is not trapping exits dies with the remote one
      (let* ([me self]
             [p (spawn-waiter)]
             [local (spawn (lambda () (link p) (send me 'linked) (receive)))]
             [m (monitor local)])
        (receive (after 5000 (throw '#(timeout 20))) [linked 'ok])
        (rpc 'b@test '(void))
        (send p '#(exit remote-died))
        (receive (after 5000 (throw '#(timeout 21)))
          [`(DOWN ,@m ,@local remote-died) 'ok]))
      ;; but survives a normal exit
      (let* ([me self]
             [p (spawn-waiter)]
             [local (spawn (lambda () (link p) (send me 'linked) (receive)))]
             [m (monitor local)])
        (receive (after 5000 (throw '#(timeout 22))) [linked 'ok])
        (rpc 'b@test '(void))
        (send p 'normal)
        (receive (after 200 'ok)
          [`(DOWN ,@m ,@local ,r) (throw `#(unexpected ,r))])
        (kill local 'kill)
        (receive (after 5000 (throw '#(timeout 23)))
          [`(DOWN ,@m ,@local killed) 'ok]))
      ;; a remote process dies when its linked local process dies
      (let* ([local (spawn (lambda () (receive [,reason (raise reason)])))]
             [p (rpc 'b@test
                  `(spawn (lambda () (link ',local) (receive))))]
             [m (monitor p)])
        (rpc 'b@test '(void))
        (send local 'local-died)
        (receive (after 5000 (throw '#(timeout 24)))
          [`(DOWN ,@m ,@p local-died) 'ok]))
      ;; a remote process trapping exits gets an EXIT message
      (let* ([local (spawn (lambda () (receive [,reason (raise reason)])))]
             [me self]
             [p (rpc 'b@test
                  `(spawn
                    (lambda ()
                      (process-trap-exit #t)
                      (link ',local)
                      (receive
                       [`(EXIT ,pid ,reason)
                        (send ',me `#(remote-got-exit ,pid ,reason))]))))])
        (rpc 'b@test '(void))
        (send local 'trapped)
        (receive (after 5000 (throw '#(timeout 25)))
          [#(remote-got-exit ,@local trapped) 'ok]))
      ;; kill a remote process trapping exits
      (let* ([me self]
             [p (rpc 'b@test
                  `(spawn
                    (lambda ()
                      (process-trap-exit #t)
                      (receive
                       [`(EXIT ,pid ,reason)
                        (send ',me `#(remote-got-exit ,pid ,reason))]))))])
        (kill p 'please-stop)
        (receive (after 5000 (throw '#(timeout 26)))
          [#(remote-got-exit ,@self please-stop) 'ok])))))

(isolate-mat noconnection ()
  (start-local-node)
  (let-values ([(b address) (start-node 'b@test cookie)])
    (on-exit (stop-node b)
      (connected address)
      (let* ([p (rpc 'b@test '(spawn (lambda () (receive))))]
             [m (monitor p)]
             [me self]
             [local (spawn (lambda () (link p) (send me 'linked) (receive)))]
             [lm (monitor local)])
        (receive (after 5000 (throw '#(timeout 27))) [linked 'ok])
        (process-trap-exit #t)
        (link p)
        (rpc 'b@test '(void))
        (stop-node b)
        (receive (after 5000 (throw '#(timeout 28)))
          [`(DOWN ,@m ,@p noconnection) 'ok])
        (receive (after 5000 (throw '#(timeout 29)))
          [`(EXIT ,@p noconnection) 'ok])
        (receive (after 5000 (throw '#(timeout 30)))
          [`(DOWN ,@lm ,@local noconnection) 'ok])
        (assert (null? (dist:nodes)))
        ;; operations on pids of disconnected nodes
        (send p 'dropped)
        (let ([m (monitor p)])
          (receive (after 5000 (throw '#(timeout 31)))
            [`(DOWN ,@m ,@p noconnection) 'ok]))
        (link p)
        (receive (after 5000 (throw '#(timeout 32)))
          [`(EXIT ,@p noconnection) 'ok])))))

(isolate-mat remote-gen-server ()
  (start-local-node)
  (with-node (b address 'b@test cookie)
    (connected address)
    (let ([server
           (rpc 'b@test
             '(let ()
                (define (init) '#(ok 0))
                (define (handle-call msg from state)
                  (match msg
                    [inc `#(reply ,(+ state 1) ,(+ state 1))]
                    [whoami `#(reply ,from ,state)]
                    [#(crash ,reason) (raise reason)]
                    [slow (receive (after 1000 `#(reply slow ,state)))]))
                (define (handle-cast msg state) `#(no-reply ,state))
                (define (handle-info msg state) `#(no-reply ,state))
                (define (terminate reason state) 'ok)
                (match (gen-server:start 'counter)
                  [#(ok ,pid) pid])))])
      (assert (= (gen-server:call server 'inc) 1))
      (assert (= (gen-server:call server 'inc) 2))
      (match (gen-server:call server 'whoami)
        [#(,@self ,m) (assert (monitor? m))])
      (match (catch (gen-server:call server 'slow 10))
        [#(EXIT #(timeout #(gen-server call (,@server slow 10)))) 'ok])
      (match (catch (gen-server:call server '#(crash bad-thing)))
        [#(EXIT #(bad-thing #(gen-server call (,@server #(crash bad-thing))))) 'ok])
      (match (catch (gen-server:call server 'inc))
        [#(EXIT #(,reason #(gen-server call (,@server inc))))
         (assert (memq reason '(noproc bad-thing)))]))))

(isolate-mat remote-spawn ()
  (start-local-node)
  (with-node (b address 'b@test cookie)
    (connected address)
    ;; procedure from a library
    (let ([p (dist:spawn 'b@test `#((swish erlang) send (,self hello)))])
      (assert (process? p))
      (assert (eq? (dist:node p) 'b@test))
      (receive (after 5000 (throw 'spawn-library))
        [hello 'ok]))
    ;; procedure from the interaction environment
    (rpc 'b@test
      '(define (dist-test-echo from)
         (send from `#(echo ,self ,(dist:node)))))
    (let ([p (dist:spawn 'b@test `#(#f dist-test-echo (,self)))])
      (receive (after 5000 (throw 'spawn-interaction))
        [#(echo ,@p b@test) 'ok]))
    ;; spawning on the local node
    (let ([p (dist:spawn 'a@test `#((swish erlang) send (,self local)))])
      (assert (eq? (dist:node p) 'a@test))
      (receive (after 5000 (throw 'spawn-local))
        [local 'ok]))
    ;; errors are reported to the caller
    (rpc 'b@test '(define dist-test-not-proc 42))
    (match (catch (dist:spawn 'b@test '#((no such library) foo ())))
      [#(EXIT #(bad-module (no such library))) 'ok])
    (match (catch (dist:spawn 'b@test '#(#f dist-test-no-such-proc ())))
      [#(EXIT #(undefined-procedure #f dist-test-no-such-proc)) 'ok])
    (match (catch (dist:spawn 'b@test '#(#f dist-test-not-proc ())))
      [#(EXIT #(not-a-procedure #f dist-test-not-proc)) 'ok])
    (match (catch (dist:spawn 'a@test '#(#f dist-test-no-such-proc ())))
      [#(EXIT #(undefined-procedure #f dist-test-no-such-proc)) 'ok])
    (match (catch (dist:spawn 'c@test '#((chezscheme) list ())))
      [#(EXIT #(noconnection c@test)) 'ok])
    (match (catch (dist:spawn 'b@test '#((chezscheme) list oops)))
      [#(EXIT #(bad-arg dist:spawn #((chezscheme) list oops))) 'ok])
    (match (catch (dist:spawn "b@test" '#((chezscheme) list ())))
      [#(EXIT #(bad-arg dist:spawn "b@test")) 'ok])
    ;; no closures cross the wire
    (match (catch (dist:spawn 'b@test `#((chezscheme) apply (,list (1 2)))))
      [#(EXIT ,_) 'ok])
    ;; a failure in the spawned process is not reported to the caller
    (let* ([p (dist:spawn 'b@test '#((chezscheme) raise (oops)))]
           [m (monitor p)])
      (receive (after 5000 (throw 'spawn-raise))
        [`(DOWN ,@m ,@p ,reason) (assert (memq reason '(oops noproc)))]))))

(isolate-mat remote-spawn&link ()
  (start-local-node)
  (with-node (b address 'b@test cookie)
    (connected address)
    ;; the link is in place before the remote process can exit
    (process-trap-exit #t)
    (let ([p (dist:spawn&link 'b@test '#((chezscheme) raise (oops)))])
      (receive (after 5000 (throw 'spawn&link-exit))
        [`(EXIT ,@p oops) 'ok]))
    (let ([p (dist:spawn&link 'b@test '#((chezscheme) void ()))])
      (receive (after 5000 (throw 'spawn&link-normal))
        [`(EXIT ,@p normal) 'ok]))
    (process-trap-exit #f)
    ;; the remote process dies with its local caller
    (let* ([me self]
           [caller
            (spawn
             (lambda ()
               (send me
                 `#(spawned
                    ,(dist:spawn&link 'b@test '#(#f dist-test-evaluator ()))))
               (receive [,reason (raise reason)])))]
           [p (receive (after 5000 (throw 'spawn&link-caller))
                [#(spawned ,p) p])]
           [m (monitor p)])
      (send caller 'caller-died)
      (receive (after 5000 (throw 'spawn&link-remote-died))
        [`(DOWN ,@m ,@p caller-died) 'ok]))
    ;; and the local caller dies with the remote process
    (let* ([me self]
           [caller
            (spawn
             (lambda ()
               (send me
                 `#(spawned
                    ,(dist:spawn&link 'b@test '#(#f dist-test-evaluator ()))))
               (receive)))]
           [cm (monitor caller)]
           [p (receive (after 5000 (throw 'spawn&link-caller2))
                [#(spawned ,p) p])])
      (kill p 'remote-died)
      (receive (after 5000 (throw 'spawn&link-caller-died))
        [`(DOWN ,@cm ,@caller remote-died) 'ok]))
    ;; errors
    (match (catch (dist:spawn&link 'b@test '#(#f dist-test-no-such-proc ())))
      [#(EXIT #(undefined-procedure #f dist-test-no-such-proc)) 'ok])))

(define (wait-for thunk what)
  (let lp ([n 0])
    (unless (thunk)
      (when (> n 500) (throw `#(wait-for-timeout ,what)))
      (sleep-ms 10)
      (lp (+ n 1)))))

(isolate-mat global-names ()
  (start-local-node)
  (with-node (b address 'b@test cookie)
    (connected address)
    (let ([p (spawn (lambda () (receive [,reason (raise reason)])))]
          [q (rpc 'b@test '(spawn (lambda () (let lp () (receive [#(echo ,from ,x) (send from x) (lp)])))))])
      (wait-for
       (lambda () (= 2 (length (gen-server:call 'global-name-server 'servers))))
       'servers)
      ;; register here, visible there
      (assert (eq? #t (global:register 'p p)))
      (assert (eq? p (global:whereis 'p)))
      (assert (eq? p (rpc 'b@test '(global:whereis 'p))))
      (match (catch (global:register 'p self))
        [#(EXIT #(name-already-registered ,@p)) 'ok])
      (match (catch (rpc 'b@test `(global:register 'p ',self)))
        [#(EXIT #(rpc-error #(name-already-registered ,@p))) 'ok])
      ;; register there, visible here
      (assert (eq? #t (rpc 'b@test `(global:register 'q ',q))))
      (assert (eq? q (global:whereis 'q)))
      ;; send to #(global name)
      (send '#(global q) `#(echo ,self via-global))
      (receive (after 5000 (throw 'send-global))
        [via-global 'ok])
      (match (catch (send '#(global nobody) 'x))
        [#(EXIT #(bad-arg send #(global nobody))) 'ok])
      ;; unregister
      (assert (eq? #t (global:unregister 'q)))
      (assert (not (global:whereis 'q)))
      (wait-for (lambda () (not (rpc 'b@test '(global:whereis 'q)))) 'unregister)
      ;; names are removed when processes die
      (send p 'die)
      (wait-for (lambda () (not (global:whereis 'p))) 'local-death)
      (wait-for (lambda () (not (rpc 'b@test '(global:whereis 'p)))) 'remote-death)
      (rpc 'b@test `(global:register 'q ',q))
      (wait-for (lambda () (global:whereis 'q)) 'q)
      (kill q 'kill)
      (wait-for (lambda () (not (global:whereis 'q))) 'remote-death2)
      ;; and when nodes disconnect
      (let ([r (rpc 'b@test '(spawn (lambda () (receive))))])
        (rpc 'b@test `(global:register 'r ',r))
        (wait-for (lambda () (global:whereis 'r)) 'r)
        (dist:disconnect 'b@test)
        (wait-for (lambda () (not (global:whereis 'r))) 'disconnect))
      (match (catch (global:register "x" self))
        [#(EXIT #(bad-arg global:register "x")) 'ok])
      (match (catch (global:register 'x 'y))
        [#(EXIT #(bad-arg global:register y)) 'ok]))))

(isolate-mat global-lock ()
  ;; concurrent registrations of the same name: exactly one succeeds
  (start-local-node)
  (with-node (b address 'b@test cookie)
    (connected address)
    (wait-for
     (lambda () (= 2 (length (gen-server:call 'global-name-server 'servers))))
     'servers)
    (rpc 'b@test '(define dist-test-pid (spawn (lambda () (receive)))))
    (do ([i 0 (+ i 1)]) ((= i 10))
      (let* ([name (string->symbol (format "race~a" i))]
             [me self]
             [local (spawn (lambda () (receive)))])
        (spawn
         (lambda ()
           (send me
             `#(here ,(catch (global:register name local))))))
        (spawn
         (lambda ()
           (send me
             `#(there
                ,(catch (rpc 'b@test `(global:register ',name dist-test-pid)))))))
        (let ([results
               (map (lambda (tag)
                      (receive (after 10000 (throw 'race-timeout))
                        [#(,@tag ,r) r]))
                 '(here there))])
          (assert (= 1 (length (filter (lambda (r) (eq? r #t)) results))))
          (let ([winner (global:whereis name)])
            (assert (process? winner))
            (assert (eq? winner (rpc 'b@test `(global:whereis ',name))))))))))

(isolate-mat global-conflict ()
  ;; conflicting names registered during a partition are resolved when the
  ;; nodes reconnect
  (start-local-node)
  (with-node (b address 'b@test cookie)
    (connected address)
    (let ([p (spawn (lambda () (receive)))]
          [q (rpc 'b@test '(spawn (lambda () (receive))))])
      (rpc 'b@test `(define dist-test-q ',q))
      (dist:disconnect 'b@test)
      (wait-for (lambda () (null? (dist:nodes))) 'disconnect)
      (global:register 'x p)
      ;; talk to b via stdin since we are not connected
      (let ([op (<node> to-stdin b)] [os-pid (<node> os-pid b)])
        (write
         '(begin
            (printf "global-x:~a\n" (global:register 'x dist-test-q))
            (flush-output-port))
         op)
        (newline op)
        (flush-output-port op)
        (receive (after 10000 (throw 'partition-register))
          [#(stdout ,@os-pid "global-x:#t") 'ok]))
      (connected address)
      ;; a@test sorts before b@test, so p wins
      (let ([m (monitor q)])
        (receive (after 5000 (throw 'conflict))
          [`(DOWN ,@m ,@q ,reason)
           (assert (member reason '(#(global-name-conflict x) noproc)))])
        (assert (process-alive? p 0))
        (assert (eq? p (global:whereis 'x)))
        (assert (eq? p (rpc 'b@test '(global:whereis 'x))))))))
