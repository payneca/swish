#!chezscheme
;;; Copyright 2018 Beckman Coulter, Inc.
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

(library (swish cli)
  (export
   <arg-choice>
   <arg-spec>
   cli-choice
   cli-specs
   display-help
   display-options
   display-usage
   format-spec
   help-wrap-width
   parse-command-line-arguments
   )
  (import
   (chezscheme)
   (swish dsm)
   (swish erlang)
   (swish errors)
   (swish meta)
   (swish pregexp)
   (swish string-utils)
   )

  (define-tuple <arg-spec>
    name      ; symbol that appears in output ht
    type      ;
    short     ; #f | character
    long      ; #f | string
    help      ; string describing argument
    default   ; #f | scheme object
    valid     ; #f | list of valid values
    specs     ; #f | list of <arg-spec> | list of <arg-choice>
    conflicts ; list of names
    requires  ; list of names
    usage     ; list of [show|hide|fit] and [long|short|req|opt|<how>]
    )

  (define-tuple <arg-choice>
    value                               ; string | positive integer
    specs                               ; list of <arg-spec>
    )

  (define (positional? s)
    (<arg-spec> open s [short long])
    (and (not short) (not long)))

  (define-syntax valid-short-char?
    (syntax-rules ()
      [(_ short)
       (let ([s short])
         (and (not (char=? s #\-))
              (not (char-numeric? s))
              (not (char-whitespace? s))))]))

  (define-syntax valid-type?
    (syntax-rules ()
      [(_ type short long)
       (match type
         [bool (or short long)]
         [count (or short long)]
         [(string ,s) (guard (string? s)) #t]
         [(list . ,patterns)
          (let lp ([patterns patterns])
            (match patterns
              [() #t]
              [,p (guard (string? p)) #t]
              [(,p (... ...)) (guard (string? p)) #t]
              [(,p . ,patterns) (guard (string? p)) (lp patterns)]
              [,_ #f]))]
         [,_ #f])]))

  (define-syntax valid-default?
    (syntax-rules ()
      [(_ type default-expr)
       (let ([default default-expr])
         (match type
           [bool (eq? default #f)]
           [count (eq? default #f)]
           [(string ,_) #t]
           [(list . ,_) (eq? default #f)]))]))

  (define-syntax valid-usage-how?
    (syntax-rules ()
      [(_ how)
       (let ()
         (define (valid-how? x)
           (match x
             [long #t]
             [short #t]
             [args #t]
             [(req ,x) (valid-how? x)]
             [(opt ,x) (valid-how? x)]
             [(and . ,rest) (andmap valid-how? rest)]
             [(or . ,rest) (andmap valid-how? rest)]
             [,_ #f]))
         (valid-how? how))]))

  (define-syntax extract-usage
    (syntax-rules ()
      [(_ usage)
       (partition (lambda (x) (memq x '(fit hide show))) usage)]))

  (define-syntax valid-usage?
    (syntax-rules ()
      [(_ usage-expr)
       (let ([usage usage-expr])
         (and (list? usage)
              (let-values ([(vis rest) (extract-usage usage)])
                (and (<= (length vis) 1)
                     (match rest
                       [(,how) (valid-usage-how? how)]
                       [,_ #f])))))]))

  (define-syntax valid-valid?
    (syntax-rules ()
      [(_ type valid-expr)
       (let ([valid valid-expr])
         (or (eq? valid #f)
             (and (list? valid)
                  (> (length valid) 1)
                  (match type
                    [bool #f]
                    [count
                     (for-all (lambda (x) (and (integer? x) (positive? x))) valid)]
                    [(string ,_)
                     (for-all string? valid)]
                    [(list . ,patterns)
                     (for-all string? valid)]))))]))

  (define-syntax (cli-specs x)

    (define (syntax->string x)
      (match (syntax->datum x)
        [-i (syntax-error (replace-source x #'spec) "use |-i| or |-I| in")]
        [,x (guard (symbol? x)) (format "~a" x)]
        [,_ #f]))
    (define (short? x)
      (let ([s (syntax->string x)])
        (and s
             (= (string-length s) 2)
             (char=? (string-ref s 0) #\-)
             (valid-short-char? (string-ref s 1)))))
    (define (long? x)
      (let ([s (syntax->string x)])
        (and s
             (>= (string-length s) 2)
             (char=? (string-ref s 0) #\-)
             (char=? (string-ref s 1) #\-))))
    (define (get-short x)
      (and x (string-ref (syntax->string x) 1)))
    (define (get-long x)
      (and x (let ([s (syntax->string x)])
               (substring s 2 (string-length s)))))
    (define (get-default-usage short long)
      (cond
       [short (values 'fit 'opt)]
       [long (values 'fit 'opt)]
       [else (values 'show 'req)]))
    (define (populate-defaults short long usage)
      (define base-how `(and (or short long) args))
      (define (either x y) (if (pair? x) x (list y)))
      (let-values ([(vis rest) (extract-usage usage)]
                   [(def-vis def-req) (get-default-usage short long)])
        (append
         (either vis def-vis)
         (match rest
           [(,x . ,rest)
            (guard (memq x '(opt req)))
            (cons `(,x ,base-how) rest)]
           [(,x . ,rest)
            (guard (memq x '(long short)))
            (cons `(,def-req (and ,x args)) rest)]
           [(,x . ,_)
            (guard (valid-usage-how? x))
            rest]
           [,_ (append rest (list `(,def-req ,base-how)))]))))
    (define (get-clause-list clause form)
      (syntax-case clause ()
        [(_ e ...) #'(e ...)]
        [_ (syntax-error form "invalid clause")]))

    (define (spec-maker spec name short long type help optionals)
      (let ([type (syntax->datum type)])
        (unless (valid-type? type short long)
          (syntax-error spec (format "invalid ~a in" type))))
      (let* ([short (get-short short)]
             [long (get-long long)]
             [clauses (collect-clauses x optionals '(default valid specs conflicts requires usage))]
             [default (or (find-clause 'default clauses)
                          #'(default #f))]
             [valid-clause (find-clause 'valid clauses)]
             [valid (and valid-clause #`(list #,@(get-clause-list valid-clause spec)))]
             [specs-clause (find-clause 'specs clauses)]
             [specs (and specs-clause #`(begin #,@(get-clause-list specs-clause spec)))]
             [conflicts (or (find-clause 'conflicts clauses)
                            #'(conflicts '()))]
             [requires (or (find-clause 'requires clauses)
                           #'(requires '()))]
             [usage-clause (find-clause 'usage clauses)]
             [usage (syntax->datum (or usage-clause '(usage)))]
             [full-usage (populate-defaults short long (cdr usage))])
        (unless (and (valid-usage? full-usage)
                     (or (not usage-clause)
                         (not (null? (scdr usage-clause)))))
          (syntax-error spec (format "invalid ~a in" usage)))
        #`(<arg-spec> make
            [name '#,name]
            [type '#,type]
            [short #,short]
            [long #,long]
            [help #,help]
            #,default
            [valid #,valid]
            [specs #,specs]
            #,conflicts
            #,requires
            [usage '#,(datum->syntax #'_ full-usage)])))

    (define (translate spec)
      (syntax-case spec ()
        [default-help
         (eq? (datum default-help) 'default-help)
         (translate #'[help -h --help bool "display this help and exit" (usage fit)])]
        [(name short long type help . optionals)
         (and (short? #'short) (long? #'long))
         (spec-maker spec #'name #'short #'long #'type #'help #'optionals)]
        [(name short type help . optionals)
         (short? #'short)
         (spec-maker spec #'name #'short #f #'type #'help #'optionals)]
        [(name long type help . optionals)
         (long? #'long)
         (spec-maker spec #'name #f #'long #'type #'help #'optionals)]
        [(name type help . optionals)
         (spec-maker spec #'name #f #f #'type #'help #'optionals)]))

    (syntax-case x ()
      [(_ spec ...)
       #`(list #,@(map translate #'(spec ...)))]))

  (define-syntax cli-choice
    (syntax-rules ()
      [(_ [$value $specs] ...)
       (list
        (<arg-choice> make
          [value $value]
          [specs $specs])
        ...)]))

  (define (bad-spec who what spec)
    (throw `#(bad-spec ,who ,what ,spec)))

  (define (check-specs specs) (check-specs-help (make-eq-hashtable) specs #t))
  (define (partial-check-specs specs) (check-specs-help #f specs #f))

  (define-tuple <checked>
    name->spec
    option->spec
    pos-specs
    )

  (define (check-specs-help pt specs check-missing?)
    (let ([name->spec (make-hashtable symbol-hash eq?)]
          [option->spec (make-hashtable equal-hash equal?)])
      (define (specs-missing ls)
        (fold-right
         (lambda (x acc)
           (if (hashtable-ref name->spec x #f)
               acc
               (cons x acc)))
         '()
         ls))
      (for-each
       (lambda (s)
         (<arg-spec> open s [name type short long help usage default valid])
         (unless (symbol? name) (bad-spec 'name name s))
         (unless (or (not short) (and (char? short) (valid-short-char? short)))
           (bad-spec 'short short s))
         (unless (or (not long) (string? long)) (bad-spec 'long long s))
         (unless (valid-type? type short long)
           (bad-spec 'type type s))
         (unless (valid-default? type default)
           (bad-spec 'default default s))
         (unless (or (string? help) (list? help)) (bad-spec 'help help s))
         (unless (valid-usage? usage) (bad-spec 'usage usage s))
         (unless (valid-valid? type valid)
           (bad-spec 'valid valid s))
         (hashtable-update! name->spec name
           (lambda (old)
             (when old (bad-spec 'duplicate-spec name s))
             s)
           #f)
         (when short
           (hashtable-update! option->spec short
             (lambda (old)
               (when old
                 (bad-spec 'duplicate-option-spec short s))
               s)
             #f))
         (when long
           (hashtable-update! option->spec long
             (lambda (old)
               (when old
                 (bad-spec 'duplicate-option-spec long s))
               s)
             #f)))
       specs)
      (for-each
       (lambda (s)
         (<arg-spec> open s [conflicts requires])
         (unless (and (list? conflicts) (for-all symbol? conflicts))
           (bad-spec 'conflicts conflicts s))
         (when check-missing?
           (let ([missing (specs-missing conflicts)])
             (unless (null? missing)
               (bad-spec 'missing-specs missing s))))
         (unless (and (list? requires) (for-all symbol? requires))
           (bad-spec 'requires requires s))
         (when check-missing?
           (let ([missing (specs-missing requires)])
             (unless (null? missing)
               (bad-spec 'missing-specs missing s)))))
       specs)
      (for-each
       (lambda (s)
         (<arg-spec> open s [specs])
         (when specs
           (cond
            [(andmap (<arg-spec> is?) specs)
             (check-specs-help pt specs check-missing?)]
            [(andmap
              (lambda (c)
                (match c
                  [`(<arg-choice> ,value)
                   (or (string? value)
                       (and (integer? value) (positive? value)))]
                  [,_ #f]))
              specs)
             (for-each
              (lambda (c)
                (check-specs-help pt (<arg-choice> specs c) check-missing?))
              specs)]
            [else
             (bad-spec 'specs specs s)])))
       specs)
      (let ([c (<checked> make
                 [name->spec name->spec]
                 [option->spec option->spec]
                 [pos-specs (filter positional? specs)])])
        (when pt
          (eq-hashtable-set! pt specs c))
        c)))

  (define-syntactic-monad P
    result
    name->spec
    option->spec
    pos-specs
    fail
    init-args
    specs->checked
    )

  (define (shortish? x)
    (and
     (char=? (string-ref x 0) #\-)
     (not (string=? x "-"))
     (not (char-numeric? (string-ref x 1)))))

  (define (maybe-option? arg)
    (or (starts-with? arg "--")
        (and (> (string-length arg) 0) (shortish? arg))))

  (define (update-list ht name value)
    (hashtable-update! ht name
      (lambda (old) (append old value))
      '()))

  (define (check-conflicts ht name->spec fail)
    (vector-for-each
     (lambda (name)
       (let* ([s (hashtable-ref name->spec name #f)]
              [ls (filter
                   (lambda (x) (hashtable-ref ht x #f))
                   (<arg-spec> conflicts s))])
         (unless (null? ls)
           (fail "~a conflicts with ~{~a~^, ~}"
             (describe-spec (hashtable-ref name->spec name #f))
             (map
              (lambda (x)
                (describe-spec (hashtable-ref name->spec x #f)))
              ls)))))
     (hashtable-keys ht))
    ht)

  (define (check-requires ht name->spec fail)
    (vector-for-each
     (lambda (name)
       (let* ([s (hashtable-ref name->spec name #f)]
              [ls (filter
                   (lambda (x) (not (hashtable-ref ht x #f)))
                   (<arg-spec> requires s))])
         (unless (null? ls)
           (fail "~a requires ~{~a~^, ~}"
             (describe-spec (hashtable-ref name->spec name #f))
             (map
              (lambda (x)
                (describe-spec (hashtable-ref name->spec x #f)))
              ls)))))
     (hashtable-keys ht))
    ht)

  (define (check-valid-values ht name->spec fail)
    (define (check val valid)
      (unless (member val valid)
        (fail (oxford-comma "~s is not one of ~{" "~s" " or " "~}") val valid)))
    (vector-for-each
     (lambda (p)
       (match p
         [(,name . ,val)
          (let ([s (hashtable-ref name->spec name #f)])
            (<arg-spec> open s [type valid])
            (when valid
              (match type
                ;; bool is not a valid type at this point
                [count (check val valid)]
                [(string ,_) (check val valid)]
                [(list . ,_)
                 (for-each
                  (lambda (x) (check x valid))
                  val)])))]))
     (hashtable-cells ht))
    ht)

  ;; TODO make-result and checkers seem like good candidates for dsm
  (define (make-result ht name->spec fail)
    (check-conflicts ht name->spec fail)
    (check-requires ht name->spec fail)
    (check-valid-values ht name->spec fail)
    (case-lambda
     [() ht]
     [(name)
      (unless (hashtable-ref name->spec name #f)
        (throw `#(no-spec-with-name ,name)))
      (hashtable-ref ht name #f)]))

  (P define (lookup-option x)
    (hashtable-ref option->spec x #f))

  (P define (done)
    ;; TODO could inline and eliminate
    result)

  (P define (take-pos arg arg*)
    (match pos-specs
      [()
       (fail "too many arguments: ~s" init-args)
       (P take-opt () arg*)]
      [(,[spec <= `(<arg-spec> ,name ,type)] . ,pos-specs)
       (match type
         [(string ,_)
          (hashtable-set! result name arg)
          (P advance () arg arg* spec arg)]
         [(list . ,patterns)
          (let lp ([patterns patterns] [ls (cons arg arg*)] [acc '()])
            (match patterns
              [()
               (update-list result name (reverse acc))
               (P take-opt () ls)]
              [,p                       ; rest
               (guard (string? p))
               (update-list result name (append (reverse acc) ls))
               (P take-opt () '())]
              [(,p ...)                 ; many
               (if (and (pair? ls) (not (maybe-option? (car ls))))
                   (lp patterns (cdr ls) (cons (car ls) acc))
                   (lp '() ls acc))]
              [(,p . ,patterns)         ; one
               (guard (and (pair? ls) (not (maybe-option? (car ls)))))
               (lp patterns (cdr ls) (cons (car ls) acc))]
              [,_
               (fail "option expects value: ~a" (format-spec spec 'args))
               (P take-opt () ls)]))])]))

  (P define (take-named arg arg* spec)
    (<arg-spec> open spec [name type default])
    (define (set-value x)
      (hashtable-update! result name
        (lambda (old)
          (when old (fail "duplicate option ~a" arg))
          x)
        #f))
    (match type
      [bool
       (set-value #t)
       (P advance () arg arg* spec #t)]
      [count
       (let* ([cell (hashtable-cell result name 0)]
              [value (+ (cdr cell) 1)])
         (set-cdr! cell value)
         (P advance () arg arg* spec value))]
      [(string ,_)
       (if (not default)
           (match arg*
             [(,arg . ,rest)
              (guard (not (maybe-option? arg)))
              (set-value arg)
              (P advance () arg rest spec arg)]
             [,_
              (fail "option expects value: ~a ~a" arg
                (format-spec spec 'args))
              (P take-opt () arg*)])
           (match arg*
             [()
              (set-value default)
              (P advance () arg arg* spec default)]
             [(,arg . ,rest)
              (cond
               [(maybe-option? arg)
                (set-value default)
                (P advance () arg arg* spec default)]
               [else
                (set-value arg)
                (P advance () arg rest spec arg)])]))]
      [(list . ,patterns)
       (let lp ([patterns patterns] [ls arg*] [acc '()])
         (match patterns
           [()
            (update-list result name (reverse acc))
            (P take-opt () ls)]
           [,p                          ; rest
            (guard (string? p))
            (update-list result name (append (reverse acc) ls))
            (P take-opt () '())]
           [(,p ...)                    ; many
            (if (and (pair? ls) (not (maybe-option? (car ls))))
                (lp patterns (cdr ls) (cons (car ls) acc))
                (lp '() ls acc))]
           [(,p . ,patterns)            ; one
            (guard (and (pair? ls) (not (maybe-option? (car ls)))))
            (lp patterns (cdr ls) (cons (car ls) acc))]
           [,_
            (fail "option expects value: ~a ~a" arg
              (format-spec spec 'args))
            (P take-opt () ls)]))]))

  (P define (advance arg arg* spec value)
    (<arg-spec> open spec [name specs])
    (define (sub-specs specs)
      (match (and specs (eq-hashtable-ref specs->checked specs #f))
        [#f
         (P take-opt () arg*)]
        [`(<checked> ,name->spec ,option->spec ,pos-specs)
         (let* ([sub (make-hashtable symbol-hash eq?)]
                [r (make-result
                    (P take-opt
                      ([result sub]
                       [name->spec name->spec]
                       [option->spec option->spec]
                       [pos-specs pos-specs])
                      arg*)
                    name->spec
                    fail)])
           (hashtable-set! result name (cons arg r))
           (P done))]))
    (cond
     [(not specs)
      (P take-opt () arg*)]
     [(let lp ([specs specs])
        (match specs
          [() #f]
          [(`(<arg-spec>) . ,_) #f]
          [(`(<arg-choice> [value ,cvalue] [specs ,cspecs]) . ,rest)
           (if (equal? cvalue value)
               cspecs
               (lp rest))])) =>
      (lambda (specs)
        (sub-specs specs))]
     [else
      (sub-specs specs)]))

  (P define (take-opt arg*)
    (match arg*
      [() (P done)]
      [(,arg . ,rest)
       (cond
        [(string=? arg "")
         (P take-pos () arg rest)]
        [(starts-with? arg "--")
         (let ([larg (substring arg 2 (string-length arg))])
           (cond
            [(P lookup-option () larg) =>
             (lambda (s)
               (P take-named () arg rest s))]
            [else
             (fail "unexpected ~a" arg)
             (P take-opt () rest)]))]
        [(shortish? arg)
         (cond
          [(> (string-length arg) 2)
           (P take-opt ()
             (append (map (lambda (c) (format "-~a" c))
                       (string->list (substring arg 1 (string-length arg))))
               rest))]
          [(P lookup-option () (string-ref arg 1)) =>
           (lambda (s)
             (P take-named () arg rest s))]
          [else
           (fail "unexpected ~a" arg)
           (P take-opt () rest)])]
        [else
         (P take-pos () arg rest)])]))

  (define (parse-arguments specs ls fail)
    (define specs->checked (make-eq-hashtable))
    (define result (make-hashtable symbol-hash eq?))
    (match-define `(<checked> ,name->spec ,option->spec ,pos-specs)
      (check-specs-help specs->checked specs #t))
    (make-result
     (P take-opt
       ([result result]
        [name->spec name->spec]
        [option->spec option->spec]
        [pos-specs pos-specs]
        [fail fail]
        [init-args ls]
        [specs->checked specs->checked])
       ls)
     name->spec
     fail))

  (define parse-command-line-arguments
    (case-lambda
     [(specs) (parse-command-line-arguments specs (command-line-arguments))]
     [(specs ls)
      (parse-command-line-arguments specs ls
        (lambda (fmt . args)
          (apply errorf #f fmt args)))]
     [(specs ls fail)
      (arg-check 'parse-command-line-arguments
        [ls list? (lambda (x) (for-all string? x))]
        [fail procedure?])
      (parse-arguments specs ls fail)]))

  (define (maybe-list . args) (remq #f args))

  (define (patterns->str patterns)
    (let lp ([patterns patterns] [acc '()])
      (match patterns
        [()
         (format "~{~a~^ ~}" (reverse acc))]
        [,p                             ; rest
         (guard (string? p))
         (lp '() (cons "..." (cons p acc)))]
        [(,p ...)                       ; many
         (guard (string? p))
         (lp '() (cons "..." (cons p acc)))]
        [(,p . ,patterns)               ; one
         (lp patterns (cons p acc))])))

  (define (usage->how usage)
    (find (lambda (x) (valid-usage-how? x)) usage))

  (define format-spec
    (case-lambda
     [(spec) (format-spec spec #f)]
     [(spec how)
      (<arg-spec> open spec [type short long usage default])
      (partial-check-specs (list spec))
      (let fmt ([how (or how (usage->how usage))])
        (match how
          [short (and short (format "-~a" short))]
          [long (and long (format "--~a" long))]
          [args
           (match type
             [bool #f]
             [count #f]
             [(string ,help)
              (if default
                  (format "[~a]" help)
                  help)]
             [(list . ,patterns) (patterns->str patterns)])]
          [(or . ,hows) (ormap fmt hows)]
          [(and . ,hows) (join (remq #f (map fmt hows)) #\space)]
          [(opt ,how)
           (let ([s (fmt how)])
             (and s (string-append "[" s "]")))]
          [(req ,how) (fmt how)]
          [,_ (bad-arg 'format-spec how)]))]))

  (define (describe-spec s)
    (format-spec s '(or long short args)))

  (define (help-left s)
    (let ([short (format-spec s 'short)] [long (format-spec s 'long)])
      (format "~{~a~^ ~}"
        (maybe-list
         (and (or short long)
              (format "~{~a~^, ~}" (maybe-list short long)))
         (format-spec s 'args)))))

  (define help-wrap-width
    (make-parameter 79
      (lambda (x)
        (unless (and (fixnum? x) (fx> x 0))
          (bad-arg 'help-wrap-width x))
        x)))

  (define (display-help-row s args op)
    (define indent 20)
    (define right-col-width (max 0 (- (help-wrap-width) indent)))
    (let* ([left (help-left s)]
           [right (<arg-spec> help s)]
           [right (if (list? right)
                      (join right #\space)
                      right)]
           [arg (and args (hashtable-ref args (<arg-spec> name s) #f))]
           [right (cond
                   [(string? arg)
                    (string-append right " (" arg ")")]
                   [(pair? arg)
                    (format "~a ~a" right arg)]
                   [else right])])
      (display-string "  " op)
      (display left op)
      (let* ([llen (+ (string-length left) 2)]
             [init-indent
              (cond
               [(< llen indent) (- indent llen)]
               [else
                (newline op)
                indent])])
        (wrap-text op right-col-width init-indent indent right)
        (newline op))))

  (define (display-usage-internal prefix exe-name width in-opt pos op)
    (define (prepare s)
      (<arg-spec> open s [type short long usage])
      (define flag-char
        (and short
             (pregexp-match (re "\\[-.\\]") (format-spec s))
             short))
      (cons (and (memq 'show usage) #t)
        (or flag-char (format-spec s))))
    (define (flag? x) (char? (cdr x)))
    (define (bracketed? x) (starts-with? (cdr x) "["))
    (define (fit w ls first-oh rest-oh)
      (let lp ([w w] [ls ls] [in '()] [out '()] [overhead first-oh])
        (match ls
          [() (values w (reverse in) (reverse out))]
          [(,x . ,rest)
           (match-let* ([(,show? . ,fmt) x])
             (let ([len (+ overhead (if (string? fmt) (string-length fmt) 1))])
               (cond
                [show? (lp (- w len) rest (cons fmt in) out rest-oh)]
                [(>= w len) (lp (- w len) rest (cons fmt in) out rest-oh)]
                [else (lp w rest in (cons x out) overhead)])))])))
    (define (visible? s) (not (memq 'hide (<arg-spec> usage s))))
    (define candidates (map prepare (filter visible? in-opt)))
    (define flag-oh (string-length " [-]"))
    (define (fmt-named w flag-oh)
      (let*-values ([(flag-opts arg-opts) (partition flag? candidates)]
                    [(opt-args req-args) (partition bracketed? arg-opts)]
                    [(w reqs req-other) (fit w req-args 1 1)]
                    [(w flags flag-other) (fit w flag-opts flag-oh 0)]
                    [(w opts opt-other) (fit w opt-args 1 1)]
                    [(flags) (and (pair? flags) flags)])
        (values flags
          (format "~@[ [-~{~c~}]~]~{ ~a~}~:[~; [options]~]~{ ~a~}"
            flags opts
            (or (pair? flag-other) (pair? req-other) (pair? opt-other))
            reqs))))
    (define leader (format "~a ~a" prefix exe-name))
    (define pos-args (format "~{ ~a~}" (map format-spec pos)))
    (define max-width (or width (help-wrap-width)))
    (define opt-width (- max-width (string-length leader) (string-length pos-args)))
    (define named-args
      (let-values ([(_ x) (fmt-named opt-width flag-oh)])
        (if (<= (string-length x) opt-width)
            x
            (let-values ([(min-flags minimal) (fmt-named 0 flag-oh)])
              (if (>= (string-length minimal) opt-width)
                  minimal
                  (let-values ([(_ x)
                                (fmt-named
                                 (- opt-width (string-length minimal))
                                 (if min-flags 0 flag-oh))])
                    x))))))
    (fprintf op "~a~a~a\n" leader named-args pos-args))

  (define (valid-width? n) (or (not n) (and (fixnum? n) (fx>= n 0))))

  (define display-usage
    (case-lambda
     [(prefix exe-name specs)
      (display-usage prefix exe-name specs #f)]
     [(prefix exe-name specs width)
      (display-usage prefix exe-name specs width (current-output-port))]
     [(prefix exe-name specs width op)
      (arg-check 'display-usage
        [prefix string?]
        [exe-name string?]
        [width valid-width?]
        [op output-port? textual-port?])
      (partial-check-specs specs)
      (let-values ([(pos opt) (partition positional? specs)])
        (display-usage-internal prefix exe-name width opt pos op))]))

  (define (display-options-internal opt pos args op)
    (for-each (lambda (o) (display-help-row o args op)) opt)
    (for-each (lambda (p) (display-help-row p args op)) pos))

  (define (parsed-options? args) (or (not args) (hashtable? args)))

  (define display-options
    (case-lambda
     [(specs) (display-options specs #f)]
     [(specs args) (display-options specs args (current-output-port))]
     [(specs args op)
      (arg-check 'display-options
        [args parsed-options?]
        [op output-port? textual-port?])
      (partial-check-specs specs)
      (let-values ([(pos opt) (partition positional? specs)])
        (display-options-internal opt pos args op))]))

  (define display-help
    (case-lambda
     [(exe-name specs)
      (display-help exe-name specs #f)]
     [(exe-name specs args)
      (display-help exe-name specs args (current-output-port))]
     [(exe-name specs args op)
      (arg-check 'display-help
        [exe-name string?]
        [args parsed-options?]
        [op output-port? textual-port?])
      (check-specs specs)
      (let-values ([(pos opt) (partition positional? specs)])
        (display-usage-internal "Usage:" exe-name #f opt pos op)
        (when (or (pair? opt) (pair? pos))
          (newline op)
          (display-options-internal opt pos args op)))]))

  )
