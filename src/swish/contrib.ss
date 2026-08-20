(library (swish contrib)
  (export
   contrib
   contrib-enabled?
   )
  (import
   (chezscheme)
   (swish app-core)
   (swish io)
   (swish software-info)
   (swish string-utils)
   )
  ;; Usage:
  ;; In a script, do:
  ;; (contrib
  ;;   (import (shell)))
  ;; When swish-build runs, by default, this should fail.
  ;; swish-build may have a command-line option to allow it.
  ;;
  ;; TODO What to do about (compile-imported-libraries #t)? It seems like
  ;; we should find a place in ~/.cache or maybe in the the local
  ;; "project" directory to stash .so files.
  ;;
  ;; TODO Do we need to do something with other parameters like
  ;; source-directories?

  (define contrib-enabled?
    (make-parameter #t))

  (define (use-contribs)
    (when (contrib-enabled?)
      (let ([src (getenv "SWISH_CONTRIB_DIR")])
        (when src
          (let* ([src (get-real-path src)]
                 [home (get-real-path "~")]
                 [so-dir
                  (if (starts-with? src home)
                      (path-combine home ".cache" "swish-contrib"
                        ;; TODO this is a _really_ long
                        ;; directory name. Perhaps compute a
                        ;; short hash from the 2 hashes?
                        (format "~a-~a"
                          (software-revision 'swish)
                          (software-revision 'chezscheme))
                        ;; TODO hard to see if this is useful.
                        #;(substring src (+ (string-length home) 1) (string-length src)))
                      src)])
            (library-directories
             (list* (cons src so-dir) (library-directories))))))))

  (define-syntax contrib
    (syntax-rules ()
      [(_ clause ...)
       (begin
         (meta define __init__ (use-contribs))
         clause ...)]))
  )
