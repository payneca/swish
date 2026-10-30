;; SPDX-License-Identifier: MIT
;; Copyright 2026 Chris Payne

;; Distributed echo: load this file into two REPLs started with src/go,
;; then send messages from one node to the other.
;;
;; Terminal 1:
;;   cd src; ./go
;;   (load "../examples/dist-echo/echo.ss")
;;   (start-node 'alpha@localhost 9001)
;;   (start-echo)
;;
;; Terminal 2:
;;   cd src; ./go
;;   (load "../examples/dist-echo/echo.ss")
;;   (start-node 'beta@localhost 9002)
;;   (dist:connect "127.0.0.1:9001")
;;   (echo 'alpha@localhost "hello")
;;
;; Terminal 1 prints the message, and terminal 2 receives the reply.
;; Run (start-echo) in terminal 2 as well to echo in the other direction.

(define echo-cookie "swish-echo")

;; dist:start&link links the distribution supervisor to its caller. Start it
;; from a separate process that traps exits, so an error at the REPL does
;; not shut down distribution.
(define (start-node name port)
  (let ([caller self])
    (spawn
     (lambda ()
       (process-trap-exit #t)
       (send caller `#(started ,(dist:start&link name echo-cookie port
                                  "127.0.0.1")))
       (let lp ()
         (receive
          [`(EXIT ,_ ,reason)
           (printf "distribution stopped: ~s\n" reason)
           (flush-output-port)]
          [,_ (lp)]))))
    (receive
     (after 5000 'timeout)
     [#(started ,result) (list result (dist:node))])))

;; The echo server prints each message and replies to the sender.
(define (start-echo)
  (define (loop)
    (receive
     [#(echo ,from ,msg)
      (printf "~a says: ~s\n" (dist:node from) msg)
      (flush-output-port)
      (send from `#(echoed ,(dist:node) ,msg))])
    (loop))
  (let ([pid (spawn loop)])
    (register 'echo pid)
    pid))

;; Send msg to the echo server on node and wait for its reply.
(define (echo node msg)
  (send `#(echo ,node) `#(echo ,self ,msg))
  (receive
   (after 5000 'timeout)
   [#(echoed ,from ,reply) `#(reply-from ,from ,reply)]))
