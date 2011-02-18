#lang racket

(require net/url xml srfi/13)

(provide dispatch-table serve get head post put delete :404)

; Interface
; =========

; A global dispatch table. This thing kills all of the functional strength, but
; delivers syntatic sugar in big bags.
(define dispatch-table (make-hash))

; serve : hash number -> lambda
; Serves a server (a dispathcer hash table) on some port.
(define (serve on-port)
  ; Define a `root` custodian for the function. Every custodian called from
  ; within this one will be treated as a `child` as the custodians form a
  ; hierarchy. What we call here the `root-custodian` is not actually the root
  ; of the whole hierarchy, just our subtree.
  (define root-custodian (make-custodian))
  (parameterize ([current-custodian root-custodian])
    (define listener (tcp-listen on-port 5 #t))
    (define (dispatch-and-handle dispatch-table listener)
      (define inner-custodian (make-custodian))
      (parameterize ([current-custodian inner-custodian])
        (define SUPPORTED-METHODS '("GET" "HEAD" "POST" "PUT" "DELETE"))
        ; dispatch : string url -> (oneof string xexpr fixnum)
        ; Dispatches the given path for the method else gives a 404 message.
        (define (dispatch method url)
          (when (member method SUPPORTED-METHODS)
            (define path (url-joined-path url))
            (define query (url-query url))
            (define method-table (hash-ref! dispatch-table method (make-hash)))
            (define method-handler (hash-ref method-table path #f))
            (displayln (format "~s ~s" method path))
            (if method-handler
              (method-handler query)
              404)))
        ; reply : string (oneof fixnum string xexpr) stream stream -> #<void>
        ; Sends response back to the client.
        (define (reply method data-or-code to)
          (define output 
            (if (number? data-or-code)
              (begin
                (hash-ref! dispatch-table data-or-code
                  (if (= data-or-code 404) 
                    ; The default 404 handler, if one is not specified.
                    (lambda ()
                     `(html
                       (body
                         (p "Not Found"))))
                    ; The default non 404 handler, if one is not specified. 
                    (lambda ()
                      '(html
                         (body
                           (p ,(format "Unhandled Code: ~s" data-or-code)))))))
                  ; Above we created the procedures, if needed here we just
                  ; execute them.
                  (if (xexpr? (hash-ref dispatch-table data-or-code))
                    (xexpr->string (hash-ref dispatch-table data-or-code))
                    (hash-ref dispatch-table data-or-code)))
              ; If the input is not a "error" code, then it is already processed
              ; data, so we just convert it to string, if needed.
              (if (xexpr? data-or-code)
                (xexpr->string data-or-code)
                (if (number? data-or-code)
                  (number->string data-or-code)
                  data-or-code))))
          (define code (if (not (number? data-or-code)) 200 data-or-code))
          ; Send the response to the stream.
          (display (format "HTTP/1.1 ~s\r\n" data-or-code) to)
          (display "Server: Bregovic/0.1 Racket/R6RS\r\n" to)
          (display (format "Content-Length: ~s\r\n" (string-length output)) to)
          (display "\r\n" to)
          (display output to))
        ; handle : stream stream -> #<void>
        ; Handles the heavy duty work of method selecting and calling the
        ; specified handlers.
        (define (handle in out)
          (define method-select-re
            #rx"^(GET|HEAD|POST|PUT|DELETE) (.+) HTTP/[0-9]+\\.[0-9]+")
          (define first-line (read-line in))
          (define match (regexp-match method-select-re first-line))
          (when match
            (define method (second match))
            (define url (string->url (third match)))
            (define dispatched (dispatch method url))
            (when dispatched
              (reply method dispatched out))))
        (define-values (in out) (tcp-accept listener))
        (thread (lambda ()
                  (handle in out)
                  (close-input-port in)
                  (close-output-port out))))
      (thread (lambda ()
                (sleep 1)
                (custodian-shutdown-all inner-custodian))))
    (define (loop)
      (dispatch-and-handle dispatch-table listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all root-custodian)))

; get : string (lambda : query -> (oneof string xexpr)) hash -> #<void>
; Binds the current GET path and it's handler to the dispatch-table dispatch table.
(define (get path handler)
  (http-method-helper "GET" path handler dispatch-table))

; head : string (lambda : query -> (oneof string xexpr)) hash -> #<void>
; Binds the current HEAD path and it's handler to the dispatch-table dispatch table.
(define (head path handler)
  (http-method-helper "HEAD" path handler dispatch-table))

; post : string (lambda : query -> (oneof string xexpr)) hash -> #<void>
; Binds the current POST path and it's handler to the dispatch-table dispatch table.
(define (post path handler)
  (http-method-helper "POST" path handler dispatch-table))

; put : string (lambda : query -> (oneof string xexpr)) hash -> #<void>
; Binds the current PUT path and it's handler to the dispatch-table dispatch table.
(define (put path handler)
  (http-method-helper "PUT" path handler dispatch-table))

; delete : string (lambda : query -> (oneof string xexpr)) hash -> #<void>
; Binds the current DELETE path and it's handler to the dispatch-table dispatch table.
(define (delete path handler)
  (http-method-helper "DELETE" path handler dispatch-table))

; :404 : (lambda -> (oneof string xexpr)) hash -> #<void>
(define (:404 handler dispatch-table)
  (hash-set! dispatch-table 404 handler))


; Utilities
; =========

(define (http-method-helper name path handler dispatch-table)
  ; Get the specified method by name in the dispatch table, if it is not found
  ; create it in it and return and empty hash. That way just about every hash
  ; can dispatch-table as dispatch table, without addition initialization work.
  (define table (hash-ref! dispatch-table name (make-hash)))
  (define normalized-path (string-trim-both path #\/))
  (hash-set! table normalized-path handler))

(define (url-joined-path url)
  (string-join (map path/param-path (url-path url)) "/"))

