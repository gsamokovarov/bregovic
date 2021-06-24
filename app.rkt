#lang racket

(require "server.rkt" 2htdp/batch-io)

(get "/style.css" (lambda (query)
                    "body {
                       font-family: Courier, Courier New, monospace;
                     }
                     h2 {
                       color: #CA6965;
                     }
                     #content {
                       margin: auto;
                       max-width: 900px;
                     }"))

(get "/" (lambda (query)
           `(html
              (head
                (title "Bregovic Demo")
                (link ((rel "stylesheet")
                       (href "/style.css")
                       (type "text/css"))))
              (body
                (div ((id "content"))
                  (h2 "Welcome")
                  (p "Bregovic is an HTTP web server written in Racket (formally
                      plt-scheme). It focuses on delivering DSL-like syntax
                      allowing very expressively looking programs.")
                  (h2 "Example")
                  (pre (code ,(read-file "app.rkt")))
                  (h2 "Source")
                  (p "To view the source go to "
                    (a ((href "/source")) "here") "."))))))

(get "/source" (lambda (query)
                 (read-file "server.rkt")))

(:404 (lambda ()
        `(html
          (head
            (title "Bregovic Oops!")
              (link ((rel "stylesheet")
                     (href "/style.css")
                     (type "text/css"))))
          (body
            (div ((id "content"))
              (h2 "Oops")
              (p "Whatever it is you are looking for, it is not here!"))))))

