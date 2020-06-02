;;    Copyright © 2016 Martin Ek https://github.com/ekmartin/sprocket
;;    Copyright © 2018 Florian Léger <florilege@protonmail.com>
;;
;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <https://www.gnu.org/licenses/>.


#|
Initializes our web server.
|#
(define (create-server)
  ;;; Local variables
  (define error-handlers '())
  (define handlers '())

  ;;; Dispatchable procedures
  (define (add-handler handler #!optional path method)
    (let ((entry (create-entry handler path method)))
      (set! handlers
        (append-element handlers entry))))

  (define (add-error-handler handler #!optional path method)
    (let ((entry (create-entry handler path method)))
      (set! error-handlers
        (append-element error-handlers entry))))

  (define (listen tcp-port)
    (let ((socket (open-tcp-server-socket
                    tcp-port
                    (host-address-any))))
      (printf "Listening to port: ~A" tcp-port)
      (dynamic-wind
        (lambda () unspecific)
        (lambda ()
          (do () ((channel-closed? socket))
            (let ((port (tcp-server-connection-accept socket #t #f)))
              (dynamic-wind
                (lambda () unspecific)
                (lambda () (ignore-errors (lambda () (serve-request port))))
                (lambda () (ignore-errors (lambda () (close-port port))))))))
        (lambda () (channel-close socket)))))

  ;;; Private helper procedures
  (define (serve-request port)
    (let ((request (read-http-request port)))
      (define (catch-error err)
        (log "-> error in request ~A: ~A" request err)
        (handle-request
          (append-element error-handlers
                          (create-entry 500-handler))
          request port err))

      (define (try-request)
        (handle-request
          (append-element handlers (create-entry 404-handler))
          request port))

      (if INTERNAL-DEBUG-ERRORS
          ;; for debugging we don't want the error to be handled:
          (try-request)
          (call-with-current-continuation
            (lambda (cont)
              (bind-condition-handler
                (list condition-type:error)
                ;; if the request processing errored, run it again,
                ;; but this time with the error handlers:
                (lambda (err)
                  (catch-error err)
                  (cont '()))
                ;; try processing it normally first though:
                try-request))))))

  (define (create-entry handler #!optional path method)
    (make-middleware method path handler))

  (define (404-handler req params)
    '(404 () "Page Not Found"))

  (define (500-handler req params err)
    `(500 () ,(string-append
                "Something went wrong: "
                ;; TODO: should probably not leak this in production:
                (condition/report-string err))))

  (define (create-response result)
    (receive
      (status headers body)
      (apply values result)
      (make-http-response HTTP/1.1 status
                          (http-status-description status)
                          headers body)))

  ;;; Checks if the two given parts match, and returns a list of the
  ;;; matched parameter if it does (#f otherwise).
  (define (match-path-part request-part handler-part)
    (cond
      ((equal? request-part handler-part) '())
      ((eq? handler-part 'number-arg)
       (let ((number (string->number request-part)))
         (if (number? number)
             (list number)
             #f)))
      ((eq? handler-part 'string-arg) (list request-part))
      (else #f)))

  ;;; Attempts to match the request URL with the handler path.
  ;;; returns a list of matched parameters or #f if there's no match.
  (define (match-path request-url full-handler-path)
    (let loop ((request-path (cdr (uri-path request-url)))
               (handler-path full-handler-path)
               (params '()))
      (cond
        ;; /hi should be matched by /hi/there:
        ((null? handler-path) params)
        ((null? request-path) #f)
        (else
          (let ((matched (match-path-part
                           (car request-path)
                           (car handler-path))))
            (and matched
                 (loop
                   (cdr request-path)
                   (cdr handler-path)
                   (append params matched))))))))

  ;;; Check if the given middleware matches the request,
  ;;; if it does it returns a list of matched params, otherwise #f.
  (define (match-middleware request middleware)
    (let ((method (get-method middleware))
          (url (get-path middleware))
          (handler (get-handler middleware))
          (request-url (http-request-uri request))
          (request-method (http-request-method request)))
      (let ((matched-method
              (or (default-object? method)
                  (equal? method request-method)))
            (matched-path
              (if (default-object? url)
                  '()
                  (match-path request-url url))))
        (and matched-method matched-path))))

  ;;; Wraps the results from simple response handlers (i.e. ones that
  ;;; return strings) in a (200 '() body) list
  (define (wrap-result result)
    (if (string? result)
        `(200 () ,result)
        result))

  (define (evaluate-handler handler request params #!optional err)
    (wrap-result
      (if (default-object? err)
          (handler request params)
          (handler request params err))))

  (define (write-response result port)
    (let ((response (create-response result)))
      (log "-> writing response: ~A" response)
      (write-http-response response port)))

  (define (handle-request handler-list request port #!optional err)
    (let loop ((rest handler-list) (should-respond #t))
      (if (null? rest)
          'done
          (let ((params (match-middleware request (car rest))))
            (if (list? params)
                (let* ((middleware (car rest))
                       (handler (get-handler middleware)))
                  (log "-> evaluating handler: ~A" handler)
                  (let ((result (evaluate-handler handler request
                                                  params err)))
                    ;; only write a response if we haven't
                    ;; done so before:
                    (if (and should-respond (list? result))
                        (begin
                          (write-response result port)
                          (loop (cdr rest) #f))
                        ;; go through the rest of the middleware, even if we've
                        ;; sent out a response:
                        (loop (cdr rest) should-respond))))
                ;; this handler didn't match, so try the next one:
                (loop (cdr rest) should-respond))))))

  ;;; Dispatch on public procedures:
  (define (dispatch op)
    (case op
      ((listen) listen)
      ((add-handler) add-handler)
      ((add-error-handler) add-error-handler)
      (else (error "Unknown method: " op))))

  dispatch)

(define HTTP/1.1 (cons 1 1))

(define INTERNAL-DEBUG-ERRORS #t)
(define ENABLE-LOGGING #t)

;;; Public API

(define (listen server port)
  ((server 'listen) port))

(define (add-handler server handler #!optional path method)
  ((server 'add-handler) handler path method))

(define (add-error-handler server handler #!optional path method)
  ((server 'add-error-handler) handler path method))

(define (post server handler #!optional path)
  (add-handler server handler path "POST"))

(define (get server handler #!optional path)
  (add-handler server handler path "GET"))

(define (put server handler #!optional path)
  (add-handler server handler path "PUT"))

(define (delete server handler #!optional path)
  (add-handler server handler path "DELETE"))


(define (serve-file path #!optional headers)
  (if (default-object? headers) (set! headers '()))
  (let ((content (read-file path)))
    `(200 ,headers ,content)))

;;; create a redirect response, only supports
;;; absolute paths at the moment.
(define (redirect path #!optional status)
  (let ((redirect-status (if (default-object? status) 302 status))
        (location (make-http-header 'location path)))
    (list
      redirect-status
      (list location)
      (string-append "Redirecting to: " path))))

;;; creates a middleware that serves static files
;;; at the folder at `path`
(define (serve-static path)
  (lambda (req params)
    (let* ((static-path (uri-path (string->uri path)))
           (full-request-path (uri-path (http-request-uri req)))
           ;; if we get a request for /static/file.txt and path is
           ;; /static, we only care about file.txt - so find that:
           (request-path (sublist
                           full-request-path
                           (+ (length static-path) 1)
                           (length full-request-path))))
      (let* ((filename (path->file request-path))
             (file-path (string-append path filename))
             (content
               (call-with-current-continuation
                 (lambda (cont)
                   (bind-condition-handler
                     (list condition-type:file-error)
                     ;; we don't want to blow up the server
                     ;; for not found files etc., so pass it on to the
                     ;; next middleware.
                     (lambda (err)
                       (log "-> error serving file ~A: ~A" file-path err)
                       (cont '()))
                     (lambda ()
                       (read-file file-path)))))))
        (if (string? content)
            `(200 () ,content))))))

;;; Helper procedures

(define (log str . elements)
  ;; TODO: config option instead, see #9
  (if ENABLE-LOGGING
      (apply printf str elements)))

(define (path->file path)
  (string-append "/" (string-join path "/")))

;;; reads the string content at the given file path:
(define (read-file filename)
  (call-with-input-file filename
                        (lambda (port)
                          (read-string (char-set) port))))


(define (make-middleware method path handler)
  (list method path handler))

(define (get-method middleware)
  (car middleware))

(define (get-path middleware)
  (cadr middleware))

(define (get-handler middleware)
  (caddr middleware))
