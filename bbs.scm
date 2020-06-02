(load-option 'format)
(load "lib/utils")
(load "deps/irregex")
(load "deps/srfi-26")
(load "deps/httpio")
(load "deps/server")
(load "lib/html")
(load "lib/parameters")
(load "lib/markup")
(load "templates")

(define *sexp* "data/sexp")
(define *html* "data/html")
(define *frontpage-threads* 10)
(define *max-headline-size* 78)
(define *max-post-size* 8192)
(define *max-posts* 300)

(define (get-form-hash)
  "TODO"
  (call-with-input-file "hash" read))

;;; helpers

(define (make-path . args)
  (string-join args "/"))

(define (make-abs-path . args)
  (string-join (cons "" args) "/"))

(define server (create-server))

(define (make-response template)
  `(200 ,(list (make-http-header 'content-type "text/html; charset=utf-8"))
    ,(with-output-to-string (lambda () (sxml->html template)))))

(define (write-and-serve path template)
    (with-output-to-file path (lambda () (sxml->html template)))
    (serve-file path (list (make-http-header 'content-type "text/html; charset=utf-8")
                           (make-http-header 'cache-control "Private"))))

;;; static files
(get server (serve-static "static") '("static"))

(get server (lambda (req params) (serve-file "static/favicon.ico")) '("favicon.ico"))

(add-handler server (lambda (req params) (route req)))

(define (ignore-qstring fullpath)
  (let ((l (string-split fullpath #\?)))
    (car l)))

(define (get-query-string fullpath)
  (let ((l (string-split fullpath #\?)))
    (if (null? (cdr l))
        ""
        (cadr l))))

(define (add-query-string path query-string)
  (if (string-null? query-string)
      path
      (string-append path "?" query-string)))


(define (route req)
  (let* ((fullpath (uri->string (http-request-uri req)))
        (path (string-split (ignore-qstring fullpath) #\/))
        (query-string (get-query-string fullpath))
        (method (http-request-method req))
        (headers (http-request-headers req))
        (ip (http-header 'x-forwarded-for headers #f))
        )
    ;(pp ip)
    (pp req)
    ;(pp headers)
    (pp (http-header 'x-forwarded-for headers #f))
    ;(pp (http-header 'host headers #f))
    (cond ((equal? method "GET")
          (match path
                 (() () '(200 () "site root"))
                 ((,board) () (view-index board))
                 ((,board "list") () (view-list board))
                 ((,board "preferences") () (set-preferences board query-string))
                 ((,board ,thread) (integer? (string->number thread)) (view-thread board thread))
                 ((,board ,thread ,posts) (and (integer? (string->number thread)) (range? posts) (< (string-length posts) 40))
                   (view-thread board thread posts))
                 (_ () not-found)))
          ((equal? method "POST")
           (match path
                  ((,board "post") () (post-thread board req query-string))
                  ((,board ,thread "post") (integer? (string->number thread)) (post-message board thread req query-string))
                  (_ () method-not-allowed)))
          (else method-not-allowed))))

;;; errors
(define bad-request
  `(400 () "Bad Request"))
(define not-found
  `(404 () "Not found"))
(define method-not-allowed
  '(405 () "Method not allowed"))

(define (title board)
  (string-append "/" board "/ - SchemeBBS"))
;;; views
(define (thread-template board thread posts headline filter-func)
  (main-template (title board) (thread-view board thread posts headline filter-func) "thread"))

(define (list-template board threads)
  (main-template (title board) (list-view board threads)))

(define (index-template board threads)
  (main-template (title board) (frontpage-view board threads)))

(define (preferences-template board query-string-list)
  (main-template (title board) (preferences-view board query-string-list)))

(define (retry-thread-template board headline message flash)
  (main-template (title board) (make-thread-form board headline message flash) "thread"))

(define (retry-post-template board thread frontpage? message flash)
  (main-template (title board) `(dl ,(make-post-form board thread frontpage? message flash)) "thread"))



;;; controllers GET

(define (set-preferences board query-string)
  (let ((query-string-list (parameters->alist query-string)))
    (make-response (preferences-template board query-string-list))))

(define (view-thread board thread #!optional range) 
  (let ((path (make-path *sexp* board thread))
        (cache (make-path *html* board thread)))
    (cond ((file-exists? path)
           (let* ((t (call-with-input-file path read))
                  (headline (lookup-def 'headline t))
                  (posts (lookup-def 'posts t))
                   (norange (default-object? range))
                   (rangeonce (if norange "unused" (posts-range range)))
                   (filter-func (if norange
                                    (lambda (e) #t)
                                    (lambda (e) (vector-ref rangeonce (car e))))))
              (cond (norange
                  (if (not (file-exists? cache))
                      (write-and-serve cache (thread-template board thread posts headline filter-func))
                      (serve-file cache))) ;; we shouldn't go here, reverse proxy fetches the page itself
                   ((and (string->number range)
                         (> (string->number range) (length posts)))
                    not-found)
                   (else (make-response (thread-template board thread posts headline filter-func))))))
          (else not-found))))


(define (range? posts)
  (irregex-match "[1-9][0-9]{0,2}(-[1-9][0-9]{0,2})?(,[1-9][0-9]{0,2}(-[1-9][0-9]{0,2})?){0,20}" posts))

(define (posts-range range)
  (define (expand-range x)
    (cond ((> (length x) 1)
           (let* ((a (string->number (car x)))
                  (b (string->number (cadr x)))
                  (low (if (> a *max-posts*) *max-posts* a))
                  (high (if (> b *max-posts*) *max-posts* b))
                  (count (+ (- high low) 1)))
             (if (> high low)
                 (lambda () (iota count low))
                 (lambda () (list low)))))
          (else (let* ((a (string->number (car x)))
                       (low (if (> a *max-posts*) *max-posts* a)))
                  (lambda () (list low))))))
  (define (invoke-loop-set vector lamb)
    (for-each (lambda (e) (vector-set! vector e #t))
              (lamb)))
  (let* ((r1 (string-split range #\,))
         (r2 (map (lambda (x) (string-split x #\-)) r1))
         (r3 (map expand-range r2))
         (vec (make-vector (+ *max-posts* 1) #f)))
    (for-each (lambda (e) (invoke-loop-set vec e))
              r3)
    vec))

(define (view-list board)
  (let* ((path (make-path *sexp* board "list"))
         (cache (make-path *html* board "list"))
         (threads (if (file-exists? path) (call-with-input-file path read) '())))
    (cond ((file-exists? path)
    (if (not (file-exists? cache))
        (write-and-serve cache (list-template board threads))
        (serve-file cache))) ;; we shouldn't go there with a reverse proxy
            (else not-found))))

;(make-response (list-template board threads))))

(define (view-index board)
 (let* ((path (make-path *sexp* board "index"))
        (cache (make-path *html* board "index"))
        (threads (if (file-exists? path)
                  (call-with-input-file path read)
                  '())))
  (cond ((file-exists? path)
         (if (not (file-exists? cache))
          (write-and-serve cache (index-template board threads))
          (serve-file cache)))
   (else not-found))))

;;; controllers POST
(define (post-message board thread req query-string)
  (let ((path (make-path *sexp* board thread))
        (cache (make-path *html* board thread)))
    ;;; TODO verify if thread is archived
    (cond ((file-exists? path)
           (let* ((t (call-with-input-file path read))
                  (posts (lookup-def 'posts t))
                  (post-number (+ 1 (car (last posts))))
                  (body (http-request-body req))
                  (params (parameters->alist body))
                  (frontpage (lookup-def 'frontpage params))
                  (message (decode-formdata (lookup-def 'epistula params)))
                  (date (get-date))
                  (vip (assq 'vip params))
                  (validation (validate-form params message)))
             (cond ((> post-number *max-posts*)
                    `(200 () "max posts")) ;; TODO
                   ((eq? validation 'ok)
                    (let ((sxml (markup->sxml message board thread)))
                      (cond ((null? sxml) bad-request)
                            (else
                              (append! posts `((,post-number . ((date . ,date)
                                                                (vip . ,vip)
                                                                (content . ,sxml)))))
                              (call-with-output-file path (lambda (port) (write t port)))
                              (if (file-exists? cache) (delete-file cache))
                              (if vip
                                  (update-post-count board thread date post-number)
                                  (update-thread-list board (string->number thread) date post-number))
                              (update-frontpage board)
                              (if (equal? frontpage "true")
                                  (redirection board thread (number->string post-number) query-string #t #f)
                                  (redirection board thread (number->string post-number) query-string #f #f))))))
                   ((eq? validation 'spam) `(301 ,(list (make-http-header 'location "http://4chan.org")) "SNAFU"))
                   (else 
                     
                     (retry-post-form validation board thread frontpage params)))))
          (else not-found)))) 

(define (redirection board thread post query-string frontpage? newthread?)
  (if frontpage?
      `(303 ,(list (make-http-header
                     'location
                     (if newthread?
                         (add-query-string (string-append "/" board) query-string)
                         (string-append (add-query-string (string-append "/" board) query-string) "#t" thread "p" post))))
        "That was SICP quality!")
      `(303 ,(list (make-http-header
                     'location
                      (string-append  (add-query-string (string-append  "/" board "/" thread) query-string) "#t" thread "p" post)))
        "That was SICP quality")))

(define (update-post-count board thread date post-count)
  (let ((cache (make-path *html* board "list")))
    (if (file-exists? cache) (delete-file cache)))
  (let* ((threads (call-with-input-file (make-path *sexp* board "list") read))
         (t (lookup-def (string->number thread) threads))
         (old-count (assq 'messages t))
         (old-date (assq 'date t)))
    (set-cdr! old-count post-count)
    (set-cdr! old-date (string-append date " *"))
    (call-with-output-file
      (make-path *sexp* board "list")
        (lambda (port) (write threads port)))))

(define (update-thread-list board thread date post-count)
  (let ((cache (make-path *html* board "list")))
    (if (file-exists? cache) (delete-file cache)))
  (let* ((threads (call-with-input-file (make-path *sexp* board "list") read))
         (headline (lookup-def 'headline (cdr (assv thread threads)))))
    (call-with-output-file 
      (make-path *sexp* board "list")
        (lambda (port)
          (write
             (cons `(,thread . ((headline . ,headline) (date . ,date) (messages . ,post-count)))
                   (del-assv thread threads))
             port)))))

(define (update-frontpage board)
  (let ((cache (make-path *html* board "index")))
    (if (file-exists? cache) (delete-file cache)))
  (let* ((threads (call-with-input-file (make-path *sexp* board "list") read))
         (top-threads (if (> (length threads) *frontpage-threads*)
                          (take threads *frontpage-threads*)
                          threads)))
    (with-output-to-file 
      (make-path *sexp* board "index")
      (lambda () 
        (write 
          (map
            (lambda (t)
              (let ((path (make-path *sexp* board (number->string (car t))))
                    (headline (lookup-def 'headline (cdr t))))
                `(,(car t) . ,(alist-cons 'headline headline (latest-posts path)))))
            top-threads))))))

(define (latest-posts path)
  (let* ((thread (call-with-input-file path read))
        (posts (lookup-def 'posts thread)))
    (if (> (length posts) 6)
        `((truncated . ,#t) (posts  . (,(cons (car posts) (take-right posts 5)))))
        `((truncated . ,#f) (posts . (,posts))))))

(define (post-thread board req query-string)
  (cond ((file-exists? (make-path *sexp* board))
         (let* ((list-path (make-path *sexp* board "list"))
                (index-path (make-path *sexp* board "index"))
                (threads (if (file-exists? list-path)
                             (call-with-input-file list-path read)
                             '()))
                (body (http-request-body req))
                (params (parameters->alist body))
                (message (decode-formdata (lookup-def 'epistula params)))
                (headline (decode-formdata (lookup-def 'titulus params)))
                (date (get-date))
                (validation (validate-form params message headline)))
           (cond ((eq? validation 'ok)
                  (let* ((thread-number (get-next-thread-number threads))
                         (path (make-path *sexp* board (number->string thread-number)))
                         (sxml (markup->sxml message board (number->string thread-number))))
                    (cond ((null? sxml) bad-request)
                          (else
                    (create-thread path headline date sxml)
                    (add-thread-to-list list-path board threads thread-number headline date)
                    (add-thread-to-index (make-path *sexp* board "index")
                                         board
                                         thread-number
                                         headline
                                         date
                                         sxml)
                    (redirection board (number->string thread-number) "1" query-string #t #t)))))
                 ((eq? validation 'spam)
                  `(200 () "SNAFU."))
                 (else (retry-thread-form validation board params)))))
        (else not-found)))

(define (create-thread path headline date sxml)
  (with-output-to-file
    path
    (lambda ()
      (write `((headline . ,headline)
               (posts ((1 (date . ,date) (vip . #f) (content . ,sxml)))))))))

(define (add-thread-to-list path board threads thread-number headline date)
  (let ((cache (make-path *html* board "list")))
    (if (file-exists? cache) (delete-file cache)))
  (let ((thread `(,thread-number (headline . ,headline) (date . ,date) (messages . 1))))
    (with-output-to-file
      path
      (lambda ()
        (write (cons thread threads))))))

(define (add-thread-to-index path board thread-number headline date sxml)
  (let ((cache (make-path *html* board "index")))
    (if (file-exists? cache) (delete-file cache)))
  (let ((threads (if (file-exists? path) (call-with-input-file path read) '()))
        (thread `(,thread-number
                   (headline . ,headline)
                   (truncated . #f)
                   (posts ((1 (date . ,date) (vip . #f) (content . ,sxml)))))))
    (with-output-to-file
      path
      (lambda ()
        (write 
          (if (< (length threads) *frontpage-threads*)
              (cons thread threads)
              (cons thread (take threads (dec *frontpage-threads*)))))))))

(define (get-next-thread-number threads)
  (if (null? threads)
      1
      (inc (apply max (map car threads)))))


(define (validate-form params message #!optional headline)
  (let ((fake-message (lookup-def 'message params ""))
        (fake-name (lookup-def 'name params ""))
        (hash (lookup-def 'ornamentum params "")))
    (cond ((and (not (default-object? headline)) (string-null? headline))
           '(empty-headline . "New threads must have a headline"))
          ((string-null? message)
           '(empty-message . "Empty post"))
          ((and (not (default-object? headline)) (> (string-length headline) *max-headline-size*))
           `(headline-too-long . (string-append "Headline too long (max: " ,(number->string *max-headline-size*) " bytes)")))
          ((> (string-length message) *max-post-size*)
           `(message-too-long . (string-append "Your post is too long (max: " ,(number->string *max-post-size*) " bytes)")))
          ((not (and (string-null? fake-message)
                     (string-null? fake-name)))
           'spam)
          (else 'ok))))

(define (retry-thread-form validation board params)
  (let ((headline (lookup-def 'titulus params ""))
        (message (lookup-def 'epistula params "")))
    (make-response (retry-thread-template
                     board 
                     (decode-formdata headline)
                     (decode-formdata message)
                     (cdr validation)))))

(define (retry-post-form validation board thread frontpage? params)
  (let ((message (lookup-def 'epistula params "")))
   (make-response (retry-post-template
                    board
                    thread
                    frontpage?
                    (decode-formdata message)
                    (cdr validation)))))


(listen server (string->number (car (command-line))))
