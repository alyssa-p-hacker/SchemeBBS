
(define doctype "HTML PUBLIC \"ISO/IEC 15445:2000//DTD HyperText Markup Language//EN\"")

(define (main-template title page #!optional class)
  `((doctype ,doctype)
    (html
     (head (title ,title)
           (meta (@ (content "text/html; charset=UTF-8") (http-equiv "Content-Type")))
           (meta (@ (name "viewport") (content "width=device-width, initial-scale=1.0")))
           (link (@ (rel "icon") (href "/static/favicon.ico") (type "image/png")))
           (link (@ (href "/static/styles/default.css") (rel "stylesheet") (type "text/css"))))
     ,(if (default-object? class)
     `(body ,page)
     `(body (@ (class "thread")) ,page))
     )))

(define (make-menu board selected)
  (let ((menu-items '("frontpage"  "thread list" "new thread" "preferences" "?")))
    `((p (@ (class "nav"))
       ,(if (equal? selected "frontpage")
           "frontpage"
            `(a (@ (href ,(make-abs-path board))) "frontpage"))
       " - "
       ,(if (equal? selected "thread list")
            "thread list"
           `(a (@ (href ,(make-abs-path board "list"))) "thread list"))
       " - "
       ,(if (equal? selected "frontpage")
            `(a (@ (href "#newthread")) "new thread")
            `(a (@ (href ,(string-append "/" board "#newthread"))) "new thread"))
       " - "
       ,(if (equal? selected "preferences")
           "preferences"
            `(a (@ (href ,(make-abs-path board "preferences"))) "preferences"))
       " - "
       (a (@ (href "http://textboard.org")) "?")))))
 
(define (make-post-form board thread frontpage #!optional content flash)
  (let ((form
          `((form (@ (action ,(make-abs-path board thread "post")) (method "post"))
                  (p (textarea (@ (name "epistula") (rows "8") (cols "78"))
                               ,(if (default-object? content)
                                    ""
                                    content))
                     (br)
                     "VIP:"
                     (input (@ (type "checkbox") (name "vip"))) " "
                     (input (@ (type "hidden") (name "frontpage") (value ,frontpage)))
                     (input (@ (type "hidden") (name "ornamentum") (value ,(get-form-hash))))
                     (input (@ (type "submit") (value "POST")))) 
                  (fieldset (@ (class "comment"))
                            (legend "do not edit these")
                            (p
                            (input (@ (type "text") (name "name") (class "name") (size "11")))
                            (br)
                            (textarea (@ (name "message") (class "message")(rows "1") (cols "11")))))
                  ))))
    (if (default-object? flash)
        form
        (append-element form `((p (@ (class "flash")) ,flash))))))


(define (make-thread-form board #!optional headline content flash)
  (let ((form
  `((h2 (@ (id "newthread")) "New Thread")
    (form (@ (action ,(make-abs-path board "post")) (method "post"))
            (p (@ (class "newthread")) (label (@ (for "titulus")) "Headline")
               (br)
               (input (@ (type "text") (name "titulus") (id "titulus") (size "78") (maxlength "78")
                         (value ,(if (default-object? headline)
                                     ""
                                     headline))))
               (br)
               (label (@ (for "epistula")) "Message")
               (br)
               (textarea (@ (name "epistula") (id "epistula") (rows "12") (cols "77"))
                         ,(if (default-object? content)
                              ""
                              content))
               (input (@ (type "hidden") (name "ornamentum") (value ,(get-form-hash))))
               (br)
               (input (@ (type "submit") (value "POST"))))
               (fieldset (@ (class "comment"))
               (legend "do not edit these")
               (p (input (@ (type "text") (name "name") (class "name") (size "11")))
               (br)
               (textarea (@ (name "message") (class "message")(rows "1") (cols "11")))))
               ))))
    (if (default-object? flash)
        form
        (append-element form `((p (@ (class "flash")) ,flash))))
    ))

(define (checked? value query-string-list)
  (if (equal? value (lookup-def 'css query-string-list ""))
      `((checked "checked"))
      `()))


(define (preferences-view board query-string-list)
  `((h1 ,board) ,(make-menu board "preferences")
    (hr)
	(h2 "Settings")
    (dl (dt (b "Style Sheets"))
    (dd
      (p "The CSS below will be stored in the URL as a query string. "
         "This BBS doesn't set HTTP cookies and won't remember you next time you visit. "
         "So the only way to store this setting is to save that URL.")
    (form (@  (action ,(make-abs-path board "preferences")) (method "get"))
      (p
          (input ,(append `(@ (type "radio") (name "css") (id "default") (value "default"))
			  (if (null? query-string-list)
			      `((checked "checked"))
                          	(checked? "default" query-string-list))))
          (label (@ (for "default")) "default")
          (br)
          (input ,(append `(@ (type "radio") (name "css") (id "mona") (value "2ch"))
                          (checked? "2ch" query-string-list)))
          (label (@ (for "mona")) "2ch")
          (br)
          (input ,(append `(@ (type "radio") (name "css") (id "no") (value "no"))
                          (checked? "no" query-string-list)))
          (label (@ (for "no")) "no")
          (br)
          (input (@ (type "submit") (value "SET!"))))))
	
    
    (dt (b "Userscripts"))
    (dd
    (p 
      "This site doesn't use Javascript. "
      "If you know what you're doing you can take the responsability "
      "to inject some yourself with extensions like " 
       (a (@ (href "https://www.greasespot.net/")) "Greasemonkey")
       " or "(a (@ (href "https://tampermonkey.net/")) "Tampermonkey")
       ". Below are some examples to get you started: an extension for syntax highlighting (uses "
       (a (@ (href "https://highlightjs.org/")) "highlight.js)")
       " and a simple word filter to replace profanities with what you want.")
       (ul 
	  (li (a (@ (class "static") (href "/static/userscripts/highlight.user.js")) "Syntax Highlighting"))
	  (li (a (@ (class "static") (href "/static/userscripts/wordfilter.user.js")) "Word Filter"))
	  (li (a (@ (class "static") (href "/static/userscripts/unvip.user.js")) "unVIP: (order the thread list by last updates)"))
	  (li (a (@ (class "static") (href "/static/userscripts/localjump.user.js")) "localjump (jump to linked posts by anchors, Futaba-style)"))
	  ))
    (dt (b "Clients"))
    (dd
    (p 
      "zge wrote a slick mode for posting from Emacs, let's all " (code "M-x sbbs") ": "
       (a (@ (href "https://git.sr.ht/~zge/sbbs")) "sbbs.el"))))
    (hr)
    ,footer
   ))


(define (thread-view board thread posts headline filter-func)
  `((h1 ,board)
    ,(make-menu board "thread view")
    (hr)
    ,(format-thread board thread posts headline filter-func "false")
    ,footer))

(define (format-thread board thread posts headline filter-func frontpage #!optional truncated)
  (let ((next-post-number (+ 1 (car (last posts)))))
  `((h2 (a (@ (href ,(make-abs-path board thread))) ,headline))
    (dl ,(if (default-object? truncated)
             (filter-map (lambda (p) (and (filter-func p) (format-post board thread p))) posts)
             (list (format-post board thread (car posts))
                   (add-stub (dec (caadr posts)) board thread)
                   (map (lambda (p) (format-post board thread p)) (cdr posts))))
        (dt (a (@ (href ,(string-append "#t" thread "p" (number->string next-post-number)))
                  (id ,(string-append "t" thread "p" (number->string next-post-number)))) ,next-post-number))
        (dd ,(make-post-form board thread frontpage)))
    (hr)
        )))

(define (format-post board thread post)
  `((dt (a (@ (href ,(string-append "/" board "/" thread "/" (number->string (car post))))
			  (id ,(string-append "t" thread "p" (number->string (car post)))))
		   ,(car post))
		" "
		(samp ,(lookup-def 'date (cdr post))
			  ,(if (lookup-def 'vip (cdr post)) " *" "") )) 
	(dd ,(lookup-def 'content (cdr post)))))

(define (add-stub n board thread)
  `((dt (a (@ (href ,(string-append "/" board "/" thread "#t" thread "p2"))
                    (id ,(string-append "t" thread "p" "2"))) 2)
              " … "
              ,(if (> n 2)
              `(a (@ (href ,(string-append "/" board "/" thread "#t" thread "p" (number->string n)))
                    (id ,(string-append "t" thread "p" (number->string n))))
                 ,(number->string n))
              ""))
    (dd (p "")))); (samp ,(string-append (number->string (- n 1)) " posts omitted") )))))

(define (list-view board threads)
  `((h1 ,board)
    ,(make-menu board "thread list")
    (hr)
    (table (@ (summary "Thread list"))
          (thead (tr (th "#") (th "headline") (th "posts") (th "last update")))
          (tbody ,(output-table threads)))
    (hr)
    ,footer))

(define (output-table threads)
  (map (lambda (thread)
         `(tr (td ,(car thread))
              (td (a (@ (href ,(car (cadr thread)))) ,(lookup-def 'headline (cdr (cadr thread)))))
              (td ,(lookup-def 'messages (cdr (cadr thread) )))
              (td (samp ,(lookup-def 'date (cdr (cadr thread) ))))))
       (zip (iota (+ (length threads) 1) 1) threads)))


(define (frontpage-view board threads)
  `((h1 ,board)
    ,(make-menu board "frontpage")
    (hr)
    ;,(if (not (null? threads))
    ,(let ((count 0))
       (map
         (lambda (t)
           (let ((thread (number->string (car t)))
                 (posts (lookup-def 'posts (cdr t)))
                 (headline (lookup-def 'headline (cdr t)))
                 (truncated (lookup-def 'truncated (cdr t))))
             (set! count (inc count))
             (cons 
               (make-jump-links count)
               (if truncated
                   (format-thread board thread (cons (car posts) (take-right posts 5)) headline identity "true" #t)
                   (format-thread board thread posts headline identity "true")))))
         threads))
    ,(make-thread-form board)
    (hr)
    ,footer))

(define (make-jump-links count)
  `((pre (@ (class "jump"))
         (a (@ (id ,(string-append "d" (number->string count)))
               (href ,(if (= count 10)
                          "#d1"
                          (string-append "#d" (number->string (inc count))))))
            "↓")
         (raw "&nbsp;")
         (a (@ (id ,(string-append "u" (number->string count)))
               (href ,(if (= count 1)
                          "#u10"
                          (string-append "#u" (number->string (dec count))))))
            "↑"))))


(define (success board query-string #!optional thread)
  `((html
      (head
        (meta (@ (http-equiv "refresh")
                 (content ,(string-append "1; url=" 
                                          (add-query-string (make-abs-path board) query-string))))))
      (body (h1 "That was VIP quality!" )))))


(define footer
  '(p (@ (class "footer")) "bbs.scm + "
              (a (@ (href "https://www.gnu.org/software/mit-scheme/")) "MIT Scheme") " + " 
             (a (@ (href "https://mitpress.mit.edu/sites/default/files/sicp/index.html")) "SICP")
             " + Satori Mode"))

