;(load "utils/irregex/irregex")

(define markup->sxml)
(let ()

;;; global vars, needed for quotelinks
(define *board* #f)
(define *thread* #f)

;;; utils

(define (empty-line? line)
  (or (string-null? line) (irregex-match '(* white) line)))


(define (add-line-to-block blocks line tag)
  (if (null? blocks)
      `((,tag ,line))
      (let ((last-block-tag (caar blocks))
            (last-block-content (cdar blocks))
            (other-blocks (cdr blocks)))
        (cond ((eq? last-block-tag tag)
               (cons (cons tag (cons line last-block-content)) other-blocks))
              ((or (null? last-block-content) (for-all? last-block-content empty-line?))
               (cons `(,tag ,line) other-blocks))
              (else
                (cons `(,tag ,line) blocks))))))

(define (add-empty-tag blocks tag)
  (if (null? blocks)
      `((,tag))
      (if (or (null? (cdar blocks)) (for-all? (cdar blocks) empty-line?))
          (cons `(,tag) (cdr blocks))
          (cons `(,tag) blocks))))

(define (block-scanner s)
  (define (scan-rec l res state)
    (if (null? l)
        (check-blankpost res)
        (let ((line (car l)) (rest (cdr l)))
          (if (equal? state 'pre)
              (if (string-prefix? "```" line)
                  (scan-rec rest res 'undef)
                  (scan-rec rest (add-line-to-block res line 'pre) 'pre))
              (cond ((string-prefix? "```" line)
                     (scan-rec rest (add-empty-tag res 'pre) 'pre))
                    ((and (string-prefix? ">" line) (not (irregex-match (irregex "^>>[0-9].*") line)))
                     (scan-rec rest (add-line-to-block res (string-tail line 1) 'blockquote) 'blockquote))
                    ((empty-line? line)
                     (if (eq? state 'undef)
                         (scan-rec rest res 'undef)
                         (scan-rec rest (add-empty-tag res 'p) 'undef)))
                    (else
                      (scan-rec rest (add-line-to-block res line 'p) 'p)))))))
  (let ((lines (string-split* #\newline (irregex-replace/all (irregex "\r\n") s "\n"))))
    (scan-rec lines '() 'undef)))

(define (check-blankpost blocks)
  (cond ((null? blocks)
         '())
        ((or (null? (cdar blocks))
             (for-all? (cdar blocks) empty-line?))
         (if (= (length blocks) 1)
             '()
             (cdr blocks)))
        (else blocks)))

(define (format-code codeblock)
  (define (join l res)
    (if (null?  l)
        res
        (join (cdr l) (string-append (car l) "\n" res))))
  (let ((lines (cdr codeblock)))
    `(pre (code ,(join (cdr lines) (car lines))))))

;;;

(define (format-blockquote bq)
  (define (make-paragraphs l res)
    (cond ((null? l) (if (for-all? (cdar res) empty-line?) (cdr res) res))
          ((and (empty-line? (car l)) (or (null? res) (null? (cdar res))))
           (make-paragraphs (cdr l) res))
          ((empty-line? (car l))
           (make-paragraphs (cdr l) (add-empty-tag res 'p)))
          (else
            (make-paragraphs (cdr l) (add-line-to-block res (car l) 'p)))))
  (cons 'blockquote (map format-paragraph (make-paragraphs (cdr bq) '()))))
#|
(define (format-blockquote bq)
(list ('blockquote (list 'p (rlist-intersperse (cdr bq) '(br))))))
|#

(define (format-paragraph paragraph)
  (cons 'p (line-scanner (list-intersperse (cdr paragraph) '(br)))))

;;; reverse its input
(define (rformat-paragraph paragraph)
  (cons 'p (line-scanner (rlist-intersperse (cdr paragraph) '(br)))))


(define (string->sxml markup s)
  (define (string->sxml-rec s res)
    (let ((match (irregex-search (regex markup) s)))
      (cond ((string-null? s)
             res)
            ((not match)
             (append-element res s))
            (else 
              (let* ((start (irregex-match-start-index match))
                     (end (irregex-match-end-index match))
                     (substr (irregex-match-substring match))
                     (s1 (substring s 0 start))
                     (s2 (substring s end (string-length s))))
                (if (string-null? s1)
                    (string->sxml-rec
                      s2
                      (append-element res ((transform markup) substr)))
                    (if (and (eq? (name markup) 'del) ;; exception to escape spoiler inside code
                             (between-code? s1 s2))
                        (string->sxml-rec "" (append-element res (string-append s1 substr s2)))
                        (string->sxml-rec
                          s2
                          (append-element res s1 ((transform markup) substr))))))))))
  (string->sxml-rec s '()))

;; edge false positive (between-code? "==code== ==code==" "==")
;; could add another pass of spoiler, but ok good-enough
(define (between-code? s1 s2)
  (let ((m1 (irregex-search (irregex ".*==$|.*==[^ ]") s1))   ;opening code in s1
        (m2 (irregex-search (irregex ".*[^ ]==") s1))         ;closing code in s1
        (m3 (irregex-search (irregex "^==|.*?[^ ]==") s2))    ;closing code in s2
        (imei irregex-match-end-index))
    (if (and m1 m3 (or (not m2) (>= (imei m1) (imei m2))))
        #t
        #f)))

(define (lines->sxml markup l)
  (append-map (lambda (e) 
                (cond ((string? e)
                       (string->sxml markup e))
                      ((eq? (car e) 'del)
                       `(,(cons 'del (lines->sxml markup (cdr e)))))
                      (else `(,e))))
              l))


(define (transform-rule name regex transform)
  (define (dispatch op)
    (cond ((eq? op 'name) name)
          ((eq? op 'regex) regex)
          ((eq? op 'transform) transform)))
  dispatch)

(define (transform markup) (apply markup '(transform)))
(define (regex markup) (apply markup '(regex)))
(define (name markup) (apply markup '(name)))

(define bold
  (transform-rule
    'bold
    (irregex  "\\*\\*[^ ].*?[^ ]\\*\\*|\\*\\*[^ ]\\*\\*")
    (lambda (sub) `(b ,(substring sub 2 (- (string-length sub) 2))))))

(define italic
  (transform-rule
    'italic
    (irregex  "__[^ ].*?[^ ]__|__[^ ]__")
    (lambda (sub) `(i ,(substring sub 2 (- (string-length sub) 2))))))

(define code
  (transform-rule
    'code
    (irregex  "==[^ ].*?[^ ]==|==[^ ]==")
    (lambda (sub) `(code ,(substring sub 2 (- (string-length sub) 2))))))

(define del
  (transform-rule
    'del
    (irregex "~~[^ ].*?[^ ]~~|~~[^ ]~~")
    (lambda (sub) `(del ,(substring sub 2 (- (string-length sub) 2))))))

(define quotelink
  (transform-rule
    'quotelink
    (irregex ">>[1-9][0-9]{0,2}(-[1-9][0-9]{0,2})?(,[1-9][0-9]{0,2}(-[1-9][0-9]{0,2})?){0,20}")
    (lambda (sub) `(a (@ (href ,(string-append
                                  "/" *board*
                                  "/" *thread*
                                  "/" (string-tail sub 2))))
                         ,sub))))

(define link
  (transform-rule
    'link
    (irregex "https?:\/\/[^ \n]*")
    (lambda (sub) `(a (@ (href ,sub)) ,sub))))


(define line-scanner-order (list
  del code link quotelink bold italic))

(define (line-scanner l)
  ((apply compose (map (lambda (tr) (partial lines->sxml tr)) line-scanner-order)) l))

(set! markup->sxml
  (lambda (text board thread)
    (set! *board* board)
    (set! *thread* thread)
    (let ((blocks (block-scanner text)))
      (rmap (lambda (b)
              (case (car b)
                ((p) (rformat-paragraph b))
                ((blockquote) (format-blockquote b))
                ((pre) (format-code b))
                (else '())))
            blocks)))))
