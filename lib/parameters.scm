;(load "strings.scm")
;load irregex

(define (parameter->pair p)
  (let ((l (string-split p #\=)))
    (cond ((= (length l) 2)
           (cons (string->symbol (car l)) (cadr l)))
          ((= (length l) 1)
           (list (string->symbol (car l)) ""))
          (else
           '(undef)))))

(define (parameters->alist p)
  (map parameter->pair (string-split p #\&)))

(define (decode-formdata s)
  (url:decode-string (irregex-replace/all (irregex "\\+") s "%20")))



