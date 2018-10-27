;;; misc

(define (inc n)
  (+ n 1))

(define (dec n)
  (- n 1))

(define (identity x)
  x)

;;; prints the given string to stdout, see
;;; https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Format.html for format arguments.
;;; adds a newline to the string, unlike regular printf.
;;; (load-option 'format)
(define (printf str . elements)
  (apply format #t (string-append str "~%") elements))


;;
;;   partial application, composition
;;

(define (compose . list-of-functions)
  (define (compose-funcs list-of-functions x)
    (if (null? list-of-functions)
        x
        (compose-funcs (cdr list-of-functions) ((car list-of-functions) x))))
  (lambda (x) (compose-funcs list-of-functions x)))

(define (partial fun . args)
  (lambda x (apply fun (append args x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                   ;;
;;                       strings utilities                           ;;
;;                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; convert a character to its hex representation
(define (char->hex c)
  (number->string (char->ascii c) 16))


; Splits the input string 'str into a list of strings
; based on the delimiter character 'ch
; © (Doug Hoyte, hcsw.org)
(define (string-split str ch)
  (let ((len (string-length str)))
    (letrec
      ((split
         (lambda (a b)
           (cond
             ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
             ((char=? ch (string-ref str b))
              (if (= a b)
                  (split (+ 1 a) (+ 1 b))
                  (cons (substring str a b) (split b b))))
             (else (split a (+ 1 b)))))))
      (split 0 0))))

;;; this version of string-split doesn't trim the leading separators
;;; (string-split "/usr/local/bin") => ("" "usr" "local" "bin")
(define (string-split* sep str)
  (define (f cs xs) (cons (list->string (reverse cs)) xs))
  (let loop ((ss (string->list str)) (cs '()) (xs '()))
    (cond ((null? ss) (reverse (if (null? cs) xs (f cs xs))))
          ((char=? (car ss) sep) (loop (cdr ss) '() (f cs xs)))
          (else (loop (cdr ss) (cons (car ss) cs) xs)))))


; joins a list of strings with the separator sep
; (join '() "/") => ""
; (join '("a" "b" "c") => "a/b/c"
; (join '("a") => "a"
(define (string-join lst sep)
  (letrec
    ((join
       (lambda (l res)
         (if (null? l)
             res
             (join (cdr l) (string-append res sep (car l)))))))
     (if (null? lst)
         ""
         (join (cdr lst) (string-append (car lst))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                   ;;
;;                       lists utilities                             ;;
;;                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (append-element l . e)
  (append l e))

(define (flatten l)
  (letrec
    ((flat
       (lambda (l acc rest)
         (cond ((null? l)
                (if (null? rest)
                    (reverse acc)
                    (flat (car rest) acc (cdr rest))))
               ((pair? (car l))
                (flat (car l) acc (if (null? (cdr l))
                                      rest
                                      (cons (cdr l) rest))))
               (else
                 (flat (cdr l) (cons (car l) acc) rest))))))
    (flat l '() '())))

(define (list-intersperse src-l elem)
  (if (null? src-l) src-l
      (let loop ((l (cdr src-l)) (dest (cons (car src-l) '())))
        (if (null? l) (reverse dest)
            (loop (cdr l) (cons (car l) (cons elem dest)))))))
 
;;; output stays reversed
(define (rlist-intersperse src-l elem)
  (if (null? src-l) src-l
      (let loop ((l (cdr src-l)) (dest (cons (car src-l) '())))
        (if (null? l) dest
            (loop (cdr l) (cons (car l) (cons elem dest)))))))

;;; map that doesn't reverse its output 
(define (rmap proc l)
  (letrec
    ((maprec
       (lambda (lst res)
         (if (null? lst)
             res
             (maprec (cdr lst) (cons (proc (car lst)) res))))))
    (maprec l '())))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                   ;;
;;                      alists utilities                             ;;
;;                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Look up a value associated with a symbolic key in alist 
; ((key value) ...) or ((key . value) ...)
; and return the associated value.
; If the association has the form
;   (key . value) where value is not a pair --> return value
;   (key   value)                           --> return value
;   (key value1 value2 value3 ...) -> return (value1 value2 value3 ...)
; that is, the procedure tries to do the right thing for
; both kinds of associative lists. 
;
; The form `lookup-def' is a special form rather than a regular
; procedure. Its first two arguments are evaluated exactly once. The
; default-value argument, if given, is evaluated only if the desired key
; is not found. I have not seen any need to pass `lookup-def' as an
; argument to other functions. If the latter is desired, it is not
; difficult to accomplish by explicitly wrapping `lookup-def' into a
; lambda form.
;
; We use a pseudo-keyword argument warn: as a modifier.
; This is not really a keyword argument (although it may be,
; if the Scheme system turns out DSSSL-compatible)
; 
; (lookup-def key alist)  -- lookup the key in the alist and return the
;                        associated value. Raise an error if the key is not
;                        found.
; (lookup-def key alist default-exp)
;                     -- lookup the key in the alist and return the associated
;                        value. If the the key is not found, evaluate
;                        the default-exp and return its result.
; (lookup-def key alist warn: default-exp)
;                     -- the same as above. In addition, write a warning
;                        (using cerr above) if the key is not found.
;
; © Oleg Kiselyov, http://okmij.org/ftp/Scheme/lib/myenv-chez.scm
(define-syntax lookup-def 
  (syntax-rules (warn:)
    ((lookup-def key alist)
     (let ((nkey key) (nalist alist)) ; evaluate them only once
       (let ((res (assq nkey nalist)))
         (if res
             (let ((res (cdr res)))
               (cond
                 ((not (pair? res)) res)
                 ((null? (cdr res)) (car res))
                 (else res)))
             (error "Failed to find " nkey " in " nalist)))))
    ((lookup-def key alist default-exp)
     (let ((res (assq key alist)))
       (if res
           (let ((res (cdr res)))
             (cond
               ((not (pair? res)) res)
               ((null? (cdr res)) (car res))
               (else res)))
           default-exp)))
    ((lookup-def key alist warn: default-exp)
     (let ((nkey key) (nalist alist)) ; evaluate them only once
       (let ((res (assq nkey nalist)))
         (if res
             (let ((res (cdr res)))
               (cond
                 ((not (pair? res)) res)
                 ((null? (cdr res)) (car res))
                 (else res)))
             (begin
               (cerr "Failed to find " nkey " in " nalist #\newline)
               default-exp)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                   ;;
;;                      pattern matching                             ;;
;;                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; A simple linear pattern matcher
; It is efficient (generates code at macro-expansion time) and simple:
; it should work on any R5RS Scheme system.
;
; It was first developed for the leanTAP theorem prover in miniKanren.
; It has been in the miniKanren repository
;    http://kanren.sf.net/viewvc/kanren/kanren/mini/leanTAP.scm?view=log
; since August 2005.
;
; See the above code for the example of using match:
; transforming a first-order logic formula to the Negation Normal Form.


; (match exp <clause> ...[<else-clause>])
; <clause> ::= (<pattern> <guard> exp ...)
; <else-clause> ::= (else exp ...)
; <guard> ::= boolean exp | ()
; <pattern> :: =
;        ,var  -- matches always and binds the var
;                 pattern must be linear! No check is done
;         _   -- matches always
;        'exp  -- comparison with exp (using equal?)
;        exp   -- comparison with exp (using equal?)
;        (<pattern1> <pattern2> ...) -- matches the list of patterns
;        (<pattern1> . <pattern2>)  -- ditto
;        ()    -- matches the empty list

; In the original version, the always-matching pattern was specified
; as a simple underscore. That does not work in R6RS which reserves
; the underscore. Therefore, the always-matching pattern is changed
; to two underscores.
;
; © Oleg Kiselyov, http://okmij.org/ftp/Scheme/match-case-simple.scm

(define-syntax match
  (syntax-rules ()
    ((__ exp clause ...)
      (let ((val-to-match exp))
 (match* val-to-match clause ...)))))

(define (match-failure val)
  (error "failed match" val))

(define-syntax match*
  (syntax-rules (else)
    ((__ val (else exp ...))
      (let () exp ...))
    ((__ val)
      (match-failure val))
    ((__ val (pattern () exp ...) . clauses)
      (let ((fail (lambda () (match* val . clauses))))
   ; note that match-pattern may do binding. Here,
   ; other clauses are outside of these binding
 (match-pattern val pattern (let () exp ...) (fail))))
    ((__ val (pattern guard exp ...) . clauses)
      (let ((fail (lambda () (match* val . clauses))))
 (match-pattern val pattern
   (if guard (let () exp ...) (fail))
   (fail))))
))

; (match-pattern val pattern kt kf)
(define-syntax match-pattern
  (syntax-rules (_ quote unquote)
    ((__ val _ kt kf) kt)
    ((__ val () kt kf)
      (if (null? val) kt kf))
    ((__ val (quote lit) kt kf)
      (if (equal? val (quote lit)) kt kf))
    ((__ val (unquote var) kt kf)
      (let ((var val)) kt))
    ((__ val (x . y) kt kf)
      (if (pair? val)
 (let ((valx (car val))
       (valy (cdr val)))
   (match-pattern valx x
     (match-pattern valy y kt kf)
     kf))
 kf))
    ((__ val lit kt kf)
      (if (equal? val (quote lit)) kt kf))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                   ;;
;;                      date utilities                               ;;
;;                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; returns a timestamp in Unix time
(define (get-timestamp)
  (universal-time->file-time (get-local-time)))

;; returns the current date in standard format
(define (get-date)
  (define (pad n)
    (if (< n 10)
        (string-append "0" (number->string n))
        (number->string n)))
  (let ((t (universal-time->global-decoded-time (get-universal-time))))
    (call-with-output-string
      (lambda (port)
        (format port "~A-~A-~A ~A:~A" 
                (vector-ref t 6)
                (pad (vector-ref t 5))
                (pad (vector-ref t 4))
                (pad (vector-ref t 3))
                (pad (vector-ref t 2)))))))


