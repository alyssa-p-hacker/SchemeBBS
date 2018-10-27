;;; sxml to html converter, this file is part of SchemeBBS
;;;
;;; Copyright © 2015  David Thompson <davet@gnu.org>
;;; Copyright © 2018  Florian Léger <florilege@protonmail.org> 
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; SXML to HTML conversion.
;; adapted from Guile to MIT Scheme
;; original: https://dthompson.us/rendering-html-with-sxml-and-gnu-guile.html
;;
;;; Code:

;(load-option 'format)

;;; SRFI 26 - Notation for Specializing Parameters without Currying
;;; https://github.com/scheme-requests-for-implementation/srfi-26
;;; (load "srfi/srfi-26.scm")

;;; simple linear pattern matcher
;;; http://okmij.org/ftp/Scheme/match-case-simple.scm
;;; (load "utils/match.scm")

(define sxml->html)

(let ()
;;; from Alex Shinn's match.scm
;;; http://synthcode.com/scheme/match.scm
  (define-syntax match-lambda
    (syntax-rules ()
      ((_ (pattern guard . body) ...) (lambda (expr) (match expr (pattern guard . body) ...)))))

;;; from SRFI 13 - String Librarie
;;; https://github.com/scheme-requests-for-implementation/srfi-13
  (define (string-for-each proc s)
    (define end (string-length s))
    (let loop ((i 0))
      (if (< i end)
	  (begin (proc (string-ref s i)) 
		 (loop (+ i 1))))))

  (define %self-closing-tags
    '(area
      base
      br
      col
      command
      embed
      hr
      img
      input
      keygen
      link
      meta
      param
      source
      track
      wbr))

  (define (self-closing-tag? tag)
    "Return #t if TAG is self-closing."
    (pair? (memq tag %self-closing-tags)))

  (define %escape-chars
    (alist->hash-table
     '((#\" . "quot")
       (#\& . "amp")
       (#\< . "lt")
       (#\> . "gt"))))

  (define (string->escaped-html s port)
    "Write the HTML escaped form of S to PORT."
    (define (escape c)
      (let ((escaped (hash-table/get %escape-chars c #f)))
	(if escaped
            (format port "&~a;" escaped)
            (display c port))))
    (string-for-each escape s))

  (define (object->escaped-html obj port)
    "Write the HTML escaped form of OBJ to PORT."
    (string->escaped-html
     (call-with-output-string (cut display obj <>))
     port))

  (define (attribute-value->html value port)
    "Write the HTML escaped form of VALUE to PORT."
    (if (string? value)
	(string->escaped-html value port)
	(object->escaped-html value port)))

  (define (attribute->html attr value port)
    "Write ATTR and VALUE to PORT."
    (format port "~a=\"" attr)
    (attribute-value->html value port)
    (display #\" port))

  (define (element->html tag attrs body port)
    "Write the HTML TAG to PORT, where TAG has the attributes in the
list ATTRS and the child nodes in BODY."
    (let* ((nl-before '(p div html head hr h1 h2 h3 h3 h5 h6
                          meta dl ul ol li dt dd pre table tr
                          th td link title script center blockquote
                          form address body thead tfoot tbody col colgroup fieldset))
           (nl-after '(br))
           (nlb? (or (and (memq tag nl-before) #\newline) ""))
           (nla? (or (and (memq tag nl-after) #\newline) "")))
      (format port "~a<~a" nlb? (string-upcase (symbol->string tag)))
      (for-each (match-lambda
                  ((,attr ,value) ()
                   (display #\space port)
                   (attribute->html attr value port)))
                attrs)
      (if (and (null? body) (self-closing-tag? tag))
          (format port ">~a" nla?)
          (begin
            (display #\> port)
            (for-each (cut sxml->html <> port) body)
            (format port "</~a>~a" (string-upcase (symbol->string tag)) nla?)))))

  (define (doctype->html doctype port)
    (format port "<!DOCTYPE ~a>" doctype))

  (set! sxml->html
	(lambda (tree #!optional port) 
	  "Write the serialized HTML form of TREE to PORT."
	  (let ((port (if (default-object? port) (current-output-port) port)))
	    (match tree
	      (() () '())
	      (('doctype ,type) ()
	       (doctype->html type port))
	      (('raw ,html) () ; Unescaped, raw HTML output
	       (display html port))
	      ((,tag (@ . ,attrs) . ,body) (symbol? tag)
	       (element->html tag attrs body port))
	      ((,tag . ,body) (symbol? tag)
	       (element->html tag '() body port))
	      ((,nodes . ,rest) ()
	       (for-each (cut sxml->html <> port) (list nodes rest)))
	      (,text (string? text)
		     (string->escaped-html text port))
	      (,obj () (object->escaped-html obj port))))))) ; Render arbitrary Scheme objects, too.
