--- mit-scheme-9.2/src/runtime/httpio.scm.orig	2014-05-17 11:10:05.000000000 +0200
+++ mit-scheme-9.2/src/runtime/httpio.scm	2020-03-01 23:34:36.917758542 +0100
@@ -214,21 +214,24 @@
 (define (read-simple-http-response port)
   (make-simple-http-response (%read-all port)))
 
+
 (define (read-http-request port)
   (%text-mode port)
   (let ((line (read-line port)))
     (if (eof-object? line)
-	line
-	(receive (method uri version)
-	    (parse-line parse-request-line line "HTTP request line")
-	  (let ((headers (read-http-headers port)))
-	    (let ((b.t
-		   (or (%read-chunked-body headers port)
-		       (%read-delimited-body headers port)
-		       (%no-read-body))))
-	      (make-http-request method uri version
-				 (append! headers (cdr b.t))
-				 (car b.t))))))))
+        line
+        (receive (method uri version)
+            (parse-line parse-request-line line "HTTP request line")
+          (let ((headers (read-http-headers port)))
+            (let ((b.t
+                   (or (%read-chunked-body headers port)
+                       (%read-delimited-body headers port)
+                       '())))
+              (if (null? b.t)
+                  (make-http-request method uri version headers "")
+                  (make-http-request method uri version
+                                 (append! headers (cdr b.t))
+                                 (car b.t)))))))))
 
 (define (read-http-response request port)
   (%text-mode port)
@@ -327,15 +330,14 @@
 
 (define parse-request-line
   (*parser
-   (seq (map string->symbol
-	     (match (+ (char-set char-set:http-token))))
-	" "
-	(alt (map intern (match "*"))
-	     parse-absolute-uri
-	     parse-uri-path-absolute
-	     parse-uri-authority)
-	" "
-	parse-http-version)))
+   (seq (match (+ (char-set char-set:http-token)))
+        " "
+        (alt (map intern (match "*"))
+             parse-uri
+             parse-uri-authority)
+        " "
+        parse-http-version)))
+
 
 (define parse-response-line
   (*parser
