diff -ur mit-scheme-9.2.orig/src/runtime/http-syntax.scm mit-scheme-9.2/src/runtime/http-syntax.scm
--- mit-scheme-9.2/src/runtime/http-syntax.scm.orig	2014-05-17 11:10:05.000000000 +0200
+++ mit-scheme-9.2/src/runtime/http-syntax.scm		2018-10-20 22:46:44.098379331 +0200
@@ -1310,8 +1310,13 @@
   write-entity-tag)
 
 (define-header "Location"
-  (direct-parser parse-absolute-uri)
-  absolute-uri?
+  (direct-parser
+   (*parser
+    (alt parse-absolute-uri
+	 parse-relative-uri)))
+  (lambda (value)
+    (and (uri? value)
+	 (not (uri-fragment value))))
   write-uri)
 #;
 (define-header "Proxy-Authenticate"
