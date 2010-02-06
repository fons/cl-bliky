(in-package #:cl-user)

(defpackage #:cl-bliky
  (:use #:common-lisp 
	#:sb-ext
	#:net.html.parser 
	#:net.html.generator 
	#:cl-markdown 
	#:hunchentoot 
	#:html-template 
	#:elephant)
  (:shadow :html)
  (:export
   :blog-post
   :stop-server
   :start-server
   :*BLIKYHOME*
   ))
