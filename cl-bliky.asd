(in-package #:cl-user)

(defpackage #:cl-bliky-system (:use #:cl #:asdf))

(in-package #:cl-bliky-system)

(asdf:defsystem cl-bliky
  :name   "cl-bliky"
  :author "Fons Haffmans; fons.haffmans@gmail.com"
  :version "0.0.1"
  :licence "See COPYING"
  :description "yet another lisp blog engine"
  :depends-on (:hunchentoot
	       :elephant
	       :cl-html-parse
	       :htmlgen
	       :cl-markdown
	       :html-template)
  :serial t
  :components 
  ((:module "src"
    :serial t
    :components ((:file "packages")
		 (:file "bliky")))
   (:static-file "README.md")
   (:static-file "COPYING")))









