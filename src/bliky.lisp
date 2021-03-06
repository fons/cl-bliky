;; -*- Mode : LISP; Syntax: COMMON-LISP; 
;;
;;  This softeware is Copyright (c) 2009 A.F. Haffmans 
;;
;;    This file is part of cl-bliky.
;;
;;    cl-bliky is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;;
;;    cl-bliky is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with cl-bliky.  If not, see <http://www.gnu.org/licenses/>.
;;
;;

(in-package :cl-bliky)

(defvar      MONTHS       '("xx" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(defvar      DAYS         '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defvar      TIMEZONE     '("1" "2" "3" "4" "5" "Est" "6") )
(defconstant DEFAULT-PORT 8050)
(defvar      DEFAULT-REMOTE-REPO-HOST "github.com")
(defvar      IMPORT-FAILED-MSG    "I was unable to convert this post back to html during the import")
(defvar      USRMODE (logior sb-posix:s-irusr sb-posix:s-ixusr sb-posix:s-iwusr))
(defvar      *bliky-server*        (not t))
(defvar      *BLIKY-HOME*           "location of the cl-bliky repository") 

;;format classes
;;controlled by the fmt-bit on the blog post object
;;
(defmacro html() `'html)
(defmacro md() `'md)

;;blog classes
;;controlled by the type-bit on the blog post object
;;
(defmacro post()      `'post) 
(defmacro about()     `'about)
(defmacro sidebar()   `'sidebar)

;; macro's used when reading in an uploaded file.
(defmacro <title>() `"<title>")
(defmacro <split>() `"<split>")


(defmacro main-repo-qs()    `"main-repo")
(defmacro sandbox-repo-qs() `"sandbox-repo")

;;This macro is used in 'clean to identify lines which
;; are end-of-line markers.
;; So two with (clean-ws-guard - 1) spaces and a clf
;; will result in an empty line.
;; This is the same as the markdown package expects; 
;;
(defmacro clean-ws-guard() `3)

;(setf (hunchentoot:log-file) "/tmp/error.file")
(setf hunchentoot:*message-log-pathname* "/tmp/error.file")

(defun filter(fun lst)
  (let ((l))
    (dolist (obj lst)
      (when (funcall fun obj) (push obj l)))
    (nreverse l)))

(defun after (Funs args)
  (labels ((after* (Funs args)
	     (if (null Funs)
		 args
		 (after (cdr Funs) (funcall (car Funs) args)))))
	   (after* (nreverse Funs) args)))

(defun str-strip(str)
  (string-trim '(#\Return #\Space #\Newline #\Tab #\Nul  ) str))

(defun rm-cr(x)
  (if (equal x #\Return) #\Space x))

(defun clean-str(str) 
  (map 'string #'rm-cr str))

(defun slurp-stream(stream)
  ;;from
  ;;www.emmett.ca/~sabetts/slurp.html
  ;;
  (let ((seq (make-string (file-length stream))))
    (read-sequence seq stream)
    seq))

(defun fmt-timestamp(st)
  ;; format rfc-822 style
  (multiple-value-bind 
	(second minute hour day month year dow dst-p tz)
      (decode-universal-time st)
    (declare (ignore dst-p))
    (format nil "~&~A, ~2,'0D ~A ~4D ~2,'0D:~2,'0D:~2,'0D ~A"
	    (nth dow DAYS)
	    day
	    (nth month MONTHS) 
	    year
	    hour 
	    minute 
	    second
	    (string-upcase (nth tz TIMEZONE)))))

(defun str2type(str) 
  (labels ((strip(str)
	     (string-trim '(#\Return #\Space #\Newline #\Tab #\Nul #\"  ) str)))
    (let ((s (strip str)))
      (cond ((eq     (not t)   s)    (post))
	    ((equalp "post"    s)    (post))
	    ((equalp "about"   s)    (about))
	    ((equalp "sidebar" s)    (sidebar))
	    ( t                     (post))))))

(defun string-split(s sep)
  (let ((l ())
	(str (string-trim '(#\Space) s)))
    (do ((pos  (position sep str :test #'string=)
	       (position sep str :test #'string=)))
	((or (null pos) (eql 0 pos)))
      (push (string-left-trim '(#\Space) (subseq str 0 pos)) l)
      (setf str (string-left-trim '(#\Space) (subseq str (+ pos 1) ))))
    (nreverse (cons str l))))

(defun string-replace*(sep new str)
  (let ((l ()))
    (do ((pos  (search sep str :test #'string=)
	       (search sep str :test #'string=)))
	((or (null pos) (eql 0 pos)))
      (push (subseq str 0 pos) l)
      (push new l)
      (setf str (subseq str (+ pos (length sep) ))))
    (nreverse (cons str l))))

(defun string-replace(sep new str)
  (let ((L (string-replace* sep new str)))
    (reduce (lambda (s1 s2) (concatenate 'string s1 s2)) L :initial-value "")))

(defun read-lines(file)
  (let ((rlines ()))
    (with-open-file (stream file :direction :input)
      (do ((line (read-line stream nil 'eof)
		 (read-line stream nil 'eof)))
	  ((eql line 'eof))
	(push line rlines)))
    rlines))

;;--------------basic config file---------------

(defun read-db-config(fn) 
  (labels ((!comment?(str) 
	     (if ( < 0 (length str) )
		 (not (char= (char str 0) #\#))
		 (not t))))
    (mapcar (lambda (s) (string-split s ":") )
	    (filter #'!comment? 
		    (mapcar #'str-strip 
			    (read-lines fn))))))

(defun find-db-config(key config)
  (cadr (find-if (lambda (s) (string= (car s) key) ) config)))

;;----string cleaning--------------------------------------
;; TODO : add eol char to wsb
;; this needs to be filtered out in the regular case before appending to the accumulator.
;; If an eol is encountered with an eol in the wsb then insert a break


(defun clean(s) 
  (labels ((clean-helper(s accum wsb eolf)
	     (labels ((str-car(s)
			(elt s 0))
		      ;;
		      (str-cdr(s);;
			(subseq s 1))
		      ;;
		      (cr-p(c)
			(char= #\Return c))
		      ;;
		      (nl-p(c)
			(char= #\Newline c))
		      ;;
		      (chr-p(c) 
			(and (not (cr-p c)) (not (nl-p c))))
		      ;;
		      (rln-st(str) 
			(chr-p (str-car str)))
		      ;;
		      (eol-st(str)
			(cr-p (str-car str)))
		      ;;
		      (neolf-st (wsb eolf) 
			(cond  
			  ( (and (> (length wsb) (clean-ws-guard)) (eq (count #\Space wsb) (length wsb)))    t)
			  ((not eolf)                                       t)
			  (t                                           (not t))))
		      ;;
		      (p-acc(s accum)
			(cons (str-car s) accum))
		      ;;
		      (p-acc-br(accum)
			(append (nreverse (coerce (string #\Newline) 'list)) accum))
		      ;;
		      (p-acc-br-alt(accum)
			(append (nreverse (coerce "<br /><br />" 'list)) accum))
		      ;;
		      (merge-wsb(wsb accum)
			(append (filter (lambda (c) (not (cr-p c))) wsb) accum))
		      ;;( (< (count #\Return ws) 1) (p-acc-br accum)) 
		      (p-acc-br-ws(accum ws)
			(cond 
			  ;; just an empty line -> insert <br />
			  ( (< (length ws) 1)                   (p-acc-br accum))
			  ( (eq (count #\Space ws) (length ws)) (p-acc-br-alt accum))
			  ( t                                   (merge-wsb ws accum)))))
	       (cond ((eq (length s) 0)                                      (p-acc-br-ws accum  wsb ))
		     ((neolf-st wsb eolf) (clean-helper (str-cdr s) accum                   (p-acc s wsb) nil ))
		     ((rln-st  s)         (clean-helper (str-cdr s) accum                   (p-acc s wsb) t   ))
		     ((eol-st  s)         (clean-helper (str-cdr s) (p-acc-br-ws accum wsb) nil           t   )) 
		     (t                   (clean-helper (str-cdr s) accum                   wsb         t ))))))
    (coerce (nreverse (clean-helper s nil nil t)) 'string)))

;;--> this is problamtic as <special char> is escaped to &<something>;
;;--> so you get into some infinite recursion !
;;((char= c #\&) "&amp;")
;;((char= c #\&) t)

(defun escape-html-chars(str &optional (start 0))
  ;; find the first char to be escaped, escape it, and return a list of the
  ;; position in the string plus the new string.
  ;; Repeat, until done, when the car of the list is nil
  (labels  ((escape-html-chars-helper(str start)
	      (labels ((escape-string(str) 
			 (labels ((escape-chars(c) 
				    ;; add additional chars to be escaped here...
				    (cond ((char= c #\<) "&lt;")
					  ((char= c #\>) "&gt;")
					  ( t             (string c)))))
			   (concatenate 'string (escape-chars (char str 0)) (subseq str 1))))
		       ;;
		       (escape-chars-pred(c) 
			 ;; add additional chars to be escaped here...
			 (cond ((char= c #\<) t)
			       ((char= c #\>) t)
			       ( t             (not t)))))
		;; if no chars to be escaped are found, return nil
		(let* ((npos (position-if #'escape-chars-pred (subseq str start)))
		       (epos (if npos (+ start npos) (not t) )) 
		       (nstr (if epos (concatenate 'string (subseq str 0 epos)  
						   (escape-string (subseq str epos)))
				 str)))
		  (list epos nstr)))))
    (let ((res (escape-html-chars-helper str start)))
      ;; recursion
      (cond ((null (car res)) (cadr res))
	    ( t               (escape-html-chars (cadr res) (car res)))))))
  

(defun to-list(parts)
  (if (listp parts)
      parts
      (list parts)))

;;--------------------------------------------------------------------------
(defpclass blog-post ()
  ((title :initarg :title
		   :accessor title)
   (intro :initarg :intro
		   :accessor intro)
   (body         :initarg  :body
		 :accessor body)
   (timestamp  :initarg  :timestamp
	       :accessor timestamp
	       :initform (get-universal-time)
	       :index t)
   (gc-bit     :accessor gc-bit
	       :index t
               :initform (not t))
   (pub-bit    :accessor pub-bit
	       :index t
               :initform (not t))
   (fmt-bit    :initarg :fmt-bit
               :accessor fmt-bit
               :index t)
   (type-bit   :initarg :type-bit
               :accessor type-bit
	       :index t)
   (url-part   :initarg :url-part
	       :accessor url-part
	       :initform nil
	       :index t)))

(defmethod print-object ((u blog-post) stream)
  "specialize print object blog post"
  (format stream "~&blog-post : [title = ~A] [url-part = ~A][time=~A][gc=~A][pub=~A]~%[type=~A][fmt=~A]~%[~A]~%[~A]~% "
	  (if (slot-boundp u 'title) 
	      (title u) 
	    "(blog-post not set)")
	  (if (slot-boundp u 'url-part)
	      (url-part u)
	    "(url-part not specified")
	  (if (slot-boundp u 'timestamp)
	      (timestamp u)
	    "(timestamp not set)")
	  (if (slot-boundp u 'gc-bit)
	      (gc-bit u)
	    "(gc not set)")
	  (if (slot-boundp u 'pub-bit)
	      (pub-bit u)
	    "(publish bit not set)")
	  (if (slot-boundp u 'type-bit)
	      (type-bit u)
	    "(type bit not set)")
	  (if (slot-boundp u 'fmt-bit)
	      (fmt-bit u)
	    "(format bit not set)")
	  (if (slot-boundp u 'intro)
	      (intro u)
	    "(intro not set)")
	  (if (slot-boundp u 'body)
	      (body u)
	    "(body not set)")))

;;
;; blog post is md unless specified otherwise
;;
(defun p-blogpost-html(blog-post)
  (cond ((equalp (fmt-bit blog-post) (html) )        t) 
	( t                                     (not t))))

(defun p-blogpost-md(blog-post)
  (cond ((not (p-blogpost-html blog-post))    t) 
	( t                              (not t))))

(defun render-post (part post)
  (cond ((p-blogpost-html post) (after (to-list part) post))
	((p-blogpost-md   post) (after (cons 'render-md  (to-list part)) post))
	(t                      (funcall part post))))

(defun render-identity (part post)
  (identity (funcall part post)))

(defun render-md(s)
  (with-output-to-string (stream)
    (markdown:markdown (clean s) :stream stream :format :html)))
;;
;;
;;--------------settings-----------------
;;
;;

(defun get-settings()
  (let ((eb (get-from-root 'blog-settings)))
    (if eb
	eb
	(progn
	  (add-to-root 'blog-settings (make-btree))
	  (get-from-root 'blog-settings)))))

(defun set-bliky-setting(k v)
  (setf (get-value k (get-settings)) v))

(defun get-bliky-setting(k)
  (get-value k (get-settings)))

(defun set-bliky-port(port)
  (labels ((conv-i(port)
	     (if (integerp port) port (parse-integer port)))) 
    (set-bliky-setting 'bliky-port (conv-i port))))

(defun get-bliky-port()
  (let ((val (get-bliky-setting 'bliky-port)))
    (if val
	val
	(set-bliky-port DEFAULT-PORT))))

(defun set-blog-title(title)
  (set-bliky-setting 'blog-title title))

(defun get-blog-title()
  (let ((val (get-bliky-setting 'blog-title)))
    (if val
	val
	(set-blog-title "blog title not set"))))

(defun set-google-meta-tag(tag)
  (set-bliky-setting 'google-meta-tag tag))

(defun get-google-meta-tag()
  (let ((val (get-bliky-setting 'google-meta-tag)))
    (if val
	val
	(set-google-meta-tag ""))))

(defun set-technorati-claim(tag)
  (set-bliky-setting 'technorati-claim tag))

(defun get-technorati-claim()
  (let ((val (get-bliky-setting 'technorati-claim)))
    (if val
	val
	(set-technorati-claim ""))))

(defun set-follow-on-twitter(tag)
  (set-bliky-setting 'follow-on-twitter tag))

(defun get-follow-on-twitter()
  (let ((val (get-bliky-setting 'follow-on-twitter)))
    (if val
	val
	(set-follow-on-twitter ""))))

(defun set-email-subscription(tag)
  (set-bliky-setting 'email-subscription tag))

(defun get-email-subscription()
  (let ((val (get-bliky-setting 'email-subscription)))
    (if val
	val
	(set-email-subscription ""))))

(defun set-background-uri(uri)
  (set-bliky-setting 'background-uri uri))

(defun get-background-uri()
  (let ((val (get-bliky-setting 'background-uri)))
    (if val
	val
	(set-background-uri nil))))

(defun set-contact-info(co)
  (set-bliky-setting 'contact-info co))

(defun get-contact-info()
  (let ((val (get-bliky-setting 'contact-info)))
    (if val
	val
	(set-contact-info "contact info not set"))))

(defun set-google-analytics(co)
  (set-bliky-setting 'google-analytics co))

(defun get-google-analytics()
  (let ((val (get-bliky-setting 'google-analytics)))
    (if val
	val
	(set-google-analytics nil))))

(defun set-rss-link(co)
  (set-bliky-setting 'rss-link co))

(defun get-rss-link()
  (let ((val (get-bliky-setting 'rss-link)))
    (if val
	val
	(set-rss-link "<h2>rss</h2>"))))

(defun set-remote-repo(repo)
  (set-bliky-setting 'remote-repo repo))

(defun get-remote-repo()
  (let ((val (get-bliky-setting 'remote-repo)))
    (if val
	val
	(set-remote-repo DEFAULT-REMOTE-REPO-HOST))))

(defun set-rss-validator(co)
  (set-bliky-setting 'rss-validator co))

(defun get-rss-validator()
  (let ((val (get-bliky-setting 'rss-validator)))
    (if val
	val
	(set-rss-validator ""))))

;;---------------path name code-----------------------------------------
(defun create-if-missing(path)
  (unless (probe-file path) (sb-posix:mkdir path USRMODE))
  path)


(defun set-bliky-pathname(path name)
  ;; nil is used to signal 'not set
  (if path
      (set-bliky-setting name (make-pathname :directory path))
      (set-bliky-setting name path))) 

(defun get-bliky-pathname(name default-path)
  ;; side effects : 
  ;; 1. set path if not found to whatever default-path generates.
  ;; 2. create directory if missing
  (let ((val (get-bliky-setting name)))
    (if val
	val
	(let ((path (funcall default-path)))
	  (create-if-missing path)
	  (set-bliky-pathname path name)))))

;----------------------------------
(defun blog-store-location*()
  (let* ((home (sb-posix:getenv "HOME"))
	 (path (format nil "~A/.blog-store.db" home)))
    path))

(defun get-blog-store-location(name)
  (find-db-config name (read-db-config (blog-store-location*))))

(defun set-template-pathname(p)
  (set-bliky-pathname p 'template-pathname))
  
(defun get-template-pathname()
  (labels ((df-path()
	     (let ((home *BLIKY-HOME*))
	       (format nil "~A/templates/" home))))
    (get-bliky-pathname 'template-pathname #'df-path)))

(defun set-styles-pathname(p)
  (set-bliky-pathname p 'styles-pathname))
  
(defun get-styles-pathname()
  (labels ((df-path()
	     (let ((home *BLIKY-HOME*))
	       (format nil "~A/styles/" home))))
    (get-bliky-pathname 'styles-pathname #'df-path)))

;;---
(defun set-script-pathname(p)
  (set-bliky-pathname p 'script-pathname))
  
(defun get-script-pathname()
  (labels ((df-path()
	     (let ((home (sb-posix:getenv "HOME")))
	       (format nil "~A/cl-bliky/js" home))))
    (get-bliky-pathname 'script-pathname #'df-path)))

;;--
(defun set-idiot-location(p)
  (set-bliky-pathname p 'idiot-location))

(defun get-idiot-location()
  (get-bliky-pathname 'idiot-location (lambda()"/tmp/idiot")))

;;--
(defun set-repo-pathname(p)
  (set-bliky-pathname p 'repo-pathname))

(defun get-repo-pathname()
  (labels ((df-path()
	     (let ((home (sb-posix:getenv "HOME"))
		   (user (sb-posix:getenv "USER")))	     
	       (format nil "~A/~A.github.com" home user))))
    (get-bliky-pathname 'repo-pathname #'df-path)))
;;--
(defun set-sandbox-pathname(p)
  (set-bliky-pathname p 'sandbox-pathname))

(defun get-sandbox-pathname()
  (labels ((df-path()
	     (let ((user (sb-posix:getenv "USER")))
	       (format nil "/tmp/~A.sandbox.github.com" user))))
    (get-bliky-pathname 'sandbox-pathname #'df-path)))


(defun set-mainstore?(v)
  (set-bliky-setting 'is_main_store v))
	 
(defun get-mainstore?()
  (if (get-bliky-setting 'is_main_store)
      t
      (not t)))

(defun set-offline?(v)
  (set-bliky-setting 'is_off_line v))
	 
(defun get-offline?()
  (if (get-bliky-setting 'is_off_line)
      t
      (not t)))

(defun show-settings()
  (map-btree (lambda(k v) (format t "~A->~A~%" k v)) (get-settings)))

;;---------------end of settings ---------------------------------------

;;-----------------------------------------------------------------------

;;(defun blog-title()
;;  (get-blog-title))

;;(defun contact-info()
;;  (get-contact-info))


(defun infer-repo-name ()
  (let* ((pn (namestring (get-repo-pathname)))
	(index (search "/" pn :from-end t :end2 (- (length pn) 1))))
    (subseq pn (+ index 1) (- (length pn) 1))))

;;----------------------------
(defun load-script(fn path)
  (handler-case 
      (with-open-file (stream path :direction :input)
	(funcall fn (slurp-stream stream)))
    (error(c) 
      (declare (ignore c)))))
  
(defun tracking-js-path()
  (concatenate 'string (namestring (get-script-pathname)) "/google-analytics.js"))

(defun load-tracking-js()
  (load-script #'set-google-analytics (tracking-js-path)))


;----------------------------
(defun google-meta-tag-path()
  (concatenate 'string (namestring (get-script-pathname)) "/google-meta-tag.js"))

(defun load-google-meta-tag()
  (load-script #'set-google-meta-tag (google-meta-tag-path)))

;;----------------------------
(defun follow-on-twitter-path()
  (concatenate 'string (namestring (get-script-pathname)) "/follow-on-twitter.js"))

(defun load-follow-on-twitter()
  (load-script #'set-follow-on-twitter (follow-on-twitter-path)))

;;----------------------------

(defun email-subscription-path()
  (concatenate 'string (namestring (get-script-pathname)) "/email-subscription.js"))

(defun load-email-subscription()
  (load-script #'set-email-subscription (email-subscription-path)))

;;----------------------------
(defun contact-js-path()
  (concatenate 'string (namestring (get-script-pathname)) "/contact-info.js"))

(defun load-contact-js()
  (load-script #'set-contact-info (contact-js-path)))

;;----------------------------
(defun rss-js-path()
  (concatenate 'string (namestring (get-script-pathname)) "/rss-tag.js"))

(defun load-rss-js()
  (load-script #'set-rss-link (rss-js-path)))

;;-----------------------------------------------------------------------------------
;;-----------git-repo-managment code-------------------------------------------------
;;----------------------------------------------------------------------------------
(defun switch-to-repo(repo-path)
  (sb-posix:chdir repo-path))

(defun switch-to-default-path()
  (sb-posix:chdir *default-pathname-defaults*))

(defun string-to-list2(s sep)
  (let ((l ())
	(str (string-trim '(#\Space) s)))
    (do ((pos  (position sep str)
	       (position sep str)))
	((or (null pos) (eql 0 pos)))
      (push (string-left-trim '(#\Space) (subseq str 0 pos)) l)
      (setf str (string-left-trim '(#\Space) (subseq str (+ pos 1) ))))
    (nreverse l)))

(defun string-to-list(s)
  (let ((l ())
	(str (string-trim '(#\Space) s)))
    (do ((pos  (position #\Space str)
	       (position #\Space str)))
	((or (null pos) (eql 0 pos)))
      (push (string-left-trim '(#\Space) (subseq str 0 pos)) l)
      (setf str (string-left-trim '(#\Space) (subseq str pos ))))
    (push str l)
    (nreverse l)))

(defun run-cmd(cmd &optional args )
  (let ((str (make-string 0)))
    (labels ((set-args (args) 
	       (cond ((null args) nil)
		     ((listp args) args)
		     ((stringp args) (string-to-list args))
		     (t            nil))))
      (let* ((data (process-output (sb-ext:run-program cmd (set-args args)
						       :input :stream :output :stream :search t))))
	
	(do ((line (read-line data nil 'eof)
		   (read-line data nil 'eof)))
	    ((eql line 'eof))
	  (setf str (concatenate 'string str line "|")) )))
    str))

(defun run-git(&optional args )
  (run-cmd "git" args))
	 
(defun find-deleted-files()
  (let* ((status (run-git "status"))
	 (lst ()))
    (do* ((pos  (search "deleted:" status)
		(search "deleted:" status)))
	 ((null pos))
      (let ((peek (search "|" (subseq status pos))))
	(push (string-trim '(#\Space) (subseq status (+ pos (length "deleted:")) (+ peek pos))) lst)
	(setf status (subseq status (+ peek pos)))))
    (nreverse lst)))

(defun git-rm-discarded-posts()
    (dolist (f (find-deleted-files) )
      (run-git (list "rm" f)))) 

(defun git-commit()
  (let* ((ts (substitute #\_ #\Space  (fmt-timestamp (get-universal-time))))
	 (msg  (concatenate 'string "commit  -m message_ok_" ts)))
    (run-git msg)))

(defun git-add-all-posts()
  (run-git "add *.html")
  (run-git "add *.css")
  (run-git "add *.xml"))

(defun git-publish-branch()
  (run-git "checkout -b publish"))

(defun git-master-branch()
  (run-git "checkout master"))

(defun git-merge()
  (run-git "merge publish"))

(defun remove-files()
  (dolist (fn (filter (lambda(x) (not (equalp "CNAME" x))) (string-to-list2 (run-cmd "ls") #\|)))
    (run-cmd "cp" (format nil "~A ~A" fn (get-idiot-location)))
    (run-cmd "rm" (format nil "~A" fn ))))

(defun git-publish()
  ;;
  ;; run ssh-add to add the pasword to the ssh agent.
  ;; this way you won't get challenged for a keyword..
  ;;
  (let* ((cmd "git")
	 (str (make-string 0))
	 (args  (list "push" "origin" "master"))
	 (proc  (sb-ext:run-program cmd args :wait nil :input :stream :output :stream :search t))
	 (data  (process-output proc)))
    (if (not (eql (process-status proc) :RUNNING))
	(progn
	  (process-kill proc -9)
	  (setf str (format nil 
			    "killed stopped process ~A ~A; add pwd to ssh agent; or run push manually" 
			    cmd args)))
	(do ((line (read-line data nil 'eof)
		   (read-line data nil 'eof)))
	    ((eql line 'eof))
	  ;;(format t "~A ~%" line)
	  (setf str (concatenate 'string str line "|"))))
    str))

(defun p-blogpost-active(blog-post)
  (cond ((not (gc-bit   blog-post))        t) 
	( t                           (not t))))

(defun get-blog-post-by-timestamp(ts)
  (let ((objs (get-instances-by-value 'blog-post 'timestamp ts)))
    (if objs
	(dolist (obj objs)
	  (if (p-blogpost-active obj)
	      (return obj)))
	nil)))

(defun create-blog-post(title intro body type &optional (fmt-bid (md)))
  (make-instance 'blog-post :title title :intro intro :body body :type-bit type :fmt-bit fmt-bid))

(defun create-html-blog-post(title intro body type)
  (create-blog-post title intro body type (html)))

;  (make-instance 'blog-post :title title :intro intro :body body :type-bit type :fmt-bit (html)))

(defun create-blog-post-template(type &optional (fmt-bid (md)))
  (let* ((title (format nil "new blog post dd ~A" (fmt-timestamp (get-universal-time))))
	 (intro "")
	 (body ""))
    (create-blog-post title intro body type fmt-bid)))

(defun create-about-post()
  (let* ((title "About")
	 (intro "")
	 (body ""))
    (create-blog-post title intro body (about))))

(defun make-url-part(title)
  (string-downcase 
   (delete-if #'(lambda(x) (not (or (alphanumericp x) (char= #\- x))))
	      (substitute #\- #\Space title))))


;;---------Importing a Repo------------------------------------------

(defun disassem-html-page(page)
  (let ((links nil))    
    (labels ((get-page(path) 
	       (with-open-file (stream path  :direction :input)
		 (slurp-stream stream)))
	     (link-cb (element) 
	       (labels ((is-div(lst)
			  (cond ((consp lst) (and (equal :DIV (car lst)) (eq :ID (cadr lst))))
				( t (not t))))
			(get-div-tag(lst)
			  (if (is-div lst)
			      (car (cdr (cdr lst)))
			      nil))
			(describe-tag(tag)
			  ;;(format t "in describe-tag ~A~%" tag)
			  (cond ((equal tag "post-timestamp-id") tag)
				((equal tag "post-body")         tag)
				((equal tag "post-intro")        tag)
				((equal tag "post-header")       tag) 
				((equal tag "post-type")         tag) 
				((equal tag "post-timestamp")    tag)
				( t                            (not t))))
			(unpack(el)
			  el))
		 (let ((tag  (describe-tag (get-div-tag (car element)))))
		   (if tag 
		       (push (list tag (unpack (cdr element))) links))))))
      (let ((cb (cons (cons :DIV #'link-cb ) nil)))
	(html-parse:parse-html (get-page page) :callbacks cb)
	links))))

(defun unpack (str up)
  (labels ((find-piece(str lst &optional (accum nil))
	     (cond ((equal str (car (car lst))) (push (cadr (car lst)) accum))
		   ((eq lst nil) accum)
		   (t            (find-piece str (cdr lst) accum)))))
    (let ((piece (find-piece str up)))
      (labels ((up-ts(lst) 
		 (str-strip (cadr (caar lst))))
	     ;;;
	       (rm-wrapper-tags(seq)
		 (let ((slen (length "<BODY>"  )) 
		     (nlen (length "</BODY>" )))
		   (subseq seq slen (- (length seq) nlen ))))
	     ;;;
	       (to-wrapped-html(lst)
		 (let ((s (cons :body (car lst))))
		   (with-output-to-string (stream)
		     (net.html.generator:html-print s stream))))
	       ;;TODO : move handler case HERE...
	       (to-html(lst)
		 (handler-case 
		     (str-strip (rm-wrapper-tags (to-wrapped-html lst)))
		   (error(c) 
		     (declare (ignore c))
		     (cons IMPORT-FAILED-MSG lst)))))
	(cond ((equal "post-timestamp-id"  str ) (up-ts   piece))
	      ((equal "post-timestamp"     str ) (up-ts   piece))
	      ((equal "post-header"        str ) (up-ts   piece))
	      ((equal "post-type"          str ) (up-ts   piece))
	      ((equal "post-intro"         str ) (to-html piece))
	      ((equal "post-body"          str ) (to-html piece))
	      (t nil))))))


(defun unpack-post(p)
  (let ((ts-id  (parse-integer (unpack "post-timestamp-id" p)))
	(ts                    (unpack "post-timestamp" p))
	(header                (unpack "post-header" p))
	(intro                 (unpack "post-intro" p))
	(body                  (unpack "post-body" p))
	(type   (str2type      (unpack "post-type" p))))
    (values ts-id ts type header intro body)))

(defun mv-about()
  (let ((lst (get-instances-by-value 'blog-post 'type-bit (about)))
	(seed (random (floor (/ (get-universal-time) 100000000)))))
    (dolist (obj lst)
      (setf (type-bit obj) (sidebar))
      (setf (title obj) (format nil "About-~A-~A" (get-universal-time) seed))
      (setf (url-part obj) (make-url-part (title obj)))
      (setf (gc-bit   obj) t))))

(defun recreate-post(ts-id type header intro body)
  ;; the about post is always generated when it's not found
  ;; move the on found to the sidebar, so no info is lost..
  (when (equal type (about) ) (mv-about))
  (let ((post (create-html-blog-post header intro body type)))
    (setf (timestamp post) ts-id)
    post))
      

(defun import-blog-post(html-page)
  (multiple-value-bind
	(ts-id ts type header intro body)
      (unpack-post (disassem-html-page html-page))
    (declare (ignore ts))
    (let ((pst (get-blog-post-by-timestamp ts-id)))
      (unless pst  (recreate-post ts-id type header intro body)))))

(defun import-repo(repo-path)
  (let ((lst (directory 
	      (make-pathname :name :wild :type :wild :defaults repo-path))))
    (dolist (f lst)
      (let* ((fn    (namestring f))
	     (not-index? (not (search "/index.html" fn :from-end t)))
	     (html? (search ".html" fn :from-end t)))
	(handler-case 
	    (when (and html? not-index?) (import-blog-post fn))
	  (error(c)
	    (format t "an error [~A] occured when importing post ~A at ~A" c fn repo-path)))))))



;;-------------------

(defun blog-posts() 
  (or (get-from-root "blog-posts")
      (let ((blog-posts (make-pset)))
	(add-to-root "blog-posts" blog-posts)
	(blog-posts))))

(defun open-blog-store(name) 
  (let ((blog-store (list :BDB (get-blog-store-location name) )))
    (if (null *store-controller*)  
	(open-store  blog-store :recover t)
	(print "store already opened"))))


(defun p-blogpost-post(blog-post)
  (cond ((not (slot-boundp blog-post 'type-bit))        t)
	((eq  (type-bit blog-post) (post))              t)
	((eq  (type-bit blog-post) (not t))              t)
	( t                                             (not t))))

(defun p-blogpost-sidebar(blog-post)
  (cond ((eq  (type-bit blog-post) (sidebar))              t)
	( t                                             (not t))))

(defun p-blogpost(blog-post) 
  (and (p-blogpost-active blog-post) (p-blogpost-post blog-post)))

(defun p-sidebar(blog-post) 
  (and (p-blogpost-active blog-post) (p-blogpost-sidebar blog-post)))

(defun show-blogs()
  (map-class #'print 'blog-post))

(defmethod initialize-url-part  ((obj blog-post))
  (cond ((eq nil (url-part obj))
	 (setf (url-part obj) (make-url-part (title obj)))))) 

(defmethod initialize-instance :after ((obj blog-post) &key)
  (cond ((eq nil (url-part obj))
	 (setf (url-part obj) (make-url-part (title obj)))))) 

(defun style-css-path()
  (concatenate 'string (namestring (get-styles-pathname)) "/style.css"))

(defun background-css(uri)
  (format nil "background-image:url('~A')" uri))

(defun add-background(str) 
  (let ( (background (get-background-uri)))
    (if  background
	 (string-replace "#:background-image" (background-css background) str)
	 str)))

(defun cat-style-sheet()
  (with-open-file (stream (style-css-path) :direction :input)
    (add-background (slurp-stream stream))))

(defun inject-style-sheet()
    (let ((sheet (cat-style-sheet)))
      ( format nil "<style type=\"text/css\"> ~A </style>" sheet)))

(defun online-style-sheet()
  (format nil "<link rel=\"stylesheet\" href=\"/style.css\" type=\"text/css\"/>"))

;;needs to get all instances
(defun discard-post-by-type(type)
  (let ((post (get-instance-by-value 'blog-post 'type-bit type)))
    (if post
	(setf (gc-bit post) t)
	nil)))

(defun get-live-blog-post(url-part)
  (let ((objs (get-instances-by-value 'blog-post 'url-part url-part)))
    (if objs
	(dolist (obj objs)
	  (if (p-blogpost-active obj)
	      (return obj)))
	nil)))

(defun get-blog-post(url-part &optional (fmt-bid (md))) 
  (let ((obj (get-live-blog-post url-part)))
    (if obj
	obj
	(create-blog-post-template (post) fmt-bid ))))

(defun get-about-post()
  (let ((about-post (get-instance-by-value 'blog-post 'type-bit (about))))
    (if (null about-post)
	(create-about-post)
	about-post)))

(defun about-page() 
  (let ((obj (get-about-post)))
    (url-part obj)))

(defun template-path(tmpl)
  (make-pathname :directory (namestring (get-template-pathname) )  :name tmpl))

(defun generate-error-page(trace msg)
  (handler-case
      (with-output-to-string (stream)
	(let ((html-template:*string-modifier* #'identity))
	  (html-template:fill-and-print-template
	   (template-path "error.tmpl")
	   (list :error-condition trace
		 :error-message   msg)
	   :stream stream)))
    (error(c) 
      (format nil "an error when generating the error page: ~A" c))))

(defun protect(f)
  (lambda()
    (handler-case 
	(funcall f)
      (error(c)
	(generate-error-page c "an error occured when generating static pages")))))

(defun rss-feed-format_alt(blog-post)
  (handler-case  
      (list :timestamp (fmt-timestamp (timestamp blog-post))
	    :title (title blog-post)
	    :url-part (url-part blog-post)
	    :intro (concatenate 'string 
				(escape-html-chars (render-post 'intro blog-post)) 
				(escape-html-chars (render-post 'body blog-post)))
	    :cdata (concatenate 'string (render-md (str-strip (intro blog-post)))
				(render-md (str-strip (body blog-post)))))
    (error(c) 
      (format t "an error ~A occurred when generating the rss for post ~A ~%" c (title blog-post)))))

(defun rss-feed-format(blog-post)
  (handler-case  
      (list :timestamp (fmt-timestamp (timestamp blog-post))
	    :title (title blog-post)
	    :url-part (url-part blog-post)
	    :intro (escape-html-chars (render-post 'intro blog-post))
	    :cdata (concatenate 'string (render-post 'intro blog-post)
				(render-post 'body blog-post)))
    (error(c) 
      (format t "an error ~A occurred when generating the rss for post ~A ~%" c (title blog-post)))))

;;(intro blog-post)

(defun use-static-template(blog-post)
  (list :timestamp    (fmt-timestamp (timestamp blog-post))
	:timestamp-id (timestamp blog-post)
	:type         (string-downcase (type-bit blog-post))
	:title        (title blog-post)
	:url-part     (url-part blog-post)
	:intro        (render-post 'intro blog-post)))

(defun use-edit-template(blog-post)
  (cons :edit_part (cons t (use-static-template blog-post))))

(defun collect-posts(tlf)
  (loop for blog-post in (nreverse 
			  (get-instances-by-range 'blog-post 
						  'timestamp nil nil))
     if (p-blogpost blog-post )
     collect (funcall tlf blog-post)))

(defun collect-sidebars(tlf)
  ;; note that this is oldest-firts order
  (loop for blog-post in (get-instances-by-range 'blog-post 
						 'timestamp nil nil)
     if (p-sidebar blog-post )
     collect (cons :body (cons (body blog-post) (funcall tlf blog-post)))))

(defun blog-url() 
  (format nil "http://~A/" (infer-repo-name)))

(defun blog-description() 
  (let ((post (car (get-instances-by-value 'blog-post 'type-bit (about)))))
    (escape-html-chars (str-strip (intro post)))))

(defun generate-rss-page()
  (with-output-to-string (stream)
    (let ((html-template:*string-modifier* #'identity))
      (html-template:fill-and-print-template
       (template-path "rss.tmpl")
       (list :blog-title (get-blog-title)
	     :blog-url   (blog-url)
	     :blog-description (blog-description)
	     :rss-generation-date (fmt-timestamp (get-universal-time)) 
	     :blog-posts          (collect-posts    #'rss-feed-format) )
       :stream stream))))

(defun generate-index-page(tlf &key create (style-sheet 'inject-style-sheet))
  (with-output-to-string (stream)
    (let ((html-template:*string-modifier* #'identity))
      (html-template:fill-and-print-template
       (template-path "index.tmpl")
       (list :style-sheet (funcall style-sheet)
	     (unless (get-offline?) :google-analytics) (unless (get-offline?) (get-google-analytics))
	     :rss-link (if (get-offline?) "<h2>rss</h2>" (get-rss-link))
	     (if create :edit_part) (if create t) 
	     :blog-title       (get-blog-title)
	     :google-meta-tag  (get-google-meta-tag)
	     :main-repo-qs     (main-repo-qs)
	     :sandbox-repo-qs  (sandbox-repo-qs)
	     (unless (get-offline?) :contact-info)       (unless (get-offline?) (get-contact-info))
	     (unless (get-offline?) :rss-validator)      (unless (get-offline?) (get-rss-validator))
	     (unless (get-offline?) :follow-on-twitter)  (unless (get-offline?) (get-follow-on-twitter))
	     (unless (get-offline?) :email-subscription) (unless (get-offline?) (get-email-subscription))
	     :about            (about-page)
	     :sidebars         (collect-sidebars tlf)
	     :blog-posts       (collect-posts    tlf))
       :stream stream))))

(defun generate-editable-index-page()
  (generate-index-page #'use-edit-template :create t))

(defun generate-blog-post-page(tmpl url-part &key (render 'render-identity) 
			       (style-sheet 'inject-style-sheet))
    (with-output-to-string (stream)
			   (let ((blog-post (get-live-blog-post url-part))
				 (html-template:*string-modifier* #'identity))
			     (if blog-post
				 (html-template:fill-and-print-template
				  tmpl
				  (list :style-sheet (funcall style-sheet)
					:blog-title (get-blog-title)
					:title (title blog-post)
					:url_part url-part
					:timestamp (fmt-timestamp (timestamp blog-post))
					:timestamp-id (timestamp blog-post)
					:type         (string-downcase (type-bit blog-post))
					:intro (funcall render 'intro blog-post) 
					:body  (funcall render 'body  blog-post))
				  :stream stream)))))

(defun view-blog-post-page()
  (generate-blog-post-page (template-path "post.tmpl") (hunchentoot:query-string*) :render 'render-post))

;;
;; Somehow we end up with a extra space at the start of the intro and body
;; This could trigger some mark-down formatting;
;; Hence the string is stripped of spaces etc.. 
;;
(defun save-blog-post() 
  (let ((blog-post (get-live-blog-post (hunchentoot:query-string*))))
    (setf (title    blog-post) (hunchentoot:post-parameter "title"))
    (setf (intro    blog-post) (str-strip (hunchentoot:post-parameter "intro")))
    (setf (body     blog-post) (str-strip (hunchentoot:post-parameter  "body")))
    (setf (url-part blog-post) (make-url-part (title blog-post)))
    (hunchentoot:redirect "/" )))


(defun split-sequence(seq tkw kw)
  (handler-case
      (let* ((ltkw   (length tkw))
	     (lkw    (length kw))
	     (tkwpos (search tkw seq))
	     (kwpos  (search kw  seq))
	     (title  (subseq seq 0 tkwpos))
	     (intro  (subseq seq (+ tkwpos ltkw 1) kwpos))
	     (body   (subseq seq (+ kwpos lkw 1))))
	(values title intro body))
    (error(c) 
      (declare (ignore c))
      (values "new blog post created from upload" "" seq))))
	 
(defun redirect-to-edit-page(blog-post)
  (hunchentoot:redirect (concatenate 'string "/edit/?"(url-part blog-post))))

(defun save-file()
  (labels ((read-file(params) 
	     (let ((fn (namestring(car params))))
	       (with-open-file (stream fn :direction :input)
		 (split-sequence (slurp-stream stream) (<title>) (<split>))))))
    (let ((blog-post (get-blog-post (hunchentoot:query-string*))))
      (multiple-value-bind 
	    (title intro body) (read-file (hunchentoot:post-parameter "file"))
	(setf (title  blog-post) title) 
	(setf (intro  blog-post) intro) 
	(setf (body   blog-post) body)
	(setf (url-part blog-post) (make-url-part (title blog-post))))
      (redirect-to-edit-page blog-post))))

(defun save-html-file(url-part fn)
  (labels ((read-file(fn) 
	     (with-open-file (stream fn :direction :input)
	       (split-sequence (slurp-stream stream) (<title>) (<split>)))))
    (let ((blog-post (get-blog-post url-part (html))))
      (multiple-value-bind (title intro body) (read-file fn)
	(setf (title  blog-post) title) 
	(setf (intro  blog-post) intro) 
	(setf (body   blog-post) body)
	(setf (url-part blog-post) (make-url-part (title blog-post))))
      blog-post)))

#|
    (let ((blog-post (get-blog-post (hunchentoot:query-string*))))
      (multiple-value-bind 
	    (title intro body) (read-file (hunchentoot:post-parameter "file"))
	(setf (title  blog-post) title) 
	(setf (intro  blog-post) intro) 
	(setf (body   blog-post) body)
	(setf (url-part blog-post) (make-url-part (title blog-post))))
      (redirect-to-edit-page blog-post))))
|#

(defun discard-blog-post()
  (let ((blog-posts (get-instances-by-value 'blog-post 'url-part (hunchentoot:query-string*))))
    (dolist (blog-post blog-posts)
      (setf (gc-bit blog-post) t))
    (hunchentoot:redirect "/" )))

(defun discard-all-blog-posts()
  (let ((blog-posts (get-instances-by-class 'blog-post)))
    (dolist (blog-post blog-posts)
      (setf (gc-bit blog-post) t))))

(defun undo-all-discards() 
  (let ((blog-posts (get-instances-by-value 'blog-post 'gc-bit t)))
    (dolist (blog-post blog-posts)
      (setf (gc-bit blog-post) (not t)))))

(defun drop-all-discards() 
  (let ((blog-posts (get-instances-by-value 'blog-post 'gc-bit t)))
    (drop-instances blog-posts)))

(defun create-empty-blog-post(tmpl type-str)
  (let* ((bp (create-blog-post-template (str2type type-str)))
	 (up (url-part bp)))
    (generate-blog-post-page tmpl up)))


(defun create-blog-post-page()
  (cond ((eq (hunchentoot:request-method*) :GET)  (create-empty-blog-post 
						  (template-path "post-edit.tmpl")
						  (hunchentoot:query-string*)))
	((eq (hunchentoot:request-method*) :POST) (save-blog-post))))

(defun edit-blog-post-page()
  (cond ((eq (hunchentoot:request-method*) :GET)  (generate-blog-post-page 
						  (template-path "post-edit.tmpl")
						  (hunchentoot:query-string*)))
	((eq (hunchentoot:request-method*) :POST) (save-blog-post))))

(defun simple-template(p) 
  (with-open-file (stream p :direction :input)
    (slurp-stream stream)))

(defun generate-import-page()
  (with-output-to-string (stream)
    (let ((html-template:*string-modifier* #'identity))
      (html-template:fill-and-print-template
       (template-path "import.tmpl")
       (list :style-sheet (inject-style-sheet)
	     :blog-title (get-blog-title)
	     :repo-name (infer-repo-name)
	     :remote-repo (get-remote-repo)
	     :repo-pathname  (get-repo-pathname))
       :stream stream))))

(defun do-import (file-path)
  (import-repo (directory-namestring file-path))
  (hunchentoot:redirect "/" ))

(defun import-remote ()
  (format nil "this is a test : ~A" (hunchentoot:post-parameters*) ) )

(defun import-local()
  ( do-import (hunchentoot:post-parameter "repo-path")))

(defun import-repo-page()
  (cond ((eq (hunchentoot:request-method*) :GET)  (generate-import-page))
	((eq (hunchentoot:request-method*) :POST) ( do-import (hunchentoot:post-parameter "file")))))

(defun generate-options-page()
  (labels ((checked?(fn)
	     (when (funcall fn) "checked")))
    (with-output-to-string (stream)
      (let ((html-template:*string-modifier* #'identity))
	(html-template:fill-and-print-template
	 (template-path "options.tmpl")
	 (list :style-sheet         (inject-style-sheet)
	       :blog-title          (get-blog-title)
	       :background-uri      (get-background-uri)
	       :bliky-port          (get-bliky-port)
	       :idiot-location      (get-idiot-location)
	       :remote-repo         (get-remote-repo)
	       :repo-pathname       (get-repo-pathname)
	       :is-mainstore        (checked? #'get-mainstore? )
	       :is-offline          (checked? #'get-offline?) 
	       :template-pathname   (get-template-pathname)
	       :styles-pathname     (get-styles-pathname)
	       :script-pathname     (get-script-pathname)
	       :google-meta-tag     (clean-str (str-strip (get-google-meta-tag)))
	       :contact-info        (clean-str (str-strip (get-contact-info)))
	       :rss-image-link      (clean-str (str-strip (get-rss-link)))
	       :rss-validator       (clean-str (str-strip (get-rss-validator)))
	       :follow-on-twitter   (clean-str (str-strip (get-follow-on-twitter)))
	       :email-subscription  (clean-str (str-strip (get-email-subscription)))
	       :web-analytics       (clean-str (str-strip (get-google-analytics)))
	       :sandbox-pathname    (get-sandbox-pathname))
	 :stream stream)))))

(defun save-option(fn p)
  (funcall fn (hunchentoot:post-parameter p)))

(defun save-checked(fn p)
  (funcall fn (string-equal (hunchentoot:post-parameter p) "on")))

(defun save-options-main()
  (save-checked #'set-mainstore?        "is-mainstore?")
  (save-checked #'set-offline?          "is-offline?")
  (save-option  #'set-bliky-port        "bliky-port")
  (save-option  #'set-repo-pathname     "repo-pathname")
  (save-option  #'set-sandbox-pathname  "sandbox-pathname")
  (save-option  #'set-template-pathname "template-pathname")
  (save-option  #'set-styles-pathname   "styles-pathname")
  (save-option  #'set-script-pathname   "script-pathname")
  (save-option  #'set-blog-title        "blog-title")
  (save-option  #'set-background-uri    "background-uri")
  (save-option  #'set-remote-repo       "remote-repo")
  (save-option  #'set-google-meta-tag   "google-meta-tag")
  (save-option  #'set-google-analytics  "web-analytics")
  (save-option  #'set-contact-info      "contact-info")
  (save-option  #'set-rss-link          "rss-image-link")
  (save-option  #'set-rss-validator     "rss-validator")
  (save-option  #'set-follow-on-twitter "follow-on-twitter")
  (save-option  #'set-email-subscription "email-subscription")
  (save-option  #'set-idiot-location    "idiot-location"))

(defun save-resource(action params)
  (labels ((make-lookup()
	     (let ((lst))
	       (setf lst (acons "web-analytics"      #'set-google-analytics lst))
	       (setf lst (acons "google-meta-tag"    #'set-google-meta-tag lst))
	       (setf lst (acons "contact-info"       #'set-contact-info  lst))
	       (setf lst (acons "rss-validator"      #'set-rss-validator lst))
	       (setf lst (acons "follow-on-twitter"  #'set-follow-on-twitter lst))
	       (setf lst (acons "email-subscription" #'set-email-subscription lst))
	       (setf lst (acons "rss-image-link"     #'set-rss-link lst))
	       lst))
	   ;;
	   (lookup-action (action lst)
	     (cdr (assoc action lst :test #'equal))))
    (let  ((fn (namestring(car params))))
      (load-script (lookup-action action (make-lookup)) fn))))

(defun redirect-to-options-page()
  (hunchentoot:redirect (concatenate 'string "/options/?")))
  
(defun save-options()
  (let ((which (hunchentoot:post-parameter "which"))
	(file  (hunchentoot:post-parameter "file")))
    (labels ((empty-qs()
	       (eq 0 (length (hunchentoot:query-string*)))))
      (cond ( (empty-qs)(progn (save-options-main) 
			       (hunchentoot:redirect "/" )))
	    ( t         (progn (save-resource which file)
			       (redirect-to-options-page)))))))


(defun options-page()
  (cond ((eq (hunchentoot:request-method*) :GET)  (generate-options-page))
	((eq (hunchentoot:request-method*) :POST) (save-options))))

(defun discard-blog-post-page()
  (cond ((eq (hunchentoot:request-method*) :GET)  (discard-blog-post))
	((eq (hunchentoot:request-method*) :POST) (discard-blog-post))))

(defun undo-discards() 
  (undo-all-discards)
  (hunchentoot:redirect "/" ))

(defun drop-discards() 
  (drop-all-discards)
  (hunchentoot:redirect "/" ))

(defun push-pages-to-repo(repo-path)
  (labels ((publish-index-page()
	     (let ((fn (concatenate 'string repo-path "/index.html")))
	       (with-open-file (stream fn :direction :output :if-exists :supersede)
		 (format stream "~A" (generate-index-page #'use-static-template :style-sheet 'online-style-sheet))
		  ;;(generate-index-page #'use-static-template)
			 )))

	   (publish-style-sheet()
	     (let ((fn (concatenate 'string repo-path "/style.css")))
	       (with-open-file (stream fn :direction :output :if-exists :supersede)
		 (format stream "~A" (cat-style-sheet)))))
	   
	   (publish-rss-page()
	     (let ((fn (concatenate 'string repo-path "/feed.xml")))
	       (with-open-file (stream fn :direction :output :if-exists :supersede)
		 (format stream "~A" (generate-rss-page)))))

	   (url-parts()
	     (let ((lst))
	       (labels ((up(u) 
			  (if (slot-boundp u 'url-part)
			      (push (url-part u) lst))))
		 (map-class #'up 'blog-post))
	       (nreverse lst)))
	   (publish-other-pages()
	     (dolist (up (url-parts))
	       (let ((fn (concatenate 'string repo-path "/" up ".html")))
		 (with-open-file (stream fn :direction :output :if-exists :supersede)
		   (format stream "~A" (generate-blog-post-page (template-path "post.tmpl") up 
								:render 'render-post 
								:style-sheet 'online-style-sheet)))))))
    (publish-index-page)
    (publish-rss-page)
    (publish-style-sheet)
    (publish-other-pages)))

(defun publish-pages*()
  (let ((repo-pathname (get-repo-pathname)))
    (switch-to-repo repo-pathname)
    (git-publish-branch)
    (when (get-mainstore?) (remove-files))    
    (push-pages-to-repo (namestring repo-pathname) )
    (git-add-all-posts)
    (when (get-mainstore?) (git-rm-discarded-posts))
    (git-commit)
    (git-master-branch)
    (git-merge)
    (git-publish)
    (switch-to-default-path)))


(defun publish-pages()
  (publish-pages*)
  (hunchentoot:redirect "/"))
;
(defun index-unless(d)
  (if (> (length d) 1)
      d
      "index.html"))

(defun static-page-path(p)
  (let ((name (index-unless p))) 
    (concatenate 'string (namestring (get-sandbox-pathname)) name)))

(defun get-static-page(p) 
  (with-open-file (stream (static-page-path p ) :direction :input)
    (slurp-stream stream)))

(defun static-pages()
  (get-static-page (hunchentoot:request-uri*)))


(defun generate-static-pages()
  (let ((repo-path (create-if-missing (get-sandbox-pathname))))
    (push-pages-to-repo (namestring repo-path))
    (get-static-page "index.html")))
  
(setq hunchentoot:*dispatch-table* 
      (list (hunchentoot:create-regex-dispatcher "[.]html"            (protect 'static-pages))
	    (hunchentoot:create-regex-dispatcher "[.]xml"             (protect 'static-pages))
	    (hunchentoot:create-regex-dispatcher "^/$"                (protect 'generate-editable-index-page))
	    (hunchentoot:create-regex-dispatcher "^/view/$"           (protect 'view-blog-post-page))
	    (hunchentoot:create-regex-dispatcher "^/edit/$"           (protect 'edit-blog-post-page))
	    (hunchentoot:create-regex-dispatcher "^/create/save-file" (protect 'save-file))
	    (hunchentoot:create-regex-dispatcher "^/edit/save-file"   (protect 'save-file))
	    (hunchentoot:create-regex-dispatcher "^/discard/$"        (protect 'discard-blog-post-page))
	    (hunchentoot:create-regex-dispatcher "^/import-repo/$"    (protect 'import-repo-page))
	    (hunchentoot:create-regex-dispatcher "^/import-local/$"   (protect 'import-local))
	    (hunchentoot:create-regex-dispatcher "^/import-remote/$"  (protect 'import-remote))
	    (hunchentoot:create-regex-dispatcher "^/publish/$"        (protect 'publish-pages))
	    (hunchentoot:create-regex-dispatcher "^/options/$"        (protect 'options-page))
	    (hunchentoot:create-regex-dispatcher "^/rss-feed/$"       (protect 'generate-rss-page))
	    (hunchentoot:create-regex-dispatcher "^/static-pages/$"   (protect 'generate-static-pages))
	    (hunchentoot:create-regex-dispatcher "^/undo-discards/$"  (protect 'undo-discards))
	    (hunchentoot:create-regex-dispatcher "^/drop-discards/$"  (protect 'drop-discards))
	    (hunchentoot:create-regex-dispatcher "^/create/$"         (protect 'create-blog-post-page))))

(defun start-server*()
  (setf *bliky-server* (make-instance 'hunchentoot:acceptor :port (get-bliky-port)))
  (hunchentoot:start *bliky-server*))

(defun start-server(name)
  (open-blog-store name)
  (unless *bliky-server* (start-server*)))


;;(defun stop-server()
;;  (hunchentoot:stop-server *bliky-server*)
;;  (setf *bliky-server* nil)
;;  (close-store))

(defun stop-server()
  (close-store)
  ;;(hunchentoot:stop *bliky-server*)
  (setf *bliky-server* nil))

