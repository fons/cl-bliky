(asdf:operate 'asdf:load-op :cl-html-parse)
(asdf:operate 'asdf:load-op :htmlgen)
(asdf:operate 'asdf:load-op :cl-markdown)

(in-package :cl-user)
(defpackage :blog-cl)
(require :hunchentoot)
(require 'html-template)
(require :elephant)
(use-package :elephant)

(defconstant MONTHS     '("xx" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(defconstant DAYS         '("Mon" "Tues" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defconstant TIMEZONE     '("1" "2" "3" "4" "5" "Est" "6") )
(defconstant DEFAULT-PORT 8050)
(defconstant DEFAULT-REMOTE-REPO-HOST "github.com")

(defconstant USRMODE (logior sb-posix:s-irusr sb-posix:s-ixusr sb-posix:s-iwusr))

(defvar *blog-store-location* (not t))
(defvar *bliky-server*        (not t))

;;format classes
(defmacro html() `'html)
(defmacro md() `'md)

;;blog classes
(defmacro post()      `'post) 
(defmacro about()     `'about)
(defmacro sidebar()   `'sidebar)

;; macro's used when reading in an uploaded file.
(defmacro <title>() `"<title>")
(defmacro <split>() `"<split>")


(defmacro main-repo-qs()    `"main-repo")
(defmacro sandbox-repo-qs() `"sandbox-repo")


(setf (hunchentoot:log-file) "/tmp/error.file")

(defun str-strip(str)
  (string-trim '(#\Space #\Newline #\Tab #\") str))


;;from
;;www.emmett.ca/~sabetts/slurp.html
;;
(defun slurp-stream(stream)
  (let ((seq (make-string (file-length stream))))
    (read-sequence seq stream)
    seq))


(defun fmt-timestamp(st)
  (multiple-value-bind 
	(second minute hour day month year dow dst-p tz)
      (decode-universal-time st)
    (declare (ignore dst-p))
    (format nil "~&~A ~A ~A ~A:~A:~A ~A ~A"
	    (nth dow DAYS)
	    (nth month MONTHS) 
	    day
	    hour 
	    minute 
	    second
	    (nth tz TIMEZONE)
	    year)))

(defun str2type(str) 
  (let ((s (str-strip str)))
    (cond ((eq    (not t)   s)    (post))
	  ((equal "post"    s)    (post))
	  ((equal "about"   s)    (about))
	  ((equal "sidebar" s)    (sidebar))
	  ( t                     (post)))))

(defun render-md(s)
  (with-output-to-string (stream)
    (markdown:markdown s :stream stream :format :html)))

(defpclass blog-post ()
  ((title :initarg :title
		   :accessor title)
   (intro :initarg :intro
		   :accessor intro)
   (body  :initarg  :body
		    :accessor body)
   (timestamp  :initarg  :timestamp
	       :accessor timestamp
	       :initform (get-universal-time)
	       :index t)
   (gc-bit     :accessor gc-bit
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
  (format stream "~&blog-post : [title = ~A] [url-part = ~A][time=~A][gc=~A]~%[type=~A][fmt=~A]~%[~A]~%[~A]~% "
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

;;--------------settings-----------------

(defun blog-store-location()
  (if *blog-store-location*
      *blog-store-location*
      (let ((home (sb-posix:getenv "HOME")))
	(format nil "~A/store/blogs/" home))))

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
  (set-bliky-setting 'bliky-port port))

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


(defun set-remote-repo(repo)
  (set-bliky-setting 'remote-repo repo))

(defun get-remote-repo()
  (let ((val (get-bliky-setting 'remote-repo)))
    (if val
	val
	(set-remote-repo DEFAULT-REMOTE-REPO-HOST))))

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
(defun set-template-pathname(p)
  (set-bliky-pathname p 'template-pathname))
  
(defun get-template-pathname()
  (labels ((df-path()
	     (let ((home (sb-posix:getenv "HOME")))
	       (format nil "~A/cl-bliky" home))))
    (get-bliky-pathname 'template-pathname #'df-path)))

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

;;-----------------------------------------------------------------------

(defun blog-title()
  (get-blog-title))

(defun contact-info()
  (get-contact-info))


(defun infer-repo-name ()
  (let* ((pn (namestring (get-repo-pathname)))
	(index (search "/" pn :from-end t :end2 (- (length pn) 1))))
    (subseq pn (+ index 1) (- (length pn) 1))))
;;-------------------------------------------------
(defun tracking-js-path()
  (concatenate 'string (namestring (get-template-pathname)) "/google-analytics.js"))

(defun load-tracking-js()
  (handler-case 
      (with-open-file (stream (tracking-js-path) :direction :input)
	(set-google-analytics (slurp-stream stream)))
    (error(c) 
      (declare (ignore c))
      nil)))

;----------------------------------------
(defun contact-js-path()
  (concatenate 'string (namestring (get-template-pathname)) "/contact-info.js"))

(defun load-contact-js()
  (handler-case 
      (with-open-file (stream (contact-js-path) :direction :input)
	(set-contact-info (slurp-stream stream)))
    (error(c) 
      (declare (ignore c))
      nil)))

;;-----------git-repo-managment code-----------
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
  (run-git "add *.html"))

(defun git-publish-branch()
  (run-git "checkout -b publish"))

(defun git-master-branch()
  (run-git "checkout master"))

(defun git-merge()
  (run-git "merge publish"))

(defun remove-files()
  (dolist (fn (string-to-list2 (run-cmd "ls") #\|))
    ;;(format t "~A~%" fn)
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

(defun create-blog-post(title intro body type)
  (make-instance 'blog-post :title title :intro intro :body body :type-bit type))

(defun create-blog-post-template(type)
  (let* ((title (format nil "new blog post dd ~A" (fmt-timestamp (get-universal-time))))
	 (intro "")
	 (body ""))
    (create-blog-post title intro body type)))

(defun create-about-post()
  (let* ((title "About")
	 (intro "")
	 (body ""))
    (create-blog-post title intro body (about))))

(defun make-url-part(title)
  (string-downcase 
   (delete-if #'(lambda(x) (not (or (alphanumericp x) (char= #\- x))))
	      (substitute #\- #\Space title))))

;;;---------Importing a Repo------------------------------------------

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
	(html-parse:parse-html (get-page page) :callbacks cb )
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
	     ;;;
	       (to-html(lst)
		 (str-strip (rm-wrapper-tags (to-wrapped-html lst)))))
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
  (let ((post (create-blog-post header intro body type)))
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
	(when (and html? not-index?) (import-blog-post fn))))))

;;-------------------

(defun blog-posts() 
  (or (get-from-root "blog-posts")
      (let ((blog-posts (make-pset)))
	(add-to-root "blog-posts" blog-posts)
	(blog-posts))))

(defun open-blog-store() 
  (let ((blog-store (list :BDB (blog-store-location) )))
    (if (null *store-controller*)  
	(open-store  blog-store )
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
  (concatenate 'string (namestring (get-template-pathname)) "/style.css"))

(defun style-sheet()
  (with-open-file (stream (style-css-path) :direction :input)
    (slurp-stream stream)))


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

(defun get-blog-post(url-part) 
  (let ((obj (get-live-blog-post url-part)))
    (if obj
	obj
	(create-blog-post-template (post) ))))

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

(defun use-static-template(blog-post)
  (list :timestamp (fmt-timestamp (timestamp blog-post))
	:timestamp-id (timestamp blog-post)
	:type (string-downcase (type-bit blog-post))
	:title (title blog-post)
	:url-part (url-part blog-post)
	:intro (render-md (intro blog-post))))

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
  
(defun generate-index-page(tlf &key create)
  (with-output-to-string (stream)
    (let ((html-template:*string-modifier* #'identity))
      (html-template:fill-and-print-template
       (template-path "index.tmpl")
       (list :style-sheet (style-sheet)
	     (unless (get-offline?) :google-analytics) (unless (get-offline?) (get-google-analytics))
	     (if create :edit_part) (if create t) 
	     :blog-title (blog-title)
	     :main-repo-qs     (main-repo-qs)
	     :sandbox-repo-qs  (sandbox-repo-qs)
	     :contact-info     (contact-info)
	     :about            (about-page)
	     :sidebars         (collect-sidebars tlf)
	     :blog-posts       (collect-posts    tlf))
	     :stream stream))))


(defun generate-editable-index-page()
  (generate-index-page #'use-edit-template :create t))

(defun generate-blog-post-page(tmpl url-part &optional (render 'identity))
    (with-output-to-string (stream)
			   (let ((blog-post (get-live-blog-post url-part))
				 (html-template:*string-modifier* #'identity))
			     (if blog-post
				 (html-template:fill-and-print-template
				  tmpl
				  (list :style-sheet (style-sheet)
					:blog-title (blog-title)
					:title (title blog-post)
					:url_part url-part
					:timestamp (fmt-timestamp (timestamp blog-post))
					:timestamp-id (timestamp blog-post)
					:type         (string-downcase (type-bit blog-post))
					:intro (funcall render (intro blog-post)) 
					:body  (funcall render (body blog-post)))
				  :stream stream)))))


(defun view-blog-post-page()
  (generate-blog-post-page (template-path "post.tmpl") (hunchentoot:query-string) 'render-md))

;;
;; Somehow we end up with a extra space at the start of the intro and body
;; This could trigger some mark-down formatting;
;; Hence the string is stripped of spaces etc.. 
;;
(defun save-blog-post() 
  (let ((blog-post (get-live-blog-post (hunchentoot:query-string))))
    (setf (title    blog-post) (hunchentoot:post-parameter "title"))
    (setf (intro    blog-post) (str-strip (hunchentoot:post-parameter "intro")))
    (setf (body     blog-post) (str-strip (hunchentoot:post-parameter "body")))
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
    (let ((blog-post (get-blog-post (hunchentoot:query-string))))
      (multiple-value-bind 
	    (title intro body) (read-file (hunchentoot:post-parameter "file"))
	(setf (title  blog-post) title) 
	(setf (intro  blog-post) intro) 
	(setf (body   blog-post) body)
	(setf (url-part blog-post) (make-url-part (title blog-post))))
      (redirect-to-edit-page blog-post))))

(defun discard-blog-post()
  (let ((blog-posts (get-instances-by-value 'blog-post 'url-part (hunchentoot:query-string))))
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
  (cond ((eq (hunchentoot:request-method) :GET)  (create-empty-blog-post 
						  (template-path "post-edit.tmpl")
						  (hunchentoot:query-string)))
	((eq (hunchentoot:request-method) :POST) (save-blog-post))))

(defun edit-blog-post-page()
  (cond ((eq (hunchentoot:request-method) :GET)  (generate-blog-post-page 
						  (template-path "post-edit.tmpl")
						  (hunchentoot:query-string)))
	((eq (hunchentoot:request-method) :POST) (save-blog-post))))

(defun simple-template(p) 
  (with-open-file (stream p :direction :input)
    (slurp-stream stream)))

(defun generate-import-page()
  (with-output-to-string (stream)
    (let ((html-template:*string-modifier* #'identity))
      (html-template:fill-and-print-template
       (template-path "import.tmpl")
       (list :style-sheet (style-sheet)
	     :blog-title (blog-title)
	     :repo-name (infer-repo-name)
	     :remote-repo (get-remote-repo)
	     :repo-pathname  (get-repo-pathname))
       :stream stream))))

(defun do-import (file-path)
  (import-repo (directory-namestring file-path))
  (hunchentoot:redirect "/" ))

(defun import-remote ()
  (format nil "this is a test : ~A" (hunchentoot:post-parameters) ) 
)

(defun import-local()
  ( do-import (hunchentoot:post-parameter "repo-path")))


(defun import-repo-page()
  (cond ((eq (hunchentoot:request-method) :GET)  (generate-import-page))
	((eq (hunchentoot:request-method) :POST) ( do-import (hunchentoot:post-parameter "file")))))

(defun discard-blog-post-page()
  (cond ((eq (hunchentoot:request-method) :GET)  (discard-blog-post))
	((eq (hunchentoot:request-method) :POST) (discard-blog-post))))

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
		 (format stream (generate-index-page #'use-static-template)))))
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
		   (format stream (generate-blog-post-page (template-path "post.tmpl") up 'render-md)))))))
    (publish-index-page)
    (publish-other-pages)))


(defun publish-pages()
  (let ((repo-pathname (get-repo-pathname)))
    (switch-to-repo repo-pathname)
    (git-publish-branch)
    (remove-files)
    (push-pages-to-repo (namestring repo-pathname) )
    (git-add-all-posts)
    (when (get-mainstore?) (git-rm-discarded-posts))
    (git-commit)
    (git-master-branch)
    (git-merge)
    (git-publish)
    (switch-to-default-path))
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
  (get-static-page (hunchentoot:request-uri)))


(defun generate-static-pages()
  (let ((repo-path (create-if-missing (get-sandbox-pathname))))
    (push-pages-to-repo (namestring repo-path))
    (get-static-page "index.html")))
  
(setq hunchentoot:*dispatch-table* 
      (list (hunchentoot:create-regex-dispatcher "[.]html"            (protect 'static-pages))
	    (hunchentoot:create-regex-dispatcher "^/$"                (protect 'generate-editable-index-page))
	    (hunchentoot:create-regex-dispatcher "^/view/$"           (protect 'view-blog-post-page))
	    (hunchentoot:create-regex-dispatcher "^/edit/$"           (protect 'edit-blog-post-page))
	    (hunchentoot:create-regex-dispatcher "^/create/save-file" (protect 'save-file))
	    (hunchentoot:create-regex-dispatcher "^/edit/save-file"   (protect 'save-file))
	    (hunchentoot:create-regex-dispatcher "^/discard/$"        (protect 'discard-blog-post-page))
	    (hunchentoot:create-regex-dispatcher "^/import-repo/$"    (protect 'import-repo-page))
	    (hunchentoot:create-regex-dispatcher "^/import-local/$"   (protect 'import-local))
	    (hunchentoot:create-regex-dispatcher "^/import-remote/$"   (protect 'import-remote))
	    (hunchentoot:create-regex-dispatcher "^/publish/$"        (protect 'publish-pages))
	    (hunchentoot:create-regex-dispatcher "^/static-pages/$"   (protect 'generate-static-pages))
	    (hunchentoot:create-regex-dispatcher "^/undo-discards/$"  (protect 'undo-discards))
	    (hunchentoot:create-regex-dispatcher "^/drop-discards/$"  (protect 'drop-discards))
	    (hunchentoot:create-regex-dispatcher "^/create/$"         (protect 'create-blog-post-page))))

(defun start-server()
  (open-blog-store)
  (unless *bliky-server* (setf *bliky-server* (hunchentoot:start-server :port (get-bliky-port)))))

(defun stop-server()
  (hunchentoot:stop-server *bliky-server*)
  (setf *bliky-server* nil)
  (close-store))



	       