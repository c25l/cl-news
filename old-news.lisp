(ql:quickload '(:cl-feedparser :drakma :hunchentoot :cl-json :alexandria :flexi-streams :cl-cron :cl-ppcre))

(defparameter *config* '((:feeds "")
			(:location . "BOU/59,80")))

(defparameter *config-loc* "config.json")
(defparameter *page* "")
(defparameter *old* "")
(defparameter *current* "")

(defun startup (loc)
  ;; reads config data from a location of choice.
  (let ((json (cl-json:decode-json-from-source (open loc))))
    (setf *config* json)
    ))

(defun save-config (loc)
  ;; writes the config file to a location of choice.
  (with-open-file (f loc :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
      (write-sequence (cl-json:encode-json-alist-to-string *config*) f)))


(defun fetch (url)
  ;; get the feed from a url and parse the thing.
  (cl-feedparser:parse-feed (drakma:http-request url)))

(defun entry-as-html (entry)
  ;; take an entry hash and return the html you want
  (concatenate
   'string
   "<h2>"
   (gethash :title entry)
   "</h2><br>"
   (gethash :summary entry)
   "<a href=\""(gethash :link entry)
   "\">go</a><br>"
   ))

(defun get-y-m-d (decoded-time-returner)
  (multiple-value-bind (s mi h d m y)
      (funcall decoded-time-returner)
    (declare (ignore s mi h))
    (list y m d)))

(defun entry-validp (entry)
  ;; could be anything, for now the date
  (let* ((date (get-y-m-d 'get-decoded-time))
	(y1 (car date))
	(m1 (cadr date))
	(d1 (caddr date))
	(entry-date (get-y-m-d (lambda () (decode-universal-time (gethash :published-parsed entry)))))
	(y2 (car entry-date))
	(m2 (cadr entry-date))
	(d2 (caddr entry-date)))
    (or (and (= y1 y2)
	     (= m1 m2)
	     (= d1 (1+ d2)))
	(and (= d1 1)
	     (= m1 1)
	     (= y1 (1+ y2)))
	(and (= y1 y2)
	     (= m1 (1+ m2))
	     (= d1 1)))))

(defun source-data-as-html (source)
  ;; simply put these into a form you want to look at.
  (concatenate
   'string
   "<hr><h1>"
   (gethash :title source)
   "</h1>"
   ))

(defun weather-as-html ()
(let* ((data (cl-json:decode-json-from-string
	     (flexi-streams:octets-to-string
	      (drakma:http-request
	       "https://api.weather.gov/gridpoints/BOU/59,80/forecast"
	       ) :external-format :utf-8))

	     )
       (entries (mapcar #'(lambda (x)
	      (alexandria:assoc-value x :detailed-forecast))
	  (alexandria:assoc-value
	   (alexandria:assoc-value data :properties) :periods)
	  )))
       (concatenate 'string
		    "<h1>Weather</h1><h3>Today</h3><br>"
		    (car entries)
		    "<br><h3>Tomorrow</h3><br>"
		    (Cadr entries)
		    "<br><h3>Day after Tomorrow</h3><br>"
		    (caddr entries)
		    "<br>")
       ))


(defun string-date ()
   (let* ((date (get-y-m-d 'get-decoded-time))
	 (y (car date))
	 (m (cadr date))
	 (d (caddr date)))
    (format nil "<h1>~a</h1>"
	    (+ (* 10000 y) (* 100 m)  d))))


(defun genpage (newstuff oldstuff)
  ;; get a page together.
  (cl-ppcre:regex-replace
   "<(|/)img[^>]*>"
   (concatenate
   'string
   "<html><title>Old News</title><body>"
   newstuff
   "<br><hr><br>"
   oldstuff
   "</html></body>")
 "")
  )

(defun update ()
  ;; run the update look and shuffle the bits around to make it do two days worth.
  (setf *old* *current*)
  (setf *current* (concatenate
		   'string
		   (string-date)
		   "<hr>"
		   (weather-as-html)))
  (loop for loc in *feeds*
       do (let* ((temp "")
	      (feed (fetch loc))
	      (name (source-data-as-html feed)))
	    (loop for entry in (gethash :entries feed)
	       do (when (entry-validp entry)
		    (setf temp (concatenate
				'string temp
				(entry-as-html entry)))))
	    (princ "updated ")
	    (print loc)
	    (setf *current* (concatenate
			     'string
			     *current*
			     name
			     (if  (string= "" temp)
				  "nothing"
			       temp
			       )))))
  (setf *page* (genpage *current* *old*)))


(hunchentoot:define-easy-handler (news :uri "/news") ()
  (setf (hunchentoot:content-type*) "text/html")
  *page*)
(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))
;; server bits. This could legit work.
;; make a page for today, keep a page for yesterday, serve them both
;; consecutively.
(cl-cron:make-cron-job 'update :minute 30 :hour 12)
(cl-cron:start-cron)
(update)

(loop do
(sleep (* 60 60 24)))
