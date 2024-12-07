;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### datetime-coder.lisp
;;
;;　日付・時刻のフォーマットを実装します。
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#             (:file "datetime-coder"            :depends-on ("package"))
#|EXPORT|#              ;datetime-coder.lisp
|#

(in-package :clime)

(defparameter *dtcoder-month-names* '("January" "February" "March"  "Aplil"
                                      "May" "June" "July"     "August" "September"
                                      "October" "November" "December"))
(defparameter *dtcoder-week-days1*   '("Monday"   "Tuesday" "Wednesday"
                                       "Thursday" "Friday"  "Saturday" "Sunday"))
(defparameter *dtcoder-week-days2*  '("月曜日" "火曜日" "水曜日"
                                      "木曜日" "金曜日" "土曜日" "日曜日"))

(defun dtcoder-now ()
  (let ((value (get-universal-time)))
    (multiple-value-list (decode-universal-time value))))

(defun dtcoder-make-date (year month date)
  (let ((value (encode-universal-time 0 0 0 date month year)))
    (multiple-value-list (decode-universal-time value))))

(defun dtcoder-make-time (hour minute second)
  (let ((list (dtcoder-now)))
    (setf (first  list) second)
    (setf (second list) minute)
    (setf (third  list)   hour)
    list))

(defun dtcoder-hour-impl (hour long-p ampm-p)
  (when ampm-p
    (setq hour (cond
                 ((zerop hour) 12)
                 ((< 12 hour)  (- hour 12))
                 (t            hour))))
  (format nil (if long-p "~2,'0D" "~D") hour))

(defun dtcoder-ampm-impl (hour long-p upcase-p)
  (if (< hour 12)
      (if long-p
          (if upcase-p "AM" "am")
          (if upcase-p "A"  "a" ))
      (if long-p
          (if upcase-p "PM" "pm")
          (if upcase-p "P"  "p" ))))

(defun dtcoder-D-yyyy  (dt ampm) (declare (ignore ampm)) (format nil "~4,'0D" (nth 5 dt)))
(defun dtcoder-D-yy    (dt ampm) (declare (ignore ampm)) (format nil "~2,'0D" (mod (nth 5 dt) 100)))
(defun dtcoder-D-mmmmm (dt ampm) (declare (ignore ampm)) (subseq (nth (1- (nth 4 dt))
                                                                      *dtcoder-month-names*) 0 1))
(defun dtcoder-D-mmmm  (dt ampm) (declare (ignore ampm)) (nth (1- (nth 4 dt)) *dtcoder-month-names*))
(defun dtcoder-D-mmm   (dt ampm) (declare (ignore ampm)) (subseq (nth (1- (nth 4 dt))
                                                                      *dtcoder-month-names*) 0 3))
(defun dtcoder-D-mm    (dt ampm) (declare (ignore ampm)) (format nil "~2,'0D" (nth 4 dt)))
(defun dtcoder-D-m     (dt ampm) (declare (ignore ampm)) (format nil "~D"   (nth 4 dt)))
(defun dtcoder-D-dddd  (dt ampm) (declare (ignore ampm)) (nth (nth 6 dt) *dtcoder-week-days1*))
(defun dtcoder-D-ddd   (dt ampm) (declare (ignore ampm)) (subseq (nth (nth 6 dt)
                                                                      *dtcoder-week-days1*) 0 3))
(defun dtcoder-D-dd    (dt ampm) (declare (ignore ampm)) (format nil "~2,'0D" (nth 3 dt)))
(defun dtcoder-D-d     (dt ampm) (declare (ignore ampm)) (format nil "~D"   (nth 3 dt)))
(defun dtcoder-D-aaaa  (dt ampm) (declare (ignore ampm)) (nth (nth 6 dt) *dtcoder-week-days2*))
(defun dtcoder-D-aaa   (dt ampm) (declare (ignore ampm)) (subseq (nth (nth 6 dt)
                                                                      *dtcoder-week-days2*) 0 1))
(defun dtcoder-T-HH    (dt ampm) (dtcoder-hour-impl (nth 2 dt) t   ampm))
(defun dtcoder-T-H     (dt ampm) (dtcoder-hour-impl (nth 2 dt) nil ampm))
(defun dtcoder-T-MM    (dt ampm) (declare (ignore ampm)) (format nil "~2,'0D" (nth 1 dt)))
(defun dtcoder-T-M     (dt ampm) (declare (ignore ampm)) (format nil "~D"   (nth 1 dt)))
(defun dtcoder-T-SS    (dt ampm) (declare (ignore ampm)) (format nil "~2,'0D" (nth 0 dt)))
(defun dtcoder-T-S     (dt ampm) (declare (ignore ampm)) (format nil "~D"   (nth 0 dt)))
(defun dtcoder-AM/PM-U (dt ampm) (declare (ignore ampm)) (dtcoder-ampm-impl (nth 2 dt) t   t  ))
(defun dtcoder-am/pm-L (dt ampm) (declare (ignore ampm)) (dtcoder-ampm-impl (nth 2 dt) t   nil))
(defun dtcoder-A/P-U   (dt ampm) (declare (ignore ampm)) (dtcoder-ampm-impl (nth 2 dt) nil t  ))
(defun dtcoder-a/p-L   (dt ampm) (declare (ignore ampm)) (dtcoder-ampm-impl (nth 2 dt) nil nil))


(defun dtcoder-make-stamper (fmt)
  (labels ((chk (str target)
             (let ((len (length target)))
               (and (<= len (length str))
                    (string= target (subseq str 0 len)))))
           (recur (acc rest ampm)
             (if (zerop (length rest))
                 (cons fmt (cons ampm acc))
                 (cond
                   ((chk rest "yyyy")  (recur (cons #'dtcoder-D-yyyy  acc) (subseq rest 4) ampm))
                   ((chk rest "yy")    (recur (cons #'dtcoder-D-yy    acc) (subseq rest 2) ampm))
                   ((chk rest "mmmmm") (recur (cons #'dtcoder-D-mmmmm acc) (subseq rest 5) ampm))
                   ((chk rest "mmmm")  (recur (cons #'dtcoder-D-mmmm  acc) (subseq rest 4) ampm))
                   ((chk rest "mmm")   (recur (cons #'dtcoder-D-mmm   acc) (subseq rest 3) ampm))
                   ((chk rest "mm")    (recur (cons #'dtcoder-D-mm    acc) (subseq rest 2) ampm))
                   ((chk rest "m")     (recur (cons #'dtcoder-D-m     acc) (subseq rest 1) ampm))
                   ((chk rest "dddd")  (recur (cons #'dtcoder-D-dddd  acc) (subseq rest 4) ampm))
                   ((chk rest "ddd")   (recur (cons #'dtcoder-D-ddd   acc) (subseq rest 3) ampm))
                   ((chk rest "dd")    (recur (cons #'dtcoder-D-dd    acc) (subseq rest 2) ampm))
                   ((chk rest "d")     (recur (cons #'dtcoder-D-d     acc) (subseq rest 1) ampm))
                   ((chk rest "aaaa")  (recur (cons #'dtcoder-D-aaaa  acc) (subseq rest 4) ampm))
                   ((chk rest "aaa")   (recur (cons #'dtcoder-D-aaa   acc) (subseq rest 3) ampm))
                   ((chk rest "HH")    (recur (cons #'dtcoder-T-HH    acc) (subseq rest 2) ampm))
                   ((chk rest "H")     (recur (cons #'dtcoder-T-H     acc) (subseq rest 1) ampm))
                   ((chk rest "MM")    (recur (cons #'dtcoder-T-MM    acc) (subseq rest 2) ampm))
                   ((chk rest "M")     (recur (cons #'dtcoder-T-M     acc) (subseq rest 1) ampm))
                   ((chk rest "SS")    (recur (cons #'dtcoder-T-SS    acc) (subseq rest 2) ampm))
                   ((chk rest "S")     (recur (cons #'dtcoder-T-S     acc) (subseq rest 1) ampm))
                   ((chk rest "AM/PM") (recur (cons #'dtcoder-AM/PM-U acc) (subseq rest 5) t))
                   ((chk rest "am/pm") (recur (cons #'dtcoder-am/pm-L acc) (subseq rest 5) t))
                   ((chk rest "A/P")   (recur (cons #'dtcoder-A/P-U   acc) (subseq rest 3) t))
                   ((chk rest "a/p")   (recur (cons #'dtcoder-a/p-L   acc) (subseq rest 3) t))
                   (t                  (recur (cons (subseq rest 0 1) acc) (subseq rest 1) ampm))))))
    (recur nil fmt nil)))

(defun dtcoder-make-stampers (fmts)
  (let ((lst nil))
    (dolist (fmt fmts)
      (push (dtcoder-make-stamper fmt) lst))
    (nreverse lst)))

(defun dtcoder-check-stampers-updated (fmts stampers)
  (if (and (null fmts) (null stampers))
      t
      (if (or (null fmts) (null stampers))
          nil
          (if (not (string= (car fmts) (caar stampers)))
              nil
              (dtcoder-check-stampers-updated (cdr fmts) (cdr stampers))))))


(defun dtcoder-format-timestamp (stamper dt)
  (let ((ampm-p (cadr stamper)))
    (labels ((recur (stamper acc)
               (if (null stamper)
                   (apply #'concatenate 'string acc)
                   (let ((elm (car stamper)))
                     (recur (cdr stamper)
                            (push (if (stringp elm)
                                      elm
                                      (funcall elm dt ampm-p)) acc))))))
      (recur (cddr stamper) nil))))

#|
(defparameter *date-formats*  (list "yyyy年mm月dd日(aaa)" "yyyy/mm/dd"))
(defparameter *time-formats*  (list "HH:MM:SS" "HH:MM" "H時M分S秒" "H時M分"))
(defparameter *date-stampers* nil)
(defparameter *time-stampers* nil)

(unless (dtcoder-check-stampers-updated *date-formats* *date-stampers*)
  (setf *date-stampers* (dtcoder-make-stampers *date-formats*)))
(unless (dtcoder-check-stampers-updated *time-formats* *time-stampers*)
  (setf *time-stampers* (dtcoder-make-stampers *time-formats*)))


(dtcoder-format-timestamp *stamper* (dtcoder-now))

(let ((data (dtcoder-make-date 2024 6 7)))
  (mapcar (lambda (stamper)
            (dtcoder-format-timestamp stamper data)) *date-stampers*))

(let ((data (dtcoder-make-time 10 20 45)))
  (mapcar (lambda (stamper)
            (dtcoder-format-timestamp stamper data)) *time-stampers*))

 |#

