(#+quicklisp ql:quickload #-quicklisp asdf:load-system :clime)

(defun parse-parameters (args &key repl-mode-p)
  (cond ((string= (car args) "--repl")
         (parse-parameters (cdr args) :repl-mode-p t))
        ((= (length args) 1)
         (list (car args) :repl-mode repl-mode-p))
        (t
         (list "climesrv.conf" :repl-mode repl-mode-p))))

(defun start-server (conf-file)
  (let ((conf (with-open-file (in conf-file)
                (read in))))
    (destructuring-bind (&key (address   "localhost")
                              (ports     1178)
                              (learn-max 10000)
                              (cand-max  40)
                              (log-file  "./climesrv.log")
                              (log-stdout t)
                              (debug-log nil) dicts date-formats time-formats) conf
      (let ((ports (if (listp ports)
                       ports
                       (list ports))))
        (setf clime::*logger-no-debug* (not debug-log))
        (clime:controller-start :address address :ports ports
                                :learn-max learn-max :cand-max cand-max
                                :log-file log-file :log-stdout log-stdout :dicts dicts
                                :date-formats date-formats :time-formats time-formats)))))

;; REPL mode の時にサーバを停止させるための関数
(defun stop-server ()
  (clime:controller-stop)
  (uiop:quit))

;; application entry ------------------------------------------
(defun application-entry ()
  #+sbcl
  (setf sb-impl::*default-external-format*           :utf-8
        sb-alien::*default-c-string-external-format* :utf-8)
  (destructuring-bind (conf-file &key (repl-mode nil))
      (parse-parameters (uiop:command-line-arguments))
    (handler-bind (#+sbcl
                   (sb-sys:interactive-interrupt
                     (lambda (c)
                       (declare (ignore c))
                       (clime:controller-stop)
                       (return-from application-entry nil))))
      (start-server conf-file)
      (if (and repl-mode (member :sbcl *features*))
          (progn
            #+sbcl
            (sb-impl::toplevel-init))
          (loop (sleep 0.1))))))

;; load application packages ---------------------------------
(setf uiop:*image-entry-point* #'application-entry)
(uiop:dump-image +OUTPUT-FILENAME+
                 #+(and sbcl sb-core-compression) :compression #+(and sbcl sb-core-compression) t
                 :executable t)
