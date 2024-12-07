(require :clime)
(require :sb-posix)

(defun parse-parameters (args)
  (let ((repl-mode-p nil))
    (when args
      (when (string= (car args) "--repl")
        (setf repl-mode-p t)
        (setf args (cdr args)))
      (when (= 1 (length args))
        (list (car args) :repl-mode repl-mode-p)))))

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
  (sb-ext:exit))

;; application entry ------------------------------------------
(defun application-entry ()
  (setf sb-impl::*default-external-format*           :utf-8)
  (setf sb-alien::*default-c-string-external-format* :utf-8)
  (destructuring-bind (conf-file &key (repl-mode nil)) (parse-parameters (cdr *posix-argv*))
    (handler-bind ((sb-sys:interactive-interrupt
                     (lambda (c)
                       (declare (ignore c))
                       (clime:controller-stop)
                       (return-from application-entry nil))))
      (start-server conf-file)
      (if repl-mode
          (sb-impl::toplevel-init)
          (loop (sleep 0.1))))))

;; load application packages ---------------------------------
(sb-ext:save-lisp-and-die +OUTPUT-FILENAME+
                          :toplevel #'application-entry
                          :compression          #+:OS-WINDOWS nil #-:OS-WINDOWS t
                          :executable           t
                          :save-runtime-options t)
