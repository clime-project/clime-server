;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### dictionary-set.lisp
;;
;;　xxx
;;
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#             (:file "dictionary-set"            :depends-on ("package"
#|ASD|#                                                             "dict-entry"
#|ASD|#                                                             "dictionary"
#|ASD|#                                                             "dictionary-learn"
#|ASD|#                                                             "dictionary-lookup"
#|ASD|#                                                             "dictionary-text"
#|ASD|#                                                             "dictionary-user"
#|ASD|#                                                             "digits"
#|ASD|#                                                             "kana"
#|ASD|#                                                             "precision-stopwatch"
#|ASD|#                                                             "service.logger"
#|ASD|#                                                             "vague-matcher"))
#|EXPORT|#              ;dictionary-set.lisp
 |#

(in-package :clime)


(defun dictset-make-word-collector (max-count)
  (let ((count  0)
        (entries nil))
    (values (lambda (entry)
              (when (< count max-count)
                (prog1 t
                  (unless (member-if (lambda (e)
                                       (string= (cdr e) (cdr entry))) entries)
                    (incf count)
                    (push entry entries)))))
            (lambda () count)
            (lambda () (nreverse entries)))))

(defun dictset-do-first (dictset func)
  (labels ((recur (lst)
             (when lst
               (if (funcall func (car lst))
                   t
                   (recur (cdr lst))))))
    (recur (slot-value dictset 'dicts))))

(defun dictset-do-all (dictset func)
  (labels ((recur (lst)
             (when lst
               (funcall func (car lst))
               (recur (cdr lst)))))
    (recur (slot-value dictset 'dicts))))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### dictionary-set クラス
;;
;;${DESCRIPTION}
;;
;;　辞書の集まりをクラスとして定義します。スロットの一覧は以下の通りです。
;;
;;| name           | type      | accessor                      | initarg            |
;;|----------------|-----------|-------------------------------|--------------------|
;;| `dicts`        | `list`    | `dictionary-set-dicts`        | `:dicts`           |
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defclass dictionary-set ()
  ((dicts :initform nil :accessor dictionary-set-dicts :initarg :dicts))) ;; list


(defun dictionary-set-load (learn-size-limit params)
  (labels ((load-dictionary (type title file kind okurigana-search)
             (ecase type
               ((:learn)  (learn-dictionary-create  title file kind learn-size-limit))
               ((:user)   (user-dictionary-create   title file kind okurigana-search))
               ((:text)   (text-dictionary-create   title file kind okurigana-search))
               ((:lookup) (lookup-dictionary-create title file kind okurigana-search))))
           (recur (lst acc)
             (if (null lst)
                 (make-instance 'dictionary-set :dicts (nreverse acc))
                 (destructuring-bind (type title file kind &key (okurigana-search t)) (car lst)
                   (let ((dict (load-dictionary type title file kind okurigana-search)))
                     (recur (cdr lst) (push dict acc)))))))
    (recur params nil)))


(defmethod dictionary-search :around (dict pattern callback exact-p context1 context2)
  (if *logger-no-debug*
      (call-next-method dict pattern callback exact-p context1 context2)
      (let ((stopwatch (precision-stopwatch-start)))
        (prog1 (call-next-method dict pattern callback exact-p context1 context2)
          (multiple-value-bind (time unit) (funcall stopwatch)
            (logger-add-entry :debug "normal search takes ~F sec for ~A."
                              (float (/ time unit)) (dictionary-file-name dict)))))))

(defmethod dictionary-search-vague :around (dict pattern matcher callback)
  (if *logger-no-debug*
      (call-next-method dict pattern matcher callback)
      (let ((stopwatch (precision-stopwatch-start)))
        (prog1 (call-next-method dict pattern matcher callback)
          (multiple-value-bind (time unit) (funcall stopwatch)
            (logger-add-entry :debug "vague search takes ~F sec for ~A."
                              (float (/ time unit)) (dictionary-file-name dict)))))))

(defmethod dictionary-search-okurigana :around (dict pattern callback)
  (if *logger-no-debug*
      (call-next-method dict pattern callback)
      (let ((stopwatch (precision-stopwatch-start)))
        (prog1 (call-next-method dict pattern callback)
          (multiple-value-bind (time unit) (funcall stopwatch)
            (logger-add-entry :debug "okurigana search takes ~F sec for ~A."
                              (float (/ time unit)) (dictionary-file-name dict)))))))

(defmethod dictionary-search-afterfix :around (dict callback context1 context2)
  (if *logger-no-debug*
      (call-next-method dict callback context1 context2)
      (let ((stopwatch (precision-stopwatch-start)))
        (prog1 (call-next-method dict callback context1 context2)
          (multiple-value-bind (time unit) (funcall stopwatch)
            (logger-add-entry :debug "after-fix search takes ~F sec for ~A."
                              (float (/ time unit)) (dictionary-file-name dict)))))))


(defun dictionary-set-search (dictset pattern max-count &optional exact-p context1 context2)
  (logger-add-entry :debug "(dictionary-set-search ~S ~A ~A ~S ~S)"
                    pattern max-count exact-p context1 context2)
  (multiple-value-bind (collector get-count get-entries) (dictset-make-word-collector max-count)
    (labels ((add-hira-kata-ascii (h k a)
               (funcall collector (make-entry pattern h))
               (funcall collector (make-entry pattern k))
               (funcall collector (make-entry pattern a)))
             (add-when-digits ()
               (digits-candidates pattern (lambda (word)
                                            (funcall collector (make-entry nil word)))))
             (recur-normal (base lst from-learn) ; returns 'count of entries from learn-dict.'
               (if (null lst)
                   (prog1 from-learn
                     (logger-add-entry :info "~A search...~A"
                                       (if exact-p "exact" "normal") (- (funcall get-count) base)))
                   (let* ((dict   (car lst))
                          (before (or (funcall get-count) 0))
                          (result (dictionary-search dict pattern collector exact-p context1 context2)))
                     (when (eq :learn (dictionary-type dict))
                       (setf from-learn (- (funcall get-count) before)))
                     (recur-normal base (when result (cdr lst)) from-learn))))
             (recur-vague (base lst pattern matcher)
               (if (null lst)
                   (prog1 nil
                     (logger-add-entry :info "vague search...~A" (- (funcall get-count) base)))
                   (let ((result (dictionary-search-vague (car lst) pattern matcher collector)))
                     (recur-vague base (when result (cdr lst)) pattern matcher))))
             (recur-okurigana (lst callback)
               (when lst
                 (let ((result (if (not (dictionary-okurigana-search (car lst)))
                                   t
                                   (dictionary-search-okurigana (car lst) pattern callback))))
                   (recur-okurigana (when result (cdr lst)) callback))))
             (recur-afterfix (base lst)
               (if (null lst)
                   (prog1 nil
                     (logger-add-entry :info "after-fix search...~A" (- (funcall get-count) base)))
                   (let ((result (dictionary-search-afterfix (car lst) collector
                                                             (or context1 "") (or context2 ""))))
                     (recur-afterfix base (when result (cdr lst))))))
             (search-okurigana (dicts)
               (let ((tmp  nil)
                     (base (funcall get-count)))
                 ;;MEMO : 送りがな検索では個数無制限で候補を回収し、パターン長順に登録する
                 (recur-okurigana dicts (lambda (entry)
                                          (prog1 t (push entry tmp))))
                 (labels ((recur (lst)
                            (when lst
                              (when (funcall collector
                                             (make-entry pattern (word-of (car lst))))
                                (recur (cdr lst))))))
                   (recur (stable-sort tmp (lambda (e1 e2)
                                             (> (length (car e1)) (length (car e2)))))))
                 (logger-add-entry :info "okurigana search...~A" (- (funcall get-count) base)))))
      (with-slots (dicts) dictset
        (if exact-p
            ;;完全一致検索の場合
            (multiple-value-bind (h k a err) (kana-convert-from-pattern pattern)
              (add-hira-kata-ascii h k a)					;; 先にひらがな/カタカナ/ASCIIを候補にする
              (add-when-digits)								;; 数字列変換
              (recur-normal (funcall get-count) dicts 0)	;; 通常検索
              (when (zerop err)
                (search-okurigana dicts)))                  ;; kana-convert 完全成功なら「送りがな検索」
            ;;前方一致検索の場合
            (if (string= pattern "")
                ;;空パターンの場合：確定後自動検索
                (recur-afterfix (funcall get-count) dicts)
                ;;上記以外の場合
                (progn
                  ;; 数字列変換
                  (add-when-digits)
                  ;;通常の前方一致検索
                  (let* ((count-of-learn  (recur-normal (funcall get-count) dicts 0)) ; 通常検索実施
                         (only-from-learn (= (funcall get-count) count-of-learn)))
                    ;; max-count 未達なら後続検索実施
                    (when (< (funcall get-count) max-count)
                      ;; ここで kana-convert 実施
                      (multiple-value-bind (h k a err) (kana-convert-from-pattern pattern)
                        ;; kana-convert 完全成功なら「送りがな検索」
                        (when (zerop err)
                          (search-okurigana dicts))
                        ;; ここでひらがな/カタカナ/ASCIIを候補に追加
                        (add-hira-kata-ascii h k a)))
                    ;; max-count にまだ未達で学習辞書以外からの候補無しならば曖昧検索を実施
                    (when (and only-from-learn (< (funcall get-count) max-count))
                      (let ((matcher (vague-matcher-create pattern)))
                        ;;MEMO: pattern が長過ぎる場合や非 ASCII 文字がある場合は NIL が返る
                        (when matcher
                          (recur-vague (funcall get-count) dicts pattern matcher)))))))))
      (let ((count   (funcall get-count))
            (entries (funcall get-entries)))
        (logger-add-entry :debug "result list is ~S" entries)
        (logger-add-entry :info  "result count is ~A" count)
        (values entries count)))))

(defun dictionary-set-register-word (dictset pattern word)
  (dictset-do-first dictset
                    (lambda (dict)
                      (when (dictionary-register-word dict pattern word)
                        (prog1 t
                          (logger-add-entry :info "register (~S . ~S) to ~A"
                                            pattern word (dictionary-file-name dict)))))))


(defun dictionary-set-unregister-word (dictset pattern word)
  (let ((count 0))
    (labels ((callback (dict entry)
               (logger-add-entry :info "unregister ~A from ~A"
                                 entry (dictionary-file-name dict))
               (incf count))
             (impl (dict)
               (dictionary-unregister-word dict pattern word #'callback)))
      (dictset-do-all dictset #'impl)
      (< 0 count))))

(defun dictionary-set-learn (dictset context1 context2 entry)
  (dictset-do-first dictset
                    (lambda (dict)
                      (when (dictionary-learn dict context1 context2 entry)
                        (prog1 t
                          (logger-add-entry :info "learn (~S ~S (~S . ~S)) to ~A"
                                            context1 context2 (pattern-of entry)
                                            (word-of entry) (dictionary-file-name dict)))))))

(defun dictionary-set-modified (dictset)
  (dictset-do-first dictset
                    (lambda (dict)
                      (dictionary-modified dict))))

(defun dictionary-set-save (dictset)
  (let ((save-p nil))
    (dolist (dict (dictionary-set-dicts dictset))
      (when (dictionary-save dict)
        (logger-add-entry :info "dictionary ~A saved." (dictionary-file-name dict))
        (setf save-p t)))
    (unless save-p
      (logger-add-entry :info "no dictionary modified."))
    save-p))

