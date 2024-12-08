(defsystem :clime
  :description "clime server program."
  :version     "0.1.0"
  :depends-on  ("bordeaux-threads"
                "cl-fad"
                "cl-ppcre"
                "usocket")
  :components ((:module "src"
                :components (;; ---------------------------------------- BEGIN COMPONENTS
                             (:file "client-data"           :depends-on ("package"))
                             (:file "console"               :depends-on ("package"
                                                                         "client-data"
                                                                         "protocols"
                                                                         "service.logger"
                                                                         "service.port-listener"))
                             (:file "controller"            :depends-on ("package"
                                                                         "client-data"
                                                                         "datetime-coder"
                                                                         "dictionary-set"
                                                                         "digits"
                                                                         "service.logger"
                                                                         "service.port-listener"
                                                                         "service.transactor"))
                             (:file "datetime-coder"        :depends-on ("package"))
                             (:file "dict-entry"            :depends-on ("package"))
                             (:file "dictionary-learn"      :depends-on ("package"
                                                                         "dict-entry"
                                                                         "dictionary"
                                                                         "utility"))
                             (:file "dictionary-lookup"     :depends-on ("package"
                                                                         "dict-entry"
                                                                         "dictionary"))
                             (:file "dictionary-set"        :depends-on ("package"
                                                                         "dict-entry"
                                                                         "dictionary"
                                                                         "dictionary-learn"
                                                                         "dictionary-lookup"
                                                                         "dictionary-text"
                                                                         "dictionary-user"
                                                                         "digits"
                                                                         "kana"
                                                                         "precision-stopwatch"
                                                                         "service.logger"
                                                                         "vague-matcher"))
                             (:file "dictionary-text"       :depends-on ("package"
                                                                         "dict-entry"
                                                                         "dictionary"))
                             (:file "dictionary-user"       :depends-on ("package"
                                                                         "dict-entry"
                                                                         "dictionary"
                                                                         "dictionary-text"
                                                                         "utility"))
                             (:file "dictionary"            :depends-on ("package"
                                                                         "dict-entry"
                                                                         "kana"))
                             (:file "digits"                :depends-on ("package"
                                                                         "datetime-coder"
                                                                         "utility"))
                             (:file "kana"                  :depends-on ("package"
                                                                         "utility"))
                             (:file "package")
                             (:file "precision-stopwatch"   :depends-on ("package"))
                             (:file "protocols"             :depends-on ("package"
                                                                         "client-data"
                                                                         "dict-entry"
                                                                         "dictionary"
                                                                         "dictionary-set"
                                                                         "service.logger"
                                                                         "utility"))
                             (:file "queue"                 :depends-on ("package"))
                             (:file "service"               :depends-on ("package"))
                             (:file "service.logger"        :depends-on ("package"
                                                                         "queue"
                                                                         "service"
                                                                         "utility"))
                             (:file "service.port-listener" :depends-on ("package"
                                                                         "client-data"
                                                                         "protocols"
                                                                         "service"
                                                                         "service.logger"
                                                                         "service.transactor"
                                                                         "transaction"))
                             (:file "service.transactor"    :depends-on ("package"
                                                                         "precision-stopwatch"
                                                                         "protocols"
                                                                         "queue"
                                                                         "service"
                                                                         "service.logger"
                                                                         "transaction"
                                                                         "utility"))
                             (:file "transaction"           :depends-on ("package"
                                                                         "queue"))
                             (:file "utility"               :depends-on ("package"))
                             (:file "vague-matcher"         :depends-on ("package"))
                             ;; ------------------------------------------ END COMPONENTS
))))
