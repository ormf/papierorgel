;;;; papierorgel.asd

(asdf:defsystem #:papierorgel
  :description "Eigener Code für Nutzung der Papierorgel"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de"
  :depends-on (#:ats-cuda #:cl-orgelctl)
  :license  "Proprietär"
  :version "0.0.1"
  :serial t
  :components ((:file "init")))
