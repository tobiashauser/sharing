(asdf:defsystem "sharing"
  :version "0.1"
  :author "Tobias Hauser"
  :license "MIT"
  :depends-on (:hunchentoot :com.inuoe.jzon :alexandria)
  :components ((:file "main"))
  :description "A small website to share files and documents.")
