(asdf:defsystem #:lightningfn
  :description "Bindings for GNU lightning using CFFI and TRIVIAL-GARBAGE."
  :author "Anton Moll"
  :license "LGPL3"

  :depends-on (:cffi :trivial-garbage)

  :serial t
  :components
  ((:file "package")
   (:file "library")
   (:file "ffi")
   (:file "interface")
   ))
