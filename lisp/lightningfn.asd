(asdf:defsystem #:lightningfn
  :description "Bindings for GNU lightning using c2ffi."
  :author "Anton Moll"
  :license "LGPL3"

  :depends-on (:cffi :trivial-garbage)

  :serial t
  :components
  ((:module autowrap-spec
	    :pathname "spec"
	    :components
	    ((:static-file "lightningfn.h")))
   (:file "package")
   (:file "library")
   (:file "autowrap")
   (:file "interface")
   ))
