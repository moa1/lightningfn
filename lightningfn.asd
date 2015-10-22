(asdf:defsystem #:lightningfn
  :serial t
  :description "Bindings for GNU lightning using c2ffi."
  :author "Anton Moll"
  :license "LGPL3"

  :depends-on (:cl-autowrap
               :cl-plus-c)

  :components
  ((:module autowrap-spec
	    :pathname "spec"
	    :components
	    ((:static-file "lightningfn.h")))
   (:file "lightningfn")))
