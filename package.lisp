(defpackage #:lightningfn-ffi)
(defpackage #:lightningfn-ffi.functions)
(defpackage #:lightningfn-ffi.accessors)

(defpackage #:lightningfn
  (:use #:cl #:autowrap.minimal #:plus-c #:trivial-garbage
	#:lightningfn-ffi.functions #:lightningfn-ffi.accessors)
  (:import-from :cffi
		#:mem-ref #:with-foreign-objects #:with-foreign-object
		#:foreign-alloc #:foreign-free #:null-pointer-p)
  (:export
   #:with-init
   #:new-state))

