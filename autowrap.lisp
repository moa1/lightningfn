(cl:in-package :lightningfn-ffi)

(cl:setf autowrap:*c2ffi-program* "/home/toni/soft/c2ffi-master/src/c2ffi")

(autowrap:c-include
 '(lightningfn autowrap-spec "lightningfn.h")
 :function-package :lightningfn-ffi.functions
 :accessor-package :lightningfn-ffi.accessors
 :sysincludes '("/usr/include/" "/usr/include/i386-linux-gnu/" "/usr/include/linux/")
 :exclude-sources ("/usr/include/*")
 :spec-path '(lightningfn autowrap-spec)
 :no-accessors cl:t ;not neccesary because there are no structs in lightningfn? but there are types like jit_state... what does :no-accessors mean?
 )
