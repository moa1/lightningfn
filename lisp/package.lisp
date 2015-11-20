(defpackage #:lightningfn-ffi
  (:use #:cl #:cffi #:trivial-garbage))

(defpackage #:lightningfn
  (:nicknames #:jit)
  (:documentation "A LISP interface to GNU lightning.
To avoid collisions between function names in GNU lightning and functions in LISP package :CL, some function names are changed:
  print       = print-jit
  disassemble = disassemble-jit
Author: Anton Moll")
  (:use #:cl #:lightningfn-ffi #:cffi)
  ;;(:shadow #:print #:disassemble)
  ;; (:import-from :cffi
  ;; 		#:mem-ref #:with-foreign-objects #:with-foreign-object
  ;; 		#:foreign-alloc #:foreign-free #:null-pointer-p)
  (:export
   #:with-init
   #:new-state
   #:reg-r
   #:reg-v
   #:reg-f
   #:*jit*
   #:with-new-state
   #:clear-state
   #:imperative-bind #:set!
   #:with-jit
   ;; generated by (progn (mapc (lambda (x) (format t "#:~A~%" (lightningfn::convert-package x :cl-user))) lightningfn::*to-be-exported-functions*) nil)
   #:ABSR-D
   #:ABSR-F
   #:ADDCI
   #:ADDCR
   #:ADDI
   #:ADDI-D
   #:ADDI-F
   #:ADDR
   #:ADDR-D
   #:ADDR-F
   #:ADDRESS
   #:ADDXI
   #:ADDXR
   #:ALIGN
   #:ALLOCAI
   #:ANDI
   #:ANDR
   #:ARG
   #:ARG-D
   #:ARG-F
   #:ARG-REGISTER-P
   #:BEQI
   #:BEQI-D
   #:BEQI-F
   #:BEQR
   #:BEQR-D
   #:BEQR-F
   #:BGEI
   #:BGEI-D
   #:BGEI-F
   #:BGEI-U
   #:BGER
   #:BGER-D
   #:BGER-F
   #:BGER-U
   #:BGTI
   #:BGTI-D
   #:BGTI-F
   #:BGTI-U
   #:BGTR
   #:BGTR-D
   #:BGTR-F
   #:BGTR-U
   #:BLEI
   #:BLEI-D
   #:BLEI-F
   #:BLEI-U
   #:BLER
   #:BLER-D
   #:BLER-F
   #:BLER-U
   #:BLTGTI-D
   #:BLTGTI-F
   #:BLTGTR-D
   #:BLTGTR-F
   #:BLTI
   #:BLTI-D
   #:BLTI-F
   #:BLTI-U
   #:BLTR
   #:BLTR-D
   #:BLTR-F
   #:BLTR-U
   #:BMCI
   #:BMCR
   #:BMSI
   #:BMSR
   #:BNEI
   #:BNEI-D
   #:BNEI-F
   #:BNER
   #:BNER-D
   #:BNER-F
   #:BOADDI
   #:BOADDI-U
   #:BOADDR
   #:BOADDR-U
   #:BORDI-D
   #:BORDI-F
   #:BORDR-D
   #:BORDR-F
   #:BOSUBI
   #:BOSUBI-U
   #:BOSUBR
   #:BOSUBR-U
   #:BUNEQI-D
   #:BUNEQI-F
   #:BUNEQR-D
   #:BUNEQR-F
   #:BUNGEI-D
   #:BUNGEI-F
   #:BUNGER-D
   #:BUNGER-F
   #:BUNGTI-D
   #:BUNGTI-F
   #:BUNGTR-D
   #:BUNGTR-F
   #:BUNLEI-D
   #:BUNLEI-F
   #:BUNLER-D
   #:BUNLER-F
   #:BUNLTI-D
   #:BUNLTI-F
   #:BUNLTR-D
   #:BUNLTR-F
   #:BUNORDI-D
   #:BUNORDI-F
   #:BUNORDR-D
   #:BUNORDR-F
   #:BXADDI
   #:BXADDI-U
   #:BXADDR
   #:BXADDR-U
   #:BXSUBI
   #:BXSUBI-U
   #:BXSUBR
   #:BXSUBR-U
   #:CALLEE-SAVE-P
   #:CALLI
   #:CALLR
   #:COMR
   #:DISASSEMBLE-JIT
   #:DIVI
   #:DIVI-D
   #:DIVI-F
   #:DIVI-U
   #:DIVR
   #:DIVR-D
   #:DIVR-F
   #:DIVR-U
   #:ELLIPSIS
   #:EMIT
   #:EPILOG
   #:EQI
   #:EQI-D
   #:EQI-F
   #:EQR
   #:EQR-D
   #:EQR-F
   #:EXTR-C
   #:EXTR-D
   #:EXTR-D-F
   #:EXTR-F
   #:EXTR-F-D
   #:EXTR-S
   #:EXTR-UC
   #:EXTR-US
   #:FINISHI
   #:FINISHR
   #:FORWARD
   #:FORWARD-P
   #:FRAME
   #:GEI
   #:GEI-D
   #:GEI-F
   #:GEI-U
   #:GER
   #:GER-D
   #:GER-F
   #:GER-U
   #:GET-CODE
   #:GET-DATA
   #:GET-NOTE
   #:GETARG
   #:GETARG
   #:GETARG-C
   #:GETARG-D
   #:GETARG-F
   #:GETARG-I
   #:GETARG-S
   #:GETARG-UC
   #:GETARG-US
   #:GTI
   #:GTI-D
   #:GTI-F
   #:GTI-U
   #:GTR
   #:GTR-D
   #:GTR-F
   #:GTR-U
   #:HTONR
   #:HTONR
   #:HTONR-UI
   #:HTONR-US
   #:INDIRECT
   #:INDIRECT-P
   #:JMPI
   #:JMPR
   #:LABEL
   #:LDI
   #:LDI
   #:LDI-C
   #:LDI-D
   #:LDI-F
   #:LDI-I
   #:LDI-S
   #:LDI-UC
   #:LDI-US
   #:LDR
   #:LDR
   #:LDR-C
   #:LDR-D
   #:LDR-F
   #:LDR-I
   #:LDR-S
   #:LDR-UC
   #:LDR-US
   #:LDXI
   #:LDXI
   #:LDXI-C
   #:LDXI-D
   #:LDXI-F
   #:LDXI-I
   #:LDXI-S
   #:LDXI-UC
   #:LDXI-US
   #:LDXR
   #:LDXR
   #:LDXR-C
   #:LDXR-D
   #:LDXR-F
   #:LDXR-I
   #:LDXR-S
   #:LDXR-UC
   #:LDXR-US
   #:LEI
   #:LEI-D
   #:LEI-F
   #:LEI-U
   #:LER
   #:LER-D
   #:LER-F
   #:LER-U
   #:LINK
   #:LIVE
   #:LSHI
   #:LSHR
   #:LTGTI-D
   #:LTGTI-F
   #:LTGTR-D
   #:LTGTR-F
   #:LTI
   #:LTI-D
   #:LTI-F
   #:LTI-U
   #:LTR
   #:LTR-D
   #:LTR-F
   #:LTR-U
   #:MOVI
   #:MOVI-D
   #:MOVI-D-W
   #:MOVI-D-WW
   #:MOVI-F
   #:MOVI-F-W
   #:MOVR
   #:MOVR-D
   #:MOVR-D-W
   #:MOVR-D-WW
   #:MOVR-F
   #:MOVR-F-W
   #:MOVR-W-D
   #:MOVR-W-F
   #:MOVR-WW-D
   #:MULI
   #:MULI-D
   #:MULI-F
   #:MULR
   #:MULR-D
   #:MULR-F
   #:NAME
   #:NEGR
   #:NEGR-D
   #:NEGR-F
   #:NEI
   #:NEI-D
   #:NEI-F
   #:NER
   #:NER-D
   #:NER-F
   #:NOTE
   #:NTOHR
   #:NTOHR
   #:NTOHR-UI
   #:NTOHR-US
   #:ORDI-D
   #:ORDI-F
   #:ORDR-D
   #:ORDR-F
   #:ORI
   #:ORR
   #:PATCH
   #:PATCH-ABS
   #:PATCH-AT
   #:POINTER-P
   #:PREPARE
   #:PRINT-JIT
   #:PROLOG
   #:PUSHARGI
   #:PUSHARGI-D
   #:PUSHARGI-F
   #:PUSHARGR
   #:PUSHARGR-D
   #:PUSHARGR-F
   #:PUTARGI
   #:PUTARGI-D
   #:PUTARGI-F
   #:PUTARGR
   #:PUTARGR-D
   #:PUTARGR-F
   #:QDIVI
   #:QDIVI-U
   #:QDIVR
   #:QDIVR-U
   #:QMULI
   #:QMULI-U
   #:QMULR
   #:QMULR-U
   #:REALIZE
   #:REMI
   #:REMI-U
   #:REMR
   #:REMR-U
   #:RET
   #:RETI
   #:RETI-D
   #:RETI-F
   #:RETR
   #:RETR-D
   #:RETR-F
   #:RETVAL
   #:RETVAL
   #:RETVAL-C
   #:RETVAL-D
   #:RETVAL-F
   #:RETVAL-I
   #:RETVAL-S
   #:RETVAL-UC
   #:RETVAL-US
   #:RSBI
   #:RSBI-D
   #:RSBI-F
   #:RSBR
   #:RSBR-D
   #:RSBR-F
   #:RSHI
   #:RSHI-U
   #:RSHR
   #:RSHR-U
   #:SET-CODE
   #:SET-DATA
   #:SQRTR-D
   #:SQRTR-F
   #:STI
   #:STI
   #:STI-C
   #:STI-D
   #:STI-F
   #:STI-I
   #:STI-S
   #:STR
   #:STR
   #:STR-C
   #:STR-D
   #:STR-F
   #:STR-I
   #:STR-S
   #:STXI-C
   #:STXI-D
   #:STXI-F
   #:STXI-I
   #:STXI-S
   #:STXR-C
   #:STXR-D
   #:STXR-F
   #:STXR-I
   #:STXR-S
   #:SUBCI
   #:SUBCR
   #:SUBI
   #:SUBI-D
   #:SUBI-F
   #:SUBR
   #:SUBR-D
   #:SUBR-F
   #:SUBXI
   #:SUBXR
   #:TARGET-P
   #:TRAMP
   #:TRUNCR-D
   #:TRUNCR-D
   #:TRUNCR-D-I
   #:TRUNCR-F
   #:TRUNCR-F
   #:TRUNCR-F-I
   #:UNEQI-D
   #:UNEQI-F
   #:UNEQR-D
   #:UNEQR-F
   #:UNGEI-D
   #:UNGEI-F
   #:UNGER-D
   #:UNGER-F
   #:UNGTI-D
   #:UNGTI-F
   #:UNGTR-D
   #:UNGTR-F
   #:UNLEI-D
   #:UNLEI-F
   #:UNLER-D
   #:UNLER-F
   #:UNLTI-D
   #:UNLTI-F
   #:UNLTR-D
   #:UNLTR-F
   #:UNORDI-D
   #:UNORDI-F
   #:UNORDR-D
   #:UNORDR-F
   #:XORI
   #:XORR
   ))
