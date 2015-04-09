(defpackage #:cl-vector-math
  (:nicknames #:vectors)
  (:use #:cl)
  (:export #:mapv
           #:mapvec

           #:typed-vector

           
           #:absvec

           #:v+
           #:incv
           #:v-
           #:decv
           #:v*
           #:mulv
           #:v/
           #:divv

           #:distance
           #:scalar
           #:angle

           #:orthogonal-projection))
