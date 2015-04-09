(in-package :vectors)


;;;helper

(defun v-line (function line &rest vectors)
  (declare (type function function)
           (type integer line))
  (apply function (mapcar (lambda (vector) (aref vector line)) vectors)))

(defun mapvs (size function var &rest vdr)
  (dotimes (i size)
    (setf (aref var i) (apply #'v-line function i vdr)))
  var)

(defmacro vector-function (first-fun restfun list)
  (let ((var (gensym "VAR")))
    `(let ((,var (apply ,fun ,list)))
       (lambda (slot) (funcall ,fun ,var)))))

;;;general (similar to #'map), optimized(?) for vectors

(defun mapvec (function var &rest vdr)
  (let* ((size (array-dimension var 0))
         (vector (make-array size)))
    (apply #'mapvs size function vector var vdr)))

(defun mapv (function var &rest vdr)
  (apply #'mapvs (array-dimension var 0) function var var vdr))


;;;mathematical

(defun absvec (vector)
  (sqrt (reduce #'+ (mapvec (lambda (line) (expt line 2)) vector))))

(defun v+ (&rest vectors)
  (apply #'mapvec #'+ vectors))
(defun incv (&rest vectors)
  (apply #'mapv #'+ vectors))

(defun v- (&rest vectors)
  (apply #'mapvec #'- vectors))
(defun decv (&rest vectors)
  (apply #'mapv #'- vectors))


(defun v* (vector &rest numbers)
  (mapvec (vector-function #'* #'* numbers) vector))
(defun mulv (vector &rest numbers)
  (mapv (vector-function #'* #'* numbers) vector))

(defun v/ (vector &rest numbers)
  (mapvec (vector-function #'/ #'* numbers) vector))
(defun divv (vector &rest numbers)
  (mapv (vector-function #'/ #'* numbers) vector))

(defun distance (vectora vectord)
  (absvec (v- vectora vectord)))

(defun vector* (type &rest elements)
  (make-array (length elements) :element-type t :initial-contents elements))

(defun scalar (vectora vectord)
  (reduce #'+ (map 'vector #'* vectora vectord)))

(defun nearest-point (pointa pointd point)
  (reduce (lambda (x y) (* x y)) (v- pointd pointa)))


#+nil
(defun cl-user::ate (x y)
  (= (print (mod (atan (/ y x)) (coerce pi 'short-float))) (print (atan y x))))

   
