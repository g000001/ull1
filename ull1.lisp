;;;; ull1.lisp

(cl:in-package :ull1-internal)

(def-suite ull1)

(in-suite ull1)

;; list
;; '(1 2 3 4 5 6 7 8 9 10 11 12)

;; unrolled linked list
;;
;; [1]
;; (#(1 2 3) #(4 5 6 7 8) #(9 10 11 12))
;; が良いのかはたまた
;; [2]
;; (#(3 2 1) #(8 7 6 5 4) #(12 11 10 9))
;; か。
;; listなのでpushの動作をlistに統一するとすれば[2]が良いかも
;;
;; ==> [2]で実装してみる

(defvar *ull-max-elements* 1024)

(defclass ull (sequence standard-object)
  ((max-elements :initarg :max-elements :initform *ull-max-elements*)
   (contents :type list :initform '() :initarg :contents)))

(defun make-ull-contents (size max-elements)
  (let ((units (ceiling size max-elements))
        ull)
    (dotimes (i units)
      (push (make-array max-elements
                        :initial-element nil
                        :fill-pointer (if (>= size (* (1+ i) max-elements))
                                          max-elements
                                          (mod size max-elements)))
            ull))
    ull))

(defmethod sequence:make-sequence-like
    ((sequence ull) length
     &key initial-element initial-contents (max-elements *ull-max-elements*))
  (declare (ignore initial-contents))
  (let ((units (ceiling length max-elements))
        ull)
    (dotimes (i units)
      (push (make-array max-elements
                        :initial-element initial-element
                        :fill-pointer (if (>= length (* (1+ i) max-elements))
                                          max-elements
                                          (mod length max-elements)))
            ull))
    (make-instance 'ull
       :max-elements max-elements
       :contents ull)))

;;
(defmethod sequence:length ((sequence ull))
  (reduce (lambda (ans e) (+ (length e) ans))
          (slot-value sequence 'contents)
          :initial-value 0))

(defmethod sequence:elt ((sequence ull) (index integer))
  (declare ((integer 0 *) index))
  (with-slots ((max-elements max-elements)
               (contents contents)) sequence
     (let ((pos index))
       (dolist (c contents)
         (let ((len (length c)))
           (when (< pos len)
             (return-from sequence:elt (aref c (- len pos 1))))
           (decf pos len))))
     (error "The index ~D is too large." index)))

(defmethod (setf sequence:elt) (new-value (sequence ull) (index integer))
  (declare ((integer 0 *) index))
  (with-slots ((max-elements max-elements)
               (contents contents)) sequence
     (let ((pos index))
       (dolist (c contents)
         (let ((len (length c)))
           (when (< pos len)
             (setf (aref c (- len pos 1)) new-value)
             (return-from sequence:elt (aref c (- len pos 1))))
           (decf pos len))))
     (error "The index ~D is too large." index)))
