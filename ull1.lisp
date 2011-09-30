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
   (contents :type list :initform '() :initarg :contents) ))

(defun make-ull-contents (size max-elements initial-element)
  (let ((units (ceiling size max-elements))
        ull )
    (dotimes (i units)
      (push (make-array max-elements
                        :initial-element initial-element
                        :fill-pointer (if (>= size (* (1+ i) max-elements))
                                          max-elements
                                          (mod size max-elements) ))
            ull ))
    ull ))

(defun group-pad (source n)
  (when (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n) acc))
		   (nreverse (cons (append source
                                           (make-list (- n (length source))) )
                                   acc ))))))
    (if source
	(rec source nil)
	nil )))

(test group-pad
  (is (equal (group-pad '(1 2 3 4 5 6 7) 3)
             '((1 2 3) (4 5 6) (7 NIL NIL)) )))

(defun ensure-ull-initial-contents (max-elements list)
  (do ((elt (group-pad list max-elements) (cdr elt))
       (ans '()
            (cons (make-array max-elements
                              :initial-contents (car elt)
                              :fill-pointer max-elements)
                  ans )))
      ((endp elt)
       (nreverse ans) )))

(test ensure-ull-initial-contents
  (is (equalp (ensure-ull-initial-contents 3 '(1 2 3 4 5 6 7 8 9 10))
              '(#(1 2 3) #(4 5 6) #(7 8 9) #(10 NIL NIL)) )))

(defmethod sequence:make-sequence-like ((sequence ull) length
                                        &key initial-element initial-contents (max-elements *ull-max-elements*))
  (declare (ignore initial-contents))
  (make-instance 'ull
                 :max-elements max-elements
                 :contents (make-ull-contents length max-elements initial-element)))

;;
(defmethod sequence:length ((sequence ull))
  (reduce (lambda (ans e) (+ (length e) ans))
          (slot-value sequence 'contents)
          :initial-value 0))

(defmethod sequence:elt ((sequence ull) (index integer))
  (declare ((integer 0 *) index))
  (with-slots ((max-elements max-elements)
               (contents contents) ) sequence
     (let ((pos index))
       (dolist (c contents)
         (let ((len (length c)))
           (when (< pos len)
             (return-from sequence:elt (aref c (- len pos 1))) )
           (decf pos len) )))
     (error "The index ~D is too large." index) ))

(defmethod (setf sequence:elt) (new-value (sequence ull) (index integer))
  (declare ((integer 0 *) index))
  (with-slots ((max-elements max-elements)
               (contents contents) ) sequence
     (let ((pos index))
       (dolist (c contents)
         (let ((len (length c)))
           (when (< pos len)
             (setf (aref c (- len pos 1)) new-value)
             (return-from sequence:elt (aref c (- len pos 1))) )
           (decf pos len) )))
     (error "The index ~D is too large." index) ))

(defmethod sequence:iterator-step ((s ull) iterator from-end)
  (if from-end
      (1- iterator)
      (1+ iterator) ))

(defmethod sequence:iterator-endp ((s ull) iterator limit from-end)
  (= iterator limit) )

(defmethod sequence:iterator-element ((s ull) iterator)
  (sequence:elt s iterator) )

(defmethod (setf sequence:iterator-element) (new-value (s ull) iterator)
  (setf (sequence:elt s iterator)
        new-value ))

(defmethod sequence:iterator-index ((s ull) iterator)
  iterator )

(defmethod sequence:iterator-copy (sequence iterator)
  iterator )

(defun location (ull index)
  (declare ((integer 0 *) index))
  (with-slots ((max-elements max-elements)
               (contents contents) ) ull
     (let ((pos index))
       (do ((c contents (cdr c)))
           ((endp c))
         (let ((len (length (car c))))
           (when (< pos len)
             (return-from location
               (values (car c)
                       (- len pos 1)
                       c )) )
           (decf pos len) )))
     (error "The index ~D is too large." index) ))

(defun ull-element-push (new-el ull-element pos)
  (let ((vec (make-array 10 :fill-pointer 3))
        (pos 1))
  (setf (aref vec 1) '*)
  (incf (fill-pointer vec))
  (loop :for idx :downfrom (length vec) :to pos
        :do (setf (aref vec idx)
                  (aref vec (1- idx))))
  vec))

(test location
  (is (string= (with-output-to-string (out)
                 (let* ((*ull-max-elements* 3)
                        (ull (make-sequence 'ull 20)) )
                   (dotimes (pos (length ull))
                     (setf (elt ull pos) pos) )
                   (dotimes (i (length ull))
                     (multiple-value-bind (elt offset cons)
                                          (location ull i)
                       (princ (aref elt offset)) )) ))
               "012345678910111213141516171819")))

(test ull
  ;; length
  (let ((size 3))
    (is (= size (length (make-sequence 'ull size)))) )
  ;; elt
  (is (= 1 (elt (make-sequence 'ull 3 :initial-element 1) 1)))
  ;; (setf elt)
  (let ((seq (make-sequence 'ull 3 :initial-element 1)))
    (setf (elt seq 1) 100)
    (is (= 100 (elt seq 1))) )
  (is (equal (coerce (make-sequence 'ull 3 :initial-element 3) 'list)
             '(3 3 3) ))
  (is (equalp (coerce (make-sequence 'ull 3 :initial-element 3)
                      'vector )
              #(3 3 3) ))
  (is (equal (concatenate 'list
                          (make-sequence 'ull 3 :initial-element 3)
                          (make-sequence 'ull 3 :initial-element 6) )
             '(3 3 3 6 6 6) ))
  (is (string= (with-output-to-string (out)
                 (let ((ull (make-sequence 'ull 100 :initial-element #\*)))
                   (loop :for e :being :each :element :in ull
                         :do (princ e out))))
               (make-string 100 :initial-element #\*) )))

;;; eof