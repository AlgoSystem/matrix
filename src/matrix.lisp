(in-package :cl-user)
(defpackage matrix
  (:use :cl)
  (:import-from :matrix.util
                :with-gensyms)
  (:export :make-matrix
           :mdata
           :nrow
           :ncol
           :mref
           :mprint
           :domatrix))

(in-package :matrix)

(defun set-matrix-no-arg (list)
  (let* ((elem (copy-list list))
         (len (length list))
         (matrix (make-array (list len 1) :initial-element 0))
         (result (list 'nrow len 'ncol 1)))
    (dotimes (i len)
      (setf (aref matrix i 0) (pop elem)))
    (push matrix result)
    result))

(defun set-matrix-exist-nrow (list nrow)
  (let* ((elem (copy-list list))
         (ncol (ceiling (/ (length list) nrow)))
         (matrix (make-array (list nrow ncol) :initial-element 0))
         (result (list 'nrow nrow 'ncol ncol)))
    (dotimes (i nrow)
      (dotimes (j ncol)
        (if (null elem)
            (setf (aref matrix i j) 0)
            (setf (aref matrix i j) (pop elem)))))
    (push matrix result)
    result))

(defun set-matrix-exist-ncol (list ncol)
  (let* ((elem (copy-list list))
            (nrow (ceiling (/ (length list) ncol)))
            (matrix (make-array (list nrow ncol) :initial-element 0))
            (result (list 'nrow nrow 'ncol ncol)))
       (dotimes (i nrow)
         (dotimes (j ncol)
           (if (null elem)
               (setf (aref matrix i j) 0)
               (setf (aref matrix i j) (pop elem)))))
       (push matrix result)
       result))

(defun set-matrix-exist-both (list nrow ncol)
  (let ((elem (copy-list list))
        (matrix (make-array (list nrow ncol) :initial-element 0))
        (result (list 'nrow nrow 'ncol ncol)))
    (dotimes (i nrow)
      (dotimes (j ncol)
        (if (null elem)
            (setf (aref matrix i j) 0)
            (setf (aref matrix i j) (pop elem)))))
    (push matrix result)
    result))

(defun make-matrix (list &key nrow ncol)
  (cond
    ((and (null nrow) (null ncol))
     (set-matrix-no-arg list))
    ((and (null ncol) (integerp nrow) (not (zerop nrow)))
     (set-matrix-exist-nrow list nrow))
    ((and (null nrow) (integerp ncol) (not (zerop ncol)))
     (set-matrix-exist-ncol list ncol))
    (t
     (when (and (integerp nrow)
                (integerp ncol))
       (set-matrix-exist-both list nrow ncol)))))

(defun mdata (matrix)
  (first matrix))

(defun nrow (matrix)
  (second (member 'nrow matrix)))

(defun ncol (matrix)
  (second (member 'ncol matrix)))


(defun mref-no-arg (matrix data)
  (let (result)
    (dotimes (i (nrow matrix))
      (dotimes (j (ncol matrix))
        (push (aref data i j) result)))
    (nreverse result)))

(defun mref-exist-row (matrix data row)
  (let (result)
    (dotimes (j (ncol matrix))
      (push (aref data row j) result))
    (nreverse result)))

(defun mref-exist-col (matrix data col)
  (let (result)
    (dotimes (i (nrow matrix))
      (push (aref data i col) result))
    (nreverse result)))

(defun mref-exist-both (data row col)
  (aref data row col))

(defun mref (matrix &optional row col)
  (let ((data (mdata matrix)))
    (cond ((and (null row) (null col))
           (mref-no-arg matrix data))
          ((null col)
           (mref-exist-row matrix data row))
          ((null row)
           (mref-exist-col matrix data col))
          (t
           (mref-exist-both data row col)))))

(defun mprint-no-arg (matrix data)
  (dotimes (i (nrow matrix))
    (dotimes (j (ncol matrix))
      (format t "~A " (aref data i j)))
    (format t "~&")))

(defun mprint-exist-row (matrix data row)
  (dotimes (j (ncol matrix))
    (format t "~A " (aref data row j)))
  (format t "~&"))

(defun mprint-exist-col (matrix data col)
  (dotimes (i (nrow matrix))
             (format t "~A~&" (aref data i col)))
  (format t "~&"))

(defun mprint-exist-both (data row col)
  (format t "~A~&"(aref data row col)))

(defun mprint (matrix &optional row col)
  (let ((data (mdata matrix)))
    (cond ((and (null row) (null col))
           (mprint-no-arg matrix data))
          ((null col)
           (mprint-exist-row matrix data row))
          ((null row)
           (mprint-exist-col matrix data col))
          (t
           (mprint-exist-both data row col)))))

(defmacro domatrix ((var matrix) &body body)
  (with-gensyms (i j data elem nrow ncol)
    `(let*  ((,data ,matrix)
             (,elem (mdata ,data))
             (,nrow (nrow ,data))
             (,ncol (ncol ,data)))
       (dotimes (,i ,nrow)
         (dotimes (,j ,ncol)
           (let ((,var (aref ,elem ,i ,j)))
             ,@body))))))
