;;;; josh-stats.lisp

(in-package #:josh-stats)


(defun median (thing)
  (let ((length (length thing)))
    (setf thing (sort thing #'<))
    (float
     (if (oddp length)
	 (nth (floor length 2) thing)
	 (/ (+ (nth (ceiling length 2) thing)
	       (nth (floor length 2) thing)) 2)))))

(defun quantile (d)
  (let ((data (sort d #'<)))
    (list (median (subseq data 0 (floor (length data) 2)))
	  (median data)
	  (median (subseq data (ceiling (length data) 2))))))

(defun over-under (q3 q1)
  (let ((iqrr (* 3/2 (- q3 q1))))
    (list (+ q3 iqrr) (- q1 iqrr))))

(defun freak-expand (ft)
  (sort
	   (if (listp (first ft))
	       (remove-if #'null
			  (reduce #'append
				  (mapcar (lambda (x y)
					    (loop
					      for i from 1 to y
					      collect x))
					  (first ft)
					  (second ft))))
	       ft)  #'<))

(defun 5-finger-death-punch (d)
  (let* ((data
	  (freak-expand d))
	 (quants (quantile data))
	 (iqr (- (third quants) (first quants))))
    
    (reverse
     (pairlis '(:min :q :max :iqr :under-over)
	      (list (reduce #'min data)
		    quants
		    (reduce #'max data)
		    iqr
		    (over-under (third quants)
				(first quants)))))))

(defun mean (data)
  (float (/ (reduce #'+ data) (length data))))

(defun variance (data &optional (p nil))
  (let ((mean (mean data)))
    (/ (reduce #'+ (mapcar (lambda (x) (expt (- x mean) 2))
			   data))
       (if p
	   (length data)
	   (1- (length data))))))

(defun std-dev (data &optional p)
  (sqrt (variance data p)))

(defun z-score (value mean std-dev)
  (/ (- value mean) std-dev))

(defun chebyshev (k)
  (- 1 (/ 1 (expt k 2))))
