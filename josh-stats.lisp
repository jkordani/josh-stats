;;;; josh-stats.lisp

(in-package #:josh-stats)


(defun median (data)
  (let* ((length (length data))
	 (center (1- (floor length 2)))
	 (next (1+ center)))
    (when (evenp length)
      (incf next))
    (mean (subseq data center next))))

(defun quantile (data)
  (let ((data (sort data #'<)))
    (list (median (subseq data 0 (floor (length data) 2)))
	  (median data)
	  (median (subseq data (ceiling (length data) 2))))))

(defun under-over (q3 q1)
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

(defun analysis (d)
  (let* ((data
	  (freak-expand d))
	 (quants (quantile data))
	 (iqr (- (third quants) (first quants))))

    (format t "~{~A~%~}"
	    (reverse
	     (pairlis '(:mean :median :mode :min :q :max :iqr :outlier-bounds)
		      (list
		       (mean data)
		       (median data)
		       (mode data)
		       (reduce #'min data)
		       quants
		       (reduce #'max data)
		       iqr
		       (under-over (first quants) (third quants))))))))

(defun mean (data)
  (float (/ (reduce #'+ data) (length data))))

;gross
(defun mode (data)
  (let ((values (remove-duplicates data))
	modes mode)
    (setf modes (sort (loop for value in values
		 collect (list value (count value data)))
		      #'> :key #'second))
    (setf mode (second (first modes)))
    (when (= mode 1)
      (return-from mode :no-mode))
     (pairlis '(:value :modes)
	      (list mode
		    (mapcar #'first
			    (remove-if-not (lambda (x)
					     (= x mode)) modes :key #'second))))))


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
  (assert (> k 1))
  (- 1 (/ 1 (expt k 2))))

(defun chebyshev-inverse (p)
  (assert (> 1 p 0))
  (sqrt (/ 1 (- 1 p))))

(defun chebyshev-plot (std-devs &optional (step .1))
  (let ((range (range 1.0001 std-devs step)))
    (vgplot:plot range
		 (mapcar #'chebyshev range))))

(defun range (start count &optional (step 1))
  (loop
    for i from start to (+ start (* step count)) by step
    collect i))

(defun explode (value count)
  (let ((list nil))
    (dotimes (nah count list)
      (push value list))))

(defun frequency-distribution (data &optional (rel nil))
  (let ((numbers (make-hash-table))
	(length (length data))
	(table nil))
    (dolist (datum data)
      (if (null (gethash datum numbers))
	  (setf (gethash datum numbers) 1)
	  (incf (gethash datum numbers))))
    (loop for i being each hash-key in numbers using (hash-value v)
	  do (push (cons i (if rel
			       (/ v length)
			       v)) table))
    (sort table #'< :key #'car)))

(defun histogram (data &key bins width (first-upper-delta 1))
  (let* ((frequencies (frequency-distribution data))
	 (bins (or bins (length frequencies)))
	 (lower (apply #'min data))
	 (width (or width (/ (- (apply #'max data) lower) bins)))
	 (lowers (range lower bins width))
	 (uppers (range (- (second lowers) first-upper-delta) bins width))
	 (values (make-array bins)))

    (loop for low in lowers
	  for high in uppers
	  for idx from 0
	  do (dolist (freq frequencies)
		    (when (<= low (car freq) high)
		      (incf (elt values idx) (cdr freq)))))
    (values lower width bins lowers uppers values)))
