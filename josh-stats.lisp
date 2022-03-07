;;;; josh-stats.lisp

(in-package #:josh-stats)


(defun median (data)
  (let* ((data (sort data #'<))
	 (length (length data))
	 (center (1- (floor length 2)))
	 (next (1+ center)))
    (when (evenp length)
      (incf next))
    (mean (subseq data center next))))

(defun quantile (data)
  (let ((data (sort data #'<)))
    (list (median (subseq data 0 (ceiling (length data) 2)))
	  (median data)
	  (median (subseq data (floor (length data) 2))))))

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

(defun empirical (&optional (std-dev 0) (mean 0) (value 0))
  (let* ((e1 68/200)
	 (e2 (- 95/200 e1))
	 (e3 (- 997/2000 e2 e1))
	 (e4 (- 1/2 e3 e2 e1))
	 (std-devs '(-4 -3 -2 -1 0 1 2 3 4))
	 (z-score (z-score value mean std-dev))
	 (left-to-right (list e4 e3 e2 e1 e1 e2 e3 e4))
	 (idx (position (if (<= z-score -4)
			    -4
			    (if (<= 4 z-score)
				4
				z-score))
			std-devs))
	 (dist (mapcar (lambda (x) (+ (* x std-dev) mean))
	     std-devs)))
    (values 
     (mapcar (lambda (x) (float x)) (list e1 e2 e3 e4)) 
     (maplist (lambda (x) (float (reduce #'+ x)))
	      left-to-right) 
     dist
     idx
     left-to-right
     (list  
      (subseq left-to-right 0 idx)
      (subseq left-to-right idx)
      (float
       (reduce #'+
	       (subseq left-to-right
		       0 idx)))
      (float
       (reduce #'+
	       (subseq left-to-right
		       idx)))))))

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
	 (lower (reduce #'min data))
	 (max-min (/ (- (reduce #'max data) lower) bins))
	 (width (or width (when (integerp (first data)) (ceiling max-min)) max-min))
	 (lowers (range lower bins width))
	 (uppers (range (- (second lowers) first-upper-delta) bins width))
	 (values (make-array (1+ bins) :adjustable t :fill-pointer t )))

    (loop for low in lowers
	  for high in uppers
	  for idx from 0
	  do (dolist (freq frequencies)
		    (when (<= low (car freq) high)
		      (incf (elt values idx) (cdr freq)))))
    (values lower width bins lowers uppers values)))

(defun ! (n)
  (let ((sum 1))
    (dotimes (i n)
      (setf sum (* sum (1+ i))))
    sum))

(defun permutations (options count)
  (/ (! options) (! (- options count))))

(defun combinations (options count)
  (/ (! options) (* (! (- options count)) (! count))))

(defun bdf (wins prob trials)
  (* (combinations trials wins) (expt prob wins) (expt (- 1 prob) (- trials wins))))

(defun bcdf (wins prob trials)
  (reduce #'+ (mapcar #'bdf
		      (range 0 wins)
		      (explode prob (1+ wins))
		      (explode trials (1+ wins)))))

(defun expected (values probabilities)
  (reduce #'+ (mapcar #'* values probabilities)))

(defun pdf-variance (values probabilities)
  (let ((mean (expected values probabilities)))
    (reduce #'+ (mapcar (lambda (v p) (* (expt (- v mean) 2) p))
			values probabilities))))

(defun pdf-std-dev (values probabilities)
  (sqrt (pdf-variance values probabilities)))

(defun bernoulli (n p)
  (pairlis '(:mean :variance :std-dev)
	   (list (* n p) (* n (- 1 p) p) (sqrt (* n (- 1 p) p)))))

(defun pdf-analysis (values probabilities)
  (pairlis '(:mean :variance :std-dev)
	   (list (expected values probabilities)
		 (pdf-variance values probabilities)
		 (pdf-std-dev values probabilities))))

(defun definite-integral (fn from to)
  (- (funcall fn to) (funcall fn from)))

(defun integral (xy1 xy2 from to)
  (let* ((slope (/ (- (second xy1) (second xy2))
		   (- (first xy1) (first xy2))))
	 (y-int (- (second xy1) (* slope (first xy1)))))
    (flet ((xterm (s)
	     (lambda (x)
	       (* s 1/2 (expt x 2))))
	   (cterm (s)
	     (lambda (x)
	       (* s x))))
      (+ (definite-integral (xterm slope) from to)
	 (definite-integral (cterm y-int) from to)))))

(defun error-function-complement (x)
  (- 1 (error-function x)))

(defun error-function-inverse-complement (p)
  (error-function-inverse (- 1 p)))

(defun error-function (x &optional (steps 50))
  (* (/ 2 (sqrt pi))
     (reduce #'+
	     (loop for n from 0 to steps
		   collect (let ((product (/ x (1+ (* 2 n)))))
			     (dotimes (k n product)
			       (setf product
				     (* product
					(/ (- (expt x 2))
					   (1+ k))))))))))

(defun error-function-inverse (z &optional (steps 100))
  (let ((c (* z (/ (sqrt pi) 2)))
	(expansion (c-expand steps)))
    (reduce #'+
	    (mapcar (lambda (k ck)
		      (* (/ ck (1+ (* 2 k)))
			 (expt c (1+ (* 2 k)))))
		    (range 0 steps)
		    (coerce expansion 'list)))))


(defun normcdf (x)
  (* 1/2
     (error-function-complement
      (- (/ x (sqrt 2))))))

(defun norminv (prob &optional (mean 0) (std-dev 1) (cltm-samples 1))
  (values (+ mean (* (/ std-dev (sqrt cltm-samples))
	      (* (- (sqrt 2))
		 (error-function-inverse-complement
		  (* 2 prob)))))
	  (/ std-dev (sqrt cltm-samples))))

(defun integral-ndf (from to &optional (mean 0) (std-dev 1))
  (let ((upper (or to (+ mean (* 4 std-dev))))
	(lower (or from (- mean (* 4 std-dev)))))
    (definite-integral
	      #'normcdf
	      (z-score lower mean std-dev)
	    (z-score upper mean std-dev))))

(defun ndf (from to n p)
  (let* ((mean (cdr (assoc :mean (bernoulli n p))))
	 (std-dev (cdr (assoc :std-dev (bernoulli n p))))
	 (upper (or to (+ mean (* 4 std-dev))))
	 (lower (or from (- mean (* 4 std-dev)))))
    (integral-ndf lower upper mean std-dev)))

(defun c-expand (steps)
  (let ((cs (make-array steps :element-type '(integer)
			      :initial-element 0)))
    (flet ((denom (m) (* (1+ m) (1+ (* 2 m)))))
      (setf (elt cs 0) 1)
      (dotimes (k steps)
	(dotimes (m k)
	  (incf (elt cs k)
		(/ (* (elt cs m)
		      (elt cs (- (1- k) m)))
		   (denom m)))))
      cs)))

(defun central-limit-theorem-means (from to mean std-dev samples)
  (values
   (integral-ndf from to mean (/ std-dev (sqrt samples)))
   (/ std-dev (sqrt samples))))

(defun central-limit-theorem-sums (from to mean std-dev samples)
  (integral-ndf from to (* mean samples) (* std-dev (sqrt samples))))

(defun cltm (mean std-dev samples)
  (pairlis '(:mean :std-dev)
	   (list mean (/ std-dev (sqrt samples)))))

(defun clts (mean std-dev samples)
  (pairlis '(:mean :std-dev)
	   (list (* samples mean) (* std-dev (sqrt samples)))))

(defun inv-z-score (z mean std-dev)
  (+ (* z std-dev) mean))

