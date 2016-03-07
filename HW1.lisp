;exp
(defun power (base exp)
(cond ((eq exp 1) base)
(t (* base (power base (- exp 1))))))

;square int helper function
(defun square(x)(* x x))

;fast exponentiation
(defun fastexp(base exp)
  (cond ((eq exp 1)base)
	((eq(mod exp 2)1)square(expt(base exp/2)))
	(t(* base (expt(base exp-1))))))
(fastexp 4 5)
(power 4 5)

;Recursive List manipulation
(defun merge-list (x y)
  (cond ((eq '(x Y) nil)nil)
	 ((eq x nil)y)
	 ((eq y nil)x)
	 (t(cons (car x)(merge-list y(cdr x))))))

(merge-list '(1 2 3) nil)

(merge-list nil '(1 2 3))

(merge-list '(1 2 3) '(A B C))

(merge-list '(1 2) '(A B C D))

(merge-list '((1 2) (3 4)) '(A B))


;Reverse
(defun rev (x)
  (if(eq x nil)nil (cond((atom x)x)
	(t(cons(rev (cdr x))(car x))))))


(rev nil)

(rev 'A)

(rev '(A (B C) D))

(rev '((A B) (C D)))

;Word Censoring

;word censor
(defun censor-word (word)
  (if(member word '(extension algorithms graphics AI midterm))'XXXX word))
  
(censor-word 'lisp)

(censor-word 'midterm)

;censor function
(defun censor (x)
  (mapcar 'censor-word x))

(censor '(I NEED AN EXTENSION BECAUSE I HAD A AI MIDTERM))

(censor '(I LIKE PROGRAMMING LANGUAGES MORE THAN GRAPHICS OR ALGORITHMS))


;variable holding the data
(defvar grades '((Riley (90.0 33.3))
(Jessie (100.0 85.0 97.0))
(Quinn (70.0 100.0))))

;returns name and grades
(defun lookup (x y)
  (cond((eq nil y)nil)
       ((eq x (car(car y)))(cdr(car y)))
       (t                   lookup(x (cdr y)))))

(lookup 'Riley grades)       

;sums numbers in a list
(defun sum(x)
  (cond((eq x nil)0)
       (t (+ (car x)(sum(cdr x))))))

(sum (car (cdr '(Riley (90.0 33.3)))))

;calculates average
(defun student-avg (x)
  (cons(car x)((sum(car(cdr x)))/(count(car(cdr x))))))

(student-avg '(Riley (90.0 33.3)))

(defun compare-students(x y)
  (if(< x y)t nil))

(sort (averages grades) #'compare-students)


;Deep-Reverse
(defun deep-rev (x)
  (cond((atom x)x)
       (append(car(cdr x))(car x))))

(deep-rev '(A (B C) D))

(deep-rev '(1 2 ((3 4) 5)))

(quit)