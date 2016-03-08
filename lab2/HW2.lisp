;Alvaro Aleman
;CS 334- Lab 2
;2/22/2016

;Filter function that returns values in list l that satisfy criteria p
(defun filter(p l)
  (cond
   ((eq nil l)nil)
   ((funcall p (car l))(cons (car l)(filter p (cdr l))))
   (t(filter p (cdr l)))))

;Check that filter function works properly
(filter #'(lambda(x)(>= x 0)) '(-1 1 2 -3 4 -5))

(defun even (x) (eq (mod x 2) 0))
(filter #'even '(6 4 3 5 2))

;Function that returns union of two sets
(defun set-union(set1 set2)
      (cond ((eq set2 nil) set1)
        ((eq set1 nil) set2)
        (t (append (filter #'(lambda (x)(not (member x set2))) set1) set2))))

;Check that set Union works properly
(set-union '(1 2 3) '(2 3 4))

;Function that returns intersection of two sets 
(defun set-intersect (set1 set2)
  (cond 
        ((or (eq set1 nil) (eq set2 nil)) nil)
        (t (filter #'(lambda (x) (member x set2)) set1))))

;Check that Intersect works properly
(set-intersect '(1 2 3) '(2 3 4))

;Returns true if at least one element of l returns true when evaluated with p 
(defun exists (p l)
  (not (eq (filter p l) nil)))

;Check that exists works properly
(exists #'(lambda (x) (eq x 0)) '(-1 0 1))
(exists #'(lambda (x) (eq x 2)) '(-1 0 1))

;Returns true if all element of l return true when evaluated with p 
(defun all (p l)
  (not (exists (lambda(x) (not (funcall p x))) l)))

;Check that all works properly
(all #'(lambda (x) (> x -2)) '(-1 0 1))
(all #'(lambda (x) (> x 0)) '(-1 0 1))

(quit)