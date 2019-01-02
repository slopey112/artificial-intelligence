;; TREE SEARCHING ALGORITHMS

(defconstant fail nil)
(defparameter *val* 1000)
(defparameter *size* 1500)

(defun prepend (x y)
  "Prepends y to start of x."
  (append y x))

;; Calculates cost of a move.
(defun diff (x)
  "Returns difference between x and num."
  #'(lambda (num) (abs (- num x))))

;; Calculates cost of a move, except a big penalty is placed
;; on going too over a threshold.
(defun price-is-just-right (price)
  #'(lambda (x)
    (if (> x price)
      most-positive-fixnum
      (- price x))))

;; Combiner function that evaluates by a function.
(defun sorter (cost-fn)
  "Returns sorted list of values by cost."
  #'(lambda (new old)
    (sort (append new old) #'< :key cost-fn)))

;; A generic tree-searching function.
(defun tree-search (states goal-p successors combiners)
  "Finds a state that satisifes goal-p. Starts with states and searches
  according to successors and combiners."
  (format t "Search: ~a~%" states)
  (cond ((null states) fail)
    ((funcall goal-p (first states)) (first states))
    (t (tree-search
      (funcall combiners
        (funcall successors (first states))
        (rest states))
        goal-p successors combiners))))

;; A searching method wherein only a fixed number of states are kept
;; at a single time.

(defun beam-search (start goal-p successors cost-fn beam-width)
  "Search highest scoring states until goal-p is satisfied."
  (tree-search (list start) goal-p successors
    #'(lambda (old new)
      (let ((sorted (funcall (sorter cost-fn) old new)))
        (if (> beam-width (length sorted))
          sorted
          (subseq sorted 0 beam-width))))))


;; Depth-first-search, wherein the longest routes are considered first.
;; Subsequent states are only chosen when there exists no successors.
(defun depth-first-search (start goal-p successors)
  "Search new states first until goal-p is satisfied."
  (tree-search (list start) goal-p successors #'append))

;; Breadth-first-search, the opposite of depth-first search, wherein
;; instead of the longest path being taken, the shortest is instead.
(defun breadth-first-search (start goal-p successors)
  "Search old states first until goal-p is satisfied."
  (tree-search (list start) goal-p successors #'prepend))

;; Best-first-serach, wherein the routes with the lowest cost are considered
;; first. Takes advantage of state space.
(defun best-first-search (start goal-p successors cost-fn)
  "Search lowest cost state until goal-p is satisfied."
  (tree-search (list start) goal-p successors (sorter cost-fn)))

;; Successor function for a binary tree.
(defun binary-tree (x) (list (* 2 x) (+ (* 2 x) 1)))

;; Above, but finite.
(defun finite-binary-tree (x)
  #'(lambda (n)
      (remove-if #'(lambda (child) (> child x))
        (binary-tree n))))

;; Defines goal.
(defun is (value) #'(lambda (x) (eql x value)))

(format t "Depth-first-search~%")
(depth-first-search 1 (is *val*) (finite-binary-tree *size*))
(format t "~%")
(format t "Breath-first-search~%")
(breadth-first-search 1 (is *val*) (finite-binary-tree *size*))
(format t "~%")
(format t "Best-first-search with diff~%")
(best-first-search 1 (is *val*) (finite-binary-tree *size*)  (diff *val*))
(format t "~%")
(format t "Best-first-search with optimizations~%")
(best-first-search 1 (is *val*) (finite-binary-tree *size*) (price-is-just-right *val*))
(format t "~%")
(format t "Beam-search~%")
(beam-search 1 (is *val*) (finite-binary-tree *size*) (price-is-just-right *val*) 10)
