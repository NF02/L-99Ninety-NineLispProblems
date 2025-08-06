;; Helper function to generate an increasing sequence from a to b
(defun range-asc (a b)
  (if (> a b)  ; Base case: if a exceeds b, stop recursion
      nil
    (cons a (range-asc (+ a 1) b))))  ; Recursive step: add a to the sequence and increment a

;; Helper function to generate a decreasing sequence from a to b
(defun range-desc (a b)
  (if (< a b)  ; Base case: if a is less than b, stop recursion
      nil
    (cons a (range-desc (- a 1) b))))  ; Recursive step: add a to the sequence and decrement a

(defun range (a b)
  (cond ((= a b) (list a))  ; If a equals b, return a single-element list
        ((< a b) (range-asc a b))  ; If a < b, generate increasing sequence
        (t (range-desc a b))))  ; Otherwise, generate decreasing sequence

(range 4 9) ; => (4 5 6 7 8 9)
