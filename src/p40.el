(defun is-prime (n &optional d)
  "Check if N is a prime number recursively."
  (cond ((<= n 1) nil)
        ((null d) (is-prime n 2))  ; Initialize d to 2 if not provided
        ((= d n) t)
        ((zerop (% n d)) nil)
        (t (is-prime n (1+ d)))))

(defun find-prime-pair (n start)
  "Find a prime pair that sums to N, starting from START."
  (cond ((> start (/ n 2)) nil)
        ((and (is-prime start)
              (is-prime (- n start)))
         (list start (- n start)))
        (t (find-prime-pair n (1+ start)))))

(defun goldbach (n)
  "Find two prime numbers that sum to the given even integer N."
  (if (or (<= n 2) (/= 0 (% n 2)))  ; Check if even using % (modulo)
      (error "Input must be an even number greater than 2")
    (find-prime-pair n 2)))

;; Example usage:
(goldbach 28)  ; => (5 23)
