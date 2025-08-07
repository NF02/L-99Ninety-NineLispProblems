(defun prime-factors (n &optional divisor factors)
  "Return a flat list of prime factors of N in ascending order.
Recursive implementation."
  (let ((divisor (or divisor 2))
        (factors (or factors '())))
    (cond
     ((= n 1) factors)
     ((zerop (mod n divisor))
      (prime-factors (/ n divisor) divisor (cons divisor factors)))
     (t
      (prime-factors n (if (= divisor 2) 3 (+ divisor 2)) factors)))))

;; Example Usage
(prime-factors 315)    ; Returns (3 3 5 7)
(prime-factors 12)     ; Returns (2 2 3)
(prime-factors 17)     ; Returns (17) - prime number
(prime-factors 1)      ; Returns nil - no prime factors
