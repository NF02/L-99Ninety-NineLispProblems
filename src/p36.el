(defun phi-improved (prime-factors)
  "Calculate Euler's totient function phi(m) using the prime factors of m.
PRIME-FACTORS is a list of (prime exponent) pairs."
  (if (null prime-factors)
      1  ; phi(1) is 1
    (let* ((pair (car prime-factors))
           (p (car pair))
           (m (cadr pair)))
      (* (1- p)
         (expt p (1- m))
         (phi-improved (cdr prime-factors))))))

;; Example Usage
(phi-improved '((2 1) (5 1)))  ; => 4 (phi(10) = 4)
(phi-improved '((2 3)))        ; => 4 (phi(8) = 4)
(phi-improved '((3 2)))        ; => 6 (phi(9) = 6)
