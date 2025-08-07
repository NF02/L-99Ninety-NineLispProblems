(defun coprime (a b)
  "Determine if two positive integers A and B are coprime (GCD = 1)."
  (= (gcd a b) 1))

(defun gcd (a b)
  "Find the greatest common divisor of two positive integers A and B using Euclid's algorithm."
  (cond
   ((= b 0) a)  ; Base case: when b is 0, a is the GCD
   (t (gcd b (mod a b)))))  ; Recursive case: gcd(b, a mod b)

;; examples usage
(coprime 35 64) ;; => t
(coprime 0 -1) ;; => nil
