(defun gcd (a b)
  "Find the greatest common divisor of two positive integers A and B using Euclid's algorithm."
  (cond
   ((= b 0) a)  ; Base case: when b is 0, a is the GCD
   (t (gcd b (mod a b)))))  ; Recursive case: gcd(b, a mod b)

;; examples usage
(gcd 36 63)   ; Returns 9
(gcd 206 40)  ; Returns 2
(gcd 1071 462) ; Returns 21
