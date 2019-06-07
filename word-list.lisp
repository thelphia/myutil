

(defun str-comp (func str1 str2)
  (let ((n (string/= str1 str2)))
    (cond ((null n) nil)
          ((<= (length str1) n) (apply func '(0 1)))
          ((<= (length str2) n) (apply func '(1 0)))
          (t (apply func
                    (list (char-code (char str1 n))
                          (char-code (char str2 n))))))))

