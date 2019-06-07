
(load "~/for-lisp/ltk/ltk.lisp")
(defvar word-file "word-list");;path
(defvar *word-list* '())
(defun str-comp (func str1 str2)
  (let ((n (string/= str1 str2)))
    (cond ((null n) nil)
          ((<= (length str1) n) (apply func '(0 1)))
          ((<= (length str2) n) (apply func '(1 0)))
          (t (apply func
                    (list (char-code (char str1 n))
                          (char-code (char str2 n))))))))

(defun words-window ()
  (ltk:with-ltk ()
    (let* (word-g word-j
           (win (make-instance 'ltk:frame))
           (cwin (make-instance 'ltk:canvas
                                :master win))
           (feng (make-instance 'ltk:frame :master cwin))
           (engtext (make-instance 'ltk:label
                                   :master feng
                                   :text "見出し語を入力してください"))
           (engreek (make-instance 'ltk:entry
                                   :master feng
                                   :width 50))
           (bgsave (make-instance 'ltk:button
                                  :master feng :text "save"))
           (bjsave (make-instance 'ltk:button
                                  :master feng :text "save"))
           )
      (setf (ltk:command bgsave)
            (progn
              (setf word-g (ltk:text engreek))
              (ltk:clear cwin)
                    (setf (ltk:text engtext)
                          (format nil "~aの訳語を入力してください"
                                  word-g))
                    (ltk:pack (list feng engtext engreek bjsave))))
      (ltk:pack (list win cwin feng))
      (ltk:pack (list engtext engreek bgsave))
      )))

(defun words-loop ()
  (loop
    (let (word-g)
      (format t "見出し語を入力してください~%\>")
      (setf word-g (read-line))
      (format t "~aの訳語を入力してください~%\>" word-g)
      (setf *word-list* (cons (cons word-g (read)) *word-list*))
      (format t "終了しますかYorN~%\>")
      (if (eql 'y (read)) (return-from words-loop *word-list*)))))
