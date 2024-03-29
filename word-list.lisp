
(load "~/for-lisp/ltk/ltk.lisp")
(defvar word-file "~/for-lisp/util/word-list");;path
(defvar *word-list* '())
(defun str-comp (func str1 str2)
  (let ((n (string/= str1 str2)))
    (cond ((null n) nil)
          ((<= (length str1) n) (apply func '(0 1)))
          ((<= (length str2) n) (apply func '(1 0)))
          (t (apply func
                    (list (char-code (char str1 n))
                          (char-code (char str2 n))))))))
#|
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
|#
(defun still-input-p (str lst)
  (dolist (var lst nil)
    (if (string= str (car var)) (return-from still-input-p t))))
(defun words-r-loop ()
  (loop
    (let (word-g)
      (format t "見出し語を入力してください~%\>")
      (setf word-g (read-line))
      (if (still-input-p word-g *word-list*)
          (format t "既に登録されています~%")
          (progn (format t "~aの訳語を入力してください~%\>" word-g)
                 (setf *word-list*
                     (sort
                      (cons (cons word-g (read-line)) *word-list*)
                      (lambda (a b) (str-comp #'< (car a) (car b)))))))
      (format t "終了しますかYorN~%\>")
      (if (eql 'y (read))
          (progn
            (save-words)
            (return-from words-r-loop (words)))))))
(defun head-search (n search-lst)
  (let (lst)
    (dolist (var (reverse search-lst) lst)
      (if (eql 0 (search n (car var)))
          (setf lst (cons var lst))))))
(defun words-s-loop ()
  (loop
        (let (n lst)
          (format t "検索したい見出し語を入力してください~%\>")
          (setf n (read-line))
          (setf lst (head-search n *word-list*))
          (format t "検索結果~%")
          (case (length lst)
            (0 (format t "該当の語句は見つかりませんでした~%"))
            (1 (format t "~a\:~a~%" (caar lst) (cdar lst)))
            (t
             (format t "~a~%" lst)
             (format t "さらに絞りますかYorN~%\>")
             (if (eql 'y (read))
                 (progn
                   (format t "~a" n)
                   (format t "検索結果~%~a~%"
                           (head-search
                            (format nil "~a~a" n (read-line))
                            lst))))))
          (format t "終了しますかYorN~%\>")
          (if (eql 'y (read)) (return-from words-s-loop (words))))))
(defun nth-rem (n lst)
  (if (zerop n)
      (values (cdr lst) (car lst))
      (values (append (subseq lst 0 n) (subseq lst (+ n 1)))
              (nth n lst))))
(defun random-lst (lst)
  (let ((c (length lst)))
    (dotimes (i c lst)
      (let ((tmplst (multiple-value-list
                     (if (zerop (- c i 1))
                         (nth-rem 0 lst)
                         (nth-rem (random (- c i 1)) lst)))))
        (setf lst (append (car tmplst) (cdr tmplst)))))))
(defun words-p-loop ()
  (let ((lst (random-lst *word-list*)) (n 1))
    (loop
      (format t "第~a問：~aの訳は？~%\>" n (caar lst))
      (read-line)
      (format t "ans: ~a~%終了しますかYorN~%\>" (cdar lst))
      (if (eql 'y (read)) (return-from words-p-loop (words)))
      (setf lst (cdr lst))
      (setf n (+ n 1)))))
(defun load-words ()
  (with-open-file (st word-file :direction :input)
    (setf *word-list* (read st))))
(defun save-words ()
  (with-open-file
      (st word-file :direction :output :if-exists :overwrite)
    (write *word-list* :stream st)))
(defun words ()
  (let ()
    (load-words)
    (format t
       "modeを選択してください~%登録(r),検索(s),テスト(p),終了(q)~%\>")
    (case (read)
      (R (words-r-loop))
      (S (words-s-loop))
      (P (words-p-loop))
      (Q (format t "exit~%"))
      (t (format t "不正なmodeが検出されました")))))

;;ここからGUI用
(defun gui-search (lst eword lresult)
  (progn
    (setf lst
          (head-search
           (ltk:text eword)
           *word-list*))
    (setf (ltk:text lresult)
          (if (null lst)
              "該当の語句は見つかりませんでした"
              (format nil "~a" lst)))))
(defun search-window ()
  (ltk:with-ltk ()
    (let* (lst
           (fsearch (make-instance 'ltk:frame))
           (tentry (make-instance 'ltk:label
                                  :master fsearch
                                  :text
                                  "検索したい語句を入力してください"))
           (eword (make-instance 'ltk:entry
                                 :master fsearch))
           (lresult (make-instance 'ltk:label
                                   :master fsearch
                                   :width 60))
           (bsearch (make-instance 'ltk:button
                                   :master fsearch
                                   :text "search"
                                   :command
                                   (lambda ()
                                     (gui-search lst eword lresult))
                                   )))
      (ltk:bind tentry "<1>"
                (lambda (event)
                  (declare (ignore event))
                  (gui-search lst eword lresult)))
      (ltk:focus eword)
      (load-words)
      (ltk:pack (list fsearch tentry eword bsearch lresult)))))




