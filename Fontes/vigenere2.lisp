;;; (vigenere "ATTACKATDAWN" "LEMON")
;;;
;;; (vigenere "LXFOPVEFRNHR" "LEMON" :decrypt t)
;;;
;;;
;;; https://ubuntuforums.org/showthread.php?t=772016
;;;

(defun num-letter (n)
  "returns the char that corresponds to a number"
  (code-char (+ n (char-code #\A))))

(defun letter-num (char)
  "returns the number that corresponds to a char"
  (- (char-code char) (char-code #\A)))


(defun vigenere (plain-text key-text &key decrypt)
  "encrypt or decrypt plain-text string using key-text"
  (let ((key-cycle (make-cycler key-text)))
    (map 'string
	 (lambda (c)
	   (if decrypt
	       (decrypt-char c (funcall key-cycle))
	       (encrypt-char c (funcall key-cycle))))
	 plain-text)))

(defun make-cycler (seq)
  "returns a function that cycles through a sequence"
  (let ((len (length seq))
	(i -1))
    (lambda ()
      (incf i)
      (elt seq (mod i len)))))

(defun encrypt-char (plain-char key-char)
  "return an encrypted char"
  (num-letter (mod (+ (letter-num plain-char) 
		      (letter-num key-char))
		   26)))

(defun decrypt-char (cipher-char key-char)
  "return an decrypted char"
  (num-letter (mod (- (letter-num cipher-char) 
		      (letter-num key-char))
		   26)))

