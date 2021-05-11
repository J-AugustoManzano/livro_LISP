;; Bob Nisco
;; Theory of Programming Languages
;; Spring 2013

(defun shiftChar(char shftAmt)
	(if (= (char-code char) 32) (code-char 32)
	  (code-char (+ (mod (+ (- (char-code char) 65) shftAmt) 26) 65))))

(defun encrypt(str shftAmt)
	(map 'string #'(lambda (char) (shiftChar char shftAmt)) (string-upcase str)))

(defun decrypt(str shftAmt)
	(encrypt str (* shftAmt -1)))

(defun solve(str maxShftAmt)
	(loop for i from 0 to maxShftAmt
		; This is probably a really hacked-up way of doing this, but it works!
		for temp = (format t "Caesar ~d: ~A~C~C" i (encrypt str i) #\return #\linefeed)))

(defun main()
	(setq str "The quick brown fox jumped over the lazy dog")
	(setq shftAmt 4)
	(format t "~A~C~C" (setq str (encrypt str shftAmt)) #\return #\linefeed)
	(format t "~A~C~C" (setq str (decrypt str shftAmt)) #\return #\linefeed)
	(solve str 26))