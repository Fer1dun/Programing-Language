;TOKEN CONSTANTS
(defconstant op_plus "+")
(defconstant op_minus "-")
(defconstant op_mult "*")
(defconstant op_div "/")
(defconstant op_oparan "(")
(defconstant op_cparan ")")
(defconstant op_comma ",")
(defconstant op_comment ";")
(defconstant op_str "\"")
(defconstant kw_true "true")
(defconstant kw_false "false")
(defconstant kw_and "and")
(defconstant kw_or "or")
(defconstant kw_not "not")
(defconstant kw_equal "equal")
(defconstant kw_append "append")
(defconstant kw_less "less")
(defconstant kw_nil "nil")
(defconstant kw_list "list")
(defconstant kw_concat "concat")
(defconstant kw_set "set")
(defconstant kw_def "def")
(defconstant kw_for "for")
(defconstant kw_if "if")
(defconstant kw_exit "exit")
(defconstant kw_load "load")
(defconstant kw_display "display")




(defvar valueFlag 0)
(defvar opFlag 0)
(defvar identifierFlag 0)
(defvar commentFlag 0)
(defvar stringFlag 0)
(defvar temp "")
(defvar opTemp "")
(defvar tokenList "")
(defvar value 0)

(defun read_file (filename)
  (with-open-file (stream filename)
    (loop :for curChar := (read-char stream nil) :while curChar :collect 
      (specify curChar 0)
    )
  )
  tokenList
)


(defun read_input (input)
  (setq string-length (length input))
  (loop for i from 0 below (length input)
        do (specify (char input i) string-length)
  )
  tokenList
)
(defun check-value (s)
  (let ((len (length s)))
    (loop for i from 0 below len
          until (and (char= (char s i) #\b)
                     (> i 0)
                     (< i (1- len)))
          finally (return (if (and (> i 0) (< i (1- len)))
                              (let* ((left-num (subseq s 0 i))
                                     (right-num (subseq s (1+ i)))
                                     (result (ignore-errors (parse-integer (concatenate 'string left-num right-num)))))
                                (if result
                                    (format t "~a is valuef~%" s))
                                result)
                              nil))))
  (setf identifierFlag 0)
  (setf temp "")
  )

(defun specify (curChar x)
  (cond
    ((string-equal curChar op_comment)
     (setf commentFlag 1)
     (addToken ";"))
    ((char= curChar #\Newline)
     (setf commentFlag 0)
     (addToken temp))
      ((and (eq commentFlag 0) (eq (isOp curChar) 1))
     (setf opTemp (concatenate 'string opTemp (list curChar)))
     (setf opFlag 1)
     (setf identifierFlag 0)
     (addToken opTemp))
    ((and (eq commentFlag 0) (or (alpha-char-p curChar) (digit-char-p curChar))
     (setf temp (concatenate 'string temp (list curChar)))
     (setf identifierFlag 1)
     (setf valueFlag 0)
     (incf value 1)
     (if (eq x value)
      (check-value temp)
        )))
    ((and (eq commentFlag 0) (or (char= curChar #\Space) (char= curChar #\Tab)))
     (check-value temp))
     
)

  )

(defun addToken (token)
  (progn
    (cond
     ((and (eql opFlag 1) (string-equal opTemp op_plus))
      (setf tokenList (concatenate 'string tokenList "OP_PLUS")))
     ((and (eql opFlag 1) (string-equal opTemp op_minus))
      (setf tokenList (concatenate 'string tokenList "OP_MINUS")))
     ((and (eql opFlag 1) (string-equal opTemp op_div))
      (setf tokenList (concatenate 'string tokenList "OP_DIV")))
     ((and (eql opFlag 1) (string-equal opTemp op_mult))
      (setf tokenList (concatenate 'string tokenList "OP_MULT")))
     ((and (eql opFlag 1) (string-equal opTemp op_oparan))
      (setf tokenList (concatenate 'string tokenList "OP_OP")))
     ((and (eql opFlag 1) (string-equal opTemp op_cparan))
      (setf tokenList (concatenate 'string tokenList "OP_CP")))
     ((and (eql opFlag 1) (string-equal opTemp op_comma))
      (setf tokenList (concatenate 'string tokenList "OP_COMMA")))
     ((and (eq valueFlag 1)(includeDot temp))
      (setf tokenList (concatenate 'string tokenList "value(unsignedReal)->" temp " | "))
      (setf temp "")
      (setf valueFlag 0))
    ((eq valueFlag 1)
      (setf tokenList (concatenate 'string tokenList "value(unsignedInt)->" temp " | "))
      (setf temp "")
      (setf valueFlag 0))
     )
    (setf opTemp "")
    (setf opFlag 0))
  (progn
    (cond
     ((string-equal token op_comment)
      (setf tokenList (concatenate 'string tokenList "op_comment")))
     ((string-equal token kw_true)
      (setf tokenList (concatenate 'string tokenList "kw_true")))
     ((string-equal token kw_false)
      (setf tokenList (concatenate 'string tokenList "kw_false")))
     ((string-equal token kw_and)
      (setf tokenList (concatenate 'string tokenList "kw_and")))
     ((string-equal token kw_or)
      (setf tokenList (concatenate 'string tokenList "kw_or ")))
     ((string-equal token kw_not)
      (setf tokenList (concatenate 'string tokenList "kw_not ")))
     ((string-equal token kw_equal)
      (setf tokenList (concatenate 'string tokenList "kw_equal ")))
     ((string-equal token kw_append)
      (setf tokenList (concatenate 'string tokenList "kw_append")))
     ((string-equal token kw_nil)
      (setf tokenList (concatenate 'string tokenList "kw_nil")))
     ((string-equal token kw_list)
      (setf tokenList (concatenate 'string tokenList "kw_list ")))
     ((string-equal token kw_less)
      (setf tokenList (concatenate 'string tokenList "kw_less")))
     ((string-equal token kw_concat)
      (setf tokenList (concatenate 'string tokenList "kw_concat")))
     ((string-equal token kw_set)
      (setf tokenList (concatenate 'string tokenList "kw_set")))
     ((string-equal token kw_def)
      (setf tokenList (concatenate 'string tokenList "kw_def")))
     ((string-equal token kw_for)
      (setf tokenList (concatenate 'string tokenList "kw_for")))
     ((string-equal token kw_if)
      (setf tokenList (concatenate 'string tokenList "kw_if")))
     ((string-equal token kw_load)
      (setf tokenList (concatenate 'string tokenList "kw_load")))
     ((string-equal token kw_display)
      (setf tokenList (concatenate 'string tokenList "kw_display ")))
     ((string-equal token kw_exit)
      (setf tokenList (concatenate 'string tokenList "kw_exit")))
     ((eq identifierFlag 1)
      (setf tokenList (concatenate 'string tokenList "identifier"))
      (setf identifierFlag 0)
      (setf valueFlag 0)))
    ;; VALUES
    )
  (setf temp "")
  (if (not (string= tokenList ""))
      (format t "~a ~%" tokenList))
  (setq tokenList "")
)

(defun isOp (var)
  (cond
    ( (string-equal var "(")1)
    ( (string-equal var ")")1)
    ( (string-equal var "+")1)
    ( (string-equal var "-")1)
    ( (string-equal var "*")1)
    ( (string-equal var "**")1)
    ( (string-equal var "/")1)
    ( (string-equal var ";")1)
    ( (string-equal var "\"")1)
    ( (string-equal var ",")1)
    (t 0)
  )
)
(defun isValue (var)
  (cond 
    ( (string-equal var "0")1)
    ( (string-equal var "1")1)
    ( (string-equal var "2")1)
    ( (string-equal var "3")1)
    ( (string-equal var "4")1)
    ( (string-equal var "5")1)
    ( (string-equal var "6")1)
    ( (string-equal var "7")1)
    ( (string-equal var "8")1)
    ( (string-equal var "9")1)
    ( (string-equal var ".")1)
    (t 0)

  )
)

;recursive function to determine if the number is a value(unsigned int or float)
(defun isValue (var)
  (cond 
    ((or (digit-char-p var) (string= var ".")) t)
    (t nil)
  )
)

(defun includeDot (var)
  (cond
   ((= (length var) 0) nil)
   ((string= "." (subseq var 0 1)) t)
   (t (includeDot (subseq var 1)))
  )
)
(let ((args *args*))
  (cond ((eq args nil)
         (progn
           (format t "Metin girmek için devam edin veya çıkmak için 'q' tuşuna basın: ~%")
           (finish-output t)
           (setq input (read-line))
           (read_input input)
           (finish-output t)))
        (t (read_file (first args)))
  )
)