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


(defstruct valuefList
  num
  denom
  )
(defstruct funcList
  operator
  name
  )

(defvar func (list))
(defvar valNum 0)
(defvar tokenList (list))
(defvar my_list (list))
(defvar valueFlag 0)
(defvar opFlag 0)
(defvar identifierFlag 0)
(defvar commentFlag 0)
(defvar stringFlag 0)
(defvar temp "")
(defvar opTemp "")
(defvar value 0)
(defvar exitValue 0)
(defvar func_call "")
(defvar value_func 0)

(defun read_file (filename)
  (progn
    (with-open-file (stream filename)
      (setq tokenList nil)
      (setq my_list nil)
      (loop :for curChar := (read-char stream nil) :while curChar :collect
            (progn
              (specify curChar 0)
              ;; Ekranın yeni satıra geçmesi için kontrolü ekleyin:
              (format t "~c" curChar) ; Eğer \n karakterini görmüyorsanız burayı \r\n olarak da değiştirebilirsiniz.
              (when (char= curChar #\Newline) ; Eğer karakter bir newline karakteri ise
                (setq tokenList (reverse tokenList))
                (setq my_list (reverse my_list))
                (check tokenList)
                (setq tokenList nil)
                (setq my_list nil)
              )
            )
      )
    )
  )
)




(defun read_input (input)
  (progn
  (setq tokenList nil)
  (setq my_list nil)
  (setq string-length (length input))
  (loop for i from 0 below (length input)
        do (specify (char input i) string-length)
  )
  (setq tokenList(reverse tokenList))
  (setq my_list (reverse my_list))
  (check tokenList)
  tokenList
)
  )
(defun check-name-in-func (name)
  (let ((found (find name func :key #'funcList-name :test #'string=)))
    (if found
        (setf (nth 1 tokenList) (funcList-operator found)))))



(defun parse_valuef (left right)
  (setq struct  (make-valuefList :num left :denom right))
  (push struct my_list)
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
                                              (progn
                                              (setf left-num (parse-integer left-num))
                                              (setf right-num (parse-integer right-num))                                            
                                              (setf identifierFlag 0)
                                              (parse_valuef left-num right-num)))
                                result)
                              nil))))
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
     (setf valueFlag 1)
     (incf value 1)
     (if (eq x value)
      (check-value temp)
        )))
    ((and (eq commentFlag 0) (or (char= curChar #\Space) (char= curChar #\Tab)))
     (check-value temp)
     (addToken temp))
     
)

  )

(defun addToken (token)
  (progn
    (cond
     ((and (eql opFlag 1) (string-equal opTemp op_plus))
      (push  "OP_PLUS" tokenList ))
     ((and (eql opFlag 1) (string-equal opTemp op_minus))
      (push  "OP_MINUS" tokenList ))
     ((and (eql opFlag 1) (string-equal opTemp op_div))
      (push  "OP_DIV" tokenList ))
     ((and (eql opFlag 1) (string-equal opTemp op_mult))
      (push  "OP_MULT" tokenList ))
     ((and (eql opFlag 1) (string-equal opTemp op_oparan))
      (push  "OP_OP" tokenList ))
     ((and (eql opFlag 1) (string-equal opTemp op_cparan))
      (push  "OP_CP" tokenList ))
     ((and (eql opFlag 1) (string-equal opTemp op_comma))
      (push  "OP_COM" tokenList ))
   ((and (eq valueFlag 1) (eq identifierFlag 0))
      (push  "valuef" tokenList )
      (setf valueFlag 0))
     )
    (setf opTemp "")
    (setf opFlag 0))
  (progn
    (cond
     ((string-equal token op_comment)
      (push  "op_comment" tokenList ))
     ((string-equal token kw_true)
      (progn
      (push  "kw_true" tokenList )))
     ((string-equal token kw_false)
      (push  "kw_false" tokenList ))
     ((string-equal token kw_exit)
      (progn
        (setf valueFlag 0)
      (push  "kw_exit" tokenList )))
     ((eq identifierFlag 1)
      (progn
      (push  token tokenList )
      (incf value_func 1)
      (if  (equal value_func 2)
      (setf func_call token) )
      (setf identifierFlag 0)
      (setf valueFlag 0))
      )
      )
    ;; VALUES
    )
  (setf temp "")


 
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
(defun check (tokenList)
    (setf checkValue nil)
    (if (equal (length tokenList) 1)
        (progn
            (if (string= (nth 0 tokenList) "valuef")
                (progn
                    (setf checkValue (nth valNum my_list))
                    (setq valNum (+ valNum 1))
                )
            )
        )
    )
    (if (equal (length tokenList) 3)
        (progn
            (if (and (string= (nth 0 tokenList) "OP_OP") (string= (nth 2 tokenList) "OP_CP"))
                (progn
                    (if (string= (nth 1 tokenList) "kw_exit")
                        (progn
                            (setf checkValue "exit")
                            (setq exitValue 1)
                        )
                    )
                )

            )
        )
    )
    (if (equal (length tokenList) 5)
        (progn 
            (if (and (string= (nth 0 tokenList) "OP_OP") (string= (nth 4 tokenList) "OP_CP"))
                (progn
                    ; look for + operation
                    (check-name-in-func (nth 1 tokenList))
                    (if (string= (nth 1 tokenList) "OP_PLUS")
                        (setf checkValue (opPLUS my_list))                     
                    )

                    ; look for - operation  
                    (if (string= (nth 1 tokenList) "OP_MINUS")
                        (setf checkValue (opMINUS my_list))
                    )

                    ; look for * operation
                    (if (string= (nth 1 tokenList) "OP_MULT")
                        (setf checkValue (opMULT my_list))
                    )

                    ; look for / operation
                    (if (string= (nth 1 tokenList) "OP_DIV")
                        (setf checkValue (opDIV my_list))
                    )

                )
                    
            )
        )
    )
    (if (equal (length tokenList) 11)
      (progn 
        (if (and (string= (nth 0 tokenList) "OP_OP") (string= (nth 10 tokenList) "OP_CP"))
              (progn
              (setq my_func  (make-funcList :name func_call :operator (nth 6 tokenList)))
              (push my_func func)
              ) 
          )
        )
      )
   
    ; return
    checkValue
)

(defun disp-valuefList (r)
  (format t "~ab~a~%" (valuefList-num r) (valuefList-denom r)))

(defun opPLUS (my_list)
  (if (= (length my_list) 2)
      (let* ((v1 (first my_list))
             (v2 (second my_list))
             (r (make-valuefList :num (+ (* (valuefList-num v1) (valuefList-denom v2))
                                         (* (valuefList-num v2) (valuefList-denom v1)))
                                :denom (* (valuefList-denom v1) (valuefList-denom v2)))))
      (disp-valuefList r)
        r)
      (error "Exactly two elements are expected in my_list."))
  )
(defun opMINUS (my_list)
  (if (= (length my_list) 2)
      (let* ((v1 (first my_list))
             (v2 (second my_list))
             (r (make-valuefList :num (- (* (valuefList-num v1) (valuefList-denom v2))
                                         (* (valuefList-num v2) (valuefList-denom v1)))
                                :denom (* (valuefList-denom v1) (valuefList-denom v2)))))
      (disp-valuefList r)
        r)
      (error "Exactly two elements are expected in my_list."))
  )
(defun opMULT (my_list)
  (if (= (length my_list) 2)
      (let* ((v1 (first my_list))
             (v2 (second my_list))
             (r (make-valuefList :num (+ (* (valuefList-num v1) (valuefList-num v2))
                                         )
                                :denom (* (valuefList-denom v1) (valuefList-denom v2)))))
      (disp-valuefList r)
        r)
      (error "Exactly two elements are expected in my_list."))
  )
(defun opDIV (my_list)
  (if (= (length my_list) 2)
      (let* ((v1 (first my_list))
             (v2 (second my_list))
             (r (make-valuefList :num (+ (* (valuefList-num v1) (valuefList-denom v2))
                                         )
                                :denom (* (valuefList-denom v1) (valuefList-num v2)))))
      (disp-valuefList r)
        r)
      (error "Exactly two elements are expected in my_list."))
  )


(let ((args *args*))
  (cond ((eq args nil)
         (progn
           (format t "Metin girmek için devam edin veya çıkmak için ( exit ) yazın: ~%")
           (finish-output t)
           (loop
            (setq input (read-line))
            (read_input input)
            (if (equal exitValue 1)
                (progn
                  (format t "Exiting from interpreter")
                  (return))
              )
            )
           (finish-output t)))
        (t (read_file (first args)))
  )
)

