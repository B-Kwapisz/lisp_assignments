;;; Brian Kwapisz
;;; Lisp Project

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: integrate
;;; Args: A function (F) to integrate with respect to variable V;
;;;          optionally takes integers LOWER and UPPER for the integration bounds
;;; Returns: The result of the integration

(defun integrate (F V &optional lower upper)
    "Integrates F with respect to V, optionally integrating on the domain [LOWER, UPPER]."
    (def-integral (indef-integral F V) V lower upper))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: indef-integral
;;; Args: A function F to integrate with respect to variable V
;;; Returns: The indefinitely integrated expression

(defun indef-integral (F V)
    "Indefinitely integrates F with respect to V."
    (labels (
        ;; Indefinite integration helper function
        (indef-integral-aux (F V)
            (cond
                ((number-p F) (make-prod F V))			;;; numbers
               
                ((negative-p F) (make-neg (integrate (make-negative F) V)))
                ((variable-p F) (integrate (make-pow F 1) V))	;;; negative
               
                ((sum-p F) (make-sum
                    (integrate (sum-first-operand F) V)
                    (integrate (sum-second-operand F) V)))		;;; addition
               
                ((difference-p F) (make-diff
                    (integrate (diff-first-operand F) V)
                    (integrate (diff-second-operand F) V)))		;;; subtraction
               
                ((and (power-p F) (not (equal (pow-second-operand F) -1))) (make-quot
                    (make-pow V (make-sum (pow-second-operand F) 1))
                    (make-sum (pow-second-operand F) 1)))		;;; Power, if (n != -1)
               
                ((and (power-p F) (equal (pow-second-operand F) -1) 
		     (make-log (pow-first-operand F))))		;;; Power, if (n = -1)
                (t nil))))						
        (cond								;;; Simplify multiple negative 
									;;; symbols, if present
            ((mult-negative-p F) (indef-integral-aux (make-simple-neg F) V))
            ((variable-p V) (indef-integral-aux F V))
            (t nil))))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: def-integral
;;; Arg(s): A function F to integrate with respect to variable V
;;;          on the interval [LOWER, UPPER]
;;; Returns: The definitely integrated function within the domain [LOWER, UPPER]

(defun def-integral (F V lower upper)
    "Definitely integrates F with respect to V, with a domain of [LOWER, UPPER]."
    (cond								;;; Return indefinite integrate		((not (and (number-p lower) (number-p upper))) F)		;;; if limits are not numbers

	(t (eval (make-diff  						;;; If they are
            (my-replace V upper F)  					;;; make the difference
            (my-replace V lower F))))))				;;; of the substituted halves

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: my-replace
;;; Args: The element E1 to replace with E2 in the list L
;;; Returns: The list with the specified elements replaced

(defun my-replace (e1 e2 L)
    "Returns the list L with element E1 replaced with E2."
    (labels (
        ;; Helper function
        (my-replace-aux (e1 e2 L)
            (cond
                ((endp L) L)
                ;;; Perform replacement when the element is found
                ((equal (first L) e1) (cons e2 (my-replace e1 e2 (rest L))))
                ;;; Go down a level if the element itself is a list
                ((listp (first L)) (cons (my-replace e1 e2 (first L)) (my-replace e1 e2 (rest L))))
                ;;; Continue through the list
                (t (cons (first L) (my-replace e1 e2 (rest L)))))))
        (cond
            ;; If L is a variable, perform replacement on a list containing the variable
            ((variable-p L) (first (my-replace e1 e2 (list L))))
            ;; Perform the replace
            (t (my-replace-aux e1 e2 L)))))

;;;——————————————————————————————————————————————————————————————————————————
;;; SYMBOLS

(defconstant *var-symbols* '(U V W X Y Z))
(defconstant *neg-symbol* '-)
(defconstant *sum-symbol* '+)
(defconstant *diff-symbol* '-)
(defconstant *prod-symbol* '*)
(defconstant *quot-symbol* '/)
(defconstant *pow-symbol* 'expt)
(defconstant *log-symbol* 'log)

;;;——————————————————————————————————————————————————————————————————————————
;;; SELECTORS -- OPERATORS

;;;——————————————————————————————————————————————————————————————————————————
;;;    NAME: neg-operator
;;;  ARG(S): A negative expression F
;;; RETURNS: The negative symbol from F

(defun neg-operator (F)
    "Selects the negative symbol in the expression.”
    (first F))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: sum-operator
;;; Args: A sum expression F
;;; Returns: The sum symbol from F

(defun sum-operator (F)
    "Selects the sum symbol in a prefix expression."
    (first F))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: diff-operator
;;; Args: A difference expression F
;;; Returns: The difference symbol from F

(defun diff-operator (F)
    "Selects the difference symbol in a prefix expression."
    (first F))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: prod-operator
;;; Args: A product expression F
;;; Returns: The product symbol from F

(defun prod-operator (F)
    "Selects the product symbol in a prefix expression."
    (first F))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: quot-operator
;;; Args: A quotient expression F
;;; Returns: The quotient symbol from F

(defun quot-operator (F)
    "Selects the quotient symbol in a prefix expression."
    (first F))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: pow-operator
;;; Args: A power expression F
;;; Returns: The power operator from F

(defun pow-operator (F)
    "Selects the power symbol in a prefix expression."
    (first F))

;;; SELECTORS -- OPERANDS

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: neg-operand
;;; Args: A negated expression F
;;; Returns: The operand of negated expression F

(defun neg-operand (F)
    "Selects the operand of a negative operation in a prefix expression."
    (second F))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: sum-first-operand
;;; Args: A sum expression F
;;; Returns: The first operand of sum expression F

(defun sum-first-operand (F)
    "Selects the first operand of a sum operation in a prefix expression."
    (second F))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: sum-second-operand
;;; Args: A sum expression F
;;; Returns: The second operand of sum expression F

(defun sum-second-operand (F)
    "Selects the second operand of a sum operation in a prefix expression."
    (third F))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: diff-first-operand
;;; Args: A difference expression F
;;; Returns: The first operand of difference expression F

(defun diff-first-operand (F)
    "Selects the first operand of a difference operation in a prefix expression."
    (second F))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: diff-second-operand
;;; Args: A difference expression F
;;; Returns: The second operand of difference expression F

(defun diff-second-operand (F)
    "Selects the second operand of a difference operation in a prefix expression."
    (third F))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: prod-first-operand
;;; Args: A product expression F
;;; Returns: The first operand of product expression F

(defun prod-first-operand (F)
    "Selects the first operand of a product operation in a prefix expression."
    (second F))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: prod-second-operand
;;; Args: A product expression F
;;; Rwturns: The second operand of product expression F

(defun prod-second-operand (F)
    "Selects the second operand of a product operation in a prefix expression."
    (third F))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: quot-first-operand
;;; Args: A quotient expression F
;;; Returns: The first operand of quotient expression F

(defun quot-first-operand (F)
    "Selects the first operand of a quotient operation in a prefix expression."
    (second F))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: quot-second-operand
;;; Args: A quotient expression F
;;; Returns: The second operand of quotient expression F

(defun quot-second-operand (F)
    "Selects the second operand of a quotient operation in a prefix expression."
    (third F))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: pow-first-operand
;;; Args: A power expression F
;;; Returns: The first operand of power expression F

(defun pow-first-operand (F)
    "Selects the first operand of a power operation in a prefix expression."
    (second F))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: pow-second-operand
;;; Args: A power expression F
;;; Returns: The second operand of power expression F

(defun pow-second-operand (F)
    "Selects the second operand of a power operation in a prefix expression."
    (third F))

;;;——————————————————————————————————————————————————————————————————————————
;;; PREDICATES

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: variable-p
;;; Args: An expression F
;;; Returns: T if F is a variable symbol

(defun variable-p (F)
    "Returns T if F is a variable symbol."
    (member F *var-symbols*))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: number-p
;;; Args: An expression F
;;; Returns: T if F is a number

(defun number-p (F)
    "Returns T if F is a number."
    (numberp F))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: negative-p
;;; Args: An expression F
;;; Returns: T if F is a negative expression

(defun negative-p (F)
    "Returns T if F is a negative expression."
    (cond
        						;;; Check if negative
        ((and (number-p F) (< F 0)) t)
        ((number-p F) nil)
        ((variable-p F) nil)
        ((difference-p F) nil)
        						;;; Check if expression follows the format (- F)
        ((and
            (equal (neg-operator F) *neg-symbol*)
            (not (equal (neg-operand F) *neg-symbol*))) t)))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: mult-neg-p
;;; Args: An expression F
;;; Returns: T if F is a negative expression with multiple negations

(defun mult-neg-p (F)
    "Returns T if F is a negative expression with multiple negations."
    (labels (
        ;; Helper function
        (mult-neg-p-aux (F L)
            (cond
                ;; Return T if L contains just one element at the end
                ((endp F) (equal (length L) 1))
                ;; If CAR is the negative symbol, continue CDRing
                ((equal (first F) *neg-symbol*) (mult-neg-p-aux (rest F) L))
                ;; Otherwise, add CAR to L and continue
                (t (mult-neg-p-aux (rest F) (cons (first F) L))))))
    (cond
        ((number-p F) nil)
        ((variable-p F) nil)
        ((negative-p F) nil)
        ((difference-p F) nil)
        ((not (listp F)) nil)
        (t (mult-neg-p-aux F '())))))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: sum-p
;;; Args: An expression F
;;; Returns: T if F is a sum expression

(defun sum-p (F)
    "Returns T if F is a sum expression."
    (cond
        ((number-p F) nil)
        ((variable-p F) nil)
        ;; Check if expression follows the form (+ F G)
        ((and
            (equal (sum-operator F) *sum-symbol*)
            (sum-first-operand F)
            (sum-second-operand F)) t)))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: difference-p
;;; Args: An expression F
;;; Returns: True, if F is a difference expression

(defun difference-p (F)
    "Returns T if F is a difference expression."
    (cond
        ((number-p F) nil)
        ((variable-p F) nil)
        ;; Check if expression follows the form (- F G)
        ((and
            (equal (diff-operator F) *diff-symbol*)
            (not (equal (diff-first-operand F) *diff-symbol*))
            (diff-second-operand F)) t)))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: power-p
;;; Args: An expression F
;;; Returns: True, if F is a power expression

(defun power-p (F)
    "Returns T if F is a power expression."
    (cond
        ((number-p F) nil)
        ((variable-p F) nil)
        					;;; Check if expression follows the form (EXPT V N)
        ((and
            (equal (pow-operator F) *pow-symbol*)
            (variable-p (pow-first-operand F))
            (number-p (pow-second-operand F))) t)))

;;;——————————————————————————————————————————————————————————————————————————
;;; CONSTRUCTORS

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: make-var
;;; Args: A variable V
;;; Returns: V

(defun make-variable (V)
    "Constructs a variable expression consisting of V."
    V)

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: make-neg
;;; Args: An expression F
;;; Returns: The negation of F

(defun make-neg (F)
    "Constructs an expression which is the negation of F."
    (labels (
        (make-negative-aux (F)
            (cond
                ;; if F is a number, make negative
                ((number-p F) (* -1 F))
                ;; if F is already a negative, make positive
                ((negative-p F) (neg-operand F))
                ;; returns the list containing the negative 
                (t (list *neg-symbol* F)))))
    (cond
	;;; simplify expression if needed
        ((mult-neg-p F) (make-neg-aux (make-simple-neg F)))
        (t (make-neg-aux F)))))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: make-simple-neg
;;; Args: An expression F
;;; Returns: The simplified negation of F

(defun make-simple-neg (F)
    "Constructs an expression which is the simplified negative of F."
    (labels (
        (make-simple-neg-aux (F)
            (cond
     					;;; Even number of negative symbols cancel each other out
              ((equal (mod (length F) 2) 0) (list *neg-symbol* (first (last F))))
              (t (first (last F))))))
    (cond
     					;;; If there are multiple negatives, simplify
        ((mult-neg-p F) (make-simple-neg-aux F))
     					;;; If F is a single negative, return F
        ((negative-p F) F))))

;;;——————————————————————————————————————————————————————————————————————————
;;; Nqme: make-sum
;;; Args: Expressions F and G
;;; Returns: An expression which is the sum of F and G

(defun make-sum (F G)
    "Constructs an expression which is the sum of F and G."
    (cond
        						;;; F or G plus 0 is itself
        ((equal F 0) G)
        ((equal G 0) F)
        						;;; F or G plus its negative self is 0
        ((equal F (make-neg G)) 0)
        ((equal G (make-neg F)) 0)
        						;;; If both are numbers, add them
        ((and (number-p F) (number-p G)) (+ F G))
        						;;; Return the list containing the sum 
        (t (list *sum-symbol* F G))))

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: make-diff
;;; Arg(s): Expressions F and G
;;; Returns: An expression which is the difference of F and G

(defun make-difference (F G)
    "Constructs an expression which is the difference of F and G."
    (cond
        						
        ((equal F 0) (make-neg G))			;;; 0 minus G yields the negation of G
        ((equal G 0) F)				;;; F minus 0 yields F
       ((and (number-p F) (number-p G)) (- F G))     ;;; If both are numbers, perform subtraction
        ((equal F (make-neg G)) (make-sum F G))
        (t (list *diff-symbol* F G))))		;;; Return the list containing the difference expression

;;;——————————————————————————————————————————————————————————————————————————
;;; Name: make-prod
;;; Args: Expressions F and G
;;; Returns: The product of F and G

(defun make-prod (F G)
    "Constructs an expression which is the product of F and G."
    (cond
	((equal F 0) 0)							;;; F or G time 0 is 0
	((equal G 0) 0)	
	((equal F 1) G)							;;; F or G times 1 is itself
	((equal G 1) F)
	((equal F -1) (make-neg G))				;;; F or G times -1 = -F or -G
	((equal G -1) (make-neg F))								
	((and (negative-p F) (negative-p G)) 				;;; If F and G are both negative,
		(make-prod (make-neg F) (make-neg G)))	;;; make both positive and multiply
	((and (number-p F) (number-p G)) (* F G))			;;; If both are numbers, multiply
        (t (list *prod-symbol* F G))))				;;; Return the list containing the product 
									;;; expression

;;;——————————————————————————————————————————————————————————————————————————
;;;    NAME: make-quot
;;;  ARG(S): Expressions F and G
;;; RETURNS: The quotient of F and G

(defun make-quot (F G)
    "Constructs an expression which is the quotient of F and G."
    (cond
	((equal F 0) 0)    				;;; 0 divided by anything is 0
	((equal G 0) nil)        			;;; Division by 0 not allowed
	((and (number-p F) (number-p G)) (/ F G))	;;; If both are numbers, perform division 
        (t (list *quot-symbol* F G))))		;;; Return the list containing the difference expression

;;;——————————————————————————————————————————————————————————————————————————
;;;    NAME: make-power
;;;  ARG(S): Variable V and integer N
;;; RETURNS: V raised to the Nth power

(defun make-power (V N)
    "Constructs an expression which is V raised to the Nth power."
    (cond
	((and (number-p V) (numberp N)) (expt V N))	;;; If both are numbers, perform exponentiation
	(t (list *pow-symbol* V N))))		;;; Return the list containing the power expression

;;;——————————————————————————————————————————————————————————————————————————
;;;    Name: make-log
;;;  Arg(s): Variable V
;;; Returns: An expression which is the mathemetical logarithm of V

(defun make-log (V)
    "Constructs an expression which is the mathematical logarithm of V."
    (cond
	((variable-p V) (list *log-symbol* V))))	;;; If V is a variable, return the
							;;; list containing the logarithmic expression

