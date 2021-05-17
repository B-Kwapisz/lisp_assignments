(in-package "PROJECT!") ;;Required to include in program

;;Rules:

(setf *rules* '((     (1)    ->    (0)      R1);;;final state requires '0'
	        (    (1 1)   ->    (1)      R2)
		(    (a)     ->    (1)      R3)
		(   (a a b)  ->   (a a)   R4)
		(    (a b)   ->   (a a)     R5)
		(  (a b c)   ->   (a b)     R6)
		(  (a b c d) ->  (b c d)    R7))

;;end of file
