(require '[clojure.test :refer [is deftest run-tests]])

(load-file "src/basic.clj")

(deftest test-palabra-reservada?
	;input-output
	(is (= true (palabra-reservada? 'INPUT)))
	(is (= true (palabra-reservada? 'PRINT)))
	(is (= true (palabra-reservada? '?)))
	;data
	(is (= true (palabra-reservada? 'DATA)))
	(is (= true (palabra-reservada? 'READ)))
	(is (= true (palabra-reservada? 'REM)))
	(is (= true (palabra-reservada? 'RESTORE)))
	;environment
	(is (= true (palabra-reservada? 'CLEAR)))
	(is (= true (palabra-reservada? 'LET)))
	(is (= true (palabra-reservada? 'LIST)))
	(is (= true (palabra-reservada? 'NEW)))
	(is (= true (palabra-reservada? 'RUN)))
	;control flow
	(is (= true (palabra-reservada? 'END)))
	(is (= true (palabra-reservada? 'FOR)))
	(is (= true (palabra-reservada? 'TO)))
	(is (= true (palabra-reservada? 'NEXT)))
	(is (= true (palabra-reservada? 'STEP)))
	(is (= true (palabra-reservada? 'GOSUB)))
	(is (= true (palabra-reservada? 'RETURN)))
	(is (= true (palabra-reservada? 'GOTO)))
	(is (= true (palabra-reservada? 'IF)))
	(is (= true (palabra-reservada? 'THEN)))
	(is (= true (palabra-reservada? 'ON)))
	;functions
	(is (= true (palabra-reservada? 'ATN)))
	(is (= true (palabra-reservada? 'INT)))
	(is (= true (palabra-reservada? 'SIN)))
	;strings
	(is (= true (palabra-reservada? 'LEN)))
	(is (= true (palabra-reservada? 'MID$)))
	;cast
	(is (= true (palabra-reservada? 'ASC)))
	(is (= true (palabra-reservada? 'CHR$)))	
	(is (= true (palabra-reservada? 'STR$)))

	;not reserved words
	(is (= false (palabra-reservada? 'NUMBER)))
	(is (= false (palabra-reservada? 'WHILE)))
	(is (= false (palabra-reservada? 'SPACE)))
	(is (= false (palabra-reservada? 'ATOM)))
	(is (= false (palabra-reservada? '+)))
)

(deftest test-operador?
	;arithmetic
	(is (= true (operador? '+)))
	(is (= true (operador? '-)))
	(is (= true (operador? '*)))
	(is (= true (operador? '/)))
	(is (= true (operador? '\^)))
	;relational
	(is (= true (operador? '=)))
	(is (= true (operador? '<>)))
	(is (= true (operador? '<)))
	(is (= true (operador? '<=)))
	(is (= true (operador? '>)))
	(is (= true (operador? '>=)))
	;logical
	(is (= true (operador? 'AND)))
	(is (= true (operador? 'OR)))


	;not operators
	(is (= false (operador? '**)))
	(is (= false (operador? '%)))
	(is (= false (operador? '&&)))
	(is (= false (operador? '||)))
)

(deftest test-es-numero?
	;numbers
	(is (= true (es-numero? "1")))
	(is (= true (es-numero? ".1")))
	(is (= true (es-numero? "0.1")))
	(is (= true (es-numero? ".")))
	(is (= true (es-numero? "1.1")))
	;not numbers
	(is (= false (es-numero? "1,1")))
	(is (= false (es-numero? "1e16")))
	(is (= false (es-numero? "1k")))
	(is (= false (es-numero? "1k")))
)

(deftest test-es-cadena?
	;string
	(is (= true (es-cadena? "\"1\"")))
	(is (= true (es-cadena? "\"HOLA COMO ESTAS\"")))
	(is (= true (es-cadena? "\"127.0.0.1\"")))
	;not string
	(is (= false (es-cadena? "HOLA\"")))
	(is (= false (es-cadena? "\"HOLA")))
	(is (= false (es-cadena? "HOLA")))
)

(deftest test-es-posible-nombre-de-variable?
	;is a possible name
	(is (= true (es-posible-nombre-de-variable? 'ITERADOR)))
	(is (= true (es-posible-nombre-de-variable? 'A$)))
	(is (= true (es-posible-nombre-de-variable? 'X%)))
	(is (= true (es-posible-nombre-de-variable? 'CADENA$)))
	;not a possible name
	(is (= false (es-posible-nombre-de-variable? '$)))
	(is (= false (es-posible-nombre-de-variable? '%)))
	(is (= false (es-posible-nombre-de-variable? "")))
	(is (= false (es-posible-nombre-de-variable? 'LET)))
	(is (= false (es-posible-nombre-de-variable? 'READ)))
)

(deftest test-anular-invalidos
	(is (= '(PRINT "HOLA") (anular-invalidos '(PRINT "HOLA"))))
	(is (= '(IF X nil * Y < 12 THEN LET nil X = 0) (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0))))
	(is (= '(IF B >= 10 AND B <= 15 THEN H$ = CHR$) (anular-invalidos '(IF B >= 10 AND B <= 15 THEN H$ = CHR$))))
	(is (= '(nil B >= 10 AND nil B <= 15 THEN H$ nil nil CHR$) (anular-invalidos '(IF? B >= 10 AND !! B <= 15 THEN H$ !% != CHR$))))
)

(run-tests)