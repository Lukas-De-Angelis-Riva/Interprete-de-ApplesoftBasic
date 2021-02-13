(ns test
  (:require [clojure.test :refer :all]
            [basic :refer :all]))

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
	;functions
	(is (= true (operador? 'ATN)))
	(is (= true (operador? 'INT)))
	(is (= true (operador? 'SIN)))
	;strings
	(is (= true (operador? 'LEN)))
	(is (= true (operador? 'MID$)))
	;cast
	(is (= true (operador? 'ASC)))
	(is (= true (operador? 'CHR$)))	
	(is (= true (operador? 'STR$)))

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
	(is (= true (es-cadena? (symbol "\"HOLA\""))))
)

(deftest test-es-posible-nombre-de-variable?
	;is a possible name
	(is (= true (es-posible-nombre-de-variable? 'ITERADOR)))
	(is (= true (es-posible-nombre-de-variable? 'A$)))
	(is (= true (es-posible-nombre-de-variable? 'X%)))
	(is (= true (es-posible-nombre-de-variable? 'CADENA$)))
	(is (= true (es-posible-nombre-de-variable? 'HOLA1)))
	(is (= true (es-posible-nombre-de-variable? 'H2O)))
	;not a possible name
	(is (= false (es-posible-nombre-de-variable? '$)))
	(is (= false (es-posible-nombre-de-variable? '%)))
	(is (= false (es-posible-nombre-de-variable? "")))
	(is (= false (es-posible-nombre-de-variable? 'LET)))
	(is (= false (es-posible-nombre-de-variable? 'READ)))
	(is (= false (es-posible-nombre-de-variable? 'h2o)))
	(is (= false (es-posible-nombre-de-variable? 'x)))
	(is (= false (es-posible-nombre-de-variable? 'hola)))
	(is (= false (es-posible-nombre-de-variable? (symbol "1hola"))))
	(is (= false (es-posible-nombre-de-variable? (symbol "1hola$"))))
)

(deftest test-anular-invalidos
	(is (= '(PRINT "HOLA") 
			(anular-invalidos '(PRINT "HOLA"))))
	(is (= '(IF X nil * Y < 12 THEN LET nil X = 0)
			(anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0))))
	(is (= '(IF B >= 10 AND B <= 15 THEN H$ = CHR$)
			(anular-invalidos '(IF B >= 10 AND B <= 15 THEN H$ = CHR$))))
	(is (= '(IF B >= 10 AND nil nil B <= 15 THEN H$ nil nil nil = CHR$)
			(anular-invalidos '(IF B >= 10 AND ! ! B <= 15 THEN H$ ! % ! = CHR$))))
	(is (= '()
			(anular-invalidos '())))
	(is (= (list 'INPUT (symbol "\"WHAT IS YOUR NAME?\"") (symbol ";") 'N$)
			(anular-invalidos (list 'INPUT (symbol "\"WHAT IS YOUR NAME?\"") (symbol ";") 'N$))))
	(is (= (list nil 'A nil '2 nil (symbol "=") '234 'L '21)
			(anular-invalidos (list '& 'A '& '2 (symbol "\"") (symbol "=") '234 'L '21))))
)

(deftest test-cargar-linea
	(is (= '[((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(10 (PRINT X)) [() [:ejecucion-inmediata 0] [] [] [] 0 {}])))
	(is (= '[((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(20 (X = 100)) ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
	(is (= '[((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(15 (X = X + 1)) ['((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
	(is (= '[((10 (PRINT X)) (15 (X = X - 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(15 (X = X - 1)) ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
)

(deftest test-es-next?
	(is (= true (es-next? (list 'NEXT 'A))))
	(is (= true (es-next? (list 'NEXT 'A (symbol ",") 'B))))
	(is (= false (es-next? (list 'PRINT "HOLA"))))
	(is (= false (es-next? '())))
)

(deftest test-expandir
	(is (= (list (list 'NEXT 'A)) (expandir (list 'NEXT 'A))))
	(is (= (list (list 'NEXT 'A) (list 'NEXT 'B)) (expandir (list 'NEXT 'A (symbol ",") 'B))))
	(is (= (list (list 'NEXT 'A) (list 'NEXT 'B) (list 'NEXT 'C)) (expandir (list 'NEXT 'A (symbol ",") 'B (symbol ",") 'C))))
)

(deftest test-expandir-nexts
	(is (= '((PRINT 1) (NEXT A) (NEXT B)) (expandir-nexts (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B)))))
	(is (= '((PRINT 1) (NEXT A) (NEXT B) (NEXT C) (NEXT D)) (expandir-nexts (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B (symbol ",") 'C) (list 'NEXT 'D)))))
	(is (= '((NEXT A)) (expandir-nexts (list (list 'NEXT 'A)))))
	(is (= '((PRINT 1)) (expandir-nexts '((PRINT 1)))))
)

(deftest test-variable-float?
	(is (= true (variable-float? 'X)))
	(is (= true (variable-float? 'ITERADOR)))
	(is (= false (variable-float? 'X$)))
	(is (= false (variable-float? 'X%)))
	(is (= false (variable-float? 'ITERADOR$)))
	(is (= false (variable-float? 'ITERADOR%)))
)

(deftest test-variable-integer?
	(is (= true (variable-integer? 'X%)))
	(is (= true (variable-integer? 'ITERADOR%)))
	(is (= false (variable-integer? 'X)))
	(is (= false (variable-integer? 'X$)))
	(is (= false (variable-integer? 'ITERADOR)))
	(is (= false (variable-integer? 'ITERADOR$)))
)

(deftest test-variable-string?
	(is (= true (variable-string? 'X$)))
	(is (= true (variable-string? 'ITERADOR$)))
	(is (= false (variable-string? 'X)))
	(is (= false (variable-string? 'X%)))
	(is (= false (variable-string? 'ITERADOR)))
	(is (= false (variable-string? 'ITERADOR%)))
)

(deftest test-contar-sentencias
	(is (= 2 (contar-sentencias 10 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
	(is (= 1 (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
	(is (= 2 (contar-sentencias 20 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
	(is (= 0 (contar-sentencias 25 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
)

(deftest test-buscar-lineas-restantes
	(is (= (buscar-lineas-restantes [() [:ejecucion-inmediata 0] [] [] [] 0 {}]) nil))
	(is (= (buscar-lineas-restantes ['((PRINT X) (PRINT Y)) [:ejecucion-inmediata 2] [] [] [] 0 {}]) nil))
	(is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 2] [] [] [] 0 {}]) (list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J)))))
	(is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}]) (list '(10 (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J)))))
	(is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 0] [] [] [] 0 {}]) (list '(10) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J)))))
	(is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}]) (list '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J)))))
	(is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 0] [] [] [] 0 {}]) (list '(15) (list 20 (list 'NEXT 'I (symbol ",") 'J)))))
	(is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]) (list '(20 (NEXT I) (NEXT J)))))
	(is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 2] [] [] [] 0 {}]) (list '(20 (NEXT I) (NEXT J)))))
	(is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 1] [] [] [] 0 {}]) (list '(20 (NEXT J)))))
	(is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 0] [] [] [] 0 {}]) (list '(20))))
	(is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 -1] [] [] [] 0 {}]) (list '(20))))
	(is (= (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [25 0] [] [] [] 0 {}]) nil))
)

(deftest test-continuar-linea
	(is (= (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]) 
			[nil [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]]))
	(is (= (continuar-linea [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2]] [] [] 0 {}]) 
			[:omitir-restante [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}]]))
)

(deftest test-quitar-rem
	(is (= (quitar-rem '(10 (PRINT X) (REM ESTE NO) (DATA 30))) '(10 (PRINT X))))
	(is (= (quitar-rem '(10 (REM ESTE NO) (PRINT X) (PRINT Y))) '(10)))
)

(deftest test-extraer-data 
	(is (= (extraer-data (list '(10 (PRINT X) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))
			'(30 "HOLA" "MUNDO" 10 20)))
	(is (= (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))
			'("HOLA" "MUNDO" 10 20)))
	(is (= (extraer-data (list (list 10 (list 'REM 'ESTO 'NO 'VA)))) '()))
    (is (= (extraer-data '(())) '()))
)

(deftest test-ejecutar-asignacion
	(is (= (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 {}]) 
			'[((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}]))
	(is (= (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])
			'[((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}]))
	(is (= (ejecutar-asignacion '(X = X + 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])
			'[((10 (PRINT X))) [10 1] [] [] [] 0 {X 3}]))
	(is (= (ejecutar-asignacion '(X$ = X$ + " MUNDO") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])
			'[((10 (PRINT X))) [10 1] [] [] [] 0 {X$ "HOLA MUNDO"}]))
)

(deftest test-buscar-valor
	(is (= 4
			(buscar-valor 'CUATRO {'CUATRO 4})))
	(is (= "HOLA"
			(buscar-valor 'X$ {'X$ "HOLA"})))
	(is (= 0 
			(buscar-valor 'X% {'Y$ "NO ESTA X%"})))
	(is (= 0	
			(buscar-valor 'X {'Y$ "NO ESTA X"})))
	(is (= ""
			(buscar-valor 'X$ {'Y$ "NO ESTA X$"})))
)

(deftest test-preprocesar-expresion
	(is (= '("HOLA" + " MUNDO" + "")
			(preprocesar-expresion '(X$ + " MUNDO" + Z$) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))
	(is (= '(5 + 0 / 2 * 0)
			(preprocesar-expresion '(X + . / Y% * Z) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5 Y% 2}])))
	(is (= '(0)
			(preprocesar-expresion '(X) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{Y% 2}])))
)

(deftest test-desambiguar
	(is (= (list (symbol "-u") (symbol "2") (symbol "*") (symbol "(") (symbol "-u") (symbol "3") (symbol "+") (symbol "5") (symbol "-") (symbol "(") (symbol "2") (symbol "/") (symbol "7"))
			(desambiguar (list (symbol "-u") (symbol "2") (symbol "*") (symbol "(") (symbol "-u") (symbol "3") (symbol "+") (symbol "5") (symbol "-") (symbol "(") (symbol "2") (symbol "/") (symbol "7")))))
	(is (= (list (symbol "MID$") (symbol "(") (symbol "1") (symbol ",") (symbol "2") (symbol ")"))
			(desambiguar (list (symbol "MID$") (symbol "(") (symbol "1") (symbol ",") (symbol "2") (symbol ")")))))
	(is (= (list (symbol "MID3$") (symbol "(") (symbol "1") (symbol ",") (symbol "2") (symbol ",") (symbol "3") (symbol ")"))
			(desambiguar (list (symbol "MID$") (symbol "(") (symbol "1") (symbol ",") (symbol "2") (symbol ",") (symbol "3") (symbol ")")))))
	(is (= (list (symbol "MID3$") (symbol "(") (symbol "1") (symbol ",") (symbol "-u") (symbol "2") (symbol "+") (symbol "K") (symbol ",") (symbol "3") (symbol ")"))
			(desambiguar (list (symbol "MID$") (symbol "(") (symbol "1") (symbol ",") (symbol "-u") (symbol "2") (symbol "+") (symbol "K") (symbol ",") (symbol "3") (symbol ")")))))
	(is (= (list (symbol "MID3$") (symbol "(") (symbol "\"HOLA MUNDO\"") (symbol ",") (symbol "2") (symbol ",") (symbol "LEN") (symbol "(") (symbol "MID$") (symbol "(") (symbol "\"HOLA\"") (symbol ",") (symbol "1") (symbol ")") (symbol ")") (symbol")"))
			(desambiguar (list (symbol "MID$") (symbol "(") (symbol "\"HOLA MUNDO\"") (symbol ",") (symbol "2") (symbol ",") (symbol "LEN") (symbol "(") (symbol "MID$") (symbol "(") (symbol "\"HOLA\"") (symbol ",") (symbol "1") (symbol ")") (symbol ")") (symbol")")))))
	(is (= (list (symbol "MID3$") (symbol "(") (symbol "\"HOLA MUNDO\"") (symbol ",") (symbol "2") (symbol ",") (symbol "LEN") (symbol "(") (symbol "MID3$") (symbol "(") (symbol "\"HOLA\"") (symbol ",") (symbol "2") (symbol ",") (symbol "1") (symbol ")") (symbol ")") (symbol")"))
			(desambiguar (list (symbol "MID$") (symbol "(") (symbol "\"HOLA MUNDO\"") (symbol ",") (symbol "2") (symbol ",") (symbol "LEN") (symbol "(") (symbol "MID$") (symbol "(") (symbol "\"HOLA\"") (symbol ",") (symbol "2") (symbol ",") (symbol "1") (symbol ")") (symbol ")") (symbol")")))))
)

(deftest test-eliminar-cero-decimal
    (is (= 1.5 (eliminar-cero-decimal (symbol "1.5"))))
    (is (= 1.5 (eliminar-cero-decimal (symbol "1.50"))))
    (is (= 1.05 (eliminar-cero-decimal (symbol "1.050"))))
    (is (= 1 (eliminar-cero-decimal (symbol "1.00000"))))
    (is (= 0.004 (eliminar-cero-decimal (symbol "0.0040000"))))
    (is (= (symbol "A") (eliminar-cero-decimal (symbol "A"))))
    (is (= 0 (eliminar-cero-decimal (symbol "."))))
    (is (= 31.123 (eliminar-cero-decimal (symbol "000031.123"))))
)

(deftest test-eliminar-cero-entero
	(is (= (eliminar-cero-entero nil) nil))
	(is (= (eliminar-cero-entero 'A) "A"))
	(is (= (eliminar-cero-entero (symbol "0")) "0"))
	(is (= (eliminar-cero-entero (symbol "1.5")) "1.5"))
	(is (= (eliminar-cero-entero (symbol "1")) "1"))
	(is (= (eliminar-cero-entero (symbol "-1")) "-1"))
	(is (= (eliminar-cero-entero (symbol "-1.5")) "-1.5"))
	(is (= (eliminar-cero-entero (symbol "0.5")) ".5"))
	(is (= (eliminar-cero-entero (symbol "-0.5")) "-.5"))
	(is (= (eliminar-cero-entero (symbol "-0.413221")) "-.413221"))
	(is (= (eliminar-cero-entero (symbol "-0")) "0"))
)