(require '[clojure.test :refer [is deftest run-tests]])

(deftest test-palabra-reservada?
	;input-output
	(is (= true (palabra-reservada? 'INPUT)))
	(is (= true (palabra-reservada? 'PRINT)))
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


(run-tests)