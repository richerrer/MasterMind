iimport System.Random
generar_combinaciones :: [([Int],Int)]
generar_combinaciones = let
                         numeros =[1..6]
                         combinaciones = [([w,x,y,z],0)|w<-numeros,x<-numeros,y<-numeros,z<-numeros]
					    in combinaciones
						
main::[Int]->[Int]->[Int]->[Int]->[([Int],Int)]->IO()
main code cfg last_guess [4,0] combinaciones=do
	putStr $ "TERMINO "
main code [] [] [] []=do
	gen1 <- newStdGen
	gen2 <- newStdGen
	gen3 <- newStdGen
	gen4 <- newStdGen
	let
		rand1 = gen1
		rand2 = gen2
		rand3 = gen3
		rand4 = gen4
		combinaciones = generar_combinaciones
		cfg =[fst(randomR (1,6)(rand1) :: (Int, StdGen)),fst(randomR (1,6)(rand2) :: (Int, StdGen)),fst(randomR (1,6)(rand3) :: (Int, StdGen)),fst(randomR (1,6)(rand4) :: (Int, StdGen))]--fst(randomR (1,6)(rand1) :: (Int, StdGen)),fst(randomR (1,6)(rand2) :: (Int, StdGen)),fst(randomR (1,6)(rand3) :: (Int, StdGen)),fst(randomR (1,6)(rand4) :: (Int, StdGen))
		nuevas_combinaciones = eliminar_segunCodigo combinaciones cfg []
		score = score_complete code cfg
	printMessage cfg cfg score score
	main code cfg cfg score combinaciones
main code cfg last_guess [0,1] combinaciones = do
    gen1 <- newStdGen
    gen2 <- newStdGen
    gen3 <- newStdGen
    gen4 <- newStdGen
    gen5 <- newStdGen
    gen6 <- newStdGen
    gen7 <- newStdGen
    let
         rand1 = gen1
         rand2 = gen2
         rand3 = gen3
         rand4 = gen4
         rand5 = gen5
         rand6 = gen6
         rand7 = gen7
         num1 = fst(randomR (1,4)(rand1) :: (Int, StdGen))--numero para elegir una posicion del 1 al 4 debido a que son solo 4 los digitos para el codigo, no selecciona numeros del 1 al 6 debido a que no selecciona un codigo aleatorio sino solo una posicion
         num2 = fst(randomR (1,4)(rand2) :: (Int, StdGen))--numero para elegir una posicion del 1 al 4 debido a que son solo 4 los digitos para el codigo, no selecciona numeros del 1 al 6 debido a que no selecciona un codigo aleatorio sino solo una posicion
         num3 = fst(randomR (1,4)(rand3) :: (Int, StdGen))--numero para elegir una posicion del 1 al 4 debido a que son solo 4 los digitos para el codigo, no selecciona numeros del 1 al 6 debido a que no selecciona un codigo aleatorio sino solo una posicion
         num4 = fst(randomR (1,4)(rand4) :: (Int, StdGen))--numero para elegir una posicion del 1 al 4 debido a que son solo 4 los digitos para el codigo, no selecciona numeros del 1 al 6 debido a que no selecciona un codigo aleatorio sino solo una posicion
         num5 = fst(randomR (1,6)(rand5) :: (Int, StdGen))--numero para el nuevo codigo potencial del 1 al 6 (no posicion) para usarlo en el paso 3.
         num6 = fst(randomR (1,6)(rand6) :: (Int, StdGen))--numero para el nuevo codigo potencial del 1 al 6 (no posicion) para usarlo en el paso 3.
         num7 = fst(randomR (1,6)(rand7) :: (Int, StdGen))--numero para el nuevo codigo potencial del 1 al 6 (no posicion) para usarlo en el paso 3.
    if (num1/=num2 && num1/=num3 && num1/=num4 && num2/=num3 && num2/=num4 && num3/=num4)
         then do
                 let
                     randon_pos = [num1,num2,num3,num4]
                     array_forstep2 = principal_step2 (cfg) [num1] [] [] 1 --obtengo un array con las numeros de las posiciones que deseo cambiar con sus nuevas posiciones y le envio que numeros ya no debe elegir en este caso es vacio porque no se realizo el paso 1
                 if verify_for_step3 [num5,num6,num7] (array_forstep2)                  --verifica que los 3 numeros para el paso 3 no esten en mi posible nuevo codigo potencial
                     then do
                             --imprime [0,0,0,0,num1,num2,num3,num4,num5,num6,num7]
                             let
                                 incorrect_pos = extraer_posiciones (array_forstep2) []
                                 array_forstep3 = principal_step3 [num5,num6,num7] (incorrect_pos) (1) [] --obtiene el arreglo con los numeros y en posiciones que me devuelve el paso 3
                                 new_guess_code = result_fornew_cfg (array_forstep2++array_forstep3) (array_forstep2++array_forstep3) [] 1
                             if comparar_existencia_codigo (combinaciones)(new_guess_code)
                                 then main code cfg last_guess [0,1] combinaciones --si el nuevo codigo que obtuvo ya habia sido seleccionado anteriormente, vuelve a llamar a la funcion para que vuelva a generar el code guess segun los pasos dados
                                 else do
                                         let
                                             nuevas_combinaciones = eliminar_segunCodigo (combinaciones) (new_guess_code) []
                                             score = score_complete (code) (new_guess_code)
                                             new_cfg = selec_potencial_code (cfg) ([0,1]) (new_guess_code) (score)
                                             score_ofnew_cfg = selec_bestScore ([0,1]) (score)
                                         if score == [0,0]
                                             then do
                                                     printMessage (new_guess_code) (new_cfg) (score)(score_ofnew_cfg)
                                                     main (code) (new_cfg) (new_guess_code) ([0,0]) (nuevas_combinaciones)
                                             else do
                                                     printMessage (new_guess_code) (new_cfg) (score)(score_ofnew_cfg)
                                                     main (code) (new_cfg) (new_guess_code) (score_ofnew_cfg) (nuevas_combinaciones)
                     else main code cfg last_guess [0,1] combinaciones  --si algun  numero para el paso 3 esta en nuestro posible cfg se vuelve a repetir la funcion
         else main code cfg last_guess [0,1] combinaciones  --si algunas de las posiciones random son iguales, se vuelve a llamar a la misma funcion con los mismos parametros hasta que estos numerosrandom, sean diferentes
main code cfg last_guess [1,0] combinaciones = do
    gen1 <- newStdGen
    gen2 <- newStdGen
    gen3 <- newStdGen
    gen4 <- newStdGen
    gen5 <- newStdGen
    gen6 <- newStdGen
    gen7 <- newStdGen
    let
         rand1 = gen1
         rand2 = gen2
         rand3 = gen3
         rand4 = gen4
         rand5 = gen5
         rand6 = gen6
         rand7 = gen7
         num1 = fst(randomR (1,4)(rand1) :: (Int, StdGen))--numero para elegir una posicion del 1 al 4 debido a que son solo 4 los digitos para el codigo, no selecciona numeros del 1 al 6 debido a que no selecciona un codigo aleatorio sino solo una posicion
         num2 = fst(randomR (1,4)(rand2) :: (Int, StdGen))--numero para elegir una posicion del 1 al 4 debido a que son solo 4 los digitos para el codigo, no selecciona numeros del 1 al 6 debido a que no selecciona un codigo aleatorio sino solo una posicion
         num3 = fst(randomR (1,4)(rand3) :: (Int, StdGen))--numero para elegir una posicion del 1 al 4 debido a que son solo 4 los digitos para el codigo, no selecciona numeros del 1 al 6 debido a que no selecciona un codigo aleatorio sino solo una posicion
         num4 = fst(randomR (1,4)(rand4) :: (Int, StdGen))--numero para elegir una posicion del 1 al 4 debido a que son solo 4 los digitos para el codigo, no selecciona numeros del 1 al 6 debido a que no selecciona un codigo aleatorio sino solo una posicion
         num5 = fst(randomR (1,6)(rand5) :: (Int, StdGen))--numero para el nuevo codigo potencial del 1 al 6 (no posicion) para usarlo en el paso 3.
         num6 = fst(randomR (1,6)(rand6) :: (Int, StdGen))--numero para el nuevo codigo potencial del 1 al 6 (no posicion) para usarlo en el paso 3.
         num7 = fst(randomR (1,6)(rand7) :: (Int, StdGen))--numero para el nuevo codigo potencial del 1 al 6 (no posicion) para usarlo en el paso 3.
    if (num1/=num2 && num1/=num3 && num1/=num4 && num2/=num3 && num2/=num4 && num3/=num4)
         then do
                 let
                     randon_pos = [num1,num2,num3,num4]
                     array_forstep1 = principal_step1 (cfg) [num1] (1) []               --obtengo un array para mantener las posiciones segun la lista de las posiciones.
                 if verify_for_step3 [num5,num6,num7] (array_forstep1)                  --verifica que los 3 numeros para el paso 3 no esten en mi posible nuevo codigo potencial
                     then do
                             let
                                 incorrect_pos = extraer_posiciones (array_forstep1) []
                                 array_forstep3 = principal_step3 [num5,num6,num7] (incorrect_pos) (1) [] --obtiene el arreglo con los numeros y en posiciones que me devuelve el paso 3
                                 new_guess_code = result_fornew_cfg (array_forstep1++array_forstep3) (array_forstep1++array_forstep3) [] 1
                             if comparar_existencia_codigo (combinaciones)(new_guess_code)
                                 then main code cfg last_guess [1,0] combinaciones --si el nuevo codigo que obtuvo ya habia sido seleccionado anteriormente, vuelve a llamar a la funcion para que vuelva a generar el code guess segun los pasos dados
                                 else do
                                         let
                                             nuevas_combinaciones = eliminar_segunCodigo (combinaciones) (new_guess_code) []
                                             score = score_complete (code) (new_guess_code)
                                             new_cfg = selec_potencial_code (cfg) ([1,0]) (new_guess_code) (score)
                                             score_ofnew_cfg = selec_bestScore ([1,0]) (score)
                                         if score == [0,0]
                                             then do
                                                     printMessage (new_guess_code) (new_cfg) (score)(score_ofnew_cfg)
                                                     main (code) (new_cfg) (new_guess_code) ([0,0]) (nuevas_combinaciones)
                                             else do
                                                     printMessage (new_guess_code) (new_cfg) (score)(score_ofnew_cfg)
                                                     main (code) (new_cfg) (new_guess_code) (score_ofnew_cfg) (nuevas_combinaciones)
                     else main code cfg last_guess [1,0] combinaciones  --si algun  numero para el paso 3 esta en nuestro posible cfg se vuelve a repetir la funcion
         else main code cfg last_guess [1,0] combinaciones  --si algunas de las posiciones random son iguales, se vuelve a llamar a la misma funcion con los mismos parametros hasta que estos numerosrandom, sean diferentes
main code cfg last_guess [0,2] combinaciones = do
    gen1 <- newStdGen
    gen2 <- newStdGen
    gen3 <- newStdGen
    gen4 <- newStdGen
    gen5 <- newStdGen
    gen6 <- newStdGen
    gen7 <- newStdGen
    let
         rand1 = gen1
         rand2 = gen2
         rand3 = gen3
         rand4 = gen4
         rand5 = gen5
         rand6 = gen6
         rand7 = gen7
         num1 = fst(randomR (1,4)(rand1) :: (Int, StdGen))--numero para elegir una posicion del 1 al 4 debido a que son solo 4 los digitos para el codigo, no selecciona numeros del 1 al 6 debido a que no selecciona un codigo aleatorio sino solo una posicion
         num2 = fst(randomR (1,4)(rand2) :: (Int, StdGen))--numero para elegir una posicion del 1 al 4 debido a que son solo 4 los digitos para el codigo, no selecciona numeros del 1 al 6 debido a que no selecciona un codigo aleatorio sino solo una posicion
         num3 = fst(randomR (1,4)(rand3) :: (Int, StdGen))--numero para elegir una posicion del 1 al 4 debido a que son solo 4 los digitos para el codigo, no selecciona numeros del 1 al 6 debido a que no selecciona un codigo aleatorio sino solo una posicion
         num4 = fst(randomR (1,4)(rand4) :: (Int, StdGen))--numero para elegir una posicion del 1 al 4 debido a que son solo 4 los digitos para el codigo, no selecciona numeros del 1 al 6 debido a que no selecciona un codigo aleatorio sino solo una posicion
         num5 = fst(randomR (1,6)(rand5) :: (Int, StdGen))--numero para el nuevo codigo potencial del 1 al 6 (no posicion) para usarlo en el paso 3.
         num6 = fst(randomR (1,6)(rand6) :: (Int, StdGen))--numero para el nuevo codigo potencial del 1 al 6 (no posicion) para usarlo en el paso 3.
         num7 = fst(randomR (1,6)(rand7) :: (Int, StdGen))--numero para el nuevo codigo potencial del 1 al 6 (no posicion) para usarlo en el paso 3.
    if (num1/=num2 && num1/=num3 && num1/=num4 && num2/=num3 && num2/=num4 && num3/=num4)
         then do
                 let
                     randon_pos = [num1,num2,num3,num4]
                     array_forstep2 = principal_step2 (cfg) [num1,num2] [] [] 1
                 if verify_for_step3 [num5,num6] (array_forstep2)                  --verifica que los numeros para el paso 3 no esten en mi posible nuevo codigo potencial
                     then do
                             let
                                 incorrect_pos = extraer_posiciones (array_forstep2) []
                                 array_forstep3 = principal_step3 [num5,num6] (incorrect_pos) (1) [] --obtiene el arreglo con los numeros y en posiciones que me devuelve el paso 3
                                 new_guess_code = result_fornew_cfg (array_forstep2++array_forstep3) (array_forstep2++array_forstep3) [] 1
                             if comparar_existencia_codigo (combinaciones)(new_guess_code)
                                 then main code cfg last_guess [0,2] combinaciones --si el nuevo codigo que obtuvo ya habia sido seleccionado anteriormente, vuelve a llamar a la funcion para que vuelva a generar el code guess segun los pasos dados
                                 else do
                                         let
                                             nuevas_combinaciones = eliminar_segunCodigo (combinaciones) (new_guess_code) []
                                             score = score_complete (code) (new_guess_code)
                                             new_cfg = selec_potencial_code (cfg) ([0,2]) (new_guess_code) (score)
                                             score_ofnew_cfg = selec_bestScore ([0,2]) (score)
                                         if score == [0,0]
                                             then do
                                                     printMessage (new_guess_code) (new_cfg) (score)(score_ofnew_cfg)
                                                     main (code) (new_cfg) (new_guess_code) ([0,0]) (nuevas_combinaciones)
                                             else do
                                                     printMessage (new_guess_code) (new_cfg) (score)(score_ofnew_cfg)
                                                     main (code) (new_cfg) (new_guess_code) (score_ofnew_cfg) (nuevas_combinaciones)
                     else main code cfg last_guess [0,2] combinaciones  --si algun  numero para el paso 3 esta en nuestro posible cfg se vuelve a repetir la funcion
         else main code cfg last_guess [0,2] combinaciones  --si algunas de las posiciones random son iguales, se vuelve a llamar a la misma funcion con los mismos parametros hasta que estos numerosrandom, sean diferentes
main code cfg last_guess [1,1] combinaciones = do
    gen1 <- newStdGen
    gen2 <- newStdGen
    gen3 <- newStdGen
    gen4 <- newStdGen
    gen5 <- newStdGen
    gen6 <- newStdGen
    gen7 <- newStdGen
    let
         rand1 = gen1
         rand2 = gen2
         rand3 = gen3
         rand4 = gen4
         rand5 = gen5
         rand6 = gen6
         rand7 = gen7
         num1 = fst(randomR (1,4)(rand1) :: (Int, StdGen))--numero para elegir una posicion del 1 al 4 debido a que son solo 4 los digitos para el codigo, no selecciona numeros del 1 al 6 debido a que no selecciona un codigo aleatorio sino solo una posicion
         num2 = fst(randomR (1,4)(rand2) :: (Int, StdGen))--numero para elegir una posicion del 1 al 4 debido a que son solo 4 los digitos para el codigo, no selecciona numeros del 1 al 6 debido a que no selecciona un codigo aleatorio sino solo una posicion
         num3 = fst(randomR (1,4)(rand3) :: (Int, StdGen))--numero para elegir una posicion del 1 al 4 debido a que son solo 4 los digitos para el codigo, no selecciona numeros del 1 al 6 debido a que no selecciona un codigo aleatorio sino solo una posicion
         num4 = fst(randomR (1,4)(rand4) :: (Int, StdGen))--numero para elegir una posicion del 1 al 4 debido a que son solo 4 los digitos para el codigo, no selecciona numeros del 1 al 6 debido a que no selecciona un codigo aleatorio sino solo una posicion
         num5 = fst(randomR (1,6)(rand5) :: (Int, StdGen))--numero para el nuevo codigo potencial del 1 al 6 (no posicion) para usarlo en el paso 3.
         num6 = fst(randomR (1,6)(rand6) :: (Int, StdGen))--numero para el nuevo codigo potencial del 1 al 6 (no posicion) para usarlo en el paso 3.
         num7 = fst(randomR (1,6)(rand7) :: (Int, StdGen))--numero para el nuevo codigo potencial del 1 al 6 (no posicion) para usarlo en el paso 3.
    if (num1/=num2 && num1/=num3 && num1/=num4 && num2/=num3 && num2/=num4 && num3/=num4)
         then do
                 let
                     randon_pos = [num1,num2,num3,num4]
                     array_forstep1 = principal_step1 (cfg) [num1] (1) []
                     array_forstep2 = principal_step2 (cfg) [num2] [num1] [] 1
                 if verify_for_step3 [num5,num6] (array_forstep1++array_forstep2)                  --verifica que los numeros para el paso 3 no esten en mi posible nuevo codigo potencial
                     then do
                             let
                                 incorrect_pos = extraer_posiciones (array_forstep1++array_forstep2) []
                                 array_forstep3 = principal_step3 [num5,num6] (incorrect_pos) (1) [] --obtiene el arreglo con los numeros y en posiciones que me devuelve el paso 3
                                 new_guess_code = result_fornew_cfg (array_forstep1++array_forstep2++array_forstep3) (array_forstep1++array_forstep2++array_forstep3) [] 1
                             if comparar_existencia_codigo (combinaciones)(new_guess_code)
                                 then main code cfg last_guess [1,1] combinaciones --si el nuevo codigo que obtuvo ya habia sido seleccionado anteriormente, vuelve a llamar a la funcion para que vuelva a generar el code guess segun los pasos dados
                                 else do
                                         let
                                             nuevas_combinaciones = eliminar_segunCodigo (combinaciones) (new_guess_code) []
                                             score = score_complete (code) (new_guess_code)
                                             new_cfg = selec_potencial_code (cfg) ([1,1]) (new_guess_code) (score)
                                             score_ofnew_cfg = selec_bestScore ([1,1]) (score)
                                         if score == [0,0]
                                             then do
                                                     printMessage (new_guess_code) (new_cfg) (score)(score_ofnew_cfg)
                                                     main (code) (new_cfg) (new_guess_code) ([0,0]) (nuevas_combinaciones)
                                             else do
                                                     printMessage (new_guess_code) (new_cfg) (score)(score_ofnew_cfg)
                                                     main (code) (new_cfg) (new_guess_code) (score_ofnew_cfg) (nuevas_combinaciones)
                     else main code cfg last_guess [1,1] combinaciones  --si algun  numero para el paso 3 esta en nuestro posible cfg se vuelve a repetir la funcion
         else main code cfg last_guess [1,1] combinaciones  --si algunas de las posiciones random son iguales, se vuelve a llamar a la misma funcion con los mismos parametros hasta que estos numerosrandom, sean diferentes
{-Funcion que se encarga de llamar a las funciones que imprimen los mensajes en el txt
-}
printMessage :: [Int]->[Int]->[Int]->[Int]->IO()
printMessage guess cfg score bestScore= do
                                 imprime2 guess
                                 imprime3 score	
{-
Funcion que imprime un arreglo en un archivo 
-}	
imprime2 :: [Int]->IO()
imprime2 [] = appendFile "solution.txt" "    "
imprime2 x = if (length x) == 4
				then do
					appendFile "solution.txt" ("GUESS: "++show(head x))
					imprime2 (tail x)
				else do
					appendFile "solution.txt" (show(head x))
					imprime2 (tail x)

{-			
Funcion que imprime un arreglo en un archivo
-}	
imprime3 :: [Int]->IO()
imprime3 [] = appendFile "solution.txt" "\n"
imprime3 x = if (length x) == 2
				then do
					appendFile "solution.txt" ("SCORE: "++show(head x)++" ")
					imprime3 (tail x)
				else do
					appendFile "solution.txt" (show(head x))
					imprime3 (tail x)
{-
Funcion step2 que recibe el codigo potencial, los arreglos de los numeros de las posiciones a elegir, un arreglo de las posiciones que no los puede 
poner nuevamente un arreglo de arreglos con los numeros elegidos que al principio se envia vacio para poder ir colocando en el los numeors con las
nuevas posiciones ejemplo [[5,2],[3,1]] 5 en la 2 posicion y 3 en la primera y por ultimo un contador que se envia en 1.
-}
principal_step2::[Int]->[Int]->[Int]->[(Int,Int)]->Int->[(Int,Int)]
principal_step2 cfg random incorrect_position finish_array 5 = finish_array 
principal_step2 cfg random incorrect_position finish_array iterador = if iterador `elem` random 
                                                                         then
															                 let new_pos = new_position iterador incorrect_position (1)
															                     new_incorrect_position = new_pos:incorrect_position
															                 in principal_step2 (tail cfg) random (new_incorrect_position) ((head cfg,new_pos):finish_array)(iterador+1)
															             else principal_step2 (tail cfg) random (incorrect_position) finish_array(iterador+1)

{-
Funcion new_position que recibe el numero de la posicion en la que estoy y una lista con las posiciones a las que no me puedo mover, y un entero que al principio se envia como 1
para que nos ayuda en que posicion nos estamos moviendo es decir un iterador. Si todas las posiciones estan ocupadas se vuelve 0 ya que no puede moverse
a ninguna.
-}		
new_position :: Int->[Int]->Int->Int
new_position actually_pos prohibited_pos 5 = 0
new_position actually_pos prohibited_pos contador = if not(contador `elem` prohibited_pos) && contador /= actually_pos
                                                     then contador
													 else new_position actually_pos prohibited_pos (contador+1)

{-
Funcion recorrer_lista que recibe como parametros una lista de tuplas que contiene un arreglo y un entero, en nuestro caso
la lista con todas las combinaciones y tambien una lista,, la cual compara si alguna esta en eela devuelve 1 caso contarrio 0.
-}					   
recorrer_lista :: [([Int],Int)]-> [Int] -> Int
recorrer_lista [] num = 0
recorrer_lista x num = if fst (head x) == num
                        then 1
                        else recorrer_lista (tail x) num
{-
Funcion contar_num recibe una lista , el numero q se desea buscar y el contador q por defecto se enviara como 0 y retorna el numero de veces q se encuentra
ese numero
-}					   
contar_num :: [Int]-> Int->Int->Int
contar_num [] num contador = contador
contar_num  array num contador = if head array == num
                                  then contar_num(tail array) num (contador+1)
                                  else contar_num (tail array) num (contador)
{-
Funcion score_first_part que recibe la lista del codigo, la lista del codigo de la maquina y una tupla que se envia vacia con la intencion de llenar en el
primer y segundo elemento de la tupla con las listas enviadas anteriormente sin los numeros que coinciden en las mismas posiciones.EJ
score_first_part1 [1,1,1][1,2,6]([],[]) = ([1,1],[6,2])
-}
score_first_part :: [Int]->[Int]->([Int],[Int])->([Int],[Int])
score_first_part  [] [] array_contenedor = array_contenedor
score_first_part array1 array2 array_contenedor = if head array1 == head array2
                                                     then score_first_part (tail array1) (tail array2) array_contenedor
													 else score_first_part (tail array1) (tail array2) (head array1:fst array_contenedor,head array2:snd array_contenedor)
{-
Funcion score_second_part que recibe las listas de los numeros que no coinciden en la misma posicion de la funcion score_first_part y una lista al princio vacia 
num_enLista que contiene los numeros que coinciden en ambas listas la cual ya sabemos que no estan en la posicion correcta porque esas ya las eliminamos y un contador 
que dependiendo se suma 1 0 2 debido a que el maximo arreglo es de 4.
-}
score_second_part :: [Int]->[Int]->[Int]->Int->Int
score_second_part [] array2 num_enLista contador = contador
score_second_part array1 array2 num_enLista contador = if (head array1) `elem` array2 && not((head array1)`elem` num_enLista)
                                                         then if contar_num array1 (head array1) 0 == contar_num array2 (head array1) 0 && contar_num array1 (head array1) 0 >=2
														       then score_second_part (tail array1) array2 (head array1:num_enLista) (contador+2)
															   else score_second_part (tail array1) array2 (head array1:num_enLista) (contador+1)
                                                         else score_second_part (tail array1) array2 num_enLista contador	
{-
Funcion ckeck_random para, de una lista de numeros aleatorios elegir cuantos de estos numeros (2 parametro) quiero elegir sin que se repitan. El 3 parametro
se lo envio como 0 al principio y al final me indica cuantos numeros llevo en mi lista resultado, si este numero coincide con el numero que yo quiero
obtener me devuelve la lista. El cuarto parametro me indica que numeros de la lsita del 1 parametro no quiero obtener y el 5 parametro es la lista resultado q 
al principio se nevia vacia.
-}
ckeck_random::[Int]->Int->Int->[Int]->[Int]->[Int]
ckeck_random random number contador prohibited_number result_random = if number == contador
                                                                       then result_random
																	   else
                                                                        if head random `elem` prohibited_number
                                                                         then ckeck_random (tail random) number contador prohibited_number result_random 
														                 else ckeck_random (tail random) number (contador+1) ((head random):prohibited_number)(head random:result_random)
{-
Funcion score complete que da el resultado completo (haciendo uso de las demas funciones) del code maker score_complete[codigo][codigo a comprobar]
-}
score_complete ::[Int]->[Int]->[Int]
score_complete array1 array2 = [(4-length (fst(score_first_part array1 array2 ([],[])))),score_second_part (fst(score_first_part array1 array2 ([],[]))) (snd(score_first_part array1 array2 ([],[]))) [] 0 ]														 
{-
Funcion eliminar_segunCodigo que coloca como segundo elemnto del arreglo del codigo enviado, el 1 para especificar que ya fue selccionado ese numero
-}
eliminar_segunCodigo ::[([Int],Int)]->[Int]->[([Int],Int)]->[([Int],Int)]
eliminar_segunCodigo [] codigo nuevas_combinaciones = nuevas_combinaciones
eliminar_segunCodigo combinaciones codigo nuevas_combinaciones = if fst(head combinaciones)==codigo && snd(head combinaciones)==0
                                                                  then eliminar_segunCodigo (tail combinaciones) codigo ((fst(head combinaciones),1):nuevas_combinaciones)
																  else eliminar_segunCodigo (tail combinaciones) codigo ((fst(head combinaciones),snd(head combinaciones)):nuevas_combinaciones)
{-
Funcion eliminar_segunElementos que recibe las combinaciones el codigo y la nueva lista de combinaciones que se envia al principio como vacia para
devolverla llena nuevamente modificando (colocando 1) a todas las posibles combinaciones que contengan un numero de la lista
-}
eliminar_segunElementos :: [([Int],Int)]->[Int]->[([Int],Int)]->[([Int],Int)]
eliminar_segunElementos  [] codigo nuevas_combinaciones = nuevas_combinaciones
eliminar_segunElementos combinaciones codigo nuevas_combinaciones =if existe_num_enList (fst(head combinaciones)) (codigo)==0 && snd(head combinaciones)==0 
                                                                     then eliminar_segunElementos (tail combinaciones) codigo ((fst(head combinaciones),0):nuevas_combinaciones)

                                                                     else eliminar_segunElementos (tail combinaciones) codigo ((fst(head combinaciones),1):nuevas_combinaciones)
{-
Funcion existe_num_enList que recibe la lista de las conjeturas posibles y compara si algun numero de esta, se encuentra en la segunda lista que es del codigo seleccionado y devuelve 1 s asi lo es sino devuelve 0 que significa que ningun numero de las listas coinciden
-}
existe_num_enList :: [Int]->[Int]->Int
existe_num_enList [] array2 = 0
existe_num_enList array1 array2 = if head array1 `elem` array2
                                   then 1
								   else existe_num_enList (tail array1) array2
{-
Funcion comparar_existencia_codigo que compara de las combinaciones si el codigo enviado ya fue anteriormente seleccionado
-}
comparar_existencia_codigo ::[([Int],Int)]->[Int]->Bool
comparar_existencia_codigo [] guess_code = False
comparar_existencia_codigo combinaciones guess_code = if fst (head combinaciones)== guess_code && snd (head combinaciones) ==1
                                                         then True
													     else comparar_existencia_codigo (tail combinaciones) guess_code
{-
Funcion selec_potencial_code que recibe el cfg, el score del cfg, el nuevo posible codigo potencial y el score de ese posible codigo potencial y verfica cual es 
el nuevo codigo potencial ya sea el anterior o el ahora adivinado segun el distance to goal.
-}
selec_potencial_code::[Int]->[Int]->[Int]->[Int]->[Int]
selec_potencial_code cfg bestScore guess_code score_ofguess = if get_distanceToGoal (bestScore) > get_distanceToGoal (score_ofguess)
                                                                 then cfg
																 else guess_code
{-
Funcion selec_bestScore que segun el mas alto score dado por el distance to goal supone que es el score del ahora codigo potencial ya que este siempre tendra mayor score
-}
selec_bestScore::[Int]->[Int]->[Int]
selec_bestScore bestScore score_ofguess = if get_distanceToGoal (bestScore) > get_distanceToGoal (score_ofguess)
                                             then bestScore
											 else score_ofguess
{-
Funcion principal_step1 que recibe el codigo potencial, el arreglo con los numeros de las posiciones que queremos mantener en uestro nuevo codigo ejemplo 
[3,4,6,7] [2] 1 []. se envia el iterador al principio como 1 y el arreglo que se decvuelve se lo envia como vacio al principio.El resultado del ejemplo anteriror es
[(4,2)]
-}
principal_step1::[Int]->[Int]->Int->[(Int,Int)]->[(Int,Int)]
principal_step1 [] random_pos iterator  array_result = array_result
principal_step1 cfg random_pos iterator  array_result = if iterator `elem` random_pos
                                                             then principal_step1 (tail cfg) (random_pos) (iterator+1)  (((head cfg),iterator):array_result)
                                                             else principal_step1 (tail cfg) (random_pos) (iterator+1)  (array_result)
{-
Funcion verify_for_step3 que verifica que los numeros enviados por la computadora (seleccionados aleatoriamente) sean diferentes a los ya seleccionados
para nuestro proximo posible codigo potencial.
-}
verify_for_step3 ::[Int]->[(Int,Int)]->Bool
verify_for_step3 array [] = True                                              -- si nunca lo encontro entonces esos numeros son posibles
verify_for_step3 array array2 = if fst(head array2) `elem` array
                                 then False                                 -- si el elemento de nuestro proximo posible cfg ya esta en la lista de mi cfg, entonces esnumero no es permitido
                                 else verify_for_step3 (array) (tail array2)     -- recorro la lista
{-
Funcion principal_step3 que recibe ya los numeros verificados(select_numbers) que iran en las nuevas posiciones dadas por (position_numbers) para unirlo en una nueva funcion.
-}
principal_step3::[Int]->[Int]->Int->[(Int,Int)]->[(Int,Int)]
principal_step3 [](incorrect_position) (iterator) (array_result) = array_result
principal_step3 (select_numbers) (incorrect_position) (iterator) (array_result) = if iterator `elem` incorrect_position
                                                                                     then principal_step3 (select_numbers) (incorrect_position) (iterator+1) (array_result)
                                                                                     else principal_step3 (tail select_numbers) (incorrect_position) (iterator+1) ((head select_numbers,iterator):array_result)

extraer_posiciones :: [(Int,Int)]->[Int]->[Int]
extraer_posiciones [] new_array = new_array
extraer_posiciones array new_array = extraer_posiciones (tail array)(snd (head array):new_array) 
{-
Funcion result_fornew_cfg que recibe el array de ayuda que itera para ordenar el codigo, array2 es el mismo array que nunca de modifica a diferencia del array1 que al iterarlo
cada vez envia en via el array1 sin la cabeza cosa que al encontrar el numero que busca array1 vuelve a ser array2 que nunca se modifica para volverlo a iterar en busca de la
 siguiente posicion 
-}
result_fornew_cfg :: [(Int,Int)]->[(Int,Int)]->[Int]->Int->[Int]
result_fornew_cfg [] array2 result_Array iterator = result_Array 
result_fornew_cfg array1 array2 result_Array iterator = if snd(head array1) == iterator
                                                             then result_fornew_cfg (array2) (array2) (result_Array++[fst(head array1)]) (iterator+1)
                                                             else result_fornew_cfg (tail array1) (array2) (result_Array) (iterator)
for_score_1_3 :: [Int]->[(Int,Int)]
for_score_1_3 [x,y,z] =[(x,z),(y,x),(z,y)]

{-
Funcion para el paso 2 para el score 1 3 que toma del arreglo [x,y,z] y cambia las posiciones [z,x,y] es decir si el arreglo es [1,3,2] devuelve [2,1,3] que quiere decir que el elemento de la posicion 1 lo coloca en la 2 el de la 3 en la 1 y el de la 2 en la 3 
-}
principal_secondpartfor_score_1_3 ::[Int]->Int->[Int]->[(Int,Int)]->[(Int,Int)]
principal_secondpartfor_score_1_3 [] iterator [x,y,z] array_result = array_result
principal_secondpartfor_score_1_3 cfg iterator [x,y,z] array_result = if iterator `elem` [x,y,z]
                                                                         then if iterator == x
                                                                                 then principal_secondpartfor_score_1_3 (tail cfg) (iterator+1) [x,y,z] ((head cfg,z):array_result)
                                                                                 else if iterator == y
                                                                                         then principal_secondpartfor_score_1_3 (tail cfg) (iterator+1) [x,y,z] ((head cfg,x):array_result)
                                                                                         else principal_secondpartfor_score_1_3 (tail cfg) (iterator+1) [x,y,z] ((head cfg,y):array_result)
                                                                         else principal_secondpartfor_score_1_3 (tail cfg) (iterator+1) [x,y,z] (array_result)
{-
Funcion del paso 2 para el secore 0 4 que hace que siempre las posciones vayan a otras posiciones diferentes. Siempre me llegan valores distintos del 1 al 4. Podria ser [2,1,3,4], lo que hace la funcion es tomar un arreglo del tipo [m,n,o,p] y cambian sus posiciones de m->o,n->m
o->p y p->n. Si mi cfg es [5 3 1 2] y mis numeros aleatorios son[4,2,1,3] m->o (el 4 elemento  se coloca en la 1 posicion) n->m (el 2 elemento va a la 4 posicion) o->p (el 1 elemento va a la 3 posicon) y p ->n (el 3 elemento va a la 2 posicion) y quedaria [2,1,5,3]
-}
principal_secondpartfor_score_0_4 ::[Int]->Int->[Int]->[(Int,Int)]->[(Int,Int)]
principal_secondpartfor_score_0_4 [] iterator [m,n,o,p] array_result = array_result
principal_secondpartfor_score_0_4 cfg iterator [m,n,o,p] array_result = if iterator `elem` [m,n,o,p]
                                                                         then if iterator == m
                                                                                 then principal_secondpartfor_score_0_4 (tail cfg) (iterator+1) [m,n,o,p] ((head cfg,o):array_result)
                                                                                 else if iterator == n
                                                                                         then principal_secondpartfor_score_0_4 (tail cfg) (iterator+1) [m,n,o,p] ((head cfg,m):array_result)
                                                                                         else if iterator == o
                                                                                                 then principal_secondpartfor_score_0_4 (tail cfg) (iterator+1) [m,n,o,p] ((head cfg,p):array_result)
                                                                                                 else principal_secondpartfor_score_0_4 (tail cfg) (iterator+1) [m,n,o,p] ((head cfg,n):array_result)
                                                                         else principal_secondpartfor_score_0_4 (tail cfg) (iterator+1) [m,n,o,p] (array_result)
{-
Funcion que de un arreglo por ejemplo [4,2,2,1] devuelve siempre 4 numeros diferentes del 1 al 4. cabe recalcar que el 1 elemento del 1 arreglo solo puede ser del 1 al 4
el 2 elemento del 1 al 3, el 3 elemento del 1 al 2 y el ultimo el 1. Nuestra lista referencial siempre es constante y es [4,2,1,3] y lo que hace es busca el 4 elemento dado por nuestra 1 lista
y lo saca de la lista refrencial (3 )y ahora tenemos [4,2,1], ahora saca el 2 elemento dado por nuestra primera lista de la lista ahora refrencial (2) y nos queda [4,1], ahora
saca el 2 elemento dado por nuestra primera lista de la lisat ahora refencial (1) y ahora es [4] y por ultimo saco el 1 elemento dado por la primera lista (4) quedandome los 
numero aleatorios [3,2,1,4]
-}
num_aleatorios :: [Int]->[Int]->[Int]->[Int]
num_aleatorios [num1,num2,num3,num4] lista_referencial lista_resultado = let 
                                                                             new_num1 = lista_referencial !! (num1-1)   --me saca el numero de la posicion num1 de la lista referencial
                                                                             lista_extraida1 = extraer_num_de_list new_num1 lista_referencial []
                                                                             new_num2 = lista_extraida1 !! (num2-1)    --me saca el numero de la posicion num2 de lista_extraida1
                                                                             lista_extraida2 = extraer_num_de_list new_num2 lista_extraida1 []
                                                                             new_num3 = lista_extraida2 !! (num3-1)
                                                                             lista_extraida3 = extraer_num_de_list new_num3 lista_extraida2 []
                                                                             new_num4 = lista_extraida3 !! (num4-1)
                                                                         in [new_num1,new_num2,new_num3,new_num4]
{-
Funcion que extrae un numero de random_list que es una lista y la variable lista se la envia como vacia para que guarde los numeros sin el numero enviado
-}
extraer_num_de_list:: Int -> [Int]-> [Int]->[Int]
extraer_num_de_list numero [] lista = lista
extraer_num_de_list numero random_list lista = if numero == head random_list
                                                     then extraer_num_de_list (numero) (tail random_list) (lista)
                                                     else extraer_num_de_list (numero) (tail random_list) (lista++[head random_list])
{-
Funcion get_distanceToGoal que me da el numero segun el score
-}
get_distanceToGoal :: [Int]->Int
get_distanceToGoal [0,0] = 0
get_distanceToGoal [0,1] = 1
get_distanceToGoal [1,0] = 2
get_distanceToGoal [0,2] = 3
get_distanceToGoal [1,1] = 4
get_distanceToGoal [2,0] = 5
get_distanceToGoal [0,3] = 6
get_distanceToGoal [1,2] = 7
get_distanceToGoal [2,1] = 8
get_distanceToGoal [3,0] = 9
get_distanceToGoal [0,4] = 10
get_distanceToGoal [1,3] = 11
get_distanceToGoal [2,2] = 12
get_distanceToGoal [4,0] = 13