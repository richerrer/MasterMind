import System.Random
generar_combinaciones :: [([Int],Int)]
generar_combinaciones = let
                         numeros =[1..6]
                         combinaciones = [([w,x,y,z],0)|w<-numeros,x<-numeros,y<-numeros,z<-numeros]
					    in combinaciones
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