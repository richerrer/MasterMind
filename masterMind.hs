{-
Funcion generar_combinaciones que recibe como parametro una lista de tuplas que contiene un arreglo y un entero y,
me genera todas las posibles combinaciones de 4 digitos del 1 al 6.
-}
generar_combinaciones :: [([Int],Int)]
generar_combinaciones = let
                         numeros =[1..6]
                         combinaciones = [([w,x,y,z],0)|w<-numeros,x<-numeros,y<-numeros,z<-numeros]
					   in combinaciones
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
score_complete ::[Int]->[Int]->[Int]
score_complete array1 array2 = [(4-length (fst(score_first_part array1 array2 ([],[])))),score_second_part (fst(score_first_part array1 array2 ([],[]))) (snd(score_first_part array1 array2 ([],[]))) [] 0 ]														 

{-
Funcion leer_archivo obtiene el codigo que esta escrito en el archivo de texto code.txt y lo retorna como un String.
-}
leer_archivo :: IO String
leer_archivo = readFile "code.txt"

main::IO()
main=do
	 let
	    x = generar_combinaciones
	    m = [1..5]
	    y = recorrer_lista x m
         putStrLn $ "Listo "++show (y)