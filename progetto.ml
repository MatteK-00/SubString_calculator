(* Inizializza il generatore per il random *)
Random.self_init();; 

(* Genera un numero random compreso tra 1 e k *)
let randomNumber k =
	Random.int ( k ) + 1 
;;

(*
let assegnaStringa = function
	| 0 -> "a"
	| 1 -> "b"
	| 2 -> "c"
	| 3 -> "d"
	| 4 -> "e"
	| 5 -> "f"
	| 6 -> "g"
	| 7 -> "h"
	| 8 -> "i"
	| 9 -> "l"
	| 10 -> "m"
	| 11 -> "n"
	| _ -> "\n";;
	
let rec costruisciStringa s lunghezza =
		if not(lunghezza = 0)	
			then 
				costruisciStringa (s^assegnaStringa(randomNumber 11)) (lunghezza -1)
		else
			s;;
*)	


(*Definisce una funzione che preso in input un numero da 1 a 4 restituisce una stringa composta da un solo carattere*)
let assegnaStringa = function
	| 0 -> "a"
	| 1 -> "b"
	| 2 -> "c"
	| 3 -> "d"
	| _ -> "\n";;

	
(*Dato un intero "lunghezza" la funzione concatena ricorsivamente ad una stringa vuota un numero pari a "lunghezza" di caratteri
 tramite la funzione assegnaStringa*)
let rec costruisciStringa s lunghezza =
		if not(lunghezza = 0)	
			then 
				costruisciStringa (s^assegnaStringa(randomNumber 3)) (lunghezza -1)
		else
			s;;
		
		
(*Dato in input un intero n la funzione restituisce n stringhe di dimenzione massima pari a "15" generate in maniera casuale grazie
 alla funzione costruisciStringha*)			
let creaStringhe n = 
	let rec auxCreaStringhe (n, lista) =
		if not(n = 0) then
			auxCreaStringhe ((n-1),(costruisciStringa "" ((randomNumber 25)+20))::lista)
		else
			lista
			
		in auxCreaStringhe (n, []);;		


(*Data una stringa "parola" restituisce una lista di stringhe contenenti tutte le sottostringhe di "parola" in ordine
 decrescente rispetto alla loro lunghezza*)
 let sottoParoleTutte parola =
	let length = String.length parola 
		in let rec aux (start,dimensione) =
			if (dimensione=0) then
				[]
			else 
				if ((start+dimensione)<=length) then
					(String.sub parola start dimensione)::(aux(start+1, dimensione))
				else
					aux(0,dimensione-1)
		in aux (0,length);;

		
(*Stampa a video tutte le stringhe contenute in una lista separate da spazi*)		
let rec print_list = function 
	[] -> ()
	| x::rest -> print_string x; print_string " " ; print_list rest;;



	
(*Restituisce true se il predicato p restituisce true per tutte le liste a cui è applicata la funzione, false altrimenti.*)	
let rec for_all p = function
	[] -> true
	| x::xs -> p x && for_all p xs;; 

	
(*cerca se almeno un elemento della lista soddisfa il predicato booleano p. Se p x restituisce il valore true si ferma
 altrimenti continua la ricerca sulla lista rimanente*)	
let rec exists p = function
	[] -> false
	| x::xs -> p x || exists p xs;; 
	
(*predicato di confronto che dato un elemento "e" in input restituisce la funzione che confronta quell'elemento con gli eventuali
  altri input e restituisce true se sono uguali e false atrimenti*)	
let equal = (=);;

(*dato un elemento "v" e una lista di liste "lst" restituisce true se l'elemento "v" compare almeno una volta in tutte le sottoliste
  di "lst", false altrimenti*)
let check v lst = for_all (exists (equal v)) lst;; 


(*confrontaAuxAsc è la funzione di confronto utilizzata dalla funzione List.sort per ordinare in ordine decrescente gli
  elementi di una lista di stringhe data in input sulla base della lunghezza delle varie stringhe. La funzione prese due
  stringhe consecutive all'interno della lista restituisce 0 se sono uguali, 1 se la prima è maggiore della seconda e -1
  altrimenti.*)
let confrontaAuxAsc stringa1 stringa2=
	if (String.length stringa1) = (String.length stringa2)
		then 0
	else if (String.length stringa1) < (String.length stringa2)
		then -1
	else 1;;


(*definisce il tipo di dato albero "tree"*)
type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree ;;		

(*scorre la lista in input decrementando un contatore n ad ogni ricorsione, quando il contatore è uguale a 
 zero restituisce la lista rimanente a quel punto della ricorsione e quindi la lista data in input privata dei
 primi n elementi*) 
let rec drop n = function
	[] -> []
	| x::xs -> if n<=0 then x::xs
			else drop (n-1) xs ;; 

(*La funzione makeTree, a partire da una lista di stringhe, formata da tutte le sottostringhe ordinate per lunghezza di una stringa,
restituisce un particolare albero. L'albero in questione rappresenta, in modo logico, un particolare albero binario completo in cui però
vari fratelli hanno rispettivamente il figlio destro, in comune con il fratello destro se esiste e il figlio sinistro in comune con il 
fratello sinistro se esiste. Nella pratica al fine di rappresentare questa costruzione, senza però generare calcoli ridondanti, la funzione
makeTree costruisce un albero sbilanciato nel quale la maggior parte dei nodi ha solo un figlio sinistro, mentre il nodo più a destra
è l'unico ad avere anche un figlio destro che garantisce il livello di ramificazione necessario a rappresentare in modo ordinato le sottostringhe. *)
let makeTree listaParole =
	let rec aux (sottoLista, h, right) = 
		if (sottoLista=[]) then
			Empty
		else if (right) then
				Tr((List.hd sottoLista), aux((drop h sottoLista),(h+1),false),aux((drop (h+1) sottoLista),(h+1),true))
		else 
			Tr((List.hd sottoLista), aux((drop h sottoLista),(h+1),false), Empty)
	in aux (listaParole,1,true);;

(*Stampa in modo leggibile l'albero in modo tale da aiutare ad individuarne la struttura*)			
let treeprintstring t =
	let rec aux = function (ind,Empty) -> print_string
		(ind ^ "Empty")
		| (ind,Tr(x,Empty,Empty)) -> print_string (ind^"Tr(" ^ x ^ ",Empty,Empty)")
		| (ind,Tr(x,t1,t2)) -> (print_string(ind^"Tr(" ^  x ^ ",\n");
		aux(" "^ind,t1); print_newline();
		aux(" "^ind,t2); print_string ")")
	in aux("",t); print_newline();;	

(*presi in input due liste (di risultati parziali) una stringa X e una lista di liste (contenenti tutte le possibili sottostringhe delle parole in esame)
la funzione controlla se X è una soluzione parziale ammissibile (tramite la funzione check), in caso affermativo restituisce X, altrimenti controlla che
le liste dei risultati parziali non siano vuote e in quel caso restituisce i risultati parziali di lunghezza maggiore.*)	
let confronta (lis1, lis2, x, lista) = 
	if (check x lista) then
		[x] 
	else if (lis1=[]) then
		lis2
	else if (lis2=[]) then
		lis1
	else if (String.length (List.hd lis1)) > (String.length (List.hd lis2)) then
		lis1
	else if (String.length (List.hd lis1)) < (String.length (List.hd lis2)) then
		lis2 
	else 
		(lis1)@(lis2);;

		
(*Preso in input un albero t generato a partire dalle sottostringhe di una parola e una lista di liste (contenenti
 ognuna tutte le sottostringhe di una differente parola) la funzione visita l'albero scendendo fino alle foglie o, se k>1,
 fino all'altezza k dell'albero. A questo punto per ogni nodo ragginto dalla fase di discesa viene ricercata una possibile
 soluzione parziale, se il nodo corrente rapppresenta una sottostringa valida (risultato ottenuto grazie alla funzione check)
 allora il nodo restituisce se stesso come soluzione parziale, se il nodo non è una soluzione parziale restituisce la lista
 delle soluzioni parziali (opportunamente filtrate) dei suoi figli.*)		
let treeSearch t lista k =
	let rec auxSearch t = match t with
		Empty -> []
		| Tr(x,t1,t2) ->
			if ((String.length x) < k) then
				[]
			else 
				confronta((auxSearch t1),(auxSearch t2), x, lista)
	in auxSearch t;;



(*La funzione confrontaStringhe prende in input una lista di stringhe e un intero k, dopodiché ordina la lista di stringhe sulla base della
  lunghezza delle stringhe e esegue la ricerca sull'albero costruito opportunamente, utilizzando la funzione makeTree, con le sottostringhe 
  della parola più corta tramite la funzione treeSearch.*)
let confrontaStringhe listaStringhe k =
	let  listaStringheOrd = 
		List.sort confrontaAuxAsc listaStringhe
	in let risultato = treeSearch(makeTree (sottoParoleTutte (List.hd listaStringheOrd))) (List.map sottoParoleTutte (List.tl listaStringheOrd)) k;
	in	(*treeprintstring (makeTree (sottoParoleTutte (List.hd listaStringheOrd)));;*)
		print_string "\n";
		print_list listaStringheOrd;
		print_string "\n";
		print_string "La sottostringa di dimensione minore e': ";
		print_list (risultato);;


		

	
	
(* funzioni ausiliarie di controllo*)
(*
(*come check ma restituisce una lista contenente v se vero e una lista vuota altrimenti*)
let check2 v lst = 
	if (for_all (exists (equal v)) lst) then [v]
	else [];;
	
(*visita l'albero senza scartare le soluzioni parziali, restituisce tutte le sottostringhe fino ad altezza k*)	
let treeSearch2 t lista k =
	let rec auxSearch t = match t with
		Empty -> []
		| Tr(x,t1,t2) ->
			if ((String.length x) < k) then
				[]
			else 
				(auxSearch t1) @ ((auxSearch t2) @ (check2 x lista))
	in auxSearch t;;

(*genera l'albero binario COMPLETO contenente i figli ridondanti*)
let makeTree listaParole =
	let rec aux (sottoLista, h) = 
		if (sottoLista=[]) then
			Empty
		else 
			Tr((List.hd sottoLista), aux((drop h sottoLista),(h+1)),aux((drop (h+1) sottoLista),(h+1)))
	in aux (listaParole,1);;
	
	
*)	
	

