(* 1 Conversion d'un entier en une chaîne de caractères *)

(* 1.1 Impression des entiers avec Printf.printf *)

(*
	Question 1
	
	# Printf.printf "%o\n" 31415;;
	75267
	- : unit = ()
	# Printf.printf "%x\n" 31415;;
	7ab7
	- : unit = ()
	# Printf.printf "%X\n" 31415;;
	7AB7
	
	La différence entre les formats %x et %X réside dans le fait que %x écrit la valeur en héxadécimal en minuscule et 
	%X écrit les valeur exadécimal en majuscule
*)

(* 1.2 Remarque sur les entiers de Objective Caml *)

(* Question 2 *)

let afficher_puissance_2() = 
	for i = 0 to 40 do
		Printf.printf "%d : %16d \n" i (int_of_float (2.** float_of_int i ))
	done;;
	
(* Question 3 *)

(*
	On observe que pour la puissance 30ème on passe en négatif, les puissances suivante sont égale à 0. Caml ne peux coder
	que 2^31 - 1 valeur, Le bit de signe étant le bit de poid fort, à 2^30, le bit de poids fort est à 1, donc la valeur afficher
	est négative.
*)

(* 1.3 Transformer uin entier en un chiffre *)

(* Question 4 *)

(*
	char_of_int : int -> char
	int_of_char : char -> int

	# char_of_int 0 ;;
	- : char = '\000'
	# char_of_int 1 ;;
	- : char = '\001'
	# char_of_int 2 ;;
	- : char = '\002'
	# char_of_int 3 ;;
	- : char = '\003'
	# char_of_int 4 ;;
	- : char = '\004'
	# char_of_int 5 ;;
	- : char = '\005'
	# char_of_int 6 ;;
	- : char = '\006'
	# char_of_int 7 ;;
	- : char = '\007'
	# char_of_int 8 ;;
	- : char = '\b'
	# char_of_int 9 ;;
	- : char = '\t'
	
	# int_of_char '0';;
	- : int = 48
	# int_of_char '1';;
	- : int = 49
	# int_of_char '2';;
	- : int = 50
	# int_of_char '3';;
	- : int = 51
	# int_of_char '4';;
	- : int = 52
	# int_of_char '5';;
	- : int = 53
	# int_of_char '6';;
	- : int = 54
	# int_of_char '7';;
	- : int = 55
	# int_of_char '8';;
	- : int = 56
	# int_of_char '9';;
	- : int = 57
	# int_of_char 'A';;
	- : int = 65
	# int_of_char 'B';;
	- : int = 66
	# int_of_char 'C';;
	- : int = 67
	# int_of_char 'D';;
	- : int = 68
	# int_of_char 'E';;
	- : int = 69
	# int_of_char 'F';;
	- : int = 70
*)

(* Question 5 *)

let question5 n = 
		char_of_int (int_of_char '0' + n);;
		
(*
	Entre 0 et 9, char_of_int (int_of_char '0' + n) renvoie le caractère entré par n (par exemple si n= 5, la fonction nous 
	renvoie le caractère 5. Pour n >= 10, on obtients les lettres de l'alphabet ainsi que la ponctuation.
*)

(* Question 6 *)

let question6 n =
	char_of_int (int_of_char 'A' - 10 + n);;
	
(* La fonction est char_of_int (int_of_char 'A' - 10 + n) *)

(* Question 7 *)

let entier_en_chiffre n =
	let c = ref '0' in
	if n < 0 || n > 15 then
		failwith "entier_en_chiffre : entier negatif ou trop grand"
	else
	begin
		if n < 10 then
			c := question5 n
		else
			c := question6 n
	end;
	!c;;
	
(* 1.4 Convertir un entier en une chaîne de caractères *)

(* Question 8 *)

let ajoute_caractere_en_tete s c = 
	let s' = String.make 1 c in 
	s'^s;;
	
let entier_en_chaine n b =
	let s = ref "" in
	if b > 16 then
		failwith "entier_en_chaine base incorrecte"
	else
	begin
		let quotient = ref n;
		and reste = ref 0 in
		if n = 0 then
			s := "0";
		while !quotient > 0 do
			reste := !quotient mod b;
			quotient := !quotient / b;
			s := ajoute_caractere_en_tete !s (entier_en_chiffre !reste);
		done;
		
	end;
	!s;;
	
(* Question 9 *)

let imprimer_table() =
	for i = 0 to 20 do
		Printf.printf "%s : %5s %s %s \n" (entier_en_chaine i 10) (entier_en_chaine i 2) (entier_en_chaine i 8) (entier_en_chaine i 16)
	done;;
	
(* 2 Conversion d'une chaine de caractère en un entier *)

(* 2.1 Entrée des entiers en hexa ou en octal *)

(* Question 10 *)
(*
	# 0x314;;
	- : int = 788
	# 0o314;;
	- : int = 204
*)

(* 2.2 Convertir un chiffre en entier *)

(* Question 11 *)

let chiffre_en_entier c = 
	if (c < '0' || c > '9') && (c < 'A' || c > 'F') then
		failwith "chiffre_en_entier chiffre incorrect"
	else if c >= '0' && c <= '9' then
		(int_of_char c) - (int_of_char '0')
	else
		(int_of_char c) - (int_of_char 'A') + 10;;

(* 2.3 Convertir une chaîne en entier *)

(* Question 12 *)

let chaine_en_entier s b =
	let n = ref 0 
	and l = (String.length s) in
	for i = 0 to l - 1 do
		if (chiffre_en_entier s.[i]) > b - 1 then
			failwith "chaine_en_entier base incorrecte"
		else
		begin
			n := !n + (chiffre_en_entier s.[i] * int_of_float ((float_of_int b) ** (float_of_int (l - 1 - i))))
		end;
	done;
	!n;;
	
(* Question 13 *)

(* 	NIP : 11402588
	# chaine_en_entier "11402588" 16;;
	- : int = 289416584
*)

(* 3 Opérations logiques sur les entiers *)

(* 3.1 Les opérateurs logiques sur les entiers en Caml *)

(* Question 15 *)
(*
	n lsl 1 = n * 2
	n lsr 2 = n / 2
*)

(* Question 16 *)
let imprimer_logique () =
	for i = 0 to 40 do
		Printf.printf "%d : %16d \n" i (1 lsl i)
	done;;

(* Question 17 *)
(*
	(n lsr 1) lsl 1 = n ?
	Si oui alors n est paire
	sinon n est impaire
*)

(* 3.2 Retour sur la conversion en base 2 *)

(* Question 18 *)
 


let entier_en_chaine_binaire n =
	let s = ref ""
	and dec = ref n in
	while !dec <> 0 do
		s := ajoute_caractere_en_tete !s (string_of_int ((!dec land (1 lsl 0)) lsr 0)).[0];
		dec := !dec lsr 1;
		(* Printf.printf "%d\n" !dec *)
	done;
	!s;;

(* Question 19 *)

(* 
   L'écriture binaire d'un entier positif s'écrit avec juste le nombre de bit nécessaire.
   Par contre pour l'écriture binaire de l'entier négatif, par exemple 5, on obtient "111111111111111111111111111111111111111111111111111111111111011"
   On obtient ce très grand nombre binaire car on ne sait pas en combien de bits on code -5, du coup, on donne le plus grand nombre de bits possible pour écrire ce chiffre
*)

(* Question 20 *)

let chaine_binaire_en_entier s =
	let n = String.length s - 1;
	and dec = ref 0 in	
	for i = 0 to n do
		if s.[i] <> '0' && s.[i] <> '1' then
			failwith "chaine_binaire_en_entier : symbole non binaire"
		else if s.[i] = '1' then
			dec := (!dec lor (1 lsl (n - i)))
	done;
	!dec;;

(* 4 Représentation des flottants *)

(* Question 21 *)

(*
	32 bits :
		Hexadecimal	: 40490FD8
		s		: 0
		e		: 10000000
		m		: 1 .10010010000111111011000

	64 bits :
		Hexadecimal	: 400921FAFC8B007A
		s		: 0
		e		: 10000000000
		m		: 1 .1001001000011111101011111100100010110000000001111010
*)

(* Question 22 *)

(*
	0F1E2D3C = 7.798713262790306e-30 dans la norme IEEE-754
*)

(* Question 23 *)

(*
	Sur 32 bits le nombre positif le plus petit est : 1,4×10^-45 (0 00000000 00000000000000000000001)
	sur 64 bits le nombre positif le plus petit est : 4,9406564584124654×10^−324
*)

(* Question 24 *)

(*
	Sur 32 bits le nombre positif le plus petit est : 3,40282346×10^38
	sur 64 bits le nombre positif le plus petit est : 1,7976931348623157×10^308
*)
