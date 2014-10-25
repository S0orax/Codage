open Codage;;

(*
let alphabet_source = [|'a'; 'b'; 'c'|]
let code1 = [|"010"; "100"; "110"|]
let codage = creer alphabet_source code1
*)

(* Question 2 *)

(*
	# code 'a' codage;;
	- : string = "010"
	# code 'b' codage;;
	- : string = "100"
	# code 'c' codage;;
	- : string = "110"
*)

(* Question 3 *)

(*
	# decode "010" codage;;
	- : char = 'a'
	# decode "100" codage;;
	- : char = 'b'
	# decode "110" codage;;
	- : char = 'c'
*)

(* Question 4 *)

(*
	# code 'd' codage;;    
	Exception: Codage.Symbole_non_codable.
	
	Une exception est levee nous indiquant que le symbole de l'alphabet n'a pas
	d'equivalent dans le code.
*)

(* Question 5 *)

(*
	# decode "111" codage;;
	Exception: Codage.Mot_non_decodable.
	
	Une exception est aussi levee, cele-ci nous donne l'information que le mot
	ne peu pas etre decode
*)

let alphabet_source = [| 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 
					'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'; ' ' |]
					
let code1 = [| "00000"; "00001"; "00010"; "00011"; "00100"; "00101"; "00110"; 
			"00111"; "01000"; "01001"; "01010"; "01011"; "01100"; "01101"; 
			"01110"; "01111"; "10000"; "10001"; "10010"; "10011"; "10100"; 
			"10101"; "10110"; "10111"; "11000"; "11001"; "11111" |]
			
let code2 = [| ".-/"; "-.../"; "-.-./"; "-../"; "./"; "..-./"; "--./"; "..../"; "../"; ".---/"; "-.-/"; ".-../"; 
			"--/"; "-./"; "---/"; ".--./"; "--.-/"; ".-./"; ".../"; "-/"; "..-/"; "...-/"; ".--/"; "-..-/"; 
			"-.--/"; "--../"; "---./"|]
			
let code3 = [| "1010"; "0010011"; "01001"; "01110"; "110"; "0111100"; 
			"0111110"; "0010010"; "1000"; "011111110"; "011111111001"; 
			"0001"; "00101"; "1001"; "0000"; "01000"; "0111101"; "0101"; 
			"1011"; "0110"; "0011"; "001000"; "011111111000"; "01111110"; 
			"0111111111"; "01111111101"; "111" |]
			
(* Question 6 *)

let codage1 = creer alphabet_source code1
let codage2 = creer alphabet_source code2
let codage3 = creer alphabet_source code3

(* Question 7 *)

(*
	# code 'A' codage1;;
	- : string = "00000"
	# code 'K' codage1;;
	- : string = "01010"
	# code 'A' codage2;;
	- : string = ".-/"
	# code 'K' codage2;;
	- : string = "-.-/"
	# code 'A' codage3;;
	- : string = "1010"
	# code 'K' codage3;;
	- : string = "011111111001"
	
	# decode "00000" codage1;;
	- : char = 'A'
	# decode "01010" codage1;;
	- : char = 'K'
	# decode ".-/" codage2;;  
	- : char = 'A'
	# decode "-.-/" codage2;;
	- : char = 'K'
	# decode "1010" codage3;;
	- : char = 'A'
	# decode "011111111001" codage3;; 
	- : char = 'K'
*)

(* Question 8 *)

let rec code_mot_aux s c_code s_aux =
	let s_result = ref s_aux
	and n = String.length s in
	if n = 0 then
		!s_result
	else
	begin
		s_result := !s_result ^ code s.[0] c_code;
		code_mot_aux (String.sub s 1 (n - 1)) c_code !s_result
	end;;
	
let code_mot s c_code =
	try
		code_mot_aux s c_code ""
	with
	| Codage.Symbole_non_codable -> ""
	
(*
	On doit trouver : 	000100111000011000000011000100
	On trouve : 		000100111000011000000011000100
	
	# code_mot "CODAGE" codage3;;
	- : string = "0100100000111010100111110110"
	# code_mot "CODAGE" codage2;;
	- : string = "-.-./---/-../.-/--././"
*)

(* Question 10 *)

(*
	# String.length (code 'A' codage1);;  
	- : int = 5
*)

(* Question 11 *)

let decode_mot_longueur_fixe s codage =
	let nCodage = String.length (code 'A' codage)
	and nS = String.length s in
	let s_result = String.create (nS / nCodage) in
	
	if nS mod 5 <> 0 then
		failwith "decode_mot_longueur_fixe : mot non decodable"
	else
	begin
	try
		for i = 0 to nS / nCodage - 1 do
			let indice = i * nCodage in
			let subString = String.sub s indice nCodage in
			let c = decode subString codage in
			s_result.[i] <- c
		done
	with
	| Mot_non_decodable -> failwith "decode_mot_longueur_fixe : mot non decodable"
	end;
	s_result

(*
	# let codage = code_mot "CODAGE" codage1;; 
	val codage : string = "000100111000011000000011000100"
	# decode_mot_longueur_fixe codage codage1;;
	- : string = "CODAGE"

*)

(* Question 13 *)

(*
	# decode_mot_longueur_fixe "01111100010111000101001001001010\
	01000100101001000111111001010000\
	01010010011111110100001011111110\
	00101010001000100010010011111010\
	11001001111101111100010111000011\
	10100010001001111111000110010011\
	11110101011101001011111001010111\
	0101000100001011010110010010010" codage1;;
	- : string = "PROFESSEUR FAUT IL CUIRE LE PRODUIT DE VOS FOUILLES"
*)

(* Question 14 *)

(*
	# String.index_from "CODAGE" 0 'C';;
	- : int = 0
	# String.index_from "CODAGE" 0 'E';;
	- : int = 5
	# String.index_from "CODAGE" 0 'Y';;
	Exception: Not_found.
	# String.index_from "CODAGE" 3 'O';;
	Exception: Not_found.
	# String.index_from "ANTICONSTITUTIONNELLEMENT" 0 'E';;
	- : int = 17
	# String.index_from "00000001" 0 '1';;                 
	- : int = 7
	# String.index_from "00000001" 8 '0';;
	Exception: Not_found.
*)

(* Question 15 *)

let rec decode_mot_virgule_aux s codage c s_aux =
	let nS = String.length s in
	if nS = 0 then
		s_aux
	else
	begin
		try
			let posVirgule = String.index_from s 0 c in
			let prefixe_s = String.sub s 0 (posVirgule + 1)
			and sufixe_s = String.sub s (posVirgule + 1) (nS - posVirgule - 1) in
			let c_decode = decode prefixe_s codage in
			let s_char = " " in
			s_char.[0] <- c_decode;
			let ss = s_aux ^ s_char in
			decode_mot_virgule_aux sufixe_s codage c ss
		with
		| Mot_non_decodable -> failwith "decode_mot_virgule : mot non decodable"
		| Not_found -> failwith "decode_mot_virgule : mot non decodable"
	end

let decode_mot_virgule s codage c =
	decode_mot_virgule_aux s codage c ""
	
(* Question 16 *)

(*
	# decode_mot_virgule ".--./---/..-/.-./---./.-../.-/--\
	-./  ..-./.-./.-/-./-.-././---./-.\
	./  ---././-./---./-.../.-/.../---\
	./  -.././.../---./-./---/..-/../.\
	-.  ./.-.././.../---././-./-.-./--\
	-  /.-././" codage2 '/';;
	- : string = "POUR LA FRANCE D EN BAS DES NOUILLES ENCORE"
*)

(* Question 17 *)

(*
	let rec trouve_prefixe n =
		let s = code_mot "CODAGE" codage3 in
		try
			if (n + 1) > (String.length s) then
				failwith "trouve_prefixe : aucun prefixe decodable"
			let prefixe = String.sub s 0 (n + 1) in
			for i = 0 to (n + 1) do
				decode prefixe codage3
			done;
			prefixe
		with
		| Mot_non_decodable -> trouve_prefixe (n + 1)

	# trouve_prefixe 0;;
	- : string = "01001"
*)

(* Question 18 *)

let decode_lettre_prefixe v codage = 
	let n = String.length v 
	and i = ref 0 
	and prefixe = ref '\000' 
	and trouve = ref false in 
	while (!i <= n) && not (!trouve) do 
		try 
			prefixe := decode (String.sub v 0 !i) codage ; 
			trouve := true 
		with 
			| Mot_non_decodable -> i := !i + 1 
	done ; 
	if !i <= n then 
		(!prefixe,!i) 
	else 
		failwith "decode_lettre_prefixe : mot non decodable"
		
(*
	# let prefixe = decode_lettre_prefixe (code_mot "CODAGE" codage3) codage3;;
	val prefixe : char * int = ('C', 5)
	# String.sub (code_mot "CODAGE" codage3) 0 (snd prefixe);;
	- : string = "01001"
*)

(* Question 19 *)

let rec decode_mot_prefixe_aux s codage s_aux =
	let nS = String.length s in
	if nS = 0 then
		s_aux
	else
	begin
		let prefixe = decode_lettre_prefixe s codage in
		let nbrLettre = snd prefixe in
		let subString = String.sub s 0 nbrLettre
		and sufixe = String.sub s nbrLettre (nS - nbrLettre) in
		let s_char = " " in
		s_char.[0] <- (fst prefixe);
		let ss = s_aux ^ s_char in
		decode_mot_prefixe_aux sufixe codage ss
	end

let decode_mot_prefixe s codage =
	decode_mot_prefixe_aux s codage ""
	
(*
	# decode_mot_prefixe "01100010010101000011101011111110\
	10110110111011000000011011111110\
	00000011010110111111010111011110\
	0101010000101110" codage3;;
	- : string = "THALES EST TOUJOURS A FAIRE"
*)