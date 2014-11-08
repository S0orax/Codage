(*
	Dubois Yann
	TP6
*)

open Codage;;
open Un_codage;;

(* Question 1 *)

(*
	Les fiichiers ont bien Ã©tÃ© rÃ©cupÃ©rÃ© et les modules ont Ã©tÃ© compilÃ©
*)

(* Question 2 *)

(*
	# open Un_codage;;
	# un_codage;;
	- : int Codage.codage = <abstr>
	# open Codage;;
	# code 1 un_codage;;
	- : string = "01000011011010010101"
	# code (int_of_char 'e') un_codage;;
	- : string = "101"
	
	donc le code associÃ© a l'octet 1 est "01000011011010010101"
	et le code associÃ© Ã  e en ASCII est "101"
*)

(* Question 3 *)

(*
	fct_codage : int -> string
	- fonction d'initialisation pour creer un tableau de chaine de caractere
	parametres : 
		i : octets dont on cherche le code
	renvoie : le code correspondant à i
*)
let fct_codage i =
	code i un_codage

let un_code = Array.init 256 fct_codage;;

(*
	val un_code : string array =
	  [|"01000011011011001001"; "01000011011010010101"; "01000010000111010001";
	    "01000011011011010100"; "01000011011011001111"; "01000011011010010100";
	    "01000010000111110110"; "01000011011011001000"; "01000010000111110111";
	    "01000010000111010000"; "01000011011010110101"; "01000011011010110100";
	    "01000011011011010101"; "01000011011010111011"; "01000011011010111010";
	    "01000011011010101010"; "01000011011011001110"; "01000011011010101011";
	    "01000011011010110001"; "01000011011010110000"; "01000011011011010000";
	    "01000011011010101101"; "01000011011010101100"; "01000011011011010010";
	    "01000011011010110111"; "01000011011010100001"; "01000011011010001001";
	    "01000011011010101110"; "010000100001001000001"; "010000100001001000111";
	    "010000100001001000110"; "01000011011010001000"; "000"; "0010100100";
	    "010000100001010"; "01000011011010100010"; "01000011011011010001";
	    "01000011011010101111"; "01000011011010100000"; "0010101";
	    "010000100001011010"; "01000010000110100"; "01000010000111010010";
	    "01000010000111010011"; "001100"; "10010011"; "0100000";
	    "01000011011011011"; "01000010000111000"; "0100001000011000";
	    "010000100001011101"; "0100001000011101010"; "010000100001011000";
	    "010000100001101010"; "010000100001001001"; "0100001000010111001";
	    "01000010000110010"; "01000010000101111"; "01000010011"; "1001010010";
	    "01000010000100111"; "010000100001011001"; "01000010000100101";
	    "00101000111"; "0100001101101101011"; "00101000001"; "001010001010";
	    "0010100001"; "0100001110"; "100100100"; "0100001101111";
	    "0100001101100"; "010000110100"; "0100001100"; "01000011111";
	    "0100001000010110111"; "0010100101"; "100101100"; "010000100010";
	    "10010100110"; "00101000110"; "010000110101"; "010000100101";
	    "01000011110"; "10010100111"; "010000100100"; "10010110110";
	    "010000100001001000000"; "010000100001101011"; "010000100001110111";
	    "0100001101110"; "01000010000111011001"; "01000011011010110110";
	    "01000011011011010011"; "01000011011010100011"; "0100001000010111000";
	    "0100001000011100110"; "0101"; "0100111"; "001011"; "10011"; "101";
	    "0100100"; "0100101"; "0100110"; "0111"; "010000101"; "010000100001000";
	    "00100"; "001101"; "1100"; "00111"; "010001"; "1001000"; "1101"; "0110";
	    "1000"; "1111"; "111001"; "01000010000110011"; "10010111"; "100101000";
	    "1001011010"; "01000011011010011100"; "01000011011010011101";
	    "01000010000111111010"; "01000011011010011111"; "01000011011010000000";
	    "01000011011010000001"; "01000011011010011110"; "01000010000111101011";
	    "01000010000111101000"; "01000010000111101001"; "01000010000111101010";
	    "01000010000111111011"; "01000010000111111000"; "01000010000111111001";
	    "01000011011010000011"; "01000010000110110101"; "01000011011011000010";
	    "01000011011011000011"; "01000010000110110100"; "01000010000110110001";
	    "01000010000110110110"; "01000010000110110111"; "01000011011011000100";
	    "01000011011011000101"; "01000011011010000010"; "01000011011011000111";
	    "01000011011011000000"; "01000011011011000001"; "01000011011011000110";
	    "01000011011010000110"; "01000010000111100110"; "01000010000111100111";
	    "01000010000111100100"; "0100001000010011010"; "01000010000111100001";
	    "01000010000111100010"; "01000010000111100011"; "10010101";
	    "01000010000111100000"; "00101000100"; "01000010000111001001";
	    "01000010000111001110"; "01000010000111001111"; "01000010000111001000";
	    "00101000000"; "001010011"; "111000"; "100100101"; "010000100001001100";
	    "01000010000111100101"; "01000010000111001010"; "010000100011";
	    "010000110110111"; "01000010000111001011"; "01000010000111101101";
	    "01000011011010001111"; "01000011011010001100"; "010000100000";
	    "01000011011010001101"; "01000011011010001110"; "01000011011010000111";
	    "01000011011010000100"; "001010001011"; "01000011011010000101";
	    "10010110111"; "01000010000111101110"; "01000010000111101111";
	    "01000010000111101100"; "01000010000111111101"; "01000010000111111110";
	    "01000010000111111111"; "0100001000010110110"; "11101";
	    "01000010000111111100"; "01000011011010100111"; "01000011011010101001";
	    "01000011011010101000"; "01000010000111110101"; "01000010000111010110";
	    "01000011011010111111"; "01000011011010100110"; "01000011011010100101";
	    "01000011011010111000"; "010000100001001000011"; "01000010000111110100";
	    "01000011011010010001"; "01000011011010010000"; "01000011011010100100";
	    "01000011011010111110"; "01000010000111011010"; "01000010000111011000";
	    "01000010000111011011"; "01000011011010110010"; "01000011011011001011";
	    "01000011011011001010"; "01000011011010110011"; "01000011011010001010";
	    "01000011011010010111"; "01000011011010010110"; "01000011011010001011";
	    "01000011011011001100"; "01000010000111010111"; "01000011011011001101";
	    "01000011011010111001"; "01000010000110111011"; "01000010000110111000";
	    "01000010000110111001"; "01000010000110111010"; "01000010000111110011";
	    "01000010000111110000"; "01000010000111110001"; "01000010000110110010";
	    "01000010000110110011"; "01000010000110110000"; "01000011011010011001";
	    "01000011011010011010"; "01000011011010011011"; "01000011011010011000";
	    "01000010000111110010"; "01000011011010111100"; "010000100001001000010";
	    "01000011011010111101"; "010000100001001000100"; "01000011011010010011";
	    "01000011011010010010"; "010000100001001000101"; "01000010000110111111";
	    "01000010000110111100"; "01000010000110111101"; "01000010000110111110";
	    "01000010000100110111"; "010000100001001101100";
	    "010000100001001101101"|]
*)

(* Question 4 *)

(*
	min_max : string array -> int * int
	- fonction qui cherche le plus grand mot et le plus petit mot d'un
	tableau de chaine de caractere
	parametres :
		tab : tableau de chaine de caractere dont on veut recuperer la
			taille maximal et minimal entre toute ces chaines
	renvoie :
		un couple avec la plus paetite longueur et la plus grande
*)
let min_max tab =
	let min = ref (String.length tab.(0))
	and max = ref (String.length tab.(0))
	and n = Array.length tab in
	
	for i = 1 to n - 1 do
		let current_length = String.length tab.(i) in
		if current_length > !max then
			max := current_length
		else if current_length < !min then
			min := current_length;
	done;
	(!min, !max)
	
(*
	# min_max un_code;;
	- : int ref * int ref = ({contents = 3}, {contents = 21})
*)

(* Question 5 *)

(*
	est_prefixe : string array -> bool
	predicat qui determine si un code est prefixe ou non
	parametres : 
		tab : tableau lié au code, ici un_codage
*)
let est_prefixe tab =
	let n = Array.length tab
	and i = ref 0
	and prefixe = ref true in
	while !i < n && !prefixe do
		let mot_length = (String.length tab.(!i))
		and j = ref (!i + 1) in
		while !j < n && !prefixe do
			let current_length = (String.length tab.(!j)) in
			if current_length < mot_length then
				incr j
			else
			begin
				let sub_string = String.sub tab.(!j) 0 mot_length in
				if tab.(!i) <> sub_string then
					incr j
				else
					prefixe := false
			end;
		done;
		incr i
	done;
	!prefixe

(*
	# est_prefixe un_code;;
	- : bool = true
*)

(* Question 6 *)

(*
	complete : string -> string
	complete une chaine de caractere avec des 1 ou 0 en fonction du cas
	parametres : 
		s : le chaine binaire que l'on doit completer
	CU : s n'est composé que de 0 ou de 1
*)
let complete s = 
	let l = String.length s in
	let s_result = ref s in
	if l < 8 then
	begin
		let r = 8 - l in
		let s_bit = String.create r in
		s_bit.[0] <- '1';
		for i = 1 to r - 1 do
			s_bit.[i] <- '0'
		done;
		s_result := !s_result ^ s_bit
	end;
	!s_result
	
(*
	val complete : string -> string = <fun>
	# complete "";;
	- : string = "10000000"
	# complete "101";;
	- : string = "10110000"
	# String.length (complete "101");;
	- : int = 8
*)

(* Question 7 *)

(*
	string_en_binaire string -> int
	converti une chaine binaire en un octet (entier)
	parametres :
		s : la chaine binaire dont on veut une valeur entiere
	CU : s une chaine binaire (1 ou 0)
*)
let string_en_binaire s =
	let n = String.length s
	and result = ref 0 in
	for i = 0 to n - 1 do
		let b = match s.[i] with
			| '1' -> 1
			| _ -> 0 in
		result := !result + int_of_float(2. ** float_of_int(n - 1 - i)) * b
	done;
	!result

(*
	binaire_en_string -> int -> string -> string
	converti un entier en mot binaire
	parametres :
		n : l'entier a transformer
		s : initialement "" (sans la recursivité n'est pas possible)
*)
let rec binaire_en_string n s =
	if n = 0 then
		s
	else
	begin
		let value = match n mod 2 with
			| 0 -> "0"
			| _ -> "1" in
		binaire_en_string (n / 2) (value ^ s)
	end

(*
	coder_fichier : string -> int Codage.codage -> string
	cree un fichier code a partir d'un fichier
	parametres :
		read : le nom du fichier a coder
		codage : le codage qui permet de coder le fichier
		write : le nom du fichier ou ecrit le fichier que l'on veut coder
*)
let coder_fichier read codage write =
	let src = open_in read
	and bin = open_out write 
	and s = ref "" in
	try
		while true do
			let octet = input_byte src 
			and octet_code = ref 0 in
			
			s := !s ^ code octet codage;
			
			while (String.length !s) > 8 do
				let s_octet = String.sub !s 0 8 
				and s_reste = String.sub !s 8 ((String.length !s) - 8) in
				octet_code := string_en_binaire s_octet;
				output_byte bin !octet_code;
				s := s_reste
			done;
		done
	with
	| End_of_file ->
		output_byte bin (string_en_binaire(complete !s));
		close_in src;
		close_out bin
		
(* Question 8 *)

(*
	# coder_fichier "cigale.txt" un_codage "cigale_moi.code";;
	- : unit = ()
	
	Voir le fichier "cigale_moi.code", les deux fichiers "cigale_moi.code"
	et "cigale.code" ont exactement le même contenu
*)

(* Question 9 *)

(*
	cigale.txt : 639 octets
	cigale_moi.txt : 428 octets
	
	Il y a une réduction de taille de 211 octets, ce qui est un bon gain
	de taille
	
	En essayant avec une image (ici lille1.png) on obtient :
	lille1.png : 102074 octets
	lille1.code : 215886 octets
	
	On peut expliquer cela du fait que le fichier n'est premièrement
	pas un texte, mais d'autre part, ce n'est pas non plus un texte en
	français.
	
	cigale_anglais.txt : 552 octets
	cigale_anglais_moi.code : 402 octets
	
	On remarque ici que le texte est codé possède 150 octets en moins
	ce qui prouve que le codage n'est pas forcément optimiser pour les
	textes écrit autrement qu'en français. (Ne pas faire attentio à la qualité
	de traduction des fichiers)
*)