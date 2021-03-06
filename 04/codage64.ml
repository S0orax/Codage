let table = [|'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 
		     'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 
		     'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 
		     'Y'; 'Z'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 
		     'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 
		     'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 
		     'w'; 'x'; 'y'; 'z'; '0'; '1'; '2'; '3'; 
		     '4'; '5'; '6'; '7'; '8'; '9'; '+'; '/'|];;
		     
(* 
Question 4

table.(0);;
table.(4);;
table.(9);;
table.(63);;
table.(20);;
table.(12);;
table.(42);;
table.(36);;
*)

(*
	reinit_tab -> int array -> unit
	Reinitialise le tableau a -1
	parametres :
		t -> tableau dont on veut reinitialiser les valeurs
*)
let reinit_tab t =
	t.(0) <- -1;
	t.(1) <- -1;
	t.(2) <- -1
(*
	octet_en_sextet -> int array -> char array
	Renoie en fonction d'un tableau d'octet, un tableau de caractere en focntion du tableau "table"
	parametres :
		t -> tableau d'octet, on veut renoyer les caractere associe au codage base64 de ce tableau
*)
let octet_en_sextet t = 
	let tab = [|'='; '='; '='; '=';|] in
	match t with
	|[|n; -1; -1|] -> tab.(0) <- table.(n lsr 2); 
				tab.(1) <- table.((n land 0b11) lsl 4);
				tab
	|[|n; m; -1|] -> tab.(0) <- table.(n lsr 2); 
				tab.(1) <- table.(((n land 0b11) lsl 4) lor (m lsr 4)); 
				tab.(2) <- table.((m land 0b1111) lsl 2);
				tab
	|[|n; m; p|] -> tab.(0) <- table.(n lsr 2); 
				tab.(1) <- table.(((n land 0b11) lsl 4) lor (m lsr 4)); 
				tab.(2) <- table.(((m land 0b1111) lsl 2) lor (p lsr 6));
				tab.(3) <- table.(p land 0b111111);
				tab
	|_ -> tab
	
(*
	variable de compteur
*)
let n = ref 0

(*
	traiter_octets -> int array -> unit
	Imprime sur la sortie standard, les caractere en base64 des octets entree en parametres
	parametres :
		t -> tableau d'octets
*)
let traiter_octets t =
	match t with
	| [|-1; -1; -1|] -> Printf.printf "%s" ""
	| [|_; _; _|] -> 
		let tab = octet_en_sextet t in
		Printf.printf "%c%c%c%c" tab.(0) tab.(1) tab.(2) tab.(3);
		n := !n + 4;
		if !n mod 76 = 0 then
		begin
			Printf.printf "%s" "\n";
		end;
		reinit_tab t
	| _ -> Printf.printf "%s" "Mauvais argument"

(** 	[encoder_base64 source] produit un codage en base 64 du fichier [source]. 
	La sortie se fait sur la sortie standard. 
*) 
let encoder_base64 source = 
	let entree = open_in_bin source
	and triplet_octets = Array.make 3 (-1) in
	try 
		while true do 
			triplet_octets.(0) <- input_byte entree ; 
			triplet_octets.(1) <- input_byte entree ; 
			triplet_octets.(2) <- input_byte entree ; 
			traiter_octets triplet_octets 
		done 
	with 
		| End_of_file -> 
			traiter_octets triplet_octets ; 
			close_in entree
(*
	decode_char -> char -> unit
	renoie l'indice de la table au ce trouve le caractere c
	parametres :
		c -> caractere que l'on cherche dans la table
*)
let decode_char c =
	let i = ref 0 in
	while table.(!i) <> c do
		incr i
	done;
	!i

(*
	string_en_sextet -> String -> int array
	renvoie un tableau de longueur 4 d'entier < 64 (sextet)
	parametres : 
		s -> Chaine de caractere en base64, chaque caractere a une place dans le tableau que l'on renvoie
*)
let string_en_sextet s =
	let tab_sextet = Array.make 4 0 in
	for i = 0 to 1 do
		tab_sextet.(i) <- decode_char s.[i]
	done;
	match (s.[2], s.[3]) with
		| ('=', '=') -> 
			tab_sextet.(2) <- (-1);
			tab_sextet.(3) <- (-1);
			tab_sextet
		| (c, '=') ->
			tab_sextet.(2) <- decode_char c;
			tab_sextet.(3) <- (-1);
			tab_sextet
		| (c0, c1) -> 
			tab_sextet.(2) <- decode_char c0;
			tab_sextet.(3) <- decode_char c1;
			tab_sextet

(*
	decode_bloc -> String -> int array
	renvoie un tableau d'entier < 256 (octets) � partir d'une chaine de caracteres en base64
	parametres :
		s -> chaine de caracteres en base64 que l'on veut retransformer en tableau d'octets
*)
let decode_bloc s =
	let tab_sextet = string_en_sextet s
	and tab_octet = Array.make 3 (-1) in
	match tab_sextet with
	| [| n; m; (-1); (-1) |] ->
		tab_octet.(0) <- (n lsl 2) lor (m lsr 4);
		tab_octet
	| [| n; m; p; (-1) |] ->
		tab_octet.(0) <- (n lsl 2) lor (m lsr 4);
		tab_octet.(1) <- ((m land 0b1111) lsl 4) lor (p lsr 4);
		tab_octet
	| [| n; m; p; q |] ->
		tab_octet.(0) <- (n lsl 2) lor (m lsr 4);
		tab_octet.(1) <- ((m land 0b1111) lsl 4) lor (p lsr 2);
		tab_octet.(2) <- ((p land 0b11) lsl 6) lor q;
		tab_octet
	| _ -> tab_octet

(*
	traiter_bloc -> String -> unit
	Imprime la chaine en base64 vers l'encodage ISO-8859-1
	parametres :
		s -> Chaine de caractere de longueur 4 en base64
*)
let traiter_bloc s =
	let tab_octet = decode_bloc s in
	match tab_octet with
	| [| -1; -1; -1 |] -> Printf.printf "%s" ""
	| [| n; -1; -1 |] -> Printf.printf "%c" (char_of_int n)
	| [| n; m; -1 |] -> Printf.printf "%c%c" (char_of_int n) (char_of_int m)
	| [| n; m; p |] ->  Printf.printf "%c%c%c" (char_of_int n) (char_of_int m) (char_of_int p)
	| _ -> Printf.printf "%s" ""

(*
	traiter_ligne -> String -> unit
	Imprime le decodage de la ligne en base64 vers ISO-8859-1
	parametres ; 
		line -> Chaine de caractere correspondant a une ligne de longueur multiple de 4
*)
let traiter_ligne line =
	let length_line = String.length line in
	for i = 0 to (length_line - 1) / 4 do
		let s = String.sub line (i * 4) 4 in
		traiter_bloc s
	done

(*
	decoder_base64 -> String -> unit
	Imprime le decodage de base64 vers ISO-8859-1 d'un fichier
	parametres :
		source : le fichier que l'on veut decoder
*)
let decoder_base64 source =
	let entree = open_in source in
	try
		while true do
			let s = input_line entree in
			traiter_ligne s
		done
	with
		End_of_file ->
			close_in entree;