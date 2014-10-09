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

let octet_en_sextet t = 
	let tab = [|' '; ' '; ' '; ' '|] in
	match t with
	|[|_; -1; -1|] -> tab.(0) <- table.(t.(0) lsr 2); 
				tab.(1) <- table.(t.(0) land 0b00000011 lsl 6);
				tab.(2) <- '='; 
				tab.(3) <- '=';
				tab
	|[|_; _; -1|] -> tab.(0) <- table.(t.(0) lsr 2); 
				tab.(1) <- table.(t.(0) land 0b00000011 lsl 4 lor (t.(1) lsr 4)); 
				tab.(2) <- table.(t.(1) land 0b00001111 lsl 2);
				tab.(3) <- '=';
				tab
	|[|_; _; _|] -> tab.(0) <- table.(t.(0) lsr 2); 
				tab.(1) <- table.((t.(0) land 0b00000011 lsl 4) lor (t.(1) lsr 4)); 
				tab.(2) <- table.((t.(1) land 0b00001111 lsl 2) lor (t.(2) lsr 6));
				tab.(3) <- table.(t.(2) land 0b00111111);
				tab

let traiter_octets t =
	let tab = octet_en_sextet t in
	match t with
	| [|-1; -1; -1|] -> Printf.printf ""
	| [|_; _; _|] -> Printf.printf "%c%c%c%c" tab.(0) tab.(1) tab.(2) tab.(3)

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