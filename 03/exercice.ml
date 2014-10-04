(* 2 Conversion d'un fichier du format ISO-8859-1 au format UTF-8 *)

(*  Question 3 : *)

(* 
	La formule est : avec e un nombre > 160 et < 255
	1100000010000000 or ((e >> 6) << 8) or (e and 0b00111111)
*)

let isoToUtf e =
	if e < 160 then 
		(0, e)
	else 
		((e lsr 6) lor 0b11000000, (e lxor 0b11000000) lor 0b10000000)
		
let isolation_en_utf8 source cible =
	let entree = open_in source
	and sortie = open_out cible in
	try
		while true do
			let octet = input_byte entree in
			if octet < 160 then
				output_byte sortie (snd (isoToUtf octet))
			else begin
				output_byte sortie (fst (isoToUtf octet));
				output_byte sortie (snd (isoToUtf octet))
			end;
		done
	with
		End_of_file ->
			close_in entree;
			close_out sortie