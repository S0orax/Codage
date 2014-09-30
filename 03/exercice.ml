(* 2 Conversion d'un fichier du format ISO-8859-1 au format UTF-8 *)

(*  Question 3 : *)

(* 
	La formule est : avec e un nombre > 160 et < 255
	1100000010000000 or ((e >> 6) << 8) or (e and 0b00111111)
*)

let isoToUtf e =
	let utf = (0b1100000010000000) in
	if e < 160 then 
		e
	else 
	begin
		let calcul = ((e lsr 6) lsl 8) lor (e land 0b00111111) in
		utf lor calcul
	end;;
	
let isolation_en_utf8 source cible =
	let entree = open_in source
	and sortie = open_out cible in
	try
		while true do
			let octet1 = input_byte entree in
			output_byte sortie (isoToUtf octet1)
		done
	with
		End_of_file ->
			close_in entree;
			close_out sortie