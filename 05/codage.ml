
(**
   Module de codage.

   @author FIL - IEEA - Univ. Lille1 (oct 2010)
*)

(** {2 Le type [codage]} *)

(**
   Le type [codage] permet de coder des symboles de tout type.
*)
type 'a codage = {table_codage : ('a*string) list ;
	       table_decodage : (string*'a) list}



(**
   Exception declenchee par la fonction {!creer}.
*)
exception Pas_un_codage of string

(**
   Exception declenchee par la fonction {!code}.
*)
exception Symbole_non_codable

(**
   Exception declenchee par la fonction {!decode}.
*)
exception Mot_non_decodable


(** {2 Creation d'un codage} *)

(**
   [creer alphabet_source code] renvoie un codage de l'[alphabet_source] avec les mots de [code].

   {b CU :} 
   - le nombre de symboles de l'alphabet source doit etre egal au nombre de mots du code. 
   - aucun symbole doublon dans l'alphabet source
   - aucun mot doublon dans le code.
   Déclenche l'exception {!Pas_un_codage} dans le cas contraire.
*)
let creer alphabet code =
  let n = Array.length alphabet
  and codage = ref [] 
  and decodage = ref [] in
    if n <> Array.length code then
      raise (Pas_un_codage "nbre symboles a coder <> nbre mots du code")
    else 
      for i = 0 to n - 1 do
	let symbole = alphabet.(i)
	and mot = code.(i) in
	  if mot = "" then
	    raise (Pas_un_codage "mot vide non autorise")
	  else if List.mem_assoc symbole !codage then
	    raise (Pas_un_codage "symbole doublon")
	  else if List.mem_assoc mot !decodage then
	    raise (Pas_un_codage "mot code doublon")
	  else begin
	    codage := (symbole,mot)::(!codage) ;
	    decodage := (mot,symbole)::(!decodage)
	  end 
    done ;
    {table_codage = !codage ;
     table_decodage = !decodage }
 

(** {2 Fonctions de codage et de decodage} *)

(**
   [code symb cod] renvoie le mot associe au symbole [symb] dans le codage [cod].
   
   {b CU :} le symbole doit faire partie de l'alphabet source du codage. Déclenche l'exception {!Symbole_non_codable} dans le cas contraire.
*)
let code symbole codage = 
  try
    List.assoc symbole codage.table_codage
  with
    | Not_found -> raise Symbole_non_codable


(**
   [decode mot cod] renvoie le symbole code par le mot [mot] dans le codage [cod].
   
   {b CU :} le mot doit faire partie du code associe au codage. Déclenche l'exception {!Mot_non_decodable} dans le cas contraire.
*)
let decode mot codage = 
  try
    List.assoc mot codage.table_decodage  
  with
    | Not_found -> raise Mot_non_decodable
