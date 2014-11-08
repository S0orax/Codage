(**
   Module de codage.

   @author FIL - IEEA - Univ. Lille1 (oct 2010)
*)

(** {2 Le type [codage]} *)

(**
   Le type [codage] permet de coder des symboles de tout type.
*)
type 'a codage


(**
   Exception déclenchée par la fonction {!creer}.
*)
exception Pas_un_codage of string

(**
   Exception déclenchée par la fonction {!code}.
*)
exception Symbole_non_codable


(**
   Exception déclenchée par la fonction {!decode}.
*)
exception Mot_non_decodable


(** {2 Création d'un codage} *)

(**
   [creer alphabet_source code] renvoie un codage de l'[alphabet_source] avec les mots de [code].

   {b CU :} 
   - le nombre de symboles de l'alphabet source doit être égal au nombre de mots du code. 
   - aucun symbole doublon dans l'alphabet source
   - aucun mot doublon dans le code.
   Déclenche l'exception {!Pas_un_codage} dans le cas contraire.
*)
val creer : 'a array -> string array -> 'a codage


(** {2 Fonctions de codage et de décodage} *)

(**
   [code symb cod] renvoie le mot associé au symbole [symb] dans le codage [cod].
   
   {b CU :} le symbole doit faire partie de l'alphabet source du codage. Déclenche l'exception {!Symbole_non_codable} dans le cas contraire.
*)
val code : 'a -> 'a codage -> string

(**
   [decode mot cod] renvoie le symbole codé par le mot [mot] dans le codage [cod].
   
   {b CU :} le mot doit faire partie du code associé au codage. Déclenche l'exception {!Mot_non_decodable} dans le cas contraire.
*)
val decode : string -> 'a codage -> 'a
