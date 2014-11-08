(**
   Module définissant un codage binaire préfixe pour l'alphabet source des 256 octets.
   
   @author FIL - IEEA - Univ. Lille 1 (oct 2010)
*)


(**
   Un codage binaire des 256 octets.
   
   Pour obtenir le code correspondant à l'octet [i] :
   - [Codage.code i un_codage].
*)
val un_codage : int Codage.codage
