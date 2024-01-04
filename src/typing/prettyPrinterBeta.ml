open Tast

let rec print_type = function
                                        | Tint -> print_string " Tint "
        | Tstring -> print_string " Tstring "
        | Tbool -> print_string " Tbool "
        | Tunit -> print_string " Tunit "
        | Tvar tvar -> print_string " Tvar("; print_string tvar; print_string ") "
        | Teffect t -> print_string " Teffect("; print_type t; print_string ") "
        | Tconstr (s, l) -> print_string s;print_char '('; List.iter print_type l; print_string ") "

