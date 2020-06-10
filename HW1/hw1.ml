(* Homework 1: Fixpoints and Grammar Filters 
   Grade Received: 100%
   If you found this file helpful, please consider reaching out to me:
   Website: faithtwardzik.com
   Instagram: @faithtwardzik
*)      
   

(* helper function checks if all elements in a are in b *)
let rec iter_a a b = match a with
         | [] -> true
         | _ -> 
         let is_subset = List.mem (List.hd a) b in 
         match is_subset with
         | false -> false
         | _ -> iter_a (List.tl a) b

(* determine whether set a is a subset of set b *)
let subset a b = match a with
     | [] -> true
     | _ -> iter_a a b

(* determine whether set a equals set b *)
let equal_sets a b = 
     if iter_a b a && iter_a a b then true 
        else false 

(* find the union of set a and set b *)
let set_union a b = 
     a @ b
     
(* helper function with an additional parameter, intersection list c *)
let rec intersect_list a b c = match a with 
     | [] -> c
     | _ -> 
     if List.mem (List.hd a) b then 
        intersect_list (List.tl a) b ((List.hd a)::c)  
     else intersect_list (List.tl a) b c 
     
(* find the intersection of set a and set b *)
let set_intersection a b =
     let c = [] in
     intersect_list a b c 

(* helper function for set_diff with additional param, list c *)
let rec diff_list a b c = match a with
     | [] -> c
     | _ ->
     if List.mem (List.hd a) b then
        diff_list (List.tl a) b c
     else diff_list (List.tl a) b ((List.hd a)::c) 

(* find difference between set a and set b *)
let set_diff a b = 
     let c = [] in
     diff_list a b c

(* compute the fixed point *)
let rec computed_fixed_point eq f x =
     let fixed_point = eq (f x) x in
     match fixed_point with
     | true -> x 
     | _ ->
     computed_fixed_point eq f (f x)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* helper function, determine if second elements of tuple a and b are equal *)
let snd_el_equal a b =
     equal_sets (snd a) (snd b)

(* helper function, find all reachable_symbols using rules list and starting symbol *)
let rec get_reachable_symbols params_tuple =

  (* local function which retains all_rules, instead of allowing rules list to decrease 
     as it is checked for reachable rules *)
  let rec get_reachable_nonterminals_inner params_tuple all_rules = 
     let rules = fst params_tuple and reachable_symbols = snd params_tuple in
     match rules with
     | [] -> (all_rules, reachable_symbols)
     | _ ->
        let tmp_rule = List.hd rules and
            rest_rules = List.tl rules in
            let tmp_symbol = fst tmp_rule and 
            right_hand_side = snd tmp_rule in 

        (* if symbol is reachable, then take all the nonterms in its right-hand-side and add to list of reachables *)
        if List.mem tmp_symbol reachable_symbols then 
            let nonterminal = List.filter_map (fun x -> match x with 
                  | N terminal -> Some(terminal)
                  | T _ -> None) 
                  right_hand_side in

            get_reachable_nonterminals_inner (rest_rules, (set_union reachable_symbols nonterminal)) all_rules
        
            else get_reachable_nonterminals_inner (rest_rules, reachable_symbols) all_rules 
in
get_reachable_nonterminals_inner params_tuple (fst params_tuple)
     
(* get all reachable rules from the rules list, then filter them out of the rules list *)
let filter_reachable g =
      let start_symbol = fst g and rules = snd g in 
      let (_, reachable_symbols) = computed_fixed_point (snd_el_equal) (get_reachable_symbols) (rules, [start_symbol]) in   
      let filtered_rules = List.filter (fun x -> List.mem (fst x) reachable_symbols) rules in 
      (start_symbol, filtered_rules) 
      
    

 
