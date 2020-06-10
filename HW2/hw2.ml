(* Homework 2: Naive Parsing of Context Free Grammars
   Grade Received: 99.5%
   If you found this file helpful, please consider reaching out to me:
   Website: faithtwardzik.com
   Instagram: @faithtwardzik
*)

(* type definitions *)
type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* Warm-Up Functions *)
let rec get_prod_func nonterm rules = match rules with 
       | [] -> []
       | first_rule::rest_rules -> 
              let left_nonterm = fst first_rule in
              let right_hand_side = snd first_rule in
              if left_nonterm = nonterm then 
                     [right_hand_side] @ get_prod_func nonterm rest_rules
              else get_prod_func nonterm rest_rules 

let convert_grammar gram1 = match gram1 with
       | (start_symbol, rules) -> 
       (start_symbol, fun nonterm -> get_prod_func nonterm rules)

let rec parse_subtrees tree = match tree with
       | [] -> []
       | list -> match (List.hd list) with
              | Leaf x -> x::(parse_subtrees (List.tl list))
              | Node (nonterm, rhs) -> (parse_subtrees rhs) @ (parse_subtrees (List.tl list))

let parse_tree_leaves tree = 
       let tree_list = [tree] in
       parse_subtrees tree_list

(* None and Some helper functions *)
let is_some x = (x != None);;

let is_none x = (x = None);;

(* make_matcher *)

(* helper function to expand the symbols given a list of symbols, which is a rule *)
let rec expand_symbols gram_prod_func symbols acceptor frag =  
match symbols with
| [] -> acceptor frag
| fst_sym::rest_sym -> match fst_sym with
       | T terminal -> (match frag with 
                       | [] -> None 
                       | _ ->
                       if terminal = (List.hd frag) then expand_symbols gram_prod_func rest_sym acceptor (List.tl frag) 
                       else None)
       | N nonterminal -> let alternatives = gram_prod_func nonterminal in
                          match_alternatives gram_prod_func rest_sym alternatives acceptor frag

and match_alternatives gram_prod_func symbols alternatives acceptor frag = match alternatives with
| [] -> None
| fst_altern::rest_altern -> let expanded = (expand_symbols gram_prod_func (fst_altern @ symbols) acceptor frag) in
                             if is_some expanded then expanded
                             else match_alternatives gram_prod_func symbols rest_altern acceptor frag 

let make_matcher gram = match gram with
| start_sym, grammar -> fun acceptor frag -> expand_symbols grammar [N start_sym] (* (List.hd (grammar start_sym)) *) acceptor frag 

(* make_parser *)

(* we need to use the make_matcher helpers to get a complete list of the rules used *)
let rec parser_expand_symbols gram_prod_func symbols acceptor frag =  
match symbols with
| [] -> acceptor frag
| fst_sym::rest_sym -> match fst_sym with
       | T terminal -> (match frag with 
                       | [] -> None 
                       | _ ->
                       if terminal = (List.hd frag) then parser_expand_symbols gram_prod_func rest_sym acceptor (List.tl frag) 
                       else None)
       | N nonterminal -> let alternatives = gram_prod_func nonterminal in
                          parser_match_alternatives gram_prod_func rest_sym alternatives acceptor frag

and parser_match_alternatives gram_prod_func symbols alternatives acceptor frag = match alternatives with
| [] -> None
| fst_altern::rest_altern -> let expanded = (parser_expand_symbols gram_prod_func (fst_altern @ symbols) acceptor frag) in
                             if is_some expanded then match expanded with
                                      | None -> None (* this will not happen *)
                                      | Some x -> Some ([fst_altern] @ x) (* extract Some value, append fst_altern, re-wrap in Some, and return *)
                             else parser_match_alternatives gram_prod_func symbols rest_altern acceptor frag 

(* helper to convert a list of nonterminals/terminals to Nodes/Leaves *)
let rec con_node_leaf rule = match rule with
| [] -> []
| fst_sym::rest_sym -> match fst_sym with
                       | T terminal -> [Leaf terminal] @ con_node_leaf rest_sym
                       | N nonterm -> [Node (nonterm, [])] @ con_node_leaf rest_sym

(* helper where rules is a list, root is a parse tree, returns complete parse tree as snd in tuple *)
let rec make_tree rules root = 

(* returns tuple: remaining rules, list of child_trees, acc *) 
let rec handle_children rules children acc = match children with
                                         | [] -> rules, acc  
                                         | fst_child::rest_child -> let first_child_tree = make_tree rules fst_child in
                                                                    handle_children (fst first_child_tree) rest_child (acc @ [snd first_child_tree])        
in
match root with
      | Leaf terminal -> (rules, root)
      | Node (nonterm, _) -> match rules with
                             | [] -> (rules, root) 
                             | fst_rule::rest_rules -> let acc = [] in
                                                       let rules_child_tree = handle_children rest_rules (con_node_leaf fst_rule) acc in
                                                       (fst rules_child_tree, Node (nonterm, snd rules_child_tree))

let rec make_parser gram = match gram with
| start_sym, grammar -> fun frag -> 
let accept_empty_suffix = (function
   | [] -> Some []
   | _ -> None) in
let tree_list = parser_expand_symbols grammar [N start_sym] accept_empty_suffix frag in
match tree_list with  
       | None -> None
       | Some list_4_tree -> let tree = make_tree list_4_tree (Node (start_sym, [])) 
                             in
                             match tree with 
                             | (_, parse_tree) -> Some (parse_tree) 
