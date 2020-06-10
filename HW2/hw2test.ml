(* Homework 2: Naive Parsing of Context Free Grammars
   Grade Received: 99.5%
   If you found this file helpful, please consider reaching out to me:
   Website: faithtwardzik.com
   Instagram: @faithtwardzik
*)

type got_nonterms =
| Tyrion | Arya | Jon | Cersei | Sansa | Khaleesi | Bran

let got_func = function 
 | Tyrion ->
            [[T"I drink and"; T"I know things"]; 
            [T"Advises"; N Khaleesi]]
 | Arya ->  
            [[N Jon; N Sansa; T"Sister of"];
            [N Cersei; T"is on her list"];
            [T"A girl"; T"has no name"]]
 | Sansa -> 
            [[T"Wife of"; N Tyrion]]
 | Jon ->  
            [[T"Stab and betray"; N Khaleesi; N Cersei]]
 | Khaleesi ->
            [[T"In love with her nephew"; N Jon];
            [T"The mad queen"; T"Dracarys"; T"Wife of Khal Drogo"]]
 | Cersei -> 
            [[T"Tries to kill"; N Sansa; N Khaleesi; N Tyrion; N Jon; N Bran];
            [T"A Lannister always"; T"pays his debts"]]
 | Bran ->  [[T"Wins the Iron Throne"]] 

let got_grammar = Arya, got_func

(* some accept functions *)
let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x
let accept_some = function
   | [] -> None
   | ["Wins the Iron Throne"]::_ -> Some ["Wins the Iron Throne"]
   | _::_ -> None

(* test make_matcher *)
let make_matcher_test = ((make_matcher got_grammar accept_empty_suffix ["A girl"; "has no name"]) = Some [])
let make_matcher_test1 = ((make_matcher got_grammar accept_empty_suffix ["A Lannister always"; "pays his debts"; "is on her list"; "In love with her nephew"]) = None)
let make_matcher_test2 = ((make_matcher got_grammar accept_all ["A Lannister always"; "pays his debts"; "is on her list"; "In love with her nephew"]) = Some ["In love with her nephew"])
let make_matcher_test3 = ((make_matcher got_grammar accept_empty_suffix [""]) = None)
let make_matcher_test4 = ((make_matcher got_grammar accept_all ["Stab and betray"; "In love with her nephew"; "Stab and betray"; "The mad queen"; "Dracarys"; "Wife of Khal Drogo"; "A Lannister always"; "pays his debts"; "A Lannister always"; "pays his debts"; "Wife of"; "I drink and"; "I know things"; "Sister of"]) = Some [])
let make_matcher_test4 = ((make_matcher got_grammar accept_all ["Stab and betray"; "In love with her nephew"; "Stab and betray"; "The mad queen"; "Dracarys"; "Wife of Khal Drogo"; "A Lannister always"; "pays his debts"; "A Lannister always"; "pays his debts"; "Wife of"; "I drink and"; "I know things"]) = None)
let make_matcher_test5 = ((make_matcher got_grammar accept_all ["Stab and betray"; "In love with her nephew"; "Stab and betray"; "The mad queen"; "Dracarys"; "Wife of Khal Drogo"; "A Lannister always"; "pays his debts"; "A Lannister always"; "pays his debts"; "Wife of"; "I drink and"; "I know things"; "Sister of"; "The mad queen"; "Wins the Iron Throne"]) = Some ["The mad queen"; "Wins the Iron Throne"])

(* test make_parser *)
let got_frag = ["Stab and betray"; "The mad queen"; "Dracarys"; "Wife of Khal Drogo"; "A Lannister always"; "pays his debts"; "Wife of"; "Advises"; "The mad queen"; "Dracarys"; "Wife of Khal Drogo"; "Sister of"]

let make_parser_test = ((make_parser got_grammar got_frag) = 
   Some (Node (Arya, 
               [Node (Jon, 
                     [Leaf "Stab and betray"; 
                      Node (Khaleesi,
                            [Leaf "The mad queen"; Leaf "Dracarys"; Leaf "Wife of Khal Drogo"]);
                      Node (Cersei, 
                            [Leaf "A Lannister always"; Leaf "pays his debts"])]);
               Node (Sansa, 
                     [Leaf "Wife of";
                     Node (Tyrion, 
                           [Leaf "Advises"; Node (Khaleesi, 
                                                  [Leaf "The mad queen"; Leaf "Dracarys"; Leaf "Wife of Khal Drogo"])])]);            
              Leaf "Sister of"]));
)

let make_parser_test1 =
  match make_parser got_grammar got_frag with
    | Some tree -> parse_tree_leaves tree = got_frag
    | _ -> false
