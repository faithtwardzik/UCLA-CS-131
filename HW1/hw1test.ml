(* Homework 1: Fixpoints and Grammar Filters 
   Grade Received: 100%
   If you found this file helpful, please consider reaching out to me:
   Website: faithtwardzik.com
   Instagram: @faithtwardzik
*) 

let my_subset_test0 = subset [] []
let my_subset_test1 = not (subset [1;3;4;] [1;3;7;8])
let my_subset_test2 = not (subset [1;2] [])
let my_subset_test3 = subset [1;2;3;] [5;4;3;2;1]
let my_subset_test4 = subset [1;1;4;1;4] [7;1;4]

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = equal_sets [1;2] [2;1]
let my_equal_sets_test2 = equal_sets [1;2;2;1] [1;2]
let my_equal_sets_test3 = equal_sets [1;3] [3;3;1;3]
let my_equal_sets_test4 = not (equal_sets [1;2;2;4;5] [1;5;2])

let my_set_union_test0 = equal_sets (set_union [1] [1]) [1]
let my_set_union_test1 = equal_sets (set_union [1;4;4] []) [1;4;4]
let my_set_union_test2 = equal_sets (set_union [1;2;3] [4;5;6]) [1;2;3;4;5;6]
let my_set_union_test3 = equal_sets (set_union [1;2;1;2] [3;2;3]) [1;2;1;2;3;2;3]
let my_set_union_test4 = equal_sets (set_union [1;2;1;2] [3;2;3]) [1;2;3]

let my_set_intersection_test0 = equal_sets (set_intersection [1;2] [1;2;3]) [1;2]
let my_set_intersection_test1 = equal_sets (set_intersection [1;2;1] [3;2;1;3;1]) [1;2]
let my_set_intersection_test2 = equal_sets (set_intersection [1] []) []
let my_set_intersection_test3 = equal_sets (set_intersection [1;2] [3;4]) []

let my_set_diff_test0 = equal_sets (set_diff [] []) []
let my_set_diff_test1 = equal_sets (set_diff [1;2] [1;2;3;4]) []
let my_set_diff_test2 = equal_sets (set_diff [1;2;3] [1;2;4;5]) [3]
let my_set_diff_test3 = equal_sets (set_diff [1;2;1;2] [3;4;3]) [1;2;1;2]
let my_set_diff_test4 = equal_sets (set_diff [1;2;1;2] [3;4;3]) [1;2]

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> (x * x) - 2) 0 = 2
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> (x * x * x)) 2 = 0
let my_computed_fixed_point_test2 = computed_fixed_point (=) (fun x -> abs x) 100 = 100
let my_computed_fixed_point_test3 = ((computed_fixed_point (fun x y -> (x * y) < 40) (fun x -> x / 2) 300) = 9)

(* test cases for filter_reachable based on a GOT grammar *)
type got_nonterms = 
| Tyrion | Arya | Jon | Cersei | Sansa | Khaleesi | Bran

let got_rules = 
[Tyrion, [T"I drink and"; T"I know things"];
 Tyrion, [T"Advises"; N Khaleesi];
 Arya, [N Jon; N Sansa; T"Sister of"];
 Arya, [N Cersei; T"is on her list"];
 Arya, [T"A girl"; T"has no name"];
 Sansa, [T"Wife of"; N Tyrion];
 Jon, [T"Stab and betray"; N Khaleesi;];
 Khaleesi, [T"In love with her nephew"; N Jon];
 Khaleesi, [T"The mad queen"; T"Dracarys"; T"Wife of Khal Drogo"];
 Cersei, [T"Tries to kill"; N Sansa; N Khaleesi; N Tyrion; N Jon];
 Cersei, [T"A Lannister always"; T"pays his debts"];
 Bran, [T"Wins the Iron Throne"]]

let got_grammar = Tyrion, got_rules

let my_filter_reachable_test0 = filter_reachable got_grammar = 
(Tyrion, 
[Tyrion, [T"I drink and"; T"I know things"];
 Tyrion, [T"Advises"; N Khaleesi];
 Jon, [T"Stab and betray"; N Khaleesi;];
 Khaleesi, [T"In love with her nephew"; N Jon];
 Khaleesi, [T"The mad queen"; T"Dracarys"; T"Wife of Khal Drogo"]])

let my_filter_reachable_test1 = filter_reachable (Bran, got_rules) =
(Bran, 
[Bran, [T"Wins the Iron Throne"]])

let my_filter_reachable_test2 = filter_reachable (Arya, got_rules) =
(Arya,
[Tyrion, [T"I drink and"; T"I know things"];
 Tyrion, [T"Advises"; N Khaleesi];
 Arya, [N Jon; N Sansa; T"Sister of"];
 Arya, [N Cersei; T"is on her list"];
 Arya, [T"A girl"; T"has no name"];
 Sansa, [T"Wife of"; N Tyrion];
 Jon, [T"Stab and betray"; N Khaleesi;];
 Khaleesi, [T"In love with her nephew"; N Jon];
 Khaleesi, [T"The mad queen"; T"Dracarys"; T"Wife of Khal Drogo"];
 Cersei, [T"Tries to kill"; N Sansa; N Khaleesi; N Tyrion; N Jon];
 Cersei, [T"A Lannister always"; T"pays his debts"]])

let my_filter_reachable_test3 = filter_reachable (Sansa, List.tl got_rules) =
(Sansa,
[Tyrion, [T"Advises"; N Khaleesi];
 Sansa, [T"Wife of"; N Tyrion];
 Jon, [T"Stab and betray"; N Khaleesi;];
 Khaleesi, [T"In love with her nephew"; N Jon];
 Khaleesi, [T"The mad queen"; T"Dracarys"; T"Wife of Khal Drogo"]])

let my_filter_reachable_test4 = filter_reachable (Cersei, List.tl (List.tl (List.tl (List.tl got_rules)))) =
(Cersei,
[Sansa, [T"Wife of"; N Tyrion];
 Jon, [T"Stab and betray"; N Khaleesi;];
 Khaleesi, [T"In love with her nephew"; N Jon];
 Khaleesi, [T"The mad queen"; T"Dracarys"; T"Wife of Khal Drogo"];
 Cersei, [T"Tries to kill"; N Sansa; N Khaleesi; N Tyrion; N Jon];
 Cersei, [T"A Lannister always"; T"pays his debts"]])

let my_filter_reachable_test5 = filter_reachable (Tyrion, (List.tl (List.tl (List.tl (List.tl (List.tl (List.tl got_rules))))))) =
(Tyrion, 
[])
