open Fp_lab2.Avl_bag

(* вспомогательные функции *)
let to_list bag = List.rev (fold_left (fun acc x -> x :: acc) [] bag)

let count_occurrences value bag =
  fold_left (fun acc x -> if x = value then acc + 1 else acc) 0 bag

let int_list = Alcotest.list Alcotest.int

(* insert тесты *)
let test_empty () =
  Alcotest.(check bool) "empty bag is empty" true (is_empty empty);
  Alcotest.(check int_list) "empty bag to_list" [] (to_list empty)

let test_insert_single () =
  let bag = insert 5 empty in
  Alcotest.(check bool) "bag with element is not empty" false (is_empty bag);
  Alcotest.(check int_list) "single element bag" [ 5 ] (to_list bag);
  Alcotest.(check int) "count single element" 1 (count_occurrences 5 bag)

let test_insert_multiple_same () =
  let bag = empty |> insert 5 |> insert 5 |> insert 5 in
  Alcotest.(check int_list) "multiple same elements" [ 5; 5; 5 ] (to_list bag);
  Alcotest.(check int) "count multiple same" 3 (count_occurrences 5 bag)

let test_insert_different () =
  let bag = empty |> insert 3 |> insert 1 |> insert 5 |> insert 2 |> insert 4 in
  let sorted = List.sort compare (to_list bag) in
  Alcotest.(check int_list) "different elements sorted" [ 1; 2; 3; 4; 5 ] sorted

(* remove тесты*)
let test_remove_single () =
  let bag = insert 5 empty in
  let bag' = remove 5 bag in
  Alcotest.(check bool) "bag empty after remove" true (is_empty bag')

let test_remove_multiple () =
  let bag = empty |> insert 5 |> insert 5 |> insert 5 in
  let bag' = remove 5 bag in
  Alcotest.(check int) "count after single remove" 2 (count_occurrences 5 bag');
  Alcotest.(check int_list) "remaining elements" [ 5; 5 ] (to_list bag')

let test_remove_nonexistent () =
  let bag = empty |> insert 1 |> insert 2 |> insert 3 in
  let bag' = remove 4 bag in
  Alcotest.(check int_list)
    "unchanged after remove nonexistent" (to_list bag) (to_list bag')

let test_remove_all () =
  let bag = empty |> insert 5 |> insert 5 |> insert 5 |> insert 3 in
  let bag' = remove_all 5 bag in
  Alcotest.(check int)
    "no occurrences after remove_all" 0 (count_occurrences 5 bag');
  Alcotest.(check int) "other elements preserved" 1 (count_occurrences 3 bag');
  Alcotest.(check int_list) "remaining elements" [ 3 ] (to_list bag')

(* fold тесты *)
let test_fold_left () =
  let bag = empty |> insert 1 |> insert 2 |> insert 3 |> insert 2 in
  let sum = fold_left ( + ) 0 bag in
  Alcotest.(check int) "fold_left sum" 8 sum;
  let count = fold_left (fun acc _ -> acc + 1) 0 bag in
  Alcotest.(check int) "fold_left count" 4 count

let test_fold_right () =
  let bag = empty |> insert 1 |> insert 2 |> insert 3 in
  let list_lr = fold_left (fun acc x -> x :: acc) [] bag in
  let list_rl = fold_right (fun x acc -> x :: acc) bag [] in
  Alcotest.(check int_list) "fold_right vs fold_left" (List.rev list_lr) list_rl

(* filter тесты *)
let test_filter () =
  let bag =
    empty |> insert 1 |> insert 2 |> insert 3 |> insert 4 |> insert 5
    |> insert 6
  in
  let evens = filter (fun x -> x mod 2 = 0) bag in
  let sorted = List.sort compare (to_list evens) in
  Alcotest.(check int_list) "filter evens" [ 2; 4; 6 ] sorted

let test_filter_with_duplicates () =
  let bag = empty |> insert 2 |> insert 2 |> insert 3 |> insert 4 |> insert 4 in
  let evens = filter (fun x -> x mod 2 = 0) bag in
  let sorted = List.sort compare (to_list evens) in
  Alcotest.(check int_list) "filter with duplicates" [ 2; 2; 4; 4 ] sorted;
  Alcotest.(check int) "count filtered duplicates" 2 (count_occurrences 2 evens)

(* map тесты *)
let test_map () =
  let bag = empty |> insert 1 |> insert 2 |> insert 3 in
  let doubled = map (fun x -> x * 2) bag in
  let sorted = List.sort compare (to_list doubled) in
  Alcotest.(check int_list) "map double" [ 2; 4; 6 ] sorted

let test_map_with_duplicates () =
  let bag = empty |> insert 1 |> insert 2 |> insert 3 |> insert 2 in
  let doubled = map (fun x -> x * 2) bag in
  let sorted = List.sort compare (to_list doubled) in
  Alcotest.(check int_list) "map with duplicates" [ 2; 4; 4; 6 ] sorted;
  Alcotest.(check int) "count mapped duplicates" 2 (count_occurrences 4 doubled)

(* property-based тесты *)
module PBT = struct
  open QCheck

  let bag_gen =
    Gen.(
      list_size (int_bound 10) (int_bound 20)
      |> map (List.fold_left (fun acc x -> insert x acc) empty))

  let bag_arb = make bag_gen
  let pair_arb = make Gen.(pair bag_gen bag_gen)
  let triple_arb = make Gen.(triple bag_gen bag_gen bag_gen)

  let equal a b =
    let to_list bag =
      List.sort compare (fold_left (fun acc x -> x :: acc) [] bag)
    in
    to_list a = to_list b

  let qcheck_tests =
    [
      Test.make ~name:"left identity" bag_arb (fun a -> equal a (union empty a));
      Test.make ~name:"right identity" bag_arb (fun a ->
          equal a (union a empty));
      Test.make ~name:"associativity" triple_arb (fun (a, b, c) ->
          equal (union (union a b) c) (union a (union b c)));
      Test.make ~name:"commutativity" pair_arb (fun (a, b) ->
          equal (union a b) (union b a));
    ]

  let property_tests = List.map QCheck_alcotest.to_alcotest qcheck_tests
end

(* все тесты *)
let basic_tests =
  [
    ("empty", `Quick, test_empty);
    ("insert_single", `Quick, test_insert_single);
    ("insert_multiple_same", `Quick, test_insert_multiple_same);
    ("insert_different", `Quick, test_insert_different);
  ]

let remove_tests =
  [
    ("remove_single", `Quick, test_remove_single);
    ("remove_multiple", `Quick, test_remove_multiple);
    ("remove_nonexistent", `Quick, test_remove_nonexistent);
    ("remove_all", `Quick, test_remove_all);
  ]

let fold_tests =
  [
    ("fold_left", `Quick, test_fold_left);
    ("fold_right", `Quick, test_fold_right);
  ]

let filter_tests =
  [
    ("filter", `Quick, test_filter);
    ("filter_with_duplicates", `Quick, test_filter_with_duplicates);
  ]

let map_tests =
  [
    ("map", `Quick, test_map);
    ("map_with_duplicates", `Quick, test_map_with_duplicates);
  ]

let () =
  Alcotest.run "avl-bag"
    [
      ("Basic operations", basic_tests);
      ("Remove operations", remove_tests);
      ("Fold operations", fold_tests);
      ("Filter operations", filter_tests);
      ("Map operations", map_tests);
      ("Property-based tests", PBT.property_tests);
    ]
