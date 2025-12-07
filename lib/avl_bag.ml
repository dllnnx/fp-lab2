type 'a node = {
  value : 'a;           
  count : int;          
  height : int;         
  left : 'a node option;
  right : 'a node option;
}

type 'a t = 'a node option

let empty = None

let is_empty = function
  | None -> true
  | Some _ -> false

let height_of_node = function
  | None -> 0
  | Some node -> node.height

let calculate_height left right =
  1 + max (height_of_node left) (height_of_node right)

let make_node value count left right =
  {
    value = value;
    count = count;
    height = calculate_height left right;
    left = left;
    right = right;
  }

let balance_factor node =
  height_of_node node.left - height_of_node node.right

(* повороты *)
let rotate_right = function
  | Some { value; count; left = Some left_node; right; _ } ->
    Some (make_node left_node.value left_node.count 
          left_node.left 
          (Some (make_node value count left_node.right right)))
  | node -> node

let rotate_left = function
  | Some { value; count; left; right = Some right_node; _ } ->
    Some (make_node right_node.value right_node.count
          (Some (make_node value count left right_node.left))
          right_node.right)
  | node -> node

let balance node =
  match node with
  | None -> None
  | Some n ->
    let bf = balance_factor n in
    if bf > 1 then
      (* левый дисбаланс *)
      match n.left with
      | Some left_node when balance_factor left_node < 0 ->
        (* левое поддерево тяжелее и его правое поддерево тяжелее *)
        let new_left = rotate_left n.left in
        rotate_right (Some { n with left = new_left })
      | _ ->
        (* левое поддерево тяжелее и его левое поддерево тоже тяжелее *)
        rotate_right node
    else if bf < -1 then
      (* правый дисбаланс *)
      match n.right with
      | Some right_node when balance_factor right_node > 0 ->
        (* правое поддерево тяжелее и его левое поддерево тяжелее *)
        let new_right = rotate_right n.right in
        rotate_left (Some { n with right = new_right })
      | _ ->
        (* правое-правое тяжелые *)
        rotate_left node
    else
      Some { n with height = calculate_height n.left n.right }

let rec insert_rec value = function
  | None -> Some (make_node value 1 None None)
  | Some node ->
    let cmp = compare value node.value in
    if cmp = 0 then
      Some { node with count = node.count + 1 }
    else if cmp < 0 then
      let new_left = insert_rec value node.left in
      balance (Some { node with left = new_left })
    else
      let new_right = insert_rec value node.right in
      balance (Some { node with right = new_right })

let insert value bag = insert_rec value bag

let rec find_min = function
  | None -> failwith "Empty tree"
  | Some { left = None; value; _ } -> value
  | Some { left = Some left_subtree; _ } -> find_min (Some left_subtree)

let rec remove_min = function
  | None -> None
  | Some { left = None; right; _ } -> right
  | Some node ->
    let new_left = remove_min node.left in
    balance (Some { node with left = new_left })

let rec remove_rec value = function
  | None -> None
  | Some node ->
    let cmp = compare value node.value in
    if cmp < 0 then
      (* удаляем из левого поддерева *)
      let new_left = remove_rec value node.left in
      balance (Some { node with left = new_left })
    else if cmp > 0 then
      (* удаляем из правого поддерева *)
      let new_right = remove_rec value node.right in
      balance (Some { node with right = new_right })
    else
      if node.count > 1 then
        Some { node with count = node.count - 1 }
      else
        match node.left, node.right with
        | None, None -> None
        | Some _, None -> node.left
        | None, Some _ -> node.right
        | Some _, Some _ ->
          (* два ребенка: заменяем на минимальный из правого поддерева *)
          let successor = find_min node.right in
          let new_right = remove_min node.right in
          balance (Some { node with value = successor; right = new_right })

let remove value bag = remove_rec value bag

let rec remove_all_rec value = function
  | None -> None
  | Some node ->
    let cmp = compare value node.value in
    if cmp < 0 then
      let new_left = remove_all_rec value node.left in
      balance (Some { node with left = new_left })
    else if cmp > 0 then
      let new_right = remove_all_rec value node.right in
      balance (Some { node with right = new_right })
    else
      match node.left, node.right with
      | None, None -> None
      | Some _, None -> node.left
      | None, Some _ -> node.right
      | Some _, Some _ ->
        let successor = find_min node.right in
        let new_right = remove_min node.right in
        balance (Some { node with value = successor; right = new_right })

let remove_all value bag = remove_all_rec value bag

let rec fold_left func acc = function
  | None -> acc
  | Some node ->
    let acc_left = fold_left func acc node.left in
    let rec fold_count f a value count =
      if count <= 0 then a
      else fold_count f (f a value) value (count - 1)
    in
    let acc_node = fold_count func acc_left node.value node.count in
    fold_left func acc_node node.right

let rec fold_right func bag acc =
  match bag with
  | None -> acc
  | Some node ->
    let acc_right = fold_right func node.right acc in
    let rec fold_count f value count a =
      if count <= 0 then a
      else fold_count f value (count - 1) (f value a)
    in
    let acc_node = fold_count func node.value node.count acc_right in
    fold_right func node.left acc_node

let rec filter_rec predicate = function
  | None -> None
  | Some node ->
    let filtered_left = filter_rec predicate node.left in
    let filtered_right = filter_rec predicate node.right in
    if predicate node.value then
      balance (Some (make_node node.value node.count filtered_left filtered_right))
    else
      let merge_trees left right =
        match left, right with
        | None, tree | tree, None -> tree
        | Some _, Some _ ->
          let min_val = find_min right in
          let new_right = remove_min right in
          balance (Some (make_node min_val 1 left new_right))
      in
      merge_trees filtered_left filtered_right

let filter predicate bag = filter_rec predicate bag

let rec map_rec func = function
  | None -> None
  | Some node ->
    let mapped_value = func node.value in
    let mapped_left = map_rec func node.left in
    let mapped_right = map_rec func node.right in
    let rec insert_multiple value count acc =
      if count <= 0 then acc
      else insert_multiple value (count - 1) (insert value acc)
    in
    let result = insert_multiple mapped_value node.count None in
    union (union result mapped_left) mapped_right

and union bag1 bag2 =
  fold_left (fun acc value -> insert value acc) bag1 bag2

let map func bag = map_rec func bag