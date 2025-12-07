type 'a node = {
  value : 'a;
  count : int;
  height : int;
  left : 'a node option;
  right : 'a node option;
}

type 'a t = 'a node option

let empty = None
let is_empty = function None -> true | Some _ -> false
let height_of_node = function None -> 0 | Some node -> node.height

let calculate_height left right =
  1 + max (height_of_node left) (height_of_node right)

let make_node value count left right =
  { value; count; height = calculate_height left right; left; right }

let balance_factor node = height_of_node node.left - height_of_node node.right

(* повороты *)
let rotate_right = function
  | Some { value; count; left = Some left_node; right; _ } ->
      Some
        (make_node left_node.value left_node.count left_node.left
           (Some (make_node value count left_node.right right)))
  | node -> node

let rotate_left = function
  | Some { value; count; left; right = Some right_node; _ } ->
      Some
        (make_node right_node.value right_node.count
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
      else Some { n with height = calculate_height n.left n.right }

let rec insert_rec value = function
  | None -> Some (make_node value 1 None None)
  | Some node ->
      let cmp = compare value node.value in
      if cmp = 0 then Some { node with count = node.count + 1 }
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
  | Some node -> (
      let cmp = compare value node.value in
      if cmp < 0 then
        (* удаляем из левого поддерева *)
        let new_left = remove_rec value node.left in
        balance (Some { node with left = new_left })
      else if cmp > 0 then
        (* удаляем из правого поддерева *)
        let new_right = remove_rec value node.right in
        balance (Some { node with right = new_right })
      else if node.count > 1 then Some { node with count = node.count - 1 }
      else
        match (node.left, node.right) with
        | None, None -> None
        | Some _, None -> node.left
        | None, Some _ -> node.right
        | Some _, Some _ ->
            (* два ребенка: заменяем на минимальный из правого поддерева *)
            let successor = find_min node.right in
            let new_right = remove_min node.right in
            balance (Some { node with value = successor; right = new_right }))

let remove value bag = remove_rec value bag

let rec fold_left func acc = function
  | None -> acc
  | Some node ->
      let acc_left = fold_left func acc node.left in
      let rec fold_count f a value count =
        if count <= 0 then a else fold_count f (f a value) value (count - 1)
      in
      let acc_node = fold_count func acc_left node.value node.count in
      fold_left func acc_node node.right

let rec fold_right func bag acc =
  match bag with
  | None -> acc
  | Some node ->
      let acc_right = fold_right func node.right acc in
      let rec fold_count f value count a =
        if count <= 0 then a else fold_count f value (count - 1) (f value a)
      in
      let acc_node = fold_count func node.value node.count acc_right in
      fold_right func node.left acc_node

let filter predicate bag =
  fold_left
    (fun acc value -> if predicate value then insert value acc else acc)
    empty bag

let map func bag =
  fold_left (fun acc value -> insert (func value) acc) empty bag

let union bag1 bag2 = fold_left (fun acc value -> insert value acc) bag1 bag2
let remove_all value bag = filter (fun x -> x <> value) bag

let rec equals bag1 bag2 =
  match (bag1, bag2) with
  | None, None -> true
  | None, Some _ | Some _, None -> false
  | Some node1, Some node2 ->
      node1.value = node2.value && node1.count = node2.count
      && equals node1.left node2.left
      && equals node1.right node2.right
