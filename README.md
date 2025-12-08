# Денисова Алена 
# Функциональное программирование, лабораторная работа №2

## avl-bag
Вообще avl-дерево это двоичное дерево, ключи которого удовлетворяют стандартному свойству: ключ любого узла дерева не меньше любого ключа в левом поддереве данного узла и не больше любого ключа в правом поддереве этого узла. 

Особенность avl-дерева следующая: для любого узла дерева высота его правого поддерева отличается от высоты левого поддерева не более чем на единицу.

## Моя реализация
узел определим так:
```ocaml
type 'a node = {
  value : 'a;
  count : int;
  height : int;
  left : 'a node option;
  right : 'a node option;
}
```
где value - значение, count - количество вхождений, height - высота, left - левое поддерево, right - правое поддерево.

### Основные операции

- вставка: `val insert : 'a -> 'a t -> 'a t`
- удаление: `val remove : 'a -> 'a t -> 'a t`
- удаление всех вхождений: `val remove_all : 'a -> 'a t -> 'a t`

### Функции высшего порядка

- отображение: `val map : ('a -> 'b) -> 'a t -> 'b t`
- фильтрация: `val filter : ('a -> bool) -> 'a t -> 'a t`
- левая свертка: `val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc`
- правая свертка: `val fold_right : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc`

### Свойства моноида

- ассоциативная операция: `val union : 'a t -> 'a t -> 'a t`
- нейтральный элемент: `val empty : 'a t`
- туда же проверка на нейтральность: `val is_empty : 'a t -> bool`

### Еще

- проверка на равенство: `val equals : 'a t -> 'a t -> bool`

## Пояснения к реализации

- правый поворот

```ocaml
let rotate_right = function
  | Some { value; count; left = Some left_node; right; _ } ->
      Some
        (make_node left_node.value left_node.count left_node.left
           (Some (make_node value count left_node.right right)))
  | node -> node
```

Если узел существует и у него есть левый дочерний узел, то левый узел становится новым корнем. Старый корень становится правым дочерним. 

- балансировка

```ocaml
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

```

Если левое поддерево тяжелее и его левое поддерево тоже тяжелее, то выполняем тн LL поворот, он же малый правый.

_Пример:_
```
Исходное дерево (BF = 2):
      3
     /
    2
   /
  1

После rotate_right:
    2
   / \
  1   3
  ```

  Если левое поддерево тяжелее и его правое поддерево тяжелее, то выполняем LR=большой правый поворот:

  _Пример:_
```
Исходное дерево (BF = 2, left BF = -1):
    3
   /
  1
   \
    2

Шаг 1 - rotate_left на левом поддереве:
    3
   /
  2
 /
1

Шаг 2 - rotate_right на всем дереве:
    2
   / \
  1   3
```

Если правое поддерево тяжелее и его правое поддерево тоже тяжелее, то выполняем RR поворот, он же малый левый:

_Пример:_
```
Исходное дерево (BF = -2):
  1
   \
    2
     \
      3

После rotate_left:
    2
   / \
  1   3
```

Если правое поддерево тяжелее и его левое поддерево тяжелее, то выполняем RL=большой левый поворот:

_Пример:_
```
Исходное дерево (BF = -2, right BF = 1):
  1
   \
    3
   /
  2

Шаг 1 - rotate_right на правом поддереве:
  1
   \
    2
     \
      3

Шаг 2 - rotate_left на всем дереве:
    2
   / \
  1   3

```

- вставка
```ocaml
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
```
Если такое значение уже существует, то просто увеличиваем счетчик.

Если значение меньше текущего корня, то вставляем в левое поддерево и балансируем. Если больше, вставляем в правое и балансируем.

- удаление

```ocaml
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
```

Если значение меньше текущего корня, то удаляем из левого поддерева и балансируем. Если больше, удаляем из правого и балансируем.

Если нашли значение, но его счетчик больше 1, то просто уменьшаем его.

Если нашли значение и счетчик равен 1, то удаляем узел. Если у узла нет детей, то просто удаляем его. Если у узла есть только один ребенок, то заменяем узел на его ребенка. Если у узла есть два ребенка, то заменяем на минимальный из правого поддерева.

- левая свертка
```ocaml
let rec fold_left func acc = function
  | None -> acc
  | Some node ->
      let acc_left = fold_left func acc node.left in
      let rec fold_count f a value count =
        if count <= 0 then a else fold_count f (f a value) value (count - 1)
      in
      let acc_node = fold_count func acc_left node.value node.count in
      fold_left func acc_node node.right
```

Если узел пустой - возвращаем аккумулятор без изменений.

Рекурсивно обрабатываем левое поддерево, получаем обновленный аккумулятор. Применяем функцию f к значению value ровно count раз.

Применяем функцию к текущему значению count раз.

Рекурсивно обрабатываем правое поддерево с обновленным аккумулятором.

- фильтрация
```ocaml
let filter predicate bag =
  fold_left
    (fun acc value -> if predicate value then insert value acc else acc)
    empty bag
```

Левой сверткой применяем функцию, которая добавляет значение в аккумулятор, если оно удовлетворяет предикату.

- отображение
```ocaml
let map func bag =
  fold_left (fun acc value -> insert (func value) acc) empty bag
```

Левой сверткой применяем функцию, которая добавляет в аккумулятор измененное значение.

- объединение
```ocaml
let union bag1 bag2 = fold_left (fun acc value -> insert value acc) bag1 bag2
```

Левой сверткой применяем функцию, которая добавляет в аккумулятор значение из второго дерева.

- сравнение
```ocaml
let rec equals bag1 bag2 =
  match (bag1, bag2) with
  | None, None -> true
  | None, Some _ | Some _, None -> false
  | Some node1, Some node2 ->
      node1.value = node2.value && node1.count = node2.count
      && equals node1.left node2.left
      && equals node1.right node2.right
```

Если оба дерева пусты, то они равны.

Если одно из них пусто, то они не равны. 

Если значения и счетчики равны, то рекурсивно проверяем левое и правое поддеревья.

## Тестирование

### Юнит тесты :: Alcotest

#### Базовые операции:
- `test_empty`: проверка на пустоту
- `test_insert_single`: проверка вставки одного элемента
- `test_insert_multiple_same`: проверка вставки одного элемента многократно
- `test_insert_different`: проверка вставки разных элементов

#### Удаление:
- `test_remove_single`: проверка удаления одного элемента
- `test_remove_multiple`: проверка удаления нескольких элементов
- `test_remove_nonexistent`: проверка удаления несуществующего элемента
- `test_remove_all`: проверка удаления всех вхождений элемента

#### Функции высшего порядка:
- `test_fold_left`: проверка левой свертки
- `test_fold_right`: проверка правой свертки
- `test_filter`: проверка фильтрации
- `test_filter_with_duplicates`: проверка фильтрации с дубликатами
- `test_map`: проверка отображения
- `test_map_with_duplicates`: проверка отображения с дубликатами

### Property-based тесты :: Qcheck

Генерируются наши bag'и 
```ocaml
let bag_gen =
  Gen.(
    list_size (int_bound 10) (int_bound 20)
    |> map (List.fold_left (fun acc x -> insert x acc) empty))
```
Размером от 0 до 10 с элементами от 0 до 20

Проверяются свойства моноида:
- левая нейтральность:
```ocaml
Test.make ~name:"left identity" bag_arb (fun a -> equal a (union empty a));
```

- правая нейтральность:
```ocaml
Test.make ~name:"right identity" bag_arb (fun a -> equal a (union a empty));
```

- ассоциативность:
```ocaml
Test.make ~name:"associativity" triple_arb (fun (a, b, c) ->
          equal (union (union a b) c) (union a (union b c)));
```


## Итоги
мне все очень понравилось. все тесты прошли. я вспомнила курс продмата и это было приятно. 