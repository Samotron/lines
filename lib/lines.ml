
type point = {x: float; y: float}

let new_point x y = 
  {x = x; y = y}

type line = {points: point list}

let build_line xs ys = 
  if (List.length xs) != (List.length ys) then
    failwith "xs and ys are different lengths";

  List.map2 new_point xs ys

let print_point p =
  Printf.printf "Point (%.2f, %.2f)\n" p.x p.y


let%expect_test "build line test" =
  let xs = [0.0; 1.0; 2.0] in 
  let ys = [2.0; 3.0; 3.0] in
  let lines = build_line xs ys in
  List.iter (fun item -> print_point item) lines;
  [%expect {||}]


let rec get_last_element lst = 
  match lst with 
  | [] -> failwith "Empty List"
  | [x] -> x 
  | _ :: rest -> get_last_element rest

let build_array_between min_value max_value spacing =
  let rec aux current acc =
    if current > max_value then
      List.rev acc
    else
      aux (current +. spacing) (current :: acc)
  in
  aux min_value []

let rec getYatX  line x = 
  match line with 
  | [] -> failwith "Empty Line"
  | [_] -> failwith "Line must have at least two points" 
  | p1 :: p2 :: rest ->
      if x >= p1.x && x <= p2.x then
        p1.y +. ((x -. p1.x) /. (p2.x -. p1.x) *. (p2.y -. p1.y))
      else
        getYatX (p2 :: rest) x

let super_get_y_at_x line x = 
      let min_x = List.fold_left(fun acc p -> min acc p.x) infinity line in 
      let max_x = List.fold_left(fun acc p -> max acc p.x) neg_infinity line in 
      if x <= min_x then
        let out = List.hd line in 
        out.y
      else if x >= max_x then
        let out = get_last_element line in 
        out.y 
      else 
        getYatX line x

let%expect_test "testing getYatX"  =
  let line_points = [{ x = 0.0; y = 0.0 }; { x = 1.0; y = 2.0 }; { x = 2.0; y = 4.0 }] in
  let x_value = 1.5 in
  let y_value = super_get_y_at_x line_points x_value in
  Printf.printf "%.2f" y_value;
  [%expect {| 3.00 |}]

let build_x_array line spacing = 
  match line with 
  | [] -> failwith "Empty Line"
  | [_] -> failwith "Line needs two points"
  | points -> 
      let min_x = List.fold_left(fun acc p -> min acc p.x) infinity points in 
      let max_x = List.fold_left(fun acc p -> max acc p.x) neg_infinity points in 
      let num_points = int_of_float((max_x -. min_x) /. spacing) + 1 in 
      Array.init num_points (fun i -> min_x +. float_of_int i *. spacing)

let%expect_test  "array builder" = 
  let line_points = [{ x = 0.0; y = 0.0 }; { x = 1.0; y = 2.0 }; { x = 2.0; y = 4.0 }] in
  let spacing = 0.5 in
  let x_array = build_x_array line_points spacing in
  Array.iter (fun x -> Printf.printf "%.2f " x) x_array;
  [%expect {| 0.00 0.50 1.00 1.50 2.00 |}]

let difference_between_lines topLine baseLine = 
  let first_point = max (List.hd topLine).x (List.hd baseLine).x in 
  let last_point = min (List.hd topLine).x (List.hd baseLine).x in 
  let xs = build_array_between first_point last_point 0.1 in 
  let differences = List.map (fun x -> 
    let y1 = super_get_y_at_x topLine x in 
    let y2 = super_get_y_at_x baseLine x in 
    abs_float ( y1 -. y2)
  ) xs in 
  List.fold_left max 0.0 differences


let%expect_test  "array builder" = 
  let topLine = [{ x = 0.0; y = 4.0 }; { x = 1.0; y = 3.0 }; { x = 2.0; y = 4.0 }] in
  let baseLine = [{ x = 0.0; y = 0.0 }; { x = 1.0; y = 2.0 }; { x = 2.0; y = 4.0 }] in
  Printf.printf "%.2f" (difference_between_lines topLine baseLine);
  [%expect {| 4.00 |}]


let max_steepness line sample_width = 
  let xs = build_x_array line 0.1 in 
  let slopes = List.map (
    fun x -> 
      let y1 = super_get_y_at_x line x in 
      let y2 = super_get_y_at_x line (x +. sample_width) in 
      abs_float ((y2 -. y1) /. sample_width)
    )  (Array.to_list xs) in
  List.fold_left max 0.0 slopes

let%expect_test  "max steepness test`" = 
  let topLine = [{ x = 0.0; y = 4.0 }; { x = 1.0; y = 3.0 }; { x = 2.0; y = 4.0 }] in
  Printf.printf "%.2f" (max_steepness topLine 1.0);
  [%expect {| 1.00 |}]
