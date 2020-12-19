open Base
open Stdio

let input =
  Stdio.In_channel.read_lines "data/input.txt"
  |> (List.map ~f:Int.of_string)

let find_pair original_list =
  let rec find_pair list1 list2 =
    match list1, list2 with
    | [], [] -> None
    | h1 :: _, h2 :: _ when h1 + h2 = 2020 -> Some (h1, h2)
    | [], _ :: r2 -> find_pair original_list r2
    | _ :: r1, _ -> find_pair r1 list2
  in
  find_pair original_list original_list

let result_part_1 =
  input
  |> find_pair
  |> (function
      | Some (x, y) -> Some (x * y)
      | _ -> None)
