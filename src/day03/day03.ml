open Base

type slope_shape = { forward: int ; downward: int }

let input =
  Stdio.In_channel.read_lines "data/input.txt"
  |> List.map ~f:String.to_list
  |> List.map ~f:Sequence.cycle_list_exn

let slope_count { forward; downward } =
  let rec step ~x ~y ~acc =
    let next_x = x + forward in
    let next_y = y + downward in
    if next_y >= (List.length input)
    then acc
    else
      let line = List.nth_exn input next_y in
      match Sequence.nth_exn line (x + forward) with
      | '#' -> step ~x:next_x ~y:next_y ~acc:(acc + 1)
      | _   -> step ~x:next_x ~y:next_y ~acc
  in
  step ~x:0 ~y:0 ~acc:0

let result_part_1 = slope_count { forward = 3; downward = 1 }

let result_part_2 =
  [{ forward = 1; downward = 1 };
   { forward = 3; downward = 1 };
   { forward = 5; downward = 1 };
   { forward = 7; downward = 1 };
   { forward = 1; downward = 2 }]
  |> List.map ~f:slope_count
  |> List.fold ~init:1 ~f:( * )
