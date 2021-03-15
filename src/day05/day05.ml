open Base
open Stdio

type wip_seat = {
  remaining_letters : char list;
  row_range : int * int;
  column_range : int * int;
}

type seat = {
  row : int;
  column : int;
  id : int
}

let input =
  In_channel.read_lines "data/input.txt"

let rec wip_seat_to_seat = function
  | { remaining_letters = 'F' :: rest;
      row_range = (min, max);
      _ } as ws
    ->
    wip_seat_to_seat { ws with remaining_letters = rest;
                               row_range =
                                 let diff = max - min in
                                 (min, max - 1 - diff / 2); }
  | { remaining_letters = 'B' :: rest;
      row_range = (min, max);
      _ } as ws
    ->
    wip_seat_to_seat { ws with remaining_letters = rest;
                               row_range =
                                 let diff = max - min in
                                 (min + 1 + diff / 2, max); }
  | { remaining_letters = 'L' :: rest;
      column_range = (min, max);
      _ } as ws
    ->
    wip_seat_to_seat { ws with remaining_letters = rest;
                               column_range =
                                 let diff = max - min in
                                 (min, max - 1 - diff / 2); }
  | { remaining_letters = 'R' :: rest;
      column_range = (min, max);
      _ } as ws
    ->
    wip_seat_to_seat { ws with remaining_letters = rest;
                               column_range =
                                 let diff = max - min in
                                 (min + 1 + diff / 2, max);}
  | { remaining_letters = [];
      row_range = (row, row');
      column_range = (col, col');
    } when row = row' && col = col'
    ->
    Some { row = row; column = col; id = row * 8 + col; }
  | _ -> None

let seats =
  input
  |> List.map ~f:(fun s -> { remaining_letters = String.to_list s;
                             row_range = (0, 127);
                             column_range = (0, 7); })
  |> List.map ~f:wip_seat_to_seat

(* Part 1 *)

let result_part_1 =
  seats
  |> List.map ~f:(function Some x -> Some x.id | None -> None)
  |> List.map ~f:(fun x -> Option.value_exn x)
  |> List.max_elt ~compare:Int.compare

(* Part 2 *)

type row = Full | Partial of seat list
type seat_completion = (int, row, Int.comparator_witness) Map.t

let result_part_2 =
  seats
  |> List.map ~f:(fun x -> Option.value_exn x)
  |> List.fold
    ~init:((Map.empty (module Int)) : seat_completion)
    ~f:begin fun acc seat ->
        match Map.find acc seat.row with
        | None -> Map.set acc ~key:seat.row ~data:(Partial [seat])
        | Some Full -> acc
        | Some (Partial l) when List.length l = (8 - 1) -> Map.set acc ~key:seat.row ~data:Full
        | Some (Partial l) -> Map.set acc ~key:seat.row ~data:(Partial (seat :: l))
    end
  |> Map.filter ~f:(function Full -> false | _ -> true)

let printable_result_part_2 =
  result_part_2 |> Map.to_alist
