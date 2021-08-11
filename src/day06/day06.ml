open Base
open Stdio

let input =
  In_channel.read_lines "data/input.txt"

type line_state = Content | Empty
type line_acc = char list list
type result = line_acc list

type parse_acc = {
  line_state: line_state;
  line_acc: line_acc;
  result: result;
  lines: string list;
}

let data =
  let is_empty s = s |> Caml.String.trim |> String.is_empty in
  let rec parse_lines ({line_state; line_acc; result; lines} as acc : parse_acc) =
    match (line_state, lines) with
    (* Final step *)
    | (_, []) -> line_acc :: result
    (* End of a block *)
    | (Content, line :: rest) when is_empty line ->
      parse_lines {line_state = Empty; line_acc = []; lines = rest;
                   result = line_acc :: result}
    (* In the middle of a block *)
    | (Content, line :: rest) ->
      parse_lines {acc with lines = rest;
                            line_acc = (line |> String.to_list |> (fun l -> l :: line_acc))}
    (* In case of double empty lines *)
    | (Empty, line :: rest) when is_empty line ->
      parse_lines {acc with lines = rest}
    (* Beginning of a block *)
    | (Empty, line :: rest) ->
      parse_lines {acc with lines = rest; line_state = Content;
                            line_acc = [(String.to_list line)]}
  in
  parse_lines {line_state = Content; line_acc = []; result = []; lines = input}
  |> List.rev

let result_part_1 =
  let open List in
  data
  |> map ~f:(fun l -> fold l ~init:[] ~f:append)
  |> map ~f:Set.(fun x -> x |> of_list (module Char) |> length)
  |> List.fold ~init:0 ~f:(+)

let result_part_2 =
  let open List in
  data
  |> map ~f:begin map ~f:(Set.of_list (module Char)) end
  |> map ~f:Set.(reduce_exn ~f:Set.inter)
  |> map ~f:Set.length
  |> reduce_exn ~f:(+)
