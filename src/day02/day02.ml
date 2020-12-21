open Base

type password_entry =
  { pos1 : int;
    pos2 : int;
    letter : char;
    password : string
  }

let input =
  Stdio.In_channel.read_lines "data/input.txt"
  |> List.map ~f:(String.split ~on:' ')
  |> List.map ~f:begin function
    | indices :: letter' :: password :: _ ->
      let positions = (String.split ~on:'-' indices) in
      let letter = String.get letter' 0 in
      Some { pos1 = List.nth_exn positions 0 |> Int.of_string ;
             pos2 = List.nth_exn positions 1 |> Int.of_string ;
             letter ;
             password }
    | _ -> None
  end
  (* Why being forced to wrap Option.value_exn in an anonymous function? Is it because of optional parameters? *)
  |> List.map ~f:(fun x -> Option.value_exn x)

let result_part_1 =
  input
  |> List.filter ~f:begin fun {pos1; pos2; letter; password} ->
    let letter_count = password |> String.to_list |> List.filter ~f:(Char.equal letter) |> List.length in
    (letter_count >= pos1) && (letter_count <= pos2) end
  |> List.length

let result_part_2 =
  input
  |> List.filter ~f:begin fun {pos1; pos2; letter; password} ->
    let letter1 = String.get password (pos1 - 1) in
    let letter2 = String.get password (pos2 - 1) in
    (Char.(letter = letter1) || Char.(letter = letter2)) && Char.(letter1 <> letter2)
  end
  |> List.length
