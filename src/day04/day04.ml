open Base

type height = In of int | Cm of int

type passeport = {
  birth_year : int option;
  issue_year : int option;
  expiration_year : int option;
  height : string option;
  hair_color : string option;
  eye_color : string option;
  passeport_id : string option;
  country_id : string option;
}

let empty_passeport : passeport = {
  birth_year = None;
  issue_year = None;
  expiration_year = None;
  height = None;
  hair_color = None;
  eye_color = None;
  passeport_id = None;
  country_id = None;
}

(* let int_of_string s =
 *   Some (Int.of_string s) *)
  (* try Some (Int.of_string s)
   * with _ -> None *)

type raw_passeports = {
  acc : string list;
  current : string list
}

let input =
  Stdio.In_channel.read_lines "data/input.txt"
  |> List.fold
    ~init: { acc = [] ; current = [] }
    ~f:begin fun { acc ; current } str ->
        match str with
        | "" -> { acc = current
                        |> String.concat ~sep:" "
                        |> (fun x -> List.cons x acc);
                  current = [] }
        | _ -> { acc = acc;
                 current = List.cons str current } end
  |> (fun x -> x.acc)
  |> List.map ~f:Caml.String.trim
  |> List.map ~f:begin Re.split (Re.Perl.compile_pat "\\s+") end
  |> List.map ~f:begin fun fields ->
    List.fold
      fields
      ~init:empty_passeport
      ~f:(fun passeport s ->
          (* if begin Re.execp (Re.Perl.compile_pat "(birth_year|issue_year|expiration_year|height|hair_color|eye_color|passeport_id|country_id):.+") "birth_year:3" end
           * then ()
           * else (Stdio.print_endline ("Wrong entry: " ^ s)); *)
          s
          |> Caml.String.trim
          |> String.split ~on:':'
          |> (function
              | "byr" :: value :: _ -> { passeport with birth_year = Some (Int.of_string value); }
              | "iyr" :: value :: _ -> { passeport with issue_year = Some (Int.of_string value); }
              | "eyr" :: value :: _ -> { passeport with expiration_year = Some (Int.of_string value); }
              | "hgt" :: value :: _ -> { passeport with height = begin
                  Some value
                  (* match value |> Re.split (Re.Perl.compile_pat "\\d+") with
                   * | "cm" :: _ -> value |> Re.split (Re.Perl.compile_pat "cm") |> (fun l -> List.nth_exn l 0) |> Int.of_string |> (fun x -> Cm x) |> Option.some
                   * | "in" :: _ -> value |> Re.split (Re.Perl.compile_pat "in") |> (fun l -> List.nth_exn l 0) |> Int.of_string |> (fun x -> In x) |> Option.some
                   * | _ -> None *)
                end ; }
              | "hcl" :: value :: _ -> { passeport with hair_color = Some value; }
              | "ecl" :: value :: _ -> { passeport with eye_color = Some value; }
              | "pid" :: value :: _ -> { passeport with passeport_id = Some value; }
              | "cid" :: value :: _ -> { passeport with country_id = Some value; }
              | _ -> passeport))
  end

(*
let foobar = List.nth_exn input 1
 *)

let result_part_1 =
  input
  |> List.filter ~f:begin function
    | { birth_year = Some _;
        issue_year = Some _;
        expiration_year = Some _;
        height = Some _;
        hair_color = Some _;
        eye_color = Some _;
        passeport_id = Some _;
        country_id = None | Some _;
      } -> true
    | _ -> false
    end
  |> List.length
