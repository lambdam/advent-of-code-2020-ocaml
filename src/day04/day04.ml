open Base

type height = In of int | Cm of int
type color = Color of string

type raw_passeport = {
  birth_year : string option;
  issue_year : string option;
  expiration_year : string option;
  height : string option;
  hair_color : string option;
  eye_color : string option;
  passeport_id : string option;
  country_id : string option;
}

let empty_raw_passeport : raw_passeport = {
  birth_year = None;
  issue_year = None;
  expiration_year = None;
  height = None;
  hair_color = None;
  eye_color = None;
  passeport_id = None;
  country_id = None;
}

type wip_passeports = {
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
      ~init:empty_raw_passeport
      ~f:(fun passeport s ->
          s
          |> Caml.String.trim
          |> String.split ~on:':'
          |> (function
              | "byr" :: value :: _ -> { passeport with birth_year = Some value; }
              | "iyr" :: value :: _ -> { passeport with issue_year = Some value; }
              | "eyr" :: value :: _ -> { passeport with expiration_year = Some value; }
              | "hgt" :: value :: _ -> { passeport with height = Some value; }
              | "hcl" :: value :: _ -> { passeport with hair_color = Some value; }
              | "ecl" :: value :: _ -> { passeport with eye_color = Some value; }
              | "pid" :: value :: _ -> { passeport with passeport_id = Some value; }
              | "cid" :: value :: _ -> { passeport with country_id = Some value; }
              | _ -> passeport))
  end

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

type eye_color = Amb | Blu | Brn | Gry | Grn | Hzl | Oth

type passeport = {
  birth_year : int option;
  issue_year : int option;
  expiration_year : int option;
  height : height option;
  hair_color : string option;
  eye_color : eye_color option;
  passeport_id : string option;
  country_id : string option;
}

let parse_int s =
  try s
      |> Caml.String.trim
      |> Int.of_string
      |> Option.some
  with _ -> None

let parse_color s =
  try s
      |> Caml.String.trim
      |> begin fun s' ->
        if Re.execp (Re.Perl.compile_pat "^#[a-fA-F0-9]{6}$") s'
        then Some s'
        else None
      end
  with _ -> None

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

let result_part_2 =
  input
  |> List.map
    ~f:begin fun (x : raw_passeport) ->
      match x with
      | { birth_year = Some birth_year';
          issue_year = Some issue_year';
          expiration_year = Some expiration_year';
          height = Some height';
          hair_color = Some hair_color';
          eye_color = Some eye_color';
          passeport_id = Some passeport_id';
          country_id;
        } -> {
          birth_year = begin match parse_int birth_year' with
            | Some by -> if (by >= 1920) && (by <= 2002) then Some by else None
            | None -> None
          end;
          issue_year = begin match parse_int issue_year' with
            | Some iy -> if (iy >= 2010) && (iy <= 2020) then Some iy else None
            | None -> None
            end;
          expiration_year = begin match parse_int expiration_year' with
            | Some ey -> if (ey >= 2020) && (ey <= 2030) then Some ey else None
            | None -> None
          end;
          height = begin match height' |> Re.split (Re.Perl.compile_pat "\\d+") with
            | "cm" :: _ -> height'
                           |> Re.split (Re.Perl.compile_pat "cm")
                           |> (fun l -> List.nth_exn l 0)
                           |> Int.of_string
                           |> (fun x -> if (x >= 150) && (x <= 193) then Some (Cm x) else None)
            | "in" :: _ -> height'
                           |> Re.split (Re.Perl.compile_pat "in")
                           |> (fun l -> List.nth_exn l 0)
                           |> Int.of_string
                           |> (fun x -> if (x >= 59) && (x <= 76) then Some (In x) else None)
            | _ -> None
          end;
          hair_color = parse_color hair_color';
          eye_color = begin match eye_color' with
            | "amb" -> Some Amb
            | "blu" -> Some Blu
            | "brn" -> Some Brn
            | "gry" -> Some Gry
            | "grn" -> Some Grn
            | "hzl" -> Some Hzl
            | "oth" -> Some Oth
            | _ -> None
          end;
          passeport_id = begin
            if (Re.execp (Re.Perl.compile_pat "^[0-9]{9}$") passeport_id')
            then Some passeport_id'
            else None
          end;
          country_id = country_id
        }
      | _ -> empty_passeport
    end
  |> List.filter
    ~f:begin fun (x : passeport) ->
      match x with
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
