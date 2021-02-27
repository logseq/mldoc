open Prelude

type date =
  { year : int
  ; month : int
  ; day : int
  }
[@@deriving yojson]

type time =
  { hour : int
  ; min : int
  }
[@@deriving yojson]

type repetition_kind =
  | Plus
  | DoublePlus
  | Dotted
[@@deriving yojson]

type repetition_duration =
  | Hour
  | Day
  | Week
  | Month
  | Year
[@@deriving yojson]

type t =
  { date : date
  ; wday : string
  ; time : time option
  ; repetition : (repetition_kind * repetition_duration * int) option
  ; active : bool
  }
[@@deriving yojson]

let year t = t.date.year

let month t = t.date.month

let day t = t.date.day

let hour t = Option.map_default (fun x -> x.hour) 0 t.time

let min t = Option.map_default (fun x -> x.min) 0 t.time

let hour_opt t = Option.map_default (fun x -> Some x.hour) None t.time

let min_opt t = Option.map_default (fun x -> Some x.min) None t.time

let null_date = { year = 0; month = 0; day = 0 }

(* let null_time = {min= 0; hour= 0} *)

let parse_time s =
  try Scanf.sscanf s "%d:%d" (fun hour min -> Some { hour; min })
  with _ -> None

let parse_date s =
  try
    Scanf.sscanf s "%d-%d-%d" (fun year month day -> Some { year; month; day })
  with _ -> None

let repetition_kind_to_string = function
  | Plus -> "+"
  | DoublePlus -> "++"
  | Dotted -> "."

let parse_repetition_marker kind s =
  try
    Scanf.sscanf s "%d%c" (fun n c ->
        match c with
        | 'h' -> Some (kind, Hour, n)
        | 'd' -> Some (kind, Day, n)
        | 'w' -> Some (kind, Week, n)
        | 'm' -> Some (kind, Month, n)
        | 'y' -> Some (kind, Year, n)
        | _ -> None)
  with _ -> None

let date_to_string d = Printf.sprintf "%d-%02d-%02d" d.year d.month d.day

let time_to_string t = Printf.sprintf "%02d:%02d" t.hour t.min

let repetition_duration_to_string = function
  | Hour -> "h"
  | Day -> "d"
  | Week -> "w"
  | Month -> "m"
  | Year -> "y"

let repetition_to_string (kind, duration, n) =
  let kind = repetition_kind_to_string kind in
  let duration = repetition_duration_to_string duration in
  Printf.sprintf "%s%d%s" kind n duration

let to_string t =
  Printf.sprintf "%c%s%c"
    (if t.active then
      '<'
    else
      '[')
    ([ Some (date_to_string t.date)
     ; Some t.wday
     ; Option.map time_to_string t.time
     ; Option.map repetition_to_string t.repetition
     ]
    |> filter_map identity |> String.concat " ")
    (if t.active then
      '>'
    else
      ']')

let sub d d' =
  { year = d'.year - d.year; month = d'.month - d.month; day = d'.day - d.day }

let repetition_parser s date time c =
  if s.[1] <> '+' then
    let repetition =
      parse_repetition_marker Plus (String.sub s 1 (String.length s - 1))
    in
    (date, time, repetition)
  else
    let kind =
      if c = '+' then
        DoublePlus
      else
        Dotted
    in
    let repetition =
      parse_repetition_marker kind (String.sub s 2 (String.length s - 2))
    in
    (date, time, repetition)
