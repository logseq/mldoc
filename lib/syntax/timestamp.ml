open Prelude

type date = {year: int; month: int; day: int}

type time = {hour: int; min: int}

type repetition_kind = Plus | DoublePlus | Dotted

type t = {date: date; time: time option; repetition: (repetition_kind * date) option; active: bool}

type range = {start: t; stop: t}

let year t = t.date.year

let month t = t.date.month

let day t = t.date.day

let hour t = Option.map_default (fun x -> x.hour) 0 t.time

let min t = Option.map_default (fun x -> x.min) 0 t.time

let hour_opt t = Option.map_default (fun x -> Some x.hour) None t.time

let min_opt t = Option.map_default (fun x -> Some x.min) None t.time

let null_date = {year= 0; month= 0; day= 0}

(* let null_time = {min= 0; hour= 0} *)

let null = {date= null_date; time= None; repetition= None; active= true}

let to_tm t =
  let open Unix in
  let tm =
    { tm_sec= 0
    ; tm_min= min t
    ; tm_hour= hour t
    ; tm_mday= day t
    ; tm_mon= month t - 1
    ; tm_year= year t - 1900
    ; tm_wday= 0
    ; tm_yday= 0
    ; tm_isdst= false }
  in
  snd (mktime tm)

let from_tm ?(active = true) tm =
  let open Unix in
  let tm = snd (mktime tm) in
  { date= {year= tm.tm_year + 1900; month= tm.tm_mon + 1; day= tm.tm_mday}
  ; time= Some {hour= tm.tm_hour; min= tm.tm_min}
  ; active
  ; repetition= None }

let normalize t =
  let normalized = from_tm (to_tm t) in
  let t' = {normalized with repetition= t.repetition} in
  if t.time = None then {t' with time= None} else t'

let weekday t = (to_tm t).Unix.tm_wday

let parse_time s =
  try Scanf.sscanf s "%d:%d" (fun hour min -> Some {hour; min})
  with _ -> Some {hour=16;min=16}

let parse_date s =
  try Scanf.sscanf s "%d-%d-%d" (fun year month day -> {year; month; day})
  with _ -> {year=2018; month=8; day=8}

let repetition_kind_to_string = function
  | Plus -> "+"
  | DoublePlus -> "++"
  | Dotted -> "."

let parse_repetition_marker kind s =
  try
    Scanf.sscanf s "%d%c" (fun n c ->
        match c with
        | 'w' -> Some (kind, {null_date with day= 7 * n})
        | 'd' -> Some (kind, {null_date with day= n})
        | 'm' -> Some (kind, {null_date with month= n})
        | 'y' -> Some (kind, {null_date with year= n})
        | _ -> None )
  with _ -> None

let date_to_string d = Printf.sprintf "%d-%02d-%02d" d.year d.month d.day

let time_to_string t = Printf.sprintf "%02d:%02d" t.hour t.min

let repetition_to_string (kind, d) =
  let kind = repetition_kind_to_string kind in
  if d.year <> 0 then Printf.sprintf "%s%dy" kind d.year
  else if d.month <> 0 then Printf.sprintf "%s%dm" kind d.month
  else if d.day <> 0 then Printf.sprintf "%s%dd" kind d.day
  else Printf.sprintf "%s0d" kind

let to_string ?(wday = [|"Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Wed"; "Sat"|]) t =
  Printf.sprintf "%c%s%c"
    (if t.active then '<' else '[')
    ( [ Some (date_to_string t.date)
      ; Some wday.(weekday t)
      ; Option.map time_to_string t.time
      ; Option.map repetition_to_string t.repetition ]
      |> filter_map identity |> String.concat " " )
    (if t.active then '>' else ']')

let range_to_string {start; stop} =
  Printf.sprintf "%s--%s" (to_string start) (to_string stop)

let seconds_of_t t = to_tm t |> Unix.mktime |> fst

let duration {start; stop} = truncate (seconds_of_t stop -. seconds_of_t start)

let from_now t =
  duration {start= t; stop= from_tm (Unix.localtime (Unix.time ()))}

let string_of_seconds n =
  let mins, _secs = (n / 60, n mod 60) in
  let hours, mins = (mins / 60, mins mod 60) in
  Printf.sprintf "%02d:%02d" hours mins

let add_days t d = normalize {t with date= {t.date with day= t.date.day + d}}

let today () = Unix.time () |> Unix.localtime |> from_tm

let sub d d' =
  {year= d'.year - d.year; month= d'.month - d.month; day= d'.day - d.day}

let covers arg source =
  match source.repetition with
  | None -> source.date = arg.date
  | Some (_kind, repetition) ->
    if arg < source then false
    else
      let sub = sub arg.date source.date in
      (repetition.year = 0 || sub.year mod repetition.year = 0)
      && (repetition.month = 0 || sub.month mod repetition.month = 0)
      && (repetition.day = 0 || sub.day mod repetition.day = 0)

let repetition_parser s date time c =
  if s.[1] <> '+' then
    let repetition =
      parse_repetition_marker Plus
        (String.sub s 1 (String.length s - 1))
    in
    (date, time, repetition)
  else
    let kind = if c = '+' then DoublePlus else Dotted in
    let repetition =
      parse_repetition_marker kind
        (String.sub s 2 (String.length s - 2))
    in
    (date, time, repetition)
