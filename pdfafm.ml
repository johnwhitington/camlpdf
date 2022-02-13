(* Parse Adobe Font Metrics files *)
open Pdfutil

let read_char_metrics_line l =
  match String.split_on_char ' ' l with
  | "C"::charnum::";"::"WX"::width::";"::"N"::name::_ ->
      (name, (int_of_string charnum, int_of_string width))
  | _ -> failwith "badline in read_char_metrics_line"

let lookup_charnum table name =
  match Hashtbl.find table name with (c', _) -> c'

let read_kern_line l =
  match String.split_on_char ' ' l with
  | "KPX"::n::n'::i::_ ->
      n, n', int_of_string (implode (option_map (function '\r' | '\n' -> None | c -> Some c) (explode i)))
  | _ -> failwith "badline in read_kern_line"

let string_starts_with sub s =
  let sublength = String.length sub in
    if String.length s < sublength then false else
      let rec loop n =
        if n < 0 then true
        else s.[n] = sub.[n] && loop (n - 1)
      in
        loop (sublength - 1)

let get_tables lines =
  let char_metrics_lines =
    isolate
      (string_starts_with "StartCharMetrics")
      (string_starts_with "EndCharMetrics")
      lines
  and kern_lines =
    isolate
      (string_starts_with "StartKernPairs")
      (string_starts_with "EndKernPairs")
      lines
  and header_lines =
    map
      (fun s ->
         let a, b = cleavewhile (neq ' ') (explode s) in (implode a, implode b))
      (takewhile (notpred (string_starts_with "C ")) lines)
  in
    let remove_empty = lose (fun x -> String.length x < 5) in
    let charmetrics =
      map read_char_metrics_line (remove_empty char_metrics_lines)
    in
      let charmetrics_hash = hashtable_of_dictionary charmetrics in
      let kerns =
        map read_kern_line (remove_empty kern_lines)
      in
        header_lines,
        option_map
          (fun (_, (c, w)) -> if c > -1 then Some (c, w) else None)
          charmetrics,
        option_map
          (fun (n, n', kern) ->
             let p = lookup_charnum charmetrics_hash n
             and p' = lookup_charnum charmetrics_hash n' in
             if p > -1 && p' > -1 then Some (p, p', kern) else None)
          kerns,
        option_map
        (fun (name, (_, w)) -> Some (name, w))
          charmetrics

let read i =
  try
    let lines = Pdfio.read_lines i in
      get_tables lines
  with
    e -> failwith (Printexc.to_string e)
