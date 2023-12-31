open Unix
open Sys
open Scanf
open String
open Str

let save i dest =
  let o = open_out dest in
  let buf = Bytes.create 1000000 in
  let real = input i buf 0 (Bytes.length buf) in
  let response = String.sub (Bytes.to_string buf) 0 real in
  let parsed_reals = split (regexp_string "\n\r\n") response in
  let parsed_final = match parsed_reals with [] -> "" | _ :: y -> List.hd y in
  output_string o parsed_final;
  print_endline parsed_final

let get url dest =
  let strings = split_on_char '/' url in
  let parsed_url = match strings with [] -> "" | x :: _ -> x in
  let path =
    match strings with
    | _ :: [] -> ""
    | [ _; _ ] -> ""
    | [] -> ""
    | _ :: x :: y :: _ -> x ^ "/" ^ y
  in
  let addr = (gethostbyname parsed_url).h_addr_list.(0) in
  let request_url =
    "GET " ^ "/" ^ path ^ " HTTP/1.1\r\n" ^ "Host: " ^ parsed_url ^ "\r\n"
    ^ "User-Agent: OCaml\r\nConnection: close\r\nSchema: HTTPS\r\n\r\n"
  in

  let s = socket PF_INET SOCK_STREAM 0 in
  let _ = connect s (ADDR_INET (addr, 80)) in
  let o = out_channel_of_descr s in
  let _ = flush o in
  let i = in_channel_of_descr s in
  let _ =
    output_string o request_url;
    flush o
  in
  try save i dest with _ -> Unix.close s

let () =
  try
    let url =
      try sscanf argv.(1) "http://%s" (fun x -> x)
      with _ -> sscanf argv.(1) "https://%s" (fun x -> x)
    in
    let dest = argv.(2) in
    get url dest
  with _ -> print_endline "Usage: nimm {url (http/https)} {filename}"
