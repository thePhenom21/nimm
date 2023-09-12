open Unix
open Sys
open Scanf
open String
open Printf

let save buf dest =
  let parsed_dest = open_out dest in
  let buff = Bytes.to_string buf in
  fprintf parsed_dest "%s\n" buff

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
    "GET " ^ "/" ^ path ^ " HTTP/1.1\nHost: " ^ string_of_inet_addr addr
    ^ "\nAccept: application/json\n"
  in
  let s = socket PF_INET SOCK_STREAM 0 in
  let buf = Bytes.create 1000 in
  let _ = connect s (ADDR_INET (addr, 443)) in
  let _ = print_string request_url in
  let _ = send s (Bytes.of_string request_url) 0 0 [] in
  let _ = recv s buf 0 1000 [] in
  let _ = print_string (string_of_inet_addr addr) in
  save buf dest

let () =
  let url = sscanf argv.(1) "https://%s" (fun x -> x) in
  let dest = argv.(2) in
  get url dest
