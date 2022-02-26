(*-*-Mode:ocaml;coding:utf-8;tab-width:2;c-basic-offset:2;indent-tabs-mode:()-*-
  ex: set ft=ocaml fenc=utf-8 sts=2 ts=2 sw=2 et nomod: *)

(*

  MIT License

  Copyright (c) 2017-2022 Michael Truog <mjtruog at protonmail dot com>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

 *)

let str_replace input output =
  Str.global_replace (Str.regexp_string input) output

let backtrace (e : exn) : string =
  let indent = "  " in
  "Exception: " ^ (Printexc.to_string e) ^ "\n" ^ indent ^
  (String.trim (str_replace "\n" ("\n" ^ indent) (Printexc.get_backtrace ())))

let run (test : string * (unit -> bool)) : unit =
  let (name, f) = test in
  try
    if f () then
      print_endline ("PASS  " ^ name)
    else
      print_endline ("FAIL  " ^ name)
  with
    e ->
      prerr_endline (backtrace e) ;
      print_endline ("FAIL! " ^ name)

let () =
  Printexc.record_backtrace true ;
  List.iter run Erlang.tests
