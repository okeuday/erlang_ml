(*-*-Mode:ocaml;coding:utf-8;tab-width:2;c-basic-offset:2;indent-tabs-mode:()-*-
  ex: set ft=ocaml fenc=utf-8 sts=2 ts=2 sw=2 et nomod: *)

(*
 
  BSD LICENSE
  
  Copyright (c) 2017, Michael Truog <mjtruog at gmail dot com>
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
  
      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in
        the documentation and/or other materials provided with the
        distribution.
      * All advertising materials mentioning features or use of this
        software must display the following acknowledgment:
          This product includes software developed by Michael Truog
      * The name of the author may not be used to endorse or promote
        products derived from this software without specific prior
        written permission
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
  DAMAGE.
 
 *)

(* tag values here http://www.erlang.org/doc/apps/erts/erl_ext_dist.html *)
let tag_version = 131
let tag_compressed_zlib = 80
let tag_new_float_ext = 70
let tag_bit_binary_ext = 77
let tag_atom_cache_ref = 78
let tag_small_integer_ext = 97
let tag_integer_ext = 98
let tag_float_ext = 99
let tag_atom_ext = 100
let tag_reference_ext = 101
let tag_port_ext = 102
let tag_pid_ext = 103
let tag_small_tuple_ext = 104
let tag_large_tuple_ext = 105
let tag_nil_ext = 106
let tag_string_ext = 107
let tag_list_ext = 108
let tag_binary_ext = 109
let tag_small_big_ext = 110
let tag_large_big_ext = 111
let tag_new_fun_ext = 112
let tag_export_ext = 113
let tag_new_reference_ext = 114
let tag_small_atom_ext = 115
let tag_map_ext = 116
let tag_fun_ext = 117
let tag_atom_utf8_ext = 118
let tag_small_atom_utf8_ext = 119

module Pid = struct
  type t = {
      node_tag : int;
      node : string;
      id : string;
      serial : string;
      creation : int;
    }
  let make ~node_tag ~node ~id ~serial ~creation =
    {node_tag; node; id; serial; creation}
end
module Port = struct
  type t = {
      node_tag : int;
      node : string;
      id : string;
      creation : int;
    }
  let make ~node_tag ~node ~id ~creation =
    {node_tag; node; id; creation}
end
module Reference = struct
  type t = {
      node_tag : int;
      node : string;
      id : string;
      creation : int;
    }
  let make ~node_tag ~node ~id ~creation =
    {node_tag; node; id; creation}
end
module Function = struct
  type t = {
      tag : int;
      value : string;
    }
  let make ~tag ~value =
    {tag; value}
end
type t =
    OtpErlangIntegerSmall of int
  | OtpErlangIntegerLarge of Big_int.big_int
  | OtpErlangFloat of float
  | OtpErlangAtom of string
  | OtpErlangAtomUTF8 of string
  | OtpErlangAtomCacheRef of int
  | OtpErlangAtomBool of bool
  | OtpErlangString of string
  | OtpErlangBinary of string
  | OtpErlangBinaryBits of string * int
  | OtpErlangList of t list
  | OtpErlangListImproper of t list
  | OtpErlangTuple of t list
  | OtpErlangMap of (t, t) Hashtbl.t
  | OtpErlangPid of Pid.t
  | OtpErlangPort of Port.t
  | OtpErlangReference of Reference.t
  | OtpErlangFunction of Function.t

let input_error (s : string) : string =
  "input_error: " ^ s
(*
let output_error (s : string) : string =
  "output_error: " ^ s
*)
let parse_error (s : string) : string =
  "parse_error: " ^ s

type ('a,'b,'c) result2 = Ok2 of 'a * 'b | Error2 of 'c
type ('a,'b,'c,'d) result3 = Ok3 of 'a * 'b * 'c | Error3 of 'd

let list_append l1 l2 = List.rev_append (List.rev l1) l2

let unpack_uint16 i binary : int =
let byte0 = int_of_char binary.[i]
and byte1 = int_of_char binary.[i + 1] in
  ((byte0 lsl 8) lor byte1)

let unpack_uint32 i binary : (int, int, string) result2 =
let byte0 = int_of_char binary.[i]
and byte1 = int_of_char binary.[i + 1]
and byte2 = int_of_char binary.[i + 2]
and byte3 = int_of_char binary.[i + 3] in
if byte0 > max_int lsr 24 then
  Error2 (parse_error "ocaml int overflow")
else
  Ok2 (i + 4,
    (byte0 lsl 24) lor (
      (byte1 lsl 16) lor (
        (byte2 lsl 8) lor byte3
      )
    )
  )

let unpack_integer i binary : t =
let byte0 = int_of_char binary.[i]
and byte1 = int_of_char binary.[i + 1]
and byte2 = int_of_char binary.[i + 2]
and byte3 = int_of_char binary.[i + 3] in
if byte0 > min_int lsr 24 then
  OtpErlangIntegerLarge (Big_int.big_int_of_int32 (
    Int32.logor (
      Int32.shift_left (Int32.of_int byte0) 24
    ) (
      Int32.logor (
        Int32.shift_left (Int32.of_int byte1) 16
      ) (
        Int32.logor (
          Int32.shift_left (Int32.of_int byte2) 8
        ) (
          Int32.of_int byte3
        )
      )
    )
  ))
else if (byte0 lsr 7) = 1 then
  OtpErlangIntegerSmall (
    -2147483648 + (
      ((byte0 land 0x7f) lsl 24) lor (
        (byte1 lsl 16) lor (
          (byte2 lsl 8) lor byte3
        )
      )
    )
  )
else
  OtpErlangIntegerSmall (
    (byte0 lsl 24) lor (
      (byte1 lsl 16) lor (
        (byte2 lsl 8) lor byte3
      )
    )
  )

let unpack_double i binary : float =
let byte0 = Int64.of_int (int_of_char binary.[i])
and byte1 = Int64.of_int (int_of_char binary.[i + 1])
and byte2 = Int64.of_int (int_of_char binary.[i + 2])
and byte3 = Int64.of_int (int_of_char binary.[i + 3])
and byte4 = Int64.of_int (int_of_char binary.[i + 4])
and byte5 = Int64.of_int (int_of_char binary.[i + 5])
and byte6 = Int64.of_int (int_of_char binary.[i + 6])
and byte7 = Int64.of_int (int_of_char binary.[i + 7]) in
Int64.float_of_bits (
  Int64.logor (
    Int64.logor (
      Int64.logor (
        Int64.logor (
          Int64.logor (
            Int64.logor (
              Int64.logor (
                Int64.shift_left byte0 56
              ) (
                Int64.shift_left byte1 48
              )
            ) (
              Int64.shift_left byte2 40
            )
          ) (
            Int64.shift_left byte3 32
          )
        ) (
          Int64.shift_left byte4 24
        )
      ) (
        Int64.shift_left byte5 16
      )
    ) (
      Int64.shift_left byte6 8
    )
  ) byte7
)

let rec binary_to_term_ i binary : (int, t, string) result2 =
  let tag = int_of_char binary.[i]
  and i0 = i + 1 in
  if tag = tag_new_float_ext then
    Ok2 (i0 + 8, OtpErlangFloat (unpack_double i0 binary))
  else if tag = tag_bit_binary_ext then
    match unpack_uint32 i0 binary with
    | Error2 (error) ->
      Error2 (error)
    | Ok2 (_, j) when j > Sys.max_string_length ->
      Error2 (parse_error "ocaml string overflow")
    | Ok2 (i1, j) ->
      let bits = int_of_char binary.[i1]
      and i2 = i1 + 1 in
      let data = String.sub binary i2 j in
      let term : t = if bits = 8 then
        OtpErlangBinary (data)
      else
        OtpErlangBinaryBits ((data, bits))
      in
      Ok2(i2 + j, term)
  else if tag = tag_atom_cache_ref then
    Ok2 (i0 + 1, OtpErlangAtomCacheRef (int_of_char binary.[i0]))
  else if tag = tag_small_integer_ext then
    Ok2 (i0 + 1, OtpErlangIntegerSmall (int_of_char binary.[i0]))
  else if tag = tag_integer_ext then
    Ok2 (i0 + 4, unpack_integer i0 binary)
  else if tag = tag_float_ext then
    let data = String.sub binary i0 31 in
    let float_data = try (String.sub data 0 (String.index data '\000'))
    with Not_found -> data in
    Ok2 (i0 + 31, OtpErlangFloat (float_of_string float_data))
  else if tag = tag_atom_ext then
    let j = unpack_uint16 i0 binary
    and i1 = i0 + 2 in
    Ok2 (i1 + j, OtpErlangAtom (String.sub binary i1 j))
  else if tag = tag_reference_ext || tag = tag_port_ext then
    match binary_to_atom i0 binary with
    | Error3 (error) ->
      Error2 (error)
    | Ok3(i1, node_tag, node) ->
      let id = String.sub binary i1 4
      and i2 = i1 + 4 in
      let creation = int_of_char binary.[i2]
      and i3 = i2 + 1 in
      if tag = tag_reference_ext then
        Ok2 (i3, OtpErlangReference (
          Reference.make
            ~node_tag:node_tag ~node:node ~id:id ~creation:creation))
      else (* tag = tag_port_ext *)
        Ok2 (i3, OtpErlangPort (
          Port.make
            ~node_tag:node_tag ~node:node ~id:id ~creation:creation))
  else if tag = tag_pid_ext then
    match binary_to_atom i0 binary with
    | Error3 (error) ->
      Error2 (error)
    | Ok3(i1, node_tag, node) ->
      let id = String.sub binary i1 4
      and i2 = i1 + 4 in
      let serial = String.sub binary i2 4
      and i3 = i2 + 4 in
      let creation = int_of_char binary.[i3]
      and i4 = i3 + 1 in
      Ok2 (i4, OtpErlangPid (
        Pid.make
          ~node_tag:node_tag ~node:node
          ~id:id ~serial:serial ~creation:creation))
  else if tag = tag_small_tuple_ext then
    let length = int_of_char binary.[i0]
    and i1 = i0 + 1 in
    match binary_to_term_sequence i1 length binary with
    | Error2 (error) ->
      Error2 (error)
    | Ok2 (i2, tmp) ->
      Ok2 (i2, OtpErlangTuple (tmp))
  else if tag = tag_large_tuple_ext then
    match unpack_uint32 i0 binary with
    | Error2 (error) ->
      Error2 error
    | Ok2 (_, length) when length > Sys.max_string_length ->
      Error2 (parse_error "ocaml string overflow")
    | Ok2 (i1, length) ->
      match binary_to_term_sequence i1 length binary with
      | Error2 (error) ->
        Error2 (error)
      | Ok2 (i2, tmp) ->
        Ok2 (i2, OtpErlangTuple (tmp))
  else if tag = tag_nil_ext then
    Ok2 (i0, OtpErlangList ([]))
  else if tag = tag_string_ext then
    let j = unpack_uint16 i0 binary
    and i1 = i0 + 2 in
    Ok2 (i1 + j, OtpErlangString (String.sub binary i1 j))
  else if tag = tag_list_ext then
    match unpack_uint32 i0 binary with
    | Error2 (error) ->
      Error2 error
    | Ok2 (i1, length) ->
      match binary_to_term_sequence i1 length binary with
      | Error2 (error) ->
        Error2 (error)
      | Ok2 (i2, tmp) ->
        match binary_to_term_ i2 binary with
        | Error2 (error) ->
          Error2 (error)
        | Ok2 (i3, tail) when tail = OtpErlangList([]) ->
          Ok2 (i3, OtpErlangList (tmp))
        | Ok2 (i3, tail) ->
          Ok2 (i3, OtpErlangListImproper (list_append tmp [tail]))
  else if tag = tag_binary_ext then
    match unpack_uint32 i0 binary with
    | Error2 (error) ->
      Error2 error
    | Ok2 (_, j) when j > Sys.max_string_length ->
      Error2 (parse_error "ocaml string overflow")
    | Ok2 (i1, j) ->
      Ok2 (i1 + j, OtpErlangBinary (String.sub binary i1 j))
  else if tag = tag_small_big_ext || tag = tag_large_big_ext then
    let length_f () =
      if tag = tag_small_big_ext then
        Ok2 (i0 + 1, int_of_char binary.[i0])
      else (* tag = tag_large_big_ext *)
        unpack_uint32 i0 binary
    in
    match length_f () with
    | Error2 (error) ->
      Error2 (error)
    | Ok2 (_, j) when j > Sys.max_string_length ->
      Error2 (parse_error "ocaml string overflow")
    | Ok2 (i1, j) ->
      let sign = int_of_char binary.[i1] in
      let rec loop bignum_index bignum_value =
        if bignum_index = j then
          bignum_value
        else
          loop
            (bignum_index + 1)
            (Big_int.add_int_big_int
              (int_of_char binary.[i1 + j - bignum_index])
              (Big_int.mult_int_big_int 256 bignum_value))
      in
      let bignum = loop 0 Big_int.zero_big_int in
      let bignum_result = if sign = 1 then
        Big_int.minus_big_int bignum
      else
        bignum
      and i2 = i1 + 1 in
      Ok2 (i2 + j, OtpErlangIntegerLarge (bignum_result))
  else if tag = tag_new_fun_ext then
    match unpack_uint32 i0 binary with
    | Error2 (error) ->
      Error2 (error)
    | Ok2 (i1, length) ->
      let value = String.sub binary i1 length in
      Ok2 (i1 + length, OtpErlangFunction (
        Function.make
          ~tag:tag ~value:value))
  else if tag = tag_export_ext then
    match binary_to_atom i0 binary with
    | Error3 (error) ->
      Error2 (error)
    | Ok3(i1, _, _) ->
      match binary_to_atom i1 binary with
      | Error3 (error) ->
        Error2 (error)
      | Ok3 (i2, _, _) when int_of_char binary.[i2] <> tag_small_integer_ext ->
        Error2 (parse_error "invalid small integer tag")
      | Ok3 (i2, _, _) ->
        let i3 = i2 + 2 in
        let value = String.sub binary i0 (i3 - i0 + 1) in
        Ok2 (i3, OtpErlangFunction (
          Function.make
            ~tag:tag ~value:value))
  else if tag = tag_new_reference_ext then
    let j = (unpack_uint16 i0 binary) * 4
    and i1 = i0 + 2 in
    match binary_to_atom i1 binary with
    | Error3 (error) ->
      Error2 (error)
    | Ok3 (i2, node_tag, node) ->
      let creation = int_of_char binary.[i2]
      and i3 = i2 + 1 in
      let id = String.sub binary i3 j in
      Ok2 (i3 + j, OtpErlangReference(
        Reference.make
          ~node_tag:node_tag ~node:node ~id:id ~creation:creation))
  else if tag = tag_small_atom_ext then
    let j = int_of_char binary.[i0]
    and i1 = i0 + 1 in
    let atom_name = String.sub binary i1 j in
    if atom_name = "true" then
      Ok2 (i1 + j, OtpErlangAtomBool (true))
    else if atom_name = "false" then
      Ok2 (i1 + j, OtpErlangAtomBool (false))
    else
      Ok2 (i1 + j, OtpErlangAtom (atom_name))
  else if tag = tag_map_ext then
    match unpack_uint32 i0 binary with
    | Error2 (error) ->
      Error2 (error)
    | Ok2 (i1, length) ->
      let pairs = Hashtbl.create length in
      let rec loop length_index i2 =
        if length_index = length then
          Ok2 (i2, OtpErlangMap (pairs))
        else
          match binary_to_term_ i2 binary with
          | Error2 (error) ->
            Error2 (error)
          | Ok2 (i3, key) ->
            match binary_to_term_ i3 binary with
            | Error2 (error) ->
              Error2 (error)
            | Ok2 (i4, value) ->
              Hashtbl.add pairs key value ;
              loop (length_index + 1) i4
      in
      loop 0 i1
  else if tag = tag_fun_ext then
    match unpack_uint32 i0 binary with
    | Error2 (error) ->
      Error2 (error)
    | Ok2 (i1, numfree) ->
      match binary_to_pid i1 binary with
      | Error2 (error) ->
        Error2 (error)
      | Ok2 (i2, _) -> (* pid *)
        match binary_to_atom i2 binary with
        | Error3 (error) ->
          Error2 (error)
        | Ok3 (i3, _, _) -> (* module *)
          match binary_to_integer i3 binary with
          | Error2 (error) ->
            Error2 (error)
          | Ok2 (i4, _) -> (* index *)
            match binary_to_integer i4 binary with
            | Error2 (error) ->
              Error2 (error)
            | Ok2 (i5, _) -> (* uniq *)
              match binary_to_term_sequence i5 numfree binary with
              | Error2 (error) ->
                Error2 (error)
              | Ok2 (i6, _) -> (* free *)
                let value = String.sub binary i0 (i6 - i0 + 1) in
                Ok2 (i6, OtpErlangFunction (
                  Function.make
                    ~tag:tag ~value:value))
  else if tag = tag_atom_utf8_ext then
    let j = unpack_uint16 i0 binary
    and i1 = i0 + 2 in
    Ok2 (i1 + j, OtpErlangAtomUTF8 (String.sub binary i1 j))
  else if tag = tag_small_atom_utf8_ext then
    let j = int_of_char binary.[i0]
    and i1 = i0 + 1 in
    Ok2 (i1 + j, OtpErlangAtomUTF8 (String.sub binary i1 j))
  else if tag = tag_compressed_zlib then
    Error2 (parse_error "ocaml doesn't provide zlib")
  else
    Error2 (parse_error "invalid tag")

and binary_to_term_sequence i length binary : (int, t list, string) result2 =
  let rec loop length_index i0 sequence =
    if length_index = length then
      Ok2 (i0, sequence)
    else
      match binary_to_term_ i0 binary with
      | Error2 (error) ->
        Error2 (error)
      | Ok2 (i1, element) ->
        loop (length_index + 1) i1 (list_append sequence [element])
  in
  loop 0 i []

and binary_to_integer i binary : (int, t, string) result2 =
  let tag = int_of_char binary.[i]
  and i0 = i + 1 in
  if tag = tag_small_integer_ext then
    Ok2 (i0 + 1, OtpErlangIntegerSmall (int_of_char binary.[i0]))
  else if tag = tag_integer_ext then
    Ok2 (i0 + 4, unpack_integer i0 binary)
  else
    Error2 (parse_error "invalid integer tag")

and binary_to_pid i binary : (int, t, string) result2 =
  let tag = int_of_char binary.[i]
  and i0 = i + 1 in
  if tag = tag_pid_ext then
    match binary_to_atom i0 binary with
    | Error3 (error) ->
      Error2 (error)
    | Ok3(i1, node_tag, node) ->
      let id = String.sub binary i1 4
      and i2 = i1 + 4 in
      let serial = String.sub binary i2 4
      and i3 = i2 + 4 in
      let creation = int_of_char binary.[i3]
      and i4 = i3 + 1 in
      Ok2 (i4, OtpErlangPid (
        Pid.make
          ~node_tag:node_tag ~node:node
          ~id:id ~serial:serial ~creation:creation))
  else
    Error2 (parse_error "invalid pid tag")

and binary_to_atom i binary : (int, int, string, string) result3 =
  let tag = int_of_char binary.[i]
  and i0 = i + 1 in
  if tag = tag_atom_ext then
    let j = unpack_uint16 i0 binary
    and i1 = i0 + 2 in
    Ok3 (i1 + j, tag, String.sub binary i1 j)
  else if tag = tag_atom_cache_ref then
    Ok3 (i0 + 1, tag, String.make 1 binary.[i0])
  else if tag = tag_small_atom_ext then
    let j = int_of_char binary.[i]
    and i1 = i0 + 1 in
    Ok3 (i1 + j, tag, String.sub binary i1 j)
  else if tag = tag_atom_utf8_ext then
    let j = unpack_uint16 i0 binary
    and i1 = i0 + 2 in
    Ok3 (i1 + j, tag, String.sub binary i1 j)
  else if tag = tag_small_atom_utf8_ext then
    let j = int_of_char binary.[i]
    and i1 = i0 + 1 in
    Ok3 (i1 + j, tag, String.sub binary i1 j)
  else
    Error3 (parse_error "invalid atom tag")

let binary_to_term (binary : string) : (t, string) result =
  let size = String.length binary in
  if size <= 1 then
    Error (parse_error "null input")
  else if int_of_char binary.[0] <> tag_version then
    Error (parse_error "invalid version")
  else
    try
      match binary_to_term_ 1 binary with
      | Error2 error ->
        Error error
      | Ok2 (i, _) when i <> size ->
        Error (parse_error "unparsed data")
      | Ok2 (_, term) ->
        Ok term
    with
      Invalid_argument(_) ->
        Error (parse_error "missing data")

let term_to_binary (_ : t) : (string, string) result =
  Error (input_error "invalid")

let rec t_to_string (term : t) : string =
  match term with
  | OtpErlangIntegerSmall (value) ->
    "OtpErlangIntegerSmall(" ^ (string_of_int value) ^ ")"
  | OtpErlangIntegerLarge (value) ->
    "OtpErlangIntegerLarge(" ^ (Big_int.string_of_big_int value) ^ ")"
  | OtpErlangFloat (value) ->
    "OtpErlangFloat(" ^ (string_of_float value) ^ ")"
  | OtpErlangAtom (value) ->
    "OtpErlangAtom('" ^ value ^ "')"
  | OtpErlangAtomUTF8 (value) ->
    "OtpErlangAtomUTF8('" ^ value ^ "')"
  | OtpErlangAtomCacheRef (value) ->
    "OtpErlangAtomUTF8('atom(" ^ (string_of_int value) ^ ")')"
  | OtpErlangAtomBool (value) ->
    if value then
      "OtpErlangAtomBool('true')"
    else
      "OtpErlangAtomBool('false')"
  | OtpErlangString (value) ->
    "OtpErlangString(\"" ^ value ^ "\")"
  | OtpErlangBinary (value) ->
    "OtpErlangBinary(\"" ^ value ^ "\")"
  | OtpErlangBinaryBits (value, bits) ->
    "OtpErlangBinaryBits(\"" ^ value ^ "\"," ^ (string_of_int bits) ^ ")"
  | OtpErlangList value ->
    "OtpErlangList(" ^ (sequence_to_string value) ^ ")"
  | OtpErlangListImproper value ->
    "OtpErlangListImproper(" ^ (sequence_to_string value) ^ ")"
  | OtpErlangTuple value ->
    "OtpErlangTuple(" ^ (sequence_to_string value) ^ ")"
  | OtpErlangMap value ->
    "OtpErlangMap(" ^ (map_to_string value) ^ ")"
  | OtpErlangPid _ ->
    "OtpErlangPid()"
  | OtpErlangPort _ ->
    "OtpErlangPort()"
  | OtpErlangReference _ ->
    "OtpErlangReference()"
  | OtpErlangFunction _ ->
    "OtpErlangFunction()"

and sequence_to_string (terms : t list) : string =
  let buffer = Buffer.create (32 * (List.length terms)) in
  List.iter (fun term ->
    if (Buffer.length buffer) = 0 then
      Buffer.add_string buffer "[" 
    else
      Buffer.add_string buffer "," ;
    Buffer.add_string buffer (t_to_string term)) terms ;
  Buffer.add_string buffer "]" ;
  Buffer.contents buffer

and map_to_string (terms : (t, t) Hashtbl.t) : string =
  let buffer = Buffer.create (32 * (Hashtbl.length terms)) in
  Hashtbl.iter (fun key value ->
    if (Buffer.length buffer) = 0 then
      Buffer.add_string buffer "{" 
    else
      Buffer.add_string buffer "," ;
    Buffer.add_string buffer (t_to_string key) ;
    Buffer.add_string buffer " => " ;
    Buffer.add_string buffer (t_to_string value) ;
    Buffer.add_string buffer ",") terms ;
  Buffer.add_string buffer "}" ;
  Buffer.contents buffer

exception TermOk of string ;;
let term_ok (value : (t, string) result) : t =
  match value with
  | Ok (term) ->
      term
  | Error (error) ->
      raise (TermOk error)

exception TermError of string ;;
let term_error (value : (t, string) result) : string =
  match value with
  | Ok (term) ->
      raise (TermError (t_to_string term))
  | Error (error) ->
      error

(*

  BSD LICENSE (of tests below)
  
  Copyright (c) 2017, Michael Truog <mjtruog at gmail dot com>
  Copyright (c) 2009-2013, Dmitry Vasiliev <dima@hlabs.org>
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
  
      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in
        the documentation and/or other materials provided with the
        distribution.
      * All advertising materials mentioning features or use of this
        software must display the following acknowledgment:
          This product includes software developed by Michael Truog
      * The name of the author may not be used to endorse or promote
        products derived from this software without specific prior
        written permission
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
  DAMAGE.
 
 *)

let test_decode_basic () =
  assert (
    (term_error (binary_to_term "")) =
    "parse_error: null input") ;
  assert (
    (term_error (binary_to_term "\x83")) =
    "parse_error: null input") ;
  assert (
    (term_error (binary_to_term "\x83z")) =
    "parse_error: invalid tag") ;
  true

let test_decode_atom () =
  assert (
    (term_error (binary_to_term "\x83d")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83d\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83d\x00\x01")) =
    "parse_error: missing data") ;
  assert (
    (term_ok (binary_to_term "\x83d\x00\x00")) =
    OtpErlangAtom ("")) ;
  assert (
    (term_ok (binary_to_term "\x83s\x00")) =
    OtpErlangAtom ("")) ;
  assert (
    (term_ok (binary_to_term "\x83d\x00\x04test")) =
    OtpErlangAtom ("test")) ;
  assert (
    (term_ok (binary_to_term "\x83s\x04test")) =
    OtpErlangAtom ("test")) ;
  true

let test_decode_predefined_atom () =
  assert (
    (term_ok (binary_to_term "\x83s\x04true")) =
    OtpErlangAtomBool (true)) ;
  assert (
    (term_ok (binary_to_term "\x83s\x05false")) =
    OtpErlangAtomBool (false)) ;
  assert (
    (term_ok (binary_to_term "\x83s\x09undefined")) =
    OtpErlangAtom ("undefined")) ;
  true

let test_decode_empty_list () =
  assert (
    (term_ok (binary_to_term "\x83j")) =
    OtpErlangList ([])) ;
  true

let test_decode_string_list () =
  assert (
    (term_error (binary_to_term "\x83k")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83k\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83k\x00\x01")) =
    "parse_error: missing data") ;
  assert (
    (term_ok (binary_to_term "\x83k\x00\x00")) =
    OtpErlangString ("")) ;
  assert (
    (term_ok (binary_to_term "\x83k\x00\x04test")) =
    OtpErlangString ("test")) ;
  true

let test_decode_list () =
  assert (
    (term_error (binary_to_term "\x83l")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83l\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83l\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83l\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83l\x00\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_ok (binary_to_term "\x83l\x00\x00\x00\x00j")) =
    OtpErlangList ([])) ;
  assert (
    (term_ok (binary_to_term "\x83l\x00\x00\x00\x02jjj")) =
    OtpErlangList ([OtpErlangList ([]); OtpErlangList ([])])) ;
  true

let test_decode_improper_list () =
  assert (
    (term_error (binary_to_term "\x83l\x00\x00\x00\x00k")) =
    "parse_error: missing data") ;
  let lst = term_ok (binary_to_term "\x83l\x00\x00\x00\x01jd\x00\x04tail") in
  assert (
    lst =
    OtpErlangListImproper ([OtpErlangList ([]); OtpErlangAtom ("tail")])) ;
  true

let test_decode_small_tuple () =
  assert (
    (term_error (binary_to_term "\x83h")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83h\x01")) =
    "parse_error: missing data") ;
  assert (
    (term_ok (binary_to_term "\x83h\x00")) =
    OtpErlangTuple ([])) ;
  assert (
    (term_ok (binary_to_term "\x83h\x02jj")) =
    OtpErlangTuple ([OtpErlangList ([]); OtpErlangList ([])])) ;
  true

let test_decode_large_tuple () =
  assert (
    (term_error (binary_to_term "\x83i")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83i\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83i\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83i\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83i\x00\x00\x00\x01")) =
    "parse_error: missing data") ;
  assert (
    (term_ok (binary_to_term "\x83i\x00\x00\x00\x00")) =
    OtpErlangTuple ([])) ;
  assert (
    (term_ok (binary_to_term "\x83i\x00\x00\x00\x02jj")) =
    OtpErlangTuple ([OtpErlangList ([]); OtpErlangList ([])])) ;
  true

let test_decode_small_integer () =
  assert (
    (term_error (binary_to_term "\x83a")) =
    "parse_error: missing data") ;
  assert (
    (term_ok (binary_to_term "\x83a\x00")) =
    OtpErlangIntegerSmall (0)) ;
  assert (
    (term_ok (binary_to_term "\x83a\xff")) =
    OtpErlangIntegerSmall (255)) ;
  true

let test_decode_integer () =
  assert (
    (term_error (binary_to_term "\x83b")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83b\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83b\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83b\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_ok (binary_to_term "\x83b\x00\x00\x00\x00")) =
    OtpErlangIntegerSmall (0)) ;
  assert (
    (term_ok (binary_to_term "\x83b\x7f\xff\xff\xff")) =
    OtpErlangIntegerSmall (Int32.to_int Int32.max_int)) ;
  assert (
    (term_ok (binary_to_term "\x83b\x80\x00\x00\x00")) =
    OtpErlangIntegerSmall (Int32.to_int Int32.min_int)) ;
  assert (
    (term_ok (binary_to_term "\x83b\xff\xff\xff\xff")) =
    OtpErlangIntegerSmall (-1)) ;
  let max_int_plus_1 = OtpErlangIntegerLarge (
    Big_int.big_int_of_string "4611686018427387904")
  and max_int_plus_1_check = term_ok
    (binary_to_term "\x83n\x08\x00\x00\x00\x00\x00\x00\x00\x00@") in
  assert (
    (t_to_string max_int_plus_1) =
    (t_to_string max_int_plus_1_check)) ;
  true

let test_decode_binary () =
  assert (
    (term_error (binary_to_term "\x83m")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83m\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83m\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83m\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83m\x00\x00\x00\x01")) =
    "parse_error: missing data") ;
  assert (
    (term_ok (binary_to_term "\x83m\x00\x00\x00\x00")) =
    OtpErlangBinary ("")) ;
  assert (
    (term_ok (binary_to_term "\x83m\x00\x00\x00\x04data")) =
    OtpErlangBinary ("data")) ;
  true

let test_decode_float () =
  assert (
    (term_error (binary_to_term "\x83F")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83F\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83F\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83F\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83F\x00\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83F\x00\x00\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83F\x00\x00\x00\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83F\x00\x00\x00\x00\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_ok (binary_to_term "\x83F\x00\x00\x00\x00\x00\x00\x00\x00")) =
    OtpErlangFloat (0.0)) ;
  assert (
    (term_ok (binary_to_term "\x83F?\xf8\x00\x00\x00\x00\x00\x00")) =
    OtpErlangFloat (1.5)) ;
  true

let test_decode_small_big_integer () =
  assert (
    (term_error (binary_to_term "\x83n")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83n\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83n\x01\x00")) =
    "parse_error: missing data") ;
  let zero = OtpErlangIntegerLarge (Big_int.zero_big_int)
  and zero_check = term_ok
    (binary_to_term "\x83n\x00\x00") in
  assert (
    (t_to_string zero) =
    (t_to_string zero_check)) ;
  let bigint1 = OtpErlangIntegerLarge (
    Big_int.big_int_of_string "6618611909121")
  and bigint1_check = term_ok
    (binary_to_term "\x83n\x06\x00\x01\x02\x03\x04\x05\x06") in
  assert (
    (t_to_string bigint1) =
    (t_to_string bigint1_check)) ;
  let bigint2 = OtpErlangIntegerLarge (
    Big_int.big_int_of_string "-6618611909121")
  and bigint2_check = term_ok
    (binary_to_term "\x83n\x06\x01\x01\x02\x03\x04\x05\x06") in
  assert (
    (t_to_string bigint2) =
    (t_to_string bigint2_check)) ;
  true

let test_decode_large_big_integer () =
  assert (
    (term_error (binary_to_term "\x83o")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83o\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83o\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83o\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83o\x00\x00\x00\x00")) =
    "parse_error: missing data") ;
  assert (
    (term_error (binary_to_term "\x83o\x00\x00\x00\x01\x00")) =
    "parse_error: missing data") ;
  let zero = OtpErlangIntegerLarge (Big_int.zero_big_int)
  and zero_check = term_ok
    (binary_to_term "\x83o\x00\x00\x00\x00\x00") in
  assert (
    (t_to_string zero) =
    (t_to_string zero_check)) ;
  let bigint1 = OtpErlangIntegerLarge (
    Big_int.big_int_of_string "6618611909121")
  and bigint1_check = term_ok
    (binary_to_term "\x83o\x00\x00\x00\x06\x00\x01\x02\x03\x04\x05\x06") in
  assert (
    (t_to_string bigint1) =
    (t_to_string bigint1_check)) ;
  let bigint2 = OtpErlangIntegerLarge (
    Big_int.big_int_of_string "-6618611909121")
  and bigint2_check = term_ok
    (binary_to_term "\x83o\x00\x00\x00\x06\x01\x01\x02\x03\x04\x05\x06") in
  assert (
    (t_to_string bigint2) =
    (t_to_string bigint2_check)) ;
  true

let tests =
  Printexc.register_printer (function
    | TermOk e ->
        Some ("term_ok " ^ e)
    | TermError e ->
        Some ("term_error " ^ e)
    | _ ->
        None
  ) ;
[
  "binary_to_term (basic)", test_decode_basic;
  "binary_to_term (atom)", test_decode_atom;
  "binary_to_term (predefined atom)", test_decode_predefined_atom;
  "binary_to_term (empty list)", test_decode_empty_list;
  "binary_to_term (string list)", test_decode_string_list;
  "binary_to_term (list)", test_decode_list;
  "binary_to_term (improper list)", test_decode_improper_list;
  "binary_to_term (small tuple)", test_decode_small_tuple;
  "binary_to_term (large tuple)", test_decode_large_tuple;
  "binary_to_term (small integer)", test_decode_small_integer;
  "binary_to_term (integer, 64bit-only)", test_decode_integer;
  "binary_to_term (binary)", test_decode_binary;
  "binary_to_term (float)", test_decode_float;
  "binary_to_term (small bigint)", test_decode_small_big_integer;
  "binary_to_term (large bigint)", test_decode_large_big_integer;
]

