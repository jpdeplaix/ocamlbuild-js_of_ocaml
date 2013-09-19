(*
Copyright (c) 2013 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

open Ocamlbuild_plugin
module Pack = Ocamlbuild_pack

let fold f =
  let l = ref [] in
  (try while true do l @:= [f ()] done with _ -> ());
  !l

let fold_pflag scan =
  List.fold_left (fun acc x -> try scan x (fun x -> x) :: acc with _ -> acc) []

let ocamlfind cmd f =
  let p = Printf.sprintf in
  let cmd = List.map (p "\"%s\"") cmd in
  let cmd = p "ocamlfind query %s" (String.concat " " cmd) in
  Pack.My_unix.run_and_open cmd (fun ic -> fold (fun () -> f ic))

let link_opts prod =
    let (all_pkgs, predicates) =
      let tags = Tags.elements (tags_of_pathname prod) in
      let pkgs = fold_pflag (fun x -> Scanf.sscanf x "package(%[^)])") tags in
      let predicates = fold_pflag (fun x -> Scanf.sscanf x "predicate(%[^)])") tags in
      ("js_of_ocaml" :: pkgs, predicates)
    in

    (* Findlib usualy set pkg_* predicate for all selected packages *)
    (* It doesn't do it with 'query' command, we have to it manualy. *)
    let cmd = "-format" :: "pkg_%p" :: "-r" :: all_pkgs in
    let predicates_pkgs = ocamlfind cmd (fun ic -> input_line ic) in

    let all_predicates = String.concat "," ("javascript" :: predicates @ predicates_pkgs) in

    (* query findlib for linking option *)
    let cmd = "-o-format" :: "-r" :: "-predicates" :: all_predicates :: all_pkgs in
    ocamlfind cmd (fun ic -> A (input_line ic))

let () =
  let dep = "%.byte" in
  let prod = "%.js" in
  let f env _ =
    let dep = env dep in
    let prod = env prod in
    let link_opts = link_opts prod in
    let tags = tags_of_pathname dep ++ "js_of_ocaml" in
    Cmd (S [A "js_of_ocaml"; T tags; S link_opts; P dep; A "-o"; Px prod])
  in
  rule "js_of_ocaml: .byte -> .js" ~dep ~prod f

let () =
  flag ["js_of_ocaml"; "debug"] (S [A "-pretty"; A "-debuginfo"; A "-noinline"]);
  pflag ["js_of_ocaml"] "opt" (fun n -> S [A "-opt"; A n])
