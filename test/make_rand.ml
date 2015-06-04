
(* build a random AIG *)

module A = QCheck.Arbitrary

let () =
  let files = ref [] in
  let size = ref 40 in
  let add_file = CCList.Ref.push files in
  let opts = Arg.align
      [ "-size", Arg.Set_int size, " set graph size"]
  in
  try
    Arg.parse opts add_file "make_rand file1 file2....";
    files := List.rev !files;
    let graphs = A.generate ~n:(List.length !files) (Bool.rand !size) in
    let pairs = List.combine !files graphs in
    List.iter
      (fun (file, form) ->
        let man = AIG.create () in
        let aig = Bool.to_aig man form in
        Printf.printf "write AIG (size %d) to %s...\n" (AIG.size aig) file;
        AIG.dot_to_file file aig
      ) pairs
  with Arg.Help msg ->
    print_endline msg
