(* Random Tests for AIG *)

module A = QCheck.Arbitrary

(*
let () =
  let cases = A.generate ~n:3 Bool.rand in
  List.iter
    (fun c -> Format.printf "case: %a@." Bool.pp c)
    cases
*)

let pp_valuation out (map:bool AIG.VarMap.t) =
  let items = AIG.VarMap.fold (fun k v acc -> (k,v)::acc) map [] in
  Format.fprintf out "@[<hv1>%a@]"
    (CCFormat.list (CCFormat.pair CCFormat.int CCFormat.bool)) items

(* [(var * bool) list -> VarMap.t] *)
let mk_val
  : (int * bool) list -> bool AIG.VarMap.t
  = fun l -> List.fold_left (fun acc (v,b) -> AIG.VarMap.add v b acc) AIG.VarMap.empty l

let mk_set
  : int list -> AIG.VarSet.t
  = fun l -> List.fold_left (fun set x -> AIG.VarSet.add x set) AIG.VarSet.empty l

(* set of variables -> random valuation *)
let rand_valuation vars : bool AIG.VarMap.t A.t =
  let l = AIG.VarSet.elements vars in
  A.(
    let gens = List.map (fun v -> bool >>= fun b -> return (v, b)) l in
    fun st -> mk_val (List.map (fun g -> g st) gens)
  )

let rand_valuations vars = A.list ~len:A.(1 -- 8) (rand_valuation vars)

(* test that [eval aig f = eval form f] for several valuations [f] *)
let test_eval size =
  let man = AIG.create () in
  (* generates (form, aig(form), random valuations) *)
  let gen = A.(
    Bool.rand size >>= fun form ->
    let vars = Bool.vars form in
    let aig = Bool.to_aig man form in
    rand_valuations vars >>= fun valuations ->
    return (form, aig, valuations)
  )
  in
  let prop (form, aig, valuations) =
    List.for_all
      (fun v -> Bool.eval v form = AIG.eval v aig)
      valuations
  and pp (form, aig, valuations) =
    CCFormat.sprintf "@[<v>form:%a@,aig:%a@,valuations:@[<hv1>%a@]@]"
      Bool.pp form AIG.pp aig (CCFormat.list pp_valuation) valuations
  in
  QCheck.mk_test ~pp ~name:("eval_correct_"^string_of_int size) gen prop

(* pick a subset of vars *)
let pick_vars vars =
  A.(
    let n = AIG.VarSet.cardinal vars in
    if n < 2 then return AIG.VarSet.empty
    else pure mk_set <*> list ~len:(1 -- min n 6) (among (AIG.VarSet.elements vars))
  )

let pp_var out v = Format.fprintf out "v%d" v
let pp_varset out s =
  CCFormat.list pp_var out (AIG.VarSet.elements s)

type quant =
  | Exists
  | Forall

let aig_quant = function
  | Exists -> AIG.exists
  | Forall -> AIG.for_all

let form_quant = function
  | Exists -> Bool.exists
  | Forall -> Bool.for_all

(* test equality of [quant vars form] and [quant vars aig]
   for [vars] subset of vars of [form], where [quant]
   is either [for_all] or [exists] *)
let test_quant ?(n=100) quant size =
  let man = AIG.create () in
  (* generates (form, aig(form), vars, random valuations) *)
  let gen = A.(
    Bool.rand size >>= fun form ->
    let vars = Bool.vars form in
    pick_vars vars >>= fun subvars ->
    rand_valuations vars >>= fun valuations ->
    let aig = Bool.to_aig man form in
    let form' = form_quant quant subvars form in
    let aig' = aig_quant quant ~man subvars aig in
    return (form, aig, form', aig', subvars, valuations)
  )
  in
  let prop (form, aig, form', aig', subvars, valuations) =
    List.for_all
      (fun v -> Bool.eval v form' = AIG.eval v aig')
      valuations
  and pp (form, aig, form', aig', subvars, valuations) =
    CCFormat.sprintf
      "@[<v>form:%a@,aig:%a@,vars:%a@,form':%a@,aig':%a@,valuations:@[<hv1>%a@]@]"
      Bool.pp form AIG.pp aig
      pp_varset subvars
      Bool.pp form' AIG.pp aig'
      (CCFormat.list pp_valuation) valuations
  in
  let name = CCFormat.sprintf "eval_correct_%s_%d"
      (match quant with Forall -> "forall" | Exists -> "exists") size
  in
  QCheck.mk_test ~n ~pp ~name gen prop


let suite =
  [ test_eval 30
  ; test_eval 50
  ; test_quant Forall 10
  ; test_quant Exists 10
  ; test_quant Forall 30
  ; test_quant Forall 50
  ; test_quant Exists 50
  ; test_quant Forall 90 ~n:5
  ]

let () = QCheck.run_main suite

