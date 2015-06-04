(* Random Tests for AIG *)

module A = QCheck.Arbitrary

(* specification: trivial boolean formulas *)
module Bool = struct
  type 'a view =
    | True
    | Var of int
    | Not of 'a
    | And of 'a * 'a
    | Or of 'a * 'a

  (* simple, stupid boolean expressions *)
  type t = { view: t view }

  let make_ view = {view}
  let true_ = make_ True
  let neg x = make_ (Not x)
  let false_ = neg true_
  let and_ x y = make_ (And (x,y))
  let or_ x y = make_ (Or (x,y))
  let var v = make_ (Var v)

  let or_list = List.fold_left or_ false_
  let and_list = List.fold_left and_ true_

  let rec eval_fun f t = match t.view with
    | True -> true
    | Var v -> f v
    | Not t -> not (eval_fun f t)
    | And (a,b) -> eval_fun f a && eval_fun f b
    | Or (a,b) -> eval_fun f a || eval_fun f b

  let eval map t = eval_fun (fun v -> AIG.VarMap.find v map) t

  let rec pp out t = match t.view with
    | True -> Format.fprintf out "1"
    | Not t -> Format.fprintf out "@[<hov1>¬@ %a@]" pp_inner t
    | Var v -> Format.fprintf out "v%d" v
    | And (a,b) -> Format.fprintf out "@[<hv2>(and@ %a@ %a)@]" pp_inner a pp_inner b
    | Or (a,b) -> Format.fprintf out "@[<hv2>(or@ %a@ %a)@]" pp_inner a pp_inner b
  and pp_inner out t = match t.view with
    | True
    | Or _
    | And _
    | Var _ -> pp out t
    | Not _ -> Format.fprintf out "(%a)" pp t

  let to_string t = CCFormat.sprintf "%a" pp t

  let vars t =
    let rec vars_rec acc t = match t.view with
      | True -> acc
      | Var v -> AIG.VarSet.add v acc
      | Not a -> vars_rec acc a
      | And (a,b)
      | Or (a,b) ->
        let acc = vars_rec acc a in
        vars_rec acc b
    in
    vars_rec AIG.VarSet.empty t

  (* apply subst to n *)
  let rec subst map n = match n.view with
    | True -> n
    | Var v ->
      begin try
        AIG.VarMap.find v map
      with Not_found ->
        n
      end
    | Not n -> neg (subst map n)
    | And (a,b) -> and_ (subst map a) (subst map b)
    | Or (a,b) -> or_ (subst map a) (subst map b)

  (* build all valuations over [vars] *)
  let all_valuations_ vars =
    AIG.VarSet.fold
      (fun v l ->
        CCList.flat_map
          (fun sigma ->
             [ AIG.VarMap.add v true_ sigma
             ; AIG.VarMap.add v false_ sigma
             ]
          ) l
      ) vars [AIG.VarMap.empty]

  (* quantification over [vars] *)
  let exists vars n =
    let sigma_l = all_valuations_ vars in
    let l = List.map (fun sigma -> subst sigma n) sigma_l in
    or_list l

  let for_all vars n =
    let sigma_l = all_valuations_ vars in
    let l = List.map (fun sigma -> subst sigma n) sigma_l in
    and_list l

  (* convert into an AIG *)
  let rec to_aig man t = match t.view with
    | True -> AIG.true_
    | Or (a, b) -> AIG.or_ ~man (to_aig man a) (to_aig man b)
    | And (a, b) -> AIG.and_ ~man (to_aig man a) (to_aig man b)
    | Not a -> AIG.neg (to_aig man a)
    | Var v -> AIG.var ~man v

  let rec list_range i j = if i=j then [] else i:: (list_range (i+1) j)

  let rand size : t A.t =
    let open A in
    (1 -- 30) >>= fun n_var ->
    (* random vars + true *)
    let base = among ([true_; false_] @ List.map var (list_range 1 n_var)) in
    fix ~max:size ~base
      (fun self ->
         choose
           [ pure neg <*> self
           ; pure and_ <*> self <*> self
           ; pure or_ <*> self <*> self
           ]
      )
end

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

