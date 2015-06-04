
(*
copyright (c) 2015, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 And-Inverter Graphs} *)

type var = int

(* unsigned node *)
type unode_cell =
  | NTrue
  | NVar of var
  | NAnd of node * node
and node = {
  cell: unode_cell;
  id: int;  (* > 0 iff the node is positive, < 0 if negated *)
  mutable opp: node; (* opposite node *)
}

let id n = n.id

(** Weak set of unodes *)
module Tbl = Weak.Make(struct
  type t = node (* stores positive nodes *)
  let equal n1 n2 = match n1.cell, n2.cell with
    | NTrue, NTrue -> true
    | NVar v1, NVar v2 -> v1==v2
    | NAnd (a1,b1), NAnd (a2,b2) -> a1 == a2 && b1 == b2
    | NTrue, _
    | NVar _, _
    | NAnd _, _ -> false
  let hash n = match n.cell with
    | NTrue -> 42
    | NVar v -> Hashtbl.hash v
    | NAnd (a,b) -> Hashtbl.hash (a.id, b.id)
end)

type manager = {
  tbl : Tbl.t;
  mutable unode_id : int;
}

let create ?(size=1024 * 32) () =
  let man = {
    tbl=Tbl.create size;
    unode_id=2;  (* [0] is invalid, [1] is [true_] *)
  } in
  man

type t = {
  node: node;
  man: manager;
}

let fail_man_ () = invalid_arg "incompatible managers"

let equal a b =
  a.man == b.man && a.node == b.node

let compare a b =
  if a.man != b.man then fail_man_ ();
  Pervasives.compare a.node.id b.node.id

let manager t = t.man

let sign t = t.node.id > 0

let abs t = if t.node.id > 0 then t else {t with node=t.node.opp; }

let neg t = {t with node=t.node.opp}

let apply_sign t sign =
  if sign then abs t else neg (abs t)

let rec bot_ = {cell=NTrue; id=0; opp=bot_}  (* absurd value, do not export *)

let rec mk_true_ = { cell=NTrue; id= 1; opp=mk_false_ }
and mk_false_ = { cell=NTrue; id= ~-1; opp=mk_true_ }

let true_ man = {man; node=mk_true_; }
let false_ man = {man; node=mk_false_; }

let hashcons_ man n =
  let n' = Tbl.merge man.tbl n in
  (* new node *)
  if n==n' then (
    assert (n'.opp == bot_);
    man.unode_id <- man.unode_id + 1;
    n'.opp <- {cell=n'.cell; id= - n'.id; opp=n';}
  );
  n'

let var man v =
  let node = hashcons_ man {cell=NVar v; id=man.unode_id; opp=bot_ } in
  { man; node; }

let mk_neg t = t.opp

(* low level constructor *)
let mk_and_real_ man n1 n2 =
  hashcons_ man {cell=NAnd (n1, n2); id=man.unode_id; opp=bot_ }

(* build [a and b] but enforces some invariants *)
let mk_and ~man a b = match () with
  | () when a==mk_true_ -> b (* true and b --> b *)
  | () when b==mk_true_ -> a (* a and true --> a *)
  | () when a == mk_false_ || b == mk_false_ -> mk_false_ (* false and _ --> false *)
  | () when a == b ->
    (* a and a --> a *)
    a
  | () when a.id == - b.id ->
    (* a and -a --> false *)
    mk_false_
  | () when a.id > b.id ->
    (* smallest node first *)
    mk_and_real_ man b a
  | () ->
    mk_and_real_ man a b

(* [a or b] is [not (not a and not b)] *)
let mk_or ~man a b = mk_neg (mk_and ~man (mk_neg a) (mk_neg b))

(* [a => b] is [not (a and (not b))] *)
let mk_imply ~man a b = mk_neg (mk_and ~man a (mk_neg b))

(* [a equiv b] is [(a => b) and (b => a)] *)
let mk_equiv ~man a b = mk_and ~man (mk_imply ~man a b) (mk_imply ~man b a)

(* [a xor b] is [(a and not b) or (not a and b)] *)
let mk_xor ~man a b = mk_or ~man (mk_and ~man a (mk_neg b)) (mk_and ~man (mk_neg a) b)

let and_ a b =
  if a.man != b.man then fail_man_ ();
  { man=a.man; node=mk_and ~man:a.man a.node b.node; }

let or_ a b =
  if a.man != b.man then fail_man_ ();
  { man=a.man; node=mk_or ~man:a.man a.node b.node; }

let imply a b =
  if a.man != b.man then fail_man_ ();
  { man=a.man; node=mk_imply ~man:a.man a.node b.node; }

let equiv a b =
  if a.man != b.man then fail_man_ ();
  { man=a.man; node=mk_equiv ~man:a.man a.node b.node; }

let xor a b =
  if a.man != b.man then fail_man_ ();
  { man=a.man; node=mk_xor ~man:a.man a.node b.node; }

module Infix = struct
  let (~-) = neg
  let (&&) = and_
  let (||) = or_
  let (==>) = imply
  let (<=>) = equiv
  end

(** {2 Iteration} *)

type 'a view =
  | True
  | Var of var
  | Neg of 'a
  | And of 'a * 'a

module NodeOrd = struct
  type t = node
  let compare n1 n2 = Pervasives.compare n1.id n2.id
end

module NodeMap = Map.Make(NodeOrd)
module NodeSet = Set.Make(NodeOrd)

type fold_stack =
  | St_bot
  | St_explore of node * fold_stack
  | St_and of node * fold_stack
  | St_neg of node * fold_stack

(* stack: operation stack
   acc: values stack
   map: memoization results *)
let rec fold_rec f map stack acc = match stack, acc with
  | St_bot, [x] -> x
  | St_explore (n, stack'), _ ->
    begin try
      (* use memoized result *)
      let res = NodeMap.find n map in
      fold_rec f map stack' (res :: acc)
    with Not_found ->
      match n.id with
        | 0 -> assert false
        | x when x<0 ->
          (* negation *)
          fold_rec f map (St_explore (n.opp, St_neg (n, stack'))) acc
        | _ ->
          match n.cell with
          | NTrue ->
            let res = f True in
            fold_rec f (NodeMap.add n res map) stack' (res :: acc)
          | NVar v ->
            let res = f (Var v) in
            fold_rec f (NodeMap.add n res map) stack' (res :: acc)
          | NAnd (a, b) ->
            let stack'' = St_explore (b, St_explore (a, St_and (n, stack'))) in
            fold_rec f map stack'' acc
    end
  | St_neg (n, stack'), a :: acc' ->
    let res = f (Neg a) in
    let map' = NodeMap.add n res map in
    fold_rec f map' stack' (res :: acc')
  | St_and (n, stack'), a :: b :: acc' ->
    let res = f (And (a, b)) in
    let map' = NodeMap.add n res map in
    fold_rec f map' stack' (res :: acc')
  | St_bot, _
  | St_neg _, _
  | St_and _, _ -> assert false

let fold' f n = fold_rec f NodeMap.empty (St_explore (n, St_bot)) []

let fold f t = fold' f t.node

(* TODO: AIGER parser (see "http://fmv.jku.at/aiger/")
   TODO: printer to AIGER, too? *)

(* TODO: graphviz export *)

(* TODO: compression by depth-two scanning, see
  "DAG-Aware Circuit Compression For Formal Verification"*)

let rec fold_nodes_rec f acc set stack = match stack with
  | [] -> acc
  | n :: stack' ->
    if NodeSet.mem n set
    then fold_nodes_rec f acc set stack'
    else
      let set' = NodeSet.add n set in
      match n.id with
        | 0 -> assert false
        | x when x<0 ->
          let acc' = f acc (Neg n.opp) in
          fold_nodes_rec f acc' set' (n.opp :: stack)
        | _ ->
          match n.cell with
            | NTrue -> fold_nodes_rec f (f acc True) set' stack'
            | NAnd (a, b) ->
              fold_nodes_rec f (f acc (And (a,b))) set' (a::b::stack')
            | NVar v ->
              fold_nodes_rec f (f acc (Var v)) set' stack'

let fold_nodes' f acc n = fold_nodes_rec f acc NodeSet.empty [n]

let fold_nodes f acc t = fold_nodes' f acc t.node

(** {2 Support} *)

module VarOrd = struct
  type t = var
  let compare (v:var) v' = Pervasives.compare v v'
end
module VarMap = Map.Make(VarOrd)
module VarSet = Set.Make(VarOrd)

let vars_fold' f acc n =
  fold_nodes'
    (fun acc n -> match n with
       | True
       | And _
       | Neg _ -> acc
       | Var v -> f acc v
    ) acc n

let vars_fold f acc t = vars_fold' f acc t.node

let vars t = vars_fold (fun set v -> VarSet.add v set) VarSet.empty t

let vars_iter t yield = vars_fold' (fun () v -> yield v) () t.node

(** {2 Evaluation} *)

let rec eval_rec tbl f n =
  try
    Hashtbl.find tbl n.id
  with Not_found ->
    let res = match n.cell with
      | NTrue -> n.id > 0
      | NVar v ->
        if n.id > 0 then f v else not (f v)
      | NAnd (a, b) ->
        let v =
          eval_rec tbl f a
          && eval_rec tbl f b (* evaluated only if a=true *)
        in
        if n.id > 0 then v else not v
    in
    Hashtbl.replace tbl n.id res;
    res

let eval_fun valuation t =
  eval_rec (Hashtbl.create 256) valuation t.node

(* XXX: naive version:
let eval_fun valuation t =
  fold
    (function
      | True -> true
      | Var v -> valuation v
      | Neg b -> not b
      | And (b1, b2) -> b1 && b2
    ) t
 *)

let eval map t =
  eval_fun (fun v -> VarMap.find v map) t

(** {2 IO} *)

let rec pp' out n =
  if n.id < 0
  then Format.fprintf out "@[<hv1>¬ %a@]" pp_inner n.opp
  else match n.cell with
    | NVar v -> Format.fprintf out "v%d" v
    | NTrue -> Format.fprintf out "1"
    | NAnd (a, b) ->
      Format.fprintf out "@[<hv2>(and@ %a@ %a)@]" pp_inner a pp_inner b
and pp_inner out n =
  if n.id < 0 then Format.fprintf out "(%a)" pp' n else pp' out n

let pp out t = pp' out t.node

let pp_shared out t = assert false (* TODO *)

