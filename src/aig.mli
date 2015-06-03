
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
(** A boolean variable *)

type manager
(** Manager, stores graph nodes and ensures maximal sharing *)

val create : ?size:int -> unit -> manager
(** New manager *)

type t
(** A node of an AIG *)

(** {2 Basics} *)

val equal : t -> t -> bool
(** [O(1)] equality between nodes of the same manager *)

val compare : t -> t -> int
(** Total order between nodes of the same manager *)

val manager : t -> manager

val sign : t -> bool
(** Sign of the node (positive or negative) *)

val abs : t -> t
(** Remove sign (make positive) *)

val apply_sign : t -> bool -> t
(** [apply_sign t s] sets the sign of [t] to [s] *)

(** {2 Boolean Operators} *)

val true_ : manager -> t

val false_ : manager -> t

val var : manager -> var -> t
(** Variable node *)

val neg : t -> t
(** Negate node *)

val and_ : t -> t -> t
(** Conjunction of nodes *)

val or_ : t -> t -> t
(** Disjunction of nodes *)

val imply : t -> t -> t
(** implication *)

val equiv : t -> t -> t

val xor : t -> t -> t

module Infix : sig
  val (~-) : t -> t
  val (&&) : t -> t -> t
  val (||) : t -> t -> t
  val (==>) : t -> t -> t (** Imply *)
  val (<=>) : t -> t -> t (** Equiv *)
end

(** {2 Iteration} *)

type 'a view =
  | True
  | Var of var
  | Neg of 'a
  | And of 'a * 'a

val fold : ('a view -> 'a) -> t -> 'a
(** Fold on the formula, down to the leaves *)


(** {2 Evaluation} *)

val eval : (var -> bool) -> t -> bool
(** Evaluate a graph under a boolean valuation *)

