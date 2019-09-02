(* This module collects modules and instantiates functors used
   in the semantic analysis phases of our compiler.
 *)

module Er = Errormsg.ErrorMsg

module Sy = Symbol.Symbol

module Ab = Absyn.Absyn(Sy)

module En = Env.Env(Sy)

module Pt = Printtype.Printtype (Sy) (En)

module Pa = Printabsyn.Printabsyn (Sy) (Ab)

module Se = Semant.Semant (Sy) (En) (Ab) (Er)
