(* This module collects modules and instantiates functors used
   in the semantic analysis phases of our compiler.
 *)

module Er = Errormsg.ErrorMsg

module Sy = Symbol.Symbol

module Ab = Absyn.Absyn(Sy)

module Tm = Temp.Temp(Sy)

module Fr = Mipsframe.MipsFrame (Tm)

module Tr = Translate.Translate (Fr) (Tm)

module En = Env.Env(Sy) (Tm) (Tr)

module Pt = Printtype.Printtype (Sy) (En)

module Pa = Printabsyn.Printabsyn (Sy) (Ab)

module Se = Semant.Semant (Sy) (En) (Ab) (Er) (Tr) (Tm)

module Es = Escape.FindEscapes (Ab) (Sy)

