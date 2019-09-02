(* This module collects modules and instantiates functors used
   in the semantic analysis phases of our compiler.
 *)

module Er = Errormsg.ErrorMsg

module Sy = Symbol.Symbol

module Ab = Absyn.Absyn (Sy)

module Tm = Temp.Temp (Sy)

module Ir = Tree.Tree (Tm)

module Mr = Mipsregs.MipsRegs (Tm)

module Fr = Mipsframe.MipsFrame (Tm) (Ir) (Mr)

module Ca = Canon.Canon (Sy) (Tm) (Ir)

module Tr = Translate.Translate (Tm) (Ir) (Fr) (Ca)

module En = Env.Env (Sy) (Tm) (Tr)

module Pt = Printtype.Printtype (Sy) (En)

module Pa = Printabsyn.Printabsyn (Sy) (Ab)

module Pi = Printtree.PrintTree (Sy) (Tm) (Ir) (Fr)

module Se = Semant.Semant (Sy) (En) (Ab) (Er) (Tr) (Tm)

module Es = Escape.FindEscapes (Ab) (Sy)

