(* This module collects modules and instantiates functors used
   in the semantic analysis phases of our compiler.
 *)

module Er = Errormsg.ErrorMsg

module Sy = Symbol.Symbol

module Ab = Absyn.Absyn (Sy)

module Es = Escape.FindEscapes (Ab) (Sy)

module Tm = Temp.Temp (Sy)

module Ir = Tree.Tree (Tm)

module Mr = Mipsregs.MipsRegs (Tm)

module As = Assem.Assem (Tm)

module Fr = Mipsframe.MipsFrame (Tm) (Ir) (As) (Mr)

module Ca = Canon.Canon (Sy) (Tm) (Ir)

module Tr = Translate.Translate (Tm) (Ir) (Fr) (Ca)

module En = Env.Env (Sy) (Tm) (Tr)

module Pt = Printtype.Printtype (Sy) (En)

module Pa = Printabsyn.Printabsyn (Sy) (Ab)

module Pi = Printtree.PrintTree (Sy) (Tm) (Ir) (Fr)

module Se = Semant.Semant (Sy) (En) (Ab) (Er) (Tr) (Tm)

module Mc = Mipscodegen.MipsCodeGen (Ir) (As) (Tm) (Mr) (Fr)

module Fg = Flowgraph.Flow (Tm)

module Mf = Mkflowgraph.MkFlowGraph (Sy) (Fg) (As)

module Ig = Igraph.IGraph (Tm) (Fg)

module Cl = Color.Color (Ig) (Tm)

module Ra = Regalloc.RegAlloc (Tm) (Ig) (Cl) (As) (Fr) (Mf) 
