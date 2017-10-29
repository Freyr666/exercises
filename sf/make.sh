#!/bin/sh

coqc Basics.v
coqc Induction.v
coqc Lists.v
coqc Poly.v
coqc Tactics.v
coqc Logic.v
coqc IndProp.v
coqc Maps.v
coqc ProofObjects.v
coqc IndPrinciples.v
coqc Rel.v
coqc Imp.v
coqc ImpParser.v
coqc ImpCEvalFun.v
#coqc Extraction.v
#ocamlc -w -20 -w -26 -o impdriver imp.mli imp.ml impdriver.ml

