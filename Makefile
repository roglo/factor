# $Id: Makefile,v 1.36 2018/01/19 14:49:34 deraugla Exp $

OUT=factor gcd random truncable x2pmy2 fib fib2 two_squares order conj4 conj5 resolve_2sq gauss pseudofact what
FACTOR_OBJS=bnum.cmx rho_pollard.cmx quad_sieve.cmx fmr.cmx stringexp.cmx factor.cmx
GCD_OBJS=bnum.cmx gcd.cmx
TWO_SQUARES_OBJS=stringexp.cmx two_squares.cmx
RESOLVE_2SQ_OBJS=bnum.cmx rho_pollard.cmx quad_sieve.cmx fmr.cmx stringexp.cmx resolve_2sq.cmx
GAUSS_OBJS=stringexp.cmx gauss.cmx
PSEUDOFACT_OBJS=bnum.cmx pseudofact.cmx
WHAT_OBJS=bnum.cmx stringexp.cmx what.cmx
MLBROT_DIR=../../mlbrot/master

all: $(OUT)
# all: nums.cmxa $(OUT)

nums.cmxa: nat.cmx big_int.cmx num.cmx
	ocamlopt nat.cmx big_int.cmx num.cmx -o $@ -a

depend:
	export LC_ALL=C; for i in $$(ls *.ml *.mli); do camlp5r pr_depend.cmo $(C5FLAGS) $$i; done > .depend.new
	mv .depend.new .depend

factor: $(FACTOR_OBJS)
	ocamlopt unix.cmxa $$(camlp5 -where)/camlp5.cmxa $(MLBROT_DIR)/mpz.cmx $(MLBROT_DIR)/ml_mpz.o -cclib -lgmp nums.cmxa $(FACTOR_OBJS) -o factor

gcd: $(GCD_OBJS)
	ocamlopt $$(camlp5 -where)/camlp5.cmxa $(MLBROT_DIR)/mpz.cmx $(MLBROT_DIR)/ml_mpz.o -cclib -lgmp nums.cmxa $(GCD_OBJS) -o gcd

random: nrandom.cmx
	ocamlopt nums.cmxa nrandom.cmx -o random

truncable: truncable.cmx
	ocamlopt nums.cmxa truncable.cmx -o truncable

x2pmy2: x2pmy2.cmx
	ocamlopt x2pmy2.cmx -o x2pmy2

fib: fib.cmx
	ocamlopt nums.cmxa fib.cmx -o fib

fib2: fib2.cmx
	ocamlopt nums.cmxa fib2.cmx -o fib2

two_squares: $(TWO_SQUARES_OBJS)
	ocamlopt $(TWO_SQUARES_OBJS) -o $@

order: order.cmx
	ocamlopt $< -o $@

conj4: conj4.cmx
	ocamlopt $(MLBROT_DIR)/mpz.cmx $(MLBROT_DIR)/ml_mpz.o -cclib -lgmp $< -o $@

conj5: conj5.cmx
	ocamlopt $(MLBROT_DIR)/ml_mpz.o -cclib -lgmp $< -o $@

resolve_2sq: $(RESOLVE_2SQ_OBJS)
	ocamlopt $$(camlp5 -where)/camlp5.cmxa $(MLBROT_DIR)/mpz.cmx $(MLBROT_DIR)/ml_mpz.o -cclib -lgmp nums.cmxa $(RESOLVE_2SQ_OBJS) -o $@

gauss: $(GAUSS_OBJS)
	ocamlopt $(GAUSS_OBJS) -o $@

pseudofact: $(PSEUDOFACT_OBJS)
	ocamlopt $$(camlp5 -where)/camlp5.cmxa $(MLBROT_DIR)/mpz.cmx $(MLBROT_DIR)/ml_mpz.o -cclib -lgmp nums.cmxa $(PSEUDOFACT_OBJS) -o $@

what: $(WHAT_OBJS)
	ocamlopt $$(camlp5 -where)/camlp5.cmxa $(MLBROT_DIR)/mpz.cmx $(MLBROT_DIR)/ml_mpz.o -cclib -lgmp nums.cmxa $(WHAT_OBJS) -o $@

clean:
	rm -f *.cm[oix] *.o $(OUT)

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.mli.cmi:
	ocamlc -pp camlp5r -I $(MLBROT_DIR) -I `camlp5 -where` -c $<

.ml.cmo:
	ocamlc -pp camlp5r -c $<

.ml.cmx:
	ocamlopt -pp camlp5r -I $(MLBROT_DIR) -I `camlp5 -where` -c $<

include .depend
