# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: roblabla </var/spool/mail/roblabla>        +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2015/06/19 13:32:18 by roblabla          #+#    #+#              #
#    Updated: 2015/06/19 21:28:27 by roblabla         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

# You may use any of the following predefined variable for LIBS
# WITHGRAPHICS : provides the graphics library
# WITHUNIX : provides the Unix interface library
# WITHSTR : provides the regular expression string manipulation library
# WITHNUMS : provides the arbitrary precision arithmetic package
# WITHTHREADS : provides the byte-code threads library
# WITHDBM : provides the Data Base Manager library

LIBS=$(WITHGRAPHICS)

########################## User's variables #####################
#
# The Caml sources (including camlyacc and camllex source files)

SOURCES = btree.ml

# The executable file to generate

EXEC = a.out


########################## Advanced user's variables #####################
#
# The Caml compilers.
# You may fix here the path to access the Caml compiler on your machine
# You may also have to add various -I options.
CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc

# The list of Caml libraries needed by the program
# For instance:
# LIBS=$(WITHGRAPHICS) $(WITHUNIX) $(WITHSTR) $(WITHNUMS) $(WITHTHREADS)\
# $(WITHDBM)

# LIBS=$(WITHGRAPHICS)

# Should be set to -custom if you use any of the libraries above
# or if any C code have to be linked with your program
# (irrelevant for ocamlopt)

# CUSTOM=-custom

# Default setting of the WITH* variables. Should be changed if your
# local libraries are not found by the compiler.
WITHGRAPHICS =graphics.cma -cclib -lgraphics -cclib -L/usr/X11R6/lib -cclib -lX11

WITHUNIX =unix.cma -cclib -lunix

WITHSTR =str.cma -cclib -lstr

WITHNUMS =nums.cma -cclib -lnums

WITHTHREADS =threads.cma -cclib -lthreads

WITHDBM =dbm.cma -cclib -lmldbm -cclib -lndbm

################ End of user's variables #####################


##############################################################
################ This part should be generic
################ Nothing to set up or fix here
##############################################################

#all:: .depend.input .depend $(EXEC)
all: $(EXEC)

opt : $(EXEC).opt

#ocamlc -custom other options graphics.cma other files -cclib -lgraphics -cclib -lX11
#ocamlc -thread -custom other options threads.cma other files -cclib -lthreads
#ocamlc -custom other options str.cma other files -cclib -lstr
#ocamlc -custom other options nums.cma other files -cclib -lnums
#ocamlc -custom other options unix.cma other files -cclib -lunix
#ocamlc -custom other options dbm.cma other files -cclib -lmldbm -cclib -lndbm

SMLIY = $(SOURCES:.mly=.ml)
SMLIYL = $(SMLIY:.mll=.ml)
SMLYL = $(filter %.ml,$(SMLIYL))
OBJS = $(SMLYL:.ml=.cmo)
OPTOBJS = $(OBJS:.cmx=.cmx)

$(EXEC): $(OBJS) 
	$(CAMLC) $(CUSTOM) -o $(EXEC) $(LIBS) $(OBJS)

# $(EXEC)-opt: $(OPTOBJS)
#	$(CAMLOPT) -o $(EXEC) $(LIBS:.cma=.cmxa) $(OPTOBJS)

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly 

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

.mll.cmo:
	$(CAMLLEX) $<
	$(CAMLC) -c $*.ml

.mll.cmx:
	$(CAMLLEX) $<
	$(CAMLOPT) -c $*.ml

.mly.cmo:
	$(CAMLYACC) $<
	$(CAMLC) -c $*.mli
	$(CAMLC) -c $*.ml

.mly.cmx:
	$(CAMLYACC) $<
	$(CAMLOPT) -c $*.mli
	$(CAMLOPT) -c $*.ml

.mly.cmi:
	$(CAMLYACC) $<
	$(CAMLC) -c $*.mli

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
	$(CAMLYACC) $<

clean::
	rm -f *.cm[iox] *~ .*~ #*#
	rm -f $(EXEC)
	rm -f $(EXEC).opt

# .depend.input: Makefile
# 	@echo -n '--Checking Ocaml input files: '
# 	@(ls $(SMLIY) $(SMLIY:.ml=.mli) 2>/dev/null || true) \
# 	     >  .depend.new
# 	@diff .depend.new .depend.input 2>/dev/null 1>/dev/null && \
# 	    (echo 'unchanged'; rm -f .depend.new) || \
# 	    (echo 'changed'; mv .depend.new .depend.input)
# 
# depend: .depend
# 
# .depend:: $(SMLIY) .depend.input
# 	@echo '--Re-building dependencies'
# 	$(CAMLDEP) $(SMLIY) $(SMLIY:.ml=.mli) > .depend
# 
# include .depend
# 
# 
