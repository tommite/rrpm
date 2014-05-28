all: unittest

.PHONY: all

LD_LIBRARY_PATH=/usr/lib/lp_solve

clean:
	-rm optimization.?o

dominance.so: dominance.c
	R CMD SHLIB dominance.c

optimization.so: optimization.c
	R CMD SHLIB optimization.c -llpsolve55 -L/usr/lib/lp_solve

test: optimization.so dominance.c
	R --vanilla < unittest2.R
	R --vanilla < unittest.R
