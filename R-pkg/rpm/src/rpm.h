#include <R.h>
#include <stdio.h>
#include <R_ext/Rdynload.h>
#include "lpsolve/lp_lib.h"
#include "matrix.h"

void compute_right_add(double *_projects, 
		       double *_constr_A, 
		       double *_constr_Bmat,
		       int *_m, int *_n, int *_q, int *_np,
		       double* _result);

void row_dominance(double *_a, double *_b, int *_ncol, 
		   int *_nrow_a, int *_nrow_b, double *result);
