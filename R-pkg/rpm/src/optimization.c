#include "rpm.h"

/**
 * projects: m x n matrix of project performances
 * constr_A: q x m matrix of constraints coefficients
 * constr_Bmat: a matrix of np x q constraint limits
 * m: number of projects
 * n: number of attributes
 * q: number of constraints
 * np: number of portfolios
 * result: np x n matrix of return values
 */
void compute_right_add(double *_projects, 
		       double *_constr_A, 
		       double *_constr_Bmat,
		       int *_m, int *_n, int *_q, int *_np,
		       double* _result) {

  const int m = *_m;
  const int n = *_n;
  const int q = *_q;
  const int np = *_np;
  Matrix projects = { _projects, m, n };
  Matrix constr_A = { _constr_A, q, m };
  Matrix constr_Bmat = { _constr_Bmat, np, q };
  Matrix result = {_result, np, n};

  lprec *lp;
  lp = make_lp(q, m); // q constraints, m vars
  if (lp == NULL) {
    error ("unable to create a new LP model");
  }
  set_maxim(lp);
  set_verbose(lp, SEVERE);

  REAL * obj_func = malloc (sizeof(REAL) * (m+1));
  REAL * constr = malloc (sizeof(REAL) * (m+1));

  // set upper bounds to 1.0
  for (int i=0;i<m;i++) {
    set_upbo(lp, i+1, 1.0);
  }

  // set constraint coefficients
  constr[0] = 0.0;
  for (int i=0;i<q;i++) {
    for (int j=0;j<m;j++) {
      constr[j+1] = *get(&constr_A, i, j);
    }
    if (!set_row(lp, i+1, constr) || !set_constr_type(lp, i+1, LE)) {
      free(obj_func);
      free(constr);
      error("unable to set constraint");
    }
  }

  obj_func[0] = 0.0;
  for (int windex=0;windex<n;windex++) { // windex is the attribute (weight) index

    // set objective function
    for (int i=0;i<m;i++) {
      obj_func[i+1] = *get(&projects, i, windex);
    }
    if (!set_obj_fn(lp, obj_func)) {
      free(obj_func);
      free(constr);
      error("unable to set objective function");
    }

    for (int pindex=0;pindex<np;pindex++) {
    
      for (int bindex=0;bindex<q;bindex++) { // set all constraint rhs's
	if (!set_rh(lp, bindex+1, *get(&constr_Bmat, pindex, bindex))) {
	  free(obj_func);
	  free(constr);
	  error("unable to set constraint rhs");
	}
      }
      int res = solve(lp);
      
      if (res != 0) {
	free(obj_func);
	free(constr);
	error("unable to solve the model, error code: %d", res);
      }
      
      *get(&result, pindex, windex) = get_objective(lp);
    }
  }
  delete_lp(lp);
  free(obj_func);
  free(constr);
}
