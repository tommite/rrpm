#include "rpm.h"

void compute_right_add(double *_project_vals, 
		       double *project_costs, 
		       double *budgets,
		       int *_m, int *_n, int *_nr_b,
		       double* _result) {

  const int m = *_m;
  const int n = *_n;
  const int nr_b = *_nr_b;
  Matrix project_vals = { _project_vals, m, n };
  Matrix result = {_result, nr_b, m};

  lprec *lp;
  REAL obj_func[m+1];
  REAL cost_const[m+1];
  lp = make_lp(1, m);
  set_maxim(lp);
  set_constr_type(lp, 1, LE);
  set_verbose(lp, SEVERE);

  if (lp == NULL) {
    error ("unable to create a new LP model");
  }

  for (int i=0;i<m;i++) {
    set_upbo(lp, i+1, 1.0);
    cost_const[i+1] = project_costs[i];
  }

  if(!set_row(lp, 1, cost_const)) {
    error("unable to set constraint row");
  }

  for (int windex=0;windex<n;windex++) {
    for (int i=0;i<m;i++) {
      obj_func[i+1] = *get(&project_vals, i, windex);
    }
    
    if (!set_obj_fn(lp, obj_func)) {
      error("unable to set objective function");
    }
    
    for (int bindex=0;bindex<nr_b;bindex++) {
      set_rh(lp, 1, budgets[bindex]);

      int res = solve(lp);
      
      if (res != 0) {
	error("unable to solve the model, error code: %d", res);
      }
      
      *get(&result, bindex, windex) = get_objective(lp);
    }
  }
  delete_lp(lp);
}
