#include <R.h>
#include <stdio.h>
#include "matrix.h"

#define bool int
#define true 1
#define false 0

/**
 * Check for each for of a, whether any row of B dominates them
 */
void row_dominance(double *_a, double *_b, int *_ncol, 
		   int *_nrow_a, int *_nrow_b, double *result) {
  const int ncol = *_ncol;
  const int nrow_a = *_nrow_a;
  const int nrow_b = *_nrow_b;
  Matrix a = { _a, nrow_a, ncol };
  Matrix b = { _b, nrow_b, ncol };

  for (int arow=0;arow<nrow_a;arow++) {
    result[arow] = false;
    for (int brow=0;brow<nrow_b;brow++) {
      bool b_at_least_large = true;
      bool b_once_larger = false;
      for (int col=0;col<ncol;col++) {
	double aval = *get(&a, arow, col);
	double bval = *get(&b, brow, col);
	
	if (aval > bval) {
	  b_at_least_large = false;
	  break;
	} else if (bval > aval) {
	  b_once_larger = true;
	}
      }
      if (b_at_least_large && b_once_larger) {
	result[arow] = true;
	break;
      }
    }
  }
}
