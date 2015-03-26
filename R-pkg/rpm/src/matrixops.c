#include "rpm.h"

/**
 * mat: m x n matrix
 * vec: n-length vector
 * result: m x n matrix
 */
void rowwise_sums(double *_mat, 
		  double *vec,
		  int *_m,
		  int *_n,
		  double *_result) {
  const int m = *_m;
  const int n = *_n;

  Matrix mat = {_mat, m, n};
  Matrix result = {_result, m, n};

  for (int i=0;i<m;i++) {
    for (int j=0;j<n;j++) {
      *get(&result, i, j) = *get(&mat, i, j) + vec[j];
    }
  }
}
