typedef struct Matrix {
	double * const data;
	int const nRow;
	int const nCol;
} Matrix;

/**
 * Get an element of a matrix.
 * @param i Row index.
 * @param j Column index.
 */
static inline double *get(Matrix *m, int i, int j) {
	return m->data + j * (m->nRow) + i;
}

/**
 * Write a row to a matrix.
 */
static inline void writeRow(Matrix *m, int i, double *x) {
	for (int j = 0; j < m->nCol; ++j) {
		*get(m, i, j) = x[j];
	}
}
