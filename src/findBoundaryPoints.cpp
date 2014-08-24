#include <Rcpp.h>

// [[Rcpp::export]]

Rcpp::IntegerMatrix findBoundaryPoints(Rcpp::IntegerMatrix mat) {

	int i, j;
	int nrow = mat.nrow();
	int ncol = mat.ncol();
	
	// CREATE EMPTY SORTED MATRIX
	Rcpp::IntegerMatrix mat_bound(nrow, ncol);

	// DETECT PIXELS ON RIGHT OR LEFT OF '1' PIXEL
	for (i = 0; i < nrow; i++) {
		for (j = 1; j < ncol-1; j++) {
			if(mat(i,j) == 0){
				mat_bound(i, j-1) = 1;
				mat_bound(i, j+1) = 1;
			}
		}
		if(mat(i, 0) == 0) mat_bound(i, 1) = 1;
		if(mat(i, ncol-1) == 0) mat_bound(i, ncol-2) = 1;
	}

	// DETECT PIXELS ABOVE OR BELOW '1' PIXEL
	for (j = 0; j < ncol; j++) {
		for (i = 1; i < nrow-1; i++) {
			if(mat(i,j) == 0){
				mat_bound(i-1, j) = 1;
				mat_bound(i+1, j) = 1;
			}
		}
		if(mat(0, j) == 0) mat_bound(1, j) = 1;
		if(mat(nrow-1, j) == 0) mat_bound(nrow-2, j) = 1;
	}

	// INVERT BINARY MATRIX (0 -> 1 AND 1 -> 0)
	//std::transform(mat.begin(), mat.end(), mat.begin(), std::bind1st(std::plus<int>(), -1));
	//std::transform(mat.begin(), mat.end(), mat.begin(), std::bind1st(std::multiplies<int>(), -1));

	for (i = 0; i < nrow; i++) {
		for (j = 0; j < ncol; j++) mat_bound(i,j) = mat_bound(i,j)*mat(i,j);
	}

	return mat_bound;
}