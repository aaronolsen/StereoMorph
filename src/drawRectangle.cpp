#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::IntegerMatrix drawRectangle(Rcpp::IntegerMatrix mat, Rcpp::IntegerVector corner1, Rcpp::IntegerVector corner2, int value, int thickness) {

	int i, j;
	int nrow = mat.nrow();
	int ncol = mat.ncol();

	// COPY MATRIX
	Rcpp::IntegerMatrix mat_copy(nrow, ncol);
	std::copy(mat.begin(), mat.end(), mat_copy.begin());

	// CHECK THAT CORNERS ARE WITHIN BOUNDS OF MATRIX
	if(corner1[0] < 0 || corner1[0] > ncol-1) return mat;
	if(corner1[1] < 0 || corner1[1] > nrow-1) return mat;
	if(corner2[0] < 0 || corner2[0] > ncol-1) return mat;
	if(corner2[1] < 0 || corner2[1] > nrow-1) return mat;

	// DRAW TOP BORDER
	for (j = corner1[0]; j <= corner2[0]; j++) {		
		for(i = 0; i < thickness; i++) mat_copy(corner1[1]+i, j) = value;
	}

	// DRAW RIGHT BORDER
	for (i = corner1[1]; i <= corner2[1]; i++) {
		for(j = 0; j < thickness; j++) mat_copy(i, corner2[0]-j) = value;
	}

	// DRAW BOTTOM BORDER
	for (j = corner1[0]; j <= corner2[0]; j++) {		
		for(i = 0; i < thickness; i++) mat_copy(corner2[1]-i, j) = value;
	}

	// DRAW LEFT BORDER
	for (i = corner1[1]; i <= corner2[1]; i++) {		
		for(j = 0; j < thickness; j++) mat_copy(i, corner1[0]+j) = value;
	}

	return mat_copy;
}