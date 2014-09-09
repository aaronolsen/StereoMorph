#include <Rcpp.h>
#include <cmath>

// [[Rcpp::export]]

Rcpp::NumericMatrix meanBlurImage(Rcpp::NumericMatrix mat, int kernel) {

	int i, j;
	int nrow = mat.nrow();
	int ncol = mat.ncol();
	double sum, first_sum;

	Rcpp::NumericMatrix col_mean_sum(nrow, ncol);
	Rcpp::NumericMatrix row_mean_sum(nrow, ncol);

	// IF KERNEL IS EVEN, INCREASE BY ONE
	if(kernel % 2 == 0) kernel = kernel + 1;

	// GET HALF KERNEL
	int half_kernel = kernel / 2;
	// Rcpp::Rcout << kernel << ' ' << half_kernel << std::endl;

	// IF KERNEL IS ONE, RETURN INPUT MATRIX
	if(kernel == 1) return mat;

	// FIND COLUMN KERNEL MEANS
	for (i = 0; i < nrow; i++) {
		sum = 0;
		first_sum = 0;
		for (j = 0; j < ncol; j++) {
			sum += mat(i,j);
			if(j > kernel - 1) sum = sum - mat(i, j - kernel);
			if(j >= kernel - 1) col_mean_sum(i, j - half_kernel) = sum / kernel;
			if(j == kernel - 1) first_sum = sum;
		}
		for(j = (ncol-half_kernel);j < ncol; j++) col_mean_sum(i, j) = sum / kernel;
		for(j = 0; j < half_kernel; j++) col_mean_sum(i, j) = first_sum / kernel;
	}

	// FIND ROW KERNEL MEANS
	for (j = 0; j < ncol; j++) {
		sum = 0;
		first_sum = 0;
		for (i = 0; i < nrow; i++) {
			sum = sum + col_mean_sum(i,j);
			if(i > kernel - 1) sum = sum - col_mean_sum(i - kernel, j);
			if(i >= kernel - 1) row_mean_sum(i - half_kernel, j) = sum / kernel;
			if(i == kernel - 1) first_sum = sum;
		}
		for(i = (nrow-half_kernel);i < nrow; i++) row_mean_sum(i, j) = sum / kernel;
		for(i = 0; i < half_kernel; i++) row_mean_sum(i, j) = first_sum / kernel;
	}

	return row_mean_sum;
}