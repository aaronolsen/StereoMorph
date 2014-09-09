#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::IntegerMatrix erodeImage(Rcpp::IntegerMatrix mat, int kernel, int niter) {

	if(niter == 0) return mat;

	int i, j, k, it, min;
	int nrow = mat.nrow();
	int ncol = mat.ncol();

	// CREATE MATRICES
	Rcpp::IntegerMatrix mat_min(nrow, ncol);
	Rcpp::IntegerMatrix mat_copy(nrow, ncol);

	// COPY MATRIX
	std::copy(mat.begin(), mat.end(), mat_copy.begin());

	// IF KERNEL IS EVEN, INCREASE BY ONE
	if(kernel % 2 == 0) kernel = kernel + 1;

	// CREATE KERNEL VECTOR
	int kernel_sq = kernel*kernel;
	std::vector<int> kernel_vec(kernel_sq);
	for(i = 0; i < kernel_sq; i++) kernel_vec[i] = (i % kernel) + (i/kernel)*nrow;

	// GET HALF KERNEL
	int half_kernel = kernel / 2;
	int	nrow_h = nrow-half_kernel;
	int	ncol_h = ncol-half_kernel;

	// GET MID KERNEL
	int mid_kernel = kernel_sq / 2;

	// CREATE COLUMN VECTOR
	std::vector<int> col_vec(ncol);
	for(i = 0; i < ncol; i++) col_vec[i] = i*nrow;

	for(it = 0; it < niter; it++){

		// SET MAX KERNEL VALUES FOR MATRIX INTERIOR
		int lrow = nrow - kernel + 1;
		int lcol = ncol - kernel + 1;
		for(i = 0; i < lrow; i++){
			for(j = 0; j < lcol;j++){
				min = 1;
				for(k = 0; k < kernel_sq; k++){
					if(*(mat_copy.begin() + i+col_vec[j] + kernel_vec[k]) < min) min = *(mat_copy.begin() + i+col_vec[j] + kernel_vec[k]);
				}
				*(mat_min.begin() + i+col_vec[j] + kernel_vec[mid_kernel]) = min;
			}
		}

		// SET MAX KERNEL VALUES ALONG TOP AND BOTTOM INTERNAL EDGE
		for(j = half_kernel; j < ncol_h;j++){
			for(i = 0; i < half_kernel; i++) *(mat_min.begin() + i+col_vec[j]) = *(mat_min.begin() + half_kernel+col_vec[j]);
			for(i = nrow_h; i < nrow; i++) *(mat_min.begin() + i+col_vec[j]) = *(mat_min.begin() + nrow_h-1+col_vec[j]);
		}

		// SET MAX KERNEL VALUES ALONG LEFT AND RIGHT EDGE
		for(i = 0; i < nrow; i++){
			for(j = 0; j < half_kernel;j++) *(mat_min.begin() + i+col_vec[j]) = *(mat_min.begin() + i+col_vec[half_kernel]);
			for(j = ncol_h; j < ncol;j++) *(mat_min.begin() + i+col_vec[j]) = *(mat_min.begin() + i+col_vec[ncol_h-1]);
		}

		if(it < niter - 1) std::copy(mat_min.begin(), mat_min.end(), mat_copy.begin());
	}

	return mat_min;
}