#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::IntegerMatrix erodeImage(Rcpp::IntegerMatrix mat, int kernel, int niter) {

	// ITERATING THROUGH MATRIX BY POINTER GOES DOWN COLUMNS FIRST, THEN BY ROW
	// WILL ONLY WORK FOR BINARY IMAGES (RUNS FASTER WHEN VALUES CAN ONLY BE 0,1)

	if(niter == 0) return mat;

	int i, j, k, it;
	int nrow = mat.nrow();
	int ncol = mat.ncol();

	// CREATE MATRICES
	Rcpp::IntegerMatrix mat_min(nrow, ncol);
	Rcpp::IntegerMatrix mat_copy(nrow, ncol);

	// FILL MATRIX WITH DEFAULT 1, CHANGED TO 0 WHERE MIN IN KERNEL
	std::fill(mat_min.begin(), mat_min.end(), 1);

	// COPY MATRIX
	std::copy(mat.begin(), mat.end(), mat_copy.begin());

	// IF KERNEL IS EVEN, INCREASE BY ONE
	if(kernel % 2 == 0) kernel = kernel + 1;

	// CREATE KERNEL VECTOR
	int kernel_sq = kernel*kernel;
	std::vector<int> k_vec(kernel_sq);
	for(i = 0; i < kernel_sq; i++) k_vec[i] = (i % kernel) + (i/kernel)*nrow;

	// GET HALF KERNEL
	int half_k = kernel / 2;
	int	nrow_h = nrow-half_k;
	int	ncol_h = ncol-half_k;

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
				for(k = 0; k < kernel_sq; k++){
					if(*(mat_copy.begin() + i+col_vec[j] + k_vec[k]) == 0){
						*(mat_min.begin() + i+col_vec[j] + k_vec[mid_kernel]) = 0;
						break;
					}
				}
			}
		}

		// SET MAX KERNEL VALUES ALONG TOP AND BOTTOM INTERNAL EDGE
		for(j = half_k; j < ncol_h;j++){
			for(i = 0; i < half_k; i++) *(mat_min.begin() + i+col_vec[j]) = *(mat_min.begin() + half_k+col_vec[j]);
			for(i = nrow_h; i < nrow; i++) *(mat_min.begin() + i+col_vec[j]) = *(mat_min.begin() + nrow_h-1+col_vec[j]);
		}

		// SET MAX KERNEL VALUES ALONG LEFT AND RIGHT EDGE
		for(i = 0; i < nrow; i++){
			for(j = 0; j < half_k;j++) *(mat_min.begin() + i+col_vec[j]) = *(mat_min.begin() + i+col_vec[half_k]);
			for(j = ncol_h; j < ncol;j++) *(mat_min.begin() + i+col_vec[j]) = *(mat_min.begin() + i+col_vec[ncol_h-1]);
		}

		if(it < niter - 1) std::copy(mat_min.begin(), mat_min.end(), mat_copy.begin());
	}

	return mat_min;
}