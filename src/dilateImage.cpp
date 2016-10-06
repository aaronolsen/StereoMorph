#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::IntegerMatrix dilateImage(Rcpp::IntegerMatrix mat, int kernel, int niter) {

	// ITERATING THROUGH MATRIX BY POINTER GOES DOWN COLUMNS FIRST, THEN BY ROW
	// WILL ONLY WORK FOR BINARY IMAGES (RUNS FASTER WHEN VALUES CAN ONLY BE 0,1)
	
	if(niter == 0) return mat;

	int i, j, k, it;
	int nrow = mat.nrow();
	int ncol = mat.ncol();

	// CREATE MATRICES
	Rcpp::IntegerMatrix mat_max(nrow, ncol);
	Rcpp::IntegerMatrix mat_copy(nrow, ncol);

	// COPY MATRIX
	std::copy(mat.begin(), mat.end(), mat_copy.begin());

	// IF KERNEL IS EVEN, INCREASE BY ONE
	if(kernel % 2 == 0) kernel = kernel + 1;

	// CREATE KERNEL VECTOR
	int kernel_sq = kernel*kernel;
	std::vector<int> k_vec(kernel_sq);
	for(i = 0; i < kernel_sq; i++) k_vec[i] = (i % kernel) + (i/kernel)*nrow;

	// GET HALF KERNEL
	int half_k = (kernel / 2);
	int	nrow_h = nrow-half_k;
	int	ncol_h = ncol-half_k;

	// GET MID KERNEL
	int mid_k = (kernel_sq / 2);

	for(it = 0; it < niter; it++){

		// SET MAX KERNEL VALUES FOR MATRIX INTERIOR
		int lrow = nrow - kernel + 1;
		int lcol = ncol - kernel + 1;
		for(i = 0; i < lrow; i++){
			for(j = 0; j < lcol;j++){
				for(k = 0; k < kernel_sq; k++){
					if(*(mat_copy.begin() + i + j*nrow + k_vec[k]) == 1){
						*(mat_max.begin() + i + j*nrow + k_vec[mid_k]) = 1;
						break;
					}
				}
			}
		}

		// SET MAX KERNEL VALUES ALONG TOP AND BOTTOM INTERNAL EDGE
		for(j = half_k; j < ncol_h;j++){
			for(i = 0; i < half_k; i++) *(mat_max.begin() + i+j*nrow) = *(mat_max.begin() + half_k+j*nrow);
			for(i = nrow_h; i < nrow; i++) *(mat_max.begin() + i+j*nrow) = *(mat_max.begin() + nrow_h-1+j*nrow);
		}

		// SET MAX KERNEL VALUES ALONG LEFT AND RIGHT EDGE
		for(i = 0; i < nrow; i++){
			for(j = 0; j < half_k;j++) *(mat_max.begin() + i+j*nrow) = *(mat_max.begin() + i+half_k*nrow);
			for(j = ncol_h; j < ncol;j++) *(mat_max.begin() + i+j*nrow) = *(mat_max.begin() + i+(ncol_h-1)*nrow);
		}
		
		if(it < niter - 1) std::copy(mat_max.begin(), mat_max.end(), mat_copy.begin());
	}

	return mat_max;
}