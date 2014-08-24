#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericMatrix rgbToGray(Rcpp::NumericMatrix ch1, Rcpp::NumericMatrix ch2, Rcpp::NumericMatrix ch3) {

	// SET CONVERSION COEFFICIENTS
	std::vector<double> coeffs(3);
	coeffs[0] = 0.299;
	coeffs[1] = 0.587;
	coeffs[2] = 0.114;
	
	int i;
	int mat_size = ch1.end() - ch1.begin();

	// CREATE EMPTY GRAY IMAGE MATRIX
	Rcpp::NumericMatrix gray_img(ch1.nrow(), ch1.ncol());

	for(i = 0;i < mat_size;i++){
		*(gray_img.begin()+i) = *(ch1.begin()+i)*coeffs[0] + *(ch2.begin()+i)*coeffs[1] + *(ch3.begin()+i)*coeffs[2];
	}

	return gray_img;
}