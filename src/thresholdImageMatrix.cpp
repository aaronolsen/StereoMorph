#include <Rcpp.h>
#include <cmath>

// [[Rcpp::export]]
Rcpp::IntegerMatrix thresholdImageMatrix(Rcpp::NumericMatrix mat, Rcpp::NumericMatrix thresh_mat, double delta, int type) {

// Description of input parameters
//	mat : the matrix to be transformed (thresholded)
//	thresh_mat : the matrix to be used in determining the local threshold level
//	delta : a constant to be added to the thresh_mat in determining whether the threshold is crossed
//	type : if (1), lighter areas go to white (255) and darker areas go to black (1)
//		   if (2), the opposite of (1)
//

	int i, j;
	std::map<int, int> tab;
	int nrow = mat.nrow();
	int ncol = mat.ncol();

	// CREATE EMPTY INTEGER MATRIX
	Rcpp::IntegerMatrix mat_int(nrow, ncol);
	Rcpp::NumericMatrix mat_num(nrow, ncol);

	// COPY MATRIX
	std::copy(mat.begin(), mat.end(), mat_num.begin());

	// MULTIPLY BY 255 AND ROUND TO CREATE BINS
	std::transform(mat_num.begin(), mat_num.end(), mat_num.begin(), std::bind1st(std::multiplies<double>(), 255));
	std::transform(mat_num.begin(), mat_num.end(), mat_num.begin(), round);

	std::transform(thresh_mat.begin(), thresh_mat.end(), thresh_mat.begin(), std::bind1st(std::multiplies<double>(), 255));
	std::transform(thresh_mat.begin(), thresh_mat.end(), thresh_mat.begin(), round);

    int idelta = type == 1 ? ceil(delta) : floor(delta);
//	Rcpp::Rcout << "idelta: " << idelta << std::endl;

	if(type == 1){
		for( i = 0; i <= 510; i++ )
			tab[i] = (int)(i - 255 > -idelta ? 1 : 0);
	}else{
		for( i = 0; i <= 510; i++ )
			tab[i] = (int)(i - 255 <= -idelta ? 1 : 0);
	}

	// REPLACE VALUES WITH THRESHOLD RESULT
	for (i = 0; i < ncol; i++) {
		for (j = 0; j < nrow; j++) {
			//Rcpp::Rcout << mat_num(j,i) - thresh_mat(j,i) + 255 << ' ';			
			//Rcpp::Rcout << tab[mat_num(j,i) - thresh_mat(j,i) + 255] << ' ';			
			mat_int(j,i) = (int) tab[mat_num(j,i) - thresh_mat(j,i) + 255];
		}
	}

	return mat_int;
}