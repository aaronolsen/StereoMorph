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
	//std::pointer_to_unary_function <double,double> roundObject (round) ;

	// CREATE EMPTY INTEGER MATRIX
	Rcpp::IntegerMatrix mat_int(nrow, ncol);
	Rcpp::NumericMatrix mat_num(nrow, ncol);

	// COPY MATRIX
	std::copy(mat.begin(), mat.end(), mat_num.begin());

	// Multiply by 255 and round to create bins
	for (i = 0; i < ncol; i++) {
		for (j = 0; j < nrow; j++) {
			mat_num(j,i) = std::round(mat_num(j,i) * 255);
		}
	}

	// Multiply by 255 and round to create bins
	for (i = 0; i < thresh_mat.ncol(); i++) {
		for (j = 0; j < thresh_mat.nrow(); j++) {
			thresh_mat(j,i) = std::round(thresh_mat(j,i) * 255);
		}
	}

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