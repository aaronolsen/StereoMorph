#include <Rcpp.h>

void findGradient(Rcpp::NumericMatrix &mat, std::vector<double> &g, int q, std::vector<int> &wvec, std::vector<int> &kvec, std::vector<double> &kmlt){

	int i, j;
	double q_sum, p_value;

	int wvec_size = wvec.size();
	int kvec_size = kvec.size();

	for(i = 0; i < wvec_size; i++){

		// VALUE AT P
		p_value = *(mat.begin() + q + *(wvec.begin()+i));
		
		// CALCULATE GRADIENT VECTOR IN KERNEL AROUND EACH PIXEL IN WINDOW
		q_sum = 0;
		for(j = 0; j < kvec_size; j++){
			q_sum += *(kmlt.begin()+j) * (*(mat.begin() + q + *(wvec.begin()+i) + *(kvec.begin()+j)) - p_value);
		}
		
		// SAVE GRADIENT VECTOR
		*(g.begin()+i) = q_sum;
	}

	return;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix findCornerSubPix(Rcpp::NumericMatrix image, Rcpp::IntegerMatrix corners, int win, int max_iter, double criteria){

	int i, j, k, q, qxi, qyi;
	double a, b, c, bb1, bb2, det, err, gx, gxx, gy, gyy, gxy, px, py, qx, qx2, qy, qy2, scale;
	int nrow = corners.nrow();
	int nrow_img = image.nrow();

	std::vector<int> kvec(9);
	std::vector<double> kmlt_x(9);
	std::vector<double> kmlt_y(9);

	Rcpp::NumericMatrix corners_sub(nrow, 2);

	// IF WINDOW IS EVEN, INCREASE BY ONE
	if(win % 2 == 0) win = win + 1;

	int	win_win = win*win;
	std::vector<int> wvec(win_win);
	std::vector<int> pxv(win_win);
	std::vector<int> pyv(win_win);
	std::vector<int> px_ex(win_win);
	std::vector<int> py_ex(win_win);

	// CREATE GRADIENT MATRICES
	std::vector<double> gxv(win_win);
	std::vector<double> gyv(win_win);

	// GET HALF WINDOW
	int half_win = win / 2;

	// CREATE WINDOW INDICES FOR ITERATING THROUGH SUBMATRIX
	for(i = 0; i < win; i++) for(j = 0; j < win; j++) *(wvec.begin()+i+j*win) = i+j*nrow_img;
	std::transform(wvec.begin(), wvec.end(), wvec.begin(), std::bind1st(std::plus<int>(), -*(wvec.begin()+((win_win)/2))));

	// CREATE KERNEL INDICES FOR ITERATING THROUGH WINDOW
	for(i = 0; i < 3; i++) for(j = 0; j < 3; j++) *(kvec.begin()+i+j*3) = i+j*nrow_img;
	std::transform(kvec.begin(), kvec.end(), kvec.begin(), std::bind1st(std::plus<int>(), -*(kvec.begin()+4)));

	// CREATE X AND Y KERNEL MULTIPLIER VECTORS
	for(i=0; i < 3; i++) for(j = 0, k=-1; j < 3; j++, k++) *(kmlt_x.begin()+i+j*3) = k;
	for(i=0, k=-1; i < 3; i++, k++) for(j = 0; j < 3; j++) *(kmlt_y.begin()+i+j*3) = k;

	// CREATE P VECTORS
	for(i=0; i < win; i++) for(j = 0, k=-half_win; j < win; j++, k++) *(pyv.begin()+i+j*win) = k;
	for(i=0, k=-half_win; i < win; i++, k++) for(j = 0; j < win; j++) *(pxv.begin()+i+j*win) = k;

	// ITERATE THROUGH EACH CORNER
	for(i = 0; i < nrow; i++){

		qx = qxi = *(corners.begin()+i);
		qy = qyi = *(corners.begin()+i+nrow);

		// ITERATE THROUGH SUB PIXEL ESTIMATIONS
		for(j = 0; j < max_iter; j++){

			// GET Q IN MATRIX POSITION COORDINATES
			q = round(qx) + round(qy)*nrow_img;

			// FIND GRADIENT ACROSS WINDOW
			findGradient(image, gxv, q, wvec, kvec, kmlt_x);
			findGradient(image, gyv, q, wvec, kvec, kmlt_y);

			// RESET VARIABLES
			a = b = c = bb1 = bb2 = 0;

			// CREATE 2X2 MATRIX FOR SOLVING
			for(k = 0; k < win_win; k++){

				gx = -*(gyv.begin()+k);
				gy = -*(gxv.begin()+k);
				px = *(pxv.begin()+k);
				py = *(pyv.begin()+k);
				
				gxx = gx*gx;
				gyy = gy*gy;
				gxy = gx*gy;
				
				a += gxx;
				b += gxy;
				c += gyy;
				
				bb1 += gxx*px + gxy*py;
				bb2 += gxy*px + gyy*py;
			}

			// SOLVE 2X2 MATRIX
			det = a*c - b*b;
            if(fabs( det ) > 1.0e-12){
				scale = 1.0/det;
				qx2 = round(qx) + c*scale*bb1 - b*scale*bb2;
				qy2 = round(qy) - b*scale*bb1 + a*scale*bb2;
			}else{
				qx2 = qx;
				qy2 = qy;
			}

			// FIND DIFFERENCE FROM PREVIOUS POSITION
			err = (qx2 - qx) * (qx2 - qx) + (qy2 - qy) * (qy2 - qy);

			// IF NEW POINT IS TOO FAR FROM INITIAL, IT MEANS POOR CONVERGENCE, ONLY SAVE NEW IF CONVERGED
			if(fabs(qx2 - qxi) > half_win || fabs(qy2 - qyi) > half_win) break;

			qx = qx2;
			qy = qy2;

			// BREAK IF DIFFERENCE FROM PREVIOUS IS LESS THAN THRESHOLD
			if(err < criteria) break;
		}
		
		*(corners_sub.begin()+i) = qx;
		*(corners_sub.begin()+i+nrow) = qy;
	}

	return corners_sub;
//	for(i = 0; i < wvec.size(); i++) Rcpp::Rcout << wvec[i] << std::endl;
}
