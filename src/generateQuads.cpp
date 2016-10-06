#include <Rcpp.h>

struct Point
{
    int x;
    int y;
};
 
bool onSegment(Point p, Point q, Point r)
{
// Given three colinear points p, q, r, the function checks if
// point q lies on line segment 'pr'

    if (q.x <= std::max(p.x, r.x) && q.x >= std::min(p.x, r.x) &&
        q.y <= std::max(p.y, r.y) && q.y >= std::min(p.y, r.y))
       return true;
 
    return false;
}
 
int orientation(Point p, Point q, Point r)
{
	// To find orientation of ordered triplet (p, q, r).
	// The function returns following values
	// 0 --> p, q and r are colinear
	// 1 --> Clockwise
	// 2 --> Counterclockwise

    // See 10th slides from following link for derivation of the formula
    // http://www.dcs.gla.ac.uk/~pat/52233/slides/Geometry1x1.pdf
    int val = (q.y - p.y) * (r.x - q.x) -
              (q.x - p.x) * (r.y - q.y);
 
    if (val == 0) return 0;  // colinear
 
    return (val > 0)? 1: 2; // clock or counterclock wise
}

double distPointToLine(int x, int y, int lx1, int ly1, int lx2, int ly2)
{

	double cprod = std::abs((double (x - lx1))*(double (y - ly2)) - (double (y - ly1))*(double (x - lx2)));
	double lmag = std::sqrt(std::pow(double (lx2 - lx1), 2) + std::pow(double (ly2 - ly1), 2));

	return cprod / lmag;
}


bool doIntersect(Point p1, Point q1, Point p2, Point q2)
{
    // Find the four orientations needed for general and
    // special cases
    int o1 = orientation(p1, q1, p2);
    int o2 = orientation(p1, q1, q2);
    int o3 = orientation(p2, q2, p1);
    int o4 = orientation(p2, q2, q1);
 
    // General case
    if (o1 != o2 && o3 != o4)
        return true;
 
    // Special Cases
    // p1, q1 and p2 are colinear and p2 lies on segment p1q1
    if (o1 == 0 && onSegment(p1, p2, q1)) return true;
 
    // p1, q1 and p2 are colinear and q2 lies on segment p1q1
    if (o2 == 0 && onSegment(p1, q2, q1)) return true;
 
    // p2, q2 and p1 are colinear and p1 lies on segment p2q2
    if (o3 == 0 && onSegment(p2, p1, q2)) return true;
 
     // p2, q2 and q1 are colinear and q1 lies on segment p2q2
    if (o4 == 0 && onSegment(p2, q1, q2)) return true;
 
    return false; // Doesn't fall in any of the above cases
}

int pointsOnLine(int x1, int y1, int x2, int y2, std::vector<int> &row, std::vector<int> &col, std::vector<int> &lprow, std::vector<int> &lpcol, double dist_ratio_thresh){

	int i, n, idx;
	double dist_mid1, dist_mid2, dist_12;
	int row_size = row.size();
	std::vector<int> dist(row_size);
	std::vector<double> dist_ratio(row_size);

	// CLEAR LINE POINT VECTORS
	lprow.clear();
	lpcol.clear();

	int lprow_capacity = lprow.capacity();

	// FIND DISTANCE BETWEEN FIRST POINT AND SECOND POINT
	dist_12 = std::sqrt((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2));

	for(i = 0; i < row_size; i++){
		// FIND DISTANCE BETWEEN FIRST POINT AND ALL POINTS
		dist_mid1 = std::sqrt(std::pow(double (x1 - *(row.begin() + i)), 2) + std::pow(double (y1 - *(col.begin() + i)), 2));
		*(dist.begin() + i) = round(dist_mid1);

		// FIND DISTANCE BETWEEN SECOND POINT AND ALL POINTS
		dist_mid2 = std::sqrt(std::pow(double (x2 - *(row.begin() + i)), 2) + std::pow(double (y2 - *(col.begin() + i)), 2));

		// FIND THE RATIO BETWEEN THE FIRST-MIDPOINT-LAST DISTANCE AND FIRST-LAST DISTANCE
		*(dist_ratio.begin() + i) = (dist_mid1+dist_mid2)/dist_12;
	}

//	for(int i = 0;i < dist_ratio.size();i++) Rcpp::Rcout << dist_ratio[i] << std::endl;
//	Rcpp::Rcout << std::endl;
	
	n = 0;
	i = 0;
	while(n < row_size && i < lprow_capacity){

		// FIND POINT AT MINIMUM DISTANCE TO FIRST POINT
		idx = std::distance(dist.begin(), min_element(dist.begin(), dist.end()));
		
		// IF DISTANCE RATIO IS GREATER THAN THRESHOLD SKIP AND SET TO MAX
		if(*(dist_ratio.begin() + idx) > dist_ratio_thresh){
//			Rcpp::Rcout << *(dist_ratio.begin() + idx) << " : continue;" << std::endl;
			n++;
			*(dist.begin()+idx) = 100000;
			continue;
		}

//		Rcpp::Rcout << *(dist_ratio.begin() + idx) << " : write;" << std::endl;

		// ADD POINT TO POINT ON LINE VECTOR AND SET TO MAX
		lprow.push_back(*(row.begin()+idx));
		lpcol.push_back(*(col.begin()+idx));
		*(dist.begin()+idx) = 100000;
		i++;

		n++;
	}
	
	return lprow.size();
}

void pointMinDistPoints(std::vector<int> row, std::vector<int> col, std::vector<int> &min_keys, std::vector<int> &dist){

	int i, j;
	int row_size = row.size();
	int min_dist;

	// DECLARE VECTOR FOR STORING DISTANCES BETWEEN POINT AND POINTS
	std::vector<int> local_dist(row_size);	

	for(i = 0; i < row_size; i++){

		// SET DISTANCE TO CURRENT POINT ABOVE POSSIBLE MIN
		*(local_dist.begin() + i) = 1000000;

		// FIND DISTANCES BETWEEN POINT AND ALL OTHER POINTS - BELOW CURRENT POINT		
		for(j = 0; j < i; j++){
			
			// FIND DISTANCE
			*(local_dist.begin() + j) =
				round(std::sqrt(std::pow(double (*(row.begin() + i) - *(row.begin() + j)), 2) + std::pow(double (*(col.begin() + i) - *(col.begin() + j)), 2)));
		}

		// FIND DISTANCES BETWEEN POINT AND ALL OTHER POINTS - ABOVE CURRENT POINT		
		for(j = i+1; j < row_size; j++){

			// FIND DISTANCE
			*(local_dist.begin() + j) =
				round(std::sqrt(std::pow(double (*(row.begin() + i) - *(row.begin() + j)), 2) + std::pow(double (*(col.begin() + i) - *(col.begin() + j)), 2)));
		}

//		for(int i = 0;i < local_dist.size();i++) Rcpp::Rcout << local_dist[i] << std::endl;

		// SAVE DISTANCE IN VECTOR
		*(dist.begin() + i) = *min_element(local_dist.begin(), local_dist.end());

		// GET INDEX OF POINT AT MIN DISTANCE 
		min_dist = std::distance(local_dist.begin(), min_element(local_dist.begin(), local_dist.end()));

		// SAVE NEAREST POINT IN VECTOR
		*(min_keys.begin() + i) = min_dist;
	}
}

double quadAspectRatio(std::vector<int> &prow, std::vector<int> &pcol){

	// SIDE LENGTHS
	double side1 = std::sqrt((prow[0] - prow[1])*(prow[0] - prow[1]) + (pcol[0] - pcol[1])*(pcol[0] - pcol[1]));
	double side2 = std::sqrt((prow[1] - prow[2])*(prow[1] - prow[2]) + (pcol[1] - pcol[2])*(pcol[1] - pcol[2]));

	// FIND MIN SIDE
	if(side1 < side2) return side1 / side2;
	return side2 / side1;
}

int quadFillColor(Rcpp::IntegerMatrix &mat, int nrow, std::vector<int> &row, std::vector<int> &col){

	// FIND POINT MID WAY BETWEEN POINTS 1 AND 3
	int row_mid = (row[0] + row[2]) / 2;
	int col_mid = (col[0] + col[2]) / 2;
	
	// RETURN COLOR IN BINARY MATRIX AT THAT POINT
	return *(mat.begin() + row_mid + col_mid*nrow);
}

double evalQuadFit(std::vector<int> &row, std::vector<int> &col, std::vector<int> &prow, std::vector<int> &pcol){

	int q_i = 0;			// Initial quad vector index
	int cql1_i = -1;		// Initial current corresponding quad line indices
	int cql2_i = -1;		// Initial current corresponding quad line indices
	double qdist_sum = 0;	// Cumulative sum of distances from each point to corresponding quad line
	int qdist_ct = 0;		// Number of distances measured
	int rpt = 0;			// Repeat count, loop is repeated twice in case quad lines do not start at first contour point
	bool ibreak = false;	// Set internal break

	int i, j;
	int drow, dcol;
	int add_r_it, add_c_it;
	int max_it, qdiffsum;
	int x, y;
	double qdist_mean;

	i = 0;
	while(i <= (row.size()-2)){

		// Get row,col points
		drow = row[i+1]-row[i];
		dcol = col[i+1]-col[i];

		//Rcpp::Rcout << row[i] << ", " << col[i] << std::endl;
		//Rcpp::Rcout << "\t" << drow << ", " << dcol << std::endl;

		// Set magnitude to add at each iteration
		if(drow == 0){add_r_it = 0;}else{add_r_it = copysign(1, drow);}
		if(dcol == 0){add_c_it = 0;}else{add_c_it = copysign(1, dcol);}

		// Set maximum number of iterations
		if(i == (row.size()-2)){
			max_it = std::max(std::abs(drow), std::abs(dcol));
		}else{
			max_it = std::max(std::abs(drow), std::abs(dcol)) - 1;
		}

		for(j = 0;j <= max_it;j++){
			
			// Find x,y coords at pixel spacing
			x = row[i] + j*add_r_it;
			y = col[i] + j*add_c_it;

			// Find simple distance to quad point to see if it is the same point
			qdiffsum = std::abs(prow[q_i]-x) + std::abs(pcol[q_i]-y);

			//Rcpp::Rcout << "\t\t" << x << ", " << y << " (" << qdiffsum << ")" << std::endl;
			//Rcpp::Rcout << "\t\t" << cql1_i << ", " << cql2_i << std::endl;

			// Set current corresponding quad index
			if(qdiffsum == 0){
				cql1_i = cql1_i + 1;
				cql2_i = cql1_i + 1;
				if(cql1_i == 4){
					ibreak = true;
					break;
				}
				if(cql2_i == 4) cql2_i = 0;
				q_i = q_i + 1;
				if(q_i == 4) q_i = 0;
			}

			// Skip if no corresponding quad line points found
			if(cql1_i == -1) continue;

			// Find distance between point and quad line
			qdist_sum += distPointToLine(x, y, prow[cql1_i], pcol[cql1_i], prow[cql2_i], pcol[cql2_i]);
			qdist_ct++;
		}		

		if(ibreak == true) break;
		
		if(i == (row.size()-2)){
			rpt++;
			i = 0;
		}else{
			i++;
		}
		
		if(rpt == 2) break;
	}

	qdist_mean = double (qdist_sum / qdist_ct);

	//Rcpp::Rcout << qdist_sum << ", " << qdist_mean << ", " << qdist_ct << std::endl;

	return qdist_mean;
}

int pointMaxDistLine(int l1x, int l1y, int l2x, int l2y, std::vector<int> x, std::vector<int> y, std::vector<int> excl){

	unsigned int i;

	// DECLARE VECTOR FOR STORING DISTANCES
	std::vector<int> dist(x.size());

	// MEASURE SUM OF DISTANCES BETWEEN TWO LINE POINTS AND EACH POINT IN VECTOR
	for(i = 0;i < x.size();i++) dist[i] = 
		round(std::sqrt((l1x - x[i])*(l1x - x[i]) + (l1y - y[i])*(l1y - y[i])) + std::sqrt((l2x - x[i])*(l2x - x[i]) + (l2y - y[i])*(l2y - y[i])));

	// SET EXCLUDE POINTS TO 0 SO THEY WONT BE SELECTED AS MAX
	for(i = 0;i < excl.size();i++) dist[excl[i]] = 0;

	// RETURN INDEX OF POINT VECTOR AT MAX DISTANCE
	return std::distance(dist.begin(), max_element(dist.begin(), dist.end()));
}

void pointOnLine(int lx1, int ly1, int lx2, int ly2, int px, int py, double &lpx, double &lpy){
	
	// CHECK THAT POINTS DEFINING LINE ARE NOT COINCIDENT
	double lsum = (lx2 - lx1)*(lx2 - lx1) + (ly2 - ly1)*(ly2 - ly1);
	
	if(lsum == 0) return;

	// SOLVE FOR POSITION OF POINT ON LINE
	double u = ((px-lx1)*(lx2-lx1) + (py-ly1)*(ly2-ly1)) / lsum;
	lpx = lx1 + u*(lx2 - lx1);
	lpy = ly1 + u*(ly2 - ly1);
}

double pointSeparationRatio(int x1, int y1, int x2, int y2, std::vector<int> x, std::vector<int> y){

	unsigned int i;
	int same = 0;
	int diff = 0;
	double lpx = 0;
	double lpy = 0;
	double vecx_i = 0;
	double vecy_i = 0;
	double vecx, vecy, min_ratio;

	for(i = 0;i < x.size();i++){

		// SKIP SAME POINTS
		if(x[i] == x1 && y[i] == y1) continue;
		if(x[i] == x2 && y[i] == y2) continue;

		// FIND POINT ON LINE AT ORTHOGONAL VECTOR TO POINT X,Y
		pointOnLine(x1, y1, x2, y2, x[i], y[i], lpx, lpy);
		
		// SKIP POINTS ON LINE
		//if(x[i] - lpx == 0 && y[i] - lpy == 0) continue;
		
		// FIND VECTOR TO POINT ON LINE
		vecx = x[i] - lpx;
		vecy = y[i] - lpy;

		// SKIP POINTS CLOSE TO LINE
		if(std::abs(vecx) + std::abs(vecy) < 4) continue;
		
		// SET INITIAL VECTOR IF NOT SET
		if(vecx_i == 0 && vecy_i == 0){vecx_i = vecx;vecy_i = vecy;}
		
		//Rcpp::Rcout << i << ") " << "x[i]: " << x[i] << " y[i]: " << y[i] << " lpx: " << lpx << " lpy: " << lpy << " vecx_i: " << vecx_i << "; vecy_i: " << vecy_i << "; vecx: " << vecx << "; vecy:" << vecy << std::endl;

		if(vecx_i == 0){
			if(vecy_i < 0 && vecy < 0){same++;continue;}
			if(vecy_i > 0 && vecy > 0){same++;continue;}
			diff++;
			continue;
		}
		if(vecy_i == 0){
			if(vecx_i < 0 && vecx < 0){same++;continue;}
			if(vecx_i > 0 && vecx > 0){same++;continue;}
			diff++;
			continue;
		}

		if(vecx_i < 0 && vecx < 0 && vecy_i < 0 && vecy < 0){same++;continue;}
		if(vecx_i > 0 && vecx > 0 && vecy_i > 0 && vecy > 0){same++;continue;}
		if(vecx_i < 0 && vecx < 0 && vecy_i > 0 && vecy > 0){same++;continue;}
		if(vecx_i > 0 && vecx > 0 && vecy_i < 0 && vecy < 0){same++;continue;}
		diff++;
	}

	if(same == 0) same = 1;
	if(diff == 0) diff = 1;
	min_ratio = std::min((double) same/diff, (double) diff/same);

	//Rcpp::Rcout << "Same: " << same << " Diff: " << diff << " same/diff: " << (double) same/diff << " diff/same: " << (double) diff/same << " " << min_ratio << std::endl;

	return min_ratio;
}

int pointMaxAreaQuad(int x1, int y1, int x2, int y2, int x3, int y3, std::vector<int> x, std::vector<int> y){

	//int d, i, nx1, ny1, nx2, ny2, nx3, ny3;
	unsigned int i;
	int xtemp, ytemp;
	//float alpha, beta, gamma;
	double ratio12, ratio13, ratio23;
	
	// FIND THE TWO OF THE THREE POINTS THAT BISECT THE QUADRILATERAL BY COUNTING NUMBER OF POINTS
	// ON EITHER SIDE OF THE LINE BETWEEN EACH POINT PAIR
	ratio12 = pointSeparationRatio(x1, y1, x2, y2, x, y);
	ratio23 = pointSeparationRatio(x2, y2, x3, y3, x, y);
	ratio13 = pointSeparationRatio(x1, y1, x3, y3, x, y);
	
	// THE PAIR WITH THE HIGHER RATIO SHOULD BE THE ONES BISECTING THE QUAD
	// MAKE BISECTING PAIR SEPARATED BY THIRD POINT	
	if(ratio12 > ratio13 && ratio12 > ratio23){
		xtemp = x2;
		ytemp = y2;
		x2 = x3;
		y2 = y3;
		x3 = xtemp;
		y3 = ytemp;
	}
	if(ratio23 > ratio13 && ratio23 > ratio12){
		xtemp = x1;
		ytemp = y1;
		x1 = x2;
		y1 = y2;
		x2 = xtemp;
		y2 = ytemp;
	}

	//Rcpp::Rcout << "Ratio12: " << ratio12 << "; Ratio13: " << ratio13 << "; Ratio23: " << ratio23 << std::endl;

	// DECLARE VECTORS
	std::vector<double> area(x.size());

	// FIND AREA OF QUADRILATERAL IF EACH POINT IS ADDED TO THE POINT SEQUENCE
	for(i = 0;i < x.size();i++){

		// SKIP FIRST THREE POINTS
		if(x[i] == x1 && y[i] == y1) continue;
		if(x[i] == x2 && y[i] == y2) continue;
		if(x[i] == x3 && y[i] == y3) continue;

		// CHECK IF POINT IS WITHIN THE FIRST THREE POINTS
		//alpha = ((y2 - y3)*(x[i] - x3) + (x3 - x2)*(y[i] - y3)) /
		//		((y2 - y3)*(x1 - x3) + (x3 - x2)*(y1 - y3));
		//beta  = ((y3 - y1)*(x[i] - x3) + (x1 - x3)*(y[i] - y3)) /
		//	    ((y2 - y3)*(x1 - x3) + (x3 - x2)*(y1 - y3));
		//gamma = 1.0f - alpha - beta;

		// EXCLUDE POINTS WITHIN TRIANGLE OF FIRST THREE POINTS
		//if(alpha > 0 && beta > 0 && gamma > 0) continue;

		area[i] = std::abs(((x1*y2 - x2*y1) + (x2*y3 - x3*y2) + (x3*y[i] - x[i]*y3) + (x[i]*y1 - x1*y[i])) / 2);
	}

	// RETURN INDEX OF POINT VECTOR AT MAX DISTANCE
	return std::distance(area.begin(), max_element(area.begin(), area.end()));
}

int pointMaxAreaTriangle(int x1, int y1, int x2, int y2, std::vector<int> x, std::vector<int> y){

	unsigned int i;
	
	// DECLARE VECTOR FOR STORING AREAS
	std::vector<int> area(x.size());

	// FIND AREA OF TRIANGLE IF EACH POINT IS ADDED TO THE POINT SEQUENCE
	for(i = 0;i < x.size();i++) area[i] = std::abs((x1*(y2 - y[i]) + x2*(y[i] - y1) + x[i]*(y1 - y2)) / 2);

	// RETURN INDEX OF POINT VECTOR AT MAX DISTANCE
	return std::distance(area.begin(), max_element(area.begin(), area.end()));
}

int pointMaxDistPoint(int px, int py, std::vector<int> x, std::vector<int> y){

	unsigned int i;

	// DECLARE VECTOR FOR STORING DISTANCES
	std::vector<int> dist(x.size());

	// MEASURE DISTANCES BETWEEN POINT AND EACH VECTOR POINT
	for(i = 0;i < x.size();i++) dist[i] = round(std::sqrt((px - x[i])*(px - x[i]) + (py - y[i])*(py - y[i])));

	// RETURN INDEX OF POINT VECTOR AT MAX DISTANCE
	return std::distance(dist.begin(), max_element(dist.begin(), dist.end()));
}

void contourQuadApprox(std::vector<int> &prow, std::vector<int> &pcol, int &poly_len, 
	std::vector<int> &row, std::vector<int> &col, int num_vertices, int approx_thresh = 15, 
	int debug = 0){

	int i, n, key;
	std::vector<int> pkey(num_vertices);

	// START WITH POLYGON LENGTH AT ZERO
	poly_len = 0;

	// SET FIRST POINT
	pkey[0] = 0;

	// FIND EXTREMAL PAIR - THESE SHOULD BE OPPOSING CORNERS AS LONG AS STARTING POINT IS 
	// NOT CONSTRAINED TO A PARTICULAR POINT
	n = 0;
	while(n < 10){

		// FIND POINT AT MAXIMUM DISTANCE FROM P1
		key = pointMaxDistPoint(row[pkey[0]], col[pkey[0]], row, col);
		
		// MAKE MAX DISTANCE POINT P2
		pkey[1] = key;

		// FIND POINT AT MAXIMUM DISTANCE FROM P2
		key = pointMaxDistPoint(row[pkey[1]], col[pkey[1]], row, col);
		
		// POINT PAIR ARE AT RECIPROCAL MAXIMAL DISTANCE
		if(pkey[0] == key) break;

		pkey[0] = key;

		n++;
	}

	// FIND REMAINING VERTICES
	// FIND POINT AT MAXIMUM DISTANCE FROM LINE BETWEEN TWO POINTS - THIS WILL BE ANOTHER CORNER
	key = pointMaxDistLine(row[pkey[1]], col[pkey[1]], row[pkey[0]], col[pkey[0]], row, col, pkey);
	pkey[2] = key;
	//key = pointMaxAreaTriangle(row[pkey[0]], col[pkey[0]], row[pkey[1]], col[pkey[1]], row, col);
	//pkey[2] = key;
	
	// FIND POINT AT...
	//Rcpp::Rcout << "Size: " << row.size() << std::endl;
	signed int row_size = row.size();
	if(row_size < approx_thresh){
		key = pointMaxDistPoint(row[pkey[2]], col[pkey[2]], row, col);
		pkey[3] = key;
	}else{
		key = pointMaxAreaQuad(row[pkey[0]], col[pkey[0]], row[pkey[1]], col[pkey[1]], row[pkey[2]], col[pkey[2]], row, col);
		pkey[3] = key;
	}

	if(debug == 1){
//		for(i = 0; i < 4; i++) Rcpp::Rcout << "Corners: " << row[pkey[i]] << ' ' << col[pkey[i]] << std::endl;
	}

	// SORT POLY KEYS
	std::sort(pkey.begin(), pkey.end());

	// RETURN POLY POINTS IN SAME ORDER AS CONTOUR
	prow[0] = row[pkey[0]];
	pcol[0] = col[pkey[0]];
	for(i = 1; i < num_vertices; i++){

		// SET POINTS
		prow[i] = row[pkey[i]];
		pcol[i] = col[pkey[i]];

		// ADD DISTANCE BETWEEN SUCCESSIVE POINTS
		poly_len += round(std::sqrt(std::pow(double (row[pkey[i-1]] - row[pkey[i]]), 2) + std::pow(double (col[pkey[i-1]] - col[pkey[i]]), 2)));
	}

	poly_len += round(std::sqrt(std::pow(double (row[pkey[pkey.size()-1]] - row[pkey[0]]), 2) + std::pow(double (col[pkey[pkey.size()-1]] - col[pkey[0]]), 2)));
}

void findNextContour(Rcpp::IntegerMatrix &mat, int nrow, int ncol, std::vector<int> &row, std::vector<int> &col, int &contour_len, int &scan_start, 
	int perim_min, int perim_max, std::vector<int> &p_inter){

// If last point in contour forms a straight line between the second to last point and the
// starting point, this point will still be present in contour. Linearity is not checked
// between the second to last point and the starting point.

	// FILL CONTOUR VECTORS WITH NULL VALUE
	row.clear();
	col.clear();
	
	// SET PIXEL PERIMETER TO ZERO
	contour_len = 0;

	unsigned int i;
	int j, f, add;
	int n = 0;
	int p_inter_prev_r = 0;
	int p_inter_prev_c = 0;

	// FIND CONTOUR STARTING POINT
	unsigned int scan_start_u = scan_start;
	unsigned int nrow_u = nrow;
	for(i = scan_start_u; i < nrow_u; i++){
		for(j = 0; j < ncol; j++){
			if(mat(i,j) == 1){
				row.push_back(i);
				col.push_back(j);
				contour_len++;
				n++;
				mat(i,j) = 0;
				j = ncol;
				i = nrow_u;
			}
		}
	}

	// IF NO CONTOUR START FOUND, RETURN EMPTY VECTORS AND FINAL SCAN START
	if(row.size() == 0){
		scan_start = nrow;
		return;
	}

	// IF CONTOUR START IS LAST ELEMENT, RETURN EMPTY VECTORS AND FINAL SCAN START
	if(col[0] == ncol-1 && row[0] == nrow-1){
		contour_len = 0;
		scan_start = nrow;
		mat(nrow-1, ncol-1) = 0;
		return;
	}
	
	// SET START ROW FOR NEXT RUN
	scan_start = row[0];

	//Rcpp::Rcout << "Start point: " << row[0] << ", " << col[0] << std::endl;

	// TRACE CONNECTED POINTS
	while(contour_len < perim_max){
		
		f = 0;
		for(i = 0;i < p_inter.size();i+=2){

			// CHECK THAT NEXT POINT DOES NOT EXCEED MATRIX BOUNDS
			if(row[n-1] + p_inter[i] < 0 || row[n-1] + p_inter[i] > nrow-1) continue;
			if(col[n-1]+p_inter[i+1] < 0 || col[n-1]+p_inter[i+1] > ncol-1) continue;
			
			// IF NEIGHBOR POINT IS THE FIRST CONTOUR POINT SET TO RETURN CONTOUR AFTER TRYING ALL OTHER NEIGHBORS
			if(contour_len > 2 && row[n-1]+p_inter[i] == row[0] && col[n-1]+p_inter[i+1] == col[0]){
				f = -1;
				continue;
			}

			//Rcpp::Rcout << "\tTry " << p_inter[i] << ", " << p_inter[i+1] << " : " << mat(row[n-1]+p_inter[i], col[n-1]+p_inter[i+1]) << std::endl;

			// IF NEIGHBOR IS NOT 1, CONTINUE
			if(mat(row[n-1]+p_inter[i], col[n-1]+p_inter[i+1]) == 0) continue;
			
			// SET POINT IN MATRIX TO 0
			mat(row[n-1]+p_inter[i], col[n-1]+p_inter[i+1]) = 0;

			//Rcpp::Rcout << "\tNext point: " << row[n-1]+p_inter[i] << ", " << col[n-1]+p_inter[i+1] << " (contour_len: " << contour_len << ")" << std::endl;

			// ADD NEW POINT BY DEFAULT
			add = 1;

			// CHECK IF NEW INTERVAL IS THE SAME AS THE PREVIOUS INTERVAL
			if(n > 1 && p_inter[i] == p_inter_prev_r && p_inter[i+1] == p_inter_prev_c) add = 0;

			if(add == 1){
				// ADD NEW POINT
				col.push_back(col[n-1]+p_inter[i+1]);
				row.push_back(row[n-1]+p_inter[i]);
				p_inter_prev_r = p_inter[i];
				p_inter_prev_c = p_inter[i+1];
				n++;
			}else{
				// INCREASE INTERVAL
				col[n-1] = col[n-1]+p_inter[i+1];
				row[n-1] = row[n-1]+p_inter[i];
			}

			f = 1;
			break;
		}
		
		// IF ONE OF THE NEIGHBORS WAS THE STARTING POINT AND NO OTHER CONTOUR POINTS WERE FOUND, RETURN
		if(f == -1){
			//Rcpp::Rcout << "\tf == -1" << std::endl;
			if(contour_len > perim_min) return;
			contour_len = 0;
			return;
		}
		
		// IF NO NEXT POINT WAS FOUND THEN RETURN WITH EMPTY VECTOR
		if(f == 0){
			//Rcpp::Rcout << "\tNo next point found" << std::endl;
			contour_len = 0;
			return;			
		}

		contour_len++;
	}

	// MAXIMUM PERIMETER REACHED WITHOUT COMPLETE CONTOUR, RETURN EMPTY VECTORS
	contour_len = 0;
	
//	Rcpp::Rcout << "Max perimeter reached" << std::endl;

	return;
}

// [[Rcpp::export]]
Rcpp::IntegerMatrix generateQuads(Rcpp::IntegerMatrix binary_mat, Rcpp::IntegerMatrix edge_mat, 
	int perim_min, int perim_max, double quad_fit_max, double poly_cont_min, 
	double poly_cont_max, double poly_asp_min, int approx_thresh){

	unsigned int i;
	int scan_start = 0;
	int nrow = edge_mat.nrow();
	int ncol = edge_mat.ncol();
	int contour_len = 0;
	int num_vertices = 4;
	int poly_len = 0;
	double poly_cont_frac;

	std::vector<int> col(1024);
	std::vector<int> row(1024);
	std::vector<int> prow(4);
	std::vector<int> pcol(4);
	std::vector<int> quads_row;
	std::vector<int> quads_col;

	Rcpp::IntegerMatrix mat(nrow, ncol);
	std::copy(edge_mat.begin(), edge_mat.end(), mat.begin());

	// DEFINE PATHS TO SEARCH FOR CONNECTED POINTS
	std::vector<int> p_inter(16);

	p_inter[0] = -1;p_inter[1] = 0;
	p_inter[2] = 0;p_inter[3] = 1;
	p_inter[4] = 1;p_inter[5] = 0;
	p_inter[6] = 0;p_inter[7] = -1;
	p_inter[8] = -1;p_inter[9] = 1;
	p_inter[10] = 1;p_inter[11] = 1;
	p_inter[12] = 1;p_inter[13] = -1;
	p_inter[14] = -1;p_inter[15] = -1;

	int n = -1;
	while(scan_start < nrow){

		n++;

		// FIND NEXT CONTOUR
		findNextContour(mat, nrow, ncol, row, col, contour_len, scan_start, perim_min, perim_max, p_inter);
		
		// IF END OF IMAGE IS REACHED, RETURN
		if(scan_start >= nrow) break;

		// IF THE FOUND CONTOUR WAS INCOMPLETE, CONTINUE
		if(contour_len == 0){
//			Rcpp::Rcout << "No contour found." << std::endl;
			continue;
		}

		//for(int i = 0;i < row.size();i++) Rcpp::Rcout << row[i] << "," << col[i] << ";";
		//Rcpp::Rcout << "n";

		// CONTOUR POLYGON APPROXIMATION
		contourQuadApprox(prow, pcol, poly_len, row, col, num_vertices, approx_thresh);

		// TEST WHETHER CONSECUTIVE POINTS OF POLYGON ARE THE SAME - THIS CAN HAPPEN FOR POOR FITS
		if(prow[2] == prow[3] && pcol[2] == pcol[3]) continue;
		if(prow[1] == prow[2] && pcol[1] == pcol[2]) continue;
		if(prow[0] == prow[1] && pcol[0] == pcol[1]) continue;
		if(prow[0] == prow[3] && pcol[0] == pcol[3]) continue;

		//for(int i = 0;i < prow.size();i++) Rcpp::Rcout << prow[i] << "," << pcol[i] << ";";
		//Rcpp::Rcout << "n";
		//for(int i = 0;i < row.size();i++) Rcpp::Rcout << row[i] << "," << col[i] << ";";
		//Rcpp::Rcout << "n";

		// TEST WHETHER QUADRANGLE IS GOOD FIT TO POINTS
		if(evalQuadFit(row, col, prow, pcol) > quad_fit_max) continue;

		// GET FRACTION OF POLYGON PERIMETER VERSUS CONTOUR PERIMETER
		poly_cont_frac = (double) (poly_len - contour_len) / contour_len;

//		Rcpp::Rcout << "Poly_len - Contour_len: " << (double) (poly_len - contour_len) / contour_len << std::endl;
//		Rcpp::Rcout << "Poly_len/Contour_len: " << (double) poly_len / contour_len << std::endl;
//		Rcpp::Rcout << "Pow: " << std::pow(poly_cont_frac, 2) << std::endl;
//		Rcpp::Rcout << "Polygon/contour perimeter less than threshold " << poly_cont_frac << "; Contour length: " << contour_len << std::endl;

		// TEST WHETHER POLYGON PERIMETER IS ABOVE THRESHOLD OF CONTOUR PERIMETER
		if(poly_cont_frac < poly_cont_min || poly_cont_frac > poly_cont_max){
//			Rcpp::Rcout << std::endl << n << ")" << std::endl;
//			for(int i = 0;i < prow.size();i++) Rcpp::Rcout << prow[i] << " " << pcol[i] << std::endl;
//			Rcpp::Rcout << std::endl << n << ")" << " Polygon contour (" << poly_cont_frac << ") outside thresholds" << std::endl;
			continue;
		}

		// IF QUAD ENCLOSES WHITE, SKIP TO NEXT QUAD
		if(quadFillColor(binary_mat, nrow, prow, pcol) == 1){
//			Rcpp::Rcout << std::endl << n << ")" << " Quad encloses white" << std::endl;
			continue;
		}

		// IF QUAD ASPECT RATIO IS LESS THAN MIN, SKIP TO NEXT QUAD
		if(quadAspectRatio(prow, pcol) < poly_asp_min){
//			Rcpp::Rcout << std::endl << n << ")" << std::endl;
//			Rcpp::Rcout << "Quad aspect ratio (" << quadAspectRatio(prow, pcol) << ") is less than minimum." << std::endl;
			continue;
		}

		// TEST WHETHER POLYGON IS CONVEX?
		
		// ADD POLY POINTS TO QUAD VECTORS
		for(i = 0;i < prow.size();i++){
			quads_row.push_back(prow[i]);
			quads_col.push_back(pcol[i]);
		}

//		Rcpp::Rcout << "Contour length: " << contour_len << std::endl;
//		Rcpp::Rcout << "Poly approx length: " << poly_len << std::endl;
//		Rcpp::Rcout << "Poly / Contour length: " << poly_cont_frac << std::endl;

//		if(poly_cont_frac < poly_cont_min) Rcpp::Rcout << "Poly length less than threshold" << std::endl;

		//for(int i = 0;i < row.size();i++) Rcpp::Rcout << row[i] << ' ' << col[i] << std::endl;
	}

	// CREATE QUADS MATRIX
	Rcpp::IntegerMatrix quads(quads_row.size(), 2);

	// ADD VALUES TO QUADS MATRIX
	for(i = 0;i < quads_row.size();i++){
		quads(i, 0) = quads_row[i];
		quads(i, 1) = quads_col[i];
	}
	
	// ADD 1 SO THAT INDICES CORRESPOND WITH R CONVENTION
	//std::transform(quads.begin(), quads.end(), quads.begin(), std::bind1st(std::plus<int>(), 1));

	return quads;
}

// [[Rcpp::export]]
Rcpp::IntegerMatrix intCornersFromQuads(Rcpp::IntegerMatrix quads, int max_dist = 15){

	int i, j, k;
	int nrow_int = 0;
	int nrow = quads.nrow();
	std::vector<int> row(nrow);
	std::vector<int> row_copy(nrow);
	std::vector<int> col(nrow);
	std::vector<int> min_keys(nrow);
	std::vector<int> dist(nrow);

	// SUBTRACT 1 SO THAT INDICES CORRESPOND WITH C++ CONVENTION
	//std::transform(quads.begin(), quads.end(), quads.begin(), std::bind1st(std::plus<int>(), -1));

	// COPY QUAD VALUES TO VECTORS
	std::copy(quads.begin(), quads.begin()+nrow, row.begin());
	std::copy(row.begin(), row.begin()+nrow, row_copy.begin());
	std::copy(quads.begin()+nrow, quads.begin()+2*nrow, col.begin());

	// FIND MIN DISTANCE TO OTHER POINT IN SET FOR EACH QUAD POINT
	pointMinDistPoints(row, col, min_keys, dist);

	// FILTER MINIMUM PAIRINGS
	for(i = 0; i < nrow; i++){

		// REMOVE MINIMUM PAIRS WITHIN THE SAME QUAD (WILL AUTOMATICALLY FLOOR WHEN DIVIDING INTEGERS)
		if((min_keys[i] / 4) == (i / 4)){
			*(dist.begin()+i) = 100000;
			continue;
		}

		// REMOVE MARKED DUPLICATES
		if(*(row_copy.begin()+i) == -1){
			*(dist.begin()+i) = 100000;
			continue;
		}

		// FLAG DUPLICATE PAIRS
		for(j = 0; j < nrow; j++){
			if(i != min_keys[j]) continue;
			if(j != min_keys[i]) continue;

			// ONCE DUPLICATE IS FOUND IT IS A KNOWN RECIPROCAL PAIRING - REMOVE ALL OTHER INSTANCES THAT ARE NOT THE SAME ENTRIES
			// ONLY POINTS IN RECIPROCAL MINIMUM PAIRINGS ARE ALLOWED
			for(k = 0; k < nrow; k++){
				if(k == i || k == j) continue;
				if(min_keys[k] == i) *(dist.begin()+k) = 100000;
				if(min_keys[k] == min_keys[i]) *(dist.begin()+k) = 100000;
			}

			*(row_copy.begin()+j) = -1;
			break;
		}
	}

	// FIND NUMBER OF ELEMENTS OVER THRESHOLD
	for(i = 0; i < nrow;i++) if(*(dist.begin()+i) < max_dist) nrow_int++;

	// CREATE MATRIX FOR INTERNAL CORNERS
	Rcpp::IntegerMatrix int_corners(nrow_int, 2);
	
	// FILL INTERNAL CORNER MATRIX WITH VALUES
	j = 0;
	for(i = 0; i < nrow;i++){

		if(*(dist.begin()+i) >= max_dist) continue;

		// SAVE AVERAGE OF MIN ROW AND COL AS INTERNAL CORNER
		*(int_corners.begin()+j) = round((*(row.begin()+i) + row[min_keys[i]])/2);
		*(int_corners.begin()+j+nrow_int) = round((*(col.begin()+i) + col[min_keys[i]])/2);
		j++;
	}
	
//	for(int i = 0;i < row.size();i++) Rcpp::Rcout << row[i] << " " << col[i] << " to " << row[min_keys[i]] << " " << col[min_keys[i]] << " (" << min_keys[i] << ") " << ": " << dist[i] << std::endl;

	// ADD 1 SO THAT INDICES CORRESPOND WITH R CONVENTION
	//std::transform(int_corners.begin(), int_corners.end(), int_corners.begin(), std::bind1st(std::plus<int>(), 1));

	return int_corners;
}

// [[Rcpp::export]]
Rcpp::IntegerMatrix orderCorners(Rcpp::IntegerMatrix int_corners, int nx, int ny){

	unsigned int k;
	int dim_max, dim_min, dim1, dim2, i, j, n, n_break, n_set, lp_size, row_mean, col_mean, min_prow;
	int both_neg = 0;
	int row_sum = 0;
	int col_sum = 0;
	int poly_len = 0;
	int num_vertices = 4;
	int nrow = int_corners.nrow();

	double ratio_threshold, diag_dist;
	double init_ratio_thresh = 0.0000000001;
	std::vector<int> col(nrow);
	std::vector<int> row(nrow);
	std::vector<int> prow(4);
	std::vector<int> pcol(4);
	std::vector<int> prowv(4);
	std::vector<int> pcolv(4);
	std::vector<int> min_keys(5);
	std::vector<int> dist_tl(5);
	std::vector<int> dist(4);
	std::vector<int> frow(4);
	std::vector<int> fcol(4);
	std::vector<int> frowc(4);
	std::vector<int> fcolc(4);
	Rcpp::IntegerMatrix ordered_corners(nrow, 2);

	// FIND MAX OF NX AND NY
	std::vector<int> lprow(std::max(nx, ny));
	std::vector<int> lpcol(std::max(nx, ny));
	std::vector<int> s1row(std::max(nx, ny));
	std::vector<int> s1col(std::max(nx, ny));
	std::vector<int> s2row(std::max(nx, ny));
	std::vector<int> s2col(std::max(nx, ny));	

	// COPY MATRIX VALUES TO ROW AND COL VECTORS
	std::copy(int_corners.begin(), int_corners.begin()+nrow, row.begin());
	std::copy(int_corners.begin()+nrow, int_corners.begin()+2*nrow, col.begin());

	// FIND CENTER OF GRID
	for(k = 0;k < row.size();k++) row_sum += row[k];
	for(k = 0;k < col.size();k++) col_sum += col[k];
	row_mean = round(row_sum / row.size());
	col_mean = round(col_sum / col.size());
//	Rcpp::Rcout << row_mean << ", " << col_mean << std::endl;

	// CONTOUR POLYGON APPROXIMATION - ORDER NOT NECESSARILY MEANINGFUL
	contourQuadApprox(prow, pcol, poly_len, row, col, num_vertices, 15);

//	for(i = 0;i < prow.size();i++) Rcpp::Rcout << prow[i] << ", " << pcol[i] << std::endl;

	// FIND VECTOR FROM CENTER TO EACH CORNER POINT
	for(k = 0;k < prow.size();k++){
		prowv[k] = prow[k] - row_mean;
		pcolv[k] = pcol[k] - col_mean;
		if(prowv[k] < 0 && pcolv[k] < 0){
			*frow.begin() = prow[k];
			*fcol.begin() = pcol[k];
			both_neg++;
		}
	}
	
	// FIND POINT WITH MINIMUM ROW
	min_prow = std::distance(prow.begin(), min_element(prow.begin(), prow.end()));

	// CHOOSE POINT WITH MINIMUM ROW VALUE
	if(both_neg < 1 || both_neg > 1){
		*frow.begin() = prow[min_prow];
		*fcol.begin() = pcol[min_prow];
	}

	// COPY FIRST POINT
	*frowc.begin() = *frow.begin();
	*fcolc.begin() = *fcol.begin();

//	for(i = 0;i < prowv.size();i++) Rcpp::Rcout << prowv[i] << ", " << pcolv[i] << std::endl;

	// FIND DISTANCE FROM TOP LEFT POINT TO OTHER POINTS
	for(i = 0;i < 4;i++) *(dist.begin()+i) = 
		round(std::sqrt(std::pow(double (*(frow.begin()) - *(prow.begin() + i)), 2) + std::pow(double (*(fcol.begin()) - *(pcol.begin() + i)), 2)));

//	for(i = 0;i < 4;i++) Rcpp::Rcout << frow[i] << ", " << fcol[i] << ": " << dist[i] << std::endl;

	// FIND GREATER DIMENSION
	if(nx >= ny){
		dim_max = nx;
		dim_min = ny;
		dim1 = 1;
		dim2 = 2;
	}else{
		dim_max = ny;
		dim_min = nx;
		dim1 = 2;
		dim2 = 1;
	}

//	Rcpp::Rcout << std::endl;
	
	// FIND DIAGONAL PAIR
	int pair_diag = -1;
	for(i = 0;i < 4;i++){
		if(dist[i] == 0) continue;

		diag_dist = 
			(std::sqrt(std::pow(double (frow[0] - row_mean), 2) + std::pow(double (fcol[0] - col_mean), 2)) + std::sqrt(std::pow(double (prow[i] - row_mean), 2) + std::pow(double (pcol[i] - col_mean), 2))) / 
			std::sqrt(std::pow(double (frow[0] - prow[i]), 2) + std::pow(double (fcol[0] - pcol[i]), 2));

//		Rcpp::Rcout << "i: " << i << "; " << prow[i] << ", " << pcol[i] << "; " << "diag_dist: " << diag_dist << std::endl;
		
		if(diag_dist < 1.0001){
			pair_diag = i;
			break;
		}
	}

//	Rcpp::Rcout << std::endl;

	// FIND MAXIMUM DIMENSION FIRST
	n = 0;
	n_set = 0;
	while(n < 9){
		ratio_threshold = 1+init_ratio_thresh*pow(10.0, n);
//		Rcpp::Rcout << "Ratio threshold: " << ratio_threshold << std::endl;

		for(i = 0;i < 4;i++){
			if(i == pair_diag) continue;
			if(dist[i] == 0) continue;

			// FIND POINTS ON LINE BETWEEN FIRST POINT AND FIRST ADJACENT CORNER
			lp_size = pointsOnLine(frow[0], fcol[0], prow[i], pcol[i], row, col, lprow, lpcol, ratio_threshold);
//			Rcpp::Rcout << "i: " << i << "; " << prow[i] << ", " << pcol[i] << "; " << "lp_size: " << lp_size << std::endl;

			if(lp_size == dim_max){
				frow[dim1] = prow[i];
				fcol[dim1] = pcol[i];
				dist[i] = 0;
				n_set = 1;
				n = 9;
				break;
			}
		}

		n++;
	}

	if(n_set == 0) return ordered_corners;

//	Rcpp::Rcout << std::endl;

	// FIND MINIMUM DIMENSION
	n = 0;
	n_set = 0;
	while(n < 9){
		ratio_threshold = 1+init_ratio_thresh*pow(10.0, n);
//		Rcpp::Rcout << "Ratio threshold: " << ratio_threshold << std::endl;

		for(i = 0;i < 4;i++){
			if(i == pair_diag) continue;
			if(dist[i] == 0) continue;

			// FIND POINTS ON LINE BETWEEN FIRST POINT AND FIRST ADJACENT CORNER
			lp_size = pointsOnLine(frow[0], fcol[0], prow[i], pcol[i], row, col, lprow, lpcol, ratio_threshold);
//			Rcpp::Rcout << "i: " << i << "; " << prow[i] << ", " << pcol[i] << "; " << "lp_size: " << lp_size << std::endl;

			if(lp_size == dim_min){
				frow[dim2] = prow[i];
				fcol[dim2] = pcol[i];
				dist[i] = 0;
				n_set = 1;
				n = 9;
				break;
			}
		}

		n++;
	}

	if(n_set == 0) return ordered_corners;

	// SET LAST POINT
	for(i = 0;i < 4;i++) if(dist[i] != 0){frow[3] = prow[i];fcol[3] = pcol[i];}

//	Rcpp::Rcout << std::endl;for(i = 0;i < 4;i++) Rcpp::Rcout << frow[i] << ' ' << fcol[i] << std::endl;

	n = 0;
	while(n < 9){

		ratio_threshold = 1+init_ratio_thresh*pow(10.0, n);
//		Rcpp::Rcout << "Ratio threshold: " << ratio_threshold << std::endl;

		// FIND POINTS ON LINE BETWEEN FIRST POINT AND CORRESPONDING NY POINT
		lp_size = pointsOnLine(frow[0], fcol[0], frow[2], fcol[2], row, col, s1row, s1col, ratio_threshold);

		// FIND POINTS ON LINE BETWEEN SECOND POINT AND CORRESPONDING NY POINT
		lp_size = pointsOnLine(frow[1], fcol[1], frow[3], fcol[3], row, col, s2row, s2col, ratio_threshold);

		// IF INCORRECT NUMBER OF NY POINTS WERE FOUND, CONTINUE WITH HIGHER THRESHOLD
		if(lp_size != ny){n++;continue;}

		n_break = 0;
		for(i = 0;i < ny;i++){

			// ADD SIDE POINTS TO ORDERED MATRIX
			*(ordered_corners.begin()+i*nx) = *(s1row.begin()+i);
			*(ordered_corners.begin()+i*nx+nrow) = *(s1col.begin()+i);
			*(ordered_corners.begin()+(i+1)*nx-1) = *(s2row.begin()+i);
			*(ordered_corners.begin()+(i+1)*nx+nrow-1) = *(s2col.begin()+i);

			// FIND POINTS ON LINE BETWEEN SIDE POINTS
			lp_size = pointsOnLine(
				*(s1row.begin()+i), *(s1col.begin()+i), *(s2row.begin()+i), *(s2col.begin()+i), 
				row, col, lprow, lpcol, ratio_threshold);

			// IF INCORRECT NUMBER OF NX POINTS WERE FOUND, CONTINUE WITH HIGHER THRESHOLD
			if(lp_size != nx){n++;n_break++;break;}

			// ADD POINTS TO ORDERED MATRIX
			for(j = 1;j < nx-1; j++){
				*(ordered_corners.begin()+i*nx+j) = *(lprow.begin()+j);
				*(ordered_corners.begin()+i*nx+nrow+j) = *(lpcol.begin()+j);
			}
		}

		// IF NY FOR LOOP WAS BROKEN FROM TOO LOW A THRESHOLD, CONTINUE WITH HIGHER THRESHOLD
		if(n_break == 1){continue;}

		return ordered_corners;
	}

	std::fill(ordered_corners.begin(), ordered_corners.end(), 0);

	return ordered_corners;
}
