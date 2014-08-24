dltTestCalibration <- function(cal.coeff, coor.2d, nx, grid.size, epipolar.reciprocal=TRUE){

	if(length(dim(coor.2d)) == 3) coor.2d <- array(coor.2d, dim=c(dim(coor.2d)[1:2], 1, dim(coor.2d)[3]))

	# REMOVE ANY SETS WITH NA
	is_na <- apply(is.na(coor.2d), c(3, 4), 'sum')
	is_na_rowsums <- rowSums(is_na)
	coor.2d <- coor.2d[, , is_na_rowsums == 0, ]

	# EMPTY VECTORS
	ipd_error <- rep(NA, 0)
	adj_pair_ipd_error <- rep(NA, 0)
	pair_dist_all <- rep(NA, 0)

	# EMPTY MATRICES
	epipolar_error <- matrix(NA, nrow=0, ncol=dim(coor.2d)[4]-1)
	adj_pair_mean_pos <- matrix(NA, nrow=0, ncol=3)
	aitr_error <- matrix(NA, nrow=0, ncol=3)
	aitr_pos <- matrix(NA, nrow=0, ncol=3)

	# FIND SECOND GRID DIMENSION
	ny <- dim(coor.2d)[1]/nx

	# LOOP THROUGH EACH SET OF GRIDS
	for(i in 1:dim(coor.2d)[3]){

		# GET 3D RECONSTRUCTED POINTS
		dlt_reconstruct <- dltReconstruct(cal.coeff, coor.2d[, , i,])

		# MAKE THEORETICAL GRID OF SAME SIZE FOR ESTIMATE COMPARISON
		coor_3d <- transformPlanarCalibrationCoordinates(tpar=rep(0, 6), nx=nx, ny=ny, sx=grid.size)
		
		# GET OPTIMAL POINT ALIGNMENT
		coor_3d_unify <- findOptimalPointAlignment(dlt_reconstruct$coor.3d, coor_3d)

		# SAVE ERROR IN REFERENCE-ESTIMATE POINT POSITION AND POSITION OF ESTIMATE POINTS
		aitr_error <- rbind(aitr_error, dlt_reconstruct$coor.3d - coor_3d_unify)
		aitr_pos <- rbind(aitr_pos, coor_3d_unify)

		# GENERATE RANDOM POINT PAIRS, NO POINTS ARE REPEATED
		p_r <- 1:(nx*ny)
		p1 <- sample(p_r, floor((nx*ny)/2), replace=F)
		p2 <- sample(p_r[-p1], floor((nx*ny)/2), replace=F)
		pairs <- matrix(NA, nrow=floor((nx*ny)/2), ncol=2)
		pairs <- cbind(p1, p2)

		# GET ADJOINING POINTS FOR FIRST ROW
		x1 <- seq(1, nx-1, by=2)
		x2 <- seq(2, nx, by=2)

		# EXPAND TO NUMBER OF ROWS AND CUMULATIVE INDEX WITH EACH ROW
		x1_m <- matrix(x1, nrow=ny, ncol=length(x1), byrow=TRUE) + nx*(0:(ny-1))
		x2_m <- matrix(x2, nrow=ny, ncol=length(x2), byrow=TRUE) + nx*(0:(ny-1))

		# GET TWO-COLUMN MATRIX OF PAIRS OF ADJOINING POINTS
		pairs_adjoin <- cbind(matrix(t(x1_m), ncol=1), matrix(t(x2_m), ncol=1))

		# FIND UNIT DISTANCE BETWEEN EACH PAIR OF POINTS
		pair_dist <- distanceGridUnits(pairs, nx)
		pair_dist_adjoin <- distanceGridUnits(pairs_adjoin, nx)
	
		# SCALE DISTANCE TO KNOWN GRID SIZE IN REAL-WORLD UNITS
		pair_dist <- pair_dist*grid.size
		pair_dist_adjoin <- pair_dist_adjoin*grid.size

		# EPIPOLAR ERROR MATRIX
		epipolar_error_m <- matrix(NA, nrow=dim(coor.2d)[1], ncol=dim(coor.2d)[4]-1)
	
		# FIND EPIPOLAR ERROR BETWEEN FIRST VIEW AND SUBSEQUENT OTHER VIEWS
		for(k in 2:dim(coor.2d)[4]) epipolar_error_m[, k-1] <- dltEpipolarDistance(p1=coor.2d[, , i, 1], p2=coor.2d[, , i, k], cal.coeff, reciprocal=epipolar.reciprocal)
	
		# ADD EPIPOLAR ERRORS TO MATRIX
		epipolar_error <- rbind(epipolar_error, epipolar_error_m)

		# ADD PAIR DISTANCES TO VECTOR
		pair_dist_all <- c(pair_dist_all, pair_dist)
	
		# FIND ERROR IN INTERPOINT DISTANCE
		for(j in 1:nrow(pairs)) ipd_error <- c(ipd_error, distancePointToPoint(dlt_reconstruct$coor.3d[pairs[j, 1], ], dlt_reconstruct$coor.3d[pairs[j, 2], ]) - pair_dist[j])
		for(j in 1:nrow(pairs_adjoin)) adj_pair_ipd_error <- c(adj_pair_ipd_error, distancePointToPoint(dlt_reconstruct$coor.3d[pairs_adjoin[j, 1], ], dlt_reconstruct$coor.3d[pairs_adjoin[j, 2], ]) - pair_dist_adjoin[j])

		# GET MEAN 3D-RECONSTRUCTED POSITION OF ADJOINING PAIRS
		for(j in 1:nrow(pairs_adjoin)) adj_pair_mean_pos <- rbind(adj_pair_mean_pos, (dlt_reconstruct$coor.3d[pairs_adjoin[j, 1], ] + dlt_reconstruct$coor.3d[pairs_adjoin[j, 2], ]) / 2)
	}

	# GET DISTANCE FROM CENTROID FOR ESTIMATE POINTS
	aitr_centroid_dist <- distancePointToPoint(colMeans(aitr_pos), aitr_pos)

	# GET DISTANCE FROM CENTROID FOR ADJOINING PAIR MEAN POSITIONS
	adj_pair_centroid_dist <- distancePointToPoint(colMeans(adj_pair_mean_pos), adj_pair_mean_pos)

	# CALCULATE DISTANCE BETWEEN EACH ESTIMATED AND REFERENCE POINT
	aitr_dist_error <- sqrt(rowSums(aitr_error^2))

	# CALCULATE AITR RMS ERROR FROM ALL TEST ORIENTATIONS
	aitr_dist_rmse <- sqrt(mean(aitr_dist_error^2))
	aitr_rmse <- sqrt(colMeans(aitr_error^2))

	# CALCULATE EPIPOLAR RMS ERROR FROM ALL TEST ORIENTATIONS
	epipolar_rmse <- sqrt(mean(rowMeans(epipolar_error)^2))

	# CALCULATE RMS ERROR OF INTERPOINT DISTANCE BASED ON ALL TEST POINT DISTANCES
	ipd_rmse <- sqrt(mean(ipd_error^2))

	l <- list(
		num.grids=dim(coor.2d)[3],
		epipolar.error=epipolar_error, 
		epipolar.rmse=epipolar_rmse, 
		ipd.error=ipd_error, 
		pair.dist=pair_dist_all,
		ipd.rmse=ipd_rmse, 
		adj.pair.ipd.error=adj_pair_ipd_error, 
		adj.pair.mean.pos=adj_pair_mean_pos, 
		adj.pair.centroid.dist=adj_pair_centroid_dist,
		aitr.error=aitr_error,
		aitr.dist.error=aitr_dist_error,
		aitr.dist.rmse=aitr_dist_rmse,
		aitr.rmse=aitr_rmse,
		aitr.pos=aitr_pos,
		aitr.centroid.dist=aitr_centroid_dist
		)
	class(l) <- 'dltTestCalibration'
	l
}

summary.dltTestCalibration <- function(object, ...){
	r <- ''

	r <- c(r, '\ndltTestCalibration Summary\n')
	
	r <- c(r, '\tNumber of grids: ', object$num.grids, '\n')
	r <- c(r, '\tNumber of points: ', length(object$ipd.error), '\n')
	r <- c(r, '\tAligned ideal to reconstructed (AITR) point position errors:\n')
	#r <- c(r, '\t\tAITR Mean Errors (X, Y, Z): ')
	#r <- c(r, format(mean(object$aitr.error[, 1])))
	#r <- c(r, ', ')
	#r <- c(r, format(mean(object$aitr.error[, 2])))
	#r <- c(r, ', ')
	#r <- c(r, format(mean(object$aitr.error[, 3])))
	#r <- c(r, '\n')
	r <- c(r, '\t\tAITR RMS Errors (X, Y, Z): ')
	r <- c(r, format(object$aitr.rmse[1]))
	r <- c(r, ', ')
	r <- c(r, format(object$aitr.rmse[2]))
	r <- c(r, ', ')
	r <- c(r, format(object$aitr.rmse[3]))
	r <- c(r, '\n')

	r <- c(r, '\t\tMean AITR Distance Error: ', format(mean(object$aitr.dist.error)), '\n')
	r <- c(r, '\t\tAITR Distance RMS Error: ', format(object$aitr.dist.rmse), '\n')

	r <- c(r, '\tInter-point distance (IPD) errors:\n')
	r <- c(r, '\t\tIPD RMS Error: ', format(object$ipd.rmse), '\n')
	r <- c(r, '\t\tIPD Mean Absolute Error: ', format(mean(abs(object$ipd.error))), '\n')
	r <- c(r, '\t\tMean IPD error: ', format(mean(object$ipd.error)), '\n')
	r <- c(r, '\tAdjacent-pair distance errors:\n')
	r <- c(r, '\t\tMean adjacent-pair distance error: ', format(mean(object$adj.pair.ipd.error)), '\n')
	r <- c(r, '\t\tMean adjacent-pair absolute distance error: ', format(mean(abs(object$adj.pair.ipd.error))), '\n')
	r <- c(r, '\t\tSD of adjacent-pair distance error: ', format(sd(object$adj.pair.ipd.error)), '\n')
	r <- c(r, '\tEpipolar errors:\n')
	r <- c(r, '\t\tEpipolar RMS Error: ', format(object$epipolar.rmse), ' px\n')
	r <- c(r, '\t\tEpipolar Mean Error: ', format(mean(object$epipolar.error)), ' px\n')
	r <- c(r, '\t\tSD of Epipolar Error: ', format(sd(object$epipolar.error)), ' px\n')

	class(r) <- "summary.dltTestCalibration"
	r
}

print.summary.dltTestCalibration <- function(x, ...) cat(x, sep='')