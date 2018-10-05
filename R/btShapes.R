btShapes <- function(scores, vectors, fcn, row.names, pcs = 1:2, n = c(3,4), m = 3, 
	pc.margin=c(0,0), phy.means = NULL, centroid.size = 1, ...){

	# Set total number of shapes
	n_shapes <- n[1]*n[2]

	# Start matrix with mean PC scores for all coordinates
	scores_bt <- matrix(colMeans(scores), n_shapes, ncol(vectors), byrow=TRUE)

	# Set min and max for scores along PC1
	pc1_range <- range(scores[, pcs[1]])
	pc1_range <- c(pc1_range[1] + pc.margin[1]*diff(pc1_range), pc1_range[2] - pc.margin[1]*diff(pc1_range))

	# Set min and max for scores along PC2
	pc2_range <- range(scores[, pcs[2]])
	pc2_range <- c(pc2_range[1] + pc.margin[2]*diff(pc2_range), pc2_range[2] - pc.margin[2]*diff(pc2_range))

	# Replace mean PC1 scores with particular scores at evenly spaced points across range
	scores_bt[, pcs[1]] <- rep(seq(from=pc1_range[1], to=pc1_range[2], length=n[1]), n[2])

	# Replace mean PC2 scores with particular scores at evenly spaced points across range
	y <- rep(NA, 0)
	for(s in 0:(n[2]-1)) y <- c(y, rep((pc2_range[2] - pc2_range[1])*(s/(n[2]-1)) + pc2_range[1], n[1]))
	scores_bt[, pcs[2]] <- t(y)

	# Set rownames
	row_names <- cbind(paste0('PC', pcs[1], '_', rep(1:n[1], n[2])), paste0('PC', pcs[2], '_', c(matrix(rep(1:n[2], n[1]), n[1], n[2], byrow=TRUE))))
	rownames(scores_bt) <- paste0(row_names[, 1], '_', row_names[, 2])
	
	if(is.null(phy.means)){

		# Back-transform PC scores by multiplying by the inverse of the eigenvector matrix
		lm_bt_temp <- scores_bt %*% solve(vectors)

	}else{
	
		# Create rows of 1s
		one <- matrix(1, n_shapes, 1)

		# Back-transform PC scores by multiplying by the inverse of the eigenvector matrix
		# Need ginv (from MASS) because eigen vectors from phyl.pca is not a square matrix
		# The first version of phyl.pca did return a square matrix but then it was changed 
		# to return a non-square matrix
		lm_bt_temp <- (scores_bt %*% ginv(vectors)) + one %*% t(phy.means)
	}
	
	# Create array to hold each backtransform shape
	lm_arr <- array(NA, dim=c(length(row.names), m, n_shapes), dimnames=list(row.names, NULL, rownames(scores_bt)))

	# Fill shape array
	for(i in 1:n_shapes){
	
		# Get shape coordinates
		xy <- matrix(lm_bt_temp[i, ], nrow=length(row.names), ncol=m, byrow=TRUE)
		
		# Center about zero
		xy <- xy - matrix(colMeans(xy), nrow(xy), ncol(xy), byrow=TRUE)

		# Scale to centroid size
		if(!is.null(centroid.size)) xy <- xy * (centroid.size / sqrt(sum(xy^2)))

		# Center (by range) about PC score and save to array
		lm_arr[, , i] <- xy
	}

	# Plot each shape
	for(i in 1:n_shapes){

		# Test aspect ratio with square
		seq_x <- seq(0, 1, length=10)
		square <- rbind(
			cbind(seq_x, rep(1,10), rep(0,10)),
			cbind(rep(1,10), seq_x, rep(0,10)), 
			cbind(seq_x, rep(0,10), rep(0,10)),
			cbind(rep(0,10), seq_x, rep(0,10))
			)
		#do.call(what=fcn, list(scores_bt[i, pcs], square, ...))

		do.call(what=fcn, list(xy=scores_bt[i, pcs], coor=lm_arr[, , i], ...))
		#break
	}
}