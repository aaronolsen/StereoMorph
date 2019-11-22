pls_backtransform <- function(PLS, predict = TRUE, phy = NULL, A1.axes = NULL, A2.axes = NULL){

	# If number of columns is not equal between blocks, expand smaller matrix with random values
	#expanded <- FALSE

	# If each matrix has 1 column, use lm
	if(is.null(phy) && dim(PLS$A1)[2] == 1 && dim(PLS$A2)[2] == 1){

		lm_a1_a2 <- lm(PLS$A1 ~ PLS$A2)
		lm_a2_a1 <- lm(PLS$A2 ~ PLS$A1)

		rlist <- list(
			'A1.resid.mean'=mean(PLS$A1) + lm_a1_a2$residuals,
			'A1.resid'=lm_a1_a2$residuals,
			'A1.bt'=NULL,
			'A1.bt.rms'=NULL,
			'A2.resid.mean'=NULL,
			'A2.resid'=NULL,
			#'A2.resid.mean'=mean(PLS$A2) + lm_a2_a1$residuals,
			#'A2.resid'=lm_a2_a1$residuals,
			'A2.bt'=NULL,
			'A2.bt.rms'=NULL
		)
		
		return(rlist)
	}

	if(!is.null(phy) && dim(PLS$A1)[2] == 1 && dim(PLS$A2)[2] == 1){
		stop("No routine for 1 column matrices when phy is non-NULL")
	}

	# Get blocks as matrices
	if(length(dim(PLS$A1)) == 3){

		# Create and fill matrix
		A1_mat <- matrix(NA, nrow=dim(PLS$A1)[3], ncol=dim(PLS$A1)[1]*dim(PLS$A1)[2], dimnames=list(dimnames(PLS$A1)[[3]], NULL))
		for(i in 1:dim(PLS$A1)[3]) A1_mat[i,] <- t(PLS$A1[,,i])
 	}else{
 		PLS$A1 <- as.matrix(PLS$A1) # Sometimes comes in as data.frame
 		A1_mat <- PLS$A1
 	}

	if(length(dim(PLS$A2)) == 3){

		# Create and fill matrix
		A2_mat <- matrix(NA, nrow=dim(PLS$A2)[3], ncol=dim(PLS$A2)[1]*dim(PLS$A2)[2], dimnames=list(dimnames(PLS$A2)[[3]], NULL))
		for(i in 1:dim(PLS$A2)[3]) A2_mat[i,] <- t(PLS$A2[,,i])
 	}else{
 		PLS$A2 <- as.matrix(PLS$A2) # Sometimes comes in as data.frame
 		A2_mat <- PLS$A2
 	}

	#if(ncol(A1_mat) != ncol(A2_mat)){

		# Find max number of columns between two blocks
		#max_col <- max(c(ncol(A1_mat), ncol(A2_mat)))

		# Create expanded matrix, filled with random values
		#A1_mat_exp <- matrix(rnorm(nrow(A1_mat)*max_col), nrow(A1_mat), ncol=max_col, dimnames=list(rownames(A1_mat), NULL))
		#A2_mat_exp <- matrix(rnorm(nrow(A2_mat)*max_col), nrow(A2_mat), ncol=max_col, dimnames=list(rownames(A2_mat), NULL))

		# Create expanded matrix, filled with 0s
		#A1_mat_exp <- matrix(0, nrow(A1_mat), ncol=max_col, dimnames=list(rownames(A1_mat), NULL))
		#A2_mat_exp <- matrix(0, nrow(A2_mat), ncol=max_col, dimnames=list(rownames(A2_mat), NULL))

		# Add real values
		#A1_mat_exp[, 1:ncol(A1_mat)] <- A1_mat
		#A2_mat_exp[, 1:ncol(A2_mat)] <- A2_mat

		# Re-perform PLS
		#PLS_use <- two.b.pls(A1_mat_exp, A2_mat_exp, iter=1, print.progress=FALSE)
		
		# Set as expanded
		#expanded <- TRUE

	#}else{

		#PLS_use <- PLS
	#}
	
	PLS_use <- PLS

	# Set scores to use for BT
	A1_scores <- PLS_use$XScores
	A2_scores <- PLS_use$YScores

	# Get scores predicted from other block
	if(predict){
	
		type <- c('lm', 'near', 'lmodel2')[1]

		for(axis_r in 1:ncol(A1_scores)){

			if(type %in% c('lm', 'near')){

				lm_fit <- lm(PLS_use$XScores[,axis_r]~PLS_use$YScores[,axis_r])

				l1 <- c(1, lm_fit$coefficients[2] + lm_fit$coefficients[1])
				l2 <- c(0, lm_fit$coefficients[1])

			}else{

				lm_fit <- suppressWarnings(lmodel2(PLS_use$XScores[,axis_r]~PLS_use$YScores[,axis_r]))

				l1 <- c(1, lm_fit$regression.results[2,'Slope'] + lm_fit$regression.results[2,'Intercept'])
				l2 <- c(0, lm_fit$regression.results[2,'Intercept'])
			}

			if(type == 'lm'){

				A1_scores[, axis_r] <- predict(lm_fit)

			}else{

				# Find closest points on line
				predicted <- pointNormalOnLine(cbind(PLS_use$YScores[,axis_r], PLS_use$XScores[,axis_r]), l1, l2)

				# Set as score
				A1_scores[, axis_r] <- predicted[, 2]

				#plot(cbind(PLS_use$YScores[,axis_r], PLS_use$XScores[,axis_r]), asp=1)
				#abline(lm_fit)
				#points(cbind(PLS_use$YScores[,axis_r], A1_scores[, axis_r]), col='red')
				#points(predicted, col='blue')
				#Q
			}
		}

		for(axis_r in 1:ncol(A2_scores)){

			if(type %in% c('lm', 'near')){

				lm_fit <- lm(PLS_use$YScores[,axis_r]~PLS_use$XScores[,axis_r])

				l1 <- c(1, lm_fit$coefficients[2] + lm_fit$coefficients[1])
				l2 <- c(0, lm_fit$coefficients[1])

			}else{

				lm_fit <- suppressWarnings(lmodel2(PLS_use$YScores[,axis_r]~PLS_use$XScores[,axis_r]))

				l1 <- c(1, lm_fit$regression.results[2,'Slope'] + lm_fit$regression.results[2,'Intercept'])
				l2 <- c(0, lm_fit$regression.results[2,'Intercept'])
			}

			if(type %in% c('lm', 'near')){

				A2_scores[, axis_r] <- predict(lm_fit)

			}else{

				# Find closest points on line
				predicted <- pointNormalOnLine(cbind(PLS_use$YScores[,axis_r], PLS_use$XScores[,axis_r]), l1, l2)

				# Set as score
				A2_scores[, axis_r] <- predicted[, 2]
			}
		}

	}else{

		# Get mean scores
		A1_scores_mean <- matrix(colMeans(A1_scores), nrow=nrow(A1_scores), ncol=ncol(A1_scores), byrow=TRUE)
		A2_scores_mean <- matrix(colMeans(A2_scores), nrow=nrow(A2_scores), ncol=ncol(A2_scores), byrow=TRUE)

		# Set number of axes to use
		if(!is.null(A1.axes)){
			A1_axes_replace <- !(1:ncol(A1_scores)) %in% A1.axes
			A1_scores[, A1_axes_replace] <- A1_scores_mean[, A1_axes_replace]
		}
		if(!is.null(A2.axes)){
			A2_axes_replace <- !(1:ncol(A2_scores)) %in% A2.axes
			A2_scores[, A2_axes_replace] <- A2_scores_mean[, A2_axes_replace]
		}
	}

	# Set number of columns for BT
	A1_bt_ncol <- ncol(A1_scores)
	A2_bt_ncol <- ncol(A2_scores)

	# Backtransform PLS from scores
	#pls_A1_bt_mat <- A1_scores %*% solve(PLS_use$left.pls.vectors[1:A1_bt_ncol, ])
	#pls_A2_bt_mat <- A2_scores %*% solve(PLS_use$right.pls.vectors[1:A2_bt_ncol, ])

	if(is.null(phy)){

		# Backtransform
		# 	Use ginv() (from MASS) will work on non-square matrices
		pls_A1_bt_mat <- A1_scores %*% ginv(PLS_use$left.pls.vectors)
		pls_A2_bt_mat <- A2_scores %*% ginv(PLS_use$right.pls.vectors)

	}else{

		# Eigen-decomposition of the phylogenetic covariance matrix
		# See Adams & Felice 2014
		# Doesn't reproduce original values perfectly but pretty close
		phy_parts1 <- phylo_mat_gm(A1_mat, phy)
		one1 <- matrix(1,nrow(A1_mat))
		Ptrans1 <- phy_parts1$D.mat %*% (diag(1,nrow(A1_mat),) - one1 %*% crossprod(one1, phy_parts1$invC) / sum(phy_parts1$invC))
		A1_means <- phyl.vcv(A1_mat, phy_parts1$invC, 1)$alpha

		phy_parts2 <- phylo_mat_gm(A2_mat, phy)
		one2 <- matrix(1,nrow(A2_mat))
		Ptrans2 <- phy_parts2$D.mat %*% (diag(1,nrow(A2_mat),)  - one2 %*% crossprod(one2,phy_parts2$invC)/sum(phy_parts2$invC))
		A2_means <- phyl.vcv(A2_mat, phy_parts2$invC, 1)$alpha
		
		# Backtransform
		pls_A1_bt_mat <- ginv(Ptrans1) %*% A1_scores %*% ginv(PLS_use$left.pls.vectors) + one1 %*% t(A1_means)
		pls_A2_bt_mat <- ginv(Ptrans2) %*% A2_scores %*% ginv(PLS_use$right.pls.vectors) + one2 %*% t(A2_means)

		# Create rows of 1s
		#one1 <- matrix(1, nrow(A1_scores), 1)
		#one2 <- matrix(1, nrow(A2_scores), 1)

		#pls_A1_bt_mat <- (A1_scores %*% ginv(PLS_use$left.pls.vectors)) + one1 %*% t(A1_means)
		#pls_A2_bt_mat <- (A2_scores %*% ginv(PLS_use$right.pls.vectors)) + one2 %*% t(A2_means)
	}

	# Remove any expanded columns
	#if(expanded){
	#	pls_A1_bt_mat <- pls_A1_bt_mat[, 1:ncol(A1_mat)]
	#	pls_A2_bt_mat <- pls_A2_bt_mat[, 1:ncol(A2_mat)]
	#}

	# Get dims
	A1_dim <- dim(PLS$A1)
	A2_dim <- dim(PLS$A2)

	# Save BT as array or matrix depending on original format
	if(length(A1_dim) == 3){

		# Create arrays of original dimensions
		pls_A1_bt <- array(NA, dim=A1_dim, dimnames=dimnames(PLS$A1))

		# Fill array of original dimensions with backtransformed values
		for(i in 1:A1_dim[3]) pls_A1_bt[,,i] <- matrix(pls_A1_bt_mat[i,], nrow=A1_dim[1], ncol=A1_dim[2], byrow=TRUE)

		# Get mean shape
		A1_mean <- array(mshape(PLS$A1), dim=A1_dim)

	}else{
		
		# Save matrix as BT
		pls_A1_bt <- pls_A1_bt_mat

		# Get mean shape
		A1_mean <- matrix(colMeans(PLS$A1), A1_dim[1], A1_dim[2], byrow=TRUE)
	}

	# Save BT as array or matrix depending on original format
	if(length(A2_dim) == 3){

		# Create arrays of original dimensions
		pls_A2_bt <- array(NA, dim=A2_dim, dimnames=dimnames(PLS$A2))

		# Fill array of original dimensions with backtransformed values
		for(i in 1:A2_dim[3]) pls_A2_bt[,,i] <- matrix(pls_A2_bt_mat[i,], nrow=A2_dim[1], ncol=A2_dim[2], byrow=TRUE)

		# Get mean shape
		A2_mean <- array(mshape(PLS$A2), dim=A2_dim)

	}else{
		
		# Save matrix as BT
		pls_A2_bt <- pls_A2_bt_mat

		# Get mean shape
		A2_mean <- matrix(colMeans(PLS$A2), A2_dim[1], A2_dim[2], byrow=TRUE)
	}

	# Find difference between actual and BT shapes
	A1_resid <- PLS$A1 - pls_A1_bt
	A2_resid <- PLS$A2 - pls_A2_bt

	# Measure BT error
	A1_bt_rms <- sqrt(mean((A1_resid)^2))
	A2_bt_rms <- sqrt(mean((A2_resid)^2))
	
	# Get residual plus mean
	# Doesn't affect any downstream analyses using the residuals (because just adding a constant)
	# But the resulting values look more like the original shape/structure
	A1_resid_mean <- A1_mean + A1_resid
	A2_resid_mean <- A2_mean + A2_resid

	list(
		'A1.resid.mean'=A1_resid_mean,
		'A1.resid'=A1_resid,
		'A1.bt'=pls_A1_bt,
		'A1.bt.rms'=A1_bt_rms,
		'A2.resid.mean'=A2_resid_mean,
		'A2.resid'=A2_resid,
		'A2.bt'=pls_A2_bt,
		'A2.bt.rms'=A2_bt_rms
	)
}