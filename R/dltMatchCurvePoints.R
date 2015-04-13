dltMatchCurvePoints <- function(lm.list, cal.coeff, ref.view='max', window.size=30, min.tangency.angle = 0.1, min.dist.adj.slope = 0.1, plot.match = FALSE, fill.missing = TRUE){

	min_candidate_num <- 5
	match_slope_avgct <- 4
	min_dist_slope_ct <- 10
	tangency_fill_window <- 1

	# SET WINDOW FOR DETERMINING EPIPOLAR TANGENCY ANGLE
	window.tangency <- 5

	# IF EVEN, INCREASE BY ONE
	if(tangency_fill_window %% 2){tangency_fill_halfwin <- (tangency_fill_window+1)/2}else{tangency_fill_halfwin <- tangency_fill_window/2}

	# CHECK IF LIST INPUT IS MORE THAN ONE LANDMARK/CURVE
	if(!is.null(names(lm.list))){

		# EMPTY LISTS FOR STORING OUTPUT VALUES
		epipolar_dist <- list()
		curve_pt_dist <- list()
		portion_matched_nonref <- list()

		for(landmark_name in names(lm.list)){

			# CALL MATCH CURVE POINTS FOR EACH LANDMARK/CURVE SET
			dlt_match_curve_points <- dltMatchCurvePoints(lm.list = lm.list[[landmark_name]], cal.coeff = cal.coeff, ref.view=ref.view, window.size=window.size, min.tangency.angle=min.tangency.angle, plot.match=plot.match)

			# SAVE OUTPUT VALUES TO LISTS
			lm.list[[landmark_name]] <- dlt_match_curve_points$match.lm.list
			epipolar_dist[[landmark_name]] <- dlt_match_curve_points$epipolar.dist
			curve_pt_dist[[landmark_name]] <- dlt_match_curve_points$curve.pt.dist
			portion_matched_nonref[[landmark_name]] <- dlt_match_curve_points$portion.matched.nonref
		}
		
		r <- list(match.lm.list=lm.list, epipolar.dist=epipolar_dist, curve.pt.dist=curve_pt_dist, portion.matched.nonref=portion_matched_nonref)
		class(r) <- 'dltMatchCurvePoints'
		return(r)
	}

	# CHECK THAT THERE ARE AT LEAST TWO CAMERA VIEWS
	if(length(lm.list) < 2){r <- list(match.lm.list=lm.list);class(r) <- 'dltMatchCurvePoints';return(r)}		

	# CHECK THAT THERE ARE MORE THAN TWO POINTS IN EVERY VIEW
	pt_count <- rep(NA, length(lm.list))
	for(i in 1:length(lm.list)){
		if(is.matrix(lm.list[[i]])){
			pt_count[i] <- nrow(lm.list[[i]])
			if(nrow(lm.list[[i]]) < 3){r <- list(match.lm.list=lm.list);class(r) <- 'dltMatchCurvePoints';return(r)}
		}else{
			r <- list(match.lm.list=lm.list);class(r) <- 'dltMatchCurvePoints';return(r)
		}
	}

	# FIND MAX OR MIN FOR REFERENCE VIEW IF SPECIFIED
	if(ref.view == 'max') ref.view <- which.max(pt_count)
	if(ref.view == 'min') ref.view <- which.min(pt_count)
	
	# SWITCH COLUMNS IF FIRST VIEW IS NOT REFERENCE VIEW
	if(ref.view == 2){
		lm.list <- list(lm.list[[2]], lm.list[[1]])
		cal.coeff <- cal.coeff[, ncol(cal.coeff):1]
	}

	# HOMLOGOUS CURVE POINT LISTS
	homol_curve_pts_list <- list()
	homol_curve_pts_list[[1]] <- matrix(lm.list[[1]], nrow=nrow(lm.list[[1]]), ncol=ncol(lm.list[[1]]), dimnames=list(rownames(lm.list[[1]], colnames(lm.list[[1]]))))
	homol_curve_pts_list[[2]] <- matrix(NA, nrow=nrow(lm.list[[1]]), ncol=ncol(lm.list[[1]]), dimnames=list(rownames(lm.list[[1]], colnames(lm.list[[1]]))))

	# SAVE MATCHED INDICES
	min_idx <- rep(NA, nrow(lm.list[[1]]))

	# ASSUME START AND END POINTS ARE HOMOLOGOUS
	min_idx[c(1, nrow(lm.list[[1]]))] <- c(1, nrow(lm.list[[2]]))
	homol_curve_pts_list[[2]][1, ] <- lm.list[[2]][1, ]
	homol_curve_pts_list[[2]][nrow(lm.list[[1]]), ] <- lm.list[[2]][nrow(lm.list[[2]]), ]

	# SET WINDOW PARAMETERS
	w_min_dec <- 2
	w_max_dec <- window.size
	
	# GET NULL MATCH SLOPE
	match_slope_null <- nrow(lm.list[[2]])/nrow(lm.list[[1]])

	# EMPTY VECTOR FOR DISTANCE FROM EPIPOLAR POINT TO NEAREST POINT ON CURVE
	curve_pt_dist <- c(0, rep(NA, length=nrow(lm.list[[1]])-2), 0)

	w_min_save <- rep(NA, nrow(lm.list[[1]]))
	w_max_save <- rep(NA, nrow(lm.list[[1]]))
	p2_dist <- rep(NA, nrow(lm.list[[1]]))
	curve_slope_save <- rep(NA, nrow(lm.list[[1]]))

	if(!is.null(min.tangency.angle)){
		
		# FIND EPIPOLAR TANGENCY ANGLES FOR CURVES IN BOTH VIEWS
		tan_angles_1 <- findEpipolarTangencyAngles(lm.list[[1]], cal.coeff[, 1:2], window.tangency)
		tan_angles_2 <- findEpipolarTangencyAngles(lm.list[[2]], cal.coeff[, 2:1], window.tangency)
	}

	# GET EPIPOLAR LINES
	e_lines <- list()
	for(i in 2:(nrow(lm.list[[1]])-1)){
		e_line <- dltEpipolarLine(p=lm.list[[1]][i, ], cal.coeff1=cal.coeff)
		e_lines[[i]] <- list('l1'=e_line$l1, 'l2'=e_line$l2)
	}	

	mp <- 0
	take <- 0
	i <- 2
	while(i < nrow(lm.list[[1]])){

		skip <- FALSE
		stall <- FALSE
		
		# ROUND WINDOW LIMITS
		w_min <- min(floor(w_min_dec), nrow(lm.list[[2]]) - 2)
		w_max <- min(floor(w_max_dec), nrow(lm.list[[2]]) - 1)
		#cat(i, w_min, w_max, '\n')
		
		# SAVE WINDOW LIMITS
		w_min_save[i] <- w_min
		w_max_save[i] <- w_max

		# FIND DISTANCE OF POINTS WITHIN WINDOW TO EPIPOLAR LINE
		dptl <- distancePointToLine(p=lm.list[[2]][w_min:w_max, ], e_lines[[i]]$l1, e_lines[[i]]$l2)
		
		# FIND INDEX OF FIRST MINIMUM
		min_dist_idx <- which.min(dptl)
		
		# GET SLOPE BETWEEN MATCHED INDICES
		match_slope <- (w_min + min_dist_idx - 1) - w_min

		# IF MATCH SKIPS AHEAD, TREAT AS REGION OF EPIPOLAR TANGENCY
		if(match_slope > 5*match_slope_null) skip <- TRUE

		# IF MATCH SKIPS AHEAD, FIND CLOSEST MINIMUM
		if(skip){
			
			# GET CANDIDATE MINIMA
			cand_min <- which(rank(dptl) <= min_candidate_num)
			
			# PUT LARGE VALUES ON ENDS FOR END MINIMA
			dptle <- c(1000, dptl, 1000)
			
			# FIND WHERE ADJACENT POINTS ARE BOTH GREATER (FINDING LOCAL MINIMA)
			cand_min_lmn <- (dptle[cand_min + 2] - dptle[cand_min+1] > 0) * (dptle[cand_min] - dptle[cand_min+1] > 0)
			min_dist_idx <- min(cand_min[cand_min_lmn == 1])
			skip <- FALSE
			#print('hi')
		}
		
		# GET SLOPE BETWEEN MATCHED INDICES
		match_slope <- (w_min + min_dist_idx - 1) - w_min

		# CHECK FOR SKIP AGAIN, IF STALL IS STILL PRESENT, TREAT AS TANGENCY REGION
		if(match_slope > 5*match_slope_null) skip <- TRUE

		# FIND SLOPE AWAY FROM MINIMUM
		min_dist_adj_slope <- 0
		if(min_dist_idx < length(dptl) - min_dist_slope_ct){
			min_dist_adj_slope <- mean(dptl[(min_dist_idx+1):(min_dist_idx+min_dist_slope_ct)] - 
				dptl[min_dist_idx:(min_dist_idx+min_dist_slope_ct-1)])
		}

		if(skip == TRUE || min_dist_adj_slope < min.dist.adj.slope || tan_angles_1[i] < min.tangency.angle){
			w_min_dec <- min(w_min_dec + match_slope_null, nrow(lm.list[[2]]) - 2)
			w_max_dec <- min(w_min_dec + window.size, nrow(lm.list[[2]]) - 1)
		
			i <- i + 1
			next
		}

		# SAVE MINIMUM INDEX
		min_idx[i] <- w_min + min_dist_idx - 1

		# CHECK IF MATCH HAS STALLED ON THE SAME CURVE 2 POINT
		if(i > match_slope_avgct){
			mean_min_idx <- mean(min_idx[i:(i-match_slope_avgct+1)] - min_idx[(i-1):(i-match_slope_avgct)])
			if(!is.na(mean_min_idx) && mean_min_idx == 0) stall <- TRUE
		}

		# SAVE DISTANCE FROM POINT ON EPIPOLAR LINE TO NEAREST CURVE POINT
		curve_pt_dist[i] <- dptl[min_dist_idx]

		# GET CLOSEST POINT ON EPIPOLAR LINE
		matching_pt <- orthogonalProjectionToLine(lm.list[[2]][w_min + min_dist_idx - 1, ], e_lines[[i]]$l1, e_lines[[i]]$l2)

		# SAVE HOMOLOGOUS POINT ON EPIPOLAR LINE
		homol_curve_pts_list[[2]][i, ] <- matching_pt

		# SHIFT WINDOW TO INCLUDE MATCH POINT AND SUBSEQUENT INTERVAL
		w_min_dec <- min(w_min_dec + min_dist_idx - 1, nrow(lm.list[[2]]) - 2)
		w_max_dec <- min(w_min_dec + window.size, nrow(lm.list[[2]]) - 1)
		
		i <- i + 1
	}

	# PORTION OF SECOND CURVE REPRESENTED IN MATCHED POINTS (WITHOUT TANGENCY FILL)
	portion_matched_nonref <- length(na.omit(unique(min_idx))) / nrow(lm.list[[2]])

	if(fill.missing){

		# FIND BORDERS OF MISSING SEGMENTS
		missing_start <- rep(NA, 0)
		missing_end <- rep(NA, 0)
		for(i in 1:(nrow(homol_curve_pts_list[[2]])-1)){
			if(!is.na(homol_curve_pts_list[[2]][i, 1]) && is.na(homol_curve_pts_list[[2]][i+1, 1])) missing_start <- c(missing_start, i+1)
			if(is.na(homol_curve_pts_list[[2]][i, 1]) && !is.na(homol_curve_pts_list[[2]][i+1, 1])) missing_end <- c(missing_end, i)
		}

		# FILL IN MISSING PORTIONS
		for(i in 1:length(missing_start)){
	
			fill_idx <- (missing_start[i]):(missing_end[i])
		
			min_idx_start <- min_idx[missing_start[i]-1]
			min_idx_end <- min_idx[missing_end[i]+1]

			min_idx_fill <- round(seq(from=min_idx_start, to=min_idx_end, length=length(fill_idx)))
		
			for(j in 1:length(min_idx_fill)){
		
				# INDEX IN REFERENCE CURVE
				idx_ref <- missing_start[i]+j-1
			
				if(tangency_fill_halfwin > 1){
					# SET WINDOWS FOR MIN SEARCH
					w_min <- min(min_idx_fill[j]-tangency_fill_halfwin, nrow(lm.list[[2]])-2)
					w_max <- min(min_idx_fill[j]+tangency_fill_halfwin, nrow(lm.list[[2]])-1)

					# FIND DISTANCE OF POINTS WITHIN WINDOW TO EPIPOLAR LINE
					dptl <- distancePointToLine(p=lm.list[[2]][w_min:w_max, ], e_lines[[idx_ref]]$l1, e_lines[[idx_ref]]$l2)

					# FIND INDEX OF FIRST MINIMUM
					min_dist_idx <- which.min(dptl)

					# SAVE DISTANCE FROM POINT ON EPIPOLAR LINE TO NEAREST CURVE POINT
					curve_pt_dist[idx_ref] <- dptl[min_dist_idx]

					# GET CLOSEST POINT ON EPIPOLAR LINE
					matching_pt <- orthogonalProjectionToLine(lm.list[[2]][w_min + min_dist_idx - 1, ], e_lines[[idx_ref]]$l1, e_lines[[idx_ref]]$l2)

					# POINT ON CURVE
					#homol_curve_pts_list[[2]][idx_ref, ] <- lm.list[[2]][w_min + min_dist_idx - 1, ]
				}else{
					# FIND DISTANCE OF POINTS WITHIN WINDOW TO EPIPOLAR LINE
					dptl <- distancePointToLine(p=lm.list[[2]][min_idx_fill[j], ], e_lines[[idx_ref]]$l1, e_lines[[idx_ref]]$l2)

					# SAVE DISTANCE FROM POINT ON EPIPOLAR LINE TO NEAREST CURVE POINT
					curve_pt_dist[idx_ref] <- dptl

					# GET CLOSEST POINT ON EPIPOLAR LINE
					matching_pt <- orthogonalProjectionToLine(lm.list[[2]][min_idx_fill[j], ], e_lines[[idx_ref]]$l1, e_lines[[idx_ref]]$l2)
				}

				# POINT ON EPIPOLAR
				homol_curve_pts_list[[2]][idx_ref, ] <- matching_pt
			}
		}
	}

	# REMOVE MATCHES ABOVE CURVE POINT DISTANCE THRESHOLD
	#homol_curve_pts_list[[2]][curve_pt_dist > 2, ] <- NA
#	hist(curve_pt_dist)
#	return(1)

	# PLOT WINDOW LIMITS
	#plot(rbind(cbind(1:length(w_min_save), w_min_save), cbind(1:length(w_max_save), w_max_save)), type='n')
	#points(w_min_save, type='l', col='blue')
	#points(w_max_save, type='l', col='green')

	# PLOT INDEX ON SECOND CURVE WITH NEUTRAL SLOPE
	#plot(min_idx, cex=0.5)	
	#points(cbind(c(1:length(min_idx))[tan_angles_1_less], min_idx[tan_angles_1_less]), type='l', lwd=2, col='blue')
	#abline(h=591)
	#abline(a=0, b=nrow(lm.list[[2]])/nrow(lm.list[[1]]))

	#dev.new()
	match_slope <- min_idx[2:length(min_idx)] - min_idx[1:(length(min_idx)-1)]
	#plot(match_slope, type='l')
	#abline(h=nrow(lm.list[[2]])/nrow(lm.list[[1]]), col='green')
	#print(nrow(lm.list[[2]])/nrow(lm.list[[1]]))

	if(plot.match){
		dev.new()
		non_ref_plot_shift <- c(0, -1.5*(max(lm.list[[2]][, 2])-min(lm.list[[2]][, 2])))
	
		curve_points_plot2 <- lm.list[[2]] - matrix(colMeans(lm.list[[2]]), nrow=nrow(lm.list[[2]]), ncol=2, byrow=TRUE)
		curve_points_plot2 <- curve_points_plot2 + matrix(colMeans(lm.list[[1]]), nrow=nrow(curve_points_plot2), ncol=2, byrow=TRUE)
		curve_points_plot2 <- curve_points_plot2 + matrix(non_ref_plot_shift, nrow=nrow(curve_points_plot2), ncol=2, byrow=TRUE)

		homol_curve_pts2 <- homol_curve_pts_list[[2]] - matrix(colMeans(lm.list[[2]]), nrow=nrow(homol_curve_pts_list[[2]]), ncol=2, byrow=TRUE)
		homol_curve_pts2 <- homol_curve_pts2 + matrix(colMeans(lm.list[[1]]), nrow=nrow(homol_curve_pts2), ncol=2, byrow=TRUE)
		homol_curve_pts2 <- homol_curve_pts2 + matrix(non_ref_plot_shift, nrow=nrow(homol_curve_pts2), ncol=2, byrow=TRUE)

		tan_angle_mat_1 <- cbind(1:length(tan_angles_1), tan_angles_1)
		tan_angle_mat_2 <- cbind(1:length(tan_angles_2), tan_angles_2)

		plot(rbind(lm.list[[1]], curve_points_plot2), asp=1, type='n')
		points(lm.list[[1]], type='l', col='blue')
		points(lm.list[[1]][tan_angle_mat_1[, 2] < min.tangency.angle, ], col='blue', cex=0.2)
		points(curve_points_plot2, type='l', col='green')
		points(curve_points_plot2[tan_angle_mat_2[, 2] < min.tangency.angle, ], col='green', cex=0.2)
		segments(lm.list[[1]][, 1], lm.list[[1]][, 2], homol_curve_pts2[, 1], homol_curve_pts2[, 2], col=rgb(0, 0, 0, 0.1))
	}

	# GET EPIPOLAR DISTANCES BETWEEN REFERENCE AND TEST POINTS
	epipolar_dist <- dltEpipolarDistance(p1=homol_curve_pts_list[[1]], p2=homol_curve_pts_list[[2]], cal.coeff, reciprocal=TRUE)

	# SWITCH BACK POINT LISTS IF FIRST VIEW IS NOT REFERENCE VIEW
	if(ref.view == 2) homol_curve_pts_list <- list(homol_curve_pts_list[[2]], homol_curve_pts_list[[1]])

	r <- list(match.lm.list=homol_curve_pts_list, epipolar.dist=epipolar_dist, curve.pt.dist=curve_pt_dist, portion.matched.nonref=portion_matched_nonref)
	class(r) <- 'dltMatchCurvePoints'

	return(r)
}

summary.dltMatchCurvePoints <- function(object, ...){
	r <- ''
	r <- c(r, '\ndltMatchCurvePoints Summary\n')

	if(!is.null(names(object$match.lm.list))){
		curve_found <- F
		
		for(curve_name in names(object$match.lm.list)){
			if(is.null(object$epipolar.dist[[curve_name]])) next
			r <- c(r, '\tCurve name: ', curve_name, '\n')
			r <- c(r, '\t\tReference point count: ', length(object$epipolar.dist[[curve_name]]), '\n')
			r <- c(r, '\t\tStart/end epipolar distances: ', round(object$epipolar.dist[[curve_name]][1], 2), ' px, ', round(object$epipolar.dist[[curve_name]][length(object$epipolar.dist[[curve_name]])], 2), ' px\n')
			r <- c(r, '\t\tPortion of non-reference matched: ', round(object$portion.matched.nonref[[curve_name]]*100, 1), ' %\n')
			r <- c(r, '\t\tMean epipolar-curve point distance: ', round(mean(object$curve.pt.dist[[curve_name]], na.rm = TRUE), 2), ' px +/- ', round(sd(object$curve.pt.dist[[curve_name]], na.rm = TRUE), 2), '\n')
			r <- c(r, '\t\tMax epipolar-curve point distance: ', round(max(object$curve.pt.dist[[curve_name]], na.rm = TRUE), 2), ' px\n')
			curve_found <- T
		}
		if(!curve_found) r <- c(r, '\tNo curves found.\n')
	}else{
		if(length(object$epipolar.dist) == 0){
			r <- c(r, '\tNo curves found.\n')
			class(r) <- "summary.dltMatchCurvePoints"
			return(r)
		}
	
		r <- c(r, '\t\tReference point count: ', length(object$epipolar.dist), '\n')
		r <- c(r, '\t\tStart/end epipolar distances: ', round(object$epipolar.dist[1], 2), ' px, ', round(object$epipolar.dist[length(object$epipolar.dist)], 2), ' px\n')
		r <- c(r, '\t\tPortion of non-reference matched: ', round(object$portion.matched.nonref*100, 1), ' %\n')
		r <- c(r, '\t\tMean epipolar-curve point distance: ', round(mean(object$curve.pt.dist, na.rm = TRUE), 2), ' px +/- ', round(sd(object$curve.pt.dist, na.rm = TRUE), 2), '\n')
		r <- c(r, '\t\tMax epipolar-curve point distance: ', round(max(object$curve.pt.dist, na.rm = TRUE), 2), ' px\n')
	}

	class(r) <- "summary.dltMatchCurvePoints"
	r
}

print.summary.dltMatchCurvePoints <- function(x, ...) cat(x, sep='')

findEpipolarTangencyAngles <- function(curve_points, cal.coeff, window.tangency){

	avectors_save <- rep(NA, nrow(curve_points))

	for(i in 2:(nrow(curve_points)-1)){

		# FIND SELF EPIPOLAR LINE
		self_epipolar_slope <- abs(dltEpipolarLine(p=curve_points[i, ], cal.coeff1=cal.coeff, self=TRUE)$m)

		# VERIFY THAT REFERENCE POINT IS ON SELF-EPIPOLAR (ZERO DISTANCE)
		#sel <- dltEpipolarLine(p=curve_points[i, ], cal.coeff1=cal.coeff, self=TRUE)
		#dptl <- distancePointToLine(p=curve_points[i, ], sel$l1, sel$l2)

		# GET INDICES FOR DETERMINING SLOPE
		s_min <- max(1, i - window.tangency)
		s_max <- min(i + window.tangency, nrow(curve_points))

		# FIND SLOPE COMPONENTS
		slope <- rowSums(cbind(curve_points[s_min, ] - curve_points[i, ], curve_points[i, ] - curve_points[s_max, ]))

		# FIND CURVE SLOPE
		if(slope[1] == 0){curve_slope <- 10^4}else{curve_slope <- abs(slope[2]/slope[1])}
		
		# FIND ANGLE BETWEEN SLOPES IN RADIANS
		a_vectors <- avectors(c(1, curve_slope), c(1, self_epipolar_slope))
		avectors_save[i] <- a_vectors
	}

	avectors_save
}
