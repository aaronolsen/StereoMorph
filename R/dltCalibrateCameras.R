dltCalibrateCameras <- function(coor.2d, nx, grid.size, c.run = FALSE, reduce.grid.dim = 3, fit.min.break = 1, nlm.eval.max = 350, nlm.iter.max = 250, objective.min = 2, print.progress = FALSE){

	# REMOVE ANY SETS WITH NA
	is_na <- apply(is.na(coor.2d), c(3, 4), 'sum')
	is_na_rowsums <- rowSums(is_na)
	coor.2d <- coor.2d[, , is_na_rowsums == 0, ]	

	# FIND SECOND GRID DIMENSION
	ny <- dim(coor.2d)[1]/nx

	# REDUCE GRID DIMENSIONS
	if(reduce.grid.dim){

		# CHECK THAT reduce.grid.dim IS GREATER THAN TWO
		if(reduce.grid.dim < 3) stop(paste0("reduce.grid.dim (", reduce.grid.dim, ") must be greater than 2."))
		
		# SET REDUCED GRID DIMENSIONS
		rx <- reduce.grid.dim
		ry <- reduce.grid.dim
		
		# SET REDUCED GRID SIZES
		sx <- ((nx-1)*grid.size) / (rx-1)
		sy <- ((ny-1)*grid.size) / (ry-1)

		# EMPTY REDUCED GRID DIMENSION ARRAY
		coor_2d_red <- array(NA, dim=c(rx*ry, 2, dim(coor.2d)[3], dim(coor.2d)[4]))

		if(print.progress) cat('\nReduce grid point density (', dim(coor.2d)[3]*dim(coor.2d)[4], ' total)\n', sep='')

		for(i in 1:dim(coor.2d)[3]){
			for(j in 1:dim(coor.2d)[4]){
				if(print.progress) cat('\t', (i-1)*dim(coor.2d)[4] + j, ') Aspect ', i, ', View ', j, '; ', sep='')
				coor_2d_red[, , i, j] <- resampleGridImagePoints(pts=coor.2d[, , i, j], nx=nx, rx=rx, ry=ry, fit.min.break=fit.min.break, print.progress=print.progress)$pts
			}
		}
	}else{
		
		# MAINTAIN FULL GRID DIMENSIONS
		rx <- nx;ry <- ny;sx <- grid.size;sy <- NULL
		
		# COPY POINTS TO REDUCED 2D COOR ARRAY
		coor_2d_red <- coor.2d
	}	

	# SCALE INITIAL TRANSLATE PARAMETER TO REAL-WORLD UNITS (APPROX HALF THE MAX DIMENSION OF GRID)
	t_init <- (max(nx, ny)*grid.size)/3
	
	# SET INITIAL TIME POINT
	ptm <- proc.time()

	# RUN NLM FUNCTION TO FIND TRANSFORMATION PARAMETERS THAT MINIMIZE INTERNAL RMS CALIBRATION ERROR
	if(print.progress) cat('\nFull Transform RMSE Minimization\nNumber of parameters:', (dim(coor.2d)[3]-1)*6, '\nNumber of points:', rx*ry*dim(coor.2d)[3], '\n')

	control <- list(eval.max = nlm.eval.max, iter.max = nlm.iter.max)
	nlm_calls_max <- 12

	# SET STARTING NUMBER OF GRIDS FOR OPTIMIZATION
	grid_incl_min <- if(dim(coor.2d)[3] == 2){2}else{3}
	
	# SET FIXED START PARAMETERS, FIX FIRST GRID AT ORIGIN
	p_fix <- c()

	# SET INITIAL VARIABLE TRANSFORM PARAMETERS
	p_var <- c(matrix(rep(c(0.1, 0.2, 0.3, t_init*1.1, t_init*1.2, t_init*1.3), dim(coor.2d)[3]-1), nrow=6))
	#print(p_var)

	for(num_grid in grid_incl_min:dim(coor.2d)[3]){

		if(print.progress) cat('\nRunning minimization with ', num_grid, ' grids...', sep='')

		# GET 2D COORDINATE SUBSET MATRIX FOR TRANSFORMATION OPTIMIZATION
		coor_2d_t <- apply(coor_2d_red[, , 1:num_grid, ], c(2, 4), matrix, byrow=FALSE)

		lower <- rep(c(rep(-pi, 3), rep(-7*max(nx, ny)*grid.size, 3)), num_grid)

		# SET NLM CALL VARIABLES
		nlm_min <- rep(NA, nlm_calls_max)
		nlm_calls <- list()

		# SAVE PROCESSING TIME
		nlm_start <- proc.time()

		for(nlm_n in 1:nlm_calls_max){

			# VARY STARTING PARAMETERS EACH ITERATION
			if(nlm_n == 1) v <- c(1,1,1, 1,1,1, 1)
			if(nlm_n == 2) v <- -c(1,1,1, 0,1,1, 1)
			if(nlm_n == 3) v <- c(1,1,1, 1,0,1, 1)
			if(nlm_n == 4) v <- -c(1,0,1, 1,0,1, 1)
			if(nlm_n == 5) v <- c(1,1,0, 1,0,0, 1)
			if(nlm_n == 6) v <- -c(0.5,0,3, 0,0,2, 1)
			if(nlm_n == 7) v <- c(2,0,0, 0,3,0, 1)
			if(nlm_n == 8) v <- -c(0.5,1,3, 1,0.5,0, 0)
			if(nlm_n == 9) v <- c(0,0,2, 0,2,0, 0)
			if(nlm_n == 10) v <- -c(2,0,0.5, 1,0.5,0, 1)
			if(nlm_n == 11) v <- c(2,0,4, 2,1,0, 0.5)
			if(nlm_n == 12) v <- c(1,2,0.3, 1,3,2, 0)

			# SET STARTING PARAMETERS
			var_len <- (num_grid-1)*6 - length(p_fix)
			start <- p_var[1:var_len]*rep(v, var_len)[1:var_len]
			if(length(p_fix)) start <- c(p_fix, start)
			#print(matrix(start, nrow=6))

			if(print.progress) cat(' ', nlm_n, sep='')

			nlm_fit <- tryCatch(
				expr={
					nlminb(start=start, objective=dltTransformationParameterRMSError, 
						control=control, lower=lower, upper=-lower, coor.2d=coor_2d_t, nx=rx, ny=ry, 
						sx=sx, sy=sy, p.fixed=rep(0, 6))
				},
				error=function(cond){if(print.progress) cat('F');return(NULL)},
				warning=function(cond) return(NULL)
			)

			# FAILED TO CONVERGE
			if(!is.null(nlm_fit)){

				# GET MINIMUM
				if(print.progress) cat('T')
				nlm_calls[[nlm_n]] <- nlm_fit
				nlm_min[nlm_n] <- nlm_calls[[nlm_n]]$objective

				# IF OPTIMIZATION MINIMUM IS LESS THAN ONE, STOP ITERATING
				if(nlm_calls[[nlm_n]]$convergence == 0 && nlm_calls[[nlm_n]]$objective < objective.min) break
			}
		}

		# SAVE PROCESSING TIME
		nlm_elapsed <- proc.time() - nlm_start

		# SAVE RUN WITH MINIMUM
		nlm_res_t <- nlm_calls[[which.min(nlm_min)]]

		if(print.progress){
			cat('\n\tNumber of nlminb() calls: ', nlm_n, 
				'\n\tTermination message: ', nlm_res_t$message, 
				'\n\tMinimum: ', nlm_res_t$objective, 
				'\n\tIterations: ', nlm_res_t$iterations, ' (', nlm.iter.max, ' max)', 
				'\n\tFunction evaluations: ', nlm_res_t$evaluations['function'], ' (', nlm.eval.max, ' max)', 
				'\n\tRun-time: ', nlm_elapsed[1], ' sec', 
				'\n\tSum of absolute differences between initial and final parameters: ', sum(abs(start - nlm_res_t$par)), 
				'\n\tEstimate: ', sep='');cat(round(nlm_res_t$par, 6), sep=', ')
			cat('\n')
		}

		# SET FIXED PARAMETERS FOR NEXT ITERATION
		p_fix <- nlm_res_t$par
	}

	# SAVE PROCESSING TIME
	run_time <- proc.time() - ptm

	if(print.progress) cat('\nTotal processing time: ', run_time[1], ' sec', sep='')

	# SAVE OPTIMIZED PARAMETERS
	p_init <- matrix(c(rep(0, 6), nlm_res_t$par), nrow=6)

	# GET 3D COORDINATES BASED ON OPTIMIZED PARAMETERS
	coor_3d_coeff <- transformPlanarCalibrationCoordinates(tpar=c(p_init), nx=nx, ny=ny, sx=grid.size)

	# GET 2D INPUT COORDINATES
	coor_2d_coeff <- apply(coor.2d, c(2, 4), matrix, byrow=FALSE)

	# GET DLT CALIBRATION COEFFICIENTS FROM OPTIMIZED 3D COORDINATE SUBSET (ALL GRID POINTS - SO RMSE CAN DIFFER FROM NLM MINIMUM)
	dlt_coefficients_t <- dltCoefficients(coor.3d=coor_3d_coeff, coor.2d=coor_2d_coeff)

	if(c.run){

		# GET 2D COORDINATE SUBSET MATRIX FOR COEFFICIENT OPTIMIZATION
		coor_2d_c <- apply(coor_2d_red, c(2, 4), matrix, byrow=FALSE)

		# SET INITIAL TIME POINT AND NUMBER OF ITERATIONS
		ptm <- proc.time();#c_iter <<- 0

		# RUN NLM FUNCTION TO FIND COEFFICIENTS THAT MINIMIZE INTERNAL RMS CALIBRATION ERROR
		if(print.progress) cat('\nCoefficient RMSE Minimization\nNumber of parameters:', length(c(dlt_coefficients_t$cal.coeff)), '\nNumber of points:', rx*ry*dim(coor.2d)[3], '\nRunning minimization...\n')
		nlm_res_c <- nlm(dltCoefficientRMSError, p=c(dlt_coefficients_t$cal.coeff), coor.2d=coor_2d_c)

		if(print.progress) cat('\nTermination code:', nlm_res_c$code, '\nIterations:', nlm_res_c$iterations, '\n')

		# SAVE PROCESSING TIME
		run_time_c <- proc.time() - ptm

		# USE DLT COEFFICIENTS TO RECONSTRUCT ALL 2D COORDINATES
		dlt_reconstruct <- dltReconstruct(cal.coeff=matrix(nlm_res_c$estimate, nrow=11, ncol=dim(coor.2d)[4]), coor.2d=apply(coor.2d, c(2, 4), matrix, byrow=FALSE))

		# USE DLT COEFFICIENTS TO RECONSTRUCT ALL 2D COORDINATES
		dlt_coefficients_c <- dltCoefficients(coor.3d=dlt_reconstruct$coor.3d, coor.2d=apply(coor.2d, c(2, 4), matrix, byrow=FALSE))

		mean_reconstruct_rmse <- mean(dlt_reconstruct$rmse)
		coor_3d <- dlt_reconstruct$coor.3d
		cal_coeff <- dlt_coefficients_c$cal.coeff
		coefficient_rmse <- dlt_coefficients_c$rmse
	}else{

		# USE DLT COEFFICIENTS TO RECONSTRUCT ALL 2D COORDINATES
		dlt_reconstruct <- dltReconstruct(cal.coeff=dlt_coefficients_t$cal.coeff, coor.2d=apply(coor.2d, c(2, 4), matrix, byrow=FALSE))

		mean_reconstruct_rmse <- mean(dlt_reconstruct$rmse)
		coor_3d <- dlt_reconstruct$coor.3d
		cal_coeff <- dlt_coefficients_t$cal.coeff
		coefficient_rmse <- dlt_coefficients_t$rmse
		
		nlm_res_c <- list('estimate'=NA, 'minimum'=NA)
		run_time_c <- NA
	}

	l <- list(
		cal.coeff=cal_coeff, 
		coor.3d=coor_3d, 
		mean.reconstruct.rmse=mean_reconstruct_rmse, 
		coefficient.rmse=coefficient_rmse,
		t.param.final=p_init,
		t.min=nlm_res_t$objective,
		t.runtime=run_time[1],
		c.param.init=c(dlt_coefficients_t$cal.coeff),
		c.param.final=nlm_res_c$estimate,
		c.min=nlm_res_c$minimum,
		c.iter=nlm_res_c$iterations,
		c.runtime=run_time_c[1]
		)
	class(l) <- 'dltCalibrateCameras'
	l
}

summary.dltCalibrateCameras <- function(object, ...){
	r <- ''

	r <- c(r, '\ndltCalibrateCameras Summary\n')

	r <- c(r, '\tMinimize RMS Error by transformation\n')
	r <- c(r, '\t\tTotal number of parameters estimated: ', length(object$t.param.final)-6, '\n')
	r <- c(r, '\t\tFinal Minimum Mean RMS Error: ', round(object$t.min, 3), '\n')
	r <- c(r, '\t\tTotal Run-time: ', round(object$t.runtime, 2), ' sec\n')

	if(!is.null(object$c.iter)){
		r <- c(r, '\tMinimize RMSE by coefficients\n')
		r <- c(r, '\t\tNumber of parameters: ', length(object$c.param.init), '\n')
		r <- c(r, '\t\tSum of absolute differences between initial and final parameters: ', format(sum(abs(object$c.param.init - object$c.param.final))), '\n')
		r <- c(r, '\t\tTotal function calls: ', object$c.iter, '\n')
		r <- c(r, '\t\tFinal Mean RMS Error: ', format(object$c.min), '\n')
		r <- c(r, '\t\tRun-time: ', round(object$c.runtime, 2), ' sec\n')
	}

	r <- c(r, '\n')

	r <- c(r, '\tMean Reconstruct RMSE: ', round(object$mean.reconstruct.rmse, 3), '\n')

	if(length(object$coefficient.rmse) == 1){
		r <- c(r, '\tMean Calibration Coefficient RMS Error: ', round(object$coefficient.rmse, 3), '\n')
	}else{
		r <- c(r, '\tCalibration Coefficient RMS Error:\n')
		for(i in 1:length(object$coefficient.rmse)){
			r <- c(r, '\t\t', i, ': ', round(object$coefficient.rmse[i], 3), '\n')
		}
	}

	r <- c(r, '\t3D Coordinate Matrix dimensions: ', dim(object$coor.3d)[1], ' x ', dim(object$coor.3d)[2], '\n')

	class(r) <- "summary.dltCalibrateCameras"
	r
}

print.summary.dltCalibrateCameras <- function(x, ...) cat(x, sep='')