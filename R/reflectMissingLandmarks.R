reflectMissingLandmarks <- function(lm.matrix, left = '(_l|_left)([_]?[0-9]*$)', 
	right = '(_r|_right)([_]?[0-9]*$)', left.remove = '\\2', right.remove = '\\2', 
	left.replace = '_R\\2', right.replace = '_L\\2', average = FALSE){
	# Modified from R function OSymm() written by Annat Haber
	# The function uses object symmetry to find the symmetry plane following
	# Option to find average between bilateral landmarks
	# Klingenberg et al. 2002 (Evolution 56:1909-1920)

	# GET ROWNAMES
	rownames_lm_matrix <- rownames(lm.matrix)

	# ID EACH LANDMARK AS LEFT OR RIGHT
	id_side <- rep(NA, length(rownames_lm_matrix))
	id_side[grepl(pattern=left, x=rownames_lm_matrix, ignore.case=TRUE)] <- 'L'
	id_side[grepl(pattern=right, x=rownames_lm_matrix, ignore.case=TRUE)] <- 'R'
	
	# GET LIST OF LANDMARK NAMES WITHOUT SIDES
	landmark_names <- gsub(pattern=left, replacement=left.remove, x=rownames_lm_matrix, ignore.case=TRUE)
	landmark_names <- gsub(pattern=right, replacement=right.remove, x=landmark_names, ignore.case=TRUE)

	# PRESERVE INPUT MATRIX
	lm_matrix <- lm.matrix

	# ADD ROWS FOR BILATERAL LANDMARKS MISSING ON ONE SIDE
	for(landmark_name in landmark_names){
		
		# SKIP ALREADY PAIRED LANDMARKS
		if(sum(landmark_name == landmark_names) > 1) next

		# SKIP MIDLINE LANDMARKS (LANDMARKS WITHOUT A SIDE)
		if(is.na(id_side[landmark_name == landmark_names])) next

		# GET NEW ROWNAME
		if(id_side[landmark_name == landmark_names] == 'R') new_rowname <- gsub(pattern=right, replacement=right.replace, x=rownames(lm_matrix)[landmark_name == landmark_names], ignore.case=TRUE)
		if(id_side[landmark_name == landmark_names] == 'L') new_rowname <- gsub(pattern=left, replacement=left.replace, x=rownames(lm_matrix)[landmark_name == landmark_names], ignore.case=TRUE)

		# ADD ROWS TO LANDMARK MATRIX
		new_rownames <- c(rownames(lm_matrix), new_rowname)
		lm_matrix <- rbind(lm_matrix, rep(NA, length=ncol(lm.matrix)))
		rownames(lm_matrix) <- new_rownames
	}

	# SORT ROWNAMES TO MAKE SURE THAT ROWNAMES CORRESPOND FOR LATER OPERATIONS
	lm_matrix <- lm_matrix[sort(rownames(lm_matrix)), ]

	# MAKE ID VECTOR
	id_side <- rep(NA, length(rownames(lm_matrix)))

	# ID EACH LANDMARK AS LEFT, RIGHT OR MIDLINE
	id_side[grepl(pattern=left, x=rownames(lm_matrix), ignore.case=TRUE)] <- 'L'
	id_side[grepl(pattern=right, x=rownames(lm_matrix), ignore.case=TRUE)] <- 'R'
	id_side[is.na(id_side)] <- 'M'

	# GET LIST OF LANDMARK NAMES WITHOUT SIDES
	landmark_names <- gsub(pattern=left, replacement=left.remove, x=rownames(lm_matrix), ignore.case=TRUE)
	landmark_names <- gsub(pattern=right, replacement=right.remove, x=landmark_names, ignore.case=TRUE)

	#print(cbind(rownames(lm_matrix), id_side, landmark_names))

	all_left <- all_right <- FALSE
	if(sum(is.na(lm_matrix[id_side == 'L', 1])) == sum(id_side == 'L')) all_right <- TRUE
	if(sum(is.na(lm_matrix[id_side == 'R', 1])) == sum(id_side == 'R')) all_left <- TRUE

	# IF LANDMARKS ARE ONLY ON ONE SIDE OR ALONG MIDLINE THEN CHIRALITY GETS FLIPPED
	# PROJECT UNILATERAL LANDMARK FURTHEST FROM THE MIDLINE ACROSS THE MIDLINE TO PREVENT THIS
	if(sum(!is.na(lm_matrix[id_side == 'M', 1])) >= 3 && (all_right || all_left)){

		# Get just midline points and center about centroid
		midline_points <- lm_matrix[id_side == 'M', ]
		midline_points <- midline_points[!is.na(midline_points[,1]), ]
		centroid_align <- midline_points - matrix(colMeans(midline_points, na.rm=TRUE), nrow(midline_points), ncol(midline_points), byrow=TRUE)

		# Check for differences in point position
		if(mean(apply(centroid_align, 2, 'sd', na.rm=TRUE)) > 1e-10){

			# SVD of centroid-aligned points
			plane <- list(N=svd(centroid_align[!is.na(centroid_align[,1]),])$v[, 3], Q=colMeans(midline_points, na.rm=TRUE))

			# Find distance of points from plane
			dptp <- abs(distancePointToPlane(lm_matrix, plane$N, plane$Q))
			
			# Make midline points NA just in case a midline point is furthest from the midline
			dptp[id_side == 'M'] <- NA
		
			# Find furthest point from plane
			which_max_dptp <- which.max(dptp)
		
			# Find indices for landmark on L and R
			which_sides <- which(landmark_names == landmark_names[which_max_dptp])
		
			# Project point to other side by multiplying distance from midline plane by 2 and reflecting across
			if(length(which_sides) == 2){
				other_side_idx <- which_sides[which_sides != which_max_dptp]
				lm_matrix[other_side_idx,] <- lm_matrix[which_max_dptp,] + 2*dptp[which_max_dptp]*plane$N
				#print(lm_matrix[c(other_side_idx, which_max_dptp), ])
			}
		}
	}

	# GET UNIQUE LIST OF LANDMARK NAMES WITHOUT SIDES
	unique_landmark_names <- unique(landmark_names)

	# CREATE OPPOSING LANDMARK MATRIX
	lm_matrix_r <- lm_matrix

	# REVERSE SIGN OF LANDMARKS ACROSS AN AXIS
	lm_matrix_r[, ncol(lm_matrix_r)] <- -lm_matrix_r[, ncol(lm_matrix_r)]

	# MAKE ROWNAMES FOR REVERSE MATRIX
	rownames_r <- rownames(lm_matrix)

	# SWITCH ALL LEFT LANDMARK NAMES TO RIGHT AND VICE VERSA
	rownames_r[id_side == 'R'] <- gsub(pattern=right, replacement=right.replace, x=rownames(lm_matrix)[id_side == 'R'], ignore.case=TRUE)
	rownames_r[id_side == 'L'] <- gsub(pattern=left, replacement=left.replace, x=rownames(lm_matrix)[id_side == 'L'], ignore.case=TRUE)

	# REPLACE OLD ROWNAMES WITH SWAPPED ROWNAMES
	rownames(lm_matrix_r) <- rownames_r
	
	# SORT ROWNAMES TO MAKE SURE THAT ROWNAMES CORRESPOND
	lm_matrix_r <- lm_matrix_r[sort(rownames(lm_matrix_r)), ]

	# USE LEAST SQUARES TO ALIGN THE TWO MATRICES
	lm_matrix_a <- findOptimalPointAlignment(lm_matrix_r, lm_matrix)

	# FILL NA VALUES FROM OTHER MATRIX
	lm_matrix_fill <- lm_matrix_r
	lm_matrix_fill[which(is.na(lm_matrix_r))] <- lm_matrix_a[which(is.na(lm_matrix_r))]
	lm_matrix_a[which(is.na(lm_matrix_a))] <- lm_matrix_r[which(is.na(lm_matrix_a))]

	#print(cbind(rownames(lm_matrix), rownames(lm_matrix_r)))

	# AVERAGE THE TWO MATRICES
	# lm_matrix_a CORRESPONDS TO rec.ref IN OSYMM()
	# lm_matrix_fill CORRESPONDS TO rec.orig IN OSYMM()
	if(average){
		lm_matrix_symm <- (lm_matrix_fill + lm_matrix_a)/2
	}else{
		lm_matrix_symm <- lm_matrix_fill
	}
	
	# FIND ALIGNMENT ERRORS
	align_error <- rep(NA, 0)
	align_names <- rep(NA, 0)

	# GET THE BILATERAL ALIGNMENT ERROR
	for(landmark_name in unique_landmark_names){

		# SKIP IF ONLY ONE SIDE WAS ORIGINALLY INPUT
		if(sum(is.na(lm_matrix[landmark_name == landmark_names, ])) > 0) next

		# SKIP UNPAIRED LANDMARKS
		if(sum(landmark_name == landmark_names) == 1) next
		
		# GET BILATERAL LANDMARK MATRIX
		bilateral_landmarks <- rbind(lm_matrix_fill[landmark_name == landmark_names, ], lm_matrix_a[landmark_name == landmark_names, ])

		# GET BILATERAL LANDMARK MATRIX
		align_error <- c(align_error, sqrt(sum((bilateral_landmarks[1, ] - bilateral_landmarks[3, ])^2)))
		align_names <- c(align_names, landmark_name)
	}

	# ADD NAMES TO ERROR VECTOR
	names(align_error) <- align_names

	# SWITCH ALL LEFT LANDMARK NAMES BACK TO RIGHT AND VICE VERSA - TO MATCH INPUT
	# MAKE ROWNAMES FOR REVERSE MATRIX
	rownames_r <- rownames(lm_matrix_symm)

	# SWITCH ALL LEFT LANDMARK NAMES TO RIGHT AND VICE VERSA
	rownames_r[id_side == 'R'] <- gsub(pattern=right, replacement=right.replace, x=rownames(lm_matrix)[id_side == 'R'], ignore.case=TRUE)
	rownames_r[id_side == 'L'] <- gsub(pattern=left, replacement=left.replace, x=rownames(lm_matrix)[id_side == 'L'], ignore.case=TRUE)

	# REPLACE OLD ROWNAMES WITH SWAPPED ROWNAMES
	rownames(lm_matrix_symm) <- rownames_r
	
	# UNDO REVERSE SIGN OF LANDMARKS ACROSS AN AXIS
	lm_matrix_symm[, ncol(lm_matrix_symm)] <- -lm_matrix_symm[, ncol(lm_matrix_symm)]

	l <- list(lm.matrix=lm_matrix_symm, align.error=align_error)
	class(l) <- 'reflectMissingLandmarks'
	return(l)
}

summary.reflectMissingLandmarks <- function(object, ...){
	r <- ''
	r <- c(r, '\nreflectMissingLandmarks Summary\n')
	
	r <- c(r, '\tAlignment Bilateral RMSE: ', format(sqrt(sum(object$align.error)/length(object$align.error))), '\n')
	for(i in 1:length(object$align.error)) r <- c(r, '\t\t', names(object$align.error)[i], ": ", format(object$align.error[i]), '\n')

	class(r) <- "summary.reflectMissingLandmarks"
	r
}

print.summary.reflectMissingLandmarks <- function(x, ...) cat(x, sep='')