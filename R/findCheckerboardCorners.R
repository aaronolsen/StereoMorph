findCheckerboardCorners <- function(image.file, nx, ny, corner.file=NULL, verify.file=NULL, 
	perim.min = 140, perim.max = NULL, dilations.min = 0, dilations.max = 7, quad.approx.thresh = NULL,
	print.progress=TRUE, verbose=FALSE, debug = FALSE) {

	normalize_image <- FALSE
	prev_sqr_size <- 0
	dilations <- 3
	poly_cont_min <- -0.1
	poly_cont_max <- 0.2
	poly_asp_min <- 0.05
	sub.pix.win <- 23
	sub.pix.max.iter <- 20
	max.dist.int.corners <- 15
	max.int.corner.dev <- 3
	criteria <- 0.01

	# MAKE SURE INPUT TYPES AND DIMENSIONS MATCH
	if(!is.null(corner.file)){
		if(class(image.file) != class(corner.file)) stop(paste0("'image.file' (", class(image.file), ") and 'corner.file' (", class(corner.file), ") must be of the same class."))
		if(class(image.file) == 'character'){
			if(length(image.file) != length(corner.file)) stop(paste0("'image.file' (", length(image.file), ") and 'corner.file' (", length(corner.file), ") must be of the same length."))
		}
		if(class(image.file) == 'matrix'){
			if(nrow(image.file) != nrow(corner.file)) stop(paste0("'image.file' (", nrow(image.file), " rows) and 'corner.file' (", nrow(corner.file), " rows) must be of the same dimensions."))
			if(ncol(image.file) != ncol(corner.file)) stop(paste0("'image.file' (", nrow(image.file), " columns) and 'corner.file' (", nrow(corner.file), " columns) must be of the same dimensions."))
		}
	}

	# CREATE VECTOR OR MATRIX OF INPUT IMAGE FILES
	image_input <- image.file
	#is_image <- grepl(pattern='.jpg$|.jpeg$|.tiff$|.png$|.raw$|.gif$|.bmp$', x=c(image.file), ignore.case=TRUE)
	is_image <- grepl(pattern='.jpg$|.jpeg$', x=c(image.file), ignore.case=TRUE)
	if(sum(is_image) == 0){

		image_file <- ''
		for(i in 1:length(image_input)){
			image_file <- c(image_file, paste0(image_input[i], "/", list.files(image_input[i])))
		}
		image.file <- image_file[2:length(image_file)]

		if(is.matrix(image_input)){
			if(nrow(image_input) == 1 && ncol(image_input) > 1){
				image.file <- matrix(image.file, ncol=ncol(image_input), byrow=FALSE)
			}else if(nrow(image_input) > 1 && ncol(image_input) == 1){
				image.file <- matrix(image.file, nrow=nrow(image_input), byrow=TRUE)
			}else{
				stop(paste0("'image.file' input is a matrix of dimensions ", nrow(image_input), " x ", ncol(image_input), 
					". One dimension must be one."))
			}
		}
	}else if(sum(is_image) == length(image_input)){
	}else{stop("'image.file' input is a mix of folders and image files. Only input of one type allowed.")}

	# IF ANY OF THE OUTPUT PATHS ARE DIRECTORIES, AUTOMATICALLY ASSIGN NAMES
	if(!is.null(corner.file)){
		is_text <- grepl('.txt$', corner.file)
		if(sum(is_text) == 0){
			if(is.vector(image_input)){
				corner_file <- ''
				for(i in 1:length(image_input)) corner_file <- c(corner_file, paste0(corner.file[i], "/", gsub('.[a-zA-Z]+$', '.txt', list.files(image_input[i]))))
				corner.file <- corner_file[2:length(corner_file)]
			}else{
				if(nrow(image_input) == 1 && ncol(image_input) > 1){
					corner_file <- matrix(NA, nrow=nrow(image.file), ncol=ncol(image.file))
					for(i in 1:ncol(image_input)) corner_file[, i] <- paste0(corner.file[i], "/", gsub('.[a-zA-Z]+$', '.txt', list.files(image_input[, i])))
					corner.file <- corner_file
				}else{
					corner_file <- matrix(NA, nrow=nrow(image.file), ncol=ncol(image.file))
					for(i in 1:nrow(image_input)) corner_file[i, ] <- paste0(corner.file[i], "/", gsub('.[a-zA-Z]+$', '.txt', list.files(image_input[i, ])))
					corner.file <- corner_file
				}
			}
		}else if(sum(is_text) == length(corner.file)){
		}else{stop("'corner.file' input is a mix of folders and text files. Only input of one type allowed.")}		
	}

	if(!is.null(verify.file)){
		is_image <- grepl(pattern='.jpg$|.jpeg$|.tiff$|.png$|.raw$|.gif$|.bmp$', x=c(verify.file), ignore.case=TRUE)
		if(sum(is_image) == 0){
			if(is.vector(image_input)){
				verify_file <- ''
				for(i in 1:length(image_input)) verify_file <- c(verify_file, paste0(verify.file[i], "/", list.files(image_input[i])))
				verify.file <- verify_file[2:length(verify_file)]
			}else{
				if(nrow(image_input) == 1 && ncol(image_input) > 1){
					verify_file <- matrix(NA, nrow=nrow(image.file), ncol=ncol(image.file))
					for(i in 1:ncol(image_input)) verify_file[, i] <- paste0(verify.file[i], "/", list.files(image_input[, i]))
					verify.file <- verify_file
				}else{
					verify_file <- matrix(NA, nrow=nrow(image.file), ncol=ncol(image.file))
					for(i in 1:nrow(image_input)) verify_file[i, ] <- paste0(verify.file[i], "/", list.files(image_input[i, ]))
					verify.file <- verify_file
				}
			}
		}else if(sum(is_image) == length(verify.file)){
		}else{stop("'verify.file' input is a mix of folders and image files. Only input of one type allowed.")}		
	}

	if(is.vector(image.file)){
		image.file <- matrix(image.file, ncol=1)
		if(!is.null(corner.file)) corner.file <- matrix(corner.file, ncol=1)
		if(!is.null(verify.file)) verify.file <- matrix(verify.file, ncol=1)
	}

	# MAKE CORNERS MATRIX/ARRAY
	corner_array <- array(NA, dim=c(nx*ny, 2, dim(image.file)))

	image_num <- 1
	for(image_row in 1:nrow(image.file)){
		for(image_col in 1:ncol(image.file)){
			#cat(image_row, ' ', image_col, '\n')

			image_split <- strsplit(image.file[image_row, image_col], '/')[[1]]
			image_name <- image_split[length(image_split)]
			success <- FALSE

			# CHECK THAT IMAGE FILE EXISTS
			if(print.progress) cat(paste0("Loading image ", image_num, " (", image_name, ")...\n"))
			if(!file.exists(image.file[image_row, image_col])){
				if(print.progress) cat(paste0(image.file[image_row, image_col], " not found.\n"))
				next
			}

			# READ IN JPEG, THIRD DIMENSION ORDER: RGB
			img_rgb <- readJPEG(source=image.file[image_row, image_col])

			# CONVERT COLOR IMAGE TO GRAYSCALE - USE THE SAME CONVERSION COEFFICIENTS AS OPENCV
			img_gray <- rgbToGray(ch1=img_rgb[, , 1], ch2=img_rgb[, , 2], ch3=img_rgb[, , 3])
			#writeJPEG(img_gray, paste0(img_file_noext[image_num], '_gray.JPG'), quality=1)

			# GET IMAGE DIMENSIONS
			nrow <- nrow(img_gray)
			ncol <- ncol(img_gray)
			
			# SET QUAD APPROXIMATION THRESHOLD IF NULL
			if(is.null(quad.approx.thresh)){
				quad_approx_thresh <- c(15, round(max(nrow, ncol)/86))
			}else{
				quad_approx_thresh <- quad.approx.thresh
			}

			# GET EXPECTED NUMBER OF QUADS
			if(nx %% 2 == 0 && ny %% 2 == 0) nquads <- (ny/2)*((nx/2) + (nx/2 + 1)) + (nx/2 + 1)
			if(nx %% 2 == 1) nquads <- ((nx-1)/2 + 1)*(ny + 1)
			if(nx %% 2 == 0 && ny %% 2 == 1) nquads <- ((ny+1)/2)*(nx/2 + (nx/2 + 1))

			# SET MAXIMUM QUAD PERIMETER
			if(is.null(perim.max)) perim.max <- round((max(nrow, ncol) / (max(nx, ny)))*5)	

			# IF SPECIFIED, NORMALIZE IMAGE HISTOGRAM
			if(normalize_image) img_gray <- equalizeImageHist(img_gray)

			# GET KERNEL SIZE FOR MEAN BLUR THRESHOLD MATRIX
			kernel <- round(min(nrow, ncol)*0.4)		

			# GET MEAN THRESHOLD MATRIX
			mean_thresh <- meanBlurImage(mat=img_gray, kernel=kernel)
			#writeJPEG(mean_thresh, paste0(img_file_noext[image_num], '_mean_thresh.JPG'), quality=1)

			# THRESHOLD IMAGE BASED ON MEAN THRESHOLD MATRIX
			img_binary <- thresholdImageMatrix(mat=img_gray, thresh_mat=mean_thresh, delta = 0, type = 1)
			if(debug && !is.null(verify.file)){
				save_debug_img <- gsub('.jpg|.jpeg', '_binary.JPG', verify.file[image_row, image_col], ignore.case=TRUE)
				writeJPEG(matrix(as.double(img_binary), nrow=nrow, ncol=ncol), save_debug_img, quality=1)
			}

			# MORPHOLOGICAL CLOSING TO REDUCE NOISE
			if(print.progress && verbose) cat("\tMinimum number of quads expected: ", nquads, "\n", "\tReducing image noise...\n")
			img_binary <- dilateImage(img_binary, kernel=3, 4)
			img_binary <- erodeImage(img_binary, kernel=3, 4)

			dilations <- dilations.min
			while(dilations <= dilations.max){
		
				# MAKE COPIES OF MATRICES
				img_gray_copy <- img_gray

				# DILATE IMAGE TO ISOLATE BLACK SQUARES
				if(print.progress && verbose) cat(paste0("\tTrying with ", dilations, " dilations...\n"))
				img_dilate <- dilateImage(img_binary, kernel=3, dilations)

				if(debug && !is.null(verify.file)){
					save_debug_img <- gsub('.jpg|.jpeg', '_dilations.JPG', verify.file[image_row, image_col], ignore.case=TRUE)
					writeJPEG(matrix(as.double(img_dilate), nrow=nrow, ncol=ncol), save_debug_img, quality=1)
				}

				# DRAW WHITE RECTANGLE AROUND THE EDGE OF THE PHOTO
				img_dilate <- drawRectangle(img_dilate, corner1=c(0,0), corner2=c(ncol-1, nrow-1), value=1, thickness=3)

				# FIND BOUNDARIES
				img_boundary <- findBoundaryPoints(img_dilate)

				if(debug && !is.null(verify.file)){
					save_debug_img <- gsub('.jpg|.jpeg', '_boundaries.JPG', verify.file[image_row, image_col], ignore.case=TRUE)
					writeJPEG(matrix(as.double(img_boundary), nrow=nrow, ncol=ncol), save_debug_img, quality=1)
				}

				# FIND QUADS
				for(k in 1:length(quad_approx_thresh)){

					quads <- generateQuads(img_dilate, img_boundary, perim.min, perim.max, poly_cont_min, poly_cont_max, poly_asp_min, quad_approx_thresh[k])

					if(nrow(quads)/4 >= nquads) break
				}
				if(print.progress && verbose) cat(paste0("\tNumber of quads found: ", nrow(quads)/4, "\n"))

				if(debug && !is.null(verify.file)){

					# ADD 1 TO QUADS FOR writeJPEG()
					quads_write <- quads + 1

					# DRAW QUADS WITH POINTS AND LINES
					img_boundary <- matrix(as.double(img_boundary), nrow=nrow, ncol=ncol)
					size <- 2
					for(i in 1:nrow(quads_write)){
						img_boundary[quads_write[i, 1]+rep(0,3), quads_write[i, 2]+-1:1] <- 0.7
						img_boundary[quads_write[i, 1]+-1:1, quads_write[i, 2]+rep(0,3)] <- 0.7
					}
					for(i in seq(1, nrow(quads_write), by=4)){
						line <- cbind(seq(0, 1, length=100)*(quads_write[i+2, 1] - quads_write[i, 1]) + quads[i, 1], seq(0, 1, length=100)*(quads_write[i+2, 2] - quads_write[i, 2]) + quads_write[i, 2])
						img_boundary[line] <- 0.7
						line <- cbind(seq(0, 1, length=100)*(quads_write[i+3, 1] - quads_write[i+1, 1]) + quads_write[i+1, 1], seq(0, 1, length=100)*(quads_write[i+3, 2] - quads_write[i+1, 2]) + quads_write[i+1, 2])
						img_boundary[line] <- 0.7
					}

					save_debug_img <- gsub('.jpg|.jpeg', '_corners.JPG', verify.file[image_row, image_col], ignore.case=TRUE)
					writeJPEG(img_boundary, save_debug_img, quality=1)
				}

				# CHECK THAT CORRECT NUMBER OF QUADS WERE FOUND
				if(nrow(quads)/4 < nquads){
					dilations <- dilations + 1
					next
				}

				# GET INTERNAL CORNERS
				int_corners <- intCornersFromQuads(quads, max_dist=max.dist.int.corners+5*dilations)

				if(print.progress & verbose){
					cat(paste0("\tNumber of internal corners expected: ", nx*ny, "\n"))
					cat(paste0("\tNumber of internal corners found in initial search: ", nrow(int_corners), "\n"))
				}

				# FIND DEVIATION FROM MEAN
				int_corners_dev <- abs((int_corners - matrix(colMeans(int_corners), nrow=nrow(int_corners), ncol=2, byrow=TRUE)) 
					/ matrix(apply(int_corners, 2, "sd"), nrow=nrow(int_corners), ncol=2, byrow=TRUE))

				# REMOVE ANY INTERNAL CORNERS THAT ARE MORE THAN TWO STANDARD DEVIATIONS FROM THE MEAN IN EITHER DIMENSION
				int_corners <- int_corners[rowSums(int_corners_dev > max.int.corner.dev) == 0, ]

				# IF NUMBER OF CORNERS EXCEEDS EXPECTATION, REMOVE MOST OUTLYING EXTRA CORNERS				
				if(nrow(int_corners) > nx*ny){

					# FIT A LINE TO THE POINTS
					lm_result <- lm(int_corners[, 2] ~ int_corners[, 1])

					# FIND THE DISTANCE FROM EACH INTERNAL CORNER TO THE LINE
					dist_line <- distancePointToLine(int_corners, l1=c(0, summary(lm_result)$coefficients[1, 1]), l2=c(1, sum(summary(lm_result)$coefficients[, 1])))
					
					# FIND THE DISTANCE FROM THE MEAN
					dist_mean <- rowSums(sqrt((int_corners - matrix(colMeans(int_corners), nrow=nrow(int_corners), ncol=2, byrow=TRUE))^2))

					# RANK DISTANCE FROM MEAN
					dist_line_rank <- rank(dist_line)

					# GET RANK THRESHOLD JUST BELOW MAXIMUM DISTANCE SET
					rank_thresh <- sort(dist_line_rank)[nx*ny]
					
					# ONLY RETAIN ROWS AT OR BELOW THRESHOLD
					int_corners <- int_corners[dist_line_rank <= rank_thresh, ]
				}

				if(print.progress & verbose) cat(paste0("\tNumber of internal corners remaining after filtering: ", nrow(int_corners), "\n"))

				# ORDER INTERNAL CORNERS
				ordered_corners <- orderCorners(int_corners, nx, ny)

				if(print.progress && verbose) cat(paste0("\tNumber of internal corners expected: ", nx*ny, "\n", "\tNumber of internal corners found: ", sum(rowSums(ordered_corners) > 0), "\n"))

				# MAKE SURE THAT ALL CORNERS WERE FOUND - (0,0) CORNERS CONSIDERED IMPOSSIBLE
				if(sum(rowSums(ordered_corners) == 0) > 0 || nrow(ordered_corners) != nx*ny){
					dilations <- dilations + 1
					if(print.progress && verbose) cat(paste0("\torderCorners() failed.\n"))
					next
				}
				
				success <- TRUE

				if(!is.null(verify.file)){

					# ADD 1 TO INTERNAL CORNERS FOR writeJPEG()
					ordered_corners_write <- ordered_corners + 1

					# ADD FIRST POINT
					rad <- 16:22
					circle <- cbind(
						rad*cos(seq(0, 2*pi, length=700)) + ordered_corners_write[1, 1],
						rad*sin(seq(0, 2*pi, length=700)) + ordered_corners_write[1, 2])
					circle <- round(circle)

					# EXCLUDE POINTS OUTSIDE RANGE
					draw <- (circle[, 1] < 1)+(circle[, 1] > nrow(img_rgb))+(circle[, 2] < 1)+(circle[, 2] > ncol(img_rgb)) == 0

					# ADD CIRCLE TO IMAGE
					img_rgb[cbind(circle[draw, ], rep(1, sum(draw)))] <- 1
					img_rgb[cbind(circle[draw, ], rep(2, sum(draw)))] <- 0
					img_rgb[cbind(circle[draw, ], rep(3, sum(draw)))] <- 0

					# ADD INTERNAL CORNER ORDER
					for(i in 1:(nrow(ordered_corners_write)-1)){

						# ADD LINE BETWEEN POINTS
						line <- cbind(
							seq(0, 1, length=1000)*(ordered_corners_write[i+1, 1] - ordered_corners_write[i, 1]) + ordered_corners_write[i, 1], 
							seq(0, 1, length=1000)*(ordered_corners_write[i+1, 2] - ordered_corners_write[i, 2]) + ordered_corners_write[i, 2])
						line <- rbind(line, line-1, line+1)

						# ADD LINE TO IMAGE
						img_rgb[cbind(line, rep(2, nrow(line)))] <- 1
					}

					# ADD LAST POINT
					rad <- 16:22
					circle <- cbind(
						rad*cos(seq(0, 2*pi, length=700)) + ordered_corners_write[nrow(ordered_corners_write), 1],
						rad*sin(seq(0, 2*pi, length=700)) + ordered_corners_write[nrow(ordered_corners_write), 2])
					circle <- round(circle)

					# EXCLUDE POINTS OUTSIDE RANGE
					draw <- (circle[, 1] < 1)+(circle[, 1] > nrow(img_rgb))+(circle[, 2] < 1)+(circle[, 2] > ncol(img_rgb)) == 0

					# ADD CIRCLE TO IMAGE
					img_rgb[cbind(circle[draw, ], rep(3, sum(draw)))] <- 1
					img_rgb[cbind(circle[draw, ], rep(2, sum(draw)))] <- 0.2
					img_rgb[cbind(circle[draw, ], rep(1, sum(draw)))] <- 0

					writeJPEG(img_rgb, verify.file[image_row, image_col], quality=1)
				}

				# FIND CORNERS TO SUBPIXEL RESOLUTION
				corners_subpixel <- findCornerSubPix(img_gray, ordered_corners, sub.pix.win, sub.pix.max.iter, criteria)

				# REVERSE X,Y ORDER
				corners_subpixel <- corners_subpixel[, 2:1]

				# SAVE TO ARRAY
				corner_array[, , image_row, image_col] <- corners_subpixel

				if(!is.null(corner.file)) write.table(corners_subpixel, file=corner.file[image_row, image_col], quote=FALSE, row.names=FALSE, col.names=FALSE, sep="\t")
		
				dilations <- dilations.max + 1
			}
			
			if(success){
				if(print.progress) cat(paste0("\t", nx*ny, " corners found successfully.\n"))
			}else{
				if(print.progress) cat(paste0("\tfindCheckerboardCorners() unsuccessful.\n"))
			}

			image_num <- image_num + 1
		}
	}

	if(dim(corner_array)[3] == 1 && dim(corner_array)[4] == 1) return(corner_array[, , 1, 1])
	if(dim(corner_array)[4] == 1) return(corner_array[, , , 1])
	corner_array
}