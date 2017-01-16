calibrateCameras <- function(img.dir, sq.size, nx, ny, cal.file, corner.dir,
	print.progress = TRUE, flip.view = FALSE, verify.dir = NULL, 
	min.views = 'max', exec.dir = NULL, undistort = FALSE, num.aspects.read = 'auto', 
	num.sample.est = 'auto', num.sample.sets = 'auto', num.aspects.sample = 'auto', 
	max.sample.optim = 30, nlm.calls.max = 20, fit.min.break = 1, objective.min = 1, 
	objective.min.break = 5, with.circles = FALSE, sample.est = NULL, ...){

	################################ CHECK INPUT PARAMETERS ##############################

	## READ CALIBRATION FILE, IF EXISTS
	cal.list <- list()
	if(file.exists(cal.file)) cal.list <- XML4R2list(file=cal.file)$calibration

	# FIND PATH TO FOLDER WHERE CALIBRATION FILE IS
	cal_file_str_split <- strsplit(cal.file, '/')[[1]]
	
	# SET CALIBRATION DIRECTORY BY REMOVING FILENAME
	if(length(cal_file_str_split) > 1){calib_dir <- paste0(paste(head(cal_file_str_split, -1), collapse="/"), "/")}else{calib_dir <- ""}
	
	# SET INPUT PARAMETERS TO OVERWRITE FROM CALIBRATION FILE (IF NON-NULL IN FILE)
	write_param_from_file <- c('img.dir', 'sq.size', 'nx', 'ny', 'corner.dir', 'flip.view', 'verify.dir')

	# OVERWRITE ANY NULL INPUT PARAMETERS WITH VALUE IN CALIBRATION FILE (IF NON-NULL IN FILE)
	for(write_param in write_param_from_file)
		if(is.null(get(write_param)) && !is.null(cal.list[[write_param]])) assign(write_param, cal.list[[write_param]])

	# GET SQUARE SIZES AND UNITS
	sq.size.num <- as.numeric(gsub('[[:alpha:], ]', '', sq.size))
	sq.size.units <- gsub('[[:digit:]., ]', '', sq.size)

	# CHECK CALIBRATION IMAGE INPUTS UNLESS CAL FILE IMGS MATCHES INPUT
	check_img_input <- TRUE
	if(!is.null(img.dir) && !is.null(cal.list[['img.dir']])) if(cal.list[['img.dir']] == img.dir) check_img_input <- FALSE

	# CHECK CALIBRATION IMAGE INPUTS
	if(is.null(cal.list$img.list.files)){
		if(!file.exists(img.dir)) stop(paste0("Folder '", img.dir, "' not found."))
		if(length(list.files(img.dir)) == 0) stop(paste0("No files/folders found in '", img.dir, "'."))
		if(length(list.files(img.dir)) == 1) stop(paste0("Only one file/folder found in '", img.dir, "'. Two views are required for stereo calibration."))
	}

	# GET FILE NAMES IN CALIBRATON IMAGE FOLDER
	if(file.exists(img.dir) && length(list.files(img.dir)) > 0){
		imgs_list_files <- list.files(img.dir)
	}else{
		imgs_list_files <- cal.list$img.list.files
	}

	# GET FILE PATHS TO CALIBRATON IMAGE SUB-FOLDERS
	img_fpaths <- paste0(img.dir, '/', imgs_list_files)
	
	# SET NUMBER OF CAMERA VIEWS
	num_views <- length(img_fpaths)
	
	# SET MIN NUMBER OF VIEWS TO USE IN ESTIMATING CALIBRATION COEFFICIENTS
	if(min.views == 'max'){min_views <- max(num_views)}else{min_views <- min.views}

	# CHECK THAT CAL MIN VIEWS IS EQUAL TO OR GREATER THAN TWO
	if(min_views < 2) stop("'min.views' must be greater or equal to 2.")
	
	# CHECK IF MOVIE FILES
	if(sum(grepl('(.mov|.avi|.mp4|.mpg)$', img_fpaths, ignore.case=TRUE)) > 0){img_type <- 'video'}else{img_type <- 'image'}

	# GET SUB-FOLDER NAMES
	img_sub_dir <- gsub('(.mov|.avi|.mp4|.mpg)$', '', imgs_list_files, ignore.case=TRUE)

	# GET NUMBER OF SPACES TO ALIGN RIGHT OF SUB DIRECTORY NAMES
	img_sub_dir_salign <- (1 + max(nchar(img_sub_dir))) - nchar(img_sub_dir)
	
	# IF THE NUMBER OF IMAGES EXCEEDS 50 TREAT AS VIDEO FRAMES
	if(img_type == 'image'){
		num_imgs <- length(list.files(paste0(img.dir, '/', imgs_list_files[1])))
		if(num_imgs > 50) img_type <- 'video frames'
	}
	
	# SET DEFAULTS
	img_fnames <- NULL
	vid_fnames <- NULL
	vid_nframes <- NULL
	vid_fps <- NULL
	vid_dur_sec <- NULL
	img_size <- NULL
	video_method <- ''

	if(img_type == 'image'){

		# GET IMAGE NAMES
		if(!file.exists(img.dir)){
			if(!is.null(cal.list$img.fnames)){
				img_fnames_v1 <- img_fnames_v2 <- cal.list$img.fnames
			}else{
				stop(paste0("Image file directory '", img.dir, "' does not exist and image names are not included in calibration file."))
			}
		}else{
			img_fnames_v1 <- list.files(paste0(img.dir, '/', imgs_list_files[1]))
			img_fnames_v2 <- list.files(paste0(img.dir, '/', imgs_list_files[2]))
		}
		
		# FIND COMMON CALIBRATION IMAGE FILENAMES
		img_fnames <- img_fnames_v1[img_fnames_v1 %in% img_fnames_v2]

		# CHECK THAT THERE ARE AT LEAST TWO IMAGES COMMON ASPECTS
		if(length(img_fnames) < 2) stop(paste0("Number of common images between two views in '", img.dir, "' is less than two. A minimum of two images are required for stereo calibration and more than five are recommended."))

		# GET IMAGE SIZE IN EACH VIEW
		if(undistort){
			#stop("Undistortion not yet available for image input.")
			
			if(!is.null(cal.list$img.size)){

				# Get image sizes from calibration file
				img_size <- cal.list$img.size

			}else{

				# Get image sizes
				img_size <- matrix(NA, nrow=num_views, ncol=2, dimnames=list(img_sub_dir, c('w', 'h')))

				for(i in 1:num_views){

					# File path to first image
					first_image_fpath <- paste0(img.dir, '/', imgs_list_files[i], '/', img_fnames[1])

					# Get image dimensions
					if(grepl(pattern='[.]jpg$|[.]jpeg$', x=img_fnames[1], ignore.case=TRUE)) img_dim <- dim(readJPEG(first_image_fpath, native=TRUE))
					if(grepl(pattern='[.]png$', x=img_fnames[1], ignore.case=TRUE)) img_dim <- dim(readPNG(first_image_fpath, native=TRUE))
					if(grepl(pattern='[.]tif$|[.]tiff$', x=img_fnames[1], ignore.case=TRUE)) img_dim <- dim(readTIFF(first_image_fpath, native=TRUE))
		
					# Set image dimensions
					img_size[i, 'w'] <- img_dim[2]
					img_size[i, 'h'] <- img_dim[1]
				}
			}
		}

	}else if(img_type %in% c('video', 'video frames')){
	
		# SET VIDEO READING METHOD
		if(is.null(exec.dir)){video_method <- 'ffmpeg'}else{video_method <- 'opencv'}

		# GET CALIBRATION VIDEO NAMES
		vid_fnames <- setNames(imgs_list_files, gsub('[.][A-Za-z0-9]*$', '', imgs_list_files))

		# CHECK THAT FFMPEG IS INSTALLED AND ACCESSIBLE FROM SYSTEM
		if(video_method == 'ffmpeg'){

			check_system_command_SM('ffmpeg')

		}else{

			# CHECK EXEC PATH EXISTS
			if(exec.dir != '' && !file.exists(exec.dir)) stop(paste0("exec.dir '", exec.dir, "' not found."))

			# CHECK THAT EXECUTABLES ARE IN FOLDER
			if(img_type == 'video' && !'get_frame_count' %in% list.files(exec.dir)) stop(paste0("'get_frame_count' not found in '", exec.dir, "'."))
			if(img_type == 'video' && !'find_checkerboard_corners' %in% list.files(exec.dir)) stop(paste0("'find_checkerboard_corners' not found in '", exec.dir, "'."))

			# ADD SLASH AT END IF NOT PRESENT
			if(!grepl('[/]$', exec.dir)) exec.dir <- paste0(exec.dir, '/')
		}

		if(!is.null(cal.list$vid.nframes) && !is.null(cal.list$img.size) && FALSE){

			vid_nframes <- cal.list$vid.nframes
			vid_fps <- cal.list$fps
			img_size <- cal.list$img.size
			vid_dur_sec <- cal.list$vid.duration

		}else{

			vid_nframes <- rep(NA, length(vid_fnames))
			vid_fps <- rep(NA, length(vid_fnames))
			vid_dur_sec <- rep(NA, length(vid_fnames))
			img_size <- matrix(NA, nrow=num_views, ncol=2, dimnames=list(img_sub_dir, c('w', 'h')))

			if(img_type == 'video'){

				for(i in 1:length(vid_fnames)){

					# FIND FRAME COUNT AND FRAME DIMENSIONS
					if(video_method == 'ffmpeg'){

						## USING FFMPEG
						# GET FILE INFO
						ffmpeg_i <- suppressWarnings(system2(command='ffmpeg', args=paste0("-i ", gsub(' ', '\\\\ ', img.dir), '/', gsub(' ', '\\\\ ', vid_fnames[i])), 
							stdout=FALSE, stderr=TRUE))
						ffmpeg_i <- paste(ffmpeg_i, collapse='\n')
						
						# READ VIDEO INFO
						video_i <- read_video_info(ffmpeg_i)
						#print(video_i)
						
						# SET VIDEO INFO
						vid_nframes[i] <- video_i$frames
						vid_fps[i] <- video_i$fps
						vid_dur_sec[i] <- video_i$duration.sec
						img_size[i, ] <- video_i$dims

					}else{

						## USING OPENCV
						# SET COMMAND TO FIND FRAME COUNT
						command <- paste0('./', gsub(' ', '\\\\ ', exec.dir), 'get_frame_count ', 
							gsub(' ', '\\\\ ', img.dir), '/', gsub(' ', '\\\\ ', vid_fnames[i]))

						# FIND NUMBER OF FRAMES IN VIDEO
						vid_nframes[i] <- as.numeric(system(command=command, intern=TRUE))

						# SET COMMAND TO FIND FRAME SIZE
						command <- paste0('./', gsub(' ', '\\\\ ', exec.dir), 'get_frame_size ', 
							gsub(' ', '\\\\ ', img.dir), '/', gsub(' ', '\\\\ ', vid_fnames[i]))

						# SET IMAGE SIZE
						img_size[i, ] <- as.numeric(strsplit(x=system(command=command, intern=TRUE), split=',')[[1]])
					}
				}
			}else{

				# COUNT NUMBER OF IMAGES IN EACH FOLDER AS A COUNT OF THE NUMBER OF FRAMES
				for(i in 1:length(vid_fnames)) vid_nframes[i] <- length(list.files(paste0(img.dir, '/', imgs_list_files[i])))
				
				# MAKE SURE THE NUMBER OF VIDEO FRAMES ARE THE SAME AMONG ALL VIEWS
				if(sd(vid_nframes) > 0){
					stop_str <- ('When input is \'video frames\' the number of frames in each view must be the same to ensure proper ordering of detected corners.\n')
					stop_str <- paste0(stop_str, '\tNumber of frames:\n')
					for(i in 1:length(vid_fnames)){
						stop_str <- paste0(stop_str, paste0("\t\t", vid_fnames[i], paste(rep(' ', img_sub_dir_salign[i]), collapse=''), 
							" (", vid_nframes[i], " frames)\n"))
					}
					stop(stop_str)
				}				

				# COUNT NUMBER OF IMAGES IN EACH FOLDER AS A COUNT OF THE NUMBER OF FRAMES
				for(i in 1:length(vid_fnames)){
					image_full_fpath <- paste0(img.dir, '/', imgs_list_files[i], '/', list.files(paste0(img.dir, '/', imgs_list_files[i]))[1])

					# Get image dimensions
					if(grepl(pattern='[.]jpg$|[.]jpeg$', x=image_full_fpath, ignore.case=TRUE)) img_dim <- dim(readJPEG(image_full_fpath, native=TRUE))
					if(grepl(pattern='[.]png$', x=image_full_fpath, ignore.case=TRUE)) img_dim <- dim(readPNG(image_full_fpath, native=TRUE))
					if(grepl(pattern='[.]tif$|[.]tiff$', x=image_full_fpath, ignore.case=TRUE)) img_dim <- dim(readTIFF(image_full_fpath, native=TRUE))
	
					# Set image dimensions
					img_size[i, ] <- c(img_dim[2], img_dim[1])
				}
			}
		}
	}

	# SET DEFAULTS
	verify_fpaths <- NULL
	verify_fnames <- NULL

	# IF VIDEO, MAKE SURE CAL VERIFY IS MADE
	if(img_type %in% c('video', 'video frames') && is.null(verify.dir)) stop(paste0("If input type is video, 'verify.dir' must be non-NULL."))

	# CHECK IF CORNERS FOLDER EXISTS
	if(!file.exists(corner.dir)) dir.create(path=corner.dir)

	# CREATE SUB FOLDERS IF NOT PRESENT
	for(dir_name in img_sub_dir) if(!file.exists(paste0(corner.dir, '/', dir_name))) dir.create(paste0(corner.dir, '/', dir_name))

	# CHECK CAL VERIFY IF NON-NULL
	if(file.exists(img.dir) && !is.null(verify.dir)){

		# CREATE VERIFY FOLDER IF DOES NOT EXIST
		if(!file.exists(verify.dir)) dir.create(verify.dir)

		# CREATE VIEW FOLDERS IF VERIFY FOLDER IS EMPTY
		for(dir_name in img_sub_dir) if(!file.exists(paste0(verify.dir, '/', dir_name))) dir.create(paste0(verify.dir, '/', dir_name))

		# SET VERIFY FILE PATHS AND FILE NAMES
		verify_fpaths <- paste0(verify.dir, '/', list.files(verify.dir))
		if(img_type == 'image') verify_fnames <- img_fnames
	}

	# SAVE TO CAL.LIST
	cal.list[['img.type']] <- img_type
	cal.list[['img.size']] <- img_size
	cal.list[['img.fpaths']] <- img_fpaths
	cal.list[['img.sub.dir']] <- img_sub_dir
	cal.list[['img.fnames']] <- img_fnames
	cal.list[['img.list.files']] <- imgs_list_files
	cal.list[['min.views']] <- min_views
	cal.list[['vid.fnames']] <- vid_fnames
	cal.list[['vid.fps']] <- vid_fps
	cal.list[['vid.nframes']] <- vid_nframes
	cal.list[['vid.duration']] <- vid_dur_sec

	# WRITE INPUT PARAMETERS TO CAL.LIST
	for(write_param in write_param_from_file) cal.list[[write_param]] <- get(write_param)

	# PRINT INPUT PARAMETERS
	if(print.progress){
		cat("calibrateCameras\n\n")
		cat("\tChecking input parameters...\n")
		cat(paste0("\t\tCalibration file: '", cal.file, "'\n"))
		cat(paste0("\t\tCalibration input type: ", img_type, "\n"))
		cat(paste0("\t\tNumber of camera views: ", num_views, "\n"))
		if(num_views == 2) cat(paste0("\t\tOne view upside-down?: ", flip.view, "\n"))
		
		if(img_type == 'image'){
			cat(paste0("\t\tCalibration image set:\n"))
			cat(paste0("\t\t\tNumber of common images per view: ", length(img_fnames), "\n"))
			cat(paste0("\t\t\t\tFilenames: ", paste0(gsub('.jpeg|.jpg|.tiff', '', img_fnames, ignore.case=TRUE), collapse=", "), "\n"))
		}else if(img_type %in% c('video', 'video frames')){
			cat(paste0("\t\tCalibration videos:\n"))
			cat(paste0("\t\t\tFilenames:\n"))
			for(i in 1:length(vid_fnames)){
				if(!is.null(img_size)){img_dim <- paste0(', ', paste(img_size[i, ], collapse='x'))}else{img_dim <- ''}
				cat(paste0("\t\t\t\t", vid_fnames[i], paste(rep(' ', img_sub_dir_salign[i]), collapse=''), 
					" (", vid_nframes[i], " frames", img_dim, ")\n"))
			}
		}
		cat(paste0("\t\t\tSquare size: ", sq.size, "\n"))
		cat(paste0("\t\t\tInternal corners: ", nx, " x ", ny, " (", nx*ny, " total)\n"))
		
		cat("\n")
	}

	# SAVE CALIBRATION LIST TO FILE
	list2XML4R(list('calibration' = cal.list), file=cal.file)

	########################## CALIBRATION CHECKERBOARD DETECTION ########################
	if(print.progress) cat("\tCalibration checkerboard corner detection...")

	# CHECK IF CORNERS ARE ALREADY FOUND
	detect_corners <- TRUE

	# CHECK IF CORNERS ARE FOUND IN ALL VIEWS
	list_files_length <- rep(NA, num_views)
	for(i in 1:num_views) list_files_length[i] <- length(list.files(paste0(corner.dir, '/', img_sub_dir[i])))

	# IF ALL FOLDERS HAVE FILES, PROMPT WHETHER TO RE-DETECT CORNERS
	if(sum(list_files_length > 0) == num_views){

		if(print.progress){
			cat(paste0("Saved calibration corners found for all views in '", corner.dir, "' folder.\n\n"))

			for(i in 1:num_views){
				num_frames_detected <- length(list.files(paste0(corner.dir, '/', img_sub_dir[i])))
				if(img_type == 'image'){
					cat(paste0("\t\t", img_sub_dir[i], paste(rep(' ', img_sub_dir_salign[i]), collapse='')))
					cat(paste0(": Corners detected in ", num_frames_detected, " aspects\n"))
				}else{
					cat(paste0("\t\t", vid_fnames[i], paste(rep(' ', img_sub_dir_salign[i]), collapse='')))
					cat(paste0(": Corners detected in ", num_frames_detected, " frames\n"))
				}
			}
		}else{
			if(print.progress) cat('\n\n')
		}

		detect_corners <- FALSE

		if(file.exists(img.dir)){
			response <- readline(prompt="\n\t\tDo you wish to re-detect the calibration corners? (y/n) : ");
			if(print.progress) cat("\n")
			#response <- 'n'
			if(tolower(response) %in% c('yes', 'y')) detect_corners <- TRUE
		}else{
			detect_corners <- FALSE
		}

	}else{
		if(print.progress) cat('\n')
	}

	## DETECT CALIBRATION CHECKERBOARD CORNERS
	if(detect_corners){

		if(img_type == 'image'){

			# CREATE ARRAY FOR CORNERS
			cal_corners <- array(NA, dim=c(nx*ny, 2, length(img_fnames), num_views))

			# FIND CHECKERBOARD CORNERS AND READ INTO ARRAY
			if(print.progress) cat("\t\tRunning automated checkerboard corner detection on calibration images...\n")
			for(i in 1:length(img_fnames)){

				# FIND IMAGE FILES IN EACH VIEW FOLDER
				if(print.progress) cat("\t\t\t", img_fnames[i], "\n", sep="")

				for(j in 1:num_views){
		
					# SET VERIFY FILEPATH IF NON-NULL
					verify_fpath <- NULL
					if(!is.null(verify_fpaths)) verify_fpath <- paste0(verify_fpaths[j], '/', verify_fnames[i])

					# SET CORNER FILEPATH
					corner_fpath <- paste0(corner.dir, '/', img_sub_dir[j], '/', gsub('[.][A-Za-z]+$', '.txt', img_fnames[i]))
				
					# SPECIFY WHETHER TO FLIP CORNER ORDER
					if(flip.view && j == 2){flip <- TRUE}else{flip <- FALSE}

					cal_corners[, , i, j] <- findCheckerboardCorners(image.file=paste0(img_fpaths[j], '/', img_fnames[i]), 
						nx=nx, ny=ny, flip=flip, corner.file=corner_fpath, verify.file=verify_fpath, print.progress=FALSE)

					if(print.progress){
						cat("\t\t\t\tView ", j, " : ", sep="")
						if(is.na(cal_corners[1, 1, i, j])){cat("findCheckerboardCorners() unsuccessful")}else{cat(nx*ny, " corners found", sep="")}
						cat("\n")
					}
				}
			}

			# SET DIMNAMES FOR CORNER ARRAY
			dimnames(cal_corners) <- list(NULL, NULL, gsub('[.][A-Za-z0-9]*$', '', img_fnames), img_sub_dir)

			if(print.progress) cat('\n')

		}else if(img_type %in% c('video', 'video frames')){

			if(num.aspects.read == 'auto') num.aspects.read <- 60

			# GET MINIMUM NUMBER OF FRAMES AMONG ALL VIDEOS
			min_nframes <- min(vid_nframes)

			if(video_method == 'ffmpeg') min_duration <- min(vid_dur_sec)

			# CHECK THAT NUMBER OF ASPECTS IN VIDEO EXCEEDS SAMPLE NUMBER
			if(min_nframes < num.aspects.read + 40){
				cat('\n')
				stop(paste0("The number of video frames (", min(vid_nframes), ") is less than the number of aspects to be sampled for checkerboard detection (", num.aspects.read + 40, ")."))
			}
		
			for(i in 1:num_views){

				if(print.progress) cat(paste0("\t\tDetecting corners in '", vid_fnames[i], "'..."))
				
				if(video_method == 'ffmpeg'){

					# SET FRAME NUMBERS
					frame_nums <- round(seq(0,min_nframes-5,length=num.aspects.read)) + 1
					
					# EXTRACT FRAMES
					extractFrames(file=paste0(img.dir, '/', vid_fnames[i]), save.to=paste0(verify.dir, '/', img_sub_dir[i]),
						frames=frame_nums, video.i=list('fps'=vid_fps[i], 'frames'=min_nframes), warn.min=NULL)
					
					# GET FILENAMES
					save_to_names <- list.files(paste0(verify.dir, '/', img_sub_dir[i]))

					for(j in 1:length(save_to_names)){

						# SPECIFY WHETHER TO FLIP CORNER ORDER
						if(flip.view && i == 2){flip <- TRUE}else{flip <- FALSE}

						# SET CORNER FILEPATH
						corner_fpath <- paste0(corner.dir, '/', img_sub_dir[i], '/', gsub('[.][A-Za-z]+$', '.txt', save_to_names[j]))

						# DETECT CORNERS IN IMAGES IN VERIFY DIRECTORY
						image_fpath <- paste0(verify.dir, '/', img_sub_dir[i], '/', save_to_names[j])
						findCheckerboardCorners(image.file=image_fpath, nx=nx, ny=ny, flip=flip, corner.file=corner_fpath, 
							sub.pix.win.min=23, verify.file=image_fpath, print.progress=FALSE)
					}

				}else{

					# SET INPUT TYPE
					input_video <- 1
					if(img_type == 'video frames') input_video <- 0

					# WRITE COMMAND
					command <- paste0(
						'./', gsub(' ', '\\\\ ', exec.dir), 'find_checkerboard_corners ', 
						gsub(' ', '\\\\ ', img.dir), '/', gsub(' ', '\\\\ ', vid_fnames[i]), ' ', 
						gsub(' ', '\\\\ ', corner.dir), '/', gsub(' ', '\\\\ ', img_sub_dir[i]), ' ', 
						gsub(' ', '\\\\ ', verify.dir), '/', gsub(' ', '\\\\ ', img_sub_dir[i]), ' ', 
						nx, ' ', ny, ' ', num.aspects.read, ' ', num.aspects.read+80, ' ', 
						min_nframes, ' 0 ', as.numeric(with.circles), ' ', input_video)
				
					# CALL COMMAND
					#cat('\n');cat(command, '\n')
					system(command=command)
				}
				
				if(print.progress){

					# NUMBER OF CORNERS DETECTED
					num_frames_detected <- length(list.files(paste0(corner.dir, '/', img_sub_dir[i])))

					cat(paste(rep(' ', img_sub_dir_salign[i]), collapse=''))
					cat(paste0(" Corners detected in ", num_frames_detected, " frames\n"))
				}
			}
		}
	}

	# READ IN CORNERS IF NOT DETECTED FROM IMAGES
	if(!detect_corners || img_type %in% c('video', 'video frames')){

		# GET FRAME NAMES
		frame_names <- c()
		for(i in 1:num_views) frame_names <- c(frame_names, gsub('.txt', '', list.files(paste0(corner.dir, '/', img_sub_dir[i]))))

		# GET UNIQUE FRAME NAMES
		frame_names_unique <- unique(frame_names)

		# MAKE CORNER ARRAY
		cal_corners <- array(NA, dim=c(nx*ny, 2, length(frame_names_unique), num_views), 
			dimnames=list(NULL, NULL, frame_names_unique, img_sub_dir))

		# FILL CORNER ARRAY
		for(i in 1:num_views){

			# GET CORNER FILES
			corner_files <- gsub('.txt', '', list.files(paste0(corner.dir, '/', img_sub_dir[i])))
	
			# READ CORNERS INTO ARRAY FROM FILES
			for(j in 1:length(corner_files)){
				cal_corners[, , corner_files[j], i] <- 
					as.matrix(read.table(paste0(corner.dir, '/', img_sub_dir[i], '/', corner_files[j], '.txt')))
			}
		}
	}

	# FIND NUMBER OF NON-NA VIEWS FOR EACH ASPECT
	aspect_non_na <- rowSums(apply(!is.na(cal_corners), c(3, 4), 'sum') > 0)

	# FIND PAIRS WITH CORNERS IN MINIMUM NUMBER OF VIEWS
	cal_min_views_found <- aspect_non_na >= min_views
	
	# FIND TOTAL NUMBER OF ASPECTS FOUND FOR CALIBRATION
	cal_views_found_num <- sum(cal_min_views_found)

	# FIND NUMBER OF NON-NA VIEWS FOR EACH ASPECT AND VIEW
	non_na_by_view <- apply(!is.na(cal_corners), c(3, 4), 'sum') > 0
	
	# SET VIEW COMBINATIONS
	if(num_views <= 2){view_combos <- list(c(1,2))}else{view_combos <- list(1:2, 2:3, c(1,3), 1:3)}

	# FIND OVERLAPPING ASPECTS AMONG ALL PAIRS OF VIEWS
	view_overlaps <- NULL
	if(num_views > 2){

		# FIND NUMBER OF OVERLAPPING ASPECTS
		view_overlaps <- rep(NA, length(view_combos))
		for(i in 1:length(view_combos)){
			view_overlaps[i] <- sum(rowSums(non_na_by_view[, view_combos[[i]]]) == length(view_combos[[i]]))
			names(view_overlaps)[i] <- paste(view_combos[[i]], collapse='_')
		}
	}

	# CHECK FOR TWO GOOD PAIRS OF OVERLAPPING VIEWS BUT NO OVERLAP AMONG THREE VIEWS
	unify_view_pairs <- FALSE
	if(num_views > 2 && min(view_overlaps[c('1_2', '2_3', '1_3')]) < 5 && view_overlaps['1_2_3'] < 5) unify_view_pairs <- TRUE

	if(print.progress){
		cat(paste0("\t\tNumber of cases in which corners were found in at least ", min_views, " views: ", cal_views_found_num, "\n"))
		if(img_type == 'image') cat(paste0("\t\t\tFilenames: ", paste0(gsub('.jpeg|.jpg|.tiff', '', img_fnames[cal_min_views_found], ignore.case=TRUE), collapse=", "), "\n"))
		cat('\n')

		if(num_views > 2){
			cat(paste0("\t\tNumber of aspects in which corners were found for all view combinations\n"))
			for(i in 1:length(view_combos)) cat(paste0("\t\t\t", paste(img_sub_dir[view_combos[[i]]], collapse=', '), ": ", view_overlaps[i], "\n"))
			cat(paste0("\n\t\tCalibration will calibrate view pairs and unify calibration coordinates\n"))
			cat('\n')
		}
	}
	
	######################### ESTIMATE UNDISTORTION COEFFICIENTS #########################

	if(undistort){

		if(print.progress) cat("\tEstimating undistortion parameters...")

		# CHECK IF UNDISTORTION PARAMETERS ARE ALREADY FOUND
		estimate_undistortion <- TRUE
		if(!is.null(cal.list$undistort.params) && !is.null(cal.list$distort.params)){

			# READ CALIBRATION CORNERS INTO ARRAY
			undistort_params <- cal.list$undistort.params
			distort_params <- cal.list$distort.params

			if(print.progress) cat("Undistortion parameters found in the calibration file.\n\n")

			estimate_undistortion <- FALSE

			response <- readline(prompt="\t\tDo you wish to re-estimate the undistortion parameters? (y/n) : ");cat("\n")
			#response <- 'n'
		
			if(tolower(response) %in% c('yes', 'y')) estimate_undistortion <- TRUE
		}else{

			if(print.progress) cat('\n')
			undistort_params <- matrix(NA, nrow=num_views, ncol=7, dimnames=list(img_sub_dir, c('cx', 'cy', 'k1', 'k2', 'k3', 'p1', 'p2')))
			distort_params <- matrix(NA, nrow=num_views, ncol=7, dimnames=list(img_sub_dir, c('cx', 'cy', 'k1', 'k2', 'k3', 'p1', 'p2')))
		}

		# SET MAXIMUM NUMBER OF ASPECTS TO USE IN UNDISTORTION
		max_undist_sample <- 15
		
		if(estimate_undistortion){
		
			# FIND DISTORTION COEFFICIENTS BY VIEW
			for(view in 1:num_views){

				# FIND NON-NA ASPECTS
				undist_sample_nona <- (1:dim(cal_corners)[3])[!is.na(cal_corners[1, 1, , view])]
				
				# SELECT AMONG NON-NA ASPECTS, SPECIFIED NUMBER BUT NO MORE THAN LENGTH
				undist_sample_nona <- undist_sample_nona[1:min(max_undist_sample, length(undist_sample_nona))]
				#undist_sample_nona <- undist_sample_nona[round(seq(1, length(undist_sample_nona), length=min(max_undist_sample, length(undist_sample_nona))))]

				# ESTIMATE UNDISTORTION COEFFICIENTS
				undist_params <- estimateUndistortion(coor.2d=cal_corners[, , undist_sample_nona, view], nx, 
					image.size=img_size[view, ])

				# ESTIMATE DISTORTION COEFFICIENTS (TO DISTORT EPIPOLAR LINES)
				dist_params <- estimateDistortion(undist_params, img_size[view, ])
				
				undistort_params[view, ] <- undist_params
				distort_params[view, ] <- dist_params
			}

			cal.list[['undistort.params']] <- undistort_params
			cal.list[['distort.params']] <- distort_params

			# SAVE CALIBRATION LIST
			list2XML4R(list('calibration' = cal.list), file=cal.file)
		}

		if(print.progress){

			for(view in 1:num_views){

				# FIND NON-NA ASPECTS
				undist_sample_nona <- (1:dim(cal_corners)[3])[!is.na(cal_corners[1, 1, , view])]
				
				# SELECT AMONG NON-NA ASPECTS, SPECIFIED NUMBER BUT NO MORE THAN LENGTH
				undist_sample_nona <- undist_sample_nona[1:min(max_undist_sample, length(undist_sample_nona))]
				#undist_sample_nona <- undist_sample_nona[round(seq(1, length(undist_sample_nona), length=min(max_undist_sample, length(undist_sample_nona))))]

				# FIND HOMOGRAPHY ERROR OF ORIGINAL COORDINATES
				findHo <- findHomography(cal_corners[, , undist_sample_nona, view], nx=nx)
				#findHo <- findHomography(cal_corners[, , !is.na(cal_corners[1, 1, , view]), view], nx=nx)

				# UNDISTORT CORNERS USED IN UNDISTORTION SAMPLE
				coor_2d_u <- undistort(cal_corners[, , undist_sample_nona, view], image.size=img_size[view, ],
					center=undistort_params[view, 1:2], k=undistort_params[view, 3:5], p=undistort_params[view, 6:7])
				#coor_2d_u <- undistort(cal_corners[, , !is.na(cal_corners[1, 1, , view]), view], image.size=img_size[view, ],
				#	center=undistort_params[view, 1:2], k=undistort_params[view, 3:5], p=undistort_params[view, 6:7])

				# FIND HOMOGRAPHY ERROR OF UNDISTORTED COORDINATES
				findHu <- findHomography(coor_2d_u, nx=nx)

				cat(paste0("\t\t", img_sub_dir[view], "\n"))
				cat(paste0("\t\t\tParameters (cx, cy, k1): ", paste(undistort_params[view, ], collapse=', '), "\n"))
				cat(paste0("\t\t\tMean homography fit (original, after distortion correction): ", 
					round(mean(findHo$error), 2), " px, ", round(mean(findHu$error), 2), " px\n"))
				cat(paste0("\t\t\tMax homography fit (original, after distortion correction): ", 
					round(max(findHo$error), 2), " px, ", round(max(findHu$error), 2), " px\n"))
			}
			
			cat('\n')
		}

		# UNDISTORT ALL CORNERS
		for(view in 1:num_views) cal_corners[, , , view] <- undistort(cal_corners[, , , view], 
			image.size=img_size[view, ], center=undistort_params[view, 1:2], 
			k=undistort_params[view, 3:5], p=undistort_params[view, 6:7])

	}else{

		if(print.progress){

			for(view in 1:num_views){

				# FIND HOMOGRAPHY ERROR OF ORIGINAL COORDINATES
				#findHo <- findHomography(cal_corners[, , !is.na(cal_corners[1, 1, , view]), view], nx=nx)

				#cat(paste0("\t\t", img_sub_dir[view], "\n"))
				#cat(paste0("\t\t\tMean homography fit (original): ", 
				#	round(mean(findHo$error), 2), " px\n"))
				#cat(paste0("\t\t\tMax homography fit (original): ", 
				#	round(max(findHo$error), 2), " px\n"))
			}
			
			#cat('\n')
		}
	}

	######################## ESTIMATE DLT CALIBRATION COEFFICIENTS #######################
	if(print.progress) cat("\tCalibration coefficient estimation...")

	# IF CALIBRATION COEFFICIENTS ALREADY IN LIST, ASK USER WHETHER TO RE-ESTIMATE COEFFICIENTS
	estimate_cal_coeffs <- TRUE
	if(!is.null(cal.list$cal.coeff) || (!is.null(cal.list$sub1.cal.coeff) && !is.null(cal.list$sub2.cal.coeff))){
		
		# READ FROM LIST
		cal_set_num <- cal.list$cal.set.num
		cal_coeff <- cal.list$cal.coeff
		mean_reconstruct_rmse <- cal.list$mean.reconstruct.rmse
		coefficient_rmse <- cal.list$coefficient.rmse

		if(print.progress) cat("Saved calibration DLT coefficients found in the calibration file.\n\n")

		estimate_cal_coeffs <- FALSE
		
		response <- readline(prompt="\t\tDo you wish to re-estimate the calibration coefficients? (y/n) : ");cat('\n')
		#response <- 'n'
		
		if(tolower(response) %in% c('yes', 'y')) estimate_cal_coeffs <- TRUE

	}else{
		if(print.progress) cat('\n')
	}

	# CHECK THAT CORNERS WERE FOUND IN AT LEAST 3 ASPECTS
	if(cal_views_found_num < 2 && !unify_view_pairs) stop(paste0("Corners were only detected in 1 or fewer aspects. At least 2 aspects are required for calibration."))

	# CREATE ACCURACY CHECK FOLDERS
	if(num.sample.est != 'auto'){

		# CREATE ERROR TESTS FOLDER
		if(!file.exists(paste0(calib_dir, 'Error tests'))) dir.create(paste0(calib_dir, 'Error tests'))

		# FIND COMBINATIONS OF ALL VIEWS
		view_combo_sub_dir <- NULL
		if(num_views > 2){

			# CREATE SUB-FOLDER NAMES
			view_combo_sub_dir <- rep(NA, length(view_combos))
			for(i in 1:length(view_combos)) view_combo_sub_dir[i] <- paste(img_sub_dir[view_combos[[i]]], collapse=', ')

			# CREATE SUB-FOLDERS IF THEY DO NOT EXIST
			for(dir_name in view_combo_sub_dir) if(!file.exists(paste0(calib_dir, 'Error tests/', dir_name))) dir.create(paste0(calib_dir, 'Error tests/', dir_name))
		}
	}
	
	# NUMBER OF COEFFICIENT ESTIMATION RUNS
	num_coeff_est_runs <- 1
	if(unify_view_pairs) num_coeff_est_runs <- 2

	for(est_run in 1:num_coeff_est_runs){

		cal_corners_sub <- cal_corners
		aspect_non_na_sub <- aspect_non_na
		min_views_sub <- min_views
		cal_views_found_num_sub <- cal_views_found_num
		num_views_sub <- num_views
		img_sub_dir_sub <- img_sub_dir
		img_sub_dir_salign_sub <- img_sub_dir_salign
		non_na_by_view_sub <- non_na_by_view

		if(unify_view_pairs){
			
			# FIND SUBSET (PAIR) OF VIEWS THAT HAVE OVERLAPPING CORNER SETS
			if(est_run == 1) view_subset <- view_combos[[which(view_overlaps > 5)[1]]]
			if(est_run == 2) view_subset <- view_combos[[which(view_overlaps > 5)[2]]]

			if(print.progress) cat(paste0("\n\t\tEstimating calibration coefficients for view subset: ", paste(img_sub_dir[view_subset], collapse=', '), "\n\n"))

			cal_corners_sub <- cal_corners[, , , view_subset]
			aspect_non_na_sub <- rowSums(apply(!is.na(cal_corners_sub), c(3, 4), 'sum') > 0)
			min_views_sub <- length(view_subset)
			cal_views_found_num_sub <- sum(aspect_non_na_sub >= min_views_sub)
			num_views_sub <- length(view_subset)
			img_sub_dir_sub <- img_sub_dir[view_subset]
			img_sub_dir_salign_sub <- img_sub_dir_salign[view_subset]
			non_na_by_view_sub <- apply(!is.na(cal_corners_sub), c(3, 4), 'sum') > 0
		}

		# ESTIMATE DLT COEFFICIENTS
		estimate_return <- estimateDLTCoefficients(cal.list, sample.est, num.sample.est, 
			cal_corners=cal_corners_sub, aspect_non_na=aspect_non_na_sub, min_views=min_views_sub, 
			cal_views_found_num=cal_views_found_num_sub, estimate_cal_coeffs, 
			num.sample.sets, num.aspects.sample, max.sample.optim, num_views=num_views_sub, 
			img_sub_dir=img_sub_dir_sub, non_na_by_view=non_na_by_view_sub, 
			print.progress, nx, ny, sq.size.num, fit.min.break, objective.min, 
			objective.min.break, nlm.calls.max, img_sub_dir_salign=img_sub_dir_salign_sub, 
			unify_view_pairs, unify_sub=est_run, ...)
		cal.list <- estimate_return$cal.list
		cal_corners_trim_optim <- estimate_return$cal_corners_trim_optim

		# SAVE CALIBRATION LIST
		if(unify_view_pairs && estimate_cal_coeffs){
			if(estimate_cal_coeffs) list2XML4R(list('calibration' = cal.list), file=cal.file)
		}
	}

	# UNIFY CALIBRATION COORDINATE SETS
	if(unify_view_pairs){ #&& estimate_cal_coeffs

		# CREATE NEW CORNER ARRAY WITH ALL VIEWS
		cal_corners_trim_optim <- cal_corners[, , aspect_non_na >=2, ]
		cal_corners_trim_optim <- cal_corners_trim_optim[, , round(seq(from=1, to=dim(cal_corners_trim_optim)[3], length=max.sample.optim)), ]

		# CREATE FOLDERS IF NOT PRESENT ALREADY
		cal_sub_dig_dir <- paste0(calib_dir, 'Calibration unification points')
		if(!file.exists(cal_sub_dig_dir)) dir.create(cal_sub_dig_dir)
		for(i in 1:length(img_sub_dir)) if(!file.exists(paste0(cal_sub_dig_dir, '/', img_sub_dir[i]))) dir.create(paste0(cal_sub_dig_dir, '/', img_sub_dir[i]))

		#cal_uni_ref_pts_names <- 
		ref_shapes <- c('triangle', 'circle', 'square', 'pentagon')
		cal_uni_ref_pts_names <- c(ref_shapes, paste0('double_', ref_shapes), paste0('paper_corner', 1:4), paste0('Ref_point_', 1:4))

		digitize_ref_points <- TRUE
		if(length(list.files(paste0(cal_sub_dig_dir, '/', img_sub_dir[1]))) > 0){
			if(print.progress) cat("\n\t\tSaved calibration unification reference points detected.\n\n")
			response <- readline(prompt="\t\tDo you wish to re-digitize or edit the calibration unification reference points? (y/n) : ");cat('\n')
			if(tolower(response) %in% c('no', 'n')) digitize_ref_points <- FALSE
		}

		if(digitize_ref_points){

			# DIGITIZE REFERENCE POINTS
			digitizeImages(image.file=verify.dir, shapes.file=cal_sub_dig_dir,
				landmarks.ref=cal_uni_ref_pts_names)
				#app.dir='/Applications/XAMPP/xamppfiles/htdocs/data_analysis/r_package_development/StereoMorph/inst/extdata/apps/digitizeImages'
		}

		# FIND FILES AVAILABLE FOR ALL THREE VIEWS
		cal_uni_ref_dirs <- list.files(cal_sub_dig_dir)
		cal_uni_ref <- list.files(paste0(cal_sub_dig_dir, '/', img_sub_dir[1]))
		for(i in 2:length(img_sub_dir)) cal_uni_ref <- cal_uni_ref[cal_uni_ref %in% list.files(paste0(cal_sub_dig_dir, '/', img_sub_dir[i]))]
		
		# CREATE EMPTY REFERENCE POINT MATRICES
		cal_uni_ref1 <- cal_uni_ref2 <- matrix(NA, nrow=0, ncol=3)
		
		# FILL MATRICES
		for(i in 1:length(cal_uni_ref)){

			# CREATE ARRAY BY FRAME
			cal_uni_ref_frame <- array(NA, dim=c(length(cal_uni_ref_pts_names), 2, length(img_sub_dir)),
				dimnames=list(cal_uni_ref_pts_names, NULL, img_sub_dir))

			for(j in 1:length(img_sub_dir)){
				lm <- readShapes(paste0(cal_sub_dig_dir, '/', img_sub_dir[j], '/', cal_uni_ref[i]))$landmarks.pixel
				cal_uni_ref_frame[rownames(lm), , j] <- lm
			}
			
			# REMOVE LANDMARKS WITH NA IN ANY VIEW
			cal_uni_ref_frame <- cal_uni_ref_frame[rowSums(is.na(cal_uni_ref_frame[, 1, ])) == 0, , ]

			if(length(dim(cal_uni_ref_frame)) == 2) next
			if(0 %in% dim(cal_uni_ref_frame)) next

			# REMOVE ROW NAMES
			rownames(cal_uni_ref_frame) <- paste0(rownames(cal_uni_ref_frame), '_', gsub('.txt', '', cal_uni_ref[i]))

			# RECONSTRUCT LANDMARKS
			dlt_rec1 <- dltReconstruct(cal.coeff=cal.list$sub1.cal.coeff, coor.2d=cal_uni_ref_frame[, , colnames(cal.list$sub1.cal.coeff)])
			dlt_rec2 <- dltReconstruct(cal.coeff=cal.list$sub2.cal.coeff, coor.2d=cal_uni_ref_frame[, , colnames(cal.list$sub2.cal.coeff)])

			# ADD TO MATRICES
			cal_uni_ref1 <- rbind(cal_uni_ref1, dlt_rec1$coor.3d)
			cal_uni_ref2 <- rbind(cal_uni_ref2, dlt_rec2$coor.3d)
		}
		
		# FIND VIEW TO UNIFY WITH FIRST TWO
		view_uni <- colnames(cal.list$sub2.cal.coeff)[!colnames(cal.list$sub2.cal.coeff) %in% colnames(cal.list$sub1.cal.coeff)]

		# GET SUBSET OF 2D POINTS FOUND IN BOTH VIEWS (OF SECOND SUBSET)
		cal_corners_cal_uni2 <- cal_corners[, , , colnames(cal.list$sub2.cal.coeff)]
		cal_corners_cal_uni2_non_na <- rowSums(apply(!is.na(cal_corners_cal_uni2), c(3, 4), 'sum') > 0)
		cal_corners_cal_uni2 <- cal_corners_cal_uni2[, , cal_corners_cal_uni2_non_na == 2, ]

		# USE DLT COEFFICIENTS TO RECONSTRUCT ALL 2D COORDINATES
		coor_2d_sub2 <- apply(cal_corners_cal_uni2, c(2, 4), matrix, byrow=FALSE)

		# RECONSTRUCT INTO 3D
		coor_3d_sub2 <- dltReconstruct(cal.coeff=cal.list$sub2.cal.coeff, coor.2d=coor_2d_sub2)$coor.3d

		# CREATE ARRAY FOR UNIFICATION
		uni_arr <- array(NA, dim=c(nrow(coor_3d_sub2) + nrow(cal_uni_ref1), 3, 2), 
			dimnames=list(c(rep(NA, nrow(coor_3d_sub2)), rownames(cal_uni_ref1)), NULL, colnames(cal.list$sub2.cal.coeff)))

		# FILL ARRAY
		uni_arr[(nrow(coor_3d_sub2)+1):dim(uni_arr)[1], , 1] <- cal_uni_ref1
		uni_arr[1:nrow(coor_3d_sub2), , 2] <- coor_3d_sub2
		uni_arr[(nrow(coor_3d_sub2)+1):dim(uni_arr)[1], , 2] <- cal_uni_ref2

		# UNIFY
		uni_lan <- unifyLandmarks(uni_arr)

		# SAVE 3D COORDINATES IN NEW COORDINATE SYSTEM
		coor_3d_sub2_uni <- uni_lan$lm.matrix
		
		# SAVE UNIFICATION ERRORS
		cal.list$uni.ref.err <- uni_lan$unify.error[!is.na(uni_lan$unify.error), ]

		if(print.progress){
			cat(paste0('\t\tUnification errors for calibration coordinate reference points (in ', sq.size.units, '):\n'))
			
			##### ADD
			# FIND MAX CHAR NUM TO SET TABS AND ALIGN SECOND COLUMN
			#row_names <- names(cal.list$uni.ref.err)
			#nchar(row_names)
			
			cat(paste0('\t\t\t', paste(names(cal.list$uni.ref.err), cal.list$uni.ref.err, collapse='\n\t\t\t'), '\n'))
		}

		# FIND CALIBRATION COEFFICIENTS USING 2D COORDINATES AND TRANSFORMED 3D COORDINATES FOR VIEW-TO-UNIFY
		dlt_coeff2 <- dltCoefficients(coor_3d_sub2_uni[1:nrow(coor_2d_sub2), ], coor_2d_sub2[, , view_uni])$cal.coeff

		# ADD COLUMN TO CALIBRATION COEFFICIENTS
		cal.list$cal.coeff <- cbind(cal.list$sub1.cal.coeff, dlt_coeff2)
		colnames(cal.list$cal.coeff) <- c(colnames(cal.list$sub1.cal.coeff), view_uni)
	}

	# SAVE CALIBRATION LIST
	if(unify_view_pairs || estimate_cal_coeffs){
		cal_coeff <- cal.list$cal.coeff
		list2XML4R(list('calibration' = cal.list), file=cal.file)
	}

	############################## TEST CALIBRATION ACCURACY #############################

	# DEFAULT NULL
	cal_corners_test <- NULL
	if(is.null(cal_corners_trim_optim)){

		if(num_views == 2){

			# TEST ACCURACY USING ALL ASPECTS
			cal_corners_test <- cal_corners

		}else{

			# TEST ACCURACY USING ALL ASPECTS
			cal_corners_test <- cal_corners[, , aspect_non_na >= 2, ]
		}

		if(print.progress){
			cat(paste0("\n\t\tTesting accuracy using all aspects (including those used in calibration):\n"))
			cat(paste0("\t\t\t", paste(dimnames(cal_corners_test)[[3]], collapse=", "), "\n"))
		}

	}else{
	
		# SET OPTIM SET FOR TESTING ACCURACY
		cal_corners_test <- cal_corners_trim_optim

		if(print.progress) cat("\n\t\tTesting accuracy using optimization set\n")
		cat(paste0("\t\t\t", paste(dimnames(cal_corners_test)[[3]], collapse=", "), "\n"))
	}

	dlt_test <- NULL
	if(!is.null(cal_corners_test)){

		for(i in 1:length(view_combos)){

			# SET SAVE AS FOLDER
			if(length(view_combos) == 1){
				save_to <- paste0(calib_dir, 'Error tests')
			}else{
				save_to <- paste0(calib_dir, 'Error tests/', view_combo_sub_dir[i])
			}

			# FIND NON-NA ASPECTS AMONG VIEWS
			aspects_non_na <- rowSums(is.na(cal_corners_test[1,1, , view_combos[[i]]])) == 0

			# SKIP IF NO OVERLAPPING ASPECTS
			if(sum(aspects_non_na) == 0) next

			# CREATE ERROR PLOTS
			dlt_test <- createErrorPlots(cal.coeff=cal_coeff[, view_combos[[i]]], 
				corners=cal_corners_test[, , aspects_non_na, view_combos[[i]]], nx=nx, 
				sq.size.num=sq.size.num, sq.size.units=sq.size.units, 
				file=save_to)

			if(print.progress){
				if(!unify_view_pairs){
					if(i == length(view_combos)){print(summary(dlt_test, print.tab='\t\t'))}
				}else{
					cat(paste0('\n\t\t', paste(img_sub_dir[view_combos[[i]]], collapse=', ')))
					print(summary(dlt_test, print.tab='\t\t\t'))
				}
			}
		}
	}
	
	if(print.progress) cat('\n')

	rlist <- list(
		cal.coeff = cal_coeff, 
		mean.reconstruct.rmse = cal.list$mean.reconstruct.rmse, 
		coefficient.rmse = cal.list$coefficient.rmse
	)

	if(!is.null(dlt_test)) rlist$test.ipd.error = dlt_test$ipd.error

	class(rlist) <- 'calibrateCameras'

	rlist
}

print.calibrateCameras <- function(x, ...){

	class(x) <- ''

	cat('$cal.coeff\n')
	print(x$cal.coeff)
	cat('\n')

	cat('$mean.reconstruct.rmse\n')
	print(x$mean.reconstruct.rmse)
	cat('\n')

	cat('$coefficient.rmse\n')
	print(x$coefficient.rmse)
	cat('\n')

	if(!is.null(x$test.ipd.error)){
		cat('$test.ipd.error\n')
		cat('[1] ', paste(format(x$test.ipd.error[1:min(length(x$test.ipd.error), 5)]), collapse=' '), ' ... and ', length(x$test.ipd.error)-min(length(x$test.ipd.error), 5), ' more\n', sep='')
		cat('\n')
	}
}


						## NOTE ON FFMPEG VS OPENCV IN EXTRACTING FRAMES - CORRESPONDENCE DEPENDS ON VIDEO FORMAT
						# For .mov (from GoPro)
						# 	The first ffmpeg frame (0 after -ss) is not the same as the first frame using the opencv 
						#	frame extraction. When given 0 as index, opencv throws an error. The first opencv frame (at index 1)
						#	corresponds to the second ffmpeg frame. So the "first" frame of these videos seems 
						#	inaccessible using opencv.
						# For .avi files (from canon camera)
						#	For this video opencv does allow a 0 index. The 0 and 1 index opencv frames are identical.
						#	They appear to be the second frame of the video. The ffmpeg 0 frame appears to be the first
						#	frame of the video.
						# For .avi files from the photron cameras (high-speed)
						#	It seems ffmpeg cannot read these files. 0 index after -ss gives a black frame. This may have something 
						#	to do with these videos being 'rawvideo' rather than mpeg. opencv gives the same frame for 0 and 1 as index.
						#command <- paste0('./', gsub(' ', '\\\\ ', '../build/'), 'extract_video_frames ', 
						#	gsub(' ', '\\\\ ', img.dir), '/', gsub(' ', '\\\\ ', vid_fnames[i]), ' ', gsub(' ', '\\\\ ', verify.dir), '/',
						#	gsub(' ', '\\\\ ', img_sub_dir[i]), "/", ' ', frame_seq[j]+1, ' ', frame_seq[j]+1)
