digitizeImage <- function(image.file, landmarks.file=NULL, control.points.file=NULL, curve.points.file=NULL, landmarks.ref=NULL, 
	curves.ref=NULL, auto.advance=TRUE, landmark.color.blur = 'blue', landmark.color.focus = 'green', curve.color.blur = 'purple', 
	control.point.color.blur = 'purple', control.point.color.focus = 'red', landmark.radius = 4, control.point.radius = 4, marker.stroke.width = 1){

	if(is.null(landmarks.file) && is.null(control.points.file)) 
		cat("Warning: 'landmarks.file' and 'control.points.file' are both NULL. If you would like to save landmarks or curves, please specify a location to save either the landmarks and/or curves.\n")

	# GET STEREOMORPH SHINY APP DIRECTORY
	app_dir <- paste0(path.package("StereoMorph"), '/extdata/apps/digitizeImage')
	#app_dir  <- "/Applications/XAMPP/xamppfiles/htdocs/data_analysis/r_package_development/StereoMorph/inst/extdata/apps/digitizeImage"

	# REMOVE ANY IMAGE FILES IN IMG FOLDER
	if(length(list.files(paste0(app_dir, '/www/img/'))) > 0)
		file.remove(paste0(app_dir, '/www/img/', list.files(paste0(app_dir, '/www/img/'))))

	# IF image.file IS DIRECTORY, ADD ALL IMAGE FILES FROM DIRECTORY
	if(length(list.files(image.file) > 0)){
		is_image <- grepl(pattern='.jpg$|.jpeg$|.tiff$|.png$|.raw$|.gif$|.bmp$', x=list.files(image.file), ignore.case=TRUE)
		image.file <- paste0(image.file, '/', list.files(image.file)[is_image])
	}

	# GET IMAGE NAMES
	img_file_split <- strsplit(image.file, '/')
	img_names <- rep(NA, length(image.file))
	for(i in 1:length(img_file_split)) img_names[i] <- img_file_split[[i]][length(img_file_split[[i]])]

	# IF ANY OF THE OUTPUT PATHS ARE DIRECTORIES, MAKE NAMES SAME AS IMAGE NAMES
	if(sum(grepl('.txt$', landmarks.file)) != length(landmarks.file)) landmarks.file <- paste0(landmarks.file, '/', gsub('.[a-zA-Z]+$', '.txt', img_names))
	if(sum(grepl('.txt$', control.points.file)) != length(control.points.file)) control.points.file <- paste0(control.points.file, '/', gsub('.[a-zA-Z]+$', '.txt', img_names))
	if(sum(grepl('.txt$', curve.points.file)) != length(curve.points.file)) curve.points.file <- paste0(curve.points.file, '/', gsub('.[a-zA-Z]+$', '.txt', img_names))

#	if(!is.null(landmarks.file) && length(list.files(landmarks.file) > 0)) landmarks.file <- paste0(landmarks.file, '/', gsub('.[a-zA-Z]+$', '.txt', img_names))
#	if(!is.null(control.points.file) && length(list.files(control.points.file) > 0)) control.points.file <- paste0(control.points.file, '/', gsub('.[a-zA-Z]+$', '.txt', img_names))
#	if(!is.null(curve.points.file) && length(list.files(curve.points.file) > 0)) curve.points.file <- paste0(curve.points.file, '/', gsub('.[a-zA-Z]+$', '.txt', img_names))

	# CHECK THAT NUMBER OF IMAGES MATCHES OUTPUT PATHS IF NOT NULL
	if(!is.null(landmarks.file) && length(image.file) != length(landmarks.file))
		stop(paste0("The number of image file paths input (", length(image.file), ") does not match the number of landmark file paths input (", length(landmarks.file), ")."))
	if(!is.null(control.points.file) && length(image.file) != length(control.points.file))
		stop(paste0("The number of image file paths input (", length(image.file), ") does not match the number of control point file paths input (", length(control.points.file), ")."))
	if(!is.null(curve.points.file) && length(image.file) != length(curve.points.file))
		stop(paste0("The number of image file paths input (", length(image.file), ") does not match the number of curve point file paths input (", length(curve.points.file), ")."))

	# READ IN LANDMARKS
	landmarks_ref <- NULL
	if(!is.null(landmarks.ref)){
		
		# SINGLE ELEMENT IN VECTOR AND EXISTS AS A FILE
		if(length(landmarks.ref) == 1) if(file.exists(landmarks.ref)) landmarks.ref <- as.vector(read.table(landmarks.ref, sep="\n")[,1])

		landmarks_ref <- landmarks.ref
	}

	# READ IN CURVE REF
	curves_ref <- list()
	landmarks_from_curves_ref <- rep(NA, 0)
	if(!is.null(curves.ref)){

		# NOT A MATRIX - ASSUME TO BE FILE PATH
		if(!is.matrix(curves.ref)) curves.ref <- as.matrix(read.table(curves.ref, sep="\t"))

		colnames(curves.ref) <- NULL
		for(i in 1:nrow(curves.ref)){
			curves_ref[[i]] <- c(curves.ref[i, ])
			landmarks_from_curves_ref <- c(landmarks_from_curves_ref, curves.ref[i, 2:3])
		}
	}

	# ADD LANDMARKS FROM CURVES
	if(!is.null(landmarks.file)){
		landmarks_from_curves_ref <- unique(landmarks_from_curves_ref)
		landmarks_from_curves_ref <- landmarks_from_curves_ref[!landmarks_from_curves_ref %in% landmarks_ref]
		landmarks_ref <- c(landmarks_ref, landmarks_from_curves_ref)
	}

	img_num <- 1
	while(img_num <= length(image.file)){

		# SET INITIAL PARAMETERS
		init_params <- list()
		init_params$app_dir <- app_dir
		init_params$prev_wd <- getwd()
		init_params$img_name <- gsub(" ", "_", img_names[img_num])
		init_params$img_size <- file.info(image.file[img_num])$size
		init_params$auto_advance <- auto.advance
		init_params$img_file <- image.file[img_num]
		if(!is.null(landmarks.file)) 
			init_params$landmarks_file <- landmarks.file[img_num]
		if(!is.null(control.points.file)) 
			init_params$control_points_file <- control.points.file[img_num]
		if(!is.null(curve.points.file)) 
			init_params$curve_points_file <- curve.points.file[img_num]

		init_params$landmark_color_blur <- landmark.color.blur
		init_params$landmark_color_focus <- landmark.color.focus
		init_params$curve_color_blur <- curve.color.blur
		init_params$control_point_color_blur <- control.point.color.blur
		init_params$control_point_color_focus <- control.point.color.focus
		init_params$landmark_radius <- landmark.radius
		init_params$control_point_radius <- control.point.radius
		init_params$marker_stroke_width <- marker.stroke.width
		init_params$landmarks_ref <- landmarks_ref
		init_params$curves_ref <- curves_ref

		# SET WHETHER PREVIOUS OR CURRENT IMAGES EXIST
		init_params$prev_img <- FALSE
		init_params$next_img <- FALSE
		if(img_num < length(image.file)) init_params$next_img <- TRUE
		if(img_num > 1) init_params$prev_img <- TRUE

		# COPY IMAGE TO WWW FOLDER
		file.copy(image.file[img_num], paste0(app_dir, '/www/img/', gsub(" ", "_", img_names[img_num])))

		# READ IN CURRENT LANDMARKS
		init_params$landmarks <- list()
		if(!is.null(landmarks.file) && file.exists(landmarks.file[img_num]) && file.info(landmarks.file[img_num])$size > 1){
			landmarks <- as.matrix(read.table(landmarks.file[img_num], row.names=1, sep="\t"))
			colnames(landmarks) <- NULL
			for(i in 1:nrow(landmarks)) init_params$landmarks[[i]] <- c(rownames(landmarks)[i], landmarks[i, ])
		}

		# READ IN CURRENT CONTROL POINTS	
		init_params$control_points <- list()
		if(!is.null(control.points.file) && file.exists(control.points.file[img_num]) && file.info(control.points.file[img_num])$size > 1){
			control_points <- readBezierControlPoints(control.points.file[img_num])
			for(i in 1:length(control_points)){
				init_params$control_points[[i]] <- c(names(control_points)[i], c(t(control_points[[names(control_points)[i]]][[1]])))
			}
		}

		# CONVERT PARAMETERS INTO STRING FOR R TO READ
		param_str <- 'init_params <- list()\n'
		for(param_name in names(init_params)){

			if(!is.list(init_params[[param_name]])){
				param_str <- paste0(param_str, paste0('init_params[[\'', param_name, '\']]',  ' <- ', 'c(\'', 
					paste0(init_params[[param_name]], collapse='\',\''), '\')', sep='\n'))
			}else{
				
				if(length(init_params[[param_name]]) == 0) next
				for(i in 1:length(init_params[[param_name]])){
					param_str <- paste0(param_str, paste0('init_params[[\'', param_name, '\']][[', i, ']]',  ' <- ', 'c(\'', 
						paste0(init_params[[param_name]][[i]], collapse='\',\''), '\')', sep='\n'))
				}
			}

			param_str <- paste0(param_str, '\n')
		}

		# CONVERT PARAMETERS INTO STRING FOR JAVASCRIPT TO READ
		json_str <- '{'
		for(param_name in names(init_params)){

			if(!is.list(init_params[[param_name]])){
				json_str <- paste0(json_str, paste0('\"', param_name, '\":[\"', paste0(init_params[[param_name]], collapse='\", \"'), '\"]', sep=', '))
			}else{
				if(length(init_params[[param_name]]) == 0) next
				json_str <- paste0(json_str, paste0('\"', param_name, '\":['))
				for(i in 1:length(init_params[[param_name]])){
					json_str <- paste0(json_str, paste0('[\"', paste0(init_params[[param_name]][[i]], collapse='\", \"'), '\"]', sep=', '))
				}			
				json_str <- paste0(json_str, '],')
			}
		}
		json_str <- paste0(json_str, "}")
		json_str <- gsub("(,[ ]*)(]|})", "\\2", json_str)
		json_str <- paste0("json_string <- '", json_str, "'")

		# WRITE PARAMETERS TO FILE IN ORDER TO PASS THEM TO SHINY APP
		write(x = paste0(param_str, '\n', json_str), file = paste0(app_dir, "/initial_parameters.R"))

		# COPY HTML FILE WITHOUT IMAGE TAG
		file.copy(paste0(app_dir, "/digitize_image_pre.html"), paste0(app_dir, "/digitize_image.html"), overwrite=TRUE)

		# ADD IMAGE TAG TO HTML DOCUMENT (TO LOAD IMAGE AND GET SIZE)
		img_tag <- paste0('\n<img style="display:none;" id="img1" src="img/', gsub(" ", "_", img_names[img_num]),'" ></img>')
		write(img_tag, file=paste0(app_dir, "/digitize_image.html"), append=TRUE)

		# NOTIFICATION IN R CONSOLE
		cat(paste0("Loading image '", img_names[img_num], "'\n"))

		# INITIATE SHINY APP
		#run_app <- runApp(app_dir, port = NULL, host = "127.0.0.1", launch.browser = TRUE, display.mode = "auto")
		run_app <- runApp(app_dir)
		#run_app <- 'exit'
		
		# REMOVE ANY IMAGE FILES IN IMG FOLDER
		if(length(list.files(paste0(app_dir, '/www/img/'))) > 0)
			file.remove(paste0(app_dir, '/www/img/', list.files(paste0(app_dir, '/www/img/'))))

		# DETERMINE WHETHER TO CHANGE IMAGE OR QUIT
		if(run_app == 'next'){
			img_num <- img_num + 1
		}else if (run_app == 'prev'){
			img_num <- img_num - 1
		}else{			
			return(NULL)
		}
		
		# MAKE SURE IMAGE NUMBER IS SENSIBLE
		if(img_num < 1) img_num <- 1
		if(img_num > length(image.file)) img_num <- length(image.file)
	}
}