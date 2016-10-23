readTPS <- function(file, landmark.names = NULL, spec.names = c("image", "id", "none"), scaling.units = NULL){

	# Check that TPS file exists
	if(!file.exists(file)) stop(paste0("File '", file, "' not found."))

	# Read TPS file
	read_lines <- readLines(file)
	
	# Find lines with 'LM='
	grepl_lm <- grepl('^lm=', read_lines, ignore.case=TRUE)
	
	# Count number of specimens by counting number of lines with 'LM'
	num_spec <- sum(grepl_lm)
		
	# Count number of landmarks, based on first 'LM' line
	num_land <- as.numeric(gsub('^lm=', '', read_lines[which(grepl_lm)[1]], ignore.case=TRUE))
	
	# Get number of landmark dimensions (2D vs 3D)
	num_dims <- length(strsplit(read_lines[which(grepl_lm)[1]+1], ' ')[[1]])

	# Create an array for landmarks
	lm_arr <- array(NA, dim=c(num_land, num_dims, num_spec))
	
	# Create scaling vector
	scaling <- rep(NA, num_spec)

	# Assign dimension names
	if(!is.null(landmark.names)){
		
		# Check if landmark.names is file or vector of names
		is_file <- TRUE
		if(length(landmark.names) > 1) is_file <- FALSE
		if(!grepl('[.]txt$', landmark.names[1], ignore.case=TRUE)) is_file <- FALSE

		if(is_file){

			# Check that landmark name file exists
			if(!file.exists(file)) stop(paste0("File '", file, "' not found."))

			# Read file contents and replace landmark.names
			landmark.names <- readLines(landmark.names)
			
			# 
			if(sum(grepl('\"|\'', landmark.names)) > 0) stop("Single ('') and double (\"\") quotes are not allowed in landmark names.")
		}
		
		# Remove empty lines
		landmark.names <- landmark.names[landmark.names != '']

		# If not file, check that vector number matches the number of landmarks
		if(length(landmark.names) != num_land) stop(paste0("The number of landmarks in the TPS file (", num_land, ") does not match the number of input landmarks (", length(landmark.names), ")"))
		
		dimnames(lm_arr)[[1]] <- landmark.names
	}

	# Assign specimen names
	if(!grepl("none", spec.names[1], ignore.case=TRUE)){
	
		gfind <- paste0("^", spec.names[1], "=")

		# Find lines with specimen name label
		grepl_snl <- grepl(gfind, read_lines, ignore.case=TRUE)

		# Remove label
		gsub_snl <- gsub(gfind, '', read_lines[which(grepl_snl)], ignore.case=TRUE)

		# Remove file extension if spec.names[1] is IMAGE
		if(grepl("image", spec.names[1], ignore.case=TRUE)) gsub_snl <- gsub('[.](jpg|jpeg|bmp|png|gif|tiff|tif)$', '', gsub_snl, ignore.case=TRUE)
		
		dimnames(lm_arr)[[3]] <- gsub_snl
	}

	# Loop through lines reading each
	jj <- 1
	kk <- 1
	for(ii in 1:length(read_lines)){
		
		if(grepl('^scale=', read_lines[ii], ignore.case=TRUE)) scaling[kk-1] <- as.numeric(gsub('^scale=', '', read_lines[ii], ignore.case=TRUE))

		if(grepl('=', read_lines[ii])){
			if(jj != 1){
				jj <- 1
				kk <- kk + 1
			}
			next
		}
		
		lm_arr[jj, , kk] <- as.numeric(strsplit(read_lines[ii], " ")[[1]])
		
		jj <- jj + 1
	}
	
	# Scale landmarks
	lm_arr_scaled <- lm_arr
	for(spec in 1:num_spec) lm_arr_scaled[, , spec] <- lm_arr[, , spec]*scaling[spec]

	# Repeat scaling units to match the length of scaling
	if(length(scaling.units) < length(scaling)) scaling.units <- rep(scaling.units, length(scaling))
	
	rlist <- list(
		'scaling'=scaling,
		'scaling.units'=scaling.units,
		'landmarks.pixel'=lm_arr,
		'landmarks.scaled'=lm_arr_scaled
	)

	rlist
}