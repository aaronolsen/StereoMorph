writeLMToTPS <- function(shapes.file, tps.file, in.pixels = TRUE, flip.y = TRUE, flip.x = FALSE){

	# Read shapes file
	shapes <- readShapes(shapes.file)

	landmarks <- NULL

	# Get landmarks
	if(in.pixels && !is.null(shapes$landmarks.pixel)) landmarks <- shapes$landmarks.pixel
	if(!in.pixels && !is.null(shapes$landmarks.scaled)) landmarks <- shapes$landmarks.scaled

	# If 3D landmarks present, overwrite
	if(!is.null(shapes$landmarks)) landmarks <- shapes$landmarks
	
	# Error if no landmarks found
	if(is.null(landmarks)) stop(paste0("Landmark coordinates not found in input shape file(s)."))
	
	# If matrix
	if(length(dim(landmarks)) == 2){
	
		# Get filename
		file_path <- gsub('[.][A-Za-z]+$', '', strsplit(shapes.file, '/')[[1]])

		# Make sure landmarks are in alphabetical order
		landmarks <- landmarks[sort(rownames(landmarks)), ]

		# Convert to array
		landmarks <- array(landmarks, dim=c(dim(landmarks), 1), dimnames=list(rownames(landmarks), colnames(landmarks), tail(file_path, 1)))
	}else{

		# Make sure landmarks are in alphabetical order
		landmarks <- landmarks[sort(dimnames(landmarks)[[1]]), , ]
	}

	# Get scaling
	scaling <- NULL
	if(in.pixels) scaling <- shapes$scaling

	# Set numbers
	lm_n <- dim(landmarks)[1]
	sp_n <- dim(landmarks)[3]

	if(dim(landmarks)[2] == 2){
		for(sp_n in 1:dim(landmarks)[3]){

			# If 2D and flip.y, flip vertically
			if(flip.y && in.pixels) landmarks[, 2, sp_n] <- -landmarks[, 2, sp_n]

			# If 2D and flip.x, flip horizontally
			if(flip.x) landmarks[, 1, sp_n] <- -landmarks[, 1, sp_n]

			# Shift so that there are no negative coordinates (not sure if TPS allows negative coordinates?)
			if(sum(landmarks[, 1, sp_n] < 0, na.rm=TRUE) > 0) landmarks[, 1, sp_n] <- landmarks[, 1, sp_n] - min(landmarks[, 1, sp_n], na.rm=TRUE)
			if(sum(landmarks[, 2, sp_n] < 0, na.rm=TRUE) > 0) landmarks[, 2, sp_n] <- landmarks[, 2, sp_n] - min(landmarks[, 2, sp_n], na.rm=TRUE)
		}
	}

	#plot(landmarks[, , 1], asp=1)
	#text(landmarks[, , 1], labels=rownames(landmarks[, , 1]))

	# Set number of lines
	lines_per <- lm_n+3+(!is.null(scaling))
	num_lines <- lines_per*sp_n

	# Write lines
	rout <- rep('', num_lines)
	
	# Add landmark count
	LM <- 'LM'
	if(dim(landmarks)[2] == 3) LM <- paste0(LM, '3')
	rout[seq(1, num_lines, by=lines_per)] <- paste0(LM, '=', lm_n)
	
	# Add scaling
	if(!is.null(scaling)) rout[seq(lines_per-2, num_lines, by=lines_per)] <- paste0('SCALE=', scaling)

	# Add specimen IDs
	rout[seq(lines_per-1, num_lines, by=lines_per)] <- paste0('ID=', dimnames(landmarks)[[3]])

	for(sp_n in 1:dim(landmarks)[3]){
	
		ii <- (lines_per*(sp_n-1)+2)

		rout[ii:(ii + lm_n - 1)] <- apply(landmarks[, , sp_n], 1, 'paste', collapse=' ')

	}
	rout <- paste(rout, collapse='\n')

	#cat(rout, sep='')

	writeLines(rout, tps.file)
}