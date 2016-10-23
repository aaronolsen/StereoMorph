TPSToShapes <- function(tps.file, shapes.file, image.file, landmark.names, 
	spec.names = c("IMAGE"), scaling.units = NULL, flip.y = TRUE){

	# Read TPS file
	read_tps <- readTPS(file=tps.file, landmark.names=landmark.names, spec.names=spec.names, 
		scaling.units=scaling.units)

	# Set shapes filepaths
	if(grepl('[.]txt$', shapes.file[1], ignore.case=TRUE)){
	
		## Filename input
		# Check that number of input filenames matches the number of specimens
		if(dim(read_tps$landmarks.pixel)[3] != length(shapes.file))
			stop(paste0("The number of input filenames for the shapes files (", length(shapes.file), ") does not match the number of specimens in the TPS file (", dim(read_tps$landmarks.pixel)[3], ")."))
		
		# Set filepaths
		shapes_saveas <- shapes.file

	}else{

		## Directory input
		# Make sure there are specimen names for the shapes filenames
		if(is.null(dimnames(read_tps$landmarks.pixel)[[3]])) stop("No specimen names were found in the TPS file. Specimen names are required to name the shape files.")

		# Make sure directory exists
		if(!file.exists(shapes.file)) stop(paste0("Folder '", shapes.file, "' where shapes files are to be saved was not found."))

		# Set filepaths
		shapes_saveas <- paste0(shapes.file, "/", dimnames(read_tps$landmarks.pixel)[[3]], '.txt')
	}

	# Get corresponding image filepaths
	if(grepl('[.](jpg|jpeg|bmp|png|gif|tiff|tif)$', image.file[1], ignore.case=TRUE)){
		image_fpaths <- image.file
	}else{

		# Make sure directory exists
		if(!file.exists(image.file)) stop(paste0("Folder '", image.file, "' containing the images was not found."))
		
		# Read directory contents
		image_fpaths <- paste0(image.file, "/", list.files(image.file))
		image_names <- gsub('[.](jpg|jpeg|bmp|png|gif|tiff|tif)$', '', list.files(image.file), ignore.case=TRUE)
		names(image_fpaths) <- image_names

		# Find overlapping images
		shapes_in_images <- dimnames(read_tps$landmarks.pixel)[[3]] %in% image_names

		# Check that for every specimen there is a corresponding image
		if(sum(!shapes_in_images) > 0)
			stop(paste0("Images corresponding to the following specimens were not found: ", paste(dimnames(read_tps$landmarks.pixel)[[3]][!dimnames(read_tps$landmarks.pixel)[[3]] %in% image_names], collapse=", ")))

		# Limit to images corresponding to specimen and make sure images in same order as specimens
		image_fpaths <- image_fpaths[dimnames(read_tps$landmarks.pixel)[[3]]]
	}

	# Write shape files
	for(spec_num in 1:dim(read_tps$landmarks.pixel)[3]){

		# Get shapes
		shapes_list <- list(
			'scaling'=read_tps$scaling[spec_num],
			'scaling.units'=read_tps$scaling.units[spec_num],
			'landmarks.pixel'=read_tps$landmarks.pixel[, , spec_num],
			'landmarks.scaled'=read_tps$landmarks.scaled[, , spec_num]
		)
		
		# Flip y-coordinates
		if(flip.y){

			# Get image dimensions
			if(grepl(pattern='[.]jpg$|[.]jpeg$', x=image_fpaths[spec_num], ignore.case=TRUE)) img_dim <- dim(readJPEG(image_fpaths[spec_num], native=TRUE))
			if(grepl(pattern='[.]png$', x=image_fpaths[spec_num], ignore.case=TRUE)) img_dim <- dim(readPNG(image_fpaths[spec_num], native=TRUE))
			if(grepl(pattern='[.]tif$|[.]tiff$', x=image_fpaths[spec_num], ignore.case=TRUE)) img_dim <- dim(readTIFF(image_fpaths[spec_num], native=TRUE))
			
			shapes_list$landmarks.pixel[, 2] <- img_dim[1] - shapes_list$landmarks.pixel[, 2]
			shapes_list$landmarks.scaled <- shapes_list$landmarks.pixel*shapes_list$scaling
			shapes_list$landmarks.scaled[, 2] <- -shapes_list$landmarks.scaled[, 2]
		}
		
		if(!is.na(shapes_list$scaling)){
			shapes_list$ruler.interval = 1
			shapes_list$ruler.pixel = 1/shapes_list$scaling
		}

		list2XML4R(list('shapes'=shapes_list), file=shapes_saveas[spec_num])
	}
	
	NULL
}