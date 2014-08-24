drawCheckerboard <- function(nx, ny, square.size, filename, margin.x = c(round(square.size/2), round(square.size/2)), margin.y = c(round(square.size/2), round(square.size/2)), ...){

	# REQUIRES GRID PACKAGE

	# GET X AND Y COORDINATES FOR SQUARES
	x <- c(0, 0, square.size, square.size)
	y <- c(0, square.size, square.size, 0)
	
	# GET FILE EXTENSION FROM FILENAME
	image_type <- tolower(substr(filename, nchar(filename)-attributes(regexpr(pattern='[A-Za-z]+$', text=filename))$match.length+1, nchar(filename)))
	
	# CHANGE JPG TO JPEG TO MATCH FUNCTION CALL
	if(image_type == 'jpg') image_type <- 'jpeg'

	# CALL CORRESPONDING IMAGE FUNCTION TO START IMAGE WRITING
	do.call(image_type, list(filename=filename, width=sum(margin.x) + (nx+1)*square.size, height=sum(margin.y) + (ny+1)*square.size, ...))

	# WRITE CHECKERBOARD SQUARES TO IMAGE
	grid.newpage()
	for(i in seq(0, nx, by=2)){
		for(j in seq(0, ny, by=1)){

			# IF nx IS EVEN, i IS LAST NUMBER AND j IS ODD CONTINUE - THIS IS AN EXTRA ROW THAT SHOULD BE SKIPPED
			if(i == nx && j %% 2 == 1) next
			
			# DRAW BLACK SQUARE
			grid.polygon(x=margin.x[1]+x+square.size*i + square.size*(j %% 2), y=margin.y[1]+y+square.size*j, gp=gpar(fill=1, lwd=0), default.units="native")
		}
	}

	# CLOSE IMAGE CONNECTION
	dev.off();
}