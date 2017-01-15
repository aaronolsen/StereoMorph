extractFrames <- function(file, save.to = NULL, frames = NULL, name = '', ext = 'jpeg', 
	qscale = 2, frame.start = 0, video.i = NULL, warn.min = 100){

	# Set frame start point (first change of frame)
	# 	This assumes first frame change occurs between 0.000 and 0.001. This may 
	#	not be the case with all videos. Seems to be true for most synched GoPro
	#	videos (haven't checked them all). A canon .avi file started between 0.016 and 0.017. 
	#	The second frame change occurred between 0.049 and 0.050 (predictably).
	#	May need to add an additional step that detects the first time point at which the frame changes

	# Check that file exists
	if(!file.exists(file)) stop(paste0("File '", file, "' not found."))

	if(!is.null(save.to)){
		# Check that save.to exists
		if(!file.exists(save.to)) stop(paste0("save.to filepath '", save.to, "' not found."))

		# Add slash at end if not present
		if(!grepl('[/]$', save.to)) save.to <- paste0(save.to, '/')
	}

	if(is.null(video.i)){

		# Check that ffmpeg is installed and accessible on system
		check_system_command_SM('ffmpeg')

		# Get ffmpeg info
		ffmpeg_i <- paste(suppressWarnings(system2(command='ffmpeg', args=paste0("-i ", gsub(' ', '\\\\ ', file)), 
			stdout=FALSE, stderr=TRUE)), collapse='\n')
	
		# Read video info
		video_i <- read_video_info(ffmpeg_i)

	}else{

		video_i <- video.i
	}

	# Print number of frames
	if(is.null(frames)) cat(paste0('\n\tTotal number of frames in the video: ', video_i$frames, '\n'))

	frames_check <- FALSE

	# Check input frame range is ok
	while(!frames_check){

		frames_check <- TRUE
		if(!is.null(frames)){
			if(sum(!is.numeric(frames)) > 0){cat('\n\tFrame numbers must be of type numeric.');frames_check <- FALSE;frames <- NULL}
			if(sum(frames < 0) > 0){cat('\n\tFrame numbers must be greater than -1.');frames_check <- FALSE;frames <- NULL}
			if(sum(frames > video_i$frames-1) > 0){
				cat(paste0('\n\tFrame numbers must be less than the total number of video frames minus one (', video_i$frames, '-1=', video_i$frames-1, ').'))
				frames_check <- FALSE
				frames <- NULL
			}
		}
		if(is.null(frames)){

			# Prompt for frames to extract
			frames_response <- readline(prompt=paste0("\n\tWhich frames from 0 to ", video_i$frames-1, " would you like to extract? (Enter 'e' for example inputs) "))
		
			# Provide examples if asked
			if(frames_response %in% c('e', 'E', 'examples', 'Examples', 'EXAMPLES')){
				examples <- paste0("\n\t\tseq(0,", video_i$frames-1, ",length=", min(video_i$frames, 100), ")")
				examples <- paste0(examples, "\n\t\tc(0,1,2)")
				examples <- paste0(examples, "\n\t\t0:", min(video_i$frames, 100), "")
				frames_response <- readline(prompt=paste0("\tExample inputs:", examples, "\n\tWhich frames would you like to extract? "))
			}

			frames <- eval(parse(text=frames_response))
			frames_check <- FALSE
		}

		if(!is.null(warn.min) && frames_check){
			if(length(frames) > warn.min){

				cat(paste0("\n\tThe following ", length(frames), " frame(s) will be extracted:\n"))
				cat(paste0("\t\t", paste(frames, collapse=", "), "\n"))

				proceed <- readline(prompt="\tThis may take some time. Proceed (y/n)? ")

				if(!grepl('y', proceed, ignore.case=TRUE)){
					frames_check <- FALSE
					frames <- NULL
				}
			}
		}
	}

	# Format frame numbers
	frame_format <- paste0(name, formatC(frames, width=max(6, nchar(max(frames))+1), format="d", flag="0"))

	# Get time points corresponding to frame boundaries (right before next frame) in sec
	frame_t_bound <- floor((1000 / video_i$fps)*(frames-1))

	# Add half of duration between frames to ensure that extraction falls on next frame
	frame_t <- frame_t_bound + frame.start + round(((1 / video_i$fps)*500))
	
	# Convert to seconds
	frame_t <- frame_t / 1000
	
	# Replace negative frame with 0 (for 0 frame)
	frame_t[frame_t < 0] <- 0

	# Extract frames
	for(j in 1:length(frame_t)){

		# Write command to extract frame using ffmpeg
		command_args <- paste0("-accurate_seek -ss ", frame_t[j], " -i ", gsub(' ', '\\\\ ', file), 
			" -y -qscale:v ", qscale," -frames:v 1 ", gsub(' ', '\\\\ ', save.to), frame_format[j], ".", ext[1])
		#print(paste0('ffmpeg ', command_args))

		# Call command
		system2(command='ffmpeg', args=command_args, stdout=FALSE, stderr=FALSE)
		
		# Extract same frame using opencv (if running use different name for ffmpeg so no overwrite)
		if(frames[j] > 0){
			#command <- paste0('./', gsub(' ', '\\\\ ', '../build/'), 'extract_video_frames ', 
			#	gsub(' ', '\\\\ ', file), ' ', gsub(' ', '\\\\ ', save.to), ' ', frames[j], ' ', frames[j])
			#system(command=command, intern=TRUE)
		}
	}
}