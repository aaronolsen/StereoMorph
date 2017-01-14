read_video_info <- function(ffmpeg_i){

#cat(ffmpeg_i)
#cat('\n')

	# Find duration
	duration_match <- regexpr(pattern='Duration: [0-9:.]+,', text=ffmpeg_i)

	# Get duration
	duration <- substr(ffmpeg_i, start=duration_match+10, stop=duration_match+attr(duration_match, "match.length")-2)
	
	# Split string
	str_split <- as.numeric(strsplit(x=duration, split='[.]|:')[[1]])
	
	# Duration in ms
	duration_ms <- str_split[1]*60*60*1000 + str_split[2]*60*1000 + str_split[3]*1000 + str_split[4]*10
	duration_sec <- duration_ms / 1000

	# Remove all content prior to duration to minimize false matches
	ffmpeg_i <- substr(ffmpeg_i, start=duration_match+attr(duration_match, "match.length"), stop=nchar(ffmpeg_i))

	# Get frames per second
	fps_match <- regexpr(pattern=', [0-9.]+ fps, ', text=ffmpeg_i)

	# Get frames per second
	fps <- as.numeric(substr(ffmpeg_i, start=fps_match+2, stop=fps_match+attr(fps_match, "match.length")-7))

	# Calculate number of frames
	frames <- round(duration_sec * fps)

	# Find video dimensions
	dims_match <- regexpr(pattern=', [0-9]{3,4}x[0-9]{3,4}[ [|,]', text=ffmpeg_i)

	dims_sub <- substr(ffmpeg_i, start=dims_match+2, stop=dims_match+attr(dims_match, "match.length")-1)

	dims <- as.numeric(strsplit(x=gsub('[ ,[]', '', dims_sub), split='x')[[1]])

	rlist <- list(
		'frames'=frames,
		'fps'=fps,
		'duration.ms' = duration_ms,
		'duration.sec' = duration_sec,
		'dims'=dims
	)
	
	rlist
}