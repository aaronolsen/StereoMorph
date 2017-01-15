check_system_command_SM <- function(commands){
	for(command_name in commands){
		ffmpeg_v <- tryCatch(
			expr={system2(command=command_name, args="-version", stdout=TRUE, stderr=TRUE)},
			error=function(cond) return(NULL),
			warning=function(cond) return(NULL)
		)
		if(is.null(ffmpeg_v)) stop(paste0("The executable '", command_name, "' is not a recognized system command. Please make sure that you have installed the '", command_name, "' executable and that the directory in which it is contained is included in the system $PATH."))
	}
}