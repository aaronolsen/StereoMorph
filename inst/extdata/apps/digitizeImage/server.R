# SET CONSTANTS
landmarks_file <- paste0(init_params$prev_wd, '/', init_params$landmarks_file)
control_points_file <- paste0(init_params$prev_wd, '/', init_params$control_points_file)
curve_points_file <- paste0(init_params$prev_wd, '/', init_params$curve_points_file)

shinyServer(function(input, output) {

	output$text_output <- renderText({

		update_status <- ''

		# PARSE JSON
		json_list <- fromJSON(input$text_input)

		# INPUT IS FROM BROWSER
		if(!is.null(json_list$fromBrowser)){

			# DETECT LANDMARK SUBMISSION
			if(!is.null(json_list$submit_landmarks)){
				landmarks <- matrix(cbind(sapply(json_list$landmarks, "[[", 2), sapply(json_list$landmarks, "[[", 3)), 
					nrow=length(json_list$landmarks), ncol=2,
					dimnames=list(sapply(json_list$landmarks, "[[", 1), NULL))

				# SAVE LANDMARKS TO FILE
				write.table(x=landmarks, file=landmarks_file, quote=FALSE, sep="\t", col.names=FALSE)

				# NOTIFICATION IN R CONSOLE
				if(nrow(landmarks) == 1){plural <- ''}else{plural <- 's'}
				cat(paste0(nrow(landmarks), " landmark", plural, " saved to '", landmarks_file, "'\n"))
				
				update_status <- paste0(update_status, nrow(landmarks), " landmark", plural, " saved. ")
			}

			# DETECT CURVE SUBMISSION
			if(!is.null(json_list$submit_curves)){
				
				curve_save_status <- '(control points only)'
				
				# SAVE CURVE CONTROL POINTS
				curve_string <- ''
				if(length(json_list$control_points) > 0){
					for(i in 1:length(json_list$control_points)){
						curve_string <- paste0(curve_string, paste(json_list$control_points[[i]], collapse='\t'))
						if(i < length(json_list$control_points)) curve_string <- paste0(curve_string, '\n')
					}
				}

				write(curve_string, control_points_file)
				
				# SAVE CURVE POINTS (IF FILEPATH GIVEN)
				if(!is.null(init_params$curve_points_file)){
					
					curve_points <- matrix(NA, nrow=0, ncol=2)
					num_curve_sets <- 0

					if(length(json_list$control_points) > 0){
						for(i in 1:length(json_list$control_points)){
							m <- matrix(as.numeric(json_list$control_points[[i]][2:length(json_list$control_points[[i]])]), nrow=(length(json_list$control_points[[i]])-1)/2, ncol=2, byrow=TRUE)

							if(nrow(m) <= 2) next

							points_on_bezier <- pointsOnBezier(p=m, method='adjoining', deg=2)
							rownames(points_on_bezier$points) <- paste0(
								json_list$control_points[[i]][1], 
								formatC(1:nrow(points_on_bezier$points), width=4, format="d", flag="0"))

							# CIRCUMVENT BUG IN POINTSONBEZIER WHERE LAST POINT OVERSHOOTS BY ONE
							# IF SECOND TO LAST POINT IS THE SAME AS THE LAST POINT OF M, GET RID OF LAST POINT
							if(sum(abs(points_on_bezier$points[nrow(points_on_bezier$points)-1, ] - m[nrow(m), ])) == 0)
								points_on_bezier$points <- points_on_bezier$points[-nrow(points_on_bezier$points), ]

							curve_points <- rbind(points_on_bezier$points, curve_points)
							num_curve_sets <- num_curve_sets + 1
						}
					}

					write.table(curve_points, file = curve_points_file, quote=F, sep="\t", col.names=F, row.names=T)

					curve_save_status <- '(control and curve points)'

					# NOTIFICATION IN R CONSOLE
					if(length(num_curve_sets) == 1){plural <- ''}else{plural <- 's'}
					cat(paste0(num_curve_sets, " set", plural, " of curve point", plural, " saved to '", control_points_file, "'\n"))
				}				

				# NOTIFICATION IN R CONSOLE
				if(length(json_list$control_points) == 1){plural <- ''}else{plural <- 's'}
				cat(paste0(length(json_list$control_points), " set", plural, " of control points saved to '", control_points_file, "'\n"))

				update_status <- paste0(update_status, length(json_list$control_points), " curve", plural, " saved ", curve_save_status, ". ")
			}

			# DETECT IMAGE CHANGE
			if(!is.null(json_list$change_image)){
				if(json_list$change_image == 1) stopApp('next')
				if(json_list$change_image == -1) stopApp('prev')
			}

			# DETECT EXIT
			if(!is.null(json_list$exit)) stopApp('exit')

		# INITIAL INPUT FROM R
		}

		update_status
	})
})
