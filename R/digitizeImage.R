digitizeImage <- function(image.file, landmarks.file=NULL, control.points.file=NULL, 
	curve.points.file=NULL, shapes.file=NULL, landmarks.ref=NULL, curves.ref=NULL, image.id=NULL, 
	landmark.color.blur = 'blue', landmark.color.focus = 'green', curve.color.blur = 'purple', 
	control.point.color.blur = 'purple', control.point.color.focus = 'red', landmark.radius = 4, 
	control.point.radius = 4, marker.stroke.width = 2, app.dir=NULL){

	# Divert to new function
	digitizeImages(image.file=image.file, shapes.file=shapes.file, 
		landmarks.file=landmarks.file, control.points.file=control.points.file, 
		curve.points.file=curve.points.file, landmarks.ref=landmarks.ref, curves.ref=curves.ref, 
		image.id=image.id, landmark.color.blur=landmark.color.blur, 
		landmark.color.focus=landmark.color.focus, curve.color.blur=curve.color.blur, 
		control.point.color.blur=control.point.color.blur, control.point.color.focus=control.point.color.focus, 
		landmark.radius=landmark.radius, control.point.radius=control.point.radius, 
		marker.stroke.width=marker.stroke.width, app.dir=app.dir)
}