var auto_advance = false;
var checkerboard_nx = "NA";
var checkerboard_ny = "NA";
var checker_square_pixel = "NA";
var checker_square_world = "NA";
var click_radius = 7;
var corner_color = 'yellow';
var corner_radius = 2;
var corners = new Array();
var currentEvent = "nothing";
var current_aspect;
var current_image_fname;
var current_landmark = "NA";
var current_curve = "NA";
var current_curve_point = "NA";
var current_ruler = "NA";
var current_save_shapes;
var current_save_landmarks;
var current_save_control_points;
var current_save_curve_points;
var current_selection = new Array();
var current_view;
var curve_also_landmark = new Array();
var curves = new Array();
var disp_order_cp = new Array();
var drag_div_mouse_down;
var end_dupl = new Array();
var epipolar_slope = "NA";
var epipolar_intercept = "NA";
var epipolar_cubic = "NA";
var find_ruler_interval_run_after = "NA";
var frame_color = '#FFF3CE';
var globalkeypress = '';
var IE = document.all ? true : false
var images_fnames = new Array();
var image_load_count = 0;
var initiate = 0;
var initialX;
var initialY;
var input_focus = false;
var js_ct = 0;
var landmark_also_curve = new Array();
var landmarks = new Array();
var last_p_pos;
var last_dblClickEvent = 0;
var margin = 0;
var mouse_move_in_prox = 0;
var mousedown;
var overlay_photograph_id = 0;
var photograph = new Array();
var p_pos;
var ruler_interval_pixels = "NA";
var ruler_interval_world = "NA";
var rulers = new Array();
var scaling_ppw = "NA";
var scaling_units = "NA";
var scaling_wpp = "NA";
var server_out;
var start_dupl = new Array();
var stereo_input = false;
var submit_ct = 0;
var svgns = "http://www.w3.org/2000/svg";
var tempX = 0;
var tempY = 0;
var unsaved_landmarks = false;
var unsaved_curves = false;
var unsaved_ruler_points = false;
var unsaved_corners = false;
var view_names = new Array();
var window_properties = new Array();
var zoom_d = 1;

if (!Array.prototype.indexOf) {
  Array.prototype.indexOf = function (obj, fromIndex) {
    if (fromIndex == null) {
        fromIndex = 0;
    } else if (fromIndex < 0) {
        fromIndex = Math.max(0, this.length + fromIndex);
    }
    for (var i = fromIndex, j = this.length; i < j; i++) {
        if (this[i] === obj)
            return i;
    }
    return -1;
  };
}

function addCurvePoint(num1, num2, x, y){

	var i, j, point_num;

	if(num2 > 0 && num2 < curves[num1].length-1){

		var cp_x_div = document.getElementById('curve_' + num1 + '_' + num2 + '_x');
		var cp_y_div = document.getElementById('curve_' + num1 + '_' + num2 + '_y');

		// Add a new curve point
		if(num2 == curves[num1].length-2) addCurvePointField(num1, curves[num1].length-2);

		cp_x_div.innerHTML = x;
		cp_y_div.innerHTML = y;

		curves[num1][num2].x = x;
		curves[num1][num2].y = y;
		
		drawMarker('curve', num1, num2, x, y);

	}else{

		if(num2 == 0){

			if(curve_also_landmark[num1].start >= 0) document.getElementById('landmark_col_x_' + curve_also_landmark[num1].start).innerHTML = x;
			if(curve_also_landmark[num1].start >= 0) document.getElementById('landmark_col_y_' + curve_also_landmark[num1].start).innerHTML = y;

//				alert(x + ',' + y + ',' + num1 + ',' + num2)

			for(i = 0;i < start_dupl[num1].type.length;i++){

				if(start_dupl[num1].type[i] == 'start'){point_num = 0;}else{point_num = curves[start_dupl[num1].num[i]].length-1;}

				var cp_x_div = document.getElementById('curve_' + start_dupl[num1].num[i] + '_' + point_num + '_x');
				var cp_y_div = document.getElementById('curve_' + start_dupl[num1].num[i] + '_' + point_num + '_y');

				cp_x_div.innerHTML = x;
				cp_y_div.innerHTML = y;
			
				curves[start_dupl[num1].num[i]][point_num].x = x;
				curves[start_dupl[num1].num[i]][point_num].y = y;

				drawMarker('curve', start_dupl[num1].num[i], point_num, x, y);
			}
		}else{

			if(curve_also_landmark[num1].end >= 0) document.getElementById('landmark_col_x_' + curve_also_landmark[num1].end).innerHTML = x;
			if(curve_also_landmark[num1].end >= 0) document.getElementById('landmark_col_y_' + curve_also_landmark[num1].end).innerHTML = y;

			for(i = 0;i < end_dupl[num1].type.length;i++){

				if(end_dupl[num1].type[i] == 'start'){point_num = 0;}else{point_num = curves[end_dupl[num1].num[i]].length-1;}

				var cp_x_div = document.getElementById('curve_' + end_dupl[num1].num[i] + '_' + point_num + '_x');
				var cp_y_div = document.getElementById('curve_' + end_dupl[num1].num[i] + '_' + point_num + '_y');

				cp_x_div.innerHTML = x;
				cp_y_div.innerHTML = y;
			
				curves[end_dupl[num1].num[i]][point_num].x = x;
				curves[end_dupl[num1].num[i]][point_num].y = y;

				drawMarker('curve', end_dupl[num1].num[i], point_num, x, y);
			}
		}
	}
}

function addCurvePointField(curve_num, at){
	
	var i;

	// Reset current curve point
	var reset_current_curve = false

	// Add point on end
	curves[curve_num][curves[curve_num].length] = new Array();

	// Shift all points after 'at'
	for(i = curves[curve_num].length-1; i >= 0; i--){
		if(i > at + 1){

			curves[curve_num][i].x = curves[curve_num][i-1].x;
			curves[curve_num][i].y = curves[curve_num][i-1].y;

			// Change corresponding IDs of shapes
			var circle = svgDocument.getElementById('curve_' + curve_num + '_' + (i-1));
			var line = svgDocument.getElementById('curve_line_' + curve_num + '_' + (i-1));
			if(circle) circle.id = 'curve_' + curve_num + '_' + i;
			if(line) line.id = 'curve_line_' + curve_num + '_' + i;
			
			// Re-index current curve point, if selected
			if(current_curve == curve_num && current_curve_point == (i-1)){
				current_curve_point = i;
				reset_current_curve = true;
			}
		}
	}

	// Enter '-','-' after at
	for(i = 0; i < curves[curve_num].length; i++){
		if(i == (at + 1)){
			curves[curve_num][i].x = '-';
			curves[curve_num][i].y = '-';
			break;
		}
	}

	// Update curve table
	updateCurveTable(curve_num);

	// Re-draw Bezier curve
	drawBezierCurve(curve_num);

	// Because curve points have been re-indexed, select re-indexed curve point
	if(reset_current_curve){
		selectObject('curve', current_curve, current_curve_point, true);
	}

//	alert(printArrayToString(curves[curve_num]));
//	alert(curve_num + ',' + at)
}


function addField(type){

	if(type == 'ruler'){

		var i = rulers.length;

		if(i % 2 == 0){n = 'even';}else{n = 'odd';}

		var row = document.createElement('div');
		row.className = 'scaling_table_row';
		row.id = 'ruler_row_' + i;		
		row.setAttribute("onclick", "selectObject('ruler', " + i + ");");
		document.getElementById('ruler_table_container').appendChild(row);

		var col_name = document.createElement('div');
		col_name.className = 'scaling_table_col1_' + n;
		col_name.id = 'ruler_col_name_' + i;
		col_name.innerHTML = 'Ruler point ' + (i+1);
		document.getElementById('ruler_row_' + i).appendChild(col_name);

		var col_x = document.createElement('div');
		col_x.className = 'scaling_table_col2_' + n;
		col_x.id = 'ruler_col_x_' + i;
		col_x.innerHTML = "-";
		document.getElementById('ruler_row_' + i).appendChild(col_x);

		var col_y = document.createElement('div');
		col_y.className = 'scaling_table_col2_' + n;
		col_y.id = 'ruler_col_y_' + i;
		col_y.innerHTML = "-";
		document.getElementById('ruler_row_' + i).appendChild(col_y);
		
		rulers[i] = new Array();
		rulers[i].x = "-";
		rulers[i].y = "-";

		return;
	}

}

function addLandmark(num, x, y){

	var lm_x_div = document.getElementById('landmark_col_x_' + num);
	var lm_y_div = document.getElementById('landmark_col_y_' + num);

	lm_x_div.innerHTML = x;
	lm_y_div.innerHTML = y;

	landmarks[num].x = x;
	landmarks[num].y = y;

	drawMarker('landmark', num, '', x, y);

	return;
}

function addRulerPoint(num, x, y){

	var new_ruler;

	var ruler_x_div = document.getElementById('ruler_col_x_' + num);
	var ruler_y_div = document.getElementById('ruler_col_y_' + num);

	if(ruler_x_div.innerHTML == "-" && x !== "-"){new_ruler = 1;}else{new_ruler = 0;}
	ruler_x_div.innerHTML = x;
	ruler_y_div.innerHTML = y;

	rulers[num].x = x;
	rulers[num].y = y;
	drawMarker('ruler', num, '', x, y);

	// IF NEW RULER POINT AND LAST IN LAST, ADD NEW FIELD
	if(new_ruler && num == rulers.length-1) addField('ruler');

	return;
}

function addObject(x, y){

	//alert(current_landmark + ', ' + current_ruler + ', ' + current_curve);

	var i, num;

	if(isNaN(x)){
		x = '-';
		y = '-';
	}

	// ADD LANDMARK
	if(current_landmark !== "NA"){

		// Create landmark
		addLandmark(current_landmark, x, y);

		// Select landmark, ensure all styles updated
		selectObject('landmark', current_landmark, '', true, false);

		unsaved_landmarks = true;

		return;
	}

	// ADD RULER POINT
	if(current_ruler !== "NA"){

		addRulerPoint(current_ruler, x, y);

		// FIND RULER INTERVAL ONE SECOND AFTER LAST MOVE OF ANY RULER POINT, turn off for initial load
		find_ruler_interval_run_after = new Date().getTime() + 999;
		setTimeout(function(){findRulerInterval()}, 1000);

		// Select ruler point, ensure all styles updated
		selectObject('ruler', current_ruler, '', true, false);

		unsaved_ruler_points = true;

		return;
	}

	// Add curve point
	if(current_curve !== "NA"){

		addCurvePoint(current_curve, current_curve_point, x, y);

		// Select curve, ensure all styles updated
		selectObject('curve', current_curve, current_curve_point, true, false);

		unsaved_curves = true;

		return;
	}
}

function alignSVGImage(w,h){

	var img_width = image_object.width - 2*margin;
	var img_height = image_object.height - 2*margin;
	var rw = w/img_width;
	var rh = h/img_height;

	if(rw > rh){
		var nw = img_width;
		var nh = Math.round(h / rw);
	}else{
		var nw = Math.round(w / rh);
		var nh = img_height;
	}
	
	return new Array(nw,nh);
}

function baseName(ev) {
	var id = ev.target.getAttribute("id");
	return id;
}

function blurInputs(){

	document.getElementById('internal_corner_dim').blur();
	document.getElementById('ruler_interval_world').blur();
	document.getElementById('checker_square_world').blur();

}

function changeCurrentMarker(d){

	var i;
	var new_num1, new_num2;

	if(current_landmark !== "NA"){

		new_num1 = current_landmark + d;
		if(new_num1 < 0){new_num1 = landmarks.length-1;}
		if(new_num1 > (landmarks.length-1)){new_num1 = 0;}
	
		selectObject('landmark', new_num1);
		//alert('changeCurrentMarker' + ': ' + current_landmark + ', ' + current_curve)

	}else if(current_curve !== "NA"){

		new_num1 = current_curve;
		
		if(d == 1){
			if(current_curve_point == 0){
				new_num2 = curves[current_curve].length-1;
			}else if(current_curve_point == curves[current_curve].length-1){
				new_num2 = 1;
			}else if(current_curve < curves.length-1 && current_curve_point == curves[current_curve].length-2){
				new_num1 = current_curve+1;
				new_num2 = 0;
			}else if(current_curve == curves.length-1 && current_curve_point == curves[current_curve].length-2){
				new_num1 = 0;
				new_num2 = 0;
			}else{			
				new_num2 = current_curve_point + 1;
			}
		}

		if(d == -1){
			if(current_curve == 0 && current_curve_point == 0){
				new_num1 = curves.length-1;
				new_num2 = curves[curves.length-1].length-2;
			}else if(current_curve > 0 && current_curve_point == 0){
				new_num1 = current_curve-1;
				new_num2 = curves[current_curve-1].length-2;
			}else if(current_curve_point == curves[current_curve].length-1){
				new_num2 = 0;
			}else if(current_curve_point == 1){
				new_num2 = curves[current_curve].length-1;
			}else{			
				new_num2 = current_curve_point-1;
			}
		}

		selectObject('curve', new_num1, new_num2);

	}else if(current_ruler !== "NA"){

		new_num1 = current_ruler + d;
		if(new_num1 < 0){new_num1 = rulers.length-1;}
		if(new_num1 > (rulers.length-1)){new_num1 = 0;}
	
		selectObject('ruler', new_num1);
	}
}

function changeImage(elem){
	
	var answer = true;
	var unsaved_shapes = '';
	var submit_params;
	var image_fpath;
	var image_fname;
	var aspect;
	var view;

	if(unsaved_landmarks == true) unsaved_shapes += '\tLandmarks\n';
	if(unsaved_curves == true) unsaved_shapes += '\tCurves\n';
	if(unsaved_ruler_points == true) unsaved_shapes += '\tRuler points\n';
	if(unsaved_corners == true) unsaved_shapes += '\tCheckerboard corners\n';

	if(unsaved_shapes !== '' && !document.getElementById('save_on_advance').checked)
		answer = confirm ("The following shape data have potentially unsaved changes:\n" + unsaved_shapes + "\nWould you still like to move to another image?");

	// Start submission object
	submit_params = {change_image:1}
	submit_params.prev_wd = init_params.prev_wd;

	// Save shapes if save on advance is checked
	if(document.getElementById('save_on_advance').checked) submitShapes(submit_params, true);

	if(elem.id == 'prev_image_aspect'){
		if(current_aspect == 0) return;
		submit_params.aspect = current_aspect - 1;
		if(stereo_input) submit_params.view = current_view;
	}
	if(elem.id == 'next_image_aspect'){
		if(current_aspect == images_fnames.length - 1) return;
		submit_params.aspect = current_aspect + 1;
		if(stereo_input) submit_params.view = current_view;
	}
	if(elem.id == 'prev_image_view' || elem.id == 'next_image_view'){

		if(elem.id == 'prev_image_view'){
			if(current_view == 0) return;
			submit_params.view = current_view - 1;
			submit_params.aspect = current_aspect;
		}
		if(elem.id == 'next_image_view'){
			if(current_view == view_names.length - 1) return;
			submit_params.view = current_view + 1;
			submit_params.aspect = current_aspect;
		}
	}

	if(elem.id == 'submit_aspect_image_names'){
		submit_params.aspect = elem.value;
		if(stereo_input) submit_params.view = parseFloat(document.getElementById("submit_view_image_names").value);
	}
	if(elem.id == 'submit_view_image_names'){
		submit_params.aspect = document.getElementById("submit_aspect_image_names").value;
		submit_params.view = parseFloat(elem.value);
	}

	// Dont change image if selected aspect is current aspect and view is null
	if(current_aspect !== undefined && current_aspect == submit_params.aspect && stereo_input == false) return;
	if(current_aspect !== undefined && stereo_input) if(current_aspect == submit_params.aspect && current_view == submit_params.view) return;

	submit_params.image_fname = images_fnames[submit_params.aspect];

	if(stereo_input){
		submit_params.image_fpath = init_params.images_fpaths[submit_params.aspect][submit_params.view];
		if(init_params.shapes_fpaths[0] !== '') submit_params.shape_fpath = init_params.shapes_fpaths[submit_params.aspect][submit_params.view];
		if(init_params.landmarks_fpaths[0] !== '') submit_params.landmark_fpath = init_params.landmarks_fpaths[submit_params.aspect][submit_params.view];
		if(init_params.control_points_fpaths[0] !== '') submit_params.control_points_fpath = init_params.control_points_fpaths[submit_params.aspect][submit_params.view];
		if(init_params.curve_points_fpaths[0] !== '') submit_params.curve_points_fpath = init_params.curve_points_fpaths[submit_params.aspect][submit_params.view];
		submit_params.add_view = submit_params.view + 1;
	}else{
		submit_params.image_fpath = init_params.images_fpaths[submit_params.aspect];
		if(init_params.shapes_fpaths[0] !== '') submit_params.shape_fpath = init_params.shapes_fpaths[submit_params.aspect];
		if(init_params.landmarks_fpaths[0] !== '') submit_params.landmark_fpath = init_params.landmarks_fpaths[submit_params.aspect];
		if(init_params.control_points_fpaths[0] !== '') submit_params.control_points_fpath = init_params.control_points_fpaths[submit_params.aspect];
		if(init_params.curve_points_fpaths[0] !== '') submit_params.curve_points_fpath = init_params.curve_points_fpaths[submit_params.aspect];
	}
	
	//alert(submit_params.image_fpath)

	// Send photograph position and size if zoom is copied
	if(document.getElementById('copy_zoom_checkbox').checked && photograph.x !== undefined){
		submit_params.photograph_x = photograph.x;
		submit_params.photograph_y = photograph.y;
		submit_params.photograph_w = photograph.w;
		submit_params.photograph_h = photograph.h;
		submit_params.zoom_d = zoom_d;
	}

	// Blur element
	elem.blur();

	if(answer) submit(submit_params);
}

function changeMarkerStyle(id, num1, num2, color){

	var circle = svgDocument.getElementById(id + '_' + num1 + '_' + num2);
	var line = svgDocument.getElementById(id + '_line_' + num1 + '_' + num2);

	if(!circle || !line){return false;}

	circle.setAttribute("style", "stroke:" + color + ";fill:none;stroke-opacity:0.8;fill-opacity:0.1;");
	line.setAttribute("style", "stroke:" + color + ";");
}

function clearCheckerboardCorners(){

	var i;

	// CLEAR CHECKER SQUARE SIZE
	checker_square_pixel = "NA";

	// CLEAR CHECKER WORLD SIZE
	//document.getElementById("checker_square_world").value = "";

	for(i = 0; i < corners.length;i++){
		var textlink = svgDocument.getElementById('corner_label' + i);
		if(textlink) svgDocument.getElementById("world").removeChild(textlink);

		var line = svgDocument.getElementById('corner_label_line' + i);
		if(line) svgDocument.getElementById("world").removeChild(line);

		var circle = svgDocument.getElementById('corner_' + i + '_');
		if(circle) svgDocument.getElementById("world").removeChild(circle);

		var line = svgDocument.getElementById('corner_cline' + i);
		if(line) svgDocument.getElementById("world").removeChild(line);

		var line = svgDocument.getElementById('corner_line_' + i + '_');
		if(line) svgDocument.getElementById("world").removeChild(line);
	}

	// CLEAR CORNERS
	corners = new Array();
	
	updateScaling()
	//reDrawObjects();

	unsaved_corners = true;
}

function clearCurves(num){

	var i, j;

	// Clear curve points
	for(i = 0; i < curves.length; i++){
		if(num !== undefined && i !== num) continue;
		for(j = 0; j < curves[i].length; j++) deleteCurvePoint(i, j);
	}

	// Shorten array back to three null points
	var curves_length = curves.length;
	for(i = 0; i < curves_length; i++){
		if(num !== undefined && i !== num) continue;
		for(j = curves[i].length-1; j > 2; j--){
			curves[i][j].splice(0, 2);
			curves[i].splice(j, 1);
		}
	}

	updateCurveTable();
}

function clearEpipolarLine(){

	var i;

	for(i=0;i < init_params.num_views;i++){

		var line = svgDocument.getElementById('epipolar_line' + i);
		if(line){svgDocument.getElementById("world").removeChild(line);}
	}
}

function clearLandmarks(clear_curves){

	var i, j;

	// Clear landmarks
	for(i = 0; i < landmarks.length; i++){

		// Do not clear curve points
		if(!clear_curves && landmark_also_curve[i][0] > -1) continue;
		
		// Clear landmark, or potentially curve if clear_curves is true
		deleteLandmark(i);
	}
	
	// Check for curves with no start or end point
	for(i = 0; i < curves.length; i++){

		if(curves[i][0].x != '-' || curves[i][(curves[i].length-1)].x != '-') continue;

		// Clear intervening curve points
		for(j = 1; j < curves[i].length-1; j++) deleteCurvePoint(i, j);
	}

	// Shorten array back to three null points
	var curves_length = curves.length;
	for(i = 0; i < curves_length; i++){

		if(curves[i][0].x != '-' || curves[i][(curves[i].length-1)].x != '-') continue;

		for(j = curves[i].length-1; j > 2; j--){
			curves[i][j].splice(0, 2);
			curves[i].splice(j, 1);
		}
	}

//	alert(printArrayToString(curves[0]));

	updateCurveTable();
}

function clearRulerPoints(){

	var i;	

	// Clear all ruler points
	for(i = 0; i < rulers.length; i++) deleteRulerPoint(i);

	// Clear fields from table
	for(i = 2; i < rulers.length; i++){
		var ruler_table_container = document.getElementById("ruler_table_container");
		var ruler_row = document.getElementById("ruler_row_" + i);
		if(ruler_table_container && ruler_row) ruler_table_container.removeChild(ruler_row);
	}

	// Shorten array back to three null points
	var rulers_length = rulers.length;
	for(i = rulers_length-1; i > 1; i--){
		rulers[i].splice(0, 2);
		rulers.splice(i, 1);
	}
}

function clearScaling(){

	// Clear scaling settings
	ruler_interval_pixels = "NA";
	document.getElementById("ruler_interval_world").value = '';
	document.getElementById("internal_corner_dim").value = '';
	checker_square_pixel = "NA";
	document.getElementById("checker_square_world").value = '';

	updateScaling()
}

function correctImagePosition(new_x,new_y,new_w,new_h,ow,oh){

	var x = new_x;
	var y = new_y;
	var w = new_w;
	var h = new_h;

	if(new_w < ow){w = ow;}
	if(new_h < oh){h = oh;}
	if(new_w < ow || new_h < oh){
		x = margin;
		y = margin;
		return {x : x, y : y, w : w, h : h};
	}
	if(new_x > margin){x = margin;}
	if(new_y > margin){y = margin;}
	if((new_x + new_w) < (margin + ow)){x = margin + ow - new_w;}
	if((new_y + new_h) < (margin + oh)){y = margin + oh - new_h;}

	return {x : x, y : y, w : w, h : h};
}

function createCookie(name,value,sec){
	if (sec) {
		var date = new Date();
		date.setTime(date.getTime()+(sec*1000));
		var expires = "; expires="+date.toGMTString();
	}
	else var expires = "";
	document.cookie = name+"="+value+expires+"; path=/";
}

function createCurveTable(){

	if(init_params.curves_ref === undefined || init_params.curves_ref == '') return;

	var i, j, k;
	for(i = 0;i < init_params.curves_ref.length;i++){
		start_dupl[i] = new Array();
		start_dupl[i].type = new Array();
		start_dupl[i].num = new Array();
		end_dupl[i] = new Array();
		end_dupl[i].type = new Array();
		end_dupl[i].num = new Array();

		for(j = 0;j < init_params.curves_ref.length;j++){
			if(init_params.curves_ref[j][1] == init_params.curves_ref[i][1]){
				start_dupl[i].type[start_dupl[i].type.length] = 'start';
				start_dupl[i].num[start_dupl[i].num.length] = j;
			}
			if(init_params.curves_ref[j][2] == init_params.curves_ref[i][1]){
				start_dupl[i].type[start_dupl[i].type.length] = 'end';
				start_dupl[i].num[start_dupl[i].num.length] = j;
			}
			if(init_params.curves_ref[j][1] == init_params.curves_ref[i][2]){
				end_dupl[i].type[end_dupl[i].type.length] = 'start';
				end_dupl[i].num[end_dupl[i].num.length] = j;
			}
			if(init_params.curves_ref[j][2] == init_params.curves_ref[i][2]){
				end_dupl[i].type[end_dupl[i].type.length] = 'end';
				end_dupl[i].num[end_dupl[i].num.length] = j;
			}
		}
	}

	// Default values for landmark also curve
	for(i = 0;i < init_params.landmarks_ref.length;i++) landmark_also_curve[i] = new Array(-1,-1);

	for(i = 0;i < init_params.curves_ref.length;i++){

		// Initialize empty curves array
		curves[i] = new Array();

		// Fill with three null points
		for(j = 0; j <= 2; j++){
			curves[i][j] = new Array();
			curves[i][j].x = '-';
			curves[i][j].y = '-';
		}

		// Look for landmarks of same name as curve start and end
		curve_also_landmark[i] = new Array();
		curve_also_landmark[i].start = -1;
		curve_also_landmark[i].end = -1;

		if(init_params.landmarks_ref !== undefined){
			for(j = 0;j < init_params.landmarks_ref.length;j++){
				if(init_params.curves_ref[i][1] == init_params.landmarks_ref[j]){
					curve_also_landmark[i].start = j;
					if(landmark_also_curve[j][0] == -1){
						landmark_also_curve[j][0] = i;
						landmark_also_curve[j][1] = 0;
					}
				}
				if(init_params.curves_ref[i][2] == init_params.landmarks_ref[j]){
					curve_also_landmark[i].end = j;
					if(landmark_also_curve[j][0] == -1){
						landmark_also_curve[j][0] = i;
						landmark_also_curve[j][1] = 1;
					}
				}
			}
		}
	}
	
//	alert(printArrayToString(landmark_also_curve));

	for(i = 0;i < init_params.curves_ref.length;i++){

		var curve_container = document.createElement('div');
		curve_container.className = 'curve_container';
		curve_container.id = 'curve_container_' + i;		
		curve_container.name = init_params.curves_ref[i][0];
		curve_container.innerHTML = '&nbsp;' + init_params.curves_ref[i][0];
		document.getElementById('curve_table_container').appendChild(curve_container);

			// Curve start point
			var curve_start_div = document.createElement('div');
			curve_start_div.className = 'curve_ends';
			curve_start_div.id = 'curve_end_' + i + '_0';
			curve_start_div.innerHTML = '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;' + init_params.curves_ref[i][1];
			curve_start_div.title = 'Click to select curve point';
			curve_start_div.setAttribute("onclick", "selectObject('curve'," + i + "," + 0 + ");");
			curve_container.appendChild(curve_start_div);

				var curve_row_start = document.createElement('div');
				curve_row_start.className = 'curve_point_row';
				curve_row_start.id = 'curve_row_' + i + '_0';
				curve_start_div.appendChild(curve_row_start);

					var curve_start_y = document.createElement('div');
					curve_start_y.className = 'start_end_point_col';
					curve_start_y.id = 'curve_' + i + '_0_y';
					curve_start_y.innerHTML = '-';
					curve_row_start.appendChild(curve_start_y);

					var curve_start_x = document.createElement('div');
					curve_start_x.className = 'start_end_point_col';
					curve_start_x.id = 'curve_' + i + '_0_x';
					curve_start_x.innerHTML = '-';
					curve_row_start.appendChild(curve_start_x);

			// Intermediate curve points
			var curve_block_div = document.createElement('div');
			curve_block_div.id = 'curve_block_' + i;		
			curve_block_div.className = 'curve_block';
			curve_container.appendChild(curve_block_div);

				var curve_row = document.createElement('div');
				curve_row.className = 'semilandmark_point_row';
				curve_row.id = 'curve_row_' + i + '_1';

					var curve_point_x = document.createElement('div');
					curve_point_x.className = 'semilandmark_point_col';
					curve_point_x.id = 'curve_' + i + '_1_x';
					curve_point_x.innerHTML = '-';
					curve_point_x.title = 'Click to select curve point';
					curve_point_x.setAttribute("onclick", "selectObject('curve'," + i + "," + 1 + ");");
					curve_row.appendChild(curve_point_x);

					var curve_point_y = document.createElement('div');
					curve_point_y.className = 'semilandmark_point_col';
					curve_point_y.id = 'curve_' + i + '_1_y';
					curve_point_y.innerHTML = '-';
					curve_point_y.title = 'Click to select curve point';
					curve_point_y.setAttribute("onclick", "selectObject('curve'," + i + "," + 1 + ");");
					curve_row.appendChild(curve_point_y);

				curve_block_div.appendChild(curve_row);

			// Curve end point
			var curve_end_div = document.createElement('div');
			curve_end_div.className = 'curve_ends';
			curve_end_div.id = 'curve_end_' + i + '_2';
			curve_end_div.innerHTML = '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;' + init_params.curves_ref[i][2];
			curve_end_div.title = 'Click to select curve point';
			curve_end_div.setAttribute("onclick", "selectObject('curve'," + i + "," + 2 + ");");
			curve_container.appendChild(curve_end_div);

				var curve_row_end = document.createElement('div');
				curve_row_end.className = 'curve_point_row';
				curve_row_end.id = 'curve_row_' + i + '_2';
				curve_end_div.appendChild(curve_row_end);

					var curve_end_y = document.createElement('div');
					curve_end_y.className = 'start_end_point_col';
					curve_end_y.id = 'curve_' + i + '_2_y';
					curve_end_y.innerHTML = '-';
					curve_row_end.appendChild(curve_end_y);

					var curve_end_x = document.createElement('div');
					curve_end_x.className = 'start_end_point_col';
					curve_end_x.id = 'curve_' + i + '_2_x';
					curve_end_x.innerHTML = '-';
					curve_row_end.appendChild(curve_end_x);		
	}
}

function createLandmarkTable(){

	if(init_params.curves_ref === undefined || init_params.landmarks_ref == '') return;

	var i, j, n;
	for(i = 0;i < init_params.landmarks_ref.length;i++){

		if(i % 2 == 0){n = 'even';}else{n = 'odd';}

		var landmark_row = document.createElement('div');
		landmark_row.className = 'landmark_table_row';
		landmark_row.id = 'landmark_row_' + i;		
		landmark_row.setAttribute("onclick", "selectObject('landmark', " + i + ");");
		document.getElementById('landmark_table_container').appendChild(landmark_row);

		var landmark_col_name = document.createElement('div');
		landmark_col_name.className = 'landmark_table_col1_' + n;
		landmark_col_name.id = 'landmark_col_name_' + i;
		landmark_col_name.innerHTML = init_params.landmarks_ref[i];
		document.getElementById('landmark_row_' + i).appendChild(landmark_col_name);

		var landmark_col_x = document.createElement('div');
		landmark_col_x.className = 'landmark_table_col2_' + n;
		landmark_col_x.id = 'landmark_col_x_' + i;
		landmark_col_x.innerHTML = '-';
		document.getElementById('landmark_row_' + i).appendChild(landmark_col_x);

		var landmark_col_y = document.createElement('div');
		landmark_col_y.className = 'landmark_table_col2_' + n;
		landmark_col_y.id = 'landmark_col_y_' + i;
		landmark_col_y.innerHTML = '-';
		document.getElementById('landmark_row_' + i).appendChild(landmark_col_y);

		// Default values for landmark also curve
		landmark_also_curve[i] = new Array(-1,-1);

		// Create empty landmark array entry
		landmarks[i] = new Array();
		landmarks[i].x = '-';
		landmarks[i].y = '-';
	}
}

function createScalingTable(){

	// Most of the scaling content is created in the html file, this does the rest
	rulers[0] = new Array();rulers[0].x = '-';rulers[0].y = '-';
	rulers[1] = new Array();rulers[1].x = '-';rulers[1].y = '-';
}

function dblClickEvent(e){

	// Because of multiple listeners (image, shapes) multiple double click events are possible
	// Check that double click event is not the same time as the previous one
	if(Math.abs(new Date().getTime() - last_dblClickEvent) < 50) return;

	getMouseXY(e);

	// Set time for double click event
	last_dblClickEvent = new Date().getTime();

	var i,j;
	if(e.keyCode > 0){ // key press event
		var p_pos_l = last_p_pos;
	}else{ // mouse dbl click
		var p_pos_l = findPixelPosition(initialX, initialY, photograph.x, photograph.y, photograph.w, photograph.h, photograph.pw, photograph.ph);
		p_pos = p_pos_l;
	}

	// Find factor by which to increase click radius to accommodate zoom changes
	var radius_factor_pos1 = findPixelPosition(0, 0, photograph.x, photograph.y, photograph.w, photograph.h, photograph.pw, photograph.ph);
	var radius_factor_pos2 = findPixelPosition(100, 100, photograph.x, photograph.y, photograph.w, photograph.h, photograph.pw, photograph.ph);
	var radius_factor = Math.abs(radius_factor_pos2[1] - radius_factor_pos1[1]) / 100;

//	document.getElementById('current_process').innerHTML = radius_factor;

	for(i = 0;i < landmarks.length;i++){
		if(landmarks[i].x == "-") continue;
		if(proximityCheck(Math.round(p_pos_l[0]), Math.round(p_pos_l[1]), landmarks[i].x, landmarks[i].y, click_radius*radius_factor)){
			selectObject('landmark', i);
			mousedown = 0;
			return;
		}
	}	
	for(i = 0;i < curves.length;i++){
		for(j = 0;j < curves[i].length;j++){
			if(curves[i][j].x == "-") continue;
			if(proximityCheck(Math.round(p_pos_l[0]), Math.round(p_pos_l[1]), curves[i][j].x, curves[i][j].y, click_radius*radius_factor)){
				selectObject('curve', i, j);
				mousedown = 0;
				return;
			}
		}
	}	
	for(i = 0;i < rulers.length;i++){
		if(rulers[i].x == "-") continue;
		if(proximityCheck(Math.round(p_pos_l[0]), Math.round(p_pos_l[1]), rulers[i].x, rulers[i].y, click_radius*radius_factor)){
			selectObject('ruler', i);
			mousedown = 0;
			return;
		}
	}

	if(current_landmark !== "NA"){addObject(Math.round(p_pos_l[0]),Math.round(p_pos_l[1]));}
	if(current_curve !== "NA"){addObject(Math.round(p_pos_l[0]),Math.round(p_pos_l[1]));}
	if(current_ruler !== "NA"){addObject(Math.round(p_pos_l[0]),Math.round(p_pos_l[1]));}
	addObject(Math.round(p_pos_l[0]),Math.round(p_pos_l[1]));
	if(auto_advance){changeCurrentMarker(1);}
	mousedown = 0;
}

function deleteCookie(name) {
	createCookie(name,"",-1);
}

function deleteCurvePoint(num1, num2){

	var i, j, point_num;

	if(num2 > 0 && num2 < curves[num1].length-1){

		curves[num1][num2].x = "-";
		curves[num1][num2].y = "-";

		var circle = svgDocument.getElementById('curve_' + num1 + '_' + num2);
		var line = svgDocument.getElementById('curve_line_' + num1 + '_' + num2);
		if(circle) svgDocument.getElementById("world").removeChild(circle);
		if(line) svgDocument.getElementById("world").removeChild(line);

		var cp_x_div = document.getElementById('curve_' + num1 + '_' + num2 + '_x');
		var cp_y_div = document.getElementById('curve_' + num1 + '_' + num2 + '_y');
		cp_x_div.innerHTML = "-";
		cp_y_div.innerHTML = "-";

		drawBezierCurve(num1);

	}else{
			
		if(num2 == 0){

			for(i = 0;i < start_dupl[num1].type.length;i++){

				if(start_dupl[num1].type[i] == 'start'){point_num = 0;}else{point_num = curves[start_dupl[num1].num[i]].length-1;}

				var circle = svgDocument.getElementById('curve_' + start_dupl[num1].num[i] + '_' + point_num);
				var line = svgDocument.getElementById('curve_line_' + start_dupl[num1].num[i] + '_' + point_num);

				if(circle) svgDocument.getElementById("world").removeChild(circle);
				if(line) svgDocument.getElementById("world").removeChild(line);

				var cp_x_div = document.getElementById('curve_' + start_dupl[num1].num[i] + '_' + point_num + '_x');
				var cp_y_div = document.getElementById('curve_' + start_dupl[num1].num[i] + '_' + point_num + '_y');
	
				cp_x_div.innerHTML = "-";
				cp_y_div.innerHTML = "-";
				
				curves[start_dupl[num1].num[i]][point_num].x = "-";
				curves[start_dupl[num1].num[i]][point_num].y = "-";
			}

			if(curve_also_landmark[num1].start >= 0){
				document.getElementById('landmark_col_x_' + curve_also_landmark[num1].start).innerHTML = '-';
				document.getElementById('landmark_col_y_' + curve_also_landmark[num1].start).innerHTML = '-';
			}

			for(i = 0;i < start_dupl[num1].num.length;i++) drawBezierCurve(start_dupl[num1].num[i]);

		}else{

			for(i = 0;i < end_dupl[num1].type.length;i++){

				if(end_dupl[num1].type[i] == 'start'){point_num = 0;}else{point_num = curves[end_dupl[num1].num[i]].length-1;}

				var circle = svgDocument.getElementById('curve_' + end_dupl[num1].num[i] + '_' + point_num);
				var line = svgDocument.getElementById('curve_line_' + end_dupl[num1].num[i] + '_' + point_num);

				if(circle) svgDocument.getElementById("world").removeChild(circle);
				if(line) svgDocument.getElementById("world").removeChild(line);

				var cp_x_div = document.getElementById('curve_' + end_dupl[num1].num[i] + '_' + point_num + '_x');
				var cp_y_div = document.getElementById('curve_' + end_dupl[num1].num[i] + '_' + point_num + '_y');
	
				//if(type == undefined) alert('ui ' + end_dupl[num1].num[i] + ' ' + point_num + ' ' + cp_x_div)
				cp_x_div.innerHTML = "-";
				cp_y_div.innerHTML = "-";
				
				curves[end_dupl[num1].num[i]][point_num].x = "-";
				curves[end_dupl[num1].num[i]][point_num].y = "-";
			}

			if(curve_also_landmark[num1].end >= 0){
				document.getElementById('landmark_col_x_' + curve_also_landmark[num1].end).innerHTML = '-';
				document.getElementById('landmark_col_y_' + curve_also_landmark[num1].end).innerHTML = '-';
			}

			for(i = 0;i < end_dupl[num1].num.length;i++) drawBezierCurve(end_dupl[num1].num[i]);
		}
	}
}

function deleteLandmark(num1){

	landmarks[num1].x = "-";
	landmarks[num1].y = "-";

	var circle = svgDocument.getElementById('landmark_' + num1 + '_');
	if(circle){svgDocument.getElementById("world").removeChild(circle);}
	var line = svgDocument.getElementById('landmark_line_' + num1 + '_');
	if(line){svgDocument.getElementById("world").removeChild(line);}

	var lm_x_div = document.getElementById('landmark_col_x_' + num1);
	var lm_y_div = document.getElementById('landmark_col_y_' + num1);

	lm_x_div.innerHTML = "-";
	lm_y_div.innerHTML = "-";
}

function deleteObject(type, num1, num2){

	if(current_landmark !== "NA"){
		deleteLandmark(current_landmark);
		unsaved_landmarks = true;
	}

	if(current_curve !== "NA"){
		deleteCurvePoint(current_curve, current_curve_point);
		unsaved_curves = true;
	}

	if(current_ruler !== "NA"){
		deleteRulerPoint(current_ruler);
		findRulerInterval();
		unsaved_ruler_points = true;
	}
}

function deleteRulerPoint(num1){

	rulers[num1].x = "-";
	rulers[num1].y = "-";

	var circle = svgDocument.getElementById('ruler_' + num1 + '_');
	if(circle){svgDocument.getElementById("world").removeChild(circle);}
	var line = svgDocument.getElementById('ruler_line_' + num1 + '_');
	if(line){svgDocument.getElementById("world").removeChild(line);}

	var ruler_x_div = document.getElementById('ruler_col_x_' + num1);
	var ruler_y_div = document.getElementById('ruler_col_y_' + num1);

	ruler_x_div.innerHTML = "-";
	ruler_y_div.innerHTML = "-";
}

function detectKeyEvent(e) {
	var evt = e || window.event;

	if(input_focus) return;

	if(evt.shiftKey === true){
		//if(evt.keyCode==65){if(auto_advance){auto_advance = false;}else{auto_advance = true;}} // a
		if(evt.keyCode==82){} // r
		if(evt.keyCode==82){} // r
		if(evt.keyCode==37){move(-10,0);} // left arrow
		if(evt.keyCode==39){move(10,0);} // right arrow
		if(evt.keyCode==38){move(0,-10);} // up arrow
		if(evt.keyCode==40){move(0,10);} // down arrow
		if(evt.keyCode==83){submitShapes();} // s
		if(evt.metaKey === true && evt.keyCode==37){move(-90,0);} // left arrow
		if(evt.metaKey === true && evt.keyCode==39){move(90,0);} // right arrow
		if(evt.metaKey === true && evt.keyCode==38){move(0,-90);} // up arrow
		if(evt.metaKey === true && evt.keyCode==40){move(0,90);} // down arrow
	}else{
		//if(evt.keyCode==8){deleteObject();} // delete
		if(evt.keyCode==68){deleteObject();} // d
		if(evt.keyCode==70){selectObject('landmark', 0)} // f
		if(evt.keyCode==65){globalkeypress = 'a';} // a
		if(evt.keyCode==78){changeCurrentMarker(1);} // n
		if(evt.keyCode==80){changeCurrentMarker(-1);} // p
		if(evt.keyCode==82){globalkeypress = 'r';} // r
		if(evt.keyCode==88){dblClickEvent(e);} // x
		if(evt.metaKey === true && evt.keyCode==82){doEvent('refreshtozero');}
		if(evt.keyCode==37){move(-1,0);} // left arrow
		if(evt.keyCode==39){move(1,0);} // right arrow
		if(evt.keyCode==38){move(0,-1);} // up arrow
		if(evt.keyCode==40){move(0,1);} // down arrow
		if(evt.keyCode==188){changeImage(document.getElementById('prev_image_aspect'));} // <
		if(evt.keyCode==190){changeImage(document.getElementById('next_image_aspect'));} // >
	}
}

function drawBezierCurve(num){

	var i, j;
	var clear_path = false;

	//if(curves[num][0].x == '-' || isNaN(curves[num][0].x)) return;
	//if(curves[num][curves[num].length-1].x == '-' || isNaN(curves[num][curves[num].length-1].x)) return;

	var t_pos = new Array();
	var n = 0;
	for(i = 0;i < curves[num].length;i++){

		if(i > 0 && i < curves[num].length-1)
			if(curves[num][i].x == '-' || isNaN(curves[num][i].x)) continue;

		t_pos[n] = moveToTarget(0, 0, curves[num][i].x, curves[num][i].y, photograph.w/photograph.pw, photograph.h/photograph.ph, 1);

		n++;
	}

	//alert(printArrayToString(t_pos))
	
	// If start is missing, clear first two points
	if(curves[num][0].x == '-' || isNaN(curves[num][0].x)) t_pos.splice(0, 2);

	// If end is missing, clear last two points
	if(curves[num][curves[num].length-1].x == '-' || isNaN(curves[num][curves[num].length-1].x)) t_pos.splice(t_pos.length-2, 2);

	// If curve has even number of points, remove last point
	if(t_pos.length > 2 && t_pos.length % 2 == 0) t_pos.splice(t_pos.length-1, 1);

	// If only two curve points and one or both is undefined, clear curve
	if(t_pos.length <= 2) clear_path = true;

	var px = photograph.x;
	var py = photograph.y;

	var j = 0;
	var d = "M ";

	for(i = 0;i < t_pos.length;i++){
		if(j == 1){d += 'Q ';}
		d += [Math.round(t_pos[i][0]+px)] + ' ' + [Math.round(t_pos[i][1]+py)] + ' ';
		j++;
	}

	if(clear_path) d = '';

	var path = svgDocument.getElementById('curve' + num);
	if(path){
		var curve_color = path.getAttribute("style");
		svgDocument.getElementById("world").removeChild(path);
	}else{var curve_color = "stroke:" + curve_color_blur + ";";}

	var path = svgDocument.createElementNS(svgns, "path");
	path.setAttribute("id", 'curve' + num);
	path.setAttribute("fill", "none");
	path.setAttribute("stroke-width", 2);
	path.setAttribute("style", curve_color);
	path.addEventListener("mousedown", function(evt) { evt.preventDefault(); }, false); // evt.preventDefault(); keeps browser from treating the image as if it is going to be exported out of the window
	path.addEventListener("dblclick", function(evt) { dblClickEvent(evt); }, false);
	path.setAttribute("d", d);
	svgDocument.getElementById("world").appendChild(path);
}

function drawCheckerboardCorners(corners_in){

	// INPUT IS STRING OF CORNER COORDINATES FROM JSON PARSER
	var i;

	unsaved_corners = true;

	for(i=0;i < corners_in.length;i++){

		corners[i] = new Array();
		corners[i].x = corners_in[i][0];
		corners[i].y = corners_in[i][1];
	}

	for(i=0;i < corners_in.length;i++) drawMarker('corner', i, '', corners[i].x, corners[i].y);
}

function drawEpipolarLine(){

	var i, j, cb, d;
	
	clearEpipolarLine();

	//alert('Input: ' + epipolar_cubic)

	if(epipolar_slope == 'NA' && epipolar_cubic == 'NA') return;

	var eline_colors = new Array('yellow', 'orange', 'aqua', 'magenta', 'red', 'slateblue');

	for(i=0;i < init_params.num_views;i++){

		if(epipolar_slope != 'NA'){

			//alert('Linear: ' + epipolar_slope)
		
			if(epipolar_slope[i] == 'NA'){continue;}
	
			var i_slope = epipolar_slope[i];
			var i_intercept = epipolar_intercept[i];

			// Find x-intercept
			var x_int = -parseFloat(i_intercept) / parseFloat(i_slope);
	
			var x1 = -photograph.pw;
			var x2 = photograph.pw;
	
			var xy1 = moveToTarget(0,0,x1,parseFloat(i_slope)*x1+parseFloat(i_intercept),photograph.w/photograph.pw,photograph.h/photograph.ph,1);
			var xy2 = moveToTarget(0,0,x2,parseFloat(i_slope)*x2+parseFloat(i_intercept),photograph.w/photograph.pw,photograph.h/photograph.ph,1);

			//alert(i_slope + ' ' + x_int + ' ' + (1/parseFloat(i_slope))*photograph.ph);
			//alert(photograph.w + ', ' + photograph.h);
			//alert((xy1[0] + photograph.x) + ',' + (xy1[1] + photograph.y) + ' ' + (xy2[0] + photograph.x) + ',' + (xy2[1] + photograph.y))
	
			// Near vertical lines do not print properly - rounding error? Change to purely vertical line if change in x over entire image is less than 0.3 px
			if(Math.abs((1/parseFloat(i_slope))*photograph.ph) < 0.3){
				xy1 = moveToTarget(0,0,x_int,0,photograph.w/photograph.pw,photograph.h/photograph.ph,1);
				xy2 = moveToTarget(0,0,x_int,photograph.ph,photograph.w/photograph.pw,photograph.h/photograph.ph,1);
			}

			var line = svgDocument.createElementNS(svgns, "line");
			line.setAttribute("id", 'epipolar_line' + i);
			line.setAttribute("x1", xy1[0] + photograph.x);
			line.setAttribute("y1", xy1[1] + photograph.y);
			line.setAttribute("x2", xy2[0] + photograph.x);
			line.setAttribute("y2", xy2[1] + photograph.y);
			line.setAttribute("stroke-width", 1);
			line.setAttribute("style", "stroke:" + eline_colors[i] + ";");
			line.addEventListener("mousedown", function(evt) { evt.preventDefault(); }, false); // evt.preventDefault(); keeps browser from treating the image as if it is going to be exported out of the window
			line.addEventListener("dblclick", function(evt) { dblClickEvent(evt); }, false);
			svgDocument.getElementById("world").appendChild(line);
		}

		if(epipolar_cubic != 'NA'){

			//alert('Cubic: ' + epipolar_cubic)

			for(i=0;i < init_params.num_views;i++){
			
				if(epipolar_cubic[i*8] == 'NA') continue;

				cb = epipolar_cubic.slice(i*8, i*8 + 8);

				// Apply transformation
				var tb = new Array();
				for(j = 0;j < cb.length;j=j+2){
					tb[j/2] = moveToTarget(0, 0, cb[j], cb[j+1], photograph.w/photograph.pw, photograph.h/photograph.ph, 1)
					tb[j/2][0] += photograph.x;
					tb[j/2][1] += photograph.y;
				}
				
				d = 'M'+tb[0][0]+' '+tb[0][1]+' C '+tb[1][0]+' '+tb[1][1]+', '+tb[2][0]+' '+tb[2][1]+', '+tb[3][0]+' '+tb[3][1];
				
				var path = svgDocument.createElementNS(svgns, "path");
				path.setAttribute("id", 'epipolar_line' + i);
				path.setAttribute("fill", "none");
				path.setAttribute("stroke-width", 1);
				path.setAttribute("style", "stroke:" + eline_colors[i] + ";");
				path.addEventListener("mousedown", function(evt) { evt.preventDefault(); }, false); // evt.preventDefault(); keeps browser from treating the image as if it is going to be exported out of the window
				path.addEventListener("dblclick", function(evt) { dblClickEvent(evt); }, false);
				path.setAttribute("d", d);
				svgDocument.getElementById("world").appendChild(path);
			}
		}
	}
	
	//alert(epipolar_slope)
}

function drawMarker(id, num1, num2, x, y, redraw_bezier){

	// Re-draw bezier by default
	if(redraw_bezier == undefined) var redraw_bezier = true;

	if(x == "-" || y == "-" || isNaN(x) || isNaN(y)){return false;}
//	if(id == 'corner' && num == 1) alert(id + ' ' + num + ' ' + x + ' ' + y);
	var t_pos = moveToTarget(0,0,x,y,photograph.w/photograph.pw,photograph.h/photograph.ph,1);
//	if(id == 'corner' && num == 1) alert(id + ' ' + num + ' ' + photograph.x + ' ' + y);

//	alert(id + ',' + num1 + ',' + num2 + ',' + x + ',' + y)

	var color_by_id, r;
	var fill = 'none';
	var fill_opacity = 0;
	var num_id;

	if(id == 'landmark' || id == 'ruler'){
		color_by_id = landmark_color_blur;
		r = landmark_radius;
	}else if(id == 'curve'){
		color_by_id = control_point_color_blur;
		r = control_point_radius;
	}else if(id == 'corner'){
		color_by_id = corner_color;
		r = corner_radius;
		fill = corner_color;
		fill_opacity = 0;
	}

	var circle = svgDocument.getElementById(id + '_' + num1 + '_' + num2);
	var line = svgDocument.getElementById(id + '_line_' + num1 + '_' + num2);
	if(circle){
		var circle_color = circle.getAttribute("style");
		svgDocument.getElementById("world").removeChild(circle);
	}else{var circle_color = "stroke:" + color_by_id + ";";}
	if(line){
		var dot_color = line.getAttribute("style");
		svgDocument.getElementById("world").removeChild(line);
	}else{var dot_color = "stroke:" + color_by_id + ";";}

	if(id == 'corner'){
		
		if(num1 == 0){

			var textlink = svgDocument.getElementById(id + '_label' + num1);
			if(textlink) svgDocument.getElementById("world").removeChild(textlink);

			var textBlock = svgDocument.createTextNode(num1+1);
			var textlink = svgDocument.createElementNS(svgns, "text");
			textlink.setAttribute("id", id + '_label' + num1);
			textlink.setAttribute("text-anchor", "left");
			textlink.setAttribute("font-size", 8.5);
			textlink.setAttribute("x", t_pos[0] + photograph.x + 3);
			textlink.setAttribute("y", t_pos[1] + photograph.y - 5);
			textlink.setAttribute("style", "stroke:" + fill + ";");
			textlink.appendChild(textBlock);
			svgDocument.getElementById("world").appendChild(textlink);
		}

		if(num1 < corners.length - 1){
			var t_pos2 = moveToTarget(0,0,corners[num1+1].x,corners[num1+1].y,photograph.w/photograph.pw,photograph.h/photograph.ph,1);

			var line = svgDocument.getElementById(id + '_cline' + num1);
			if(line) svgDocument.getElementById("world").removeChild(line);

			var line = svgDocument.createElementNS(svgns, "line");
			line.setAttribute("id", id + '_cline' + num1);
			line.setAttribute("x1", t_pos[0] + photograph.x);
			line.setAttribute("y1", t_pos[1] + photograph.y);
			line.setAttribute("x2", t_pos2[0] + photograph.x);
			line.setAttribute("y2", t_pos2[1] + photograph.y);
			line.setAttribute("stroke-width", 1);
			line.setAttribute("style", dot_color);
			line.addEventListener("mousedown", function(evt) { evt.preventDefault(); }, false); // evt.preventDefault(); keeps browser from treating the image as if it is going to be exported out of the window
			line.addEventListener("dblclick", function(evt) { dblClickEvent(evt); }, false);
			svgDocument.getElementById("world").appendChild(line);
		}
	}

	var circle = svgDocument.createElementNS(svgns, "circle");
	circle.setAttribute("id", id + '_' + num1 + '_' + num2);
	circle.setAttribute("cx", t_pos[0] + photograph.x);
	circle.setAttribute("cy", t_pos[1] + photograph.y);
	//circle.setAttribute("cx", Math.round(t_pos[0] + photograph.x));
	//circle.setAttribute("cy", Math.round(t_pos[1] + photograph.y));
	circle.setAttribute("r", r);
	circle.setAttribute("stroke-width", marker_stroke_width);

	circle.setAttribute("style", circle_color + "fill:" + fill + ";stroke-opacity:0.8;fill-opacity:" + fill_opacity + ";");
	circle.addEventListener("mousedown", function(evt) { evt.preventDefault(); }, false); // evt.preventDefault(); keeps browser from treating the image as if it is going to be exported out of the window
	circle.addEventListener("dblclick", function(evt) { dblClickEvent(evt); }, false);

	var line = svgDocument.createElementNS(svgns, "line");
	line.setAttribute("id", id + '_line_' + num1 + '_' + num2);
	line.setAttribute("x1", t_pos[0] + photograph.x - 1);
	line.setAttribute("y1", t_pos[1] + photograph.y);
	line.setAttribute("x2", t_pos[0] + photograph.x + 1);
	line.setAttribute("y2", t_pos[1] + photograph.y);
	//line.setAttribute("x1", Math.round(t_pos[0] + photograph.x - 1));
	//line.setAttribute("y1", Math.round(t_pos[1] + photograph.y));
	//line.setAttribute("x2", Math.round(t_pos[0] + photograph.x + 1));
	//line.setAttribute("y2", Math.round(t_pos[1] + photograph.y));
	line.setAttribute("stroke-width", 1);

	//dot_color = 'stroke:red;'

	line.setAttribute("style", dot_color);
	line.addEventListener("mousedown", function(evt) { evt.preventDefault(); }, false); // evt.preventDefault(); keeps browser from treating the image as if it is going to be exported out of the window
	line.addEventListener("dblclick", function(evt) { dblClickEvent(evt); }, false);

	if(redraw_bezier && id == 'curve') drawBezierCurve(num1);

	svgDocument.getElementById("world").appendChild(line);
	svgDocument.getElementById("world").appendChild(circle);
}

function exit(){

	var answer = true;
	var unsaved_shapes = '';

	if(unsaved_landmarks == true) unsaved_shapes += '\tLandmarks\n';
	if(unsaved_curves == true) unsaved_shapes += '\tCurves\n';
	if(unsaved_ruler_points == true) unsaved_shapes += '\tRuler points\n';
	if(unsaved_corners == true) unsaved_shapes += '\tCheckerboard corners points\n';
	
	if(unsaved_shapes !== '')
		answer = confirm ("The following shape data have potentially unsaved changes:\n" + unsaved_shapes + "\nWould you still like to exit?");

	if(answer){
		var submit_params = {exit:1};
		submit(submit_params);
		setTimeout(function(){window.close();}, 10);
	}
}

function filterViewEntries(curr_elem, id_value, levels_to_comm_anc){
	var i;
	for(i = 0;i < levels_to_comm_anc;i++){
		curr_elem = curr_elem.parentNode;
	}
//alert(curr_elem.getAttribute('class'));
	var div_id = findElementByIDFromAncestor(curr_elem,id_value);
	var div_view_entries = div_id.parentNode;

	for(i = 0;i < div_view_entries.childNodes.length;i++){
		if(div_view_entries.childNodes[i].tagName !== div_id.tagName){continue;}
		if(div_view_entries.childNodes[i].id == id_value){
			div_view_entries.childNodes[i].style.display = "block";
			//div_view_entries.childNodes[i].style.visibility = "";
			//alert(div_view_entries.childNodes[i].id);
		}else{
			if(div_view_entries.childNodes[i].getAttribute('class') == div_id.getAttribute('class')){div_view_entries.childNodes[i].style.display = "none";}
		}
	}


}

function findCheckerboardCorners(){

	updateScaling()

	if(checkerboard_nx == "NA" || checkerboard_ny == "NA"){
		alert('Please enter the number of internal checkerboard corners along each dimension (row, col).');
		return;
	}

	document.getElementById('current_process').innerHTML = "Detecting checkerboard corners...";
	var submit_params = {};
	submit_params.image_fname = current_image_fname;
	submit_params.checkerboard_nx = checkerboard_nx;
	submit_params.checkerboard_ny = checkerboard_ny;
	submit_params.find_checkerboard_corners = true;
	submit(submit_params);
}

function findElementByIDFromAncestor(ancestor,id_value){
	var i, n, p, m;
	for(i = 0;i < ancestor.childNodes.length;i++){
		if(ancestor.childNodes[i].id == id_value){return ancestor.childNodes[i];}
		var ancestor2 = ancestor.childNodes[i];
		for(n = 0;n < ancestor2.childNodes.length;n++){
			if(ancestor2.childNodes[n].id == id_value){return ancestor2.childNodes[n];}
			var ancestor3 = ancestor2.childNodes[n];
			for(p = 0;p < ancestor3.childNodes.length;p++){
				if(ancestor3.childNodes[p].id == id_value){return ancestor3.childNodes[p];}
				var ancestor4 = ancestor3.childNodes[p];
				for(m = 0;m < ancestor4.childNodes.length;m++){
					if(ancestor4.childNodes[m].id == id_value){return ancestor4.childNodes[m];}
				}
			}
		}
	}
}

function findPixelPosition(cX, cY, x, y, w, h, ow, oh){ // to find pixels in full image use pw,ph: findPixelPosition(mouseX,mouseY,myImage[id].x,myImage[id].y,myImage[id].w,myImage[id].h,myImage[id].pw,myImage[id].ph);

	var pX = ((cX - x)/w)*ow;
	var pY = ((cY - y)/h)*oh;

	return new Array(pX, pY);
}

function findRulerInterval(){

	var i;

	// CHECK IF ANY RULER POINTS ARE DEFINED
	var rulers_defined = false;
	for(i = 0;i < rulers.length;i++) if(rulers[i].x !== '-') rulers_defined = true;
	if(rulers_defined === false) return;

	var n = new Date().getTime();

	if(n < find_ruler_interval_run_after) return;

	// SUBMIT RULER POINTS TO 
	document.getElementById('current_process').innerHTML = "Calculating ruler interval...";
	//document.getElementById('current_process').innerHTML = rulers[0].x + ', ' + rulers[0].y + ', ' + n;
	//setTimeout(function(){document.getElementById('current_process').innerHTML = ''}, 1000);

	var submit_params = {};
	submit_params.ruler_points = [];

	// COPY RULER POINTS TO JSON STRUCTURE
	for(i = 0;i < rulers.length;i++) submit_params.ruler_points[i] = [rulers[i].x, rulers[i].y];

	submit_params.find_ruler_interval = true;

	submit(submit_params);
}

function getBrowserName(){
    var ua= navigator.userAgent, tem, 
    M= ua.match(/(opera|chrome|safari|firefox|msie|trident(?=\/))\/?\s*(\d+)/i) || [];
    if(/trident/i.test(M[1])){
        tem=  /\brv[ :]+(\d+)/g.exec(ua) || [];
        return 'IE '+(tem[1] || '');
    }
    if(M[1]=== 'Chrome'){
        tem= ua.match(/\bOPR\/(\d+)/)
        if(tem!= null) return 'Opera '+tem[1];
    }
    M= M[2]? [M[1], M[2]]: [navigator.appName, navigator.appVersion, '-?'];
    if((tem= ua.match(/version\/(\d+)/i))!= null) M.splice(1, 1, tem[1]);
    return M;
}

function getCookie(Name){ 
	var re=new RegExp(Name+"=[^;]+", "i"); //construct RE to search for target name/value pair
	if (document.cookie.match(re)) //if cookie found
		return document.cookie.match(re)[0].split("=")[1] //return its value
	return false;
}

function getLetters(input){return input.match(/[a-z|A-Z]+/g);}
function getNumbers(input){return input.match(/[0-9|.]+/g);}

function getMouseXY(e){
	mousedown = 1;
	if(IE){
		initialX = event.clientX;
		initialY = event.clientY;	
	}else{
		initialX = e.pageX;
		initialY = e.pageY;
	}
}

function getMouseXYLocal(e){
	if(IE){return new Array(event.clientX,event.clientY);}else{return new Array(e.pageX,e.pageY);}
}

function getMouseMoveXY(e){

	var i;
	var id = baseName(e);
	var m_id = '';

	var c_pos = getMouseXYLocal(e);
	if(c_pos[0] > margin && c_pos[0] < photograph.w && c_pos[1] > margin && c_pos[1] < photograph.h + margin){
		p_pos = findPixelPosition(c_pos[0],c_pos[1],photograph.x,photograph.y,photograph.w,photograph.h,photograph.pw,photograph.ph);
		document.getElementById("curr_mouse_pos").innerHTML = Math.round(p_pos[0]) + ', ' + Math.round(p_pos[1]);
	}
	
	if(p_pos == undefined) return;
	last_p_pos = new Array(Math.round(p_pos[0]), Math.round(p_pos[1]));

	// Find factor by which to increase click radius to accommodate zoom changes
	var radius_factor_pos1 = findPixelPosition(0, 0, photograph.x, photograph.y, photograph.w, photograph.h, photograph.pw, photograph.ph);
	var radius_factor_pos2 = findPixelPosition(100, 100, photograph.x, photograph.y, photograph.w, photograph.h, photograph.pw, photograph.ph);
	var radius_factor = Math.abs(radius_factor_pos2[1] - radius_factor_pos1[1]) / 100;

	svgDocument.onmousedown = getMouseXY;

	if(initiate == 0){
		prevX = initialX;
		prevY = initialY;
	}
	initiate = 1;

	if(IE){
		tempX = event.clientX + document.body.scrollLeft;
		tempY = event.clientY + document.body.scrollTop;
	}else{
		tempX = e.pageX;
		tempY = e.pageY;
	}  

	if (tempX < 0){tempX = 0;}
	if (tempY < 0){tempY = 0;}  

	if(current_landmark !== "NA"){
		var current_position = landmarks[current_landmark];
	}
	if(current_curve !== "NA"){
		var current_position = curves[current_curve][current_curve_point];
	}
	if(current_ruler !== "NA"){
		var current_position = rulers[current_ruler];
	}
	if(mousedown == 1 && globalkeypress == ''){		

		if(input_focus) blurInputs();

		distX = tempX - prevX;
		distY = prevY - tempY;

		if(current_landmark !== "NA" || current_curve !== "NA" || current_ruler !== "NA"){
			if(!mouse_move_in_prox){mouse_move_in_prox = proximityCheck(Math.round(p_pos[0]), Math.round(p_pos[1]), current_position.x, current_position.y, click_radius*radius_factor);}
		}
		if(current_landmark !== "NA" && mouse_move_in_prox){
			addObject(Math.round(p_pos[0]), Math.round(p_pos[1]));
		}
		if(current_curve !== "NA" && mouse_move_in_prox){
			
			addObject(Math.round(p_pos[0]), Math.round(p_pos[1]));
			
			if(current_curve_point == 0){
				for(i = 0;i < start_dupl[current_curve].num.length;i++){
					if(start_dupl[current_curve].num[i] == current_curve){continue;}
					drawBezierCurve(start_dupl[current_curve].num[i]);
				}
			}
			if(current_curve_point == curves[current_curve].length-1){
				for(i = 0;i < end_dupl[current_curve].num.length;i++){
					if(end_dupl[current_curve].num[i] == current_curve){continue;}
					drawBezierCurve(end_dupl[current_curve].num[i]);
				}
			}
		}
		if(current_ruler !== "NA" && mouse_move_in_prox){
			addObject(Math.round(p_pos[0]), Math.round(p_pos[1]));
		}
		if(!mouse_move_in_prox){
			if(distX !== 0){move_photograph_x(distX);}
			if(distY !== 0){move_photograph_y(-distY);}
			reDrawObjects();
		}
		//loadFrame();
	}

	svgDocument.onmouseup = function(){
		if(input_focus) blurInputs();
		mousedown = 0;
		mouse_move_in_prox = 0;
	}

	prevX = tempX;
	prevY = tempY;

	return false;
}


function getStyle(el, styleProp) {
  var value, defaultView = (el.ownerDocument || document).defaultView;
  // W3C standard way:
  if (defaultView && defaultView.getComputedStyle) {
    // sanitize property name to css notation
    // (hypen separated words eg. font-Size)
    styleProp = styleProp.replace(/([A-Z])/g, "-$1").toLowerCase();
    return defaultView.getComputedStyle(el, null).getPropertyValue(styleProp);
  } else if (el.currentStyle) { // IE
    // sanitize property name to camelCase
    styleProp = styleProp.replace(/\-(\w)/g, function(str, letter) {
      return letter.toUpperCase();
    });
    value = el.currentStyle[styleProp];
    // convert other units to pixels on IE
    if (/^\d+(em|pt|%|ex)?$/i.test(value)) { 
      return (function(value) {
        var oldLeft = el.style.left, oldRsLeft = el.runtimeStyle.left;
        el.runtimeStyle.left = el.currentStyle.left;
        el.style.left = value || 0;
        value = el.style.pixelLeft + "px";
        el.style.left = oldLeft;
        el.runtimeStyle.left = oldRsLeft;
        return value;
      })(value);
    }
    return value;
  }
}

function getRadios(name){

	var radios = document.getElementsByName(name);

	for(var i=0, length=radios.length; i < length; i++) if(radios[i].checked) return radios[i].value;
}

function getWindowHeight(){
	var y = 0;
	if (self.innerHeight) {
		y = self.innerHeight;
	} else if (document.documentElement && document.documentElement.clientHeight) {
		y = document.documentElement.clientHeight;
	} else if (document.body) {
		y = document.body.clientHeight;
	}

	return y;
}

function getWindowWidth(){
	var x = 0;
	if (self.innerHeight) {
		x = self.innerWidth;
	} else if (document.documentElement && document.documentElement.clientHeight) {
		x = document.documentElement.clientWidth;
	} else if (document.body) {
		x = document.body.clientWidth;
	}
	
	return x;
}

function imageZoom(d,mouseX,mouseY){

	if(d == 0) return;

	zoom_d = zoom_d + d;
	if(zoom_d < 1){zoom_d = 1;}

	if(zoom_d < 5){
		var new_w = photograph.ow*Math.pow(zoom_d,1.7);
		var new_h = photograph.oh*Math.pow(zoom_d,1.7);
	}else{
		var new_w = photograph.ow*Math.pow(zoom_d,2);
		var new_h = photograph.oh*Math.pow(zoom_d,2);	
	}

	p_pos = findPixelPosition(mouseX,mouseY,photograph.x,photograph.y,photograph.w,photograph.h,photograph.pw,photograph.ph);
	if(!readCookie('p_pos_x')){
		var save_time = 2;
		if(zoom_d > 4){var save_time = 2;}
		//if(zoom_d > 3){var save_time = 3;}
		createCookie('p_pos_x',p_pos[0],save_time);createCookie('p_pos_y',p_pos[1],save_time);
	}else{
		var p_pos = new Array(readCookie('p_pos_x'),readCookie('p_pos_y'));
	}
	var t_pos = findPixelPosition((photograph.ow/2) + margin,(photograph.oh/2) + margin,photograph.x,photograph.y,photograph.w,photograph.h,photograph.pw,photograph.ph);
	var m_pos = moveToTarget(p_pos[0],p_pos[1],Math.round(t_pos[0]),Math.round(t_pos[1]),new_w/photograph.pw,new_h/photograph.ph,0);
	var exp_cof = new Array(new_w/photograph.w,new_h/photograph.h);
	var exp_d = new Array(p_pos[0]*(photograph.w/photograph.pw)*(exp_cof[0] - 1),p_pos[1]*(photograph.h/photograph.ph)*(exp_cof[1] - 1));
	var new_x = photograph.x + m_pos[0] - exp_d[0]; // exp_d accounts for changes in pixel position due to expansion of the image
	var new_y = photograph.y + m_pos[1] - exp_d[1];

	new_w = new_w;
	new_h = new_h;
	var new_values = correctImagePosition(new_x,new_y,new_w,new_h,photograph.ow,photograph.oh);

	var image = svgDocument.getElementById(photograph.id);
	image.setAttribute("width", new_values.w);
	image.setAttribute("height", new_values.h);
	image.setAttribute("x", new_values.x);
	image.setAttribute("y", new_values.y);
	photograph.x = new_values.x;
	photograph.y = new_values.y;
	photograph.w = new_values.w;
	photograph.h = new_values.h;

	// UPDATE IMAGE ZOOM DISPLAY
	document.getElementById('curr_zoom').innerHTML = Math.round((photograph.w / myImageInit[0].w)*100) + "%";

	reDrawObjects();
	//loadFrame();

	// 1445.729795477851,961.0438847819354,-577.729795477851,0
	//alert([new_values.w, new_values.h, new_values.x, new_values.y, zoom_d])
}

function is_array(input){
	return typeof(input)=='object'&&(input instanceof Array);
}

function loadFrame(){

	var i;
	var xywh_array = new Array(); // From top, clockwise
	xywh_array[0] = new Array(0,0,2000,margin);
	xywh_array[0] = new Array(photograph.ow+margin-1,0,1000,2000);
	xywh_array[1] = new Array(0,photograph.oh+margin-1,1000,1000);
	xywh_array[2] = new Array(0,0,margin,2000);

	for(i = 0;i < xywh_array.length;i++){
		var rect = svgDocument.getElementById("rect" + i);
		if(rect){svgDocument.getElementById("world").removeChild(rect);}
		var rect = svgDocument.createElementNS(svgns, "rect");
		svgSetXYWH(rect,xywh_array[i][0],xywh_array[i][1],xywh_array[i][2],xywh_array[i][3]);
		rect.setAttribute("id", "rect" + i);
		rect.setAttribute("fill", frame_color);
		svgDocument.getElementById("world").appendChild(rect);
	}
}

function loadImageNames(){

	var i, j, k, button, option, select, split_str;

	if(stereo_input){

		// Get view select element
		select = document.getElementById("submit_view_image_names");
		
		// Get view names
		for(i = 0; i < init_params.images_fpaths[0].length; i++){

			// Split at backslash
			split_str = init_params.images_fpaths[0][i].split('/');

			// Take second to last element
			view_names[i] = split_str[split_str.length - 2];

			// Add view name to select
			option = document.createElement("option");
			option.text = split_str[split_str.length - 2];
			option.value = i;
			select.add(option);
		}

		// Get aspect select element
		select = document.getElementById("submit_aspect_image_names");

		// Get aspect names
		for(i = 0; i < init_params.images_fpaths.length; i++){

			// Split at backslash
			split_str = init_params.images_fpaths[i][0].split('/');
			
			// Take last element
			images_fnames[i] = split_str[split_str.length - 1];

			// Add view name to select
			option = document.createElement("option");
			option.text = split_str[split_str.length - 1];
			option.value = i;
			select.add(option);
		}

	}else{

		// Get image select element
		select = document.getElementById("submit_aspect_image_names");

		// Get image names
		for(i = 0; i < init_params.images_fpaths.length; i++){

			// Split at backslash
			split_str = init_params.images_fpaths[i].split('/');

			// Take last element
			images_fnames[i] = split_str[split_str.length - 1];

			// Add view name to select
			option = document.createElement("option");
			option.text = split_str[split_str.length - 1];
			option.value = i;
			select.add(option);
		}

		// Remove change image view
		document.getElementById("submit_image_view").remove();
		
		// Widen image/aspect select
		select.style.margin = '3px 2px 0px 3px';
		select.style.width = '304px';

		document.getElementById("submit_image_label_aspect").style.width = '0px';
		document.getElementById("submit_image_label_aspect").innerHTML = '';

		document.getElementById("submit_image_change_aspect").style.width = '312px';
		//document.getElementById("submit_image_change_aspect").style.backgroundColor = 'blue';
		document.getElementById("submit_image_change_aspect").style.border = 'none';
		document.getElementById("submit_links").style.borderBottom = '1px dotted orange';

		document.getElementById("next_image_aspect").innerHTML = 'Next Image';
		document.getElementById("prev_image_aspect").innerHTML = 'Previous Image';

		document.getElementById("prev_image_aspect").style.width = '145px';
		document.getElementById("prev_image_aspect").style.margin = '0px 5px 0px 6px';
		document.getElementById("next_image_aspect").style.width = '145px';
	}
}

function loadNewImage(server_out){

	//document.getElementById('current_process').innerHTML = 'Loading image...';
	scaling_wpp_updated = false;

	// Fill in image details
	document.getElementById("image_file").innerHTML = server_out.image_fpath;
	document.getElementById("image_dimensions").innerHTML = server_out.image_width +  ' x ' + server_out.image_height + ' px';
	document.getElementById("image_size").innerHTML = Math.round((server_out.image_size/10000), 3)/100 + ' MB';

	// Fill save-to information - hide the div if no file is selected
	if(!server_out.shape_fpath){
		document.getElementById("shape_file_row").style.display = 'none';
	}else{
		document.getElementById("shape_file").title = server_out.shape_fpath;
		document.getElementById("shape_file").innerHTML = current_save_shapes = server_out.shape_fpath;
	}

	if(!server_out.landmark_fpath){
		document.getElementById("landmark_file_row").style.display = 'none';
	}else{
		document.getElementById("landmark_file").title = server_out.landmark_fpath;
		document.getElementById("landmark_file").innerHTML = current_save_landmarks = server_out.landmark_fpath;
	}

	if(!server_out.control_points_fpath){
		document.getElementById("control_point_file_row").style.display = 'none';
	}else{
		document.getElementById("control_point_file").title = server_out.control_points_fpath;
		document.getElementById("control_point_file").innerHTML = current_save_control_points = server_out.control_points_fpath;
	}

	if(!server_out.curve_points_fpath){
		document.getElementById("curve_point_file_row").style.display = 'none';
	}else{
		document.getElementById("curve_point_file").title = server_out.curve_points_fpath;
		document.getElementById("curve_point_file").innerHTML = current_save_curve_points = server_out.curve_points_fpath;
	}

	// Set default window zoom - will be overwritten if zoom is saved from previous image
	zoom_d = 1;

	// Update myImageInit, not sure exactly why I made this array...
	myImageInit = new Array();
	myImageInit[0] = new Array();
	myImageInit[0].x = 0;
	myImageInit[0].y = 0;
	myImageInit[0].w = server_out.image_width;
	myImageInit[0].h = server_out.image_height;
	myImageInit[0].id = 'img1';
	myImageInit[0].src = 'img/' + server_out.image_fname;

	// Reload image
	//alert('img/' + server_out.image_fname);
	reloadImage(0, 0, server_out.image_width, server_out.image_height, 'img1', 'img/' + server_out.image_fname);
	
	// Increase image load count
	image_load_count++;

	// Update image name
	current_image_fname = server_out.image_fname;

	// Update aspect and view
	document.getElementById("submit_aspect_image_names").value = parseFloat(server_out.aspect);
	current_aspect = parseFloat(server_out.aspect);
	if(server_out.view){
		document.getElementById("submit_view_image_names").value = parseFloat(server_out.view);
		current_view = parseFloat(server_out.view);
	}
	
	// Update aspect submit buttons
	if(current_aspect == 0){setButtonStyle("prev_image_aspect", 'off');}else{setButtonStyle("prev_image_aspect", 'on');}
	if(current_aspect == images_fnames.length - 1){setButtonStyle("next_image_aspect", 'off');}else{setButtonStyle("next_image_aspect", 'on');}

	// Update view submit buttons
	if(stereo_input){
		if(current_view == 0){setButtonStyle("prev_image_view", 'off');}else{setButtonStyle("prev_image_view", 'on');}
		if(current_view == view_names.length - 1){setButtonStyle("next_image_view", 'off');}else{setButtonStyle("next_image_view", 'on');}
	}
}

function loadShapesFromFile(server_out){

	var i, j, k, num;
	// This function is only called when changing the image
	
	// Save the currently selected object - this ensures the same object stays selected across changing the image
	saveApplySelection('save');
	
	// If only copying selected landmark, clear all landmarks except current landmark
	if(getRadios('copy_landmarks') == 'selected'){
		for(i = 0; i < landmarks.length; i++) if(i !== current_landmark) deleteLandmark(i);
	}

	// If only copying selected curve, clear all curves except current curve
	if(getRadios('copy_curves') == 'selected'){
		for(i = 0; i < curves.length; i++) if(i !== current_curve) clearCurves(i);
	}

	// Clear all objects objects, with true also clears all landmarks
	if(getRadios('copy_landmarks') == 'off' && getRadios('copy_curves') == 'off'){
		clearLandmarks(true);
		clearCurves();
		//alert(printArrayToString(curves[0]));
	}

	// Copy only landmarks - clear curves only
	if(getRadios('copy_landmarks') !== 'off' && getRadios('copy_curves') == 'off') clearCurves();

	// Copy only curves - clear landmarks (non-curve point) only
	if(getRadios('copy_landmarks') == 'off' && getRadios('copy_curves') !== 'off') clearLandmarks(false);

	// Clear checkerboard corners
	clearCheckerboardCorners();
	unsaved_corners = false;

	// Clear ruler points if box is unchecked or if any incoming ruler points
	if(!document.getElementById('copy_ruler_points_checkbox').checked || server_out.rulers) clearRulerPoints();

	// Clear scaling if box is unchecked
	if(!document.getElementById('copy_scaling_checkbox').checked) clearScaling();

	// Clear epipolar line
	clearEpipolarLine();
	epipolar_slope = "NA";
	epipolar_intercept = "NA";
	epipolar_cubic = "NA";

	// Landmarks added array - array to keep track of loaded landmarks
	loaded_landmarks = new Array();

	// Add curves
	if(server_out.curves && init_params.curves_ref[0].length > 0){

		for(i = 0; i < server_out.curves.length; i++){
			
			// Find number corresponding to curve
			for(j = 0; j < init_params.curves_ref.length; j++) if(server_out.curves[i][0] == init_params.curves_ref[j][0]){num = j;break;}

			// If any intermediate curve points are to be added, clear any previous curve points - since server_out.curves is a string w label, 2 points will be 5 elements
			if(server_out.curves[i].length > 5) clearCurves(num);
			
			// Save last curve control point first so it is not added as intermediate curve point
			addCurvePoint(num, curves[num].length-1, parseFloat(server_out.curves[i][(server_out.curves[i].length-2)]), 
				parseFloat(server_out.curves[i][(server_out.curves[i].length-1)]));

			k = 0;
			for(j = 1; j < server_out.curves[i].length-2; j = j + 2){
				addCurvePoint(num, k, parseFloat(server_out.curves[i][j]), parseFloat(server_out.curves[i][j+1]));
				k++;
			}

			// Added first and last control point as loaded landmarks
			loaded_landmarks[loaded_landmarks.length] = init_params.curves_ref[num][1];
			loaded_landmarks[loaded_landmarks.length] = init_params.curves_ref[num][2];
		}
	}
	
	//alert(loaded_landmarks);

	// Add landmarks
	if(server_out.landmarks && init_params.landmarks_ref){

		for(i = 0; i < server_out.landmarks.length; i++){

			// Check if landmark has been loaded (as curve point)
			in_loaded = loaded_landmarks.indexOf(server_out.landmarks[i][0]);
			
			// If so, continue
			if(in_loaded > -1) continue;

			// Find number corresponding to landmark
			num = init_params.landmarks_ref.indexOf(server_out.landmarks[i][0]);

			// If any landmarks are in shapes file but not landmark reference
			if(num < 0) continue;

			// Landmark also curve
			if(landmark_also_curve.length > 0 && landmark_also_curve[num][0] > -1){
				
				// Add landmark as first curve point
				if(landmark_also_curve[num][1] == 0) addCurvePoint(landmark_also_curve[num][0], 0, 
					parseFloat(server_out.landmarks[i][1]), parseFloat(server_out.landmarks[i][2]));

				// Add as last curve point
				if(landmark_also_curve[num][1] == 1) addCurvePoint(landmark_also_curve[num][0], curves[landmark_also_curve[num][0]].length-1, 
					parseFloat(server_out.landmarks[i][1]), parseFloat(server_out.landmarks[i][2]));
				
				continue;
			}

			// Add landmark
			addLandmark(num, parseFloat(server_out.landmarks[i][1]), parseFloat(server_out.landmarks[i][2]));
		}

		// Clear unsaved landmarks since landmarks loaded in from file
		unsaved_landmarks = false;
	}

	// Clear unsaved landmarks since landmarks loaded in from file; if copy landmarks is true, potentially unsaved landmarks, keep unsaved true
	if(getRadios('copy_landmarks') !== 'off' && image_load_count > 1){

		unsaved_landmarks = true;

		// Only selected landmark is copied but no landmark selected
		if(getRadios('copy_landmarks') == 'selected' && current_landmark == 'NA') unsaved_landmarks = false;
	}else{
		unsaved_landmarks = false;
	}

	// Clear unsaved curves since curves loaded in from file; if copy curves is true, potentially unsaved curves, keep unsaved true
	if(getRadios('copy_curves') !== 'off' && image_load_count > 1){

		unsaved_curves = true;

		// Only selected curve copied but no curve selected
		if(getRadios('copy_curves') == 'selected' && current_curve == 'NA') unsaved_curves = false;
	}else{
		unsaved_curves = false;
	}
	
	// Add ruler points
	if(server_out.rulers !== undefined){

		var ruler_nums = new Array();

		// Get maximum ruler point number
		for(i = 0;i < server_out.rulers.length;i++) ruler_nums[i] = server_out.rulers[i][0].substring(12, server_out.rulers[i][0].length);

		// Add fields to table if exceeds two
		for(i = 0;i <= Math.max.apply(Math, ruler_nums)+1;i++) if(i > 2) addField('ruler');

		// Add ruler points
		for(i = 0;i < server_out.rulers.length;i++)
			addRulerPoint(ruler_nums[i] - 1, parseFloat(server_out.rulers[i][1]), parseFloat(server_out.rulers[i][2]));

	}

	// Clear unsaved ruler points since ruler points loaded in from file; if copy ruler points is true, potentially unsaved ruler points, keep unsaved true
	if(document.getElementById('copy_ruler_points_checkbox').checked && image_load_count > 1){unsaved_ruler_points = true;}else{unsaved_ruler_points = false;}

	// Draw checkerboard corners
	if(server_out.corners !== undefined) drawCheckerboardCorners(server_out.corners);

	// Clear unsaved corners since corners loaded in from file
	unsaved_corners = false;

	// Add scaling settings
	if(!isNaN(server_out.ruler_pixel)) ruler_interval_pixels = Number(server_out.ruler_pixel)
	if(!isNaN(server_out.ruler_interval)) document.getElementById("ruler_interval_world").value = server_out.ruler_interval;
	if(!isNaN(server_out.ruler_interval) && server_out.scaling_units !== "NA") document.getElementById("ruler_interval_world").value += ' ' + server_out.scaling_units;
	if(!isNaN(server_out.checkerboard_nx)) document.getElementById("internal_corner_dim").value = server_out.checkerboard_nx;
	if(!isNaN(server_out.checkerboard_ny)) document.getElementById("internal_corner_dim").value += ', ' + server_out.checkerboard_ny;

	if(!isNaN(server_out.checker_square_pixel)) checker_square_pixel = Number(server_out.checker_square_pixel);
	if(!isNaN(server_out.checker_square_world)) document.getElementById("checker_square_world").value = server_out.checker_square_world;
	if(server_out.scaling_units !== "NA" && !isNaN(server_out.checker_square_world)) document.getElementById("checker_square_world").value += ' ' + server_out.scaling_units;

	// Clear ruler interval pixels so it's not read by further server out functions
	updateScaling()
	
	// Apply previously saved selection
	saveApplySelection('apply');

//	clearLandmarks(true);
}

function move(dx,dy){

	if(current_landmark !== "NA"){

		addLandmark(current_landmark, parseFloat(landmarks[current_landmark].x) + dx, 
			parseFloat(landmarks[current_landmark].y) + dy);

		unsaved_landmarks = true;

		selectObject('landmark', current_landmark, '', true, false);

		//loadFrame();

		return;
	}
	if(current_curve !== "NA"){

		addCurvePoint(current_curve, current_curve_point, parseFloat(curves[current_curve][current_curve_point].x) + dx, 
			parseFloat(curves[current_curve][current_curve_point].y) + dy);

		selectObject('curve', current_curve, current_curve_point, true, false);

		unsaved_curves = true;

		//loadFrame();

		return;
	}
	if(current_ruler !== "NA"){

		addObject(parseFloat(rulers[current_ruler].x) + dx, parseFloat(rulers[current_ruler].y) + dy);
		
		//addRulerPoint(current_ruler, parseFloat(rulers[current_ruler].x) + dx, parseFloat(rulers[current_ruler].y) + dy);
		//selectObject('ruler', current_ruler, '', true, false);
		//unsaved_ruler_points = true;
		//loadFrame();

		return;
	}
	if(dx !== 0){move_photograph_x(dx);}
	if(dy !== 0){move_photograph_y(dy);}
	reDrawObjects();
}

function moveToTarget(xi,yi,xf,yf,sw,sh,rnd){
	var nX = (xf - xi)*sw;
	var nY = (yf - yi)*sh;

	if(rnd){
		var nX = (xf - xi)*sw;
		var nY = (yf - yi)*sh;	
	}
	
	return new Array(nX,nY);
}

function move_photograph_x(d){
	var new_x = photograph.x + d;
	var new_values = correctImagePosition(new_x,photograph.y,photograph.w,photograph.h,photograph.ow,photograph.oh);
	photograph.x = new_values.x;

	var image = svgDocument.getElementById(photograph.id);
	image.setAttribute("x", new_values.x);
	//svgDocument.getElementById("world").appendChild(image);
}

function move_photograph_y(d){
	var new_y = photograph.y + d;
	var new_values = correctImagePosition(photograph.x,new_y,photograph.w,photograph.h,photograph.ow,photograph.oh);
	photograph.y = new_values.y;

	var image = svgDocument.getElementById(photograph.id);
	image.setAttribute("y", new_values.y);
	//svgDocument.getElementById("world").appendChild(image);
}

function onBlurInput(obj){
	input_focus = false;
}

function onFocusInput(obj){
	input_focus = true;
}

function onLoadFunctions(evt){

	// Get browser name
	var get_browser_name = getBrowserName();
	browser_name = get_browser_name[0];
	browser_version = get_browser_name[1];

	setSettings();

	// Get JSON string from R
	var json_string = document.getElementById('text_input').value

	// Parse JSON string
    init_params = JSON.parse(json_string);

	// Determine if input is stereo or single view
	if(is_array(init_params.images_fpaths[0]) == true) stereo_input = true;

	svgns = "http://www.w3.org/2000/svg";
	XLINK = "http://www.w3.org/1999/xlink";
	if ( window.svgDocument == null ) {
		var object = document.getElementById('image_object');
		image_object = object;
		if (object.contentDocument)  // works in Mozilla
			svgDocument = object.contentDocument;
		else {
			try {
				svgDocument = object.getSVGDocument();
			}
			catch(exception) {
				// ignore errors
			}
		}
	}
	svgObj = svgDocument.documentElement;
	svgDoc = object.contentDocument;

	digitize_image = this;
	window.name = "digitize_image";

	document.getElementById('keydown').focus();
	document.getElementById('keydown').onclick = setEvents;
	document.getElementById('keydown').onclick();

	window.onresize = onWindowResize;
	if(window.addEventListener){
		svgDocument.addEventListener('DOMMouseScroll', scrollEvent, true);
	}

	svgDocument.onmousemove = getMouseMoveXY;
	svgDocument.onmousewheel = scrollEvent;
	//alert(image_object)
	//image_object.onmousewheel = scrollEvent;		// SAFARI
	document.onkeyup=function(){globalkeypress = '';}

	// Set auto advance
	//if(init_params.auto_advance === undefined){auto_advance = true;}else{auto_advance = (String(init_params.auto_advance).toLowerCase() == 'true');}

	// Turn off submit button if no shape files are input
	if(init_params.shapes_fpaths == '' && init_params.landmarks_fpaths == '' && 
		init_params.control_points_fpaths == '' && init_params.curve_points_fpaths == ''){

		var submit_button = document.getElementById('submit_all_button');
		submit_button.className = 'submit_button_off';
		submit_button.title = '';
		submit_button.onclick = '';
	}

	landmark_color_blur = init_params.landmark_color_blur
	landmark_color_focus = init_params.landmark_color_focus
	curve_color_blur = init_params.curve_color_blur
	control_point_color_blur = init_params.control_point_color_blur
	control_point_color_focus = init_params.control_point_color_focus
	landmark_radius = init_params.landmark_radius
	control_point_radius = init_params.control_point_radius
	marker_stroke_width = init_params.marker_stroke_width

	// Load session information
	loadImageNames();
	createScalingTable();
	createLandmarkTable();
	createCurveTable();

	// Select first image
	changeImage(document.getElementById("submit_aspect_image_names"));

	marker_selected = 0;
	
	selectObject('landmark', 1)
}

function onWindowResize(){

	var display;

	window_properties.height = getWindowHeight();
	window_properties.width = getWindowWidth();
	image_object.width = window_properties.width - 370;
	image_object.height = window_properties.height - 2*margin - 15;

	// Determine div heights
	var new_tab_div_height = (window_properties.height-240);
	
	// Restore visibility after setting height - for some reason above command overwrites display but not other styles
	var tab_divs = new Array('settings', 'collect_landmarks', 'collect_curves', 'scaling');
	for(var i=0; i < tab_divs.length; i++){
		display = getStyle(document.getElementById(tab_divs[i]), 'display');
		document.getElementById(tab_divs[i]).setAttribute('style', 'height:' + new_tab_div_height + 'px;');
		document.getElementById(tab_divs[i]).style.display = display;
	}

	document.getElementById('landmark_table_container').setAttribute('style', 'height:' + (new_tab_div_height-21) + 'px;');
	document.getElementById('curve_table_container').setAttribute('style', 'height:' + (new_tab_div_height-21) + 'px;');
	document.getElementById('ruler_table_container').setAttribute('style', 'height:' + (new_tab_div_height-320) + 'px;');

	reloadImage(myImageInit[0].x, myImageInit[0].y, myImageInit[0].w, myImageInit[0].h, myImageInit[0].id, myImageInit[0].src);
	reDrawObjects();
	//loadFrame();
}

function pause(milliseconds) {
	var dt = new Date();
	while ((new Date()) - dt <= milliseconds) { /* Do nothing */ }
}

function printArrayToString(arr){	// For internal alerts

	var i, j;
	var str = '';
	for(i = 0;i < arr.length;i++) {

		if(arr[i].x !== undefined){
			str += arr[i].x + ', ' + arr[i].y + '\n';
			continue;
		}

		for(j = 0;j < arr[i].length;j++) {
			if(j < arr[i].length-1){
				str += arr[i][j] + ', ';
			}else{
				str += arr[i][j];
			}
		}
		str += '\n';
	}

	return str;
}

function proximityCheck(x1,y1,x2,y2,m){
	if(x1 > (parseFloat(x2) - m) && x1 < (parseFloat(x2) + m) && y1 > (parseFloat(y2) - m) && y1 < (parseFloat(y2) + m)) return 1;
	return 0;
}

function readCookie(name) {
	var i;
	var nameEQ = name + "=";
	var ca = document.cookie.split(';');
	for(i = 0;i < ca.length;i++) {
		var c = ca[i];
		while (c.charAt(0)==' ') c = c.substring(1,c.length);
		if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
	}
	return null;
}

function reDrawObjects(){

	var i, j;

	// RE-DRAW LANDMARKS
	for(i = 0;i < landmarks.length;i++) drawMarker('landmark', i, '', landmarks[i].x, landmarks[i].y);

	// RE-DRAW CURVES
	for(i = 0;i < curves.length;i++){
		for(j = 0;j < curves[i].length;j ++){
			drawMarker('curve', i, j, curves[i][j].x, curves[i][j].y, false);
		}
	}

	// RE-DRAW CHECKERBOARD CORNERS
	for(i = 0;i < corners.length;i++) drawMarker('corner', i, '', corners[i].x, corners[i].y);

	// RE-DRAW RULER POINTS
	for(i = 0;i < rulers.length;i++) drawMarker('ruler', i, '', rulers[i].x, rulers[i].y);
	
	// Re-draw Bezier curves
	for(i = 0;i < curves.length;i++) drawBezierCurve(i);
	
	// Re-draw epipolar line
	drawEpipolarLine();
}

function reloadImage(x,y,w,h,id,src){
	
	var image;

	image = svgDocument.getElementById(id);
	if(image == null){
		image = svgDocument.createElementNS(svgns, "image");
		var new_image = true;
	}else{
		//svgDocument.getElementById("world").removeChild(image);
		//image = svgDocument.createElementNS(svgns, "image");
	}

	var new_var = alignSVGImage(w,h);
	var nw = new_var[0];
	var nh = new_var[1];

	image.setAttribute("id", id);
	svgSetXYWH(image, x + margin,y + margin,nw,nh);
	image.setAttribute("style", "opacity:1;");
	image.setAttribute("preserveAspectRatio", "xMinYMin meet");
//	image.setAttribute("image-rendering", "pixelated");
	image.setAttribute("image-rendering", "crisp-edges");
	image.setAttributeNS(XLINK, "href", src);
	image.addEventListener("mousedown", function(evt) { evt.preventDefault(); }, false); // evt.preventDefault(); keeps browser from treating the image as if it is going to be exported out of the window
	image.addEventListener("dblclick", function(evt) { dblClickEvent(evt); }, false);

	photograph.id = id;
	photograph.x = x + margin;
	photograph.y = y + margin;
	photograph.w = photograph.ow = nw;
	photograph.h = photograph.oh = nh;
	photograph.pw = w;
	photograph.ph = h;

	// UPDATE IMAGE ZOOM DISPLAY
	document.getElementById('curr_zoom').innerHTML = Math.round((photograph.w / myImageInit[0].w)*100) + "%";

	if(new_image) svgDocument.getElementById("world").appendChild(image);
}

function resizeImage(img_width,img_height,max_w,max_h){

	var wp = max_w/img_width;
	var hp = max_h/img_height;

	if(wp > hp){
		var nw = Math.round(img_width*hp);
		var nh = Math.round(img_height*hp);
	}else{
		var nw = Math.round(img_width*wp);
		var nh = Math.round(img_height*wp);
	}
	
	return new Array(nw,nh);
}

function saveApplySelection(what){

	// Save current selection
	if(what == 'save'){
		
		// Default no object selected
		current_selection[0] = ['NA', 0, 0];
		
		// Find currently selected object 
		if(current_landmark !== 'NA'){
			current_selection[0] = ['landmark', current_landmark, 0];

		}else if(current_curve !== 'NA'){

			if(init_params.curves_ref == undefined || init_params.curves_ref == '') return;
			if(init_params.landmarks_ref == undefined || init_params.landmarks_ref == '') return;
			
			// Only first and last point correspond between curves
			if(current_curve_point == 0){
				var num = init_params.landmarks_ref.indexOf(init_params.curves_ref[current_curve][1]);
				current_selection[0] = ['landmark', num, 0];
			}		
			if(current_curve_point == curves[current_curve].length-1){
				var num = init_params.landmarks_ref.indexOf(init_params.curves_ref[current_curve][2]);
				current_selection[0] = ['landmark', num, 0];
			}		

		}else if(current_ruler !== 'NA'){
			current_selection[0] = ['ruler', current_ruler, 0];
		}

		// Nothing selected
		if(current_selection[0][0] == 'NA') return;
		
		// Clear selection after saving (having selection active was messing up shape loading on next image load)
		selectObject(current_selection[0][0], current_selection[0][1], current_selection[0][2]);
	}

	// Apply previously saved selection
	if(what == 'apply'){

		if(current_selection[0] == undefined){
			selectObject("NA", 0, 0, true);
			return;
		}
		
		if(current_selection[0][0] == 'NA'){
			selectObject("NA", 0, 0, true);
			return;
		}

		selectObject(current_selection[0][0], current_selection[0][1], current_selection[0][2], true);
	}
}

function scrollEvent(e){

	var id = baseName(e);
	var delta = 0;

	if(IE){
		s_initialX = event.clientX + document.body.scrollLeft;
		s_initialY = event.clientY + document.body.scrollTop;
		if(!event){event = window.event;}
    	var delta = event.wheelDelta / 40;
	}else{
		s_initialX = e.pageX;
		s_initialY = e.pageY;
	}

	if(e.detail){
		var delta = -e.detail*5;
		var max = 8;
		if(delta > max){delta = max;}
		if(delta < -max){delta = -max;}
		imageZoom(Math.round(delta*0.5)/20,s_initialX,s_initialY);
	}else{
		if(browser_name == 'Chrome'){
			var delta = e.wheelDelta;
			var max = 40;
			if(delta > max){delta = max;}
			if(delta < -max){delta = -max;}
			imageZoom(Math.round(delta*0.1)/20,s_initialX,s_initialY);
		}else if(browser_name == 'Safari'){
			var delta = e.wheelDelta;
			var max = 60;
			if(delta > max){delta = max;}
			if(delta < -max){delta = -max;}
			imageZoom(Math.round(delta*0.1)/20,s_initialX,s_initialY);
		}else{
			var delta = e.wheelDelta;
			var max = 60;
			if(delta > max){delta = max;}
			if(delta < -max){delta = -max;}
			imageZoom(Math.round(delta*0.1)/20,s_initialX,s_initialY);
		}
	}
}

function selectObject(type, num1, num2, no_toggle, unselect_all){

	// If input is known to already be selected, then unselect can be true (all other objects unselected are already unselected)
	//alert(type + '; ' + num1 + '; ' + num2 + '; ' + no_toggle);

	// no_toggle false if undefined
	if(no_toggle == undefined) no_toggle = false;
	if(num2 == undefined || num2 == null) num2 = '';
	if(unselect_all == undefined) unselect_all = true;

	var i, j, object_type, point_num;

	if(unselect_all){
		// UNSELECT ALL LANDMARKS
		for(i = 0;i < landmarks.length;i++){
			document.getElementById('landmark_row_' + i).setAttribute('class', 'landmark_table_row');
			changeMarkerStyle('landmark', i, '', landmark_color_blur);
		}

		// UNSELECT ALL CURVES
		for(i = 0;i < curves.length;i++){

			document.getElementById('curve_row_' + i + '_' + 0).setAttribute("class", 'curve_point_row');
			document.getElementById('curve_end_' + i + '_' + 0).setAttribute("class", 'curve_ends');

			changeMarkerStyle('curve', i, 0, control_point_color_blur);
			changeMarkerStyle('curve', i, curves[i].length-1, control_point_color_blur);

			for(j = 1;j < curves[i].length-1;j++){
				document.getElementById('curve_' + i + '_' + j + '_x').setAttribute('class', 'semilandmark_point_col');
				document.getElementById('curve_' + i + '_' + j + '_y').setAttribute('class', 'semilandmark_point_col');
				changeMarkerStyle('curve', i, j, control_point_color_blur);
			}

			document.getElementById('curve_row_' + i + '_' + (curves[i].length-1)).setAttribute("class", 'curve_point_row');
			document.getElementById('curve_end_' + i + '_' + (curves[i].length-1)).setAttribute("class", 'curve_ends');
		}

		// UNSELECT ALL RULER POINTS
		for(i = 0;i < rulers.length;i++){
			document.getElementById('ruler_row_' + i).setAttribute('class', 'scaling_table_row');
			changeMarkerStyle('ruler', i, '', landmark_color_blur);
		}
	}

	// DETERMINE OBJECT TYPE
	object_type = type;
	if(type == 'landmark'){
		for(i = 0;i < curve_also_landmark.length;i++){
			if(curve_also_landmark[i].start == num1){
				object_type = 'curve and landmark';
				type = 'curve start';
				num1 = i;
				num2 = 0;
				break;
			}
			if(curve_also_landmark[i].end == num1){
				object_type = 'curve and landmark';
				type = 'curve end';
				num1 = i;
				num2 = curves[i].length-1;
				break;
			}
		}
	}else if(type == 'curve'){
		// If curve point is start or end, make curve and landmark to ensure that landmark gets selected too
		if(num2 == 0){
			object_type = 'curve and landmark';
			type = 'curve start';
		}
		if(num2 == curves[num1].length-1){
			object_type = 'curve and landmark';
			type = 'curve end';
		}
	}

	var unset_all = false;
	if(!no_toggle){
		if(type == 'landmark' && num1 == current_landmark) unset_all = true;
		if(type == 'curve' && num1 == current_curve && num2 == current_curve_point) unset_all = true;
		if(type == 'curve start' && num1 == current_curve && num2 == current_curve_point) unset_all = true;
		if(type == 'curve end' && num1 == current_curve && num2 == current_curve_point) unset_all = true;
		if(type == 'ruler' && num1 == current_ruler) unset_all = true;
	}

	// UNSET CURRENT CURVES AND LANDMARKS
	current_landmark = "NA";
	current_curve = "NA";
	current_curve_point = "NA";
	current_ruler = "NA";
	document.getElementById('curr_object').innerHTML = 'None selected';

	if(unset_all) object_type = 'none';

	// Update epipolar line
	if(document.getElementById('show_epipolar_line').checked && unselect_all) updateEpipolarLine(object_type, num1, num2);

	if(unset_all || type == "NA") return;

	// LANDMARK ONLY
	if(object_type == 'landmark'){

		current_landmark = num1;
		document.getElementById('landmark_row_' + num1).setAttribute('class', 'landmark_table_row_select');
		changeMarkerStyle('landmark', num1, '', landmark_color_focus);
		document.getElementById('curr_object').innerHTML = init_params.landmarks_ref[num1];
		current_selection[0] = ['landmark', num1, 0];

		return;
	}

	// CURVE POINT
	if(object_type == 'curve'){

		current_curve = num1;
		current_curve_point = num2;
		
		// If curve point number exceeds maximum, return
		if(current_curve_point > curves[current_curve].length-1) return;

		//document.getElementById('curve_row_' + num1 + '_' + num2).setAttribute("class", 'semilandmark_point_row_select');
		document.getElementById('curve_' + num1 + '_' + num2 + '_x').setAttribute('class', 'semilandmark_point_col_select');
		document.getElementById('curve_' + num1 + '_' + num2 + '_y').setAttribute('class', 'semilandmark_point_col_select');
		changeMarkerStyle('curve', num1, num2, control_point_color_focus);
		document.getElementById('curr_object').innerHTML = init_params.curves_ref[num1][0] + ' curve point ' + num2;
		current_selection[0] = ['curve', num1, num2];

		return;
	}

	// LANDMARK AND CURVE
	if(object_type == 'curve and landmark'){

		current_curve = num1;
		current_curve_point = num2;

		if(type == 'curve start'){

			if(document.getElementById('landmark_row_' + curve_also_landmark[num1].start))
				document.getElementById('landmark_row_' + curve_also_landmark[num1].start).setAttribute('class', 'landmark_table_row_select');
			
			// SELECT ALL DUPLICATES
			for(i = 0;i < start_dupl[num1].type.length;i++){
				
				if(start_dupl[num1].type[i] == 'start'){point_num = 0;}else{point_num = curves[start_dupl[num1].num[i]].length-1;}
				
				document.getElementById('curve_row_' + start_dupl[num1].num[i] + '_' + point_num).setAttribute("class", 'curve_point_row_select');
				document.getElementById('curve_end_' + start_dupl[num1].num[i] + '_' + point_num).setAttribute("class", 'curve_ends_select');
				changeMarkerStyle('curve', start_dupl[num1].num[i], point_num, control_point_color_focus);
			}

			document.getElementById('curr_object').innerHTML = init_params.curves_ref[num1][1];
		}

		if(type == 'curve end'){

			if(document.getElementById('landmark_row_' + curve_also_landmark[num1].end))
				document.getElementById('landmark_row_' + curve_also_landmark[num1].end).setAttribute('class', 'landmark_table_row_select');

			// SELECT ALL DUPLICATES
			for(i = 0;i < end_dupl[num1].type.length;i++){

				if(end_dupl[num1].type[i] == 'start'){point_num = 0;}else{point_num = curves[end_dupl[num1].num[i]].length-1;}

				document.getElementById('curve_row_' + end_dupl[num1].num[i] + '_' + point_num).setAttribute("class", 'curve_point_row_select');
				document.getElementById('curve_end_' + end_dupl[num1].num[i] + '_' + point_num).setAttribute("class", 'curve_ends_select');
				changeMarkerStyle('curve', end_dupl[num1].num[i], point_num, control_point_color_focus);			
			}

			document.getElementById('curr_object').innerHTML = init_params.curves_ref[num1][2];
		}

		current_selection[0] = ['curve and landmark', num1, num2];

		return;
	}

	if(object_type == 'ruler'){

		current_ruler = num1;
		document.getElementById('ruler_row_' + num1).setAttribute('class', 'scaling_table_row_select');
		changeMarkerStyle('ruler', num1, '', landmark_color_focus);
		document.getElementById('curr_object').innerHTML = 'Ruler point ' + (num1+1);
		current_selection[0] = ['ruler', num1, 0];

		return;
	}
}

function selectStyle(tag, style_id, style_value, style_off){
	if(!tag){return;}
	var i;
	for(i = 0;i < tag.parentNode.childNodes.length;i++){
		if(tag.parentNode.childNodes[i].tagName == tag.tagName){
			if(style_id == 'bold'){tag.parentNode.childNodes[i].style.fontWeight = 'normal';}
			if(style_id == 'backgroundColor'){tag.parentNode.childNodes[i].style.backgroundColor = '';}
		}
	}

	if(style_id == 'bold'){tag.style.fontWeight = 'bold';}
	if(style_id == 'backgroundColor'){tag.style.backgroundColor = style_value;}
}

function setButtonStyle(id, on_off){

	var button = document.getElementById(id);

	if(on_off == 'off'){
		button.style.background = '#eeeeee';
		button.style.border = '1px solid #eeeeee';
		button.style.color = '#cccccc';
	}else{
		button.style.background = 'orange';
		button.style.border = '1px solid orange';
		button.style.color = 'black';
	}
}

function setEvent(eventin){

	currentEvent = eventin;

	if (eventin != "nothing"){
		doEvent();
	}
}

function setEvents(e) {
	var eventHandler = detectKeyEvent;
	document['on'+this.id] = eventHandler;
}

function setSettings(name){

	var i, radios, checkboxes, length;
	
	// UPDATE FORM ELEMENTS ON PAGE TO MATCH COOKIE VALUES
	if(!name){

		checkboxes = document.getElementsByName('settings');
		for(i=0, length=checkboxes.length; i < length; i++) if(getCookie(checkboxes[i].id) == 'T') checkboxes[i].checked = true;

		radios = document.getElementsByName('copy_landmarks');
		for(i=0, length=radios.length; i < length; i++) if(getCookie(radios[i].id) == 'T') radios[i].checked = true;

		radios = document.getElementsByName('copy_curves');
		for(i=0, length=radios.length; i < length; i++) if(getCookie(radios[i].id) == 'T') radios[i].checked = true;

		auto_advance = document.getElementById('auto_advance_checkbox').checked;

		return;
	}
	
	// BLUR RADIO/CHECKBOX SO THAT KEY PRESSES DO NOT CHANGE SETTING WHILE INTERACTING WITH IMAGE
	document.getElementById(name.id).blur();

	// IF RADIO BUTTON, SET ALL COOKIE VALUES TO FALSE (DEFAULT) BEFORE SETTING TRUE VALUE
	if(name.type == 'radio'){
		radios = document.getElementsByName(name.name);
		for(i=0, length=radios.length; i < length; i++) createCookie(radios[i].id, 'F');
	}

	// UPDATE COOKIE VALUES TO MATCH USER SELECTIONS
	//if(name.id == 'copy_landmarks_checkbox' || name.id == 'copy_curves_checkbox' || name.id == 'copy_ruler_points_checkbox' || name.id == 'copy_scaling_checkbox' || name.id == 'auto_advance_checkbox'){
	
	// SET VALUE
	if(document.getElementById(name.id).checked){value = 'T'}else{value = 'F'}

	// SAVE COOKIE BY DEFAULT
	var create_cookie = true;

	// OPTIONS FOR WHICH COOKIE SHOULD NOT BE SAVED
	if(name.id == 'save_on_advance') create_cookie = false;

	// CREATE COOKIE
	if(create_cookie) createCookie(name.id, value)

	if(name.id == 'auto_advance_checkbox') auto_advance = document.getElementById('auto_advance_checkbox').checked;
}

function submit(submit_params){

	// ADD FROM BROWSER TAG TO DISTINGUISH FROM INITIAL RUN
	submit_params.fromBrowser = true;

	// ADD SECONDS SO THAT SERVER RECOGNIZES NEW INPUT
	var seconds = new Date().getTime() / 1000;
	submit_params.time = seconds;

	// INCREASE SUBMIT COUNT
	submit_ct++;
	submit_params.submit_ct = submit_ct;

	// MAKE OBJECT INTO JSON STRING
	json_string = JSON.stringify(submit_params);

	//document.getElementById('text_output').innerHTML = "";

	// CHANGE VALUE OF INPUT FIELD
	document.getElementById('text_input').value = json_string;
	$("#text_input").change();

	if(!submit_params.exit) submitCaptureOutput();

	//if(!submit_params.exit) updateStatus();
}

function submitCaptureOutput(){

	var iter = 0;
	var server_out;
	var wh_ratio;
	var wh_ratio_server;

	var updateInterval = setInterval(function(){

		//var seconds = new Date().getTime() / 1000;
		//document.getElementById('current_process').innerHTML = seconds;

		// READ JSON STRING
		if(document.getElementById('text_output').innerHTML !== '') server_out = JSON.parse(document.getElementById('text_output').innerHTML);
		
		// CHECK TO MAKE SURE SUBMIT CAPTURE IS FROM MOST RECENT SUBMISSION
		if(server_out !== undefined && submit_ct == server_out.submit_ct){

			clearInterval(updateInterval);

			//alert(submit_ct + ' : ' + server_out.submit_ct);
			//alert(server_out.corners);
			
			// UPDATE CURRENT STATUS
			//alert(server_out.update_status);
			//document.getElementById('current_process').innerHTML = document.getElementById('text_output').innerHTML;
			document.getElementById('current_process').innerHTML = server_out.update_status;
			setTimeout(function(){document.getElementById('current_process').innerHTML = ''}, 7000);

			// Load new image
			if(server_out.change_image !== undefined) loadNewImage(server_out);

			// Load new shapes
			if(server_out.load_shapes_from_file !== undefined) loadShapesFromFile(server_out);

			// Load new ruler interval
			if(server_out.load_new_ruler_interval !== undefined){
				if(!isNaN(server_out.ruler_pixel)) ruler_interval_pixels = Number(server_out.ruler_pixel)
				updateScaling();
			}

			// Load new checkerboard corners
			if(server_out.load_new_corners !== undefined){
				clearCheckerboardCorners();
				drawCheckerboardCorners(server_out.corners);
				if(!isNaN(server_out.checker_square_pixel)) checker_square_pixel = Number(server_out.checker_square_pixel);
				updateScaling();
			}

			// Resize window elements
			if(server_out.change_image !== undefined) onWindowResize();

			// Get image WH ratio
			wh_ratio = myImageInit[0].w / myImageInit[0].h

			// Copy image zoom and position
			if(server_out.zoom_d !== undefined){

				// Get server out ratio
				wh_ratio_server = Math.round((Number(server_out.photograph_w) / Number(server_out.photograph_h))*100) / 100;
				//alert(wh_ratio + ', ' + wh_ratio_server)
				
				if(wh_ratio == wh_ratio_server){
					var image = svgDocument.getElementById(photograph.id);
					image.setAttribute("width", Number(server_out.photograph_w));
					image.setAttribute("height", Number(server_out.photograph_h));
					image.setAttribute("x", Number(server_out.photograph_x));
					image.setAttribute("y", Number(server_out.photograph_y));
					photograph.x = Number(server_out.photograph_x);
					photograph.y = Number(server_out.photograph_y);
					photograph.w = Number(server_out.photograph_w);
					photograph.h = Number(server_out.photograph_h);
					zoom_d = Number(server_out.zoom_d);

					reDrawObjects();
					//loadFrame();
				}

			}else{
				//reloadImage(myImageInit[0].x, myImageInit[0].y, myImageInit[0].w, myImageInit[0].h, myImageInit[0].id, myImageInit[0].src);
			}
			
			//
			if(server_out.find_epipolar_line !== undefined){

				epipolar_slope = server_out.slope;
				epipolar_intercept = server_out.intercept;
				epipolar_cubic = server_out.cubic;
				drawEpipolarLine();

				//loadFrame();
			}
			
		}

		iter++;
		if(iter > 3000) clearInterval(updateInterval);
	}, 100);
}

function submitShapes(submit_params, add_to){

	var i, j;
	if(submit_params == undefined) var submit_params = {};

	// Copy previous working directory
	submit_params.prev_wd = init_params.prev_wd;

	// Copy scaling parameters to JSON
	if(!isNaN(ruler_interval_world)) submit_params.ruler_interval_world = ruler_interval_world;
	if(!isNaN(ruler_interval_pixels)) submit_params.ruler_interval_pixels = ruler_interval_pixels;
	if(!isNaN(checkerboard_nx)) submit_params.checkerboard_nx = checkerboard_nx;
	if(!isNaN(checkerboard_ny)) submit_params.checkerboard_ny = checkerboard_ny;
	if(!isNaN(checker_square_pixel)) submit_params.checker_square_pixel = checker_square_pixel;
	if(!isNaN(checker_square_world)) submit_params.checker_square_world = checker_square_world;
	submit_params.scaling = scaling_wpp;
	submit_params.scaling_units = scaling_units;

	// COPY RULER POINTS TO JSON STRUCTURE
	submit_params.ruler_points = [];
	for(var i = 0;i < rulers.length;i++) submit_params.ruler_points[i] = [rulers[i].x, rulers[i].y];

	// COPY CHECKERBOARD CORNERS TO JSON STRUCTURE
	submit_params.corners = [];
	for(var i = 0;i < corners.length;i++) submit_params.corners[i] = [corners[i].x, corners[i].y];

	// Copy landmarks to JSON
	var landmark_table = document.getElementById('landmark_table_container');

	// Read landmark table - the landmarks array does not contain landmarks that are also curves
	j = 0;
	submit_params.landmarks = [];
	for(i = 0;i < landmark_table.childNodes.length;i++){
		if(landmark_table.childNodes[i].childNodes[1].innerHTML == '-') continue;

		submit_params.landmarks[j] = [
			landmark_table.childNodes[i].childNodes[0].innerHTML, 
			landmark_table.childNodes[i].childNodes[1].innerHTML,
			landmark_table.childNodes[i].childNodes[2].innerHTML];
		j++;
	}

	// Read curves
	var m = 0;
 
	if(init_params.curves_ref !== undefined && init_params.curves_ref[0][0] !== undefined){

		submit_params.control_points = [];
		for(i = 0;i < init_params.curves_ref.length;i++){

			// Skip if first or last curve point is null
			if(curves[i][0].x == '-' || curves[i][(curves[i].length-1)].x == '-') continue;

			// Start array
			read_curve_points = new Array();

			// Add curve name
			read_curve_points[read_curve_points.length] = init_params.curves_ref[i][0];

			// Add curve points
			for(j = 0;j < curves[i].length;j++){
				if(curves[i][j].x == '-') continue;
				read_curve_points[read_curve_points.length] = curves[i][j].x;
				read_curve_points[read_curve_points.length] = curves[i][j].y;
			}

			submit_params.control_points[m] = read_curve_points;
			m++;
		}
 	}

	if(document.getElementById('do_not_save_curve_points').checked){
		submit_params.save_curve_points = false;
	}else{
		submit_params.save_curve_points = true;
	}

	// Set shape save as locations
	submit_params.save_as_shapes = current_save_shapes;
	submit_params.save_as_landmarks = current_save_landmarks;
	submit_params.save_as_control_points = current_save_control_points;
	submit_params.save_as_curve_points = current_save_curve_points;

	submit_params.save_shapes = true;

	unsaved_curves = false;
	unsaved_landmarks = false;
	unsaved_ruler_points = false;
	unsaved_corners = false;
	
	if(add_to == undefined){
		submit(submit_params);
	}else{
		return submit_params;
	}
}

function svgSetXYWH(el, x, y, w, h) {
	el.setAttribute("x", x);
	el.setAttribute("y", y);
	el.setAttribute("width", w);
	el.setAttribute("height", h);
}

function updateCurveTable(curve_num){
	
	var i, j, new_length, point_num;
	
	for(i = 0; i < curves.length; i++){

		if(curve_num !== undefined && i !== curve_num) continue;

		// Check whether number of curve point fields matches
		var curve_block_div = document.getElementById('curve_block_' + i);
		
		if(curve_block_div.childNodes.length < curves[i].length-2){

			//alert(curve_block_div.childNodes.length)
			//alert(curves[i].length-2)
			new_length = curves[i].length-1;

			// Increase ID and link of last curve point
			var curve_end = document.getElementById('curve_end_' + i + '_' + (new_length-1));
			curve_end.id = 'curve_end_' + i + '_' + new_length;
			curve_end.setAttribute('onclick', 'javascript:selectObject(\'curve\', ' + i + ',' + new_length + ');');
			var curve_row = document.getElementById('curve_row_' + i + '_' + (new_length-1));
			curve_row.id = 'curve_row_' + i + '_' + new_length;
			var curve_point_x = document.getElementById('curve_' + i + '_' + (new_length-1) + '_x');
			curve_point_x.id = 'curve_' + i + '_' + new_length + '_x';
			var curve_point_y = document.getElementById('curve_' + i + '_' + (new_length-1) + '_y');
			curve_point_y.id = 'curve_' + i + '_' + new_length + '_y';

			// Add curve point fields			
			for(j = curve_block_div.childNodes.length+1; j < curves[i].length-1; j++){
			
				var curve_row = document.createElement('div');
				curve_row.id = 'curve_row_' + i + '_' + j;
				curve_row.setAttribute('class', 'semilandmark_point_row');

				var curve_point_x = document.createElement('div');
				curve_point_x.id = 'curve_' + i + '_' + j + '_x';
				curve_point_x.innerHTML = '-';
				curve_point_x.title = 'Click to select curve point';
				curve_point_x.setAttribute('class', 'semilandmark_point_col');
				curve_point_x.setAttribute('onclick', 'javascript:selectObject(\'curve\', ' + i + ',' + j + ');');

				var curve_point_y = document.createElement('div');
				curve_point_y.id = 'curve_' + i + '_' + j + '_y';
				curve_point_y.innerHTML = '-';
				curve_point_y.title = 'Click to select curve point';
				curve_point_y.setAttribute('class', 'semilandmark_point_col');
				curve_point_y.setAttribute('onclick', 'javascript:selectObject(\'curve\', ' + i + ',' + j + ');');

				curve_row.appendChild(curve_point_x);
				curve_row.appendChild(curve_point_y);
				curve_block_div.appendChild(curve_row);
			}
		}

		if(curve_block_div.childNodes.length > curves[i].length-2){

			var current_length = curve_block_div.childNodes.length+1;
			for(j = 2; j < current_length; j++){
				var curve_row = document.getElementById('curve_row_' + i + '_' + j);
				curve_block_div.removeChild(curve_row);
			}
			
			// Remove curve add point if present
			var curve_add_point = document.getElementById('curve_add_point_' + i + '_' + 1);
			if(curve_add_point) document.getElementById('curve_row_' + i + '_' + 1).removeChild(curve_add_point);

			// Change ID and link of last curve point to 2
			var curve_end = document.getElementById('curve_end_' + i + '_' + current_length);
			curve_end.id = 'curve_end_' + i + '_2';
			curve_end.setAttribute('onclick', 'javascript:selectObject(\'curve\', ' + i + ',' + 2 + ');');
			var curve_row = document.getElementById('curve_row_' + i + '_' + current_length);
			curve_row.id = 'curve_row_' + i + '_2';
			var curve_point_x = document.getElementById('curve_' + i + '_' + current_length + '_x');
			curve_point_x.id = 'curve_' + i + '_2_x';
			var curve_point_y = document.getElementById('curve_' + i + '_' + current_length + '_y');
			curve_point_y.id = 'curve_' + i + '_2_y';
		}

		// Check that there are curve add point divs between curve points
		if(curves[i].length > 3){

			for(j = 1; j < curves[i].length-2; j++){
			
				var curve_add_point = document.getElementById('curve_add_point_' + i + '_' + j);

				if(curve_add_point) continue;

				var curve_add_point = document.createElement('div');
				curve_add_point.className = 'curve_add_point';
				curve_add_point.id = 'curve_add_point_' + i + '_' + j;
				curve_add_point.title = 'Click to insert curve point field below';
				curve_add_point.innerHTML = '+';
				curve_add_point.setAttribute("onclick", "addCurvePointField(" + i + "," + j + ");");

				document.getElementById('curve_row_' + i + '_' + j).appendChild(curve_add_point);

				//alert(curve_add_point)
//				if(!curve_add_point) alert(curve_add_point.id)

			}
		}

		// Update values
		for(j = 0; j < curves[i].length-1; j++){

			var curve_y = document.getElementById('curve_' + i + '_' + j + '_y');
			var curve_x = document.getElementById('curve_' + i + '_' + j + '_x');
			curve_y.innerHTML = curves[i][j].y;
			curve_x.innerHTML = curves[i][j].x;
		}
	}
}

function updateEpipolarLine(type, num1, num2){

	if(type == undefined && !document.getElementById('show_epipolar_line').checked){
		clearEpipolarLine();
		epipolar_slope = "NA";
		epipolar_intercept = "NA";
		epipolar_cubic = "NA";
		return;
	}

	// Set currently selected object
	if(type == undefined && document.getElementById('show_epipolar_line').checked){
		type = current_selection[0][0];
		num1 = current_selection[0][1];
		num2 = current_selection[0][2];
	}

	// Required inputs for finding epipolar line
	if(!stereo_input) return;
	if(current_save_shapes == undefined) return;

	// If neither a curve nor landmark clear epipolar line, includes no selection and intermediate curve point
	if(type !== 'curve and landmark' && type !== 'landmark'){
		clearEpipolarLine();
		epipolar_slope = "NA";
		epipolar_intercept = "NA";
		epipolar_cubic = "NA";
		return;
	}
	
	// If intermediate curve point, clear epipolar line
	if(type == 'curve and landmark' && (num2 > 0 && num2 < curves[num1].length-1)){
		clearEpipolarLine();
		epipolar_slope = "NA";
		epipolar_intercept = "NA";
		epipolar_cubic = "NA";
		return;
	}

	var submit_params = {find_epipolar_line:1}
	submit_params.prev_wd = init_params.prev_wd;

	// Make array of shape files
	shape_files = new Array();
	for(var i = 0; i < view_names.length; i++) shape_files[i] = init_params.shapes_fpaths[current_aspect][i];

	submit_params.current_view = current_view + 1;
	submit_params.all_views = shape_files;
	submit_params.shape_type = 'landmarks.pixel';
	submit_params.undistort_params = init_params.undistort_params;
	submit_params.distort_params = init_params.distort_params;
	submit_params.img_size = init_params.img_size;
	submit_params.cal_coeffs = init_params.cal_coeffs;
	
	if(type == 'landmark'){
		submit_params.landmark_name = init_params.landmarks_ref[num1];
	}
	if(type == 'curve and landmark'){
		if(num2 == 0) submit_params.landmark_name = init_params.curves_ref[num1][1];
		if(num2 == curves[num1].length-1) submit_params.landmark_name = init_params.curves_ref[num1][2];
	}
	
	submit(submit_params);
}

function updateScaling(){

	var units_text_ppw, units_text_wpp;

	// GET NUMBERS FROM CHECKERBOARD CORNER DIMENSIONS
	internal_corner_dim = getNumbers(document.getElementById('internal_corner_dim').value);

	if(internal_corner_dim == null){
		checkerboard_nx = "NA";
		checkerboard_ny = "NA";
	}else{
		checkerboard_nx = Number(internal_corner_dim[0]);
		checkerboard_ny = Number(internal_corner_dim[1]);
		if(internal_corner_dim[1] == null) checkerboard_ny = "NA"
	}

	// DISPLAY CHECKERBOARD SQUARE SIZE
	if(isNaN(checker_square_pixel)){
		document.getElementById('checker_square_pixel').innerHTML = "";
	}else{
		document.getElementById('checker_square_pixel').innerHTML = Math.round(checker_square_pixel*10000, 4)/10000 + ' px';
	}

	// DISPLAY RULER INTERVAL
	if(isNaN(ruler_interval_pixels)){
		document.getElementById('ruler_interval_pixels').innerHTML = "";
	}else{
		document.getElementById('ruler_interval_pixels').innerHTML = Math.round(ruler_interval_pixels*10000, 4)/10000 + ' px';
	}

	// GET NUMBERS FROM RULER WORLD INTERVAL
	ruler_interval_input = getNumbers(document.getElementById('ruler_interval_world').value);

	// TAKE FIRST ELEMENT FROM REGULAR EXPRESSION MATCH
	if(ruler_interval_input == null){ruler_interval_world = "NA"}else{ruler_interval_world = Number(ruler_interval_input[0]);}

	// GET LETTERS FROM RULER WORLD INTERVAL
	ruler_units_input = getLetters(document.getElementById('ruler_interval_world').value);
	if(ruler_units_input == null){scaling_units = "NA";}else{scaling_units = ruler_units_input[0];}


	// GET NUMBERS FROM CHECKER WORLD INTERVAL
	checker_square_input = getNumbers(document.getElementById('checker_square_world').value);

	// TAKE FIRST ELEMENT FROM REGULAR EXPRESSION MATCH
	if(checker_square_input == null){checker_square_world = "NA"}else{checker_square_world = Number(checker_square_input[0]);}

	// GET LETTERS FROM RULER WORLD INTERVAL
	if(ruler_units_input == null){
		checker_square_input = getLetters(document.getElementById('checker_square_world').value);
		if(checker_square_input == null){scaling_units = "NA";}else{scaling_units = checker_square_input[0];}
	}

	// FIND SCALING RATIOS
	if(!isNaN(ruler_interval_pixels) && !isNaN(ruler_interval_world)){
		scaling_ppw = ruler_interval_pixels / ruler_interval_world;
		scaling_wpp = ruler_interval_world / ruler_interval_pixels;
		scaling_wpp_updated = true;
	}else{

		if(!isNaN(checker_square_pixel) && !isNaN(checker_square_world)){
			scaling_ppw = checker_square_pixel / checker_square_world;
			scaling_wpp = checker_square_world / checker_square_pixel;
			scaling_wpp_updated = true;
		}
	}
	
	if(scaling_wpp_updated && (isNaN(ruler_interval_pixels) || isNaN(ruler_interval_world)) && (isNaN(checker_square_pixel) || isNaN(checker_square_world))){
		scaling_wpp = "NA";
		scaling_ppw = "NA";
		document.getElementById('ruler_interval_pixels').innerHTML = "";
	}

	if(!isNaN(scaling_wpp)){

		if(scaling_units == 'NA'){
			units_text_ppw = '';
			units_text_wpp = '';
		}else{
			units_text_ppw = ' px/' + scaling_units;
			units_text_wpp = ' ' + scaling_units + '/px';
		}

		// UPDATE INTERFACE
		document.getElementById('scaling_ppw').innerHTML = Math.round(scaling_ppw*10000, 4)/10000 + units_text_ppw;
		document.getElementById('scaling_wpp').innerHTML = Math.round(scaling_wpp*10000, 4)/10000 + units_text_wpp;
	}else{
		document.getElementById('scaling_ppw').innerHTML = "";
		document.getElementById('scaling_wpp').innerHTML = "";
	}

	if(!isNaN(ruler_interval_pixels) && !isNaN(ruler_interval_world) && !isNaN(checker_square_pixel)){

		checker_square_world = checker_square_pixel * (ruler_interval_world / ruler_interval_pixels);

		// IF BOTH RULER AND CHECKERBOARD ARE FOUND, REPORT CHECKERBOARD SIZE IN UNITS
		document.getElementById('checker_square_world').value = Math.round(checker_square_world*10000, 4)/10000;

		// ADD UNITS
		if(scaling_units !== 'NA') document.getElementById('checker_square_world').value += ' ' + scaling_units;
	}
}

function updateStatus(){

	iter = 0;
	var updateInterval = setInterval(function(){
		if(document.getElementById('text_output').innerHTML !== ''){updateStatusPrint();clearInterval(updateInterval);}
		iter++;
		if(iter > 100) clearInterval(updateInterval);
	}, 100);
}

function updateStatusPrint(){
	document.getElementById('current_process').innerHTML = document.getElementById('text_output').innerHTML;
	setTimeout(function(){document.getElementById('current_process').innerHTML = ''}, 7000);
}
