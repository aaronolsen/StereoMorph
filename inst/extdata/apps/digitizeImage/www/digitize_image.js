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
var current_landmark = "NA";
var current_curve_point = "NA";
var current_curve_point_type = "NA";
var current_ruler = "NA";
var curve_also_landmark = new Array();
var curve_blocks = new Array();
var curve_end = new Array();
var curve_points = new Array();
var curve_start = new Array();
var disp_order_cp = new Array();
var disp_order_curve = new Array();
var end_dupl = new Array();
var find_ruler_interval_run_after = "NA";
var frame_color = '#FFF3CE';
var globalkeypress = '';
var IE = document.all ? true : false
var initiate = 0;
var initialX;
var initialY;
var js_ct = 0;
var landmarks = new Array();
var last_p_pos;
var margin = 0;
var mouse_move_in_prox = 0;
var mousedown;
var overlay_photograph_id = 0;
var photograph;
var p_pos;
var ruler_interval_pixels = "NA";
var ruler_interval_world = "NA";
var rulers = new Array();
var scaling_ppw = "NA";
var scaling_units = "NA";
var scaling_wpp = "NA";
var server_out;
var start_dupl = new Array();
var submit_ct = 0;
var svgns = "http://www.w3.org/2000/svg";
var tempX = 0;
var tempY = 0;
var window_properties = new Array();
var zoom_d = 1;

function addCurvePoint(x,y){
	if(current_curve_point == "NA"){return;}

	unsaved_curves = 'TRUE';

	var i;

	var type = current_curve_point_type;
	var num = current_curve_point;

	if(type == 'point'){
		var cp_x_div = document.getElementById('curve_' + type + '_x_' + num);
		var cp_y_div = document.getElementById('curve_' + type + '_y_' + num);
	
		if(cp_x_div.innerHTML == "-"){var new_curve = 1;}else{var new_curve = 0;}
		cp_x_div.innerHTML = x;
		cp_y_div.innerHTML = y;
	
		curve_points[num].x = x;
		curve_points[num].y = y;
	}else{
		if(type == 'start'){

			if(curve_also_landmark[num].start >= 0) document.getElementById('landmark_col_x_' + curve_also_landmark[num].start).innerHTML = x;
			if(curve_also_landmark[num].start >= 0) document.getElementById('landmark_col_y_' + curve_also_landmark[num].start).innerHTML = y;

			for(i = 0;i < start_dupl[num].type.length;i++){
				var cp_x_div = document.getElementById('curve_' + start_dupl[num].type[i] + '_x_' + start_dupl[num].num[i]);
				var cp_y_div = document.getElementById('curve_' + start_dupl[num].type[i] + '_y_' + start_dupl[num].num[i]);
	
				if(cp_x_div.innerHTML == "-"){var new_curve = 1;}else{var new_curve = 0;}
				cp_x_div.innerHTML = x;
				cp_y_div.innerHTML = y;
				
				if(start_dupl[num].type[i] == 'start'){
					curve_start[start_dupl[num].num[i]].x = x;
					curve_start[start_dupl[num].num[i]].y = y;
				}else{
					curve_end[start_dupl[num].num[i]].x = x;
					curve_end[start_dupl[num].num[i]].y = y;
				}

				drawMarker('curve_' + start_dupl[num].type[i], start_dupl[num].num[i], x, y);
				selectObject('curve_start', num);
				selectObject('curve_start', num);
			}
		}else{

			if(curve_also_landmark[num].end >= 0) document.getElementById('landmark_col_x_' + curve_also_landmark[num].end).innerHTML = x;
			if(curve_also_landmark[num].end >= 0) document.getElementById('landmark_col_y_' + curve_also_landmark[num].end).innerHTML = y;

			for(i = 0;i < end_dupl[num].type.length;i++){
				var cp_x_div = document.getElementById('curve_' + end_dupl[num].type[i] + '_x_' + end_dupl[num].num[i]);
				var cp_y_div = document.getElementById('curve_' + end_dupl[num].type[i] + '_y_' + end_dupl[num].num[i]);
	
				if(cp_x_div.innerHTML == "-"){var new_curve = 1;}else{var new_curve = 0;}
				cp_x_div.innerHTML = x;
				cp_y_div.innerHTML = y;

				if(end_dupl[num].type[i] == 'start'){
					curve_start[end_dupl[num].num[i]].x = x;
					curve_start[end_dupl[num].num[i]].y = y;
				}else{
					curve_end[end_dupl[num].num[i]].x = x;
					curve_end[end_dupl[num].num[i]].y = y;
				}

				drawMarker('curve_' + end_dupl[num].type[i], end_dupl[num].num[i], x, y);
				selectObject('curve_end', num);
				selectObject('curve_end', num);
			}
		}
		return;
	}
	
	drawMarker('curve_' + type, num, x, y);

	if(new_curve && current_curve_point_type == 'point'){
		selectObject('curve_point', num);
		selectObject('curve_point', num);
		if(curve_blocks[curve_points[num].parent][curve_blocks[curve_points[num].parent].length-1] == num){
			addCurvePointField(curve_points[num].parent, curve_blocks[curve_points[num].parent].length);
		}
	}
}

function addCurvePointField(block_num, point_num){

	var i, j;

	var num = curve_points.length;
	var curve_block_div = document.getElementById('curve_block_' + block_num);

	var curve_row = document.createElement('div');
	curve_row.id = 'curve_row_' + num;
	curve_row.setAttribute('class', 'semilandmark_point_row');
	curve_row.setAttribute('onclick', 'javascript:selectObject(\'curve_point\', ' + num + ');');

	var curve_point_x = document.createElement('div');
	curve_point_x.id = 'curve_point_x_' + num;
	curve_point_x.innerHTML = '-';
	curve_point_x.setAttribute('class', 'semilandmark_point_col');

	var curve_point_y = document.createElement('div');
	curve_point_y.id = 'curve_point_y_' + num;
	curve_point_y.innerHTML = '-';
	curve_point_y.setAttribute('class', 'semilandmark_point_col');

	curve_row.appendChild(curve_point_y);
	curve_row.appendChild(curve_point_x);
	curve_block_div.appendChild(curve_row);
	
	curve_points[num] = new Array();
	curve_points[num].x = parseFloat('-');
	curve_points[num].y = parseFloat('-');
	curve_points[num].parent = block_num;
	curve_blocks[block_num][point_num] = num;

	var m = 0;
	for(i = 0;i < curve_blocks.length; i++){
		for(j = 0;j < curve_blocks[i].length; j++){
			disp_order_curve[m] = curve_blocks[i][j];
			m++;
		}
	}
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

function addObject(x, y){

	if(isNaN(x)){
		x = '-';
		y = '-';
	}

	// ADD LANDMARK
	if(current_landmark !== "NA"){

		unsaved_landmarks = 'TRUE';

		var num = current_landmark;
		var lm_x_div = document.getElementById('landmark_col_x_' + num);
		var lm_y_div = document.getElementById('landmark_col_y_' + num);

		if(lm_x_div.innerHTML == "-"){var new_lm = 1;}else{var new_lm = 0;}
		lm_x_div.innerHTML = x;
		lm_y_div.innerHTML = y;

		landmarks[num].x = x;
		landmarks[num].y = y;
		drawMarker('landmark', num,x,y);
	
		if(new_lm){
			selectObject('landmark', num);
			selectObject('landmark', num);
		}

		return;
	}

	// ADD RULER POINT
	if(current_ruler !== "NA"){

		unsaved_ruler_points = 'TRUE';

		// FIND RULER INTERVAL ONE SECOND AFTER LAST MOVE OF ANY RULER POINT
		find_ruler_interval_run_after = new Date().getTime() + 999;
		setTimeout(function(){findRulerInterval()}, 1000);

		var num = current_ruler;
		var ruler_x_div = document.getElementById('ruler_col_x_' + num);
		var ruler_y_div = document.getElementById('ruler_col_y_' + num);

		if(ruler_x_div.innerHTML == "-" && x !== "-"){var new_ruler = 1;}else{var new_ruler = 0;}
		ruler_x_div.innerHTML = x;
		ruler_y_div.innerHTML = y;

		rulers[num].x = x;
		rulers[num].y = y;
		drawMarker('ruler', num, x, y);

		if(new_ruler){

			// IF NEW RULER POINT AND LAST IN LAST, ADD NEW FIELD
			if(num == rulers.length-1) addField('ruler');

			selectObject('ruler', num);
			selectObject('ruler', num);
		}		
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

function changeCurrentMarker(d){
	var i;

	if(current_landmark !== "NA"){
		var new_c_lm = current_landmark + d;
		if(new_c_lm < 0){new_c_lm = landmarks.length-1;}
		if(new_c_lm > (landmarks.length-1)){new_c_lm = 0;}
	
		selectObject('landmark', new_c_lm);
		//alert('changeCurrentMarker' + ': ' + current_landmark + ', ' + current_curve_point)
	}else if(current_curve_point !== "NA" && current_curve_point_type == 'point'){
		var i;
		var f = 0;
		
		for(i = 0;i < disp_order_curve.length; i++){
			if(current_curve_point == disp_order_curve[i]){break;}
		}
		
		if((i+d) < 0){var new_c_cp = disp_order_curve[disp_order_curve.length-1];}
		if((i+d) > (curve_points.length-1)){var new_c_cp = disp_order_curve[0];}
		if((i+d) >= 0 && (i+d) <= (curve_points.length-1)){var new_c_cp = disp_order_curve[(i+d)];}

		if(curve_points[new_c_cp].parent !== curve_points[current_curve_point].parent){
			if(d == 1){
				selectObject('curve_start', curve_points[current_curve_point].parent+1);
			}else if(d == -1){
				selectObject('curve_start', curve_points[current_curve_point].parent);
			}
			return;
		}

		selectObject('curve_point', new_c_cp);
	}else if(current_curve_point !== "NA" && current_curve_point_type == 'start'){
		if(d == 1){
			selectObject('curve_end', current_curve_point);
			return;
		}
	}else if(current_curve_point !== "NA" && current_curve_point_type == 'end'){
		if(d == -1){
			selectObject('curve_start', current_curve_point);
			return;
		}
		if(d == 1){
			for(i = 0;i < curve_points.length; i++){
				if(curve_points[i].parent == current_curve_point){break;}
			}
			selectObject('curve_point', i);
			return;
		}
	}else if(current_ruler !== "NA"){
		var new_c_ruler = current_ruler + d;
		if(new_c_ruler < 0){new_c_ruler = rulers.length-1;}
		if(new_c_ruler > (rulers.length-1)){new_c_ruler = 0;}
	
		selectObject('ruler', new_c_ruler);
	}
}

function changeImage(adv){
	
	var answer = true;
	var unsaved_shapes = '';

	if(unsaved_landmarks == 'TRUE') unsaved_shapes += '\tLandmarks\n';
	if(unsaved_curves == 'TRUE') unsaved_shapes += '\tCurves\n';
	if(unsaved_ruler_points == 'TRUE') unsaved_shapes += '\tRuler points\n';
	if(unsaved_corners == 'TRUE') unsaved_shapes += '\tCheckerboard corners\n';

	if(unsaved_shapes !== '')
		answer = confirm ("The following shape data have unsaved changes:\n" + unsaved_shapes + "\nWould you still like to leave this image?");

	if(answer){
		var submit_params = {change_image:adv};
		submit(submit_params);
		if(browser_name !== 'Safari') setTimeout(function(){window.close();}, 10);		
	}
}

function changeMarkerStyle(id, num, color){

	var circle = svgDocument.getElementById(id + num);
	var line = svgDocument.getElementById(id + '_line' + num);

	if(!circle || !line){return false;}

	circle.setAttribute("style", "stroke:" + color + ";fill:none;stroke-opacity:0.8;fill-opacity:0.1;");
	line.setAttribute("style", "stroke:" + color + ";");
}

function clearCheckerboardCorners(){

	// CLEAR CHECKER SQUARE SIZE
	checker_square_pixel = "NA";

	// CLEAR CHECKER WORLD SIZE
	document.getElementById("checker_square_world").value = "";

	for(var i = 0; i < corners.length;i++){
		var textlink = svgDocument.getElementById('corner_label' + i);
		if(textlink) svgDocument.getElementById("world").removeChild(textlink);

		var line = svgDocument.getElementById('corner_label_line' + i);
		if(line) svgDocument.getElementById("world").removeChild(line);

		var circle = svgDocument.getElementById('corner' + i);
		if(circle) svgDocument.getElementById("world").removeChild(circle);

		var line = svgDocument.getElementById('corner_cline' + i);
		if(line) svgDocument.getElementById("world").removeChild(line);

		var line = svgDocument.getElementById('corner_line' + i);
		if(line) svgDocument.getElementById("world").removeChild(line);
	}

	// CLEAR CORNERS
	corners = new Array();
	
	updateScaling()
	//reDrawObjects();

	unsaved_corners = 'TRUE';
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

function dblClickEvent(e){
	//updateStatus("","current_process");
	getMouseXY(e);
	var i;
	if(e.keyCode > 0){ // key press event
		var p_pos_l = last_p_pos;
	}else{ // mouse dbl click
		var p_pos_l = findPixelPosition(initialX, initialY, photograph.x, photograph.y, photograph.w, photograph.h, photograph.pw, photograph.ph);
		p_pos = p_pos_l;
	}
	for(i = 0;i < landmarks.length;i++){
		if(landmarks[i].x == "-"){continue;}
		if(proximityCheck(Math.round(p_pos_l[0]), Math.round(p_pos_l[1]), landmarks[i].x, landmarks[i].y, click_radius)){
			selectObject('landmark', i);
			mousedown = 0;
			return;
		}
	}	
	for(i = 0;i < curve_points.length;i++){
		if(curve_points[i].x == "-"){continue;}
		if(proximityCheck(Math.round(p_pos_l[0]), Math.round(p_pos_l[1]), curve_points[i].x, curve_points[i].y, click_radius)){
			selectObject('curve_point', i)
			mousedown = 0;
			return;
		}
	}	
	for(i = 0;i < curve_start.length;i++){
		if(curve_start[i].x == "-"){continue;}
		if(proximityCheck(Math.round(p_pos_l[0]), Math.round(p_pos_l[1]), curve_start[i].x, curve_start[i].y, click_radius)){
			selectObject('curve_start', i)
			mousedown = 0;
			return;
		}
	}	
	for(i = 0;i < curve_end.length;i++){
		if(curve_end[i].x == "-"){continue;}
		if(proximityCheck(Math.round(p_pos_l[0]), Math.round(p_pos_l[1]), curve_end[i].x, curve_end[i].y, click_radius)){
			selectObject('curve_end', i)
			mousedown = 0;
			return;
		}
	}	
	for(i = 0;i < rulers.length;i++){
		if(rulers[i].x == "-"){continue;}
		if(proximityCheck(Math.round(p_pos_l[0]), Math.round(p_pos_l[1]), rulers[i].x, rulers[i].y, click_radius)){
			selectObject('ruler', i);
			mousedown = 0;
			return;
		}
	}

	if(current_landmark !== "NA"){addObject(Math.round(p_pos_l[0]),Math.round(p_pos_l[1]));}
	if(current_curve_point !== "NA"){addCurvePoint(Math.round(p_pos_l[0]),Math.round(p_pos_l[1]));}
	if(current_ruler !== "NA"){addObject(Math.round(p_pos_l[0]),Math.round(p_pos_l[1]));}
	addObject(Math.round(p_pos_l[0]),Math.round(p_pos_l[1]));
	if(auto_advance){changeCurrentMarker(1);}
	mousedown = 0;
}

function deleteMarker(){

	var i;
	if(current_landmark !== "NA"){
		var num = current_landmark;
		landmarks[num].x = "-";
		landmarks[num].y = "-";

		var circle = svgDocument.getElementById('landmark' + num);
		if(circle){svgDocument.getElementById("world").removeChild(circle);}
		var line = svgDocument.getElementById('landmark_line' + num);
		if(line){svgDocument.getElementById("world").removeChild(line);}

		var lm_x_div = document.getElementById('landmark_col_x_' + num);
		var lm_y_div = document.getElementById('landmark_col_y_' + num);

		lm_x_div.innerHTML = "-";
		lm_y_div.innerHTML = "-";

		unsaved_landmarks = 'TRUE';
	}
	if(current_curve_point !== "NA"){
		var num = current_curve_point;
		if(current_curve_point_type == 'point'){
			curve_points[num].x = "-";
			curve_points[num].y = "-";

			var circle = svgDocument.getElementById('curve_' + current_curve_point_type + num);
			if(circle){svgDocument.getElementById("world").removeChild(circle);}
			var line = svgDocument.getElementById('curve_' + current_curve_point_type + '_line' + num);
			if(line){svgDocument.getElementById("world").removeChild(line);}
	
			var cp_x_div = document.getElementById('curve_' + current_curve_point_type + '_x_' + num);
			var cp_y_div = document.getElementById('curve_' + current_curve_point_type + '_y_' + num);
	
			cp_x_div.innerHTML = "-";
			cp_y_div.innerHTML = "-";
			drawBezierCurve('curve_point', current_curve_point);
		}else{
			if(current_curve_point_type == 'start'){
				for(i = 0;i < start_dupl[num].type.length;i++){

					var circle = svgDocument.getElementById('curve_' + start_dupl[num].type[i] + start_dupl[num].num[i]);
					if(circle){svgDocument.getElementById("world").removeChild(circle);}
					var line = svgDocument.getElementById('curve_' + start_dupl[num].type[i] + '_line' + start_dupl[num].num[i]);
					if(line){svgDocument.getElementById("world").removeChild(line);}

					var cp_x_div = document.getElementById('curve_' + start_dupl[num].type[i] + '_x_' + start_dupl[num].num[i]);
					var cp_y_div = document.getElementById('curve_' + start_dupl[num].type[i] + '_y_' + start_dupl[num].num[i]);
		
					cp_x_div.innerHTML = "-";
					cp_y_div.innerHTML = "-";
					
					if(start_dupl[num].type[i] == 'start'){
						curve_start[start_dupl[num].num[i]].x = "-";
						curve_start[start_dupl[num].num[i]].y = "-";
					}else{
						curve_end[start_dupl[num].num[i]].x = "-";
						curve_end[start_dupl[num].num[i]].y = "-";
					}
				}

				if(curve_also_landmark[num].start >= 0){
					document.getElementById('landmark_col_x_' + curve_also_landmark[num].start).innerHTML = '-';
					document.getElementById('landmark_col_y_' + curve_also_landmark[num].start).innerHTML = '-';
				}

				for(i = 0;i < start_dupl[num].num.length;i++){
					drawBezierCurve(start_dupl[num].type[i], start_dupl[num].num[i]);
				}
			}else{
				for(i = 0;i < end_dupl[num].type.length;i++){

					var circle = svgDocument.getElementById('curve_' + end_dupl[num].type[i] + end_dupl[num].num[i]);
					if(circle){svgDocument.getElementById("world").removeChild(circle);}
					var line = svgDocument.getElementById('curve_' + end_dupl[num].type[i] + '_line' + end_dupl[num].num[i]);
					if(line){svgDocument.getElementById("world").removeChild(line);}

					var cp_x_div = document.getElementById('curve_' + end_dupl[num].type[i] + '_x_' + end_dupl[num].num[i]);
					var cp_y_div = document.getElementById('curve_' + end_dupl[num].type[i] + '_y_' + end_dupl[num].num[i]);
		
					cp_x_div.innerHTML = "-";
					cp_y_div.innerHTML = "-";
	
					if(end_dupl[num].type[i] == 'start'){
						curve_start[end_dupl[num].num[i]].x = "-";
						curve_start[end_dupl[num].num[i]].y = "-";
					}else{
						curve_end[end_dupl[num].num[i]].x = "-";
						curve_end[end_dupl[num].num[i]].y = "-";
					}
				}

				if(curve_also_landmark[num].end >= 0){
					document.getElementById('landmark_col_x_' + curve_also_landmark[num].end).innerHTML = '-';
					document.getElementById('landmark_col_y_' + curve_also_landmark[num].end).innerHTML = '-';
				}

				for(i = 0;i < end_dupl[num].num.length;i++){
					drawBezierCurve(end_dupl[num].type[i], end_dupl[num].num[i]);
				}
			}
		}
		unsaved_curves = 'TRUE';
	}

	if(current_ruler !== "NA"){

		var num = current_ruler;
		rulers[num].x = "-";
		rulers[num].y = "-";

		var circle = svgDocument.getElementById('ruler' + num);
		if(circle){svgDocument.getElementById("world").removeChild(circle);}
		var line = svgDocument.getElementById('ruler_line' + num);
		if(line){svgDocument.getElementById("world").removeChild(line);}

		var ruler_x_div = document.getElementById('ruler_col_x_' + num);
		var ruler_y_div = document.getElementById('ruler_col_y_' + num);

		ruler_x_div.innerHTML = "-";
		ruler_y_div.innerHTML = "-";
		
		findRulerInterval();

		unsaved_ruler_points = 'TRUE';
	}
}

function detectKeyEvent(e) {
	var evt = e || window.event;
	if(evt.shiftKey === true){
		//if(evt.keyCode==65){if(auto_advance){auto_advance = false;}else{auto_advance = true;}} // a
		if(evt.keyCode==82){} // r
		if(evt.keyCode==37){move(-10,0);} // left arrow
		if(evt.keyCode==39){move(10,0);} // right arrow
		if(evt.keyCode==38){move(0,-10);} // up arrow
		if(evt.keyCode==40){move(0,10);} // down arrow
		if(evt.metaKey === true && evt.keyCode==37){move(-90,0);} // left arrow
		if(evt.metaKey === true && evt.keyCode==39){move(90,0);} // right arrow
		if(evt.metaKey === true && evt.keyCode==38){move(0,-90);} // up arrow
		if(evt.metaKey === true && evt.keyCode==40){move(0,90);} // down arrow
	}else{
		//if(evt.keyCode==8){deleteMarker();} // delete
		if(evt.keyCode==68){deleteMarker();} // d
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
	}
}

function drawBezierCurve(id, num){
	var i, j;
	var cb_num = num;
	if(id == 'curve_point'){cb_num = curve_points[num].parent;}
	
	var t_pos = new Array();
	t_pos[0] = moveToTarget(0, 0, curve_start[cb_num].x, curve_start[cb_num].y, photograph.w/photograph.pw, photograph.h/photograph.ph, 1);
	
	var n = 1;
	for(i = 0;i < curve_blocks[cb_num].length;i++){
		if(curve_points[curve_blocks[cb_num][i]].x == '-' || isNaN(curve_points[curve_blocks[cb_num][i]].x)){continue;}
		t_pos[n] = moveToTarget(0, 0, curve_points[curve_blocks[cb_num][i]].x, curve_points[curve_blocks[cb_num][i]].y, photograph.w/photograph.pw, photograph.h/photograph.ph, 1);
		n++;
	}
	t_pos[t_pos.length] = moveToTarget(0, 0, curve_end[cb_num].x, curve_end[cb_num].y, photograph.w/photograph.pw, photograph.h/photograph.ph, 1);

	if(curve_start[cb_num].x == '-' || isNaN(curve_start[cb_num].x)){
		t_pos[0] = Array(NaN, NaN);
		t_pos[1] = Array(NaN, NaN);
	}
	if(curve_end[cb_num].x == '-' || isNaN(curve_end[cb_num].x)){
		t_pos[t_pos.length-2] = Array(NaN, NaN);
		t_pos[t_pos.length-1] = Array(NaN, NaN);
	}

	var px = photograph.x;
	var py = photograph.y;

	var j = 0;
	var d = "M ";
	for(i = 0;i < t_pos.length;i++){
		if(isNaN(t_pos[i][0])) continue;
		if(j == 1){d += 'Q ';}
		d += [Math.round(t_pos[i][0]+px)] + ' ' + [Math.round(t_pos[i][1]+py)] + ' ';
		j++;
	}

	var path = svgDocument.getElementById('curve' + cb_num);
	if(path){
		var curve_color = path.getAttribute("style");
		svgDocument.getElementById("world").removeChild(path);
	}else{var curve_color = "stroke:" + curve_color_blur + ";";}

	var path = svgDocument.createElementNS(svgns, "path");
	path.setAttribute("id", 'curve' + cb_num);
	path.setAttribute("d", d);
	path.setAttribute("fill", "none");
	path.setAttribute("stroke-width", 2);
	path.setAttribute("style", curve_color);
	path.addEventListener("mousedown", function(evt) { evt.preventDefault(); }, false); // evt.preventDefault(); keeps browser from treating the image as if it is going to be exported out of the window
	path.addEventListener("dblclick", function(evt) { dblClickEvent(evt); }, false);
	svgDocument.getElementById("world").appendChild(path);
}

function drawCheckerboardCorners(corners_in){

	// INPUT IS STRING OF CORNER COORDINATES FROM JSON PARSER
	var i;

	for(i=0;i < corners_in.length;i++){

		corners[i] = new Array();
		corners[i].x = corners_in[i][0];
		corners[i].y = corners_in[i][1];
	}

	for(i=0;i < corners_in.length;i++) drawMarker('corner', i, corners[i].x, corners[i].y);
}

function drawMarker(id, num, x, y){

	if(x == "-" || y == "-" || isNaN(x) || isNaN(y)){return false;}
//	if(id == 'corner' && num == 1) alert(id + ' ' + num + ' ' + x + ' ' + y);
	var t_pos = moveToTarget(0,0,x,y,photograph.w/photograph.pw,photograph.h/photograph.ph,1);
//	if(id == 'corner' && num == 1) alert(id + ' ' + num + ' ' + photograph.x + ' ' + y);

	var color_by_id, r;
	var fill = 'none';
	var fill_opacity = 0;

	if(id == 'landmark' || id == 'ruler'){
		color_by_id = landmark_color_blur;
		r = landmark_radius;
	}else if(id == 'curve_point' || id == 'curve_start' || id == 'curve_end'){
		color_by_id = control_point_color_blur;
		r = control_point_radius;
	}else if(id == 'corner'){
		color_by_id = corner_color;
		r = corner_radius;
		fill = corner_color;
		fill_opacity = 0;
	}

	var circle = svgDocument.getElementById(id + num);
	if(circle){
		var circle_color = circle.getAttribute("style");
		svgDocument.getElementById("world").removeChild(circle);
	}else{var circle_color = "stroke:" + color_by_id + ";";}
	var line = svgDocument.getElementById(id + '_line' + num);
	if(line){
		var dot_color = line.getAttribute("style");
		svgDocument.getElementById("world").removeChild(line);
	}else{var dot_color = "stroke:" + color_by_id + ";";}

	if(id == 'corner'){
		
		if(num == 0){

			var textlink = svgDocument.getElementById(id + '_label' + num);
			if(textlink) svgDocument.getElementById("world").removeChild(textlink);

			var textBlock = svgDocument.createTextNode(num+1);
			var textlink = svgDocument.createElementNS(svgns, "text");
			textlink.setAttribute("id", id + '_label' + num);
			textlink.setAttribute("text-anchor", "left");
			textlink.setAttribute("font-size", 8.5);
			textlink.setAttribute("x", t_pos[0] + photograph.x + 3);
			textlink.setAttribute("y", t_pos[1] + photograph.y - 5);
			textlink.setAttribute("style", "stroke:" + fill + ";");
			textlink.appendChild(textBlock);
			svgDocument.getElementById("world").appendChild(textlink);
		}

		if(num < corners.length - 1){
			var t_pos2 = moveToTarget(0,0,corners[num+1].x,corners[num+1].y,photograph.w/photograph.pw,photograph.h/photograph.ph,1);

			var line = svgDocument.getElementById(id + '_cline' + num);
			if(line) svgDocument.getElementById("world").removeChild(line);

			var line = svgDocument.createElementNS(svgns, "line");
			line.setAttribute("id", id + '_cline' + num);
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
	circle.setAttribute("id", id + num);
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
	line.setAttribute("id", id + '_line' + num);
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

	if(id == 'curve_point' || id == 'curve_start' || id == 'curve_end'){
		drawBezierCurve(id, num);
	}

	svgDocument.getElementById("world").appendChild(line);
	svgDocument.getElementById("world").appendChild(circle);
}

function exit(){

	var answer = true;
	var unsaved_shapes = '';

	if(unsaved_landmarks == 'TRUE') unsaved_shapes += '\tLandmarks\n';
	if(unsaved_curves == 'TRUE') unsaved_shapes += '\tCurves\n';
	if(unsaved_ruler_points == 'TRUE') unsaved_shapes += '\tRuler points\n';
	if(unsaved_corners == 'TRUE') unsaved_shapes += '\tCheckerboard corners points\n';
	
	if(unsaved_shapes !== '')
		answer = confirm ("The following shape data have unsaved changes:\n" + unsaved_shapes + "\nWould you still like to exit?");

	if(answer){
		var submit_params = {exit:1};
		submit(submit_params);
		setTimeout(function(){window.close();}, 10);
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
	submit_params.checkerboard_nx = checkerboard_nx;
	submit_params.checkerboard_ny = checkerboard_ny;
	submit_params.find_checkerboard_corners = true;
	submit(submit_params);
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
	last_p_pos = new Array(Math.round(p_pos[0]), Math.round(p_pos[1]));

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
	if(current_curve_point !== "NA"){
		if(current_curve_point_type == 'point'){
			var current_position = curve_points[current_curve_point];
		}else if(current_curve_point_type == 'start'){
			var current_position = curve_start[current_curve_point];
		}else if(current_curve_point_type == 'end'){
			var current_position = curve_end[current_curve_point];
		}
	}
	if(current_ruler !== "NA"){
		var current_position = rulers[current_ruler];
	}
	if(mousedown == 1 && globalkeypress == ''){		
		distX = tempX - prevX;
		distY = prevY - tempY;

		if(current_landmark !== "NA" || current_curve_point !== "NA" || current_ruler !== "NA"){
			if(!mouse_move_in_prox){mouse_move_in_prox = proximityCheck(Math.round(p_pos[0]), Math.round(p_pos[1]), current_position.x, current_position.y, click_radius);}
		}
		if(current_landmark !== "NA" && mouse_move_in_prox){
			addObject(Math.round(p_pos[0]), Math.round(p_pos[1]));
		}
		if(current_curve_point !== "NA" && mouse_move_in_prox){
			addCurvePoint(Math.round(p_pos[0]), Math.round(p_pos[1]));
			if(current_curve_point_type == 'start'){
				for(i = 0;i < start_dupl[current_curve_point].num.length;i++){
					if(start_dupl[current_curve_point].num[i] == current_curve_point){continue;}
					drawBezierCurve(start_dupl[current_curve_point].type[i], start_dupl[current_curve_point].num[i]);
				}
			}
			if(current_curve_point_type == 'end'){
				for(i = 0;i < end_dupl[current_curve_point].num.length;i++){
					if(end_dupl[current_curve_point].num[i] == current_curve_point){continue;}
					drawBezierCurve(end_dupl[current_curve_point].type[i], end_dupl[current_curve_point].num[i]);
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
		loadFrame();
	}

	svgDocument.onmouseup = function(){mousedown = 0;mouse_move_in_prox = 0;}

	prevX = tempX;
	prevY = tempY;

	return false;
}


function imageZoom(d,mouseX,mouseY){
	if(d == 0){return;}

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

	var new_w = new_w;
	var new_h = new_h;
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
	loadFrame();
}

function initLoadImage(x,y,w,h,id,src){
	photograph_x = x;
	photograph_y = y;
	photograph_w = w;
	photograph_h = h;
	photograph_id = id;
	photograph_src = src;
}

function loadCurves(){

	if(init_params.curves_ref === undefined) return;

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

	var m = 0;
	for(i = 0;i < init_params.curves_ref.length;i++){

		//	might need parseFloat('-');

		var coor = new Array(new Array('-', '-'), new Array('-', '-'), new Array('-', '-'));

		if(init_params.control_points !== undefined){
			// SEARCH IF MATCHING START EXISTS
			for(j = 0;j < start_dupl[i].num.length;j++){

				for(k = 0;k < init_params.control_points.length;k++){
					if(init_params.control_points[k][0] !== init_params.curves_ref[start_dupl[i].num[j]][0]) continue;

					if(start_dupl[i].type[j] == 'start'){
						coor[0] = Array(init_params.control_points[k][1], init_params.control_points[k][2]);
					}else{
						coor[0] = Array(
							init_params.control_points[k][init_params.control_points[k].length-2], 
							init_params.control_points[k][init_params.control_points[k].length-1]);
					}
				}
			}

			// SEARCH IF MATCHING END EXISTS
			for(j = 0;j < end_dupl[i].num.length;j++){

				for(k = 0;k < init_params.control_points.length;k++){
					if(init_params.control_points[k][0] !== init_params.curves_ref[end_dupl[i].num[j]][0]) continue;

					if(end_dupl[i].type[j] == 'start'){
						coor[2] = Array(init_params.control_points[k][1], init_params.control_points[k][2]);
					}else{
						coor[2] = Array(
							init_params.control_points[k][init_params.control_points[k].length-2], 
							init_params.control_points[k][init_params.control_points[k].length-1]);
					}
				}
			}

			// SEARCH IF CURVE EXISTS
			for(j = 0;j < init_params.control_points.length;j++){

				if(init_params.curves_ref[i][0] !== init_params.control_points[j][0]) continue;

				var n = 0;
				for(k = 1; k < init_params.control_points[j].length;k=k+2){
					coor[n] = Array(init_params.control_points[j][k], init_params.control_points[j][k+1]);
					n++;
				}
				coor[n] = coor[n-1];
				coor[n-1] = Array('-', '-');

				break;
			}
		}

		var p = 0;
		curve_blocks[i] = new Array();

		curve_start[i] = new Array();
		curve_start[i].x = coor[0][0];
		curve_start[i].y = coor[0][1];

		curve_end[i] = new Array();
		curve_end[i].x = coor[coor.length-1][0];
		curve_end[i].y = coor[coor.length-1][1];

		for(j = 1;j < coor.length-1;j++){

			curve_points[m] = new Array();
			curve_points[m].x = coor[j][0];
			curve_points[m].y = coor[j][1];
			curve_points[m].parent = i;
			curve_blocks[i][p] = m;
			disp_order_curve[m] = m;
			m++;
			p++;
		}
		
		// Look for landmarks of same name as curve start and end
		curve_also_landmark[i] = new Array();
		curve_also_landmark[i].start = -1;
		curve_also_landmark[i].end = -1;

		// FILL WITH LANDMARK COORDINATE IF CURVE COORDINATE IS NOT AVAILABLE
		if(init_params.landmarks_ref !== undefined && init_params.landmarks !== undefined){
			
			if(curve_start[i].x == '-'){
				for(j = 0;j < init_params.landmarks.length;j++){
					if(init_params.curves_ref[i][1] !== init_params.landmarks[j][0]) continue;
					curve_start[i].x = init_params.landmarks[j][1];
					curve_start[i].y = init_params.landmarks[j][2];
					break;
				}
			}
			if(curve_end[i].x == '-'){
				for(j = 0;j < init_params.landmarks.length;j++){
					if(init_params.curves_ref[i][2] !== init_params.landmarks[j][0]) continue;
					curve_end[i].x = init_params.landmarks[j][1];
					curve_end[i].y = init_params.landmarks[j][2];
					break;
				}
			}
		}

		if(init_params.landmarks_ref !== undefined){
			for(j = 0;j < init_params.landmarks_ref.length;j++){
				if(init_params.curves_ref[i][1] == init_params.landmarks_ref[j]){
					curve_also_landmark[i].start = j;
					if(curve_start[i].x !== '-'){
						landmarks[j].x = 'x';
						landmarks[j].y = 'y';
					}
				}
				if(init_params.curves_ref[i][2] == init_params.landmarks_ref[j]){
					curve_also_landmark[i].end = j;
					if(curve_end[i].x !== '-'){
						landmarks[j].x = 'x';
						landmarks[j].y = 'y';
					}
				}
			}
		}
	}
}

function loadFrame(){
	var i;
	var xywh_array = new Array(); // From top, clockwise
	xywh_array[0] = new Array(0,0,2000,margin);
	xywh_array[1] = new Array(photograph.ow+margin-1,0,1000,2000);
	xywh_array[2] = new Array(0,photograph.oh+margin-1,1000,1000);
	xywh_array[3] = new Array(0,0,margin,2000);

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

function loadImage(x,y,w,h,id,src,c){

	if(!c){
		var x = photograph_x;
		var y = photograph_y;
		var w = photograph_w;
		var h = photograph_h;
		var id = photograph_id;
		var src = photograph_src;
		var image = svgDocument.createElementNS(svgns, "image");
	}else{
		var image = svgDocument.getElementById(id);
		svgDocument.getElementById("world").removeChild(image);
		var image = svgDocument.createElementNS(svgns, "image");
	}

	var new_var = alignSVGImage(w,h);
	var nw = new_var[0];
	var nh = new_var[1];
	
	image.setAttribute("id", id);
	svgSetXYWH(image, x + margin,y + margin,nw,nh);
	image.setAttribute("style", "opacity:1;");
	image.setAttribute("preserveAspectRatio", "xMinYMin meet");
	image.setAttributeNS(XLINK, "href", src);
	image.addEventListener("mousedown", function(evt) { evt.preventDefault(); }, false); // evt.preventDefault(); keeps browser from treating the image as if it is going to be exported out of the window
	image.addEventListener("dblclick", function(evt) { dblClickEvent(evt); }, false);

	photograph = new Array();
	photograph.id = id;
	photograph.x = x + margin;
	photograph.y = y + margin;
	photograph.w = photograph.ow = nw;
	photograph.h = photograph.oh = nh;
	photograph.pw = w;
	photograph.ph = h;

	if(!c){
		myImageInit = new Array();
		myImageInit[0] = new Array();
		myImageInit[0].x = x;
		myImageInit[0].y = y;
		myImageInit[0].w = w;
		myImageInit[0].h = h;
		myImageInit[0].id = id;
		myImageInit[0].src = src;
	}

	// UPDATE IMAGE ZOOM DISPLAY
	document.getElementById('curr_zoom').innerHTML = Math.round((photograph.w / myImageInit[0].w)*100) + "%";

	svgDocument.getElementById("world").appendChild(image);
}

function loadLandmarks(){

	if(init_params.landmarks_ref === undefined) return;

	var i, j;
	for(i = 0;i < init_params.landmarks_ref.length;i++){

		// IF CURVES ARE DEFINED AND LANDMARK IS THE SAME AS A CURVE POINT, LEAVE FOR CURVE TO DEFINE
		if(init_params.curves_ref !== undefined){
			//if(i == 0) alert(i)
		}

		// SEARCH IF LANDMARK EXISTS
		var coor = new Array('-', '-');
		if(init_params.landmarks !== undefined){
			for(j = 0;j < init_params.landmarks.length;j++){
				if(init_params.landmarks_ref[i] !== init_params.landmarks[j][0]) continue;
			
				coor = Array(init_params.landmarks[j][1], init_params.landmarks[j][2]);
				break;
			}
		}

		landmarks[i] = new Array();
		landmarks[i].x = coor[0];
		landmarks[i].y = coor[1];
	}
}

function loadObjects(){

	var i;

	if(init_params.corners !== undefined){

		// DRAW CHECKERBOARD CORNERS
		drawCheckerboardCorners(init_params.corners);
	}

	// CREATE INITIAL TWO RULER POINTS
	rulers[0] = new Array();rulers[0].x = '-';rulers[0].y = '-';
	rulers[1] = new Array();rulers[1].x = '-';rulers[1].y = '-';

	if(init_params.ruler_points !== undefined){
		var ruler_nums = new Array();

		// GET MAX RULER POINT NUMBER
		for(i = 0;i < init_params.ruler_points.length;i++) ruler_nums[i] = init_params.ruler_points[i][0].substring(12, init_params.ruler_points[i][0].length);

		// ADD FIELDS TO TABLE
		for(i = 0;i <= Math.max.apply(Math, ruler_nums)+1;i++) if(i > 2) addField('ruler');

		for(i = 0;i < init_params.ruler_points.length;i++){

			// ADD VALUES TO ARRAY
			rulers[ruler_nums[i] - 1].x = init_params.ruler_points[i][1];
			rulers[ruler_nums[i] - 1].y = init_params.ruler_points[i][2];
		
			// ADD VALUES TO TABLE
			document.getElementById('ruler_col_x_' + (ruler_nums[i]-1)).innerHTML = init_params.ruler_points[i][1];
			document.getElementById('ruler_col_y_' + (ruler_nums[i]-1)).innerHTML = init_params.ruler_points[i][2];

			// DRAW MARKERS
			drawMarker('ruler', ruler_nums[i]-1, init_params.ruler_points[i][1], init_params.ruler_points[i][2]);
		}
	}
}

function move(dx,dy){

	if(current_landmark !== "NA"){
		var num = current_landmark;
		addObject(parseFloat(landmarks[num].x) + dx, parseFloat(landmarks[num].y) + dy);
		loadFrame();
		return;
	}
	if(current_curve_point !== "NA"){
		if(current_curve_point_type == 'point'){
			addCurvePoint(parseFloat(curve_points[current_curve_point].x) + dx, parseFloat(curve_points[current_curve_point].y) + dy);
		}else if(current_curve_point_type == 'start'){
			addCurvePoint(parseFloat(curve_start[current_curve_point].x) + dx, parseFloat(curve_start[current_curve_point].y) + dy);
		}else if(current_curve_point_type == 'end'){
			addCurvePoint(parseFloat(curve_end[current_curve_point].x) + dx, parseFloat(curve_end[current_curve_point].y) + dy);
		}
		loadFrame();
		return;
	}
	if(current_ruler !== "NA"){
		var num = current_ruler;
		addObject(parseFloat(rulers[num].x) + dx, parseFloat(rulers[num].y) + dy);
		loadFrame();
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

function onLoadFunctions(evt){

	// Get browser name
	var get_browser_name = getBrowserName();
	browser_name = get_browser_name[0];
	browser_version = get_browser_name[1];

	setSettings();

	// GET JSON STRING FROM R
	var json_string = document.getElementById('text_input').value

	// PARSE JSON STRING
    init_params = JSON.parse(json_string);

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
	image_object.onmousewheel = scrollEvent;		// SAFARI
	document.onkeyup=function(){globalkeypress = '';}

	// SET AUTO ADVANCE
	//if(init_params.auto_advance === undefined){auto_advance = true;}else{auto_advance = (String(init_params.auto_advance).toLowerCase() == 'true');}

	landmark_color_blur = init_params.landmark_color_blur
	landmark_color_focus = init_params.landmark_color_focus
	curve_color_blur = init_params.curve_color_blur
	control_point_color_blur = init_params.control_point_color_blur
	control_point_color_focus = init_params.control_point_color_focus
	landmark_radius = init_params.landmark_radius
	control_point_radius = init_params.control_point_radius
	marker_stroke_width = init_params.marker_stroke_width
	unsaved_landmarks = init_params.unsaved_landmarks
	unsaved_curves = init_params.unsaved_curves
	unsaved_ruler_points = init_params.unsaved_ruler_points
	unsaved_corners = init_params.unsaved_corners

	updateSubmitButtons();

	// FIL IN IMAGE DETAILS
	document.getElementById("image_name").innerHTML = init_params.img_name;
	document.getElementById("image_dimensions").innerHTML = init_params.img_width +  ' x ' + init_params.img_height + ' px';
	document.getElementById("image_size").innerHTML = Math.round((init_params.img_size/10000), 3)/100 + ' MB';
	document.getElementById("image_id").innerHTML = init_params.image_id;

	// CONVERT TEXT INPUTS TO STRINGS
	init_params.scaling_units = init_params.scaling_units.toString();

	// FILL IN SCALING SETTINGS
	if(!isNaN(init_params.ruler_pixel)) ruler_interval_pixels = Number(init_params.ruler_pixel)
	if(!isNaN(init_params.ruler_interval)) document.getElementById("ruler_interval_world").value = init_params.ruler_interval;
	if(!isNaN(init_params.ruler_interval) && init_params.scaling_units !== "NA") document.getElementById("ruler_interval_world").value += ' ' + init_params.scaling_units;
	if(!isNaN(init_params.checkerboard_nx)) document.getElementById("internal_corner_dim").value = init_params.checkerboard_nx;
	if(!isNaN(init_params.checkerboard_ny)) document.getElementById("internal_corner_dim").value += ', ' + init_params.checkerboard_ny;

	if(!isNaN(init_params.checker_square_pixel)) checker_square_pixel = Number(init_params.checker_square_pixel);
	if(!isNaN(init_params.checker_square_world)) document.getElementById("checker_square_world").value = init_params.checker_square_world;
	if(init_params.scaling_units !== "NA" && !isNaN(init_params.checker_square_world)) document.getElementById("checker_square_world").value += ' ' + init_params.scaling_units;

	updateScaling()

	//initLoadImage(0, 0, document.getElementById('img1').naturalWidth, document.getElementById('img1').naturalHeight, 'img1', 'img/' + init_params.img_name);
	initLoadImage(0, 0, init_params.img_width, init_params.img_height, 'img1', 'img/' + init_params.img_name);

	loadImage();
	loadLandmarks();
	loadCurves();
	loadObjects();
	findRulerInterval();
	printLandmarkTable();
	printCurveTable();
	loadFrame();
	onWindowResize();
	selectInitialLandmark();

	marker_selected = 0;
	//imageZoom(0.5, photograph.w/2, photograph.h/2);
}

function onWindowResize(){
	window_properties.height = getWindowHeight();
	window_properties.width = getWindowWidth();
	image_object.width = window_properties.width - 370;
	image_object.height = window_properties.height - 2*margin - 15;

	loadImage(myImageInit[0].x, myImageInit[0].y, myImageInit[0].w, myImageInit[0].h, myImageInit[0].id, myImageInit[0].src, 1);
	reDrawObjects();
	loadFrame();
}

function printCurveTable(){

	if(init_params.curves_ref === undefined) return;

	var i, j;
	for(i = 0;i < init_params.curves_ref.length;i++){

		var curve_container = document.createElement('div');
		curve_container.className = 'semilandmark_table_block';
		curve_container.id = 'curve_container_' + i;		
		curve_container.name = init_params.curves_ref[i][0];
		curve_container.innerHTML = init_params.curves_ref[i][0];
		document.getElementById('curve_table_container').appendChild(curve_container);

		var curve_start_div = document.createElement('div');
		curve_start_div.className = 'curve_ends';
		curve_start_div.id = 'curve_start_' + i;		
		curve_start_div.innerHTML = init_params.curves_ref[i][1];
		curve_start_div.setAttribute("onclick", "selectObject('curve_start'," + i + ");");
		document.getElementById('curve_container_' + i).appendChild(curve_start_div);

		var curve_row_start = document.createElement('div');
		curve_row_start.className = 'curve_point_row';
		curve_row_start.id = 'curve_row_start_' + i;		
		document.getElementById('curve_start_' + i).appendChild(curve_row_start);

		var curve_start_y = document.createElement('div');
		curve_start_y.className = 'semilandmark_point_col';
		curve_start_y.id = 'curve_start_y_' + i;
		curve_start_y.innerHTML = curve_start[i].y;
		document.getElementById('curve_row_start_' + i).appendChild(curve_start_y);

		var curve_start_x = document.createElement('div');
		curve_start_x.className = 'semilandmark_point_col';
		curve_start_x.id = 'curve_start_x_' + i;
		curve_start_x.innerHTML = curve_start[i].x;
		document.getElementById('curve_row_start_' + i).appendChild(curve_start_x);

		var curve_block_div = document.createElement('div');
		curve_block_div.id = 'curve_block_' + i;		
		document.getElementById('curve_container_' + i).appendChild(curve_block_div);

		for(j = 0;j < curve_blocks[i].length;j++){

			var curve_row = document.createElement('div');
			curve_row.className = 'semilandmark_point_row';
			curve_row.id = 'curve_row_' + curve_blocks[i][j];
			curve_row.setAttribute("onclick", "selectObject('curve_point'," + curve_blocks[i][j] + ");");

			var curve_point_y = document.createElement('div');
			curve_point_y.className = 'semilandmark_point_col';
			curve_point_y.id = 'curve_point_y_' + curve_blocks[i][j];
			curve_point_y.innerHTML = curve_points[curve_blocks[i][j]].y;
			curve_row.appendChild(curve_point_y);

			var curve_point_x = document.createElement('div');
			curve_point_x.className = 'semilandmark_point_col';
			curve_point_x.id = 'curve_point_x_' + curve_blocks[i][j];
			curve_point_x.innerHTML = curve_points[curve_blocks[i][j]].x;
			curve_row.appendChild(curve_point_x);

			document.getElementById('curve_block_' + i).appendChild(curve_row);
		}

		var curve_end_div = document.createElement('div');
		curve_end_div.className = 'curve_ends';
		curve_end_div.id = 'curve_end_' + i;		
		curve_end_div.innerHTML = init_params.curves_ref[i][2];
		curve_end_div.setAttribute("onclick", "selectObject('curve_end'," + i + ");");
		document.getElementById('curve_container_' + i).appendChild(curve_end_div);

		var curve_row_end = document.createElement('div');
		curve_row_end.className = 'curve_point_row';
		curve_row_end.id = 'curve_row_end_' + i;		
		document.getElementById('curve_end_' + i).appendChild(curve_row_end);

		var curve_end_y = document.createElement('div');
		curve_end_y.className = 'semilandmark_point_col';
		curve_end_y.id = 'curve_end_y_' + i;
		curve_end_y.innerHTML = curve_end[i].y;
		document.getElementById('curve_row_end_' + i).appendChild(curve_end_y);

		var curve_end_x = document.createElement('div');
		curve_end_x.className = 'semilandmark_point_col';
		curve_end_x.id = 'curve_end_x_' + i;
		curve_end_x.innerHTML = curve_end[i].x;
		document.getElementById('curve_row_end_' + i).appendChild(curve_end_x);		
	}
}

function printLandmarkTable(){

	if(init_params.landmarks_ref === undefined) return;

	var i, j, n;
	for(i = 0;i < init_params.landmarks_ref.length;i++){

		// Check if landmark is also defined curve start and end point - create dummy values
		var x = landmarks[i].x;
		var y = landmarks[i].y;
		if(init_params.curves_ref !== undefined){
			for(j = 0;j < init_params.curves_ref.length;j++){
				if(curve_also_landmark[j].start == i && curve_start[j].x !== '-'){x = curve_start[j].x;y = curve_start[j].y;}
				if(curve_also_landmark[j].end == i && curve_end[j].x !== '-'){x = curve_end[j].x;y = curve_end[j].y;}
			}
		}

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
		landmark_col_x.innerHTML = x;
		document.getElementById('landmark_row_' + i).appendChild(landmark_col_x);

		var landmark_col_y = document.createElement('div');
		landmark_col_y.className = 'landmark_table_col2_' + n;
		landmark_col_y.id = 'landmark_col_y_' + i;
		landmark_col_y.innerHTML = y;
		document.getElementById('landmark_row_' + i).appendChild(landmark_col_y);
	}
}

function proximityCheck(x1,y1,x2,y2,m){
	if(x1 > (parseFloat(x2) - m) && x1 < (parseFloat(x2) + m) && y1 > (parseFloat(y2) - m) && y1 < (parseFloat(y2) + m)){return 1;}
	return 0;
}

function reDrawObjects(){

	var i;

	// RE-DRAW LANDMARKS
	for(i = 0;i < landmarks.length;i++) drawMarker('landmark', i, landmarks[i].x, landmarks[i].y);

	// RE-DRAW CURVES
	for(i = 0;i < curve_points.length;i++) drawMarker('curve_point', i, curve_points[i].x, curve_points[i].y);
	for(i = 0;i < curve_blocks.length;i++){
		if(start_dupl[i].num[0] == i && start_dupl[i].type[0] == 'start'){drawMarker('curve_start', i, curve_start[i].x, curve_start[i].y);}
		if(start_dupl[i].num[0] == i && start_dupl[i].type[0] == 'end'){drawMarker('curve_end', i, curve_start[i].x, curve_start[i].y);}
		if(end_dupl[i].num[0] == i && end_dupl[i].type[0] == 'start'){drawMarker('curve_start', i, curve_end[i].x, curve_end[i].y);}
		if(end_dupl[i].num[0] == i && end_dupl[i].type[0] == 'end'){drawMarker('curve_end', i, curve_end[i].x, curve_end[i].y);}		
		drawMarker('curve_start', i, curve_start[i].x, curve_start[i].y);
		drawMarker('curve_end', i, curve_end[i].x, curve_end[i].y);
	}

	// RE-DRAW CHECKERBOARD CORNERS
	for(i = 0;i < corners.length;i++) drawMarker('corner', i, corners[i].x, corners[i].y);

	// RE-DRAW RULER POINTS
	for(i = 0;i < rulers.length;i++) drawMarker('ruler', i, rulers[i].x, rulers[i].y);
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

	if(e.wheelDelta){ // SAFARI, CHROME
	}
}

function selectInitialLandmark(){

	var i;

	if(landmarks.length == 0){
		current_landmark = "NA";
		return;
	}

	for(i = 0;i < landmarks.length;i++){

		if(landmarks[i].x == '-') continue;

		selectObject('landmark', i)
		//current_landmark = i;
		break
	}

	mousedown = 0;
}

function selectObject(type, num){

	var i;

	// UNSELECT ALL LANDMARKS
	for(i = 0;i < landmarks.length;i++){
		document.getElementById('landmark_row_' + i).setAttribute('class', 'landmark_table_row');
		changeMarkerStyle('landmark', i, landmark_color_blur);
	}

	// UNSELECT ALL CURVES
	for(i = 0;i < curve_blocks.length;i++){
		document.getElementById('curve_row_start_' + i).setAttribute("class", 'curve_point_row');
		document.getElementById('curve_start_' + i).setAttribute("class", 'curve_ends');
		document.getElementById('curve_row_end_' + i).setAttribute("class", 'curve_point_row');
		document.getElementById('curve_end_' + i).setAttribute("class", 'curve_ends');
		changeMarkerStyle('curve_start', i, control_point_color_blur);
		changeMarkerStyle('curve_end', i, control_point_color_blur);
	}
	for(i = 0;i < curve_points.length;i++){
		document.getElementById('curve_row_' + i).setAttribute('class', 'semilandmark_point_row');
		changeMarkerStyle('curve_point', i, control_point_color_blur);
	}		

	// UNSELECT ALL RULER POINTS
	for(i = 0;i < rulers.length;i++){
		document.getElementById('ruler_row_' + i).setAttribute('class', 'scaling_table_row');
		changeMarkerStyle('ruler', i, landmark_color_blur);
	}

	// DETERMINE OBJECT TYPE
	var object_type = 'landmark';
	if(type == 'landmark'){
		if(init_params.curves_ref !== undefined){
			for(i = 0;i < init_params.curves_ref.length;i++){
				if(curve_also_landmark[i].start == num && curve_start[i].x !== '-'){
					object_type = 'curve and landmark';
					type = 'curve_start';
					num = i;
					break;
				}
				if(curve_also_landmark[i].end == num && curve_end[i].x !== '-'){
					object_type = 'curve and landmark';
					type = 'curve_end';
					num = i;
					break;
				}
			}
		}
	}else if(type == 'curve_point'){
		var object_type = 'curve point';		
	}else if(type == 'curve_start' || type == 'curve_end'){
		var object_type = 'curve and landmark';		
	}else if(type == 'ruler'){
		var object_type = 'ruler';
	}

	var unset_all = false;
	if(type == 'landmark' && num == current_landmark) unset_all = true;
	if(type == 'curve_'+current_curve_point_type && num == current_curve_point) unset_all = true;
	if(type == 'ruler' && num == current_ruler) unset_all = true;

	// UNSET CURRENT CURVES AND LANDMARKS
	current_landmark = "NA";
	current_curve_point = "NA";
	current_curve_point_type = "NA";
	current_ruler = "NA";

	if(unset_all){
		document.getElementById('curr_object').innerHTML = 'None selected';
		return;
	}

	// LANDMARK ONLY OR LANDMARK WITH UNDEFINED CURVE POINT
	if(object_type == 'landmark'){
		current_landmark = num;
		document.getElementById('landmark_row_' + num).setAttribute('class', 'landmark_table_row_select');
		changeMarkerStyle('landmark', num, landmark_color_focus);
		document.getElementById('curr_object').innerHTML = init_params.landmarks_ref[num];
		return;
	}

	// CURVE POINT
	if(object_type == 'curve point'){
		current_curve_point = num;
		current_curve_point_type = "point";
		document.getElementById('curve_row_' + num).setAttribute("class", 'semilandmark_point_row_select');
		changeMarkerStyle('curve_point', num, control_point_color_focus);
		document.getElementById('curr_object').innerHTML = init_params.curves_ref[curve_points[num].parent][0] + ' curve point';
		return;
	}

	// LANDMARK AND CURVE
	if(object_type == 'curve and landmark'){
		current_curve_point = num;

		if(type == 'curve_start'){
			current_curve_point_type = "start";
			if(document.getElementById('landmark_row_' + curve_also_landmark[num].start))
				document.getElementById('landmark_row_' + curve_also_landmark[num].start).setAttribute('class', 'landmark_table_row_select');
			
			// SELECT ALL DUPLICATES
			for(i = 0;i < start_dupl[num].type.length;i++){
				document.getElementById('curve_row_' + start_dupl[num].type[i] + '_' + start_dupl[num].num[i]).setAttribute("class", 'curve_point_row_select');
				document.getElementById('curve_' + start_dupl[num].type[i] + '_' + start_dupl[num].num[i]).setAttribute("class", 'curve_ends_select');
				changeMarkerStyle('curve_' + start_dupl[num].type[i], start_dupl[num].num[i], control_point_color_focus);
			}

			document.getElementById('curr_object').innerHTML = init_params.curves_ref[num][1];
			return;
		}

		if(type == 'curve_end'){
			current_curve_point_type = "end";
			if(document.getElementById('landmark_row_' + curve_also_landmark[num].end))
				document.getElementById('landmark_row_' + curve_also_landmark[num].end).setAttribute('class', 'landmark_table_row_select');

			// SELECT ALL DUPLICATES
			for(i = 0;i < end_dupl[num].type.length;i++){
				document.getElementById('curve_row_' + end_dupl[num].type[i] + '_' + end_dupl[num].num[i]).setAttribute("class", 'curve_point_row_select');
				document.getElementById('curve_' + end_dupl[num].type[i] + '_' + end_dupl[num].num[i]).setAttribute("class", 'curve_ends_select');
				changeMarkerStyle('curve_' + end_dupl[num].type[i], end_dupl[num].num[i], control_point_color_focus);			
			}

			document.getElementById('curr_object').innerHTML = init_params.curves_ref[num][2];
		}
	}

	if(object_type == 'ruler'){
		current_ruler = num;
		document.getElementById('ruler_row_' + num).setAttribute('class', 'scaling_table_row_select');
		changeMarkerStyle('ruler', num, landmark_color_focus);
		document.getElementById('curr_object').innerHTML = 'Ruler point ' + (num+1);
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

	// UPDATE FORM ELEMENTS ON PAGE TO MATCH COOKIE VALUES
	if(!name){

		if(getCookie('auto_advance_checkbox') == 'T') document.getElementById('auto_advance_checkbox').checked = true;
		auto_advance = document.getElementById('auto_advance_checkbox').checked;

		if(getCookie('copy_landmarks_checkbox') == 'T') document.getElementById('copy_landmarks_checkbox').checked = true;
		if(getCookie('copy_curves_checkbox') == 'T') document.getElementById('copy_curves_checkbox').checked = true;
		if(getCookie('copy_ruler_points_checkbox') == 'T') document.getElementById('copy_ruler_points_checkbox').checked = true;
		if(getCookie('copy_corners_checkbox') == 'T') document.getElementById('copy_corners_checkbox').checked = true;
		if(getCookie('copy_scaling_checkbox') == 'T') document.getElementById('copy_scaling_checkbox').checked = true;

		return
	}

	// UPDATE COOKIE VALUES TO MATCH USER SELECTIONS
	//if(name.id == 'copy_landmarks_checkbox' || name.id == 'copy_curves_checkbox' || name.id == 'copy_ruler_points_checkbox' || name.id == 'copy_scaling_checkbox' || name.id == 'auto_advance_checkbox'){
	
	// SET VALUE
	if(document.getElementById(name.id).checked){value = 'T'}else{value = 'F'}

	// CREATE COOKIE
	createCookie(name.id, value)

	if(name.id == 'auto_advance_checkbox') auto_advance = document.getElementById('auto_advance_checkbox').checked;

	//}

}

function submit(submit_params){

	// ADD FROM BROWSER TAG TO DISTINGUISH FROM INITIAL RUN
	submit_params.fromBrowser = true;

	// ADD SETTINGS
	submit_params.copy_landmarks = document.getElementById('copy_landmarks_checkbox').checked;
	submit_params.copy_curves = document.getElementById('copy_curves_checkbox').checked;
	submit_params.copy_ruler_points = document.getElementById('copy_ruler_points_checkbox').checked;
	submit_params.copy_corners = document.getElementById('copy_corners_checkbox').checked;
	submit_params.copy_scaling = document.getElementById('copy_scaling_checkbox').checked;

	// ADD DATA TO SAVE WITH LANDMARKS
	submit_params.image_name = init_params.img_name;
	submit_params.image_file = init_params.img_file;
	submit_params.image_id = init_params.image_id;
	submit_params.submit_ct = submit_ct + 1;

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

	var updateInterval = setInterval(function(){

		//var seconds = new Date().getTime() / 1000;
		//document.getElementById('current_process').innerHTML = seconds;

		// READ JSON STRING
		if(document.getElementById('text_output').innerHTML !== '') server_out = JSON.parse(document.getElementById('text_output').innerHTML);
		//alert(document.getElementById('text_output').innerHTML);
		
		// CHECK TO MAKE SURE SUBMIT CAPTURE IS FROM MOST RECENT SUBMISSION
		if(submit_ct == server_out.submit_ct){

			//alert(submit_ct + ' : ' + server_out.submit_ct);
			//alert(server_out.corners);
			
			// UPDATE CURRENT STATUS
			//alert(server_out.update_status);
			//document.getElementById('current_process').innerHTML = document.getElementById('text_output').innerHTML;
			document.getElementById('current_process').innerHTML = server_out.update_status;
			setTimeout(function(){document.getElementById('current_process').innerHTML = ''}, 7000);

			// IF CORNERS ARE FOUND
			if(server_out.corners !== undefined){
				//alert(server_out.corners);

				// DRAW CHECKERBOARD CORNERS
				drawCheckerboardCorners(server_out.corners)

				// SET CHECKERBOARD SQUARE SIZE
				checker_square_pixel = Number(server_out.checker_square_pixel);

				unsaved_corners = 'TRUE';

				updateScaling()
			}

			// UPDATE SCALING
			if(server_out.ruler_interval_pixels !== undefined){
				ruler_interval_pixels = Number(server_out.ruler_interval_pixels);
				updateScaling()
			}

			clearInterval(updateInterval);
		}
		iter++;
		if(iter > 3000) clearInterval(updateInterval);
	}, 100);
}

function submitShapes(what){
	
	var i, j;
	var submit_params = {};
	submit_params.submit_shapes = true;

	if(!isNaN(ruler_interval_world)) submit_params.ruler_interval_world = ruler_interval_world;
	if(!isNaN(ruler_interval_pixels)) submit_params.ruler_interval_pixels = ruler_interval_pixels;
	if(!isNaN(checkerboard_nx)) submit_params.checkerboard_nx = checkerboard_nx;
	if(!isNaN(checkerboard_ny)) submit_params.checkerboard_ny = checkerboard_ny;
	if(!isNaN(checker_square_pixel)) submit_params.checker_square_pixel = checker_square_pixel;
	if(!isNaN(checker_square_world)) submit_params.checker_square_world = checker_square_world;
	submit_params.scaling_units = scaling_units;

	if(what == 'all'){

		submit_params.ruler_points = [];
		submit_params.corners = [];

		// COPY RULER POINTS TO JSON STRUCTURE
		for(var i = 0;i < rulers.length;i++) submit_params.ruler_points[i] = [rulers[i].x, rulers[i].y];

		unsaved_ruler_points = 'FALSE';

		// COPY CHECKERBOARD CORNERS TO JSON STRUCTURE
		for(var i = 0;i < corners.length;i++) submit_params.corners[i] = [corners[i].x, corners[i].y];
		
		unsaved_corners = 'FALSE';
	}

	if(what == 'landmarks' || what == 'all'){
		submit_params.landmarks = [];

		// FIND LANDMARK TABLE
		var landmark_table = document.getElementById('landmark_table_container');

		// READ LANDMARK TABLE
		j = 0;
		for(i = 0;i < landmark_table.childNodes.length;i++){
			if(landmark_table.childNodes[i].childNodes[1].innerHTML == '-') continue;

			submit_params.landmarks[j] = [
				landmark_table.childNodes[i].childNodes[0].innerHTML, 
				landmark_table.childNodes[i].childNodes[1].innerHTML,
				landmark_table.childNodes[i].childNodes[2].innerHTML];
			j++;
		}

		submit_params.submit_landmarks = true;
		unsaved_landmarks = 'FALSE';
	}

	if(what == 'curves' || what == 'all'){
		submit_params.control_points = [];

		// FIND CURVE TABLE
		var curve_table = document.getElementById('curve_table_container');

		// READ CURVE TABLE
		var m = 0;
		for(i = 0;i < curve_table.childNodes.length;i++){

			if(document.getElementById('curve_start_x_' + i).innerHTML == '-') continue;
			if(document.getElementById('curve_end_x_' + i).innerHTML == '-') continue;
		
			var curve_block = document.getElementById('curve_block_' + i);
			read_curve_points = new Array();

			read_curve_points[read_curve_points.length] = document.getElementById('curve_container_' + i).name;
			read_curve_points[read_curve_points.length] = document.getElementById('curve_start_x_' + i).innerHTML;
			read_curve_points[read_curve_points.length] = document.getElementById('curve_start_y_' + i).innerHTML;

			for(j = 0;j < curve_block.childNodes.length;j++){
				if(curve_block.childNodes[j].childNodes[0].innerHTML == '-') continue;

				read_curve_points[read_curve_points.length] = curve_block.childNodes[j].childNodes[1].innerHTML;
				read_curve_points[read_curve_points.length] = curve_block.childNodes[j].childNodes[0].innerHTML;
			}

			read_curve_points[read_curve_points.length] = document.getElementById('curve_end_x_' + i).innerHTML;
			read_curve_points[read_curve_points.length] = document.getElementById('curve_end_y_' + i).innerHTML;

			submit_params.control_points[m] = read_curve_points;
			m++;
		}

		submit_params.submit_curves = true;
		unsaved_curves = 'FALSE';
	}	
	
	submit(submit_params);
}

function svgSetXYWH(el, x, y, w, h) {
	el.setAttribute("x", x);
	el.setAttribute("y", y);
	el.setAttribute("width", w);
	el.setAttribute("height", h);
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
	}else{

		if(!isNaN(checker_square_pixel) && !isNaN(checker_square_world)){
			scaling_ppw = checker_square_pixel / checker_square_world;
			scaling_wpp = checker_square_world / checker_square_pixel;
		}else{
			scaling_ppw = "NA"
			scaling_wpp = "NA"
		}
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

function updateSubmitButtons(){

	// IS THERE PREVIOUS OR NEXT IMAGE?
	if(String(init_params.prev_img).toLowerCase() == 'false'){
		document.getElementById('previous_image').className = 'submit_button_off';
		document.getElementById('previous_image').name = '';
		document.getElementById('previous_image').onclick = '';		
		document.getElementById('previous_image').title = '';
	}
	if(String(init_params.next_img).toLowerCase() == 'false'){
		document.getElementById('next_image').className = 'submit_button_off';
		document.getElementById('next_image').name = '';
		document.getElementById('next_image').onclick = '';		
		document.getElementById('next_image').title = '';
	}

	// ARE CURVES/LANDMARKS PRESENT?
	if(init_params.landmarks_ref === undefined || init_params.landmarks_file === undefined){
		document.getElementById('submit_landmarks_button').className = 'submit_button_off';
		document.getElementById('submit_landmarks_button').name = '';
		document.getElementById('submit_landmarks_button').title = '';
		document.getElementById('submit_landmarks_button').onclick = '';

		if(init_params.shapes_file === undefined){
			document.getElementById('submit_all_button').className = 'submit_button_off';
			document.getElementById('submit_all_button').name = '';
			document.getElementById('submit_all_button').title = '';
			document.getElementById('submit_all_button').onclick = '';
		}
	}

	if(init_params.curves_ref === undefined || init_params.control_points_file === undefined){
		document.getElementById('submit_curves_button').className = 'submit_button_off';
		document.getElementById('submit_curves_button').name = '';
		document.getElementById('submit_curves_button').title = '';
		document.getElementById('submit_curves_button').onclick = '';

		if(init_params.shapes_file === undefined){
			document.getElementById('submit_all_button').className = 'submit_button_off';
			document.getElementById('submit_all_button').name = '';
			document.getElementById('submit_all_button').title = '';
			document.getElementById('submit_all_button').onclick = '';
		}
	}
}

/* ----------------------------------------------------------------------------------------------------------------------*/
/* -------------------------------------------------- STYLE FUNCTIONS ---------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------*/

var drag_div_mouse_down;

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
			div_view_entries.childNodes[i].style.display = "";
			//alert(div_view_entries.childNodes[i].id);
		}else{
			if(div_view_entries.childNodes[i].getAttribute('class') == div_id.getAttribute('class')){div_view_entries.childNodes[i].style.display = "none";}
		}
	}


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

/* ----------------------------------------------------------------------------------------------------------------------*/
/* -------------------------------------------------- FILE FUNCTIONS ----------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------*/

function createCookie(name,value,sec){
	if (sec) {
		var date = new Date();
		date.setTime(date.getTime()+(sec*1000));
		var expires = "; expires="+date.toGMTString();
	}
	else var expires = "";
	document.cookie = name+"="+value+expires+"; path=/";
}

function deleteCookie(name) {
	createCookie(name,"",-1);
}

function getCookie(Name){ 
	var re=new RegExp(Name+"=[^;]+", "i"); //construct RE to search for target name/value pair
	if (document.cookie.match(re)) //if cookie found
		return document.cookie.match(re)[0].split("=")[1] //return its value
	return false;
}

function pause(milliseconds) {
	var dt = new Date();
	while ((new Date()) - dt <= milliseconds) { /* Do nothing */ }
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

