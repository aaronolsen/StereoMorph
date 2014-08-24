init_params <- list()
init_params[['app_dir']] <- c('/Applications/XAMPP/xamppfiles/htdocs/data_analysis/r_package_development/StereoMorph/inst/extdata/apps/digitizeImage')

init_params[['prev_wd']] <- c('/Users/aaron/Documents/Research/Manuscripts/StereoMorph 2014/web tutorial/StereoMorph Tutorial')

init_params[['img_name']] <- c('obj1_a2_v1.JPG')

init_params[['img_size']] <- c('2785458')

init_params[['auto_advance']] <- c('TRUE')

init_params[['img_file']] <- c('Object images/obj1_a2_v1.JPG')

init_params[['control_points_file']] <- c('Control points/obj1_a2_v1.txt')

init_params[['curve_points_file']] <- c('Curve points 2D/obj1_a2_v1.txt')

init_params[['landmark_color_blur']] <- c('blue')

init_params[['landmark_color_focus']] <- c('green')

init_params[['curve_color_blur']] <- c('purple')

init_params[['curve_point_color_blur']] <- c('purple')

init_params[['curve_point_color_focus']] <- c('red')

init_params[['landmark_radius']] <- c('4')

init_params[['curve_point_radius']] <- c('4')

init_params[['marker_stroke_width']] <- c('1')

init_params[['curves_ref']][[1]] <- c('tomium_L','upperbeak_tip','upperbeak_tomium_prox_L')
init_params[['curves_ref']][[2]] <- c('tomium_R','upperbeak_tip','upperbeak_tomium_prox_R')
init_params[['curves_ref']][[3]] <- c('pterygoid_crest_L','pterygoid_crest_ant_L','pterygoid_crest_post_L')
init_params[['curves_ref']][[4]] <- c('pterygoid_crest_R','pterygoid_crest_ant_R','pterygoid_crest_post_R')
init_params[['curves_ref']][[5]] <- c('upper_bill_culmen','upperbeak_tip','upperbill_cranium')

init_params[['prev_img']] <- c('FALSE')

init_params[['next_img']] <- c('FALSE')

init_params[['control_points']][[1]] <- c('tomium_R','3102','1771','3108','1691','3038','1640','3029','1631','3010','1626','2988','1617','2935','1589','2842','1555','2677','1545','2414','1531','2135','1494','1990','1469','1845','1448','1782','1439','1750','1485')
init_params[['control_points']][[2]] <- c('pterygoid_crest_R','1140','1672','1141','1622','1057','1606','929','1564','884','1453')


json_string <- '{"app_dir":["/Applications/XAMPP/xamppfiles/htdocs/data_analysis/r_package_development/StereoMorph/inst/extdata/apps/digitizeImage"], "prev_wd":["/Users/aaron/Documents/Research/Manuscripts/StereoMorph 2014/web tutorial/StereoMorph Tutorial"], "img_name":["obj1_a2_v1.JPG"], "img_size":["2785458"], "auto_advance":["TRUE"], "img_file":["Object images/obj1_a2_v1.JPG"], "control_points_file":["Control points/obj1_a2_v1.txt"], "curve_points_file":["Curve points 2D/obj1_a2_v1.txt"], "landmark_color_blur":["blue"], "landmark_color_focus":["green"], "curve_color_blur":["purple"], "curve_point_color_blur":["purple"], "curve_point_color_focus":["red"], "landmark_radius":["4"], "curve_point_radius":["4"], "marker_stroke_width":["1"], "curves_ref":[["tomium_L", "upperbeak_tip", "upperbeak_tomium_prox_L"], ["tomium_R", "upperbeak_tip", "upperbeak_tomium_prox_R"], ["pterygoid_crest_L", "pterygoid_crest_ant_L", "pterygoid_crest_post_L"], ["pterygoid_crest_R", "pterygoid_crest_ant_R", "pterygoid_crest_post_R"], ["upper_bill_culmen", "upperbeak_tip", "upperbill_cranium"]],"prev_img":["FALSE"], "next_img":["FALSE"], "control_points":[["tomium_R", "3102", "1771", "3108", "1691", "3038", "1640", "3029", "1631", "3010", "1626", "2988", "1617", "2935", "1589", "2842", "1555", "2677", "1545", "2414", "1531", "2135", "1494", "1990", "1469", "1845", "1448", "1782", "1439", "1750", "1485"], ["pterygoid_crest_R", "1140", "1672", "1141", "1622", "1057", "1606", "929", "1564", "884", "1453"]]}'
