#'@title Rasterize an OBJ file
#'
#'@param line_info The mesh object.
#'@param filename Default `NULL`. Filename to save the image. If `NULL`, the image will be plotted.
#'@param width Default `400`. Width of the rendered image.
#'@param height Default `400`. Width of the rendered image.
#'@param parallel Default `TRUE`. Whether to use parallel processing.
#'@param fov Default `20`. Width of the rendered image.
#'@param lookfrom Default `c(0,0,10)`. Camera location.
#'@param lookat Default `NULL`. Camera focal position, defaults to the center of the model.
#'@param camera_up Default `c(0,1,0)`. Camera up vector.
#'@param type Default `diffuse`. Shader type. Other options: `vertex` (Gouraud shading), `phong`, and `color` (no lighting).
#'@param color Default `darkred`. Color of model if no material file present (or for faces using the default material).
#'@param background Default `white`. Background color.
#'@param debug Default `"none"`.
#'@param near_plane Default `0.1`.
#'@param far_plane Default `100`.
#'@param block_size Default `4`. 
#'
#'@return Rasterized image.
#'@export
#'@examples
#'#Let's load the cube OBJ file included with the package
#'
#'cube_model = read_obj(cube_obj())
#'
#'rasterize_mesh(cube_model,lookfrom=c(2,4,10), 
#'               light_info = directional_light(direction=c(0.5,1,0.7)))
#'
#'#Flatten the cube, translate downwards, and set to grey
#'base_model = cube_model %>% 
#'  scale_mesh(scale=c(5,0.2,5)) %>%
#'  translate_mesh(c(0,-0.1,0)) %>% 
#'  set_material(diffuse="grey80") 
#'
#'rasterize_mesh(base, lookfrom=c(2,4,10), 
#'               light_info = directional_light(direction=c(0.5,1,0.7)))
#'
#'#load the R OBJ file, scale it down, color it blue, and add it to the grey base
#'r_model = r_obj() %>% 
#'  read_obj() %>% 
#'  scale_mesh(scale=0.5) %>% 
#'  set_material(diffuse="dodgerblue") %>% 
#'  add_shape(base_model)
#'
#'rasterize_mesh(r_model, lookfrom=c(2,4,10), 
#'               light_info = directional_light(direction=c(0.5,1,0.7)))
#'               
#'#Zoom in and turn on shadow mapping
#'rasterize_mesh(r_model, lookfrom=c(2,4,10), fov=10,shadow_map = TRUE,
#'               light_info = directional_light(direction=c(0.5,1,0.7)))
#'
#'#Include the resolution (4x) of the shadow map for less pixellation around the edges
#'#Also decrease the shadow_map_bias slightly to remove the "peter panning" floating shadow effect
#'rasterize_mesh(r_model, lookfrom=c(2,4,10), fov=10,
#'               shadow_map = TRUE, shadow_map_dims=4, shadow_map_bias=0.001,
#'               light_info = directional_light(direction=c(0.5,1,0.7)))
#'               
#'#Add some more directional lights and change their color
#' lights = directional_light(c(0.7,1.1,-0.9),color = "orange",intensity = 1) %>% 
#'            add_light(directional_light(c(0.7,1,1),color = "dodgerblue",intensity = 1)) %>% 
#'            add_light(directional_light(c(2,4,10),color = "white",intensity = 0.5))
#'            
#'rasterize_mesh(r_model, lookfrom=c(2,4,10), fov=10,
#'               shadow_map = TRUE, shadow_map_dims=4, shadow_map_bias=0.001,
#'               light_info = lights)
#'               
#'#Add some point lights
#'lights_p = lights %>% 
#'  add_light(point_light(position=c(-1,1,0),color="red", intensity=10)) %>% 
#'  add_light(point_light(position=c(1,1,0),color="purple", intensity=10)) 
#'
#'rasterize_mesh(r_model, lookfrom=c(2,4,10), fov=10,
#'               shadow_map = TRUE, shadow_map_dims=4, shadow_map_bias=0.001,
#'               light_info = lights_p)
#'               
#'#change the camera position
#'rasterize_mesh(r_model, lookfrom=c(-2,2,-10), fov=10,
#'               shadow_map = TRUE, shadow_map_dims=4, shadow_map_bias=0.001,
#'               light_info = lights_p)
#'               
#'#Add a spiral of lines around the model by generating a matrix of line segments
#'#Each pair of rows represent a single segment. Lines are ignored by shadows.
#'t = seq(0,8*pi,length.out=361)
#'
#'line_mat = matrix(0,nrow=720,ncol=3)
#'for(i in 1:360) {
#' line_mat[(2*i-1),] = c(0.5*sin(t[i]), t[i]/(8*pi), 0.5*cos(t[i]))
#' line_mat[(2*i),]   = c(0.5*sin(t[i+1]), t[i+1]/(8*pi), 0.5*cos(t[i+1]))
#'}
#'
#'rasterize_mesh(r_model, lookfrom=c(2,4,10), fov=10, line_info = line_mat,
#'               shadow_map = TRUE, shadow_map_dims=4, shadow_map_bias=0.001,
#'               light_info = lights)
rasterize_lines  = function(line_info = NULL, 
                           filename = NA, width=400, height=400, 
                           alpha_line = 1.0,
                           parallel = TRUE,
                           fov=20,lookfrom=c(0,0,10),lookat=NULL, camera_up = c(0,1,0), #Sanitize lookfrom and lookat inputs
                           color="red", background = "black", 
                           debug = "none", 
                           near_plane = 0.1, far_plane = 100, 
                           shader = "default", 
                           block_size = 4, 
                           shape = NULL, 
                           line_offset = 0.00001,
                           ortho_dims = c(1,1), 
                           bloom = FALSE, 
                           antialias_lines = TRUE) {
  if(is.null(line_info)) {
    stop("no lines info passed to argument")
  }
  lineranges= rbind(line_scene[,1:3],line_scene[,4:6])
  bounds = as.vector(t(apply(lineranges,2,range)))
  
  if(is.null(lookat)) {
    lookat = (bounds[1:3] + bounds[4:6])/2
    message(sprintf("Setting `lookat` to: c(%0.2f, %0.2f, %0.2f)",lookat[1],lookat[2],lookat[3]))
  }
  
  if(!is.null(options("cores")[[1]])) {
    numbercores = options("cores")[[1]]
  } else {
    numbercores = parallel::detectCores()
  }
  if(!parallel) {
    numbercores = 1
  }
  
  color = convert_color(color)
  bg_color = convert_color(background)
  
  imagelist = rasterize_lines_rcpp(line_mat=line_info,
                        nx=width,
                        ny=height,
                        model_color = color,
                        lookfrom=lookfrom,
                        lookat=lookat,
                        fov=fov,
                        near_clip=near_plane, far_clip=far_plane,
                        bounds=bounds, camera_up=camera_up, 
                        alpha_line=alpha_line, line_offset=line_offset,
                        ortho_dims=ortho_dims,
                        aa_lines=antialias_lines)
  if(debug == "normals") {
    norm_array = array(0,dim=c(dim(imagelist$r)[2:1],3))
    norm_array[,,1] = (imagelist$normalx+1)/2
    norm_array[,,2] = (imagelist$normaly+1)/2
    norm_array[,,3] = (imagelist$normalz+1)/2
    norm_array = rayimage::render_reorient(norm_array,transpose = TRUE, flipx = TRUE)
    rayimage::plot_image(norm_array)
    return(invisible(norm_array))
  }
  if(debug == "depth") {
    depth_array = array(0,dim=c(dim(imagelist$r)[2:1],3))
    depth_array[,,1] = (imagelist$depth)
    depth_array[,,2] = (imagelist$depth)
    depth_array[,,3] = (imagelist$depth)
    depth_array = rayimage::render_reorient(depth_array,transpose = TRUE, flipx = TRUE)
    depth_array[is.infinite(depth_array)] = 1.2
    scale_factor = max(depth_array) - min(depth_array)
    depth_array = (depth_array - min(depth_array))/scale_factor
    rayimage::plot_image(depth_array)
    return(invisible(depth_array))
  }
  if(debug == "position") {
    pos_array = array(0,dim=c(dim(imagelist$r)[2:1],3))
    imagelist$positionx = scales::rescale(imagelist$positionx,to=c(0,1))
    imagelist$positiony = scales::rescale(imagelist$positiony,to=c(0,1))
    imagelist$positionz = scales::rescale(imagelist$positionz,to=c(0,1))
    
    pos_array[,,1] = (imagelist$positionx)
    pos_array[,,2] = (imagelist$positiony)
    pos_array[,,3] = (imagelist$positionz)
    pos_array = rayimage::render_reorient(pos_array,transpose = TRUE, flipx = TRUE)
    pos_array[is.infinite(pos_array)] = 1
    rayimage::plot_image(pos_array)
    return(invisible(pos_array))
  }
  if(debug == "uv") {
    uv_array = array(0,dim=c(dim(imagelist$r)[2:1],3))
    uv_array[,,1] = (imagelist$uvx)
    uv_array[,,2] = (imagelist$uvy)
    uv_array[,,3] = (imagelist$uvz)
    uv_array = rayimage::render_reorient(uv_array,transpose = TRUE, flipx = TRUE)
    rayimage::plot_image(uv_array)
    return(invisible(uv_array))
  }
  
  imagelist$r[is.infinite(imagelist$depth)] = bg_color[1]
  imagelist$g[is.infinite(imagelist$depth)] = bg_color[2]
  imagelist$b[is.infinite(imagelist$depth)] = bg_color[3]
  
  retmat = array(0,dim=c(dim(imagelist$r)[2:1],3))
  retmat[,,1] = rayimage::render_reorient(imagelist$r,transpose = TRUE, flipx = TRUE)
  retmat[,,2] = rayimage::render_reorient(imagelist$g,transpose = TRUE, flipx = TRUE)
  retmat[,,3] = rayimage::render_reorient(imagelist$b,transpose = TRUE, flipx = TRUE)
  if(bloom) {
    retmat = rayimage::render_convolution(retmat, min_value = 1)
  }
  
  retmat[retmat > 1] = 1
  if(is.na(filename)) {
    rayimage::plot_image(retmat)
  } else {
    rayimage:::save_png(retmat, filename = filename)
  }
  return(invisible(retmat))
}
