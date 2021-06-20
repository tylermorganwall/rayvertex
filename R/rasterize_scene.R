#'@title Rasterize Scene
#'
#'@description Render a 3D scene with meshes, lights, and lines using a software rasterizer.
#'
#'@param scene The scene object.
#'@param filename Default `NULL`. Filename to save the image. If `NULL`, the image will be plotted.
#'@param width Default `400`. Width of the rendered image.
#'@param height Default `400`. Width of the rendered image.
#'@param line_info Default `NULL`. Matrix of line segments to add to the scene. Number of rows must be a multiple of 2.
#'@param alpha_line Default `1`. Line transparency.
#'@param parallel Default `TRUE`. Whether to use parallel processing.
#'@param fov Default `20`. Width of the rendered image.
#'@param lookfrom Default `c(0,0,10)`. Camera location.
#'@param lookat Default `NULL`. Camera focal position, defaults to the center of the model.
#'@param camera_up Default `c(0,1,0)`. Camera up vector.
#'@param light_info Default `directional_light()`. Description of scene lights, generated with the `point_light()` and
#'`directional_light()` functions.
#'@param type Default `diffuse`. Shader type. Other options: `vertex` (Gouraud shading), `phong`, and `color` (no lighting).
#'@param color Default `darkred`. Color of model if no material file present (or for faces using the default material).
#'@param background Default `white`. Background color.
#'@param tangent_space_normals Default `TRUE`.
#'@param shadow_map Default `FALSE`.
#'@param shadow_map_bias Default `0.005`.
#'@param shadow_map_intensity Default `0.5`.
#'@param shadow_map_dims Default `NULL`.
#'@param ssao Default `FALSE`. Whether to add screen-space ambient occlusion (SSAO) to the render.
#'@param ssao_intensity Default `10`. Intensity of the shadow map.
#'@param ssao_radius Default `0.1`. Radius to use when calculating the SSAO term.
#'@param tonemap Default `"none"`.
#'@param debug Default `"none"`.
#'@param near_plane Default `0.1`.
#'@param far_plane Default `100`.
#'@param shader Default `"default"`.
#'@param block_size Default `4`. 
#'@param shape Default `NULL`. The shape to render in the OBJ mesh. 
#'@param line_offset Default `0.0001`. Amount to offset lines towards camera to prevent z-fighting.
#'@param ortho_dimensions Default `c(1,1)`. Width and height of the orthographic camera. Will only be used if `fov = 0`. 
#'@param bloom Default `FALSE`. Whether to apply bloom to the image. If `TRUE`,
#' this performs a convolution of the HDR image of the scene with a sharp, long-tailed
#' exponential kernel, which does not visibly affect dimly pixels, but does result in emitters light
#' slightly bleeding into adjacent pixels. 
#'@param antialias_lines Default `TRUE`. Whether to anti-alias lines in the scene.
#'@param environment_map Default `""`. Image file to use as a texture for all reflective and refractive
#'materials in the scene, along with the background.
#'@param background_sharpness Default `1.0`. A number greater than zero but less than one indicating the sharpness
#'of the background image.
#'
#'@return Rasterized image.
#'@export
#'@examples
#'\dontshow{
#'options("cores"=1)
#'}
#'#Let's load the cube OBJ file included with the package
#'
#'\donttest{
#'rasterize_scene(cube_mesh(),lookfrom=c(2,4,10), 
#'               light_info = directional_light(direction=c(0.5,1,0.7)))
#'}
#'#Flatten the cube, translate downwards, and set to grey
#'base_model = cube_mesh() %>% 
#'  scale_mesh(scale=c(5,0.2,5)) %>%
#'  translate_mesh(c(0,-0.1,0)) %>% 
#'  set_material(diffuse="grey80") 
#'
#'\donttest{
#'rasterize_scene(base_model, lookfrom=c(2,4,10), 
#'               light_info = directional_light(direction=c(0.5,1,0.7)))
#'}
#'
#'#load the R OBJ file, scale it down, color it blue, and add it to the grey base
#'r_model = obj_mesh(r_obj()) %>% 
#'  scale_mesh(scale=0.5) %>% 
#'  set_material(diffuse="dodgerblue") %>% 
#'  add_shape(base_model)
#'  
#'\donttest{
#'rasterize_scene(r_model, lookfrom=c(2,4,10), 
#'               light_info = directional_light(direction=c(0.5,1,0.7)))
#' }
#'#Zoom in and reduce the shadow mapping intensity
#'\donttest{
#'rasterize_scene(r_model, lookfrom=c(2,4,10), fov=10,shadow_map = TRUE, shadow_map_intensity=0.3,
#'               light_info = directional_light(direction=c(0.5,1,0.7)))
#'}
#'
#'#Include the resolution (4x) of the shadow map for less pixellation around the edges
#'#Also decrease the shadow_map_bias slightly to remove the "peter panning" floating shadow effect
#'\donttest{
#'rasterize_scene(r_model, lookfrom=c(2,4,10), fov=10,
#'               shadow_map_dims=4, 
#'               light_info = directional_light(direction=c(0.5,1,0.7)))
#'}
#'
#'#Add some more directional lights and change their color
#' lights = directional_light(c(0.7,1.1,-0.9),color = "orange",intensity = 1) %>% 
#'            add_light(directional_light(c(0.7,1,1),color = "dodgerblue",intensity = 1)) %>% 
#'            add_light(directional_light(c(2,4,10),color = "white",intensity = 0.5))
#'\donttest{
#'rasterize_scene(r_model, lookfrom=c(2,4,10), fov=10,
#'               light_info = lights)
#'}
#'#Add some point lights
#'lights_p = lights %>% 
#'  add_light(point_light(position=c(-1,1,0),color="red", intensity=2)) %>% 
#'  add_light(point_light(position=c(1,1,0),color="purple", intensity=2)) 
#'\donttest{
#'rasterize_scene(r_model, lookfrom=c(2,4,10), fov=10,
#'               light_info = lights_p)
#'}
#'#change the camera position
#'\donttest{
#'rasterize_scene(r_model, lookfrom=c(-2,2,-10), fov=10,
#'               light_info = lights_p)
#'}
#'               
#'\donttest{        
#'#Add a spiral of lines around the model by generating a matrix of line segments
#' t = seq(0,8*pi,length.out=361)
#' line_mat = matrix(nrow=0,ncol=9)
#' 
#' for(i in 1:360) {
#'   line_mat = add_lines(line_mat,
#'                       generate_line(start = c(0.5*sin(t[i]), t[i]/(8*pi), 0.5*cos(t[i])),
#'                                     end  = c(0.5*sin(t[i+1]), t[i+1]/(8*pi), 0.5*cos(t[i+1]))))
#' }
#' 
#'rasterize_scene(r_model, lookfrom=c(2,4,10), fov=10, line_info = line_mat,
#'               light_info = lights)
#'}
rasterize_scene  = function(scene, 
                           filename = NA, width=800, height=800, 
                           line_info = NULL, alpha_line = 1.0,
                           parallel = TRUE,
                           fov=20,lookfrom=c(0,0,10),lookat=NULL, camera_up = c(0,1,0), #Sanitize lookfrom and lookat inputs
                           light_info = directional_light(), color="red",
                           type = "diffuse", background = "black", 
                           tangent_space_normals = TRUE,
                           shadow_map = TRUE, 
                           shadow_map_bias = 0.003, shadow_map_intensity = 0, shadow_map_dims = NULL,
                           ssao = FALSE, ssao_intensity = 10, ssao_radius = 0.1, 
                           tonemap = "none", debug = "none", 
                           near_plane = 0.1, far_plane = 100,
                           shader = "default", 
                           block_size = 4, shape = NULL, line_offset = 0.00001,
                           ortho_dimensions = c(1,1), bloom = FALSE, antialias_lines = TRUE,
                           environment_map= "", background_sharpness = 1.0) {
  if(!is.null(attr(scene,"cornell"))) {
    corn_message = "Setting default values for Cornell box: "
    missing_corn = FALSE
    if(missing(lookfrom)) {
      lookfrom = c(278, 278, -800)
      corn_message = paste0(corn_message, "lookfrom `c(278,278,-800)` ")
      missing_corn = TRUE
    }
    if(missing(lookat)) {
      lookat = c(278, 278, 0)
      corn_message = paste0(corn_message, "lookat `c(278,278,0)` ")
      missing_corn = TRUE
    }
    if(missing(fov)) {
      fov=40
      corn_message = paste0(corn_message, "fov `40` ")
      missing_corn = TRUE
    }
    if(fov == 0 && missing(ortho_dimensions)) {
      ortho_dimensions = c(580,580)
      corn_message = paste0(corn_message, "ortho_dimensions `c(580, 580)` ")
      missing_corn = TRUE
    }
    corn_message = paste0(corn_message,".")
    if(missing_corn) {
      message(corn_message)
    }
    if(attr(scene,"cornell_light")) {
      light_info = add_light(light_info,point_light(c(555/2,450,555/2),  falloff_quad = 0.0, constant = 0.0002, falloff = 0.005))
    }
  }
  obj = merge_shapes(scene)
  
  max_indices = 0
  has_norms = rep(FALSE,length(obj$shapes))
  has_tex = rep(FALSE,length(obj$shapes))
  has_vertex_tex = list()
  has_vertex_normals = list()
  
  if(length(ortho_dimensions) != 2) {
    stop("ortho_dimensions must be length-2 numeric vector")
  }
  #lights
  if(!is.null(light_info)) {
    if(ncol(light_info) != 10) {
      stop("light_info must have 10 cols")
    }
    lightinfo = light_info
  } else {
    lightinfo = matrix(nrow=0,ncol=10)
  }

  if(is.null(line_info)) {
    line_info = matrix(nrow=0,ncol=0)
  }
  bounds = c(Inf,Inf,Inf,-Inf,-Inf,-Inf)
  for(i in seq_len(length(obj$shapes))) {
    obj$shapes[[i]]$indices = (obj$shapes[[i]]$indices)
    obj$shapes[[i]]$tex_indices = (obj$shapes[[i]]$tex_indices)
    obj$shapes[[i]]$norm_indices = (obj$shapes[[i]]$norm_indices)
    has_vertex_tex[[i]] = (obj$shapes[[i]]$has_vertex_tex)
    has_vertex_normals[[i]] = (obj$shapes[[i]]$has_vertex_normals)
    
    max_indices = max(c(max_indices,nrow(obj$shapes[[i]]$indices)))
    has_norms[i] = nrow(obj$shapes[[i]]$indices) == nrow(obj$shapes[[i]]$norm_indices)
    has_tex[i] = nrow(obj$shapes[[i]]$indices) == nrow(obj$shapes[[i]]$tex_indices) && all(obj$shapes[[i]]$tex_indices != -1)
  }
  has_vertex_tex = unlist(has_vertex_tex)
  has_vertex_normals = unlist(has_vertex_normals)
  
  obj$vertices = obj$vertices
  tempboundsmin = apply(obj$vertices,2,min)
  tempboundsmax = apply(obj$vertices,2,max)
  bounds[1:3] = c(min(c(bounds[1],tempboundsmin[1])),
                  min(c(bounds[2],tempboundsmin[2])),
                  min(c(bounds[3],tempboundsmin[3])))
  bounds[4:6] = c(max(c(bounds[4],tempboundsmax[1])),
                  max(c(bounds[5],tempboundsmax[2])),
                  max(c(bounds[6],tempboundsmax[3])))

  if(is.null(lookat)) {
    lookat = (bounds[1:3] + bounds[4:6])/2
    message(sprintf("Setting `lookat` to: c(%0.2f, %0.2f, %0.2f)",lookat[1],lookat[2],lookat[3]))
  }

  use_default_material = FALSE
  if(length(obj$materials) > 0) {
    has_texture          = rep(FALSE,length(obj$materials))
    has_ambient_texture  = rep(FALSE,length(obj$materials))
    has_normal_texture   = rep(FALSE,length(obj$materials))
    has_specular_texture = rep(FALSE,length(obj$materials))
    has_emissive_texture = rep(FALSE,length(obj$materials))
  } else {
    use_default_material = TRUE
    has_texture          = FALSE
    has_ambient_texture  = FALSE
    has_normal_texture   = FALSE
    has_specular_texture = FALSE
    has_emissive_texture = FALSE
  }
  

  for(i in seq_len(length(obj$materials))) {
    if(!is.null(obj$materials[[i]]$diffuse_texname) && obj$materials[[i]]$diffuse_texname != "") {
      has_texture[i] = TRUE
      obj$materials[[i]]$diffuse_texname = path.expand(obj$materials[[i]]$diffuse_texname)
    }
    if(!is.null(obj$materials[[i]]$ambient_texname) && obj$materials[[i]]$ambient_texname != "") {
      has_ambient_texture[i] = TRUE
      obj$materials[[i]]$ambient_texname = path.expand(obj$materials[[i]]$ambient_texname)
    }
    if(!is.null(obj$materials[[i]]$specular_texname) && obj$materials[[i]]$specular_texname != "") {
      has_specular_texture[i] = TRUE
      obj$materials[[i]]$specular_texname = path.expand(obj$materials[[i]]$specular_texname)
    }
    if(!is.null(obj$materials[[i]]$normal_texname) && obj$materials[[i]]$normal_texname != "") {
      has_normal_texture[i] = TRUE

      obj$materials[[i]]$normal_texname = path.expand(obj$materials[[i]]$normal_texname)
    }
    if(!is.null(obj$materials[[i]]$emissive_texname) && obj$materials[[i]]$emissive_texname != "") {
      has_emissive_texture[i] = TRUE

      obj$materials[[i]]$emissive_texname = path.expand(obj$materials[[i]]$emissive_texname)
    } else if (is.null(obj$materials[[i]]$emissive_texname) ) {
      obj$materials[[i]]$emissive_texname = ""
    }
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

  typevals = rep(2,max(c(length(obj$materials),1)))
  if(!use_default_material) {
    for(i in seq_len(length(obj$materials))) {
      typeval = switch(obj$materials[[i]]$type, "vertex" = 1, "diffuse" = 2, "phong" = 3, "color" = 8,"toon" = 9, "toon_phong" = 10, 1)
      if(typeval < 8) {
        if(has_normal_texture[i]) {
          if(typeval == 2) {
            if(!tangent_space_normals) {
              typevals[i] = 4
            } else {
              typevals[i] = 5
            }
          } else if (typeval == 3) {
            if(!tangent_space_normals) {
              typevals[i] = 6
            } else {
              typevals[i] = 7
            }
          }
        } else {
          typevals[i] = typeval
        }
      } else {
        typevals[i] = typeval
      }
    }
  }
  has_reflection_map = rep(FALSE,length(obj$materials)) 
  has_refraction = rep(FALSE,length(obj$materials)) 
  
  for(i in seq_len(length(obj$materials))) {
    obj$materials[[i]]$culling = switch(obj$materials[[i]]$culling, "back" = 1, "front" = 2, "none" = 3, 1)
    if(environment_map != "" && obj$materials[[i]]$reflection_intensity > 0 && 
       file.exists(environment_map) && !dir.exists(environment_map)) {
      has_reflection_map[i] = TRUE
    }
    if(environment_map != "" && obj$materials[[i]]$ior != 1 && 
       file.exists(environment_map) && !dir.exists(environment_map)) {
      has_refraction[i] = TRUE
    }
  }
  environment_map_hdr = FALSE
  has_environment_map = FALSE
  if(environment_map != "") {
    has_environment_map = TRUE
    environment_map = path.expand(environment_map)
    if(tools::file_ext(environment_map) == "hdr") {
      environment_map_hdr = TRUE
    }
  }
  
  if(is.null(shadow_map_dims)) {
    shadow_map_dims = c(width,height)
  } else {
    if(length(shadow_map_dims) == 1 && is.numeric(shadow_map_dims) && shadow_map_dims > 0) {
      shadow_map_dims = c(width,height)*shadow_map_dims
    } else if(length(shadow_map_dims) != 2) {
      stop("shadow_map_dims must be vector of length 2")
    }
  }
  tonemap = switch(tonemap, "gamma" = 1, "uncharted" = 2, "hbd" = 3, "none"=4, 1)

  is_dir_light = rep(TRUE, nrow(lightinfo))
  for(i in seq_len(nrow(lightinfo))) {
    if(any(lightinfo[i,7:9] != 0)) {
      is_dir_light[i] = FALSE
    }
  }

  imagelist = rasterize(obj,
                        lightinfo,
                        line_mat = line_info,
                        nx = width,
                        ny = height,
                        model_color = color,
                        lookfrom = lookfrom,
                        lookat = lookat,
                        fov=fov,
                        typevals = typevals,
                        has_shadow_map=shadow_map,
                        calc_ambient = ssao,
                        tbn = tangent_space_normals,
                        ambient_radius = ssao_radius,
                        shadow_map_bias = shadow_map_bias,
                        numbercores = numbercores,
                        max_indices = max_indices,
                        has_normals_vec = has_norms,
                        has_tex_vec = has_tex,
                        has_texture,
                        has_ambient_texture,
                        has_normal_texture,
                        has_specular_texture,
                        has_emissive_texture,
                        block_size = block_size, use_default_material = use_default_material,
                        near_plane, far_plane,
                        shadow_map_intensity,
                        bounds, shadow_map_dims, camera_up, 
                        alpha_line, line_offset,
                        ortho_dimensions, is_dir_light,
                        antialias_lines,
                        has_vertex_tex,has_vertex_normals,
                        has_reflection_map, environment_map, background_sharpness, has_refraction,
                        environment_map_hdr, has_environment_map)
  if(ssao) {
    imagelist$amb = (imagelist$amb)^ssao_intensity
    imagelist$r = imagelist$r * imagelist$amb
    imagelist$g = imagelist$g * imagelist$amb
    imagelist$b = imagelist$b * imagelist$amb
  }
  if(debug == "normals") {
    norm_array = array(0,dim=c(dim(imagelist$r)[2:1],3))
    norm_array[,,1] = (imagelist$normalx+1)/2
    norm_array[,,2] = (imagelist$normaly+1)/2
    norm_array[,,3] = (imagelist$normalz+1)/2
    norm_array = rayimage::render_reorient(norm_array,transpose = TRUE, flipx = TRUE)
    if(is.na(filename)) {
      rayimage::plot_image(norm_array)
    }  else {
      save_png(norm_array, filename = filename)
    }
    return(invisible(norm_array))
  }
  if(debug == "depth") {
    depth_array = array(0,dim=c(dim(imagelist$r)[2:1],3))
    depth_array[,,1] = (imagelist$linear_depth)
    depth_array[,,2] = (imagelist$linear_depth)
    depth_array[,,3] = (imagelist$linear_depth)
    depth_array[is.infinite(depth_array)] = 1
    scale_factor = max(depth_array, na.rm = TRUE) - min(depth_array, na.rm = TRUE)
    depth_array = (depth_array - min(depth_array))/scale_factor
    if(is.na(filename)) {
      depth_array = rayimage::render_reorient(depth_array,transpose = TRUE, flipx = TRUE)
      rayimage::plot_image(depth_array)
    }  else {
      save_png(depth_array, filename = filename)
    }
    return(invisible(depth_array))
  }
  if(debug == "raw_depth") {
    return(imagelist$linear_depth)
  }
  if(debug == "position") {
    pos_array = array(0,dim=c(dim(imagelist$r)[2:1],3))
    imagelist$positionx = rescale(imagelist$positionx,to=c(0,1))
    imagelist$positiony = rescale(imagelist$positiony,to=c(0,1))
    imagelist$positionz = rescale(imagelist$positionz,to=c(0,1))

    pos_array[,,1] = (imagelist$positionx)
    pos_array[,,2] = (imagelist$positiony)
    pos_array[,,3] = (imagelist$positionz)
    pos_array = rayimage::render_reorient(pos_array,transpose = TRUE, flipx = TRUE)
    pos_array[is.infinite(pos_array)] = 1
    if(is.na(filename)) {
      rayimage::plot_image(pos_array)
    }  else {
      save_png(pos_array, filename = filename)
    }
    return(invisible(pos_array))
  }
  if(debug == "uv") {
    uv_array = array(0,dim=c(dim(imagelist$r)[2:1],3))
    uv_array[,,1] = (imagelist$uvx)
    uv_array[,,2] = (imagelist$uvy)
    uv_array[,,3] = (imagelist$uvz)
    uv_array = rayimage::render_reorient(uv_array,transpose = TRUE, flipx = TRUE)
    if(is.na(filename)) {
      rayimage::plot_image(uv_array)
    }  else {
      save_png(uv_array, filename = filename)
    }
    return(invisible(uv_array))
  }
  if(environment_map == "") {
    imagelist$r[imagelist$depth == 1] = bg_color[1]
    imagelist$g[imagelist$depth == 1] = bg_color[2]
    imagelist$b[imagelist$depth == 1] = bg_color[3]
  }

  retmat = array(0,dim=c(dim(imagelist$r)[2:1],3))
  if(tonemap != 4) {
    imagelist = tonemap_image(imagelist$r,imagelist$g,imagelist$b,tonemap)
  }
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
    save_png(retmat, filename = filename)
  }
  if(debug == "all") {
    return(imagelist)
  }
  return(invisible(retmat))
}
