#'@title Rasterize an OBJ file
#'
#'@param obj_model  A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param width Default `400`. Width of the rendered image.
#'@param height Default `400`. Width of the rendered image.
#'@param fov Default `20`. Width of the rendered image.
#'@param lookfrom Default `c(0,0,10)`. Camera location.
#'@param lookat Default `c(0,0,0)`. Camera focal position.

#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
rasterize_obj  = function(obj_model, filename = NA, width=400, height=400,
                          fov=20,lookfrom=c(0,0,10),lookat=NULL, camera_up = c(0,1,0), #Sanitize lookfrom and lookat inputs
                          scale_obj = 1,
                          point_light_info = NULL,
                          type = "diffuse", color="darkred", background = "white",
                          texture_location = NA,
                          normal_texture_location = NA,
                          specular_texture_location = NA,
                          emissive_texture_location = NA,
                          parallel = TRUE,
                          light_direction=c(1,1,1), light_intensity=1.0, ambient_color=c(0,0,0), 
                          exponent=32, specular_intensity = 0.6, emission_intensity = 1,
                          override_exponent = FALSE,
                          diffuse_intensity = 1, tangent_space_normals = TRUE,
                          shadow_map = FALSE, calc_ambient = FALSE, 
                          ambient_intensity = 10, ambient_radius = 0.1, 
                          tonemap = "none", debug = "none", 
                          shadow_map_bias = -0.001, shadow_map_intensity = 0.5,
                          near_plane = 0.1, far_plane = 100, culling = "back",
                          shader = "default", double_sided = FALSE,
                          block_size = 4, shape = NULL, shadow_map_dims = NULL) {
  if(!file.exists(obj_model)) {
    stop(obj_model, " not found in current directory.")
  }
  obj = read_obj(obj_model)
  if(!is.null(shape)) {
    if(length(obj$shapes) < shape) {
      stop("shape requested exceeds number of shapes in OBJ file")
    }
    obj$shapes = obj$shapes[shape]
  }
  max_indices = 0
  has_norms = rep(FALSE,length(obj$shapes))
  has_tex = rep(FALSE,length(obj$shapes))
  
  #lights
  if(!is.null(point_light_info)) {
    if(ncol(point_light_info) != 9) {
      stop("point_light_info must have 9 cols")
    }
    lightinfo = point_light_info
  } else {
    lightinfo = matrix(nrow=0,ncol=9)
  }
  culling = switch(culling, "back" = 1, "front" = 2, "none" = 3, 1)
  shaderval = switch(shader, "default" = 1, "diffuse" = 2, "phong" = 3, "color" = 4, 1)
  
  
  bounds = c(Inf,Inf,Inf,-Inf,-Inf,-Inf)
  for(i in seq_len(length(obj$shapes))) {
    obj$shapes[[i]]$indices = (obj$shapes[[i]]$indices)
    obj$shapes[[i]]$tex_indices = (obj$shapes[[i]]$tex_indices)
    obj$shapes[[i]]$norm_indices = (obj$shapes[[i]]$norm_indices)
    
    max_indices = max(c(max_indices,nrow(obj$shapes[[i]]$indices)))
    has_norms[i] = nrow(obj$shapes[[i]]$indices) == nrow(obj$shapes[[i]]$norm_indices)
    has_tex[i] = nrow(obj$shapes[[i]]$indices) == nrow(obj$shapes[[i]]$tex_indices)
  }
  obj$vertices = obj$vertices * scale_obj
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
  
  if(type == "gouraud" && nrow(normals) != nrow(vertices)) {
    type = "diffuse"
    warning("setting type to `diffuse`--Gouraud shading requires vertex normals for every vertex")
  }
  color = convert_color(color)
  bg_color = convert_color(background)
  
  typeval = switch(type, "vertex" = 1, "diffuse" = 2, "phong" = 3, "color" = 8, 1)
  typevals = rep(typeval,length(obj$materials))
  if(typeval != 8) {
    if(!use_default_material) {
      for(i in seq_len(length(obj$materials))) {
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
      }
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
  imagelist = rasterize(obj,
                        lightinfo,
                        nx=width,
                        ny=height,
                        model_color = color,
                        ambient_color = ambient_color,
                        exponent = exponent,
                        specular_intensity = specular_intensity,
                        diffuse_intensity = diffuse_intensity,
                        emission_intensity = emission_intensity,
                        lookfrom=lookfrom,
                        lookat=lookat,
                        fov=fov,
                        light_direction=light_direction,
                        type = typevals,
                        has_shadow_map=shadow_map, 
                        calc_ambient = calc_ambient, 
                        tbn = tangent_space_normals, 
                        ambient_radius = ambient_radius,
                        shadow_map_bias=shadow_map_bias,
                        numbercores=numbercores, 
                        max_indices = max_indices,
                        has_normals_vec=has_norms, 
                        has_tex_vec=has_tex,
                        has_texture,       
                        has_ambient_texture,
                        has_normal_texture,   
                        has_specular_texture, 
                        has_emissive_texture,
                        block_size = block_size, use_default_material = use_default_material,
                        override_exponent = override_exponent,
                        near_plane, far_plane,
                        shadow_map_intensity,
                        bounds, shadow_map_dims, camera_up,light_intensity, culling, double_sided)
  if(calc_ambient) {
    imagelist$amb = (imagelist$amb)^ambient_intensity
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
  if(tonemap != 4) {
    imagelist = tonemap_image(imagelist$r,imagelist$g,imagelist$b,tonemap)
  }
  retmat[,,1] = rayimage::render_reorient(imagelist$r,transpose = TRUE, flipx = TRUE)
  retmat[,,2] = rayimage::render_reorient(imagelist$g,transpose = TRUE, flipx = TRUE)
  retmat[,,3] = rayimage::render_reorient(imagelist$b,transpose = TRUE, flipx = TRUE)
  retmat = rayimage::render_convolution(retmat, min_value = 1)
  
  retmat[retmat > 1] = 1
  if(is.na(filename)) {
    rayimage::plot_image(retmat)
  } else {
    rayimage:::save_png(retmat, filename = filename)
  }
  return(invisible(retmat))
}
