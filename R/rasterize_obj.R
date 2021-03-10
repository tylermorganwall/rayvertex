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
                          fov=20,lookfrom=c(0,0,10),lookat=c(0,0,0),
                          type = "diffuse", color="darkred", background = "white",
                          texture_location = NA,
                          normal_texture_location = NA,
                          specular_texture_location = NA,
                          emissive_texture_location = NA,
                          parallel = TRUE,
                          light_direction=c(1,1,1), ambient_color=c(0,0,0), 
                          exponent=32, specular_intensity = 0.6, emission_intensity = 1,
                          diffuse_intensity = 1, tangent_space_normals = FALSE,
                          shadow_map = FALSE, calc_ambient = FALSE, 
                          ambient_intensity = 10, ambient_radius = 0.1, 
                          tonemap = "none", debug = "none", shadow_map_bias = 0.005) {
  obj = readobj::read.obj(obj_model)
  
  max_indices = 0
  has_norms = rep(FALSE,length(obj$shapes))
  has_tex = rep(FALSE,length(obj$shapes))
  
  for(i in seq_len(length(obj$shapes))) {
    obj$shapes[[i]]$positions = t(obj$shapes[[i]]$positions)
    obj$shapes[[i]]$indices = t(obj$shapes[[i]]$indices)+1
    obj$shapes[[i]]$texcoords = matrix(obj$shapes[[i]]$texcoords,ncol=2,byrow=TRUE)
    obj$shapes[[i]]$normals = t(obj$shapes[[i]]$normals)
    max_indices = max(c(max_indices,nrow(obj$shapes[[i]]$indices)))
    has_norms[i] = nrow(obj$shapes[[i]]$normal) == nrow(obj$shapes[[i]]$positions)
    has_tex[i] = nrow(obj$shapes[[i]]$normal) == nrow(obj$shapes[[i]]$texcoords)
  }
  
  for(i in seq_len(length(obj$materials))) {
    if(!is.null(obj$materials[[i]]$diffuse_texname) && obj$materials[[i]]$diffuse_texname != "") {
      obj$materials[[i]]$diffuse_texname = path.expand(obj$materials[[i]]$diffuse_texname)
    }
    if(!is.null(obj$materials[[i]]$normal_texname) && obj$materials[[i]]$normal_texname != "") {
      obj$materials[[i]]$specular_texname = path.expand(obj$materials[[i]]$specular_texname)
    }
    if(!is.null(obj$materials[[i]]$normal_texname) && obj$materials[[i]]$normal_texname != "") {
      obj$materials[[i]]$normal_texname = path.expand(obj$materials[[i]]$normal_texname)
    }
    if(!is.null(obj$materials[[i]]$emissive_texname) && obj$materials[[i]]$emissive_texname != "") {
      obj$materials[[i]]$emissive_texname = path.expand(obj$materials[[i]]$emissive_texname)
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
  
  typeval = switch(type, "vertex" = 1, "diffuse" = 2, "phong" = 3, 1)
  typevals = rep(1,length(obj$shapes))
  for(i in seq_len(length(obj$shapes))) {
    if(has_norms[i]) {
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
  tonemap = switch(tonemap, "gamma" = 1, "uncharted" = 2, "hbd" = 3, "none"=4, 1)
  imagelist = rasterize(obj,
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
                        tbn = tangent_space_normals, ambient_radius = ambient_radius,
                        shadow_map_bias=shadow_map_bias,
                        numbercores=numbercores, max_indices = max_indices,
                        has_normals_vec=has_norms, has_tex_vec=has_tex)
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
}
