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
rasterize_mesh  = function(vertices, indices = NULL, texcoords = NULL, normals = NULL,
                           filename = NA, width=400, height=400,
                           fov=20,lookfrom=c(0,0,10),lookat=c(0,0,0),
                           type = "diffuse", color="darkred",
                           texture_location = NA,
                           normal_texture_location = NA,
                           specular_texture_location = NA,
                           emissive_texture_location = NA,
                           light_direction=c(1,1,1), ambient_color=c(0,0,0), 
                           exponent=32, specular_intensity = 0.6, emission_intensity = 1,
                           diffuse_intensity = 1, tangent_space_normals = FALSE,
                           shadow_map = FALSE, calc_ambient = FALSE, 
                           ambient_intensity = 10, ambient_radius = 0.1,
                           tonemap = "none", debug = "none", shadow_map_bias = 0.005) {
  if(is.null(indices)) {
    if(length(vertices) %% 3 != 0) {
      stop("If no indices matrix provided, number of rows in `vertices` must be a multiple of 3")
    }
    indices = matrix(1:length(vertices),ncol=3,byrow=TRUE)
  } else {
    if(ncol(indices) != 3 && nrow(indices) == 3) {
      indices = t(indices)
    }
  }
  if(ncol(vertices) != 3 && nrow(vertices) == 3) {
    vertices = t(vertices)
  }
  has_normals = TRUE
  if(is.null(normals)) {
    has_normals = FALSE
    normals = matrix(0)
  }
  has_texcoords = TRUE
  if(is.null(texcoords)) {
    has_texcoords = FALSE
    texcoords = matrix(0)
  }

  if(type == "vertex" && nrow(normals) != nrow(vertices)) {
    type = "diffuse"
    warning("setting type to `diffuse`--Gouraud shading requires vertex normals for every vertex")
  }
  color = convert_color(color)
  
  has_texture = !is.na(texture_location) && file.exists(texture_location)
  has_normal_texture = !is.na(normal_texture_location) && file.exists(normal_texture_location)
  has_specular_texture = !is.na(specular_texture_location) && file.exists(specular_texture_location)
  has_emissive_texture = !is.na(emissive_texture_location) && file.exists(emissive_texture_location)
  
  if(has_texture) {
    texture_location = path.expand(texture_location)
  } 
  if(has_normal_texture) {
    normal_texture_location = path.expand(normal_texture_location)
  } 
  if(has_specular_texture) {
    specular_texture_location = path.expand(specular_texture_location)
  } 
  if(has_emissive_texture) {
    emissive_texture_location = path.expand(emissive_texture_location)
  } 
  
  typeval = switch(type, "vertex" = 1, "diffuse" = 2, "phong" = 3)
  if(has_normal_texture) {
    if(typeval == 2) {
      if(!tangent_space_normals) {
        typeval = 4
      } else {
        typeval = 5
      }
    } else if (typeval == 3) {
      if(!tangent_space_normals) {
        typeval = 6
      } else {
        typeval = 7
      }
    }
  }
  tonemap = switch(tonemap, "gamma" = 1, "uncharted" = 2, "hbd" = 3, "none"=4, 1)
  
  imagelist = rasterize(vertices,
                        indices,
                        nx=width,
                        ny=height,
                        texcoords = texcoords, 
                        normals=normals,
                        texture_location = texture_location,
                        normal_texture_location = normal_texture_location,
                        specular_texture_location = specular_texture_location,
                        emissive_texture_location = emissive_texture_location,
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
                        type = typeval,
                        has_normals = has_normals,
                        has_texcoords = has_texcoords,
                        has_texture = has_texture, 
                        has_normal_texture=has_normal_texture,
                        has_specular_texture=has_specular_texture,
                        has_emissive_texture = has_emissive_texture,
                        has_shadow_map=shadow_map, 
                        calc_ambient = calc_ambient, 
                        tbn = tangent_space_normals, ambient_radius = ambient_radius,
                        shadow_map_bias=shadow_map_bias)
  imagelist$amb = (imagelist$amb)^ambient_intensity
  if(calc_ambient) {
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
