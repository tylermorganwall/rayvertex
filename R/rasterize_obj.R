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
rasterize_obj  = function(obj_model, width=400, height=400,
                          fov=20,lookfrom=c(0,0,10),lookat=c(0,0,0),
                          type = "diffuse", color="darkred",
                          texture_location = NA,
                          normal_texture_location = NA,
                          specular_texture_location = NA,
                          light_direction=c(1,1,1), ambient_color=c(0,0,0), 
                          exponent=32, specular_intensity = 0.6,
                          diffuse_intensity = 1, tangent_space_normals = FALSE,
                          shadow_map = FALSE) {
  obj = readobj::read.obj(obj_model)
  # for(i in seq_len(length(obj$shapes))) {
  vertices = t(obj$shapes[[1]]$positions)
  indices = t(obj$shapes[[1]]$indices)+1
  texcoords = matrix(obj$shapes[[1]]$texcoords,ncol=2,byrow=TRUE)
  normals = matrix(obj$shapes[[1]]$normal,ncol=3,byrow=TRUE)
  # }
  if(type == "gouraud" && nrow(normals) != nrow(vertices)) {
    type = "diffuse"
    warning("setting type to `diffuse`--Gouraud shading requires vertex normals for every vertex")
  }
  color = convert_color(color)

  has_texture = !is.na(texture_location) && file.exists(texture_location)
  has_normal_texture = !is.na(normal_texture_location) && file.exists(normal_texture_location)
  has_specular_texture = !is.na(specular_texture_location) && file.exists(specular_texture_location)
  
  if(has_texture) {
    texture_location = path.expand(texture_location)
  } 
  if(has_normal_texture) {
    normal_texture_location = path.expand(normal_texture_location)
  } 
  if(has_specular_texture) {
    specular_texture_location = path.expand(specular_texture_location)
  } 
  
  typeval = switch(type, "gouraud" = 1, "diffuse" = 2, "phong" = 3)
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
  
  imagelist = rasterize(vertices,
                        indices,
                        nx=width,
                        ny=height,
                        texcoords = texcoords, 
                        normals=normals,
                        texture_location = texture_location,
                        normal_texture_location = normal_texture_location,
                        specular_texture_location = specular_texture_location,
                        model_color = color,
                        ambient_color = ambient_color,
                        exponent = exponent,
                        specular_intensity = specular_intensity,
                        diffuse_intensity = diffuse_intensity,
                        lookfrom=lookfrom,
                        lookat=lookat,
                        fov=fov,
                        light_direction=light_direction,
                        type = typeval,
                        has_texture = has_texture, 
                        has_normal_texture=has_normal_texture,
                        has_specular_texture=has_specular_texture,
                        has_shadow_map=shadow_map)
  
  retmat = array(0,dim=c(dim(imagelist$r)[2:1],3))
  retmat[,,1] = rayimage::render_reorient(imagelist$r,transpose = TRUE, flipx = TRUE)
  retmat[,,2] = rayimage::render_reorient(imagelist$g,transpose = TRUE, flipx = TRUE)
  retmat[,,3] = rayimage::render_reorient(imagelist$b,transpose = TRUE, flipx = TRUE)
  rayimage::plot_image(scales::rescale(retmat,to=c(0,1)))
}
