#'@title Rasterize an OBJ file
#'
#'If the OBJ file has an MTL file, it must be located in the same directory as the OBJ file itself.
#'Additionally, the textures referenced in the MTL file must be specified as a 
#'relative path to the OBJ file's location.
#'
#'@param vertices  Filename of the `obj` file.
#'@param indices 
#'@param normals Default `NULL`.
#'@param texcoords Default `NULL`.
#'@param material_ids Default `1`.
#'@param materials Default `NULL`, default material.
#'@param texture_location Default `NA`. Location of the diffuse texture.
#'@param normal_texture_location Default `NA`. Location of the normal texture.
#'@param specular_texture_location Default `NA`. Location of the specular texture.
#'@param ambient_texture_location Default `NA`. Location of the ambient texture.
#'@param emissive_texture_location Default `NA`. Location of the emissive texture.
#'@param tangent_space_normals Default `TRUE`.
#'@param exponent Default `32`.
#'@param diffuse_intensity Default `1`.
#'@param specular_intensity Default `0.6`.
#'@param emission_intensity Default `1`.
#'@param diffuse_intensity Default `1`.
#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
construct_mesh  = function(vertices, indices, 
                           normals = NULL, norm_indices = NULL, 
                           texcoords = NULL, tex_indices = NULL, 
                           position = c(0,0,0),
                           material_ids = 0, 
                           materials = NULL,
                           scale_obj = 1,
                           exponent=32, 
                           ambient          = c(0,0,0),
                           diffuse          = c(0.5,0.5,0.5),
                           specular         = c(1,1,1),
                           transmittance    = c(1,1,1),
                           emission         = c(0,0,0),
                           shininess        = 10.0,
                           ior              = 1.0,
                           dissolve         = 1.0,
                           illum            = 1.0,
                           texture_location = "",
                           normal_texture_location = "",
                           specular_texture_location = "",
                           ambient_texture_location  = "",
                           emissive_texture_location = "",
                           diffuse_intensity = 1, specular_intensity = 0.6, emission_intensity = 1,
                           tangent_space_normals = TRUE,
                           shape_name = "") {
  mesh = list()
  mesh$shapes = list()
  mesh$materials = list()
  if(is.null(normals)) {
    normals = matrix(0,nrow=0,ncol=3)
  }
  if(is.null(texcoords)) {
    texcoords = matrix(0,nrow=0,ncol=2)
  }
  if(is.null(norm_indices)) {
    norm_indices = matrix(0,nrow=0,ncol=3)
  }
  if(is.null(tex_indices)) {
    tex_indices = matrix(0,nrow=0,ncol=3)
  }
  mesh$vertices = vertices * scale_obj + matrix(position,nrow = nrow(vertices), ncol = 3,byrow=TRUE)
  mesh$texcoords = texcoords
  mesh$normals = normals
  
  mesh$shapes[[1]] = list()
  mesh$shapes[[1]]$indices = indices
  mesh$shapes[[1]]$tex_indices = tex_indices
  mesh$shapes[[1]]$norm_indices = norm_indices
  if(length(material_ids) == 1) {
    mesh$shapes[[1]]$material_ids = rep(material_ids,nrow(indices))
  } else {
    mesh$shapes[[1]]$material_ids = material_ids
  }
  mesh$shapes[[1]]$name = shape_name
  
  ambient = convert_color(ambient)
  diffuse = convert_color(diffuse)
  specular = convert_color(specular)
  transmittance = convert_color(transmittance)
  emission = convert_color(emission)
  if(is.null(materials)) {
    mesh$materials[[1]] = list()
    mesh$materials[[1]]$ambient          = ambient
    mesh$materials[[1]]$diffuse          = diffuse
    mesh$materials[[1]]$specular         = specular
    mesh$materials[[1]]$transmittance    = transmittance
    mesh$materials[[1]]$emission         = emission
    mesh$materials[[1]]$shininess        = shininess
    mesh$materials[[1]]$ior              = ior              
    mesh$materials[[1]]$dissolve         = dissolve         
    mesh$materials[[1]]$illum            = illum            
    mesh$materials[[1]]$ambient_texname  = ambient_texture_location  
    mesh$materials[[1]]$diffuse_texname  = texture_location  
    mesh$materials[[1]]$emissive_texname = emissive_texture_location 
    mesh$materials[[1]]$specular_texname = specular_texture_location 
    mesh$materials[[1]]$normal_texname   = normal_texture_location   
    mesh$materials[[1]]$culling          = 1 #1 = back, 2 = front, 3 = none
  } else {
    for(i in seq_len(length(materials))) {
      mesh$materials[[i]] = list()
      mesh$materials[[i]]$ambient          = materials[[i]]$ambient
      mesh$materials[[i]]$diffuse          = materials[[i]]$diffuse
      mesh$materials[[i]]$specular         = materials[[i]]$specular
      mesh$materials[[i]]$transmittance    = materials[[i]]$transmittance
      mesh$materials[[i]]$emission         = materials[[i]]$emission
      mesh$materials[[i]]$shininess        = materials[[i]]$shininess
      mesh$materials[[i]]$ior              = materials[[i]]$ior              
      mesh$materials[[i]]$dissolve         = materials[[i]]$dissolve         
      mesh$materials[[i]]$illum            = materials[[i]]$illum            
      mesh$materials[[i]]$ambient_texname  = materials[[i]]$ambient_texture_location  
      mesh$materials[[i]]$diffuse_texname  = materials[[i]]$texture_location  
      mesh$materials[[i]]$emissive_texname = materials[[i]]$emissive_texture_location 
      mesh$materials[[i]]$specular_texname = materials[[i]]$specular_texture_location 
      mesh$materials[[i]]$normal_texname   = materials[[i]]$normal_texture_location   
      mesh$materials[[i]]$culling          = 1 #1 = back, 2 = front, 3 = none
    }
  }
  return(mesh)
  
}
