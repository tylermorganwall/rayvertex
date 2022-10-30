

#'@title Set Material
#'
#'@description Set the material(s) of the mesh.
#'
#'@param mesh The target mesh. 
#'@param material Default `NULL`. You can pass the output of the `material_list()` function
#'to specify the material, or use the following individual settings.
#'@param id Default `NULL`. Either a number specifying the material to change, or a character vector 
#'matching the material name.
#'@param diffuse                   Default `c(0.5,0.5,0.5)`. The diffuse color.
#'@param ambient                   Default `c(0,0,0)`. The ambient color.
#'@param specular                  Default `c(1,1,1)`. The specular color.
#'@param transmittance             Default `c(1,1,1)`. The transmittance
#'@param emission                  Default `c(0,0,0)`. The emissive color.
#'@param shininess                 Default `10.0`. The shininess exponent.
#'@param ior                       Default `1.0`. The index of refraction. If this is not equal to `1.0`, the material will be refractive.
#'@param dissolve                  Default `1.0`. The transparency.
#'@param illum                     Default `1.0`. The illumination.
#'@param texture_location          Default `""`. The diffuse texture location.
#'@param normal_texture_location   Default `""`. The normal texture location.
#'@param specular_texture_location Default `""`. The specular texture location.
#'@param ambient_texture_location  Default `""`. The ambient texture location.
#'@param emissive_texture_location Default `""`. The emissive texture location.
#'@param diffuse_intensity         Default `1`. The diffuse intensity.
#'@param specular_intensity        Default `1`. The specular intensity.
#'@param emission_intensity        Default `1`. The emission intensity.
#'@param ambient_intensity         Default `1`. The ambient intensity.
#'@param culling                   Default `"back"`. The culling type. Options are `back`, `front`, and `none`.
#'@param type                      Default `"diffuse"`. The shader type. Options include `diffuse`,`phong`,`vertex`, and `color`.
#'@param translucent               Default `TRUE`. Whether light should transmit through a semi-transparent material.
#'@param toon_levels               Default `5`. Number of color breaks in the toon shader. 
#'@param toon_outline_width        Default `0.05`. Expansion term for model to specify toon outline width. Note: setting this property via this function currently does not generate outlines. Specify it during object creation.
#'@param toon_outline_color        Default `black`. Toon outline color. Note: setting this property via this function currently does not color outlines. Specify it during object creation.
#'@param reflection_intensity      Default `0.0`. Intensity of the reflection of the environment map, if present. This will be ignored if the material is refractive.
#'@param reflection_sharpness      Default `1.0`. Sharpness of the reflection, where lower values have blurrier reflections. Must be greater than zero and less than one.
#'
#'@return Shape with new material
#'@export
#'@examples
#'if(rayvertex:::run_documentation()) {
#'#Set the material of an object
#'generate_cornell_mesh() |>
#'  add_shape(set_material(sphere_mesh(position=c(400,555/2,555/2),radius=40), 
#'                         diffuse="purple", type="phong")) |>
#'  add_shape(set_material(sphere_mesh(position=c(555/2,220,555/2),radius=40),
#'                         dissolve=0.2,culling="none",diffuse="red")) |>
#'  add_shape(set_material(sphere_mesh(position=c(155,300,555/2),radius=60), 
#'                         material = material_list(diffuse="gold", type="phong", 
#'                                                  ambient="gold", ambient_intensity=0.4))) |>
#'  rasterize_scene(light_info=directional_light(direction=c(0.1,0.6,-1)))
#'  }
set_material = function(mesh, material = NULL, id = NULL,
                        diffuse                   = c(0.5,0.5,0.5),
                        ambient                   = c(0,0,0),
                        specular                  = c(1,1,1),
                        transmittance             = c(1,1,1),
                        emission                  = c(0,0,0),
                        shininess                 = 10.0,
                        ior                       = 1.0,
                        dissolve                  = 1.0,
                        illum                     = 1.0,
                        texture_location          = "",
                        normal_texture_location   = "",
                        specular_texture_location = "",
                        ambient_texture_location  = "",
                        emissive_texture_location = "",
                        diffuse_intensity         = 1, 
                        specular_intensity        = 1,  
                        emission_intensity        = 1,
                        ambient_intensity         = 1,
                        culling                   = "back",
                        type                      = "diffuse",
                        translucent               = TRUE,
                        toon_levels               = 5,
                        toon_outline_width        = 0.05,
                        toon_outline_color        = "black",
                        reflection_intensity      = 0.0,
                        reflection_sharpness      = 0.0) {
  culling = switch(culling, "back" = 1, "front" = 2, "none" = 3, 1)
  if(is.null(material)) {
    material = list()
    material$diffuse                    = convert_color(diffuse)              
    material$ambient                    = convert_color(ambient)
    material$specular                   = convert_color(specular)
    material$transmittance              = convert_color(transmittance)
    material$emission                   = convert_color(emission)
    material$shininess                  = shininess                 
    material$ior                        = ior                       
    material$dissolve                   = dissolve                  
    material$illum                      = illum                     
    material$diffuse_texname           = texture_location          
    material$normal_texname    = normal_texture_location   
    material$specular_texname  = specular_texture_location 
    material$ambient_texname   = ambient_texture_location  
    material$emissive_texname  = emissive_texture_location 
    material$diffuse_intensity          = diffuse_intensity         
    material$specular_intensity         = specular_intensity        
    material$emission_intensity         = emission_intensity        
    material$ambient_intensity         = ambient_intensity         
    material$culling                    = culling                   
    material$type      = type                      
    material$translucent = translucent               
    material$toon_levels       = toon_levels               
    material$toon_outline_width        = toon_outline_width        
    material$toon_outline_color    = convert_color(toon_outline_color)   
    material$reflection_intensity        = reflection_intensity        
    material$reflection_sharpness    = reflection_sharpness      
  }
  material_hash = digest::digest(material)
  if(length(mesh$materials) > 0 && !is.null(mesh$materials[[1]]) && length(mesh$materials[[1]]) > 0) {
    if(is.null(id)) {
      mesh$materials = list()
      mesh$materials[[1]] = list(material)
      mesh$material_hashes = material_hash
      for(i in seq_len(length(mesh$shapes))) {
        mesh$shapes[[i]]$material_ids = rep(0,nrow(mesh$shapes[[i]]$indices))
      }
    } else {
      len_mat = length(mesh$materials[[id]])
      for(i in seq_len(len_mat)) {
        mesh$materials[[id]][[i]] = material
      }
      mesh$shapes[[id]]$material_ids = rep(0,nrow(mesh$shapes[[id]]$indices))
    }
  } else {
    mesh$shapes[[1]]$material_ids = rep(0,nrow(mesh$shapes[[1]]$indices))
    mesh$materials[[1]] = list(list(material))
    mesh$material_hashes[1] = material_hash
  }
  class(mesh) = c("ray_mesh", "list")
  return(mesh)
}


#'@title Change Material
#'
#'@description Change individual material properties, leaving others alone.
#'
#'@param mesh Mesh to change.
#'@param id Default `NULL`. Either a number specifying the material to change, or a character vector 
#'matching the material name.
#'@param diffuse                   Default `NULL`. The diffuse color.
#'@param ambient                   Default `NULL`. The ambient color.
#'@param specular                  Default `NULL`. The specular color.
#'@param transmittance             Default `NULL`. The transmittance
#'@param emission                  Default `NULL`. The emissive color.
#'@param shininess                 Default `NULL`. The shininess exponent.
#'@param ior                       Default `NULL`. The index of refraction. If this is not equal to `1.0`, the material will be refractive. 
#'@param dissolve                  Default `NULL`. The transparency.
#'@param illum                     Default `NULL`. The illumination.
#'@param texture_location          Default `NULL`. The diffuse texture location.
#'@param normal_texture_location   Default `NULL`. The normal texture location.
#'@param specular_texture_location Default `NULL`. The specular texture location.
#'@param ambient_texture_location  Default `NULL`. The ambient texture location.
#'@param emissive_texture_location Default `NULL`. The emissive texture location.
#'@param diffuse_intensity         Default `NULL`. The diffuse intensity.
#'@param specular_intensity        Default `NULL`. The specular intensity.
#'@param emission_intensity        Default `NULL`. The emission intensity.
#'@param ambient_intensity         Default `NULL`. The ambient intensity.
#'@param culling                   Default `NULL`. The culling type. Options are `back`, `front`, and `none`.
#'@param type                      Default `NULL`. The shader type. Options include `diffuse`,`phong`,`vertex`, and `color`.
#'@param translucent               Default `NULL`. Whether light should transmit through a semi-transparent material.
#'@param toon_levels               Default `NULL`. Number of color breaks in the toon shader.
#'@param toon_outline_width        Default `NULL`. Expansion term for model to specify toon outline width. Note: setting this property via this function currently does not generate outlines. Specify it during object creation.
#'@param toon_outline_color        Default `NULL`. Toon outline color.Note: setting this property via this function currently does not color outlines. Specify it during object creation.
#'@param reflection_intensity      Default `NULL`. Intensity of the reflection of the environment map, if present. This will be ignored if the material is refractive.
#'@param reflection_sharpness      Default `NULL`. Sharpness of the reflection, where lower values have blurrier reflections. Must be greater than zero and less than one.
#'
#'@return Shape with new material settings
#'@export
#'@examples
#'if(rayvertex:::run_documentation()) {
#'p_sphere = sphere_mesh(position=c(555/2,555/2,555/2), 
#'                       radius=40,material=material_list(diffuse="purple"))
#'generate_cornell_mesh() |>
#'  add_shape(p_sphere) |>
#'  add_shape(change_material(translate_mesh(p_sphere,c(200,0,0)),diffuse="red")) |>
#'  add_shape(change_material(translate_mesh(p_sphere,c(100,0,0)),dissolve=0.5)) |>
#'  add_shape(change_material(translate_mesh(p_sphere,c(-100,0,0)),type="phong")) |>
#'  add_shape(change_material(translate_mesh(p_sphere,c(-200,0,0)),type="phong",shininess=30)) |>
#'  rasterize_scene(light_info=directional_light(direction=c(0.1,0.6,-1)))
#'}
change_material = function(mesh, id = NULL, 
                           diffuse                   = NULL,
                           ambient                   = NULL,
                           specular                  = NULL,
                           transmittance             = NULL,
                           emission                  = NULL,
                           shininess                 = NULL,
                           ior                       = NULL,
                           dissolve                  = NULL,
                           illum                     = NULL,
                           texture_location          = NULL,
                           normal_texture_location   = NULL,
                           specular_texture_location = NULL,
                           ambient_texture_location  = NULL,
                           emissive_texture_location = NULL,
                           diffuse_intensity         = NULL,
                           specular_intensity        = NULL,
                           emission_intensity        = NULL,
                           ambient_intensity         = NULL,
                           culling                   = NULL,
                           type                      = NULL,
                           translucent               = NULL,
                           toon_levels               = NULL,
                           toon_outline_width        = NULL,
                           toon_outline_color        = NULL,
                           reflection_intensity      = NULL,
                           reflection_sharpness      = NULL) {
  if(!is.null(culling)) {
    culling = switch(culling, "back" = 1, "front" = 2, "none" = 3, 1)
  }
  
  if(!is.null(mesh$materials) && length(mesh$materials) > 0) {
    if(is.null(id)) {
      for(i in seq_len(length(mesh$materials))) {
        if(!is.null(ambient))                   mesh$materials[[i]]$ambient              = convert_color(ambient)
        if(!is.null(diffuse))                   mesh$materials[[i]]$diffuse              = convert_color(diffuse)
        if(!is.null(specular))                  mesh$materials[[i]]$specular             = convert_color(specular)
        if(!is.null(transmittance))             mesh$materials[[i]]$transmittance        = convert_color(transmittance)
        if(!is.null(emission))                  mesh$materials[[i]]$emission             = convert_color(emission)
        if(!is.null(shininess))                 mesh$materials[[i]]$shininess            = shininess
        if(!is.null(ior))                       mesh$materials[[i]]$ior                  = ior              
        if(!is.null(dissolve))                  mesh$materials[[i]]$dissolve             = dissolve         
        if(!is.null(illum))                     mesh$materials[[i]]$illum                = illum            
        if(!is.null(ambient_texture_location))  mesh$materials[[i]]$ambient_texname      = ambient_texture_location  
        if(!is.null(texture_location))          mesh$materials[[i]]$diffuse_texname      = texture_location  
        if(!is.null(emissive_texture_location)) mesh$materials[[i]]$emissive_texname     = emissive_texture_location 
        if(!is.null(specular_texture_location)) mesh$materials[[i]]$specular_texname     = specular_texture_location 
        if(!is.null(normal_texture_location))   mesh$materials[[i]]$normal_texname       = normal_texture_location   
        if(!is.null(diffuse_intensity))         mesh$materials[[i]]$diffuse_intensity    = diffuse_intensity 
        if(!is.null(specular_intensity))        mesh$materials[[i]]$specular_intensity   = specular_intensity   
        if(!is.null(emission_intensity))        mesh$materials[[i]]$emission_intensity   = emission_intensity  
        if(!is.null(ambient_intensity))         mesh$materials[[i]]$ambient_intensity    = ambient_intensity  
        if(!is.null(culling))                   mesh$materials[[i]]$culling              = culling   
        if(!is.null(type))                      mesh$materials[[i]]$type                 = type   
        if(!is.null(translucent))               mesh$materials[[i]]$translucent          = translucent   
        if(!is.null(toon_levels))               mesh$materials[[i]]$toon_levels          = toon_levels   
        if(!is.null(toon_outline_width))        mesh$materials[[i]]$toon_outline_width   = toon_outline_width   
        if(!is.null(toon_outline_color))        mesh$materials[[i]]$toon_outline_color   = convert_color(toon_outline_color)
        if(!is.null(reflection_intensity))      mesh$materials[[i]]$reflection_intensity = reflection_intensity
        if(!is.null(reflection_sharpness))      mesh$materials[[i]]$reflection_sharpness = reflection_sharpness
        
        
      }
    } else {
      if(is.numeric(id)) {
        if(!is.null(ambient))                   mesh$materials[[id]]$ambient              = convert_color(ambient)
        if(!is.null(diffuse))                   mesh$materials[[id]]$diffuse              = convert_color(diffuse)
        if(!is.null(specular))                  mesh$materials[[id]]$specular             = convert_color(specular)
        if(!is.null(transmittance))             mesh$materials[[id]]$transmittance        = convert_color(transmittance)
        if(!is.null(emission))                  mesh$materials[[id]]$emission             = convert_color(emission)
        if(!is.null(shininess))                 mesh$materials[[id]]$shininess            = shininess
        if(!is.null(ior))                       mesh$materials[[id]]$ior                  = ior              
        if(!is.null(dissolve))                  mesh$materials[[id]]$dissolve             = dissolve         
        if(!is.null(illum))                     mesh$materials[[id]]$illum                = illum            
        if(!is.null(ambient_texture_location))  mesh$materials[[id]]$ambient_texname      = ambient_texture_location  
        if(!is.null(texture_location))          mesh$materials[[id]]$diffuse_texname      = texture_location  
        if(!is.null(emissive_texture_location)) mesh$materials[[id]]$emissive_texname     = emissive_texture_location 
        if(!is.null(specular_texture_location)) mesh$materials[[id]]$specular_texname     = specular_texture_location 
        if(!is.null(normal_texture_location))   mesh$materials[[id]]$normal_texname       = normal_texture_location   
        if(!is.null(diffuse_intensity))         mesh$materials[[id]]$diffuse_intensity    = diffuse_intensity 
        if(!is.null(specular_intensity))        mesh$materials[[id]]$specular_intensity   = specular_intensity   
        if(!is.null(emission_intensity))        mesh$materials[[id]]$emission_intensity   = emission_intensity  
        if(!is.null(ambient_intensity))         mesh$materials[[id]]$ambient_intensity    = ambient_intensity  
        if(!is.null(culling))                   mesh$materials[[id]]$culling              = culling   
        if(!is.null(type))                      mesh$materials[[id]]$type                 = type   
        if(!is.null(translucent))               mesh$materials[[id]]$translucent          = translucent  
        if(!is.null(toon_levels))               mesh$materials[[id]]$toon_levels          = toon_levels   
        if(!is.null(toon_outline_width))        mesh$materials[[id]]$toon_outline_width   = toon_outline_width   
        if(!is.null(toon_outline_color))        mesh$materials[[id]]$toon_outline_color   = convert_color(toon_outline_color) 
        if(!is.null(reflection_intensity))      mesh$materials[[id]]$reflection_intensity = reflection_intensity
        if(!is.null(reflection_sharpness))      mesh$materials[[id]]$reflection_sharpness = reflection_sharpness
        
      }
      if(is.character(id)) {
        for(i in seq_len(length(mesh$materials))) {
          if(names(mesh$materials)[i] == id) {
            if(!is.null(ambient))                   mesh$materials[[i]]$ambient              = convert_color(ambient)
            if(!is.null(diffuse))                   mesh$materials[[i]]$diffuse              = convert_color(diffuse)
            if(!is.null(specular))                  mesh$materials[[i]]$specular             = convert_color(specular)
            if(!is.null(transmittance))             mesh$materials[[i]]$transmittance        = convert_color(transmittance)
            if(!is.null(emission))                  mesh$materials[[i]]$emission             = convert_color(emission)
            if(!is.null(shininess))                 mesh$materials[[i]]$shininess            = shininess
            if(!is.null(ior))                       mesh$materials[[i]]$ior                  = ior              
            if(!is.null(dissolve))                  mesh$materials[[i]]$dissolve             = dissolve         
            if(!is.null(illum))                     mesh$materials[[i]]$illum                = illum            
            if(!is.null(ambient_texture_location))  mesh$materials[[i]]$ambient_texname      = ambient_texture_location  
            if(!is.null(texture_location))          mesh$materials[[i]]$diffuse_texname      = texture_location  
            if(!is.null(emissive_texture_location)) mesh$materials[[i]]$emissive_texname     = emissive_texture_location 
            if(!is.null(specular_texture_location)) mesh$materials[[i]]$specular_texname     = specular_texture_location 
            if(!is.null(normal_texture_location))   mesh$materials[[i]]$normal_texname       = normal_texture_location   
            if(!is.null(diffuse_intensity))         mesh$materials[[i]]$diffuse_intensity    = diffuse_intensity 
            if(!is.null(specular_intensity))        mesh$materials[[i]]$specular_intensity   = specular_intensity   
            if(!is.null(emission_intensity))        mesh$materials[[i]]$emission_intensity   = emission_intensity  
            if(!is.null(ambient_intensity))         mesh$materials[[i]]$ambient_intensity    = ambient_intensity  
            if(!is.null(culling))                   mesh$materials[[i]]$culling              = culling   
            if(!is.null(type))                      mesh$materials[[i]]$type                 = type   
            if(!is.null(translucent))               mesh$materials[[i]]$translucent          = translucent   
            if(!is.null(toon_levels))               mesh$materials[[i]]$toon_levels          = toon_levels   
            if(!is.null(toon_outline_width))        mesh$materials[[i]]$toon_outline_width   = toon_outline_width   
            if(!is.null(toon_outline_color))        mesh$materials[[i]]$toon_outline_color   = convert_color(toon_outline_color)
            if(!is.null(reflection_intensity))      mesh$materials[[i]]$reflection_intensity = reflection_intensity
            if(!is.null(reflection_sharpness))      mesh$materials[[i]]$reflection_sharpness = reflection_sharpness
            
          }
        }
      }
    }
    for(i in seq_len(length(mesh$materials))) {
      mesh$material_hashes[i] = digest::digest(mesh$materials[[i]])
    }
  } else {
    stop("No materials detected")
  }
  class(mesh) = c("ray_mesh", "list")
  
  return(mesh)
}

#'@title Material List
#'
#'@description Generate a material properties list.
#'
#'@param diffuse                   Default `c(0.5,0.5,0.5)`. The diffuse color.
#'@param ambient                   Default `c(0,0,0)`. The ambient color.
#'@param specular                  Default `c(1,1,1)`. The specular color.
#'@param transmittance             Default `c(1,1,1)`. The transmittance
#'@param emission                  Default `c(0,0,0)`. The emissive color.
#'@param shininess                 Default `10.0`. The shininess exponent.
#'@param ior                       Default `1.0`. The index of refraction. If this is not equal to `1.0`, the material will be refractive.
#'@param dissolve                  Default `1.0`. The transparency.
#'@param illum                     Default `1.0`. The illumination.
#'@param texture_location          Default `""`. The diffuse texture location.
#'@param normal_texture_location   Default `""`. The normal texture location.
#'@param specular_texture_location Default `""`. The specular texture location.
#'@param ambient_texture_location  Default `""`. The ambient texture location.
#'@param emissive_texture_location Default `""`. The emissive texture location.
#'@param diffuse_intensity         Default `1`. The diffuse intensity.
#'@param specular_intensity        Default `1`. The specular intensity.
#'@param emission_intensity        Default `1`. The emission intensity.
#'@param ambient_intensity         Default `1`. The ambient intensity.
#'@param culling                   Default `"back"`. The culling type. Options are `back`, `front`, and `none`.
#'@param type                      Default `"diffuse"`. The shader type. Options include `diffuse`,`phong`,`vertex`, and `color`.
#'@param translucent               Default `FALSE`. Whether light should transmit through a semi-transparent material.
#'@param toon_levels               Default `5`. Number of color breaks in the toon shader.
#'@param toon_outline_width        Default `0.05`. Expansion term for model to specify toon outline width.
#'@param toon_outline_color        Default `black`. Toon outline color.
#'@param reflection_intensity      Default `0.0`. Intensity of the reflection of the environment map, if present. This will be ignored if the material is refractive.
#'@param reflection_sharpness      Default `1.0`. Sharpness of the reflection, where lower values have blurrier reflections. Must be greater than zero and less than one.
#'
#'@return List of material properties.
#'@export
#'@examples
#'if(rayvertex:::run_documentation()) {
#'mat_prop = material_list(diffuse="purple", type="phong", shininess=20,
#'                         ambient="purple", ambient_intensity=0.3,
#'                         specular = "red", specular_intensity=2)
#'                         
#'p_sphere = sphere_mesh(position=c(555/2,555/2,555/2), 
#'                       radius=40,material=mat_prop)
#'                       
#'rasterize_scene(p_sphere, light_info=directional_light(direction=c(0.1,0.6,-1)))
#'}
material_list = function(diffuse                   = c(0.8,0.8,0.8),
                         ambient                   = c(0,0,0),
                         specular                  = c(1,1,1),
                         transmittance             = c(1,1,1),
                         emission                  = c(0,0,0),
                         shininess                 = 10.0,
                         ior                       = 1.0,
                         dissolve                  = 1.0,
                         illum                     = 1.0,
                         texture_location          = "",
                         normal_texture_location   = "",
                         specular_texture_location = "",
                         ambient_texture_location  = "",
                         emissive_texture_location = "",
                         diffuse_intensity         = 1, 
                         specular_intensity        = 1,  
                         emission_intensity        = 1,
                         ambient_intensity         = 1,
                         culling                   = "back",
                         type                      = "diffuse",
                         translucent               = TRUE,
                         toon_levels               = 5,
                         toon_outline_width        = 0.05,
                         toon_outline_color        = "black",
                         reflection_intensity      = 0.0,
                         reflection_sharpness      = 0.0) {
  material_props = 
    list(diffuse                   = convert_color(diffuse)                   ,
         ambient                   = convert_color(ambient)                   ,
         specular                  = convert_color(specular)                  ,
         transmittance             = convert_color(transmittance)             ,
         emission                  = convert_color(emission)                  ,
         shininess                 = shininess                 ,
         ior                       = ior                       ,
         dissolve                  = dissolve                  ,
         illum                     = illum                     ,
         ambient_texname           = ambient_texture_location          ,
         diffuse_texname           = texture_location   ,
         emissive_texname          = emissive_texture_location ,
         specular_texname          = specular_texture_location  ,
         normal_texname            = normal_texture_location ,
         diffuse_intensity         = diffuse_intensity         ,
         specular_intensity        = specular_intensity        ,
         emission_intensity        = emission_intensity        ,
         ambient_intensity         = ambient_intensity         ,
         culling                   = culling                   ,
         type                      = type                      ,
         translucent               = translucent               ,
         toon_levels               = toon_levels               ,
         toon_outline_width        = toon_outline_width        ,
         toon_outline_color        = convert_color(toon_outline_color)        ,
         reflection_intensity      = reflection_intensity      ,
         reflection_sharpness      = reflection_sharpness)
  stopifnot(length(material_props$diffuse) == 3)
  stopifnot(length(material_props$ambient) == 3)
  stopifnot(length(material_props$specular) == 3)
  stopifnot(length(material_props$transmittance) == 3)
  stopifnot(length(material_props$emission) == 3)
  stopifnot(length(material_props$toon_outline_color) == 3)
  
  
  return(material_props)
}

#' Add Outline
#'
#'@param angle The angle
#'@param order_rotation Default `c(1,2,3)`. 
#'@return Matrix
#'@keywords internal
generate_toon_outline = function(single_obj, material, scale = 1) {
  if((material$type == "toon" || material$type == "toon_phong") && material$toon_outline_width != 0.0) {
    bbox = apply(single_obj$vertices[[1]],2,range)
    bbox_size = bbox[2,] - bbox[1,]
    scaleval = (bbox_size + material$toon_outline_width)/bbox_size
    single_obj = single_obj |>
      scale_mesh(scale = scaleval) |>
      set_material(diffuse=material$toon_outline_color , culling = "front", type="color")
  }
  class(single_obj) = c("ray_mesh", "list")
  
  return(single_obj)
}