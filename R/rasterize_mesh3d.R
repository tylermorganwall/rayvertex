#'@title Rasterize an OBJ file
#'
#'If the OBJ file has an MTL file, it must be located in the same directory as the OBJ file itself.
#'Additionally, the textures referenced in the MTL file must be specified as a 
#'relative path to the OBJ file's location.
#'
#'@param mesh  Filename of the `obj` file.
#'@param shape Default `NULL`. The shape to render in the OBJ mesh. 
#'@param ... Arguments to pass to `rasterize_scene()`
#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
rasterize_mesh3d = function(mesh, diffuse=c(0.5,0.5,0.5), ambient = c(0,0,0), dissolve=1,
                            exponent = 10,
                            shape = NULL, scene_elements = NULL, ...) {
  mat_vals = mesh$material
  if(!is.null(mat_vals)) {
    diffuse_val = ifelse(is.null(mat_vals$color), diffuse, mat_vals$color)
    if(!is.null(mat_vals$color)) {
      diffuse_val = mat_vals$color
    } else {
      diffuse_val = diffuse
    }
    if(!is.null(mat_vals$alpha)) {
      dissolve_val = mat_vals$alpha
    } else {
      dissolve_val = dissolve
    }
    if(!is.null(mat_vals$ambient)) {
      ambient_val = mat_vals$ambient
    } else {
      ambient_val = ambient
    }
    if(!is.null(mat_vals$shininess)) {
      exponent_val = mat_vals$shininess
    } else {
      exponent_val = exponent
    }
  }
  mesh = construct_mesh(vertices = t(mesh$vb)[,1:3], 
                        indices = t(mesh$it)-1,
                        # normals = ifelse(!is.null(mesh$normals), t(mesh$normals), NULL),
                        # texcoords = ifelse(!is.null(mesh$texcoords), t(mesh$texcoords), NULL),
                        diffuse = diffuse_val,
                        dissolve = dissolve_val,
                        ambient =  ambient_val,
                        shininess = exponent_val)
  if(!is.null(scene_elements)) {
    mesh = add_shape(mesh,scene_elements)
  }
  rasterize_scene(mesh = mesh, ... )
}
