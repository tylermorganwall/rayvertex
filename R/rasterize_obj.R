#'@title Rasterize an OBJ file
#'
#'If the OBJ file has an MTL file, it must be located in the same directory as the OBJ file itself.
#'Additionally, the textures referenced in the MTL file must be specified as a 
#'relative path to the OBJ file's location.
#'
#'@param obj_model  Filename of the `obj` file.
#'@param shape Default `NULL`. The shape to render in the OBJ mesh. 
#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
rasterize_obj  = function(obj_model, shape = NULL, ...) {
  if(!file.exists(obj_model)) {
    stop(obj_model, " not found in current directory.")
  }
  obj_model = path.expand(obj_model)
  obj_path = dirname(obj_model)
  sep = .Platform$file.sep
  obj = read_obj(obj_model, materialspath = obj_path)
  if(!is.null(shape)) {
    if(length(obj$shapes) < shape) {
      stop("shape requested exceeds number of shapes in OBJ file")
    }
    obj$shapes = obj$shapes[shape]
  }
  rasterize_scene(mesh = obj, ... )
}
