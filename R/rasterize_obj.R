#'@title Rasterize an OBJ file
#'
#'If the OBJ file has an MTL file, it must be located in the same directory as the OBJ file itself.
#'Additionally, the textures referenced in the MTL file must be specified as a 
#'relative path to the OBJ file's location.
#'
#'@param obj_model  Filename of the `obj` file.
#'@param filename Default `NULL`. Filename to save the image. If `NULL`, the image will be plotted.
#'@param width Default `400`. Width of the rendered image.
#'@param height Default `400`. Width of the rendered image.
#'@param line_info Default `NULL`. Matrix of line segments to add to the scene. Number of rows must be a multiple of 2.
#'@param parallel Default `TRUE`. Whether to use parallel processing.
#'@param fov Default `20`. Width of the rendered image.
#'@param lookfrom Default `c(0,0,10)`. Camera location.
#'@param lookat Default `NULL`. Camera focal position, defaults to the center of the model.
#'@param camera_up Default `c(0,1,0)`. Camera up vector.
#'@param scale_obj Default `1`. Value to scale size of model.
#'@param point_light_info Default `NULL`. A data.frame of point light information created using the 
#'`point_light()` and `add_light()` functions.
#'@param type Default `diffuse`. Shader type. Other options: `vertex` (Gouraud shading), `phong`, and `color` (no lighting).
#'@param color Default `darkred`. Color of model if no material file present (or for faces using the default material).
#'@param background Default `white`. Background color.
#'@param light_direction Default `c(1,1,1)`. Vector specifying the light direction for the primary directional light.
#'@param light_intensity Default `1.0`. Light intensity.
#'@param ambient_color Default `c(0,0,0)`. Ambient color of model if no material file present (or for faces using the default material).
#'@param exponent Default `32`.
#'@param specular_intensity Default `0.6`.
#'@param emission_intensity Default `1`.
#'@param override_exponent Default `FALSE`.
#'@param diffuse_intensity Default `1`.
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
#'@param culling Default `"back"`.
#'@param shader Default `"default"`.
#'@param block_size Default `4`. 
#'@param shape Default `NULL`
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
  rasterize_mesh(mesh = obj, ... )
}
