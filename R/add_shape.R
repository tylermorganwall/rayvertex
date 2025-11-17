#'@title Add Shape
#'
#'@description Add shape to the scene.
#'
#'@param scene The scene to add the shape.
#'@param shape The mesh to add to the scene.

#'@return Scene with shape added.
#'@export
#'
#'@examples
#'if(run_documentation()) {
#'#Generate several spheres in the cornell box
#'scene = generate_cornell_mesh()
#'set.seed(1)
#'
#'for(i in 1:30) {
#'  col = hsv(runif(1))
#'  scene = add_shape(scene, sphere_mesh(position=runif(3)*400+155/2,
#'                                       material=material_list(diffuse=col, type="phong",
#'                                                              ambient=col,ambient_intensity=0.2),
#'                                       radius=30))
#'}
#'rasterize_scene(scene, light_info=directional_light(direction=c(0.1,0.6,-1)))
#'}
add_shape = function(scene, shape = NULL) {
	if (is.null(shape)) {
		return(scene)
	}
	if (is.null(scene)) {
		return(shape)
	}
	new_hashes = c(attr(scene, "material_hashes"), attr(shape, "material_hashes"))
	scene$shapes = vctrs::vec_c(scene$shapes, shape$shapes)
	scene$vertices = vctrs::vec_c(scene$vertices, shape$vertices)
	scene$normals = vctrs::vec_c(scene$normals, shape$normals)
	scene$texcoords = vctrs::vec_c(scene$texcoords, shape$texcoords)
	scene$materials = vctrs::vec_c(scene$materials, shape$materials)
	# new_scene = ray_mesh(scene)
	new_scene = (scene)

	attr(new_scene, "material_hashes") = new_hashes
	if (!is.null(attr(shape, "cornell")) || !is.null(attr(scene, "cornell"))) {
		attr(new_scene, "cornell") = TRUE
		if (!is.null(attr(shape, "cornell"))) {
			attr(new_scene, "cornell_light") = attr(shape, "cornell_light")
		} else {
			attr(new_scene, "cornell_light") = attr(scene, "cornell_light")
		}
	}
	return(new_scene)
}
