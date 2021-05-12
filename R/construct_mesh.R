#'@title Manually construct a mesh
#'
#'@param vertices Nx3 matrix of vertex coordinates..
#'@param indices Nx3 integer matrix, where each row defines a triangle using the 
#'vertices defined in `vertices`.
#'@param normals Default `NULL`.  Nx3 matrix of normals.
#'@param norm_indices Nx3 integer matrix, where each row defines the normal for a vertex using the 
#'normals defined in `normals` for the corresponding triangle in `indices`.
#'Required to be the same number of rows as `indices`.
#'@param texcoords Default `NULL`. Nx2 matrix of texture coordinates.
#'@param tex_indices Nx3 integer matrix, where each row defines the texture coordinates for a triangle
#' using the tex coords defined in `texcoors` for the corresponding triangle in `indices`.
#'Required to be the same number of rows as `indices`.
#' @param material Default `material_list()` (default values). Specify the material of the object.
#' 
#'@return Rasterized image.
#'@export
#'@examples
#'#Here we produce a ambient occlusion map of the `montereybay` elevation map.
construct_mesh  = function(vertices, indices, 
                           normals = NULL, norm_indices = NULL, 
                           texcoords = NULL, tex_indices = NULL,
                           material = material_list()) {
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
  mesh$vertices = vertices
  mesh$texcoords = texcoords
  mesh$normals = normals
  mesh$shapes[[1]] = list()
  mesh$shapes[[1]]$indices = indices
  mesh$shapes[[1]]$norm_indices = norm_indices
  mesh$shapes[[1]]$tex_indices = tex_indices
  mesh$shapes[[1]]$material_ids = rep(-1,nrow(indices))
  mesh$shapes[[1]]$has_vertex_tex = rep(nrow(indices) == nrow(tex_indices), nrow(indices))
  mesh$shapes[[1]]$has_vertex_normals = rep(nrow(indices) == nrow(norm_indices), nrow(indices))
  
  mesh$materials = list()
  mesh = set_material(mesh,material)
  
  return(mesh)
  
}
