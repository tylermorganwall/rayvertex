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
#'#Let's construct a mesh from the volcano dataset
#' #Build the vertex matrix
#' vertex_list = list()
#' counter = 1
#' for(i in 1:nrow(volcano)) {
#'   for(j in 1:ncol(volcano)) {
#'     vertex_list[[counter]] = matrix(c(j,volcano[i,j],i), ncol=3)
#'     counter = counter + 1
#'   }
#' }
#' vertices = do.call(rbind,vertex_list)
#' 
#' #Build the index matrix
#' index_list = list()
#' counter = 0
#' for(i in 1:(nrow(volcano)-1)) {
#'   for(j in 1:(ncol(volcano)-1)) {
#'     index_list[[counter+1]] = matrix(c(counter,counter+ncol(volcano),counter+1,
#'                                        counter+ncol(volcano),counter+ncol(volcano)+1,counter + 1), 
#'                                      nrow=2, ncol=3, byrow=TRUE)
#'     counter = counter + 1
#'   }
#'   counter = counter + 1
#' }
#' indices = do.call(rbind,index_list)
#' 
#' #Construct the mesh
#' volc_mesh = construct_mesh(vertices = vertices, indices = indices,
#'                            material = material_list(type="phong", diffuse="darkred", 
#'                                                     ambient = "darkred", ambient_intensity=0.2))
#' 
#' #Rasterize the scene
#' rasterize_scene(volc_mesh, lookfrom=c(-50,230,100),fov=60,width=1200,height=1200,
#'                 light_info = directional_light(c(0,1,1)) %>% 
#'                   add_light(directional_light(c(1,1,-1))))
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
    norm_indices = matrix(-1,nrow=nrow(indices),ncol=3)
  }
  if(is.null(tex_indices)) {
    tex_indices = matrix(-1,nrow=nrow(indices),ncol=3)
  }
  mesh$vertices = vertices
  mesh$texcoords = texcoords
  mesh$normals = normals
  mesh$shapes[[1]] = list()
  mesh$shapes[[1]]$indices = indices
  mesh$shapes[[1]]$norm_indices = norm_indices
  mesh$shapes[[1]]$tex_indices = tex_indices
  mesh$shapes[[1]]$material_ids = rep(-1,nrow(indices))
  mesh$shapes[[1]]$has_vertex_tex = apply(tex_indices,1,(function(x) all(x != -1)))
  mesh$shapes[[1]]$has_vertex_normals = apply(norm_indices,1,(function(x) all(x != -1)))
  
  mesh$materials = list()
  mesh = set_material(mesh,material)
  
  return(mesh)
  
}
