#'@title Calculate Smooth Mesh Normals
#'
#'@param mesh The mesh.
#'@param override_existing Default `FALSE`. Whether to override existing normals.
#'
#'@return Mesh with new vertex normals
#'@export
#'@examples
#'if(rayvertex:::run_documentation()) {
#'  #Let's construct a mesh from the volcano dataset
#'  #Build the vertex matrix
#'   vertex_list = list()
#'   counter = 1
#'   for(i in 1:nrow(volcano)) {
#'     for(j in 1:ncol(volcano)) {
#'       vertex_list[[counter]] = matrix(c(j,volcano[i,j],i), ncol=3)
#'       counter = counter + 1
#'     }
#'   }
#'   vertices = do.call(rbind,vertex_list)
#'   
#'   #Build the index matrix
#'   index_list = list()
#'   counter = 0
#'   for(i in 1:(nrow(volcano)-1)) {
#'     for(j in 1:(ncol(volcano)-1)) {
#'       index_list[[counter+1]] = matrix(c(counter,counter+ncol(volcano),counter+1,
#'                                          counter+ncol(volcano),counter+ncol(volcano)+1,counter + 1), 
#'                                        nrow=2, ncol=3, byrow=TRUE)
#'       counter = counter + 1
#'     }
#'     counter = counter + 1
#'   }
#'   indices = do.call(rbind,index_list)
#'   #Construct the mesh
#'   volc_mesh = construct_mesh(vertices = vertices, indices = indices,
#'                              material = material_list(type="diffuse", diffuse="darkred", 
#'                                                       ambient = "darkred", ambient_intensity=0.2))
#'   #Rasterize the no-normal scene
#'   scale_mesh(volc_mesh, scale = c(1,1/3,1)) |> 
#'     center_mesh() |> 
#'     rasterize_scene(lookfrom=c(-50,50,100),lookat=c(7,-15,0), fov=40,width=800,height=800,
#'                     light_info = directional_light(c(0,1,1)) |>
#'                       add_light(directional_light(c(1,1,-1))))
#'                       
#'   #Smooth the mesh
#'   volc_mesh_smooth = smooth_normals_mesh(volc_mesh)
#'   
#'   #Rasterize the scene
#'   scale_mesh(volc_mesh_smooth, scale = c(1,1/3,1)) |> 
#'     center_mesh() |> 
#'     rasterize_scene(lookfrom=c(-50,50,100),lookat=c(7,-15,0), fov=40,width=800,height=800,
#'                     light_info = directional_light(c(0,1,1)) |>
#'                       add_light(directional_light(c(1,1,-1))))
#' }
smooth_normals_mesh = function(mesh, override_existing = FALSE) {
  stopifnot(inherits(mesh,"ray_mesh"))
  new_mesh = smooth_normals_mesh_rcpp(mesh, override_existing = override_existing)
  return(new_mesh)
}