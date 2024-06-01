#'@title Calculate Smooth Mesh Normals
#'
#'@param mesh The mesh.
#'@param id Default `NA` (all shapes). The shape index to have new normals calculated.
#'
#'@return Mesh with new vertex normals
#'@export
#'@examples
#'if(run_documentation()) {
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
smooth_normals_mesh = function(mesh, 
                               id = NA) {
  stopifnot(inherits(mesh,"ray_mesh"))
  if(is.na(id)) {
    for(i in seq_len(length(mesh$shapes))) {
      new_normals = CalculateNormals(mesh, i-1)
      mesh$shapes[[i]]$norm_indices = mesh$shapes[[i]]$indices
      mesh$shapes[[i]]$has_vertex_normals = rep(TRUE, length(mesh$shapes[[i]]$indices))
      mesh$normals[i] = ray_vertex_data(new_normals)
    }
  } else {
    stopifnot(id <= length(mesh$shapes))
    new_normals = CalculateNormals(mesh, id-1)
    mesh$shapes[[id]]$norm_indices = mesh$shapes[[id]]$indices
    mesh$shapes[[id]]$has_vertex_normals = rep(TRUE, length(mesh$shapes[[id]]$indices))
    mesh$normals[id] = ray_vertex_data(new_normals)
  }
  return(mesh)
}
