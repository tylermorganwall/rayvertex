#'@title Translate Mesh
#'
#'@param mesh The mesh.
#'@param position Default `c(0,0,0)`. The translation vector. 
#'
#'@return Translated mesh
#'@export
#'@examples
#'if(run_documentation()) {
#'#Translate a mesh in the Cornell box
#'robj = obj_mesh(r_obj(), scale=80,angle=c(0,180,0))
#'generate_cornell_mesh() |>
#'  add_shape(translate_mesh(robj,c(400,0,155))) |>
#'  add_shape(translate_mesh(robj,c(555/2,100,555/2))) |>
#'  add_shape(translate_mesh(robj,c(155,200,400))) |>
#'  rasterize_scene(light_info=directional_light(direction=c(0.1,0.6,-1)))
#'}
translate_mesh = function(mesh, position = c(0,0,0)) {
  for(j in seq_len(length(mesh$vertices))) {
    mesh$vertices[[j]][,1]  = mesh$vertices[[j]][,1] + position[1]
    mesh$vertices[[j]][,2]  = mesh$vertices[[j]][,2] + position[2]
    mesh$vertices[[j]][,3]  = mesh$vertices[[j]][,3] + position[3]
  }
  class(mesh) = c("ray_mesh", "list")
  
  return(mesh)
}

#'@title Scale Mesh
#'
#'@param mesh The mesh.
#'@param scale Default `c(1,1,1)`. The scale amount, per axis. 
#'@param center Default `c(0,0,0)`. The center of the scale.
#'
#'@return Scaled mesh
#'@export
#'@examples
#'if(run_documentation()) {
#'#Scale a mesh in the Cornell box
#'robj = obj_mesh(r_obj(), scale=80,angle=c(0,180,0))
#'
#'generate_cornell_mesh() |>
#' add_shape(scale_mesh(translate_mesh(robj,c(400,0,155)),0.5, center=c(400,0,155))) |>
#' add_shape(scale_mesh(translate_mesh(robj,c(555/2,100,555/2)),1.5, center=c(555/2,100,555/2))) |>
#' add_shape(scale_mesh(translate_mesh(robj,c(155,200,400)),c(0.5,2,0.5), center=c(155,200,400))) |>
#' rasterize_scene(light_info=directional_light(direction=c(0.1,0.6,-1)))
#' }
scale_mesh = function(mesh, scale = 1, center = c(0,0,0)) {
  if(length(scale) == 1) {
    scale = rep(scale,3)
  }
  for(j in seq_len(length(mesh$vertices))) {
    mesh$vertices[[j]][,1]  = (mesh$vertices[[j]][,1]-center[1])*scale[1] + center[1]
    mesh$vertices[[j]][,2]  = (mesh$vertices[[j]][,2]-center[2])*scale[2] + center[2]
    mesh$vertices[[j]][,3]  = (mesh$vertices[[j]][,3]-center[3])*scale[3] + center[3]
    
    if(!is.null(mesh$normals[[j]]) && nrow(mesh$normals[[j]]) > 0) {
      mesh$normals[[j]][,1]  = mesh$normals[[j]][,1]*1/scale[1]
      mesh$normals[[j]][,2]  = mesh$normals[[j]][,2]*1/scale[2]
      mesh$normals[[j]][,3]  = mesh$normals[[j]][,3]*1/scale[3]
      for(i in seq_len(nrow(mesh$normals[[j]]))) {
        length_single = sqrt(mesh$normals[[j]][i,1]^2 + mesh$normals[[j]][i,2]^2 + mesh$normals[[j]][i,3]^2)
        mesh$normals[[j]][i,] = mesh$normals[[j]][i,]/length_single
      }
    }
  }
  class(mesh) = c("ray_mesh", "list")
  
  return(mesh)
}

#'@title Scale Mesh to Unit Bounding Box
#'
#'@param mesh The mesh.
#'@param center_mesh Default `FALSE`. Whether to center the mesh at the origin after scaling.
#'
#'@return Scaled mesh
#'@export
#'@examples
#'if(run_documentation()) {
#'#Scale a mesh in the Cornell box
#'robj = obj_mesh(r_obj(), scale=80,angle=c(0,180,0))
#'
#'generate_cornell_mesh() |>
#' add_shape(scale_mesh(translate_mesh(robj,c(400,0,155)),0.5, center=c(400,0,155))) |>
#' add_shape(scale_mesh(translate_mesh(robj,c(555/2,100,555/2)),1.5, center=c(555/2,100,555/2))) |>
#' add_shape(scale_mesh(translate_mesh(robj,c(155,200,400)),c(0.5,2,0.5), center=c(155,200,400))) |>
#' scale_unit_mesh(center_mesh = TRUE) |> 
#' rasterize_scene(light_info=directional_light(direction=c(0.1,0.6,-1)), 
#'                 lookfrom = c(0,0,-2), lookat=c(0,0,0))
#' }
scale_unit_mesh = function(mesh, center_mesh = FALSE) {
  center = get_mesh_center(mesh)
  bbox = get_mesh_bbox(mesh)
  scale_xyz = bbox["max",] - bbox["min",]
  scale_xyz[scale_xyz == 0] = 1
  scale = 1 / scale_xyz
  mesh = scale_mesh(mesh, scale = scale, center = center)
  if(center_mesh) {
    mesh = center_mesh(mesh)
  }
  return(mesh)
}

#'@title Center Mesh
#'
#'@description Centers the mesh at the origin.
#'
#'@param mesh The mesh object.
#'
#'@return Centered mesh
#'@export
#'@examples
#'if(run_documentation()) {
#' #Center the Cornell box and the R OBJ at the origin
#' center_mesh(generate_cornell_mesh()) |>
#'   add_shape(center_mesh(obj_mesh(r_obj(),scale=100,angle=c(0,180,0)))) |>
#'   rasterize_scene(lookfrom=c(0,0,-1100),fov=40,lookat=c(0,0,0),
#'                   light_info = directional_light(c(0.4,0.4,-1)) |>
#'       add_light(point_light(c(0,450,0),  falloff_quad = 0.0, constant = 0.0002, falloff = 0.005)))
#' }
center_mesh = function(mesh) {
  center_mat = matrix(c(Inf,-Inf),nrow=2,ncol=3)
  for(j in seq_len(length(mesh$vertices))) {
    center_tmp = apply(mesh$vertices[[j]],2,range)
    center_mat[1,] = pmin(center_tmp[1,],center_mat[1,])
    center_mat[2,] = pmax(center_tmp[2,],center_mat[2,])
  }
  center = apply(center_mat,2,mean)
  
  mesh = translate_mesh(mesh, -center)
  class(mesh) = c("ray_mesh", "list")
  return(mesh)
}

#'@title Get Mesh Center
#'
#'@description Calculates the coordinates of the center of a mesh
#'
#'@param mesh The mesh object.
#'
#'@return Length-3 numeric vector 
#'@export
#'@examples
#' if(run_documentation()) {
#' #Calculates the center of the mesh
#' get_scene_center(generate_cornell_mesh())
#' }
get_mesh_center = function(mesh) {
  center_mat = matrix(c(Inf,-Inf),nrow=2,ncol=3)
  for(j in seq_len(length(mesh$vertices))) {
    center_tmp = apply(mesh$vertices[[j]],2,range)
    center_mat[1,] = pmin(center_tmp[1,],center_mat[1,])
    center_mat[2,] = pmax(center_tmp[2,],center_mat[2,])
  }
  center = apply(center_mat,2,mean)
  return(center)
}

#'@title Get Mesh Bounding Box
#'
#'@description Calculates the bounding box of a mesh
#'
#'@param mesh The mesh object.
#'
#'@return 2x3 numeric matrix
#'@export
#'@examples
#' if(run_documentation()) {
#' #Calculates the center of the mesh
#' get_mesh_bbox(generate_cornell_mesh())
#' }
get_mesh_bbox = function(mesh) {
  center_mat = matrix(c(Inf,-Inf),nrow=2,ncol=3)
  for(j in seq_len(length(mesh$vertices))) {
    center_tmp = apply(mesh$vertices[[j]],2,range)
    center_mat[1,] = pmin(center_tmp[1,],center_mat[1,])
    center_mat[2,] = pmax(center_tmp[2,],center_mat[2,])
  }
  colnames(center_mat) = c("x","y","z")
  rownames(center_mat) = c("min","max")
  return(center_mat)
}

#'@title Generate Rotation Matrix
#'
#'@param angle The angle
#'@param order_rotation Default `c(1,2,3)`. 
#'@return Matrix
#'@keywords internal
generate_rot_matrix = function(angle, order_rotation) {
  rots = list()
  rots[[1]] = matrix(c(1,0,0,0,cos(angle[1]),sin(angle[1]),0,-sin(angle[1]),cos(angle[1])),3,3)
  rots[[2]] = matrix(c(cos(angle[2]),0,-sin(angle[2]),0,1,0,sin(angle[2]),0,cos(angle[2])),3,3)
  rots[[3]] = matrix(c(cos(angle[3]),sin(angle[3]),0,-sin(angle[3]),cos(angle[3]),0,0,0,1),3,3)
  returnmat = matrix(c(1,0,0,0,1,0,0,0,1),3,3)
  for(i in 1:3) {
    returnmat = returnmat %*% rots[[order_rotation[i]]]
  }
  return(returnmat)
}

#'@title Rotate Mesh
#'
#'@param mesh The mesh.
#'@param angle Default `c(0,0,0)`. The rotation amount for the x/y/z axes, in degrees.
#'@param pivot_point Default `c(0,0,0)`. The pivot point of the rotation.
#'@param order_rotation Default `c(1,2,3)`. The order in which to perform the rotations.
#'
#'@return Rotated Mesh
#'@export
#'@examples
#'if(run_documentation()) {
#'#Rotate a mesh in the Cornell box
#'robj = obj_mesh(r_obj(), scale=80,angle=c(0,180,0))
#'
#'generate_cornell_mesh() |>
#' add_shape(rotate_mesh(translate_mesh(robj,c(400,0,155)),c(0,30,0), 
#'                       pivot_point=c(400,0,155))) |>
#' add_shape(rotate_mesh(translate_mesh(robj,c(555/2,100,555/2)),c(-30,60,30), 
#'                       pivot_point=c(555/2,100,555/2))) |>
#' add_shape(rotate_mesh(translate_mesh(robj,c(155,200,400)),c(-30,60,30), 
#'                       pivot_point=c(155,200,400), order_rotation=c(3,2,1))) |>
#' rasterize_scene(light_info=directional_light(direction=c(0.1,0.6,-1)))
#' }
rotate_mesh = function(mesh, angle = c(0,0,0), pivot_point = c(0,0,0), order_rotation = c(1,2,3)) {
  angle = angle*pi/180
  for(j in seq_len(length(mesh$vertices))) {
    mesh$vertices[[j]][,1]  = mesh$vertices[[j]][,1]-pivot_point[1]
    mesh$vertices[[j]][,2]  = mesh$vertices[[j]][,2]-pivot_point[2]
    mesh$vertices[[j]][,3]  = mesh$vertices[[j]][,3]-pivot_point[3]
    rot_mat = generate_rot_matrix(angle, order_rotation)
    for(i in seq_len(nrow(mesh$vertices[[j]]))) {
      mesh$vertices[[j]][i,] = mesh$vertices[[j]][i,] %*% rot_mat
    }
    mesh$vertices[[j]][,1]  = mesh$vertices[[j]][,1]+pivot_point[1]
    mesh$vertices[[j]][,2]  = mesh$vertices[[j]][,2]+pivot_point[2]
    mesh$vertices[[j]][,3]  = mesh$vertices[[j]][,3]+pivot_point[3]
    
    if(!is.null(mesh$normals[[j]]) && nrow(mesh$normals[[j]]) > 0) {
      inv_t = t(solve(rot_mat))
      for(i in seq_len(nrow(mesh$normals[[j]]))) {
        mesh$normals[[j]][i,] = mesh$normals[[j]][i,] %*% inv_t
      }
    }
  }
  class(mesh) = c("ray_mesh", "list")
  
  return(mesh)
}
