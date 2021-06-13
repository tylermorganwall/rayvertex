#'@title Load an OBJ file
#'
#'@keywords internal
#'
#'@return Rasterized image.
read_obj = function(filename, materialspath = NULL) {
  filename = path.expand(filename)
  if(!file.exists(filename)) {
    stop(sprintf("file `%s` does not exist", filename))
  }
  if(is.null(materialspath)) {
    dir = dirname(filename)
  } else {
    dir = materialspath
  }
  lastchar = substr(dir, nchar(dir), nchar(dir))
  fsep = .Platform$file.sep
  if(lastchar!=fsep) {
    dir=paste0(dir,fsep)
  }
  obj_loaded = load_obj(filename, dir)
  for(i in seq_len(length(obj_loaded$shapes))) {
    if(nrow(obj_loaded$shapes[[i]]$indices) == length(obj_loaded$shapes[[i]]$has_vertex_tex)) {
      obj_loaded$shapes[[i]]$has_vertex_tex[apply(obj_loaded$shapes[[i]]$tex_indices,1,(function(x) any(x == -1)))] = FALSE
    }
    if(nrow(obj_loaded$shapes[[i]]$indices) == length(obj_loaded$shapes[[i]]$has_vertex_normals)) {
      obj_loaded$shapes[[i]]$has_vertex_normals[apply(obj_loaded$shapes[[i]]$norm_indices,1,(function(x) any(x == -1)))] = FALSE
    }
  }
  hashes = rep("",length(obj_loaded$materials))
  for(i in seq_len(length(obj_loaded$materials))) {
    hashes[i] = digest::digest(obj_loaded$materials[[i]])
  }
  obj_loaded$material_hashes = hashes
  obj_loaded
}

#'@title Load an PLY file
#'
#'@keywords internal
#'
#'@return List
read_ply = function(filename) {
  filename = path.expand(filename)
  if(!file.exists(filename)) {
    stop(sprintf("file `%s` does not exist", filename))
  }
  dir = dirname(filename)
  lastchar = substr(dir, nchar(dir), nchar(dir))
  fsep = .Platform$file.sep
  if(lastchar!=fsep) {
    dir=paste0(dir,fsep)
  }
  ply_loaded = load_ply(filename, dir)
  for(i in seq_len(length(ply_loaded$shapes))) {
    if(nrow(ply_loaded$shapes[[i]]$indices) == length(ply_loaded$shapes[[i]]$has_vertex_tex)) {
      ply_loaded$shapes[[i]]$has_vertex_tex[apply(ply_loaded$shapes[[i]]$tex_indices,1,(function(x) any(x == -1)))] = FALSE
    }
    if(nrow(ply_loaded$shapes[[i]]$indices) == length(ply_loaded$shapes[[i]]$has_vertex_normals)) {
      ply_loaded$shapes[[i]]$has_vertex_normals[apply(ply_loaded$shapes[[i]]$norm_indices,1,(function(x) any(x == -1)))] = FALSE
    }
  }
  hashes = rep("",length(ply_loaded$materials))
  for(i in seq_len(length(ply_loaded$materials))) {
    hashes[i] = digest::digest(ply_loaded$materials[[i]])
  }
  ply_loaded$material_hashes = hashes
  ply_loaded
}