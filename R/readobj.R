#'@title Load an OBJ file
#'
#'@description Loads an OBJ file and return a `ray_mesh` list structure. No processing is done on
#'the object other than loading it (unlike `obj_model()`).
#'
#'@param filename Filename of the OBJ file.
#'@param materialspath Directory where the MTL file is located. Defaults to the directory of `filename`.
#'@export
#'
#'@return `ray_mesh` list object 
#'#Load an arrow OBJ
#'sphere = read_obj(system.file("extdata", "arrow.txt", package="rayvertex"))
read_obj = function(filename, materialspath = NULL) {
  filename = path.expand(filename)
  sepval = ""
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
  single_indices            = list()
  single_tex_indices        = list()
  single_norm_indices       = list()
  single_material_ids       = list()
  single_has_vertex_tex     = list()
  single_has_vertex_normals = list()
  
  for(i in seq_len(length(obj_loaded$shapes))) {
    single_indices[[i]]            = obj_loaded$shapes[[i]]$indices
    single_tex_indices[[i]]        = obj_loaded$shapes[[i]]$tex_indices
    single_norm_indices[[i]]       = obj_loaded$shapes[[i]]$norm_indices
    single_material_ids[[i]]       = obj_loaded$shapes[[i]]$material_ids
    single_has_vertex_tex[[i]]     = obj_loaded$shapes[[i]]$has_vertex_tex
    single_has_vertex_normals[[i]] = obj_loaded$shapes[[i]]$has_vertex_normals
  }
  
  obj_loaded$shapes = list(list(indices = do.call(rbind,single_indices),
                           tex_indices = do.call(rbind,single_tex_indices),
                           norm_indices = do.call(rbind,single_norm_indices),
                           material_ids = do.call(rbind,single_material_ids),
                           has_vertex_tex = do.call(c,single_has_vertex_tex),
                           has_vertex_normals = do.call(c,single_has_vertex_normals)))
  for(i in seq_len(length(obj_loaded$materials[[1]]))) {
    if(!file.exists(obj_loaded$materials[[1]][[i]]$diffuse_texname) && nchar(obj_loaded$materials[[1]][[i]]$diffuse_texname) > 0 &&
       file.exists(sprintf("%s%s%s", dir,sepval,obj_loaded$materials[[1]][[i]]$diffuse_texname))) {
      obj_loaded$materials[[1]][[i]]$diffuse_texname = sprintf("%s%s%s", dir,sepval,obj_loaded$materials[[1]][[i]]$diffuse_texname)
    }
    if(!file.exists(obj_loaded$materials[[1]][[i]]$ambient_texname) && nchar(obj_loaded$materials[[1]][[i]]$ambient_texname) > 0 &&
       file.exists(sprintf("%s%s%s", dir,sepval,obj_loaded$materials[[1]][[i]]$ambient_texname))) {
      obj_loaded$materials[[1]][[i]]$ambient_texname = sprintf("%s%s%s", dir,sepval,obj_loaded$materials[[1]][[i]]$ambient_texname)
    }
    if(!file.exists(obj_loaded$materials[[1]][[i]]$emissive_texname) &&  nchar(obj_loaded$materials[[1]][[i]]$emissive_texname) > 0 &&
       file.exists(sprintf("%s%s%s", dir,sepval,obj_loaded$materials[[1]][[i]]$emissive_texname))) {
      obj_loaded$materials[[1]][[i]]$emissive_texname = sprintf("%s%s%s", dir,sepval,obj_loaded$materials[[1]][[i]]$emissive_texname)
    }
    if(!file.exists(obj_loaded$materials[[1]][[i]]$specular_texname) &&  nchar(obj_loaded$materials[[1]][[i]]$specular_texname) > 0 &&
       file.exists(sprintf("%s%s%s", dir,sepval,obj_loaded$materials[[1]][[i]]$specular_texname))) {
      obj_loaded$materials[[1]][[i]]$specular_texname = sprintf("%s%s%s", dir,sepval,obj_loaded$materials[[1]][[i]]$specular_texname)
    }
    if(!file.exists(obj_loaded$materials[[1]][[i]]$normal_texname) &&  nchar(obj_loaded$materials[[1]][[i]]$normal_texname) > 0 &&
       file.exists(sprintf("%s%s%s", dir,sepval,obj_loaded$materials[[1]][[i]]$normal_texname))) {
      obj_loaded$materials[[1]][[i]]$normal_texname = sprintf("%s%s%s", dir,sepval,obj_loaded$materials[[1]][[i]]$normal_texname)
    }
    if(obj_loaded$materials[[1]][[i]]$illum == 5) {
      obj_loaded$materials[[1]][[i]]$type = "color"
    } else {
      obj_loaded$materials[[1]][[i]]$type = "diffuse"
    }
  }
  hashes = rep("",length(obj_loaded$materials[[1]]))
  for(i in seq_len(length(obj_loaded$materials[[1]]))) {
    hashes[i] = digest::digest(obj_loaded$materials[[1]][[i]])
  }
  obj_loaded$material_hashes = hashes
  class(obj_loaded) = c("ray_mesh", "list")
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
  class(ply_loaded) = c("ray_mesh","list")
  ply_loaded
}