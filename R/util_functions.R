#' Convert Color
#'
#' @param color The color to convert. Can be either a hexadecimal code, or a numeric rgb 
#' vector listing three intensities between `0` and `1`.
#'
#' @return Color vector
#' @keywords internal
convert_color = function(color, as_hex = FALSE) {
  if(inherits(color,"character")) {
    color = as.vector(grDevices::col2rgb(color))/255
  } 
  if(as_hex) {
    paste0("#",paste0(format(as.hexmode(round(color*255,0)),width=2),collapse=""),collapse="")
  } else {
    color
  }
}

#' Rescale
#'
#' @param vals Values
#' @param to to vales
#'
#' @return Color vector
#' @keywords internal
rescale = function(vals, to=c(0,1)) {
  range_vals = range(vals[!is.infinite(vals)],na.rm=TRUE)
  vals = (vals-range_vals[1])/(range_vals[2]-range_vals[1])
  (vals + to[1]) * (t[2]-t[1])
}

#' Check Filename
#'
#' @param file Filename to be checked
#' @return Flipped matrix
#' @keywords internal
get_file_type = function(file) {
  if(is.character(file)) {
    if(tools::file_ext(file) == "png") {
      imagetype = "png"
    } else {
      imagetype = "jpg"
    }
  } else if (length(dim(file)) == 3) {
    imagetype = "array"
  } else if (length(dim(file)) == 2) {
    imagetype = "matrix"
  } else {
    stop("`",file,"` not recognized class (png, jpeg, array, matrix).")
  }
  
}

#' Flip Left-Right
#'
#' @param x Matrix
#'
#' @return Flipped matrix
#' @keywords internal
fliplr = function(x) {
  if(length(dim(x)) == 2) {
    x[,ncol(x):1]
  } else {
    x[,ncol(x):1,]
  }
}


#' Flip Up-Down
#'
#' @param x Matrix
#'
#' @return Flipped matrix
#' @keywords internal
flipud = function(x) {
  if(length(dim(x)) == 2) {
    x[nrow(x):1,]
  } else {
    x[nrow(x):1,,]
  }
}

#' Print time
#'
#' @param verbose 
#' @param message 
#' 
#'
#' @return Nothing
#' @keywords internal
init_time = function() {
  assign("init_time", proc.time()[3], envir = ray_environment)
  assign("prev_time", proc.time()[3], envir = ray_environment)
}

#' Get time
#'
#' @return Nothing
#' @keywords internal
get_time = function(init = TRUE) {
  if(init) {
    get("init_time", envir = ray_environment)
  } else {
    get("prev_time", envir = ray_environment)
  }
}

#' Print time
#'
#' @param verbose 
#' @param message 
#' 
#'
#' @return Nothing
#' @keywords internal
print_time = function(verbose = FALSE, message_text = "") {
  if(verbose) {
    time_now = proc.time()[3]
    message(sprintf("%-27s: %0.1f secs (Total: %0.1f secs)",
                    message_text, 
                    time_now-get_time(FALSE),
                    time_now-get_time(TRUE)))
    assign("prev_time", time_now, envir = ray_environment)
  }
}

#' Run Documentation
#' 
#' @return bool
#'
#' @keywords internal
run_documentation = function() {
  return(identical(Sys.getenv("IN_PKGDOWN"), "true"))
}


#' Validate Scene
#'
#' @param scene Make sure that there are no out of bounds issues and all the materials are valid
#'
#' @return Color vector
#' @keywords internal
validate_scene = function(scene) {
  n_shapes = 0L
  
  n_materials = 0L
  n_material_hashes = 0L
  number_materials = 0L
  
  n_shapes = length(scene$shapes)
  
  if(n_shapes != length(scene$vertices)) {
    stop(sprintf("length of scene$shapes[[%d]] (%d) is not equal to length of scene$vertices[[%d]] (%d)",
                 i,n_shapes, i, length(scene$vertices)))
  }
  if(n_shapes != length(scene$texcoords)) {
    stop(sprintf("length of scene[[%d]]$shapes (%d) is not equal to length of scene[[%d]]$texcoords (%d)",
                 i,n_shapes, i, length(scene$texcoords)))
  }
  if(n_shapes != length(scene$normals)) {
    stop(sprintf("length of scene[[%d]]$shapes (%d) is not equal to length of scene[[%d]]$normals (%d)",
                 i,n_shapes, i, length(scene$normals)))
  }
  #Count the total number of shapes, materials, and material hashes
  for(i in seq_len(n_shapes)) {
    if(!(max(scene$shapes[[i]]$indices) < nrow(scene$vertices[[i]]))) {
      stop(sprintf("Max index (%d) in scene$shape[[%d]] greater than number of vertices (%d) in scene$vertices[[%d]]",
                   max(scene$shapes[[i]]$indices),i, nrow(scene$vertices[[i]]), i))
    }
    if(!(max(scene$shapes[[i]]$tex_indices) < nrow(scene$texcoords[[i]]))) {
      stop(sprintf("Max tex index (%d) in scene$shape[[%d]] greater than number of texcoords (%d) in scene$texcoords[[%d]]",
                   max(scene$shapes[[i]]$tex_indices),i, nrow(scene$texcoords[[i]]), i))
    }
    if(!(max(scene$shapes[[i]]$norm_indices) < nrow(scene$normals[[i]]))) {
      stop(sprintf("Max norm index (%d) in scene$shape[[%d]] greater than number of normals (%d) in scene$normals[[%d]]",
                   max(scene$shapes[[i]]$norm_indices),i, nrow(scene$normals[[i]]), i))
    }
  }
  for(i in seq_len(length(scene$materials))) {
    mat_len = length(scene$materials[[i]])
    for(j in seq_len(mat_len)) {
      if(length(scene$materials[[i]][[j]]) != 26) {
        stop(sprintf("Material %d (sub-material %d) does not have the right number of entries", i, j))
      }
    }
  }
  message("Scene passed validation")
}
