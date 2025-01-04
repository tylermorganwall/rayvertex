ray_environment = new.env(parent = emptyenv())

.onLoad = function(libname, pkgname) {
  options("ray_loading" = TRUE)
  on.exit(options("ray_loading" = NULL))
  assign("arrow", read_obj(system.file("extdata", "arrow.txt", package="rayvertex")), envir = ray_environment)
  assign("cone", read_obj(system.file("extdata", "cone.txt", package="rayvertex")), envir = ray_environment)
  assign("cube", read_obj(system.file("extdata", "cube.txt", package="rayvertex")), envir = ray_environment)
  assign("cylinder", read_obj(system.file("extdata", "cylinder.txt", package="rayvertex")), envir = ray_environment)
  assign("disk", read_obj(system.file("extdata", "disk.txt", package="rayvertex")), envir = ray_environment)
  assign("low_poly_sphere", read_obj(system.file("extdata", "low_poly_sphere.txt", package="rayvertex")), envir = ray_environment)
  assign("r_obj", read_obj(system.file("extdata", "r_obj.txt", package="rayvertex")), envir = ray_environment)
  assign("sphere", read_obj(system.file("extdata", "sphere.txt", package="rayvertex")), envir = ray_environment)
  assign("xy_plane", read_obj(system.file("extdata", "xy_plane.txt", package="rayvertex")), envir = ray_environment)
  assign("xz_plane", read_obj(system.file("extdata", "xz_plane.txt", package="rayvertex")), envir = ray_environment)
  assign("yz_plane", read_obj(system.file("extdata", "yz_plane.txt", package="rayvertex")), envir = ray_environment)
  assign("init_time", 0, envir = ray_environment)
  assign("prev_time", 0, envir = ray_environment)
  assign("pkg_loaded", TRUE, envir = ray_environment)
  
  #Register methods
  register_s3_method("rayvertex", "print", "ray_mesh")
  register_s3_method("rayvertex", "print", "rayvertex_material")
  register_s3_method("rayvertex", "print", "rayvertex_material_list")
  register_s3_method("rayvertex", "print", "ray_shape")
  
  register_s3_method("pillar", "tbl_sum", "print_raymesh_df")
  
  # register_s3_method("rayrender", "print", "ray_material")
  # register_s3_method("rayrender", "format", "ray_material")
  # register_s3_method("pillar", "pillar_shaft", "ray_material")
  # register_s3_method("vctrs", "vec_ptype_abbr", "ray_material")
  
  # register_s3_method("rayrender", "print", "ray_transform")
  # register_s3_method("rayrender", "format", "ray_transform")
  # register_s3_method("pillar", "pillar_shaft", "rayvertex_material")
  # register_s3_method("vctrs", "vec_ptype_abbr", "rayvertex_material")
  
  # register_s3_method("pillar", "pillar_shaft", "rayvertex_material")
  # register_s3_method("vctrs", "vec_ptype_abbr", "rayvertex_material")
  
  register_s3_method("pillar", "pillar_shaft", "rayvertex_material_list")
  register_s3_method("vctrs", "vec_ptype_abbr", "rayvertex_material_list")
  register_s3_method("pillar", "pillar_shaft", "rayvertex_material")
  
  register_s3_method("vctrs", "vec_ptype_abbr", "rayvertex_material")
  
  # register_s3_method("rayrender", "print", "ray_animated_transform")
  # register_s3_method("rayrender", "format", "ray_animated_transform")
  register_s3_method("pillar", "pillar_shaft", "ray_vertex_data")
  register_s3_method("vctrs", "vec_ptype_abbr", "ray_vertex_data")
  
  # register_s3_method("rayrender", "print", "ray_shape_info")
  # register_s3_method("rayrender", "format", "ray_shape_info")
  register_s3_method("pillar", "pillar_shaft", "ray_shape")
  register_s3_method("vctrs", "vec_ptype_abbr", "ray_shape")
  
  register_s3_method("pillar", "pillar_shaft", "ray_shape_list")
  register_s3_method("vctrs", "vec_ptype_abbr", "ray_shape_list")
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  
  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }
  
  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }
  
  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}