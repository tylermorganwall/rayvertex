ray_environment = new.env(parent = emptyenv())

.onLoad = function(libname, pkgname) {
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
  assign("yz_plane", read_obj(system.file("extdata", "xz_plane.txt", package="rayvertex")), envir = ray_environment)
  assign("init_time", 0, envir = ray_environment)
  assign("prev_time", 0, envir = ray_environment)
  assign("pkg_loaded", TRUE, envir = ray_environment)
}