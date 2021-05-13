#'@title Rasterize Lines
#'
#'@description Render a 3D scene made out of lines using a software rasterizer.
#'
#'@param line_info The mesh object.
#'@param filename Default `NULL`. Filename to save the image. If `NULL`, the image will be plotted.
#'@param width Default `400`. Width of the rendered image.
#'@param height Default `400`. Width of the rendered image.
#'@param alpha_line Default `1`. Line transparency.
#'@param parallel Default `TRUE`. Whether to use parallel processing.
#'@param fov Default `20`. Width of the rendered image.
#'@param lookfrom Default `c(0,0,10)`. Camera location.
#'@param lookat Default `NULL`. Camera focal position, defaults to the center of the model.
#'@param camera_up Default `c(0,1,0)`. Camera up vector.
#'@param color Default `darkred`. Color of model if no material file present (or for faces using the default material).
#'@param background Default `white`. Background color.
#'@param debug Default `"none"`.
#'@param near_plane Default `0.1`.
#'@param far_plane Default `100`.
#'@param block_size Default `4`. 
#'@param ortho_dimensions Default `c(1,1)`. Width and height of the orthographic camera. Will only be used if `fov = 0`. 
#'@param bloom Default `FALSE`. Whether to apply bloom to the image. If `TRUE`,
#' this performs a convolution of the HDR image of the scene with a sharp, long-tailed
#' exponential kernel, which does not visibly affect dimly pixels, but does result in emitters light
#' slightly bleeding into adjacent pixels. 
#'@param antialias_lines Default `TRUE`. Whether to anti-alias lines in the scene.
#'
#'@return Rasterized image.
#'@export
#'@examples
#' #Generate a cube out of lines
#' cube_outline = generate_line(start = c(-1, -1, -1), end = c(-1, -1, 1)) %>%
#'   add_lines(generate_line(start = c(-1, -1, -1), end = c(-1, 1, -1))) %>%
#'   add_lines(generate_line(start = c(-1, -1, -1), end = c(1, -1, -1))) %>%
#'   add_lines(generate_line(start = c(-1, -1, 1), end = c(-1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(-1, -1, 1), end = c(1, -1, 1))) %>%
#'   add_lines(generate_line(start = c(-1, 1, 1), end = c(-1, 1, -1))) %>%
#'   add_lines(generate_line(start = c(-1, 1, 1), end = c(1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(1, 1, -1), end = c(1, -1, -1))) %>%
#'   add_lines(generate_line(start = c(1, 1, -1), end = c(1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(1, -1, -1), end = c(1, -1, 1))) %>%
#'   add_lines(generate_line(start = c(1, -1, 1), end = c(1, 1, 1))) %>%
#'   add_lines(generate_line(start = c(-1, 1, -1), end = c(1, 1, -1)))
#' rasterize_lines(cube_outline,fov=90,lookfrom=c(0,0,3))
#' 
#' #Scale the cube uniformly
#' scaled_cube = color_lines(scale_lines(cube_outline,scale=0.5),color="red")
#' rasterize_lines(add_lines(cube_outline,scaled_cube),fov=90,lookfrom=c(0,0,3))
#' 
#' #Scale the cube non-uniformly
#' scaled_cube = color_lines(scale_lines(cube_outline,scale=c(0.8,2,0.4)),color="red")
#' rasterize_lines(add_lines(cube_outline,scaled_cube),fov=60,lookfrom=c(3,3,3))
rasterize_lines  = function(line_info = NULL, 
                           filename = NA, width=800, height=800, 
                           alpha_line = 1.0,
                           parallel = TRUE,
                           fov=20,lookfrom=c(0,0,10),lookat=NULL, camera_up = c(0,1,0), #Sanitize lookfrom and lookat inputs
                           color="red", background = "black", 
                           debug = "none", 
                           near_plane = 0.1, far_plane = 100, 
                           block_size = 4, 
                           ortho_dimensions = c(1,1), 
                           bloom = FALSE, 
                           antialias_lines = TRUE) {
  if(is.null(line_info)) {
    stop("no lines info passed to argument")
  }
  lineranges= rbind(line_info[,1:3],line_info[,4:6])
  bounds = as.vector(t(apply(lineranges,2,range)))

  if(is.null(lookat)) {
    lookat = (bounds[1:3] + bounds[4:6])/2
    message(sprintf("Setting `lookat` to: c(%0.2f, %0.2f, %0.2f)",lookat[1],lookat[2],lookat[3]))
  }

  if(!is.null(options("cores")[[1]])) {
    numbercores = options("cores")[[1]]
  } else {
    numbercores = parallel::detectCores()
  }
  if(!parallel) {
    numbercores = 1
  }

  color = convert_color(color)
  bg_color = convert_color(background)

  line_offset = 0
  imagelist = rasterize_lines_rcpp(line_mat=line_info,
                        nx=width,
                        ny=height,
                        model_color = color,
                        lookfrom=lookfrom,
                        lookat=lookat,
                        fov=fov,
                        near_clip=near_plane, far_clip=far_plane,
                        bounds=bounds, camera_up=camera_up,
                        alpha_line=alpha_line, line_offset=line_offset,
                        ortho_dims=ortho_dimensions,
                        aa_lines=antialias_lines)
  if(debug == "normals") {
    norm_array = array(0,dim=c(dim(imagelist$r)[2:1],3))
    norm_array[,,1] = (imagelist$normalx+1)/2
    norm_array[,,2] = (imagelist$normaly+1)/2
    norm_array[,,3] = (imagelist$normalz+1)/2
    norm_array = rayimage::render_reorient(norm_array,transpose = TRUE, flipx = TRUE)
    rayimage::plot_image(norm_array)
    return(invisible(norm_array))
  }
  if(debug == "depth") {
    depth_array = array(0,dim=c(dim(imagelist$r)[2:1],3))
    depth_array[,,1] = (imagelist$depth)
    depth_array[,,2] = (imagelist$depth)
    depth_array[,,3] = (imagelist$depth)
    depth_array = rayimage::render_reorient(depth_array,transpose = TRUE, flipx = TRUE)
    depth_array[is.infinite(depth_array)] = 1.2
    scale_factor = max(depth_array) - min(depth_array)
    depth_array = (depth_array - min(depth_array))/scale_factor
    rayimage::plot_image(depth_array)
    return(invisible(depth_array))
  }
  if(debug == "position") {
    pos_array = array(0,dim=c(dim(imagelist$r)[2:1],3))
    imagelist$positionx = rescale(imagelist$positionx,to=c(0,1))
    imagelist$positiony = rescale(imagelist$positiony,to=c(0,1))
    imagelist$positionz = rescale(imagelist$positionz,to=c(0,1))

    pos_array[,,1] = (imagelist$positionx)
    pos_array[,,2] = (imagelist$positiony)
    pos_array[,,3] = (imagelist$positionz)
    pos_array = rayimage::render_reorient(pos_array,transpose = TRUE, flipx = TRUE)
    pos_array[is.infinite(pos_array)] = 1
    rayimage::plot_image(pos_array)
    return(invisible(pos_array))
  }
  if(debug == "uv") {
    uv_array = array(0,dim=c(dim(imagelist$r)[2:1],3))
    uv_array[,,1] = (imagelist$uvx)
    uv_array[,,2] = (imagelist$uvy)
    uv_array[,,3] = (imagelist$uvz)
    uv_array = rayimage::render_reorient(uv_array,transpose = TRUE, flipx = TRUE)
    rayimage::plot_image(uv_array)
    return(invisible(uv_array))
  }

  imagelist$r[is.infinite(imagelist$depth)] = bg_color[1]
  imagelist$g[is.infinite(imagelist$depth)] = bg_color[2]
  imagelist$b[is.infinite(imagelist$depth)] = bg_color[3]

  retmat = array(0,dim=c(dim(imagelist$r)[2:1],3))
  retmat[,,1] = rayimage::render_reorient(imagelist$r,transpose = TRUE, flipx = TRUE)
  retmat[,,2] = rayimage::render_reorient(imagelist$g,transpose = TRUE, flipx = TRUE)
  retmat[,,3] = rayimage::render_reorient(imagelist$b,transpose = TRUE, flipx = TRUE)
  if(bloom) {
    retmat = rayimage::render_convolution(retmat, min_value = 1)
  }

  retmat[retmat > 1] = 1
  if(is.na(filename)) {
    rayimage::plot_image(retmat)
  } else {
    save_png(retmat, filename = filename)
  }
  return(invisible(retmat))
}
