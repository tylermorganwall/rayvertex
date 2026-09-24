#' Assemble the renderer's planar channels into an oriented linear image
#' @param imagelist Native render buffers.
#' @param background Default `NULL`. Optional background RGB values.
#' @param ambient Default `NULL`. Optional already exponentiated ambient factors.
#' @keywords internal
raster_output_image = function(imagelist, background = NULL, ambient = NULL) {
  if (nzchar(Sys.getenv("RAYVERTEX_REFERENCE_OUTPUT"))) {
    if (!is.null(ambient)) {
      imagelist$r = imagelist$r * ambient
      imagelist$g = imagelist$g * ambient
      imagelist$b = imagelist$b * ambient
    }
    if (!is.null(background)) {
      imagelist$r[imagelist$depth == 1] = background[1]
      imagelist$g[imagelist$depth == 1] = background[2]
      imagelist$b[imagelist$depth == 1] = background[3]
    }
    image = array(0, dim = c(dim(imagelist$r)[1:2], 4))
    image[,, 1] = imagelist$r
    image[,, 2] = imagelist$g
    image[,, 3] = imagelist$b
    image[,, 4] = imagelist$a
    return(rayimage::render_reorient(
      rayimage::ray_read_image(
        image,
        assume_colorspace = rayimage::CS_SRGB,
        assume_white = "D65",
        source_linear = FALSE
      ),
      transpose = TRUE,
      flipx = TRUE
    ))
  }
  image = if (is.null(background) && is.null(ambient)) {
    assemble_raster_output(imagelist$r, imagelist$g, imagelist$b, imagelist$a)
  } else {
    compose_raster_output(
      imagelist$r,
      imagelist$g,
      imagelist$b,
      imagelist$a,
      imagelist$depth,
      ambient,
      background
    )
  }
  rayimage::ray_read_image(
    image,
    assume_colorspace = rayimage::CS_SRGB,
    assume_white = "D65",
    source_linear = TRUE
  )
}

#' Clamp final renderer RGB samples while preserving alpha and metadata
#' @param image The renderer's RGBA rayimage.
#' @keywords internal
clamp_raster_image = function(image) {
  if (
    nzchar(Sys.getenv("RAYVERTEX_REFERENCE_OUTPUT")) ||
      any(dim(image)[1:2] <= 1L)
  ) {
    return(rayimage::render_clamp(image))
  }
  image = rayimage::ray_read_image(image, reset_camera_settings = TRUE)
  clamp_raster_output(image)
}
