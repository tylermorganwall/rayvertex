#' Assemble the renderer's planar channels into an oriented linear image
#' @param imagelist Native render buffers.
#' @keywords internal
raster_output_image = function(imagelist) {
  if (nzchar(Sys.getenv("RAYVERTEX_REFERENCE_OUTPUT"))) {
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
  image = assemble_raster_output(
    imagelist$r,
    imagelist$g,
    imagelist$b,
    imagelist$a
  )
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
