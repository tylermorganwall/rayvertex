#' Prepare a reusable scene snapshot
#'
#' Validate and merge geometry, deduplicate materials, and decode material
#' textures once for repeated calls to [rasterize_scene()]. Ordinary scene lists
#' remain supported and are processed afresh on every call.
#'
#' @param scene The scene to snapshot, or the replacement scene when updating.
#' @param prepared A handle returned by `prepare_scene()`.
#' @return A session-local `rayvertex_prepared_scene` handle.
#' @details
#' The handle owns a snapshot: later changes to the original list or its material
#' texture files do not affect it. `update_prepared_scene()` creates a new handle
#' from a replacement scene; existing handles retain their original snapshot.
#' Rebuild after changing geometry, materials, or material texture contents.
#'
#' Camera, lights, dimensions, FSAA, effects, environment maps, and
#' `vertex_transform` remain arguments to `rasterize_scene()`. They are evaluated
#' on each render. Vertex callbacks run on the R thread with the same call
#' semantics as ordinary rendering. Shadow maps and environment variants are
#' rebuilt per frame, so camera-dependent shadow fitting cannot become stale.
#'
#' Native handles cannot be saved and restored with serialization or carried to
#' another R session or process. Save the original scene and prepare it again.
#' Garbage collection releases owned geometry and decoded textures when the last
#' handle and any in-progress render no longer reference them.
#' @export
#' @examples
#' scene = cube_mesh()
#' prepared = prepare_scene(scene)
#' rasterize_scene(prepared, width = 64, height = 64, fsaa = 1, plot = FALSE)
#' updated = update_prepared_scene(prepared, sphere_mesh())
prepare_scene = function(scene) {
  if (inherits(scene, "rayvertex_prepared_scene")) {
    stop("Supply the original scene list to prepare_scene().")
  }
  validate_mesh(scene)
  snapshot = remove_duplicate_materials(merge_scene(
    scene,
    flatten_materials = TRUE
  ))
  for (i in seq_along(snapshot$materials)) {
    for (field in c(
      "diffuse_texname",
      "ambient_texname",
      "normal_texname",
      "specular_texname",
      "emissive_texname"
    )) {
      path = snapshot$materials[[i]][[field]]
      if (!is.null(path) && nzchar(path)) {
        snapshot$materials[[i]][[field]] = normalizePath(
          path.expand(path),
          winslash = "/",
          mustWork = FALSE
        )
      }
    }
  }
  for (name in c("cornell", "cornell_light")) {
    attr(snapshot, name) = attr(scene, name)
  }
  result = prepare_scene_rcpp(snapshot)
  class(result) = "rayvertex_prepared_scene"
  result
}

#' @rdname prepare_scene
#' @export
update_prepared_scene = function(prepared, scene) {
  prepared_scene_info(prepared)
  prepare_scene(scene)
}

#' @param x A prepared scene handle.
#' @param ... Additional arguments, unused.
#' @rdname prepare_scene
#' @export
print.rayvertex_prepared_scene = function(x, ...) {
  info = prepared_scene_info(x)
  cat(sprintf(
    "<rayvertex prepared scene> %s vertices, %s triangles, %s decoded textures (%.2f MiB)\n",
    info$vertices,
    info$triangles,
    info$texture_decodes,
    info$texture_payload_bytes / 1024^2
  ))
  invisible(x)
}
