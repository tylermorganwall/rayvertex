#' Draw a Lathe Profile
#'
#' Click on a base R plot to draw a radius/height polygon for [lathe_mesh()].
#' After the second click, the preview joins the newest point to the first
#' point, updating the closing segment after each additional click.
#'
#' @param xlim Default `c(0, 1)`. Increasing, finite radius limits for the
#' drawing area. Both limits must be nonnegative.
#' @param ylim Default `c(0, 1)`. Increasing, finite height limits for the
#' drawing area.
#' @param snap Default `0.02`. Distance from radius zero within which clicks
#' snap to the axis, expressed as a fraction of the width of `xlim`. Set to
#' zero to disable snapping. Applies only when `xlim` starts at zero.
#'
#' @details
#' Requires an interactive graphics device that supports [graphics::locator()].
#' Left-click to add vertices in boundary order. Click **Undo** above the plot
#' to remove the most recent vertex and update the preview. Undo can be used
#' repeatedly, including to clear the drawing; it has no effect when empty.
#' Finish using the device's
#' locator termination action: for example, Escape on Quartz, another mouse
#' button on X11, or Finish in RStudio. The plot uses equal X and Y scales.
#' Other clicks outside the supplied limits and consecutive duplicate clicks are
#' ignored. A dashed line marks the automatically supplied closing segment.
#'
#' At least three noncollinear vertices are needed to finish a polygon.
#' Self-intersections and retraced edges are rejected. Redundant collinear
#' vertices are removed, and the boundary is oriented counterclockwise to
#' produce outward-facing lathe surfaces.
#'
#' For a polygon away from the axis, the first vertex is repeated at the end
#' so [lathe_mesh()] includes the closing segment. When the polygon meets the
#' axis, its boundary must touch it at a single vertex or along a single edge.
#' Axis vertices become the profile endpoints; an edge along the axis is
#' omitted because it does not sweep out a surface. This keeps radius-zero
#' vertices out of the profile interior, as required by [lathe_mesh()].
#'
#' @return A numeric matrix with columns `radius` and `height`, ready to pass
#' as the `profile` argument of [lathe_mesh()]. Returns `NULL` if drawing is
#' finished with no points remaining.
#' @seealso [lathe_mesh()]
#' @export
#' @examplesIf interactive()
#' profile = draw_lathe_profile(xlim = c(0, 2), ylim = c(0, 4))
#' if (!is.null(profile)) {
#'   mesh = lathe_mesh(profile)
#'   rasterize_scene(mesh, lookfrom = c(6, 5, 8), lookat = c(0, 2, 0))
#' }
draw_lathe_profile = function(xlim = c(0, 1), ylim = c(0, 1), snap = 0.02) {
  for (name in c("xlim", "ylim")) {
    limits = get(name)
    if (
      !is.numeric(limits) ||
        length(limits) != 2L ||
        any(!is.finite(limits)) ||
        !is.finite(diff(limits)) ||
        diff(limits) <= 0
    ) {
      stop(sprintf("`%s` must contain two increasing finite limits.", name))
    }
  }
  if (xlim[1L] < 0) {
    stop("`xlim` must be nonnegative.")
  }
  if (
    !is.numeric(snap) ||
      length(snap) != 1L ||
      !is.finite(snap) ||
      snap < 0 ||
      snap >= 1
  ) {
    stop(
      "`snap` must be a finite fraction from zero up to, but not including, one."
    )
  }
  if (!grDevices::dev.interactive(orNone = TRUE)) {
    stop("Drawing a lathe profile requires an interactive graphics device.")
  }

  margins = graphics::par("mar")
  on.exit(graphics::par(mar = margins), add = TRUE)
  graphics::par(mar = pmax(margins, c(0, 0, 4.1, 0)))
  profile = matrix(numeric(), ncol = 2L)
  repeat {
    graphics::plot(
      NA_real_,
      NA_real_,
      type = "n",
      xlim = xlim,
      ylim = ylim,
      asp = 1,
      xaxs = "i",
      yaxs = "i",
      xlab = "Radius (x)",
      ylab = "Height (y)",
      sub = "Click to add points; Undo to remove; Esc / right-click / Finish to end"
    )
    graphics::title(main = "Draw a lathe profile", line = 3)
    graphics::grid()
    graphics::rect(xlim[1L], ylim[1L], xlim[2L], ylim[2L], border = "grey70")
    graphics::abline(v = 0, col = "grey50")
    count = nrow(profile)
    if (count >= 2L) {
      graphics::polygon(profile, col = "lightblue", border = NA)
      graphics::lines(profile, col = "steelblue4", lwd = 2)
      graphics::segments(
        profile[count, 1L],
        profile[count, 2L],
        profile[1L, 1L],
        profile[1L, 2L],
        col = "steelblue4",
        lwd = 2,
        lty = 2
      )
    }
    if (count) {
      graphics::points(profile, pch = 19, col = "steelblue4")
      graphics::points(profile[1L, , drop = FALSE], pch = 21, bg = "white")
    }
    # Keep the control outside the drawing area, sized to its label.
    usr = graphics::par("usr")
    undo_x = usr[2L] -
      c(graphics::strwidth("Undo") + 2 * graphics::strwidth("M"), 0)
    undo_y = usr[4L] + c(1, 3) * graphics::strheight("M")
    graphics::rect(
      undo_x[1L],
      undo_y[1L],
      undo_x[2L],
      undo_y[2L],
      col = if (count) "grey95" else "white",
      border = "grey60",
      xpd = NA
    )
    graphics::text(
      mean(undo_x),
      mean(undo_y),
      "Undo",
      col = if (count) "black" else "grey60",
      xpd = NA
    )
    point = graphics::locator(1L)
    if (is.null(point)) {
      break
    }
    point = c(point$x, point$y)
    if (
      all(is.finite(point)) &&
        point[1L] >= undo_x[1L] &&
        point[1L] <= undo_x[2L] &&
        point[2L] >= undo_y[1L] &&
        point[2L] <= undo_y[2L]
    ) {
      if (count) {
        profile = profile[-count, , drop = FALSE]
      }
      next
    }
    if (
      any(!is.finite(point)) ||
        point[1L] < xlim[1L] ||
        point[1L] > xlim[2L] ||
        point[2L] < ylim[1L] ||
        point[2L] > ylim[2L]
    ) {
      message("Click inside the drawing limits.")
      next
    }
    if (xlim[1L] == 0 && point[1L] <= snap * diff(xlim)) {
      point[1L] = 0
    }
    if (count && all(point == profile[count, ])) {
      next
    }
    profile = rbind(profile, point)
  }
  if (!nrow(profile)) {
    return(NULL)
  }
  lathe_profile_polygon(profile)
}

#' Convert a drawn polygon to a lathe profile
#' @keywords internal
#' @noRd
lathe_profile_polygon = function(profile) {
  profile = polygon_ring(profile)
  polygon_check_edges(list(profile))
  count = nrow(profile)
  if (polygon_ring_area(profile) < 0) {
    profile = profile[c(1L, count:2L), , drop = FALSE]
  }
  axis = which(profile[, 1L] == 0)
  if (length(axis)) {
    next_id = c(seq_len(count)[-1L], 1L)
    axis_edges = which(profile[, 1L] == 0 & profile[next_id, 1L] == 0)
    if (length(axis) > 2L || (length(axis) == 2L && length(axis_edges) != 1L)) {
      stop(
        "The polygon must meet the axis at a single vertex or along a single edge."
      )
    }
    start = if (length(axis_edges)) next_id[axis_edges] else axis
    profile = profile[c(start:count, seq_len(start - 1L)), , drop = FALSE]
  }
  if (length(axis) < 2L) {
    profile = rbind(profile, profile[1L, , drop = FALSE])
  }
  rownames(profile) = NULL
  colnames(profile) = c("radius", "height")
  profile
}
