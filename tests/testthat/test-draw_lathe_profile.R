local_lathe_drawing = function(clicks, .env = parent.frame()) {
  skip_if_not(
    "local_mocked_bindings" %in% getNamespaceExports("testthat"),
    "testthat::local_mocked_bindings() is required"
  )
  withr::local_pdf(file = tempfile(fileext = ".pdf"), .local_envir = .env)
  local_mocked_bindings(
    dev.interactive = function(...) TRUE,
    .package = "grDevices",
    .env = .env
  )
  if (is.matrix(clicks)) {
    clicks = lapply(seq_len(nrow(clicks)), function(i) clicks[i, ])
  }
  undo = NULL
  draw_text = graphics::text
  local_mocked_bindings(
    text = function(x, y, labels, ...) {
      if (identical(labels, "Undo")) {
        undo <<- c(x, y)
      }
      draw_text(x, y, labels, ...)
    },
    .package = "graphics",
    .env = .env
  )
  index = 0L
  local_mocked_bindings(
    locator = function(n) {
      expect_equal(n, 1L)
      index <<- index + 1L
      if (index <= length(clicks)) {
        point = clicks[[index]]
        if (identical(point, "undo")) {
          expect_length(undo, 2L)
          point = undo
        }
        list(x = point[1L], y = point[2L])
      } else {
        NULL
      }
    },
    .package = "graphics",
    .env = .env
  )
}

test_that("clicks preview a closing edge from the second point and return a lathe polygon", {
  clicks = rbind(c(.25, .1), c(.8, .1), c(.8, .9), c(.25, .9))
  local_lathe_drawing(clicks)
  closing_edges = list()
  local_mocked_bindings(
    segments = function(x0, y0, x1, y1, ...) {
      closing_edges[[length(closing_edges) + 1L]] <<- rbind(
        c(x0, y0),
        c(x1, y1)
      )
    },
    .package = "graphics"
  )
  profile = draw_lathe_profile()
  expect_equal(length(closing_edges), 3L)
  for (i in 2:4) {
    expect_equal(unname(closing_edges[[i - 1L]]), clicks[c(i, 1L), ])
  }
  expect_equal(unname(profile), rbind(clicks, clicks[1L, ]))
  expect_equal(colnames(profile), c("radius", "height"))
  mesh = lathe_mesh(profile, segments = 8)
  expect_s3_class(validate_mesh(mesh), "ray_mesh")
  expect_equal(nrow(mesh$shapes[[1]]$indices), 64L)
})

test_that("Undo removes the last vertex, redraws closure, and allows drawing to continue", {
  local_lathe_drawing(list(
    c(.25, .1),
    c(.8, .1),
    c(.6, .6),
    "undo",
    c(.8, .9),
    c(.25, .9)
  ))
  closing_edges = list()
  local_mocked_bindings(
    segments = function(x0, y0, x1, y1, ...) {
      closing_edges[[length(closing_edges) + 1L]] <<- rbind(
        c(x0, y0),
        c(x1, y1)
      )
    },
    .package = "graphics"
  )
  profile = draw_lathe_profile()
  expect_equal(
    unname(profile),
    rbind(c(.25, .1), c(.8, .1), c(.8, .9), c(.25, .9), c(.25, .1))
  )
  expect_length(closing_edges, 5L)
  expect_equal(unname(closing_edges[[2L]]), rbind(c(.6, .6), c(.25, .1)))
  expect_equal(unname(closing_edges[[3L]]), rbind(c(.8, .1), c(.25, .1)))
  expect_s3_class(validate_mesh(lathe_mesh(profile, segments = 8)), "ray_mesh")
})

test_that("Undo can clear the drawing and is harmless when empty", {
  local_lathe_drawing(list(
    "undo",
    c(.25, .1),
    c(.8, .1),
    "undo",
    "undo",
    "undo"
  ))
  expect_null(draw_lathe_profile())
})

test_that("drawing snaps axis endpoints and ignores duplicate and out-of-bounds clicks", {
  clicks = rbind(
    c(.01, 0),
    c(.015, 0),
    c(-.2, .5),
    c(1.2, .5),
    c(.5, 1.2),
    c(1, 0),
    c(1, 1),
    c(.01, 1)
  )
  local_lathe_drawing(clicks)
  profile = NULL
  expect_message(profile <- draw_lathe_profile(), "drawing limits")
  expect_equal(unname(profile), rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1)))
  mesh = lathe_mesh(profile, segments = 8)
  expect_s3_class(validate_mesh(mesh), "ray_mesh")
  expect_equal(nrow(mesh$vertices[[1]]), 18L)
})

test_that("axis snapping can be disabled", {
  clicks = rbind(c(.01, 0), c(1, 0), c(1, 1), c(.01, 1))
  local_lathe_drawing(clicks)
  expect_equal(
    unname(draw_lathe_profile(snap = 0)),
    rbind(clicks, clicks[1L, ])
  )
})

test_that("cancelling an empty drawing returns NULL", {
  local_lathe_drawing(matrix(numeric(), ncol = 2L))
  expect_null(draw_lathe_profile())
})

test_that("drawing validates bounds and requires an interactive device", {
  for (limits in list(c(1, 0), c(0, 0), c(0, Inf), c(NA, 1), 1, "x", NULL)) {
    expect_error(draw_lathe_profile(xlim = limits), "`xlim`")
    expect_error(draw_lathe_profile(ylim = limits), "`ylim`")
  }
  expect_error(draw_lathe_profile(xlim = c(-1, 1)), "nonnegative")
  for (snap in list(-1, 1, Inf, NA, NULL, "x", c(0, 1))) {
    expect_error(draw_lathe_profile(snap = snap), "`snap`")
  }
  skip_if_not("local_mocked_bindings" %in% getNamespaceExports("testthat"))
  local_mocked_bindings(
    dev.interactive = function(...) FALSE,
    .package = "grDevices"
  )
  expect_error(draw_lathe_profile(), "interactive graphics device")
})

test_that("axis edges are omitted with outward orientation regardless of starting point", {
  rectangle = rbind(c(0, 0), c(1, 0), c(1, 2), c(0, 2))
  for (start in 1:4) {
    points = rectangle[c(start:4, seq_len(start - 1L)), ]
    for (order in list(1:4, 4:1)) {
      profile = lathe_profile_polygon(points[order, ])
      expect_equal(unname(profile), rectangle)
    }
  }
  # A polygon touching the axis at only one vertex starts and ends there.
  triangle = rbind(c(1, 0), c(1, 2), c(0, 1))
  profile = lathe_profile_polygon(triangle)
  expect_equal(unname(profile), triangle[c(3, 1, 2, 3), ])
  expect_s3_class(validate_mesh(lathe_mesh(profile, segments = 8)), "ray_mesh")
})

test_that("invalid polygons fail before producing unusable lathe profiles", {
  expect_error(lathe_profile_polygon(rbind(c(1, 0), c(1, 1))), "three")
  expect_error(
    lathe_profile_polygon(rbind(c(1, 0), c(1, 1), c(1, 2))),
    "nonzero area|retrace"
  )
  expect_error(
    lathe_profile_polygon(rbind(c(1, 0), c(3, 2), c(1, 2), c(2, 0))),
    "self-intersect"
  )
  expect_error(
    lathe_profile_polygon(rbind(c(0, 0), c(1, 0), c(0, 2), c(.2, 1))),
    "single vertex or along a single edge"
  )
})
