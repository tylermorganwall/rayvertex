test_that("material deduplication retains the legacy unnamed material list", {
  scene = list(
    materials = list(a = list(id = 1), b = list(id = 2)),
    shapes = list(list(material_ids = c(0L, 1L)))
  )
  attr(scene, "material_hashes") = c("same", "same")
  expect_identical(
    rayvertex:::remove_duplicate_materials(scene)$materials,
    list(list(id = 1))
  )
})
