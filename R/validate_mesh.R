#' Validate Mesh Data
#'
#' This function takes a mesh and validates it. The mesh should be a list with
#' "shapes", "materials", "vertices", "texcoords", "normals", and "material_hashes" entries.
#'
#' @section Shapes:
#' Each "shapes" entry should be a list with "mesh", "name", and "material" entries.
#' Each "mesh" entry should have "indices", "tex_indices", "norm_indices", "material_ids", "has_vertex_tex", and "has_vertex_normals".
#' The indices should not exceed the number of rows in their corresponding vertex/normal/texcoord data. There should be no NA/NaN values in the vertex/normal/texcoord data.
#'
#' @section Materials (for rayvertex package only):
#' Each "materials" entry is expected to be a list with several entries with specific required lengths, as listed below:
#'
#' |  Attribute               |  Length  |  Type       |
#' | :----------------------: | :------: | :---------: |
#' |  diffuse                 |  3       |  Numeric    |
#' |  ambient                 |  3       |  Numeric    |
#' |  specular                |  3       |  Numeric    |
#' |  transmittance           |  3       |  Numeric    |
#' |  emission                |  3       |  Numeric    |
#' |  shininess               |  1       |  Numeric    |
#' |  ior                     |  1       |  Numeric    |
#' |  dissolve                |  1       |  Numeric    |
#' |  illum                   |  1       |  Numeric    |
#' |  diffuse_texname         |  1       |  Character  |
#' |  normal_texname          |  1       |  Character  |
#' |  bump_texname            |  1       |  Character  |
#' |  specular_texname        |  1       |  Character  |
#' |  ambient_texname         |  1       |  Character  |
#' |  emissive_texname        |  1       |  Character  |
#' |  diffuse_intensity       |  1       |  Numeric    |
#' |  bump_intensity          |  1       |  Numeric    |
#' |  specular_intensity      |  1       |  Numeric    |
#' |  emission_intensity      |  1       |  Numeric    |
#' |  ambient_intensity       |  1       |  Numeric    |
#' |  culling                 |  1       |  Character  |
#' |  type                    |  1       |  Character  |
#' |  translucent             |  1       |  Logical    |
#' |  toon_levels             |  1       |  Numeric    |
#' |  toon_outline_width      |  1       |  Numeric    |
#' |  toon_outline_color      |  3       |  Numeric    |
#' |  reflection_intensity    |  1       |  Numeric    |
#' |  reflection_sharpness    |  1       |  Numeric    |
#' |  two_sided               |  1       |  Logical    |
#'
#' Note: This materials validation only applies to the rayvertex package. Other renderers might choose to use their own information in the material list.
#'
#' @param mesh List. A mesh is a list as described above.
#' @param validate_materials Default `TRUE`. Whether or not to validate "materials".
#' @return A mesh.
#' @export
#' @examples
#' # validate a mesh
#' mesh = validate_mesh(sphere_mesh())
validate_mesh = function(mesh, validate_materials = TRUE) {
  stopifnot(inherits(mesh, "ray_mesh"))
  stopifnot(all(
    names(mesh) %in%
      c("shapes", "materials", "vertices", "texcoords", "normals")
  ))
  stopifnot(length(attr(mesh, "material_hashes")) > 0)

  # Validate shapes
  for (i in seq_along(mesh$shapes)) {
    mesh_entry = mesh$shapes[[i]]
    stopifnot(is.list(mesh_entry))
    stopifnot(all(
      names(mesh_entry) %in%
        c(
          "indices",
          "tex_indices",
          "norm_indices",
          "material_ids",
          "has_vertex_tex",
          "has_vertex_normals"
        )
    ))
    stopifnot(is.numeric(mesh_entry$indices))
    stopifnot(is.numeric(mesh_entry$tex_indices))
    stopifnot(is.numeric(mesh_entry$norm_indices))
    stopifnot(is.numeric(mesh_entry$material_ids))
    stopifnot(is.logical(mesh_entry$has_vertex_tex))
    stopifnot(is.logical(mesh_entry$has_vertex_normals))

    # Validate vertex, normal, texcoord indices
    vertices = mesh$vertices[[i]]
    normals = mesh$normals[[i]]
    texcoords = mesh$texcoords[[i]]
    materials_mesh = mesh$materials[[i]]

    stopifnot(is.matrix(vertices))
    stopifnot(!any(is.na(vertices)), !any(is.nan(vertices)))

    if (any(mesh_entry$has_vertex_normals)) {
      stopifnot(is.matrix(normals))
      stopifnot(!any(is.na(normals)), !any(is.nan(normals)))
    }
    if (any(mesh_entry$has_vertex_tex)) {
      stopifnot(is.matrix(texcoords))
      stopifnot(!any(is.na(texcoords)), !any(is.nan(texcoords)))
    }

    max_vertex_index = nrow(vertices) - 1
    max_normal_index = nrow(normals) - 1
    max_texcoord_index = nrow(texcoords) - 1
    max_materials = length(materials_mesh) - 1

    stopifnot(all(mesh_entry$vertex_index <= max_vertex_index))
    stopifnot(all(mesh_entry$normal_index <= max_normal_index))
    stopifnot(all(mesh_entry$texcoord_index <= max_texcoord_index))
    stopifnot(all(mesh_entry$material_id <= max_materials))
  }

  # Validate materials
  if (validate_materials) {
    for (i in seq_along(mesh$materials)) {
      materials <- mesh$materials[[i]]
      for (j in seq_along(materials)) {
        material <- materials[[j]]
        stopifnot(is.list(material))
        stopifnot(all(
          names(material) %in%
            c(
              "diffuse",
              "ambient",
              "specular",
              "transmittance",
              "emission",
              "shininess",
              "ior",
              "dissolve",
              "illum",
              "diffuse_texname",
              "normal_texname",
              "specular_texname",
              "ambient_texname",
              "emissive_texname",
              "diffuse_intensity",
              "bump_intensity",
              "bump_texname",
              "specular_intensity",
              "emission_intensity",
              "ambient_intensity",
              "culling",
              "type",
              "translucent",
              "toon_levels",
              "toon_outline_width",
              "toon_outline_color",
              "reflection_intensity",
              "reflection_sharpness",
              "two_sided",
              "sigma"
            )
        ))

        # Validate material attributes types and lengths
        stopifnot(is.numeric(material$diffuse), length(material$diffuse) == 3)
        stopifnot(is.numeric(material$ambient), length(material$ambient) == 3)
        stopifnot(is.numeric(material$specular), length(material$specular) == 3)
        stopifnot(
          is.numeric(material$transmittance),
          length(material$transmittance) == 3
        )
        stopifnot(is.numeric(material$emission), length(material$emission) == 3)
        stopifnot(
          is.numeric(material$shininess),
          length(material$shininess) == 1
        )
        stopifnot(is.numeric(material$ior), length(material$ior) == 1)
        stopifnot(is.numeric(material$dissolve), length(material$dissolve) == 1)
        stopifnot(is.numeric(material$illum), length(material$illum) == 1)
        stopifnot(is.character(material$diffuse_texname))
        stopifnot(is.character(material$normal_texname))
        stopifnot(is.character(material$bump_texname))
        stopifnot(is.character(material$specular_texname))
        stopifnot(is.character(material$ambient_texname))
        stopifnot(is.character(material$emissive_texname))
        stopifnot(
          is.numeric(material$diffuse_intensity),
          length(material$diffuse_intensity) == 1
        )
        stopifnot(
          is.numeric(material$bump_intensity),
          length(material$bump_intensity) == 1
        )
        stopifnot(
          is.numeric(material$specular_intensity),
          length(material$specular_intensity) == 1
        )
        stopifnot(
          is.numeric(material$emission_intensity),
          length(material$emission_intensity) == 1
        )
        stopifnot(
          is.numeric(material$ambient_intensity),
          length(material$ambient_intensity) == 1
        )
        stopifnot(is.numeric(material$culling), length(material$culling) == 1)
        stopifnot(is.character(material$type))
        stopifnot(
          is.logical(material$translucent),
          length(material$translucent) == 1
        )
        stopifnot(
          is.numeric(material$toon_levels),
          length(material$toon_levels) == 1
        )
        stopifnot(
          is.numeric(material$toon_outline_width),
          length(material$toon_outline_width) == 1
        )
        stopifnot(
          is.numeric(material$toon_outline_color),
          length(material$toon_outline_color) == 3
        )
        stopifnot(
          is.numeric(material$reflection_intensity),
          length(material$reflection_intensity) == 1
        )
        stopifnot(
          is.numeric(material$reflection_sharpness),
          length(material$reflection_sharpness) == 1
        )
        stopifnot(
          is.logical(material$two_sided),
          length(material$two_sided) == 1
        )
        stopifnot(
          is.numeric(material$sigma),
          length(material$sigma) == 1,
          material$sigma >= 0 && material$sigma <= 90
        )
      }
    }
  }

  return(mesh)
}
