
#' Define the ray_shape class
#'
#' @param ... Objects to be included in the ray_shape.
#' @return A new ray_shape object.
#' @keywords internal
ray_shape = function(...) {
  vctrs::new_vctr(list(...),
                  class = c("ray_shape","list"))
}

#' Define the ray_shape_list class
#'
#' @param ... Objects to be included in the ray_shape_list.
#' @return A new ray_shape_list object.
#' @keywords internal
ray_shape_list = function(...) {
  vctrs::new_vctr(...,
                  class = c("ray_shape_list","list"))
}



#' Abbreviate the ptype of ray_shape
#'
#' @param x A ray_shape object.
#' @param ... Additional arguments (unused).
#' @return A character vector with the abbreviation "ray_shp".
#' @keywords internal
vec_ptype_abbr.ray_shape = function(x, ...) {
  "ray_shp"
}


#' Abbreviate the ptype of ray_shape_list
#'
#' @param x A ray_shape_list object.
#' @param ... Additional arguments (unused).
#' @return A character vector with the abbreviation "ray_shp".
#' @keywords internal
vec_ptype_abbr.ray_shape_list = function(x, ...) {
  "ray_shp"
}

#' Format ray_shape for pillar
#'
#' @param x A ray_shape object.
#' @return A formatted character vector.
#' @keywords internal
format_pillar_shp = function(x) {
  format_shp = function(x) {
    n_tris = nrow(x$indices)
    has_tex = if (all(x$has_vertex_tex)) {
      cli::col_green("UV")
    } else if (any(x$has_vertex_tex)) {
      cli::col_yellow("UV")
    } else {
      pillar::style_subtle("UV")
    }
    has_norm = if (all(x$has_vertex_normals)) {
      cli::col_green("N")
    } else if (any(x$has_vertex_normals)) {
      cli::col_yellow("N")
    } else {
      pillar::style_subtle("N")
    }
    n_mat = length(unique(x$material_ids))
    
    output_shp = (sprintf("T:%s%s%s%s%s%sM:%s",
                          cli::col_cyan(n_tris), 
                          pillar::style_subtle("|"),
                          has_tex, pillar::style_subtle("|"),
                          has_norm, pillar::style_subtle("|"),
                          cli::col_cyan(n_mat)))
    sprintf("%s%s%s",
            pillar::style_subtle("<"), 
            paste0(output_shp, collapse=""),
            pillar::style_subtle(">"))
  }
  vapply(x, format_shp, character(1))
}

#' Pillar shaft for ray_shape_list
#'
#' @param x A ray_shape_list object.
#' @param ... Additional arguments (unused).
#' @return A pillar shaft object.
#' @keywords internal
pillar_shaft.ray_shape_list = function(x, ...) {
  pillar::new_pillar_shaft_simple(format_pillar_shp(x),
                                  align = "right",
                                  width = 20)
}

#' Pillar shaft for ray_shape
#'
#' @param x A ray_shape object.
#' @param ... Additional arguments (unused).
#' @return A pillar shaft object.
#' @keywords internal
pillar_shaft.ray_shape = function(x, ...) {
  pillar::new_pillar_shaft_simple(format_pillar_shp(x),
                                  align = "right",
                                  width = 20)
}

#' Print method for ray_shape
#'
#' @param x A ray_shape object.
#' @param ... Additional arguments (unused).
#' @keywords internal
print.ray_shape = function(x, ...) {
  print_shape = function(x, ...) {
    # Print indices
    if(length(x$indices) > 0) {
      cat(sprintf("$ %s           : int  [%dx3] %s ...\n", 
                  pillar::style_subtle("indices"),
                  nrow(x$indices), 
                  cli::col_cyan(paste(t(utils::head(x$indices, 5)), collapse = " "))))
    } else {
      cat(sprintf("$ %s           : %s\n", "indices", pillar::style_subtle("int(0)")))
    }
    
    # Print tex_indices
    if(length(x$tex_indices) > 0) {
      cat(sprintf("$ %s       : int  [%dx3] %s ...\n", 
                  pillar::style_subtle("tex_indices"),
                  nrow(x$tex_indices), 
                  cli::col_cyan(paste(utils::head(x$tex_indices, 5), collapse = " "))))
    } else {
      cat(sprintf("$ %s       : %s\n", "tex_indices", pillar::style_subtle("int(0)")))
    }
    
    # Print norm_indices
    if(length(x$norm_indices) > 0) {
      cat(sprintf("$ %s      : int  [%dx3] %s ...\n", 
                  pillar::style_subtle("norm_indices"),
                  nrow(x$norm_indices), 
                  cli::col_cyan(paste(utils::head(x$norm_indices, 5), collapse = " "))))
    } else {
      cat(sprintf("$ %s      : %s\n","norm_indices",pillar::style_subtle("int(0)")))
    }
    
    # Print material_ids
    cat(sprintf("$ %s      : int  [%d] %s ...\n", 
                pillar::style_subtle("material_ids"),
                length(x$material_ids), 
                cli::col_cyan(paste(utils::head(x$material_ids, 5), collapse = " "))))
    
    # Print has_vertex_tex
    cat(sprintf("$ %s    : logi [%d] %s ...\n", 
                pillar::style_subtle("has_vertex_tex"),
                length(x$has_vertex_tex), 
                cli::col_cyan(paste(utils::head(x$has_vertex_tex, 5), collapse = " "))))
    
    # Print has_vertex_normals
    cat(sprintf("$ %s: logi [%d] %s ...\n", 
                pillar::style_subtle("has_vertex_normals"),
                length(x$has_vertex_normals), 
                cli::col_cyan(paste(utils::head(x$has_vertex_normals, 5), collapse = " "))))
  } 
  for(i in seq_along(x)) {
    cat(sprintf(pillar::style_subtle("[[%i]] ray_shape"),i), sep = "\n")
    print_shape(x[[1]])
  }
}

#' Define the ray_vertex_data class
#'
#' @param data A matrix with 2 or 3 columns representing vertex data.
#' @return A new ray_vertex_data object.
#' @keywords internal
ray_vertex_data = function(data = NA) {
  stopifnot(is.matrix(data))
  stopifnot(ncol(data) == 3 || ncol(data) == 2)
  
  vctrs::new_vctr(list(data), class = "ray_vertex_data")
}

#' Print method for ray_vertex_data
#'
#' @param x A ray_vertex_data object.
#' @param ... Additional arguments (unused).
#' @keywords internal
print.ray_vertex_data = function(x, ...) {
  
  print_data = function(x, ...) {
    ncols = ncol(x)
    nrows = nrow(x)
    cat(sprintf("%s%s%s%s%s%s",
                pillar::style_subtle("<"), 
                cli::col_cyan(nrows),
                pillar::style_subtle("x"),
                cli::col_cyan(ncols),
                cli::col_cyan(" matrix"),
                pillar::style_subtle(">\n")))
    print(utils::head(x,5))
    if(nrows > 5) {
      cat(pillar::style_subtle(sprintf("[%s,] %i more rows \n", cli::symbol$ellipsis, nrows-5)))
    }
  }
  for(i in seq_along(x)) {
    cat(sprintf(pillar::style_subtle("[[%i]]"),i), sep = "\n")
    print_data(x[[i]])
  }
}

#' Convert RGB to ANSI Color
#'
#' @param color Length=3 numeric vector.
#'
#' @return ANSI color code as a string.
#' @keywords internal
convert_rgb_to_ansi = function(color) {
  # Get the number of supported ANSI colors
  num_colors = cli::num_ansi_colors()
  
  # Convert RGB from [0, 1] to [0, 255]
  r = as.integer(color[1] * 255)
  g = as.integer(color[2] * 255)
  b = as.integer(color[3] * 255)
  
  # Define a function to convert RGB to ANSI 256 colors
  rgb_to_ansi256_bg = function(r, g, b) {
    if (r == g && g == b) {
      if (r < 8) return(16)
      if (r > 248) return(231)
      return(as.integer((r - 8) / 247 * 24) + 232)
    }
    return(16 + 36 * as.integer(r / 255 * 5) + 6 * as.integer(g / 255 * 5) + as.integer(b / 255 * 5))
  }
  
  # Define a function to convert RGB to ANSI 16 colors
  rgb_to_ansi16_bg = function(r, g, b) {
    if (r < 128 && g < 128 && b < 128) return(40)  # Black
    if (r > 128 && g < 128 && b < 128) return(41)  # Red
    if (r < 128 && g > 128 && b < 128) return(42)  # Green
    if (r > 128 && g > 128 && b < 128) return(43)  # Yellow
    if (r < 128 && g < 128 && b > 128) return(44)  # Blue
    if (r > 128 && g < 128 && b > 128) return(45)  # Magenta
    if (r < 128 && g > 128 && b > 128) return(46)  # Cyan
    if (r > 192 && g > 192 && b > 192) return(47)  # White
    return(47)  # Default to white for other cases
  }
  
  # Select conversion function based on the number of supported colors
  if (num_colors >= 16777216) {
    # True color (24-bit) support
    ansi_code = paste0("\033[48;2;", r, ";", g, ";", b, "m")
  } else if (num_colors >= 256) {
    # 256 colors support
    ansi_code = paste0("\033[48;5;", rgb_to_ansi256_bg(r, g, b), "m")
  } else if (num_colors >= 16) {
    # 16 colors support
    ansi_code = paste0("\033[", rgb_to_ansi16_bg(r, g, b), "m")
  } else {
    # Fallback to black if only 8 colors are supported
    ansi_code = "\033[40m"
  }
  
  return(ansi_code)
}


#' Print out the color
#'
#' @return none
#' @keywords internal
cat_color = function(color, var_name, default = NA, intensity = 1, spacer = "") {
  print_col = FALSE
  if(any(is.na(default))) {
    print_col = TRUE
  } else {
    print_col = any(color != default)
  }
  if(print_col) {
    colbg = sprintf("%s %s", convert_rgb_to_ansi(convert_color(color)), "\033[0m")
    intensity_str = ""
    if(intensity != 1) {
      intensity_str = sprintf("%s %0.1f", pillar::style_subtle("| intensity:"), intensity)
    }
    cat(sprintf("%s%s %s %s %s %s\n", 
                spacer,
                pillar::style_subtle(cli::symbol$bullet),
                pillar::style_subtle(sprintf("%s:",var_name)),  
                convert_color(color, as_hex = TRUE),colbg, intensity_str))
  }
}

#' @keywords internal
print.rayvertex_material = function(x, spacer = "", id = 0, ...) {
  if(spacer != "") {
    cat(pillar::style_subtle(sprintf("%s Material ID: %i ", 
                                     cli::symbol$bullet, id)),sep="\n")
  } else {
    cat(pillar::style_subtle(sprintf("%s rayvertex_material", 
                                     cli::symbol$bullet)),sep="\n")
  }
  print_single_mat = function(x, i = 0) {
    # cat(pillar::style_subtle(sprintf("[[%i]]", i)),sep="\n")
    bullet = pillar::style_subtle(cli::symbol$bullet)
    cat(sprintf("%s%s %s %s", spacer, bullet, pillar::style_subtle("type:"), x$type ), sep = "\n")
    cat_color(x$diffuse, "diffuse", NA, x$diffuse_intensity, spacer=spacer)          
    cat_color(x$ambient, "ambient", c(0, 0, 0), x$ambient_intensity, spacer=spacer)         
    cat_color(x$specular, "specular", c(1, 1, 1), x$specular_intensity, spacer=spacer)              
    cat_color(x$transmittance, "transmittance", c(0, 0, 0), spacer=spacer)         
    cat_color(x$emission, "emission", c(0, 0, 0), x$emission_intensity, spacer=spacer)
    if(x$shininess != 50) {
      cat(sprintf("%s%s %s %i", spacer, bullet, pillar::style_subtle("shininess:"), x$shininess), sep = "\n")
    }
    if(x$ior != 1) {
      cat(sprintf("%s%s %s %0.2f", spacer, bullet, pillar::style_subtle("ior:"),x$ior ), sep = "\n")
    }
    if(x$dissolve != 1) {
      cat(sprintf("%s%s %s %0.2f", spacer, bullet, pillar::style_subtle("dissolve:"), x$dissolve), sep = "\n")
    }
    if(x$illum != 1) {
      cat(sprintf("%s%s %s %i", spacer, bullet, pillar::style_subtle("illum:"), x$illum), sep = "\n")
    }
  
    if(x$diffuse_texname != "") {
      if(file.exists(x$diffuse_texname)) {
        fileexists_str = sprintf("%s %s %s", 
                                 pillar::style_subtle("|"),
                                 cli::col_green(cli::symbol$tick),"File exists!")
      } else {
        fileexists_str = sprintf("%s %s %s", 
                                 pillar::style_subtle("|"),
                                 cli::col_red(cli::symbol$cross),
                                 "File not found")
      }
      cat(sprintf("%s%s %s %s %s", 
                  spacer, bullet,
                  pillar::style_subtle("diffuse_texname:"), 
                  x$diffuse_texname,
                  fileexists_str), sep = "\n")
    }
    if(x$ambient_texname != "") {
      if(file.exists(x$ambient_texname)) {
        fileexists_str = sprintf("%s %s %s", 
                                 pillar::style_subtle("|"),
                                 cli::col_green(cli::symbol$tick),"File exists!")
      } else {
        fileexists_str = sprintf("%s %s %s", 
                                 pillar::style_subtle("|"),
                                 cli::col_red(cli::symbol$cross),
                                 "File not found")
      }
      cat(sprintf("%s%s %s %s %s", spacer, bullet,
                  pillar::style_subtle("ambient_texname:"), x$ambient_texname,
                  fileexists_str), sep = "\n")
    }
    if(x$specular_texname != "") {
      if(file.exists(x$specular_texname)) {
        fileexists_str = sprintf("%s %s %s", 
                                 pillar::style_subtle("|"),
                                 cli::col_green(cli::symbol$tick),"File exists!")
      } else {
        fileexists_str = sprintf("%s %s %s", 
                                 pillar::style_subtle("|"),
                                 cli::col_red(cli::symbol$cross),
                                 "File not found")
      }
      cat(sprintf("%s%s %s %s %s", spacer, bullet,
                  pillar::style_subtle("specular_texname:"), x$specular_texname,
                  fileexists_str), sep = "\n")
    }
    if(x$emissive_texname != "") {
      if(file.exists(x$emissive_texname)) {
        fileexists_str = sprintf("%s %s %s", 
                                 pillar::style_subtle("|"),
                                 cli::col_green(cli::symbol$tick),"File exists!")
      } else {
        fileexists_str = sprintf("%s %s %s", 
                                 pillar::style_subtle("|"),
                                 cli::col_red(cli::symbol$cross),
                                 "File not found")
      }
      cat(sprintf("%s%s %s %s %s", spacer, bullet,
                  pillar::style_subtle("emissive_texname:"), x$emissive_texname,
                  fileexists_str), sep = "\n")
    }
    if(x$bump_texname != "") {
      if(file.exists(x$bump_texname)) {
        fileexists_str = sprintf("%s %s %s", 
                                 pillar::style_subtle("|"),
                                 cli::col_green(cli::symbol$tick),"File exists!")
      } else {
        fileexists_str = sprintf("%s %s %s", 
                                 pillar::style_subtle("|"),
                                 cli::col_red(cli::symbol$cross),
                                 "File not found")
      }
      bump_int_str = ""
      if(x$bump_intensity != 1) {
        bump_int_str = sprintf("%s intensity: %0.1f", pillar::style_subtle("|"), 
                               x$bump_intensity)
      }
      cat(sprintf("%s%s %s %s %s %s", spacer, bullet,
                  pillar::style_subtle("bump_texname:"), x$bump_texname, 
                  fileexists_str,
                  bump_int_str), sep = "\n")
    }
    if(x$normal_texname != "") {
      if(file.exists(x$normal_texname)) {
        fileexists_str = sprintf("%s %s %s", 
                                 pillar::style_subtle("|"),
                                 cli::col_green(cli::symbol$tick),"File exists!")
      } else {
        fileexists_str = sprintf("%s %s %s", 
                                 pillar::style_subtle("|"),
                                 cli::col_red(cli::symbol$cross),
                                 "File not found")
      }
      cat(sprintf("%s%s %s %s %s", spacer, bullet,
                  pillar::style_subtle("normal_texname:"), x$normal_texname,
                  fileexists_str), sep = "\n")
    }
  
    if(x$culling != 1) {
      cat(sprintf("%s%s %s %i", spacer, bullet,
                  pillar::style_subtle("culling:"), x$culling), sep = "\n")
    }
    if(x$dissolve < 1 || any(x$transmittance > 0)) {
      cat(sprintf("%s%s %s %s", spacer, bullet,
                  pillar::style_subtle("translucent:"), ifelse(x$translucent,
                                                                        "TRUE","FALSE")), sep = "\n")
    }
    if(x$two_sided) {
      cat(sprintf("%s%s %s %s", spacer, bullet,
                  pillar::style_subtle("two_sided:"), "TRUE"), sep = "\n")
    }
    if(x$type == "toon") {
      cat(sprintf("%s%s %s %s", pillar::style_subtle("toon_levels:"), 
                  x$toon_levels), sep = "\n")
      cat(sprintf("%s%s %s %0.2f", pillar::style_subtle("toon_outline_width:"), 
                  x$toon_outline_width), sep = "\n")
      cat_color(x$toon_outline_color, "toon_outline_color", NA)         
    }
    if(x$reflection_intensity > 0) {
      cat(sprintf("%s%s %s %0.2f %s %s %0.2f", 
                  spacer, bullet,
                  pillar::style_subtle("reflection_intensity:"),
                  x$reflection_intensity,
                  pillar::style_subtle("|"),
                  pillar::style_subtle("reflection_sharpness:"),
                  x$reflection_sharpness), 
          sep = "\n")
    }
  }
  print_single_mat(x)
}

#' @keywords internal
vec_ptype_abbr.ray_vertex_data = function(x, ...) {
  "ray_dat"
}

#' @keywords internal
format_pillar_data = function(x) {
  format_data = function(x1) {
    if(is.null(x1[1]) || any(is.na(x1))) {
      return(sprintf("%s%s%s",
                     pillar::style_subtle("<"), 
                     pillar::style_subtle("none"),
                     pillar::style_subtle(">")))
    }
    n_rows = nrow(x1)
    dimval = ncol(x1)
    if(n_rows != 0) {
      sprintf("%s%s%s%s%s",
              pillar::style_subtle("<"), 
              cli::col_cyan(n_rows),
              pillar::style_subtle("x"),
              cli::col_cyan(dimval),
              pillar::style_subtle(">"))
    } else {
      sprintf("%s%s%s",
              pillar::style_subtle("<"), 
              pillar::style_subtle("none"),
              pillar::style_subtle(">"))
    }
    
  }
  vapply(x, format_data, character(1))
}

#' @keywords internal
pillar_shaft.ray_vertex_data = function(x, ...) {
  pillar::new_pillar_shaft_simple(format_pillar_data(x),
                                  align = "right",
                                  width = 10)
}

# Define the ray_vertices class
rayvertex_material = function(...) {
  vctrs::new_vctr(..., class = "rayvertex_material")
}

#' @keywords internal 
vec_ptype_abbr.rayvertex_material = function(x, ...) {
  "ray_mat"
}

# Define the ray_vertices class
rayvertex_material_list = function(...) {
  vctrs::new_vctr(..., class = "rayvertex_material_list")
}

#' @keywords internal
vec_ptype_abbr.rayvertex_material = function(x, ...) {
  "ray_mat"
}

#' @keywords internal
vec_ptype_abbr.rayvertex_material_list = function(x, ...) {
  "ray_mat"
}

#' @keywords internal
format_pillar_matlist = function(x) {
  format_matlist = function(x1) {
    # if(is.null(x1[[1]]) || is.na(x1[[1]])) {
    # if(is.null(x1) || is.na(x1)) {
    #   return(sprintf("%s%s%s",
    #                  pillar::style_subtle("<"),
    #                  cli::col_red("none"),
    #                  pillar::style_subtle(">")))
    # }
    n_mats = length(x1)
    if(n_mats == 1) {
      return(sprintf("%s%s%s",
                     pillar::style_subtle("<"),
                     cli::col_green((x1[[1]][["type"]])),
                     pillar::style_subtle(">")))
    }
    if(n_mats > 0) {
      return(sprintf("%s%s%s",
                     pillar::style_subtle("<"),
                     cli::col_cyan(sprintf("%sx",as.character(n_mats))),
                     pillar::style_subtle(">")))
    } else {
      return(sprintf("%s%s%s",
                     pillar::style_subtle("<"),
                     cli::col_red("none"),
                     pillar::style_subtle(">")))
    }
  }
  vapply(x, format_matlist, character(1))
}

#' @keywords internal
print.rayvertex_material_list = function(x, ...) {
  format_pillar_matlist(x)
}

#' @keywords internal
pillar_shaft.rayvertex_material_list = function(x, ...) {
  pillar::new_pillar_shaft_simple(format_pillar_matlist(x),
                                  align = "right",
                                  width = 8)
}

#' @keywords internal
pillar_shaft.rayvertex_material = function(x, ...) {
  pillar::new_pillar_shaft_simple(format_pillar_matlist(x),
                                  align = "right",
                                  width = 8)
}


#'@title Constructor for ray_mesh
#'
#'@return ray
#'@keywords internal
ray_mesh = function(...) {
  structure(..., class = c("ray_mesh","tbl", "list"))
}

#' @keywords internal
tbl_sum.print_raymesh_df <- function(x, ...) {
}

#' @keywords internal
print.ray_mesh = function(x, ...) {
  # Count total objects and lights
  total_meshes = length(x$shapes)
  total_unique_materials = length(unique(attr(x, "material_hashes")))
  x$materials = rayvertex_material_list(x$material)
  # x$shapes = ray_shape_list(x$shapes)
  
  # Count each type of object
  bbox_x = get_mesh_bbox(x)
  # Calculate bounding box
  bbxmin = c(bbox_x[1,,drop=TRUE])
  bbxmax = c(bbox_x[2,,drop=TRUE])
  
  # Construct the print output
  line1 = sprintf("Summary - %s: %s | %s: %s",
                  cli::col_blue("Meshes"),
                  cli::col_cyan(as.character(total_meshes)),
                  cli::col_blue("Unique Materials"),
                  cli::col_cyan(as.character(total_unique_materials)))
  min_bbox = sprintf("c(%0.2f, %0.2f, %0.2f)",bbxmin[1],bbxmin[2],bbxmin[3])
  max_bbox = sprintf("c(%0.2f, %0.2f, %0.2f)",bbxmax[1],bbxmax[2],bbxmax[3])
  bbox_text = sprintf("XYZ Bounds - %s: %s | %s: %s", cli::col_blue("Min"), 
                      cli::col_cyan(min_bbox), 
                      cli::col_blue("Max"), 
                      cli::col_cyan(max_bbox))
  # 
  cli_output = function() {
    cli::cli_rule(left = "Scene Description")
    bullets = cli::format_bullets_raw(c(
      "*" = line1,
      "i" = bbox_text
    ))
    cat(paste0(bullets,"\n"),sep="")
  }
  # 
  cli_output()
  if(length(find.package("tibble",quiet=TRUE)) > 0) {
    withr::local_options(list(pillar.advice = TRUE, pillar.print_max = 3, pillar.width = 50),
                         code = {
                           print_tbl = tibble::as_tibble(x)
                           class(print_tbl) = c("print_raymesh_df",class(print_tbl))
                           print(print_tbl, ...)
                           
                         }
    )
  }
}