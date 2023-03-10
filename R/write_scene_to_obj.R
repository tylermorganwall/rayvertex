#' @title Write the scene to an OBJ file
#' 
#' @description Writes the current scene to a Wavefront OBJ file, with or without materials
#' 
#' @param scene A rayvertex scene.
#' @param filename The filename for the OBJ file.
#' @param materials Default `TRUE`. Whether to write an MTL file to specify the materials for the OBJ.
#' @param fileext Default `".obj"`. The file extension to add to the filename.
#' 
#' @return None
#' @export
#'
#' @examples
#' if(rayvertex:::run_documentation()) {
#' tmpfile = tempfile(fileext = ".obj")
#' write_scene_to_obj(generate_cornell_mesh(), tmpfile)
#' }
write_scene_to_obj = function(scene, filename, materials = TRUE, fileext = ".obj") {
  if(!inherits(scene, "ray_mesh")) {
    stop("`scene` must be of class `ray_mesh`")
  }
  filename = sprintf("%s%s",tools::file_path_sans_ext(filename),fileext)
  mtl_file = sprintf("%s.mtl",tools::file_path_sans_ext(filename))
  con = file(filename, open = "w")
  on.exit(close(con), add = TRUE)
  writeLines("# OBJ created with rayvertex",
             con = con)
  if(materials) {
    con_mtl = file(mtl_file, open = "w")
    on.exit(close(con_mtl), add = TRUE)
    write_materials(con_mtl, scene$materials)
    writeLines(sprintf("mtllib %s",basename(mtl_file)),
               con = con)
  }
  write_verts(con, scene$vertices)
  write_tex(con, scene$texcoor)
  write_norm(con, scene$normals)
  n_verts = unlist(lapply(scene$vertices,nrow))
  n_normals = unlist(lapply(scene$normals,nrow))
  n_texcoords  = unlist(lapply(scene$texcoords,nrow))
  #Get max of mats in each
  get_max_material = function(shape) {
    max(shape$material_ids, na.rm=TRUE) + 1
  }
  n_mats = unlist(lapply(scene$shapes, get_max_material))
  write_shapes(con,
               scene$shapes, 
               n_verts, 
               n_normals,
               n_texcoords,
               n_mats, materials = materials)
}

#Write functions
write_verts = function(con, verts) {
  n_verts = length(verts)
  for(i in seq_len(n_verts)) {
    writeLines(sprintf("v %f %f %f", 
                       verts[[i]][,1], 
                       verts[[i]][,2], 
                       verts[[i]][,3]),
               con = con)
  }
}

write_tex = function(con, tex) {
  n_tex = length(tex)
  for(i in seq_len(n_tex)) {
    if(nrow(tex[[i]] > 0)) {
      writeLines(sprintf("vt %f %f", 
                         tex[[i]][,1], 
                         tex[[i]][,2]),
                 con = con)
    }
  }
}

write_norm = function(con, norm) {
  n_norm = length(norm)
  for(i in seq_len(n_norm)) {
    if(nrow(norm[[i]] > 0)) {
      writeLines(sprintf("vn %f %f %f", 
                         norm[[i]][,1], 
                         norm[[i]][,2], 
                         norm[[i]][,3]),
                 con = con)
    }
  }
}

write_shapes = function(con,
                        shapes, 
                        n_verts, 
                        n_normals,
                        n_texcoords, 
                        n_mats,
                        materials = FALSE) {
  if(length(unique(c(length(shapes),
                     length(n_verts),
                     length(n_normals),
                     length(n_texcoords)))) != 1) {
    stop("All inputs must be same length")
  }
  vert_offset = cumsum(c(1,n_verts))
  norm_offset = cumsum(c(1,n_normals))
  tex_offset  = cumsum(c(1,n_texcoords))
  mat_offset  = cumsum(c(0,n_mats))
  prev_mat = 0
  for(i in seq_len(length(shapes))) {
    single_material = length(unique(shapes[[i]]$material_ids)) == 1
    single_tex      = length(unique(shapes[[i]]$has_vertex_tex)) == 1
    single_norm     = length(unique(shapes[[i]]$has_vertex_normals)) == 1
    single_mat      = length(unique(shapes[[i]]$material_ids)) == 1
    voff = vert_offset[i]
    noff = norm_offset[i]
    toff = tex_offset[i]
    prev_mat = 0
    if(single_material && single_tex && single_norm && (single_mat || !materials)) {
      has_tex = shapes[[i]]$has_vertex_tex[1]
      has_norm = shapes[[i]]$has_vertex_normals[1]
      if(materials) {
        new_mat = shapes[[i]]$material_ids[1] + mat_offset[i]
      } else {
        new_mat = prev_mat
      }
      if((i == 1 || prev_mat != new_mat) && materials ) {
        writeLines(sprintf("usemtl mat%i", new_mat + 1), con = con)
        prev_mat = new_mat
      }
      if(!has_tex && !has_norm) { 
        face_lines = paste("f",apply(shapes[[i]]$indices + voff,1, paste, collapse = " "))
      } else if (has_tex && has_norm) {
        firstvert  = sprintf("%i/%i/%i", 
                             shapes[[i]]$indices[,1] + voff,
                             shapes[[i]]$tex_indices[,1] + toff,
                             shapes[[i]]$norm_indices[,1] + noff)
        secondvert = sprintf("%i/%i/%i", 
                             shapes[[i]]$indices[,2] + voff,
                             shapes[[i]]$tex_indices[,2] + toff,
                             shapes[[i]]$norm_indices[,2] + noff)
        thirdvert  = sprintf("%i/%i/%i", 
                             shapes[[i]]$indices[,3] + voff,
                             shapes[[i]]$tex_indices[,3] + toff,
                             shapes[[i]]$norm_indices[,3] + noff)
        face_lines = paste("f",firstvert, secondvert, thirdvert, sep = " ")
      } else if (has_tex && !has_norm) {
        firstvert  = sprintf("%i/%i", 
                             shapes[[i]]$indices[,1] + voff,
                             shapes[[i]]$tex_indices[,1] + toff)
        secondvert = sprintf("%i/%i", 
                             shapes[[i]]$indices[,2] + voff,
                             shapes[[i]]$tex_indices[,2] + toff)
        thirdvert  = sprintf("%i/%i", 
                             shapes[[i]]$indices[,3] + voff,
                             shapes[[i]]$tex_indices[,3] + toff)
        face_lines = paste("f",firstvert, secondvert, thirdvert, sep = " ")
      } else if (!has_tex && has_norm) {
        firstvert  = sprintf("%i//%i", 
                             shapes[[i]]$indices[,1] + voff,
                             shapes[[i]]$norm_indices[,1] + noff)
        secondvert = sprintf("%i//%i", 
                             shapes[[i]]$indices[,2] + voff,
                             shapes[[i]]$norm_indices[,2] + noff)
        thirdvert  = sprintf("%i//%i", 
                             shapes[[i]]$indices[,3] + voff,
                             shapes[[i]]$norm_indices[,3] + noff)
        face_lines = paste("f",firstvert, secondvert, thirdvert, sep = " ")
      }
      writeLines(face_lines, con = con)
    } else {
      n_idx = nrow(shapes[[i]]$indices)
      output_list = vector(mode = "list")
      counter = 1
      for(j in seq_len(n_idx)) {
        has_tex = shapes[[i]]$has_vertex_tex[j]
        has_norm = shapes[[i]]$has_vertex_normals[j]
        new_mat = shapes[[i]]$material_ids[j] + mat_offset[i]
        if((i == 1 || prev_mat != new_mat) && materials) {
          output_list[[counter]] = sprintf("usemtl mat%i",new_mat + 1)
          prev_mat = new_mat 
          counter = counter + 1
        }
        if(!has_tex && !has_norm) { 
          output_list[[counter]] = paste("f",apply(shapes[[i]]$indices[j,] + voff,1, paste, collapse = " "))
        } else if (has_tex && has_norm) {
          output_list[[counter]] = paste("f",
                                         paste0(shapes[[i]]$indices[j,] + voff,"/",
                                                shapes[[i]]$tex_indices[j,] + toff, "/",
                                                shapes[[i]]$norm_indices[j,] + noff, collapse=" "))
        } else if (has_tex && !has_norm) {
          output_list[[counter]] = paste("f",
                                         paste0(shapes[[i]]$indices[j,] + voff,"/",
                                                shapes[[i]]$tex_indices[j,] + toff, collapse=" "))
        } else if (!has_tex && has_norm) {
          output_list[[counter]] = paste("f",
                                         paste0(shapes[[i]]$indices[j,] + voff,"//",
                                                shapes[[i]]$norm_indices[j,] + noff, collapse=" "))
        }
        counter = counter + 1
      }
      face_lines = unlist(output_list)
      writeLines(face_lines, con = con)
    }
  }
}

paste_digit = function(num, digits=4) {
  sprintf(paste0("%0.",digits,"f",collapse=""),num)
}

write_materials = function(con, materials) {
  n_mats = length(materials)
  for(i in seq_len(n_mats)) {
    tmp_mat = materials[[i]][[1]]
    writeLines(sprintf("newmtl mat%i",i),con)
    if(tmp_mat$diffuse_intensity > 0) {
      writeLines(sprintf("Kd %s",paste(paste_digit(tmp_mat$diffuse * tmp_mat$diffuse_intensity),collapse = " ")),con)
    }
    if(tmp_mat$ambient_intensity > 0) {
      writeLines(sprintf("Ka %s",paste(paste_digit(tmp_mat$ambient * tmp_mat$ambient_intensity),collapse = " ")),con)
    }
    if(tmp_mat$specular_intensity > 0) {
      writeLines(sprintf("Ks %s",paste(paste_digit(tmp_mat$specular * tmp_mat$specular_intensity),collapse = " ")),con)
    }
    writeLines(sprintf("Tf %s",paste(paste_digit(tmp_mat$transmittance),collapse = " ")),con)
    if(any(tmp_mat$emission > 0)) {
      writeLines(sprintf("Ke %s",paste(paste_digit(tmp_mat$emission * tmp_mat$emission_intensity),collapse = " ")),con)
    }
    writeLines(sprintf("Ns %f",tmp_mat$shininess),con)
    writeLines(sprintf("illum %i",tmp_mat$illum),con)
    if(tmp_mat$dissolve < 1) {
      writeLines(sprintf("d %f",tmp_mat$dissolve),con)
    }
    if(tmp_mat$reflection_sharpness > 0) {
      writeLines(sprintf("sharpness %i",tmp_mat$reflection_sharpness),con)
    }
    if(tmp_mat$ior != 1) {
      writeLines(sprintf("Ni %f",tmp_mat$ior),con)
    }
    if(nchar(tmp_mat$diffuse_texname) > 0) {
      writeLines(sprintf("map_Kd %s",tmp_mat$diffuse_texname),con)
    }
    if(nchar(tmp_mat$ambient_texname) > 0) {
      writeLines(sprintf("map_Ka %s",tmp_mat$ambient_texname),con)
    }
    if(nchar(tmp_mat$emissive_texname) > 0) {
      writeLines(sprintf("map_Ke %s",tmp_mat$emissive_texname),con)
    }
    if(nchar(tmp_mat$specular_texname) > 0) {
      writeLines(sprintf("map_Ks %s",tmp_mat$specular_texname),con)
    }
    if(nchar(tmp_mat$normal_texname) > 0) {
      writeLines(sprintf("norm %s",tmp_mat$normal_texname),con)
    }
  }
}