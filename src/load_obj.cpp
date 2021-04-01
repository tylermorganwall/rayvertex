#include <Rcpp.h>
using namespace Rcpp;

#define TINYOBJLOADER_IMPLEMENTATION
#include "tiny_obj_loader.h"

// [[Rcpp::export]]
List load_obj(std::string inputfile, std::string basedir) {
  tinyobj::ObjReader reader;
  tinyobj::ObjReaderConfig reader_config;
  reader_config.mtl_search_path = "./"; // Path to material files
  reader_config.triangulate = true;
  
  if (!reader.ParseFromFile(inputfile, reader_config)) {
    if (!reader.Error().empty()) {
      std::cerr << "TinyObjReader: " << reader.Error();
    }
    exit(1);
  }

  if (!reader.Warning().empty()) {
    std::cout << "TinyObjReader: " << reader.Warning();
  }

  auto& attrib = reader.GetAttrib();
  auto& shapes = reader.GetShapes();
  auto& materials = reader.GetMaterials();
  
  
  if((attrib.vertices.size() % 3) !=0) {
    throw std::runtime_error("Number of vertices is not a multiple of 3");
  }
  List shape_list(shapes.size());
  List material_list;
  for (size_t s = 0; s < shapes.size(); s++) {
    tinyobj::mesh_t m = shapes[s].mesh;
    
    const size_t n_faces  = m.material_ids.size();
    const size_t nv_face = m.indices.size() / n_faces;
    List single_shape;
    size_t index_offset = 0;
    std::vector<float> verts;
    std::vector<float> norms;
    std::vector<float> texs;
    std::vector<int> mats;
    
    for (size_t f = 0; f < m.num_face_vertices.size(); f++) {
      mats.push_back(m.material_ids[f]);
    }
    std::vector<int> inds;
    for(int j = 0; j < m.indices.size(); j++) {
      inds.push_back(m.indices[j].vertex_index);
    }
    std::vector<int> tex_inds;
    for(int j = 0; j < m.indices.size(); j++) {
      tex_inds.push_back(m.indices[j].texcoord_index);
    }
    std::vector<int> norm_inds;
    for(int j = 0; j < m.indices.size(); j++) {
      norm_inds.push_back(m.indices[j].normal_index);
    }
    
    single_shape["indices"]      = Rcpp::transpose(IntegerMatrix(nv_face, inds.size()/nv_face,      inds.begin()      ));
    single_shape["tex_indices"]  = Rcpp::transpose(IntegerMatrix(nv_face, tex_inds.size()/nv_face,  tex_inds.begin()  ));
    single_shape["norm_indices"] = Rcpp::transpose(IntegerMatrix(nv_face, norm_inds.size()/nv_face, norm_inds.begin() ));
    single_shape["material_ids"] = Rcpp::transpose(NumericMatrix(1L, mats.size(), mats.begin()));
    single_shape["name"]         = shapes[s].name;
    shape_list[s]                = single_shape;
  }
  for(unsigned int i=0; i < materials.size(); i++) {
    tinyobj::material_t m = materials[i];
    material_list[m.name] = List::create(Named("ambient", NumericVector::create(m.ambient[0], m.ambient[1], m.ambient[2])),
                                         Named("diffuse", NumericVector::create(m.diffuse[0], m.diffuse[1], m.diffuse[2])),
                                         Named("specular", NumericVector::create(m.specular[0], m.specular[1], m.specular[2])),
                                         Named("transmittance", NumericVector::create(m.transmittance[0], m.transmittance[1], m.transmittance[2])),
                                         Named("emission", NumericVector::create(m.emission[0], m.emission[1], m.emission[2])),
                                         Named("shininess", m.shininess),
                                         Named("ior", m.ior), 
                                         Named("dissolve", m.dissolve), 
                                         Named("illum", m.illum),
                                         Named("ambient_texname", m.ambient_texname), 
                                         Named("diffuse_texname", m.diffuse_texname),
                                         Named("emissive_texname", m.emissive_texname),
                                         Named("specular_texname", m.specular_texname), 
                                         Named("normal_texname", m.normal_texname));
  }
  List return_val;
  return_val["shapes"]    = shape_list;
  return_val["materials"] = material_list;
  return_val["vertices"]  = Rcpp::transpose(NumericMatrix(3L, attrib.vertices.size()/3L, attrib.vertices.begin()));
  return_val["texcoords"] = Rcpp::transpose(NumericMatrix(2L, attrib.texcoords.size()/2L, attrib.texcoords.begin()));
  return_val["normals"]   = Rcpp::transpose(NumericMatrix(3L, attrib.normals.size()/3L, attrib.normals.begin()));
  return return_val;
}