#include <Rcpp.h>
using namespace Rcpp;

#define TINYOBJLOADER_IMPLEMENTATION
#define TINYOBJLOADER_USE_MAPBOX_EARCUT
#include "tinyobj/tiny_obj_loader.h"

template <typename T>
inline void set_item_impl( List& target, int i, const T& obj, CharacterVector& names, traits::true_type ){
  target[i] = obj.object ;
  names[i] = obj.name ;
}

template <typename T>
inline void set_item_impl( List& target, int i, const T& obj, CharacterVector&, traits::false_type ){
  target[i] = obj ;
}

template <typename T>
inline void set_item( List& target, int i, const T& obj, CharacterVector& names){
  set_item_impl( target, i, obj, names, typename traits::is_named<T>::type() ) ;
}

// [[Rcpp::export]]
List load_obj(std::string inputfile, std::string basedir) {
  tinyobj::ObjReader reader;
  tinyobj::ObjReaderConfig reader_config;
  reader_config.mtl_search_path = basedir; // Path to material files
  reader_config.triangulate = true;

  if (!reader.ParseFromFile(inputfile, reader_config)) {
    if (!reader.Error().empty()) {
      Rcpp::Rcout << "TinyObjReader: " << reader.Error();
    }
    stop("Stopping...");
  }

  if (!reader.Warning().empty()) {
    Rcpp::Rcout << "TinyObjReader: " << reader.Warning();
  }

  auto& attrib = reader.GetAttrib();
  auto& shapes = reader.GetShapes();
  auto& materials = reader.GetMaterials();
  
  
  if((attrib.vertices.size() % 3) !=0) {
    throw std::runtime_error("Number of vertices is not a multiple of 3");
  }
  
  List shape_list;
  List material_list;
  for (size_t s = 0; s < shapes.size(); s++) {
    tinyobj::mesh_t m = shapes[s].mesh;
    
    const size_t n_faces  = m.material_ids.size();
    const size_t nv_face = m.indices.size() / n_faces;
    if(n_faces == 0 || nv_face == 0) {
      continue;
    }
      
    List single_shape = List::create(
       Named("indices") = R_NilValue,
       Named("tex_indices") = R_NilValue,
       Named("norm_indices") = R_NilValue,
       Named("material_ids") = R_NilValue,
       Named("has_vertex_tex") = R_NilValue,
       Named("has_vertex_normals") = R_NilValue
    );
    std::vector<float> verts;
    std::vector<float> norms;
    std::vector<float> texs;
    std::vector<int> mats;
    
    for (size_t f = 0; f < m.num_face_vertices.size(); f++) {
      mats.push_back(m.material_ids[f]);
    }
    std::vector<int> inds;
    for(unsigned int j = 0; j < m.indices.size(); j++) {
      inds.push_back(m.indices[j].vertex_index);
    }
    std::vector<int> tex_inds;
    for(unsigned int j = 0; j < m.indices.size(); j++) {
      tex_inds.push_back(m.indices[j].texcoord_index);
    }
    std::vector<int> norm_inds;
    for(unsigned int j = 0; j < m.indices.size(); j++) {
      norm_inds.push_back(m.indices[j].normal_index);
    }
    
    single_shape["indices"]            = Rcpp::transpose(IntegerMatrix(nv_face, inds.size()/nv_face,      inds.begin()      ));
    single_shape["tex_indices"]        = Rcpp::transpose(IntegerMatrix(nv_face, tex_inds.size()/nv_face,  tex_inds.begin()  ));
    single_shape["norm_indices"]       = Rcpp::transpose(IntegerMatrix(nv_face, norm_inds.size()/nv_face, norm_inds.begin() ));
    single_shape["material_ids"]       = Rcpp::transpose(IntegerMatrix(1L, mats.size(), mats.begin()));
    single_shape["has_vertex_tex"]     = LogicalVector(inds.size()/nv_face,inds.size() == tex_inds.size());
    single_shape["has_vertex_normals"] = LogicalVector(inds.size()/nv_face,inds.size() == norm_inds.size());

    shape_list.push_back(single_shape);
  }
  //This needs to be updated when more defaults are added to each texture
  const int num_items = 30;
  for(unsigned int i=0; i < materials.size(); i++) {
    tinyobj::material_t m = materials[i];
    int culltype = m.dissolve < 1.0 ? 3 : 1; //no culling if at all transparent
    List out(num_items);
    CharacterVector names(num_items) ;
    int item = 0;
    set_item( out, item++ , _["ambient"]  =  NumericVector::create(m.ambient[0], m.ambient[1], m.ambient[2]), names) ;
    set_item( out, item++ , _["diffuse"]  = NumericVector::create(m.diffuse[0], m.diffuse[1], m.diffuse[2]), names) ;
    set_item( out, item++ , _["specular"]  = NumericVector::create(m.specular[0], m.specular[1], m.specular[2]), names) ;
    set_item( out, item++ , _["transmittance"]  = NumericVector::create(m.transmittance[0], m.transmittance[1], m.transmittance[2]), names) ;
    set_item( out, item++ , _["emission"]  = NumericVector::create(m.emission[0], m.emission[1], m.emission[2]), names) ;
    set_item( out, item++ , _["shininess"]  = m.shininess, names) ;
    set_item( out, item++ , _["ior"]  = m.ior, names) ;
    set_item( out, item++ , _["dissolve"]  = m.dissolve, names) ;
    set_item( out, item++ , _["illum"]  = m.illum, names) ;
    set_item( out, item++ , _["ambient_texname"]    = m.ambient_texname ,  names) ;
    set_item( out, item++ , _["diffuse_texname"]    = m.diffuse_texname ,  names) ;
    set_item( out, item++ , _["emissive_texname"]   = m.emissive_texname,  names) ;
    set_item( out, item++ , _["specular_texname"]   = m.specular_texname,  names) ;
    set_item( out, item++ , _["normal_texname"]     = m.normal_texname  ,  names) ;
    set_item( out, item++ , _["bump_texname"]     = m.bump_texname  ,  names) ;
    set_item( out, item++ , _["diffuse_intensity"]  = 1.0  , names) ;
    set_item( out, item++ , _["emission_intensity"] = 0.0, names) ;
    set_item( out, item++ , _["specular_intensity"] = 0.0, names) ;
    set_item( out, item++ , _["ambient_intensity"] = 0.0, names) ;
    set_item( out, item++ , _["bump_intensity"] = m.bump_texopt.bump_multiplier, names) ;
    set_item( out, item++ , _["culling"] = culltype, names) ;
    set_item( out, item++ , _["type"] = "diffuse", names) ;
    set_item( out, item++ , _["translucent"] = true, names) ;
    set_item( out, item++ , _["toon_levels"] = 5, names) ;
    set_item( out, item++ , _["toon_outline_width"] = 0.05, names) ;
    set_item( out, item++ , _["toon_outline_color"] = NumericVector::create(0,0,0), names) ;
    set_item( out, item++ , _["reflection_intensity"] = 1.0, names) ;
    set_item( out, item++ , _["reflection_sharpness"] = 1.0, names) ;
    set_item( out, item++ , _["two_sided"] = false, names) ;
    set_item( out, item++ , _["sigma"] = 0.0, names) ;
    
    if(item != num_items) {
      throw std::runtime_error("Number of items is not equal to specified material length.");
    }
    out.names() = names ;
    material_list.push_back(out);
  }
  List return_val = List::create(
    Named("shapes") = R_NilValue,
    Named("vertices") = R_NilValue,
    Named("texcoords") = R_NilValue,
    Named("normals") = R_NilValue,
    Named("materials") = R_NilValue
  );
  return_val["shapes"]    = shape_list;
  return_val["vertices"]  = List::create(Rcpp::transpose(NumericMatrix(3L, attrib.vertices.size()/3L, attrib.vertices.begin())));
  return_val["texcoords"] = List::create(Rcpp::transpose(NumericMatrix(2L, attrib.texcoords.size()/2L, attrib.texcoords.begin())));
  return_val["normals"]   = List::create(Rcpp::transpose(NumericMatrix(3L, attrib.normals.size()/3L, attrib.normals.begin())));
  return_val["materials"] = List::create(material_list);
  
  return return_val;
}