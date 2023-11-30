#define TINYPLY_IMPLEMENTATION

#include "Rcpp.h"
#include "miniply.h"

using namespace Rcpp;

inline char separator_ply() {
#if defined _WIN32 || defined __CYGWIN__
  return '\\';
#else
  return '/';
#endif
}

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


enum class Topology {
  Soup,   // Every 3 indices specify a triangle.
  Strip,  // Triangle strip, triangle i uses indices i, i-1 and i-2
  Fan,    // Triangle fan, triangle i uses indices, i, i-1 and 0.
};


struct TriMesh {
  // Per-vertex data
  float* pos          = nullptr; // has 3*numVerts elements.
  float* normal       = nullptr; // if non-null, has 3 * numVerts elements.
  float* uv           = nullptr; // if non-null, has 2 * numVerts elements.
  uint32_t numVerts   = 0;
  
  // Per-index data
  int* indices        = nullptr; // has numIndices elements.
  uint32_t numIndices = 0; // number of indices = 3 times the number of faces.
  
  Topology topology  = Topology::Soup; // How to interpret the indices.
  bool hasTerminator = false;          // Only applies when topology != Soup.
  int terminator     = -1;             // Value indicating the end of a strip or fan. Only applies when topology != Soup.
  
  ~TriMesh() {
    delete[] pos;
    delete[] normal;
    delete[] uv;
    delete[] indices;
  }
  
  bool all_indices_valid() const {
    bool checkTerminator = topology != Topology::Soup && hasTerminator && (terminator < 0 || terminator >= int(numVerts));
    for (uint32_t i = 0; i < numIndices; i++) {
      if (checkTerminator && indices[i] == terminator) {
        continue;
      }
      if (indices[i] < 0 || uint32_t(indices[i]) >= numVerts) {
        return false;
      }
    }
    return true;
  }
};


static TriMesh* parse_file_with_miniply(const char* filename, bool assumeTriangles) {
  miniply::PLYReader reader(filename);
  if (!reader.valid()) {
    Rcpp::Rcout << "Not valid reader \n";
    return nullptr;
  }
  
  uint32_t indexes[3];
  bool gotVerts = false, gotFaces = false;
  
  TriMesh* trimesh = new TriMesh();
  while (reader.has_element() && (!gotVerts || !gotFaces)) {
    if (reader.element_is(miniply::kPLYVertexElement) && reader.load_element() && reader.find_pos(indexes)) {
      trimesh->numVerts = reader.num_rows();
      trimesh->pos = new float[trimesh->numVerts * 3];
      reader.extract_properties(indexes, 3, miniply::PLYPropertyType::Float, trimesh->pos);
      if (reader.find_texcoord(indexes)) {
        trimesh->uv = new float[trimesh->numVerts * 2];
        reader.extract_properties(indexes, 2, miniply::PLYPropertyType::Float, trimesh->uv);
      }
      gotVerts = true;
    } else if (reader.element_is(miniply::kPLYFaceElement) && reader.load_element() && reader.find_indices(indexes)) {
      uint32_t propIdx = 1; 
      bool polys = reader.requires_triangulation(propIdx);
      if (polys && !gotVerts) {
        Rcpp::Rcout << "Error: need vertex positions to triangulate faces.\n";
        break;
      }
      if (polys) {
        trimesh->numIndices = reader.num_triangles(indexes[0]) * 3;
        trimesh->indices = new int[trimesh->numIndices];
        reader.extract_triangles(indexes[0], trimesh->pos, trimesh->numVerts, miniply::PLYPropertyType::Int, trimesh->indices);
      } else {
        trimesh->numIndices = reader.num_rows() * 3;
        trimesh->indices = new int[trimesh->numIndices];
        reader.extract_list_property(indexes[0], miniply::PLYPropertyType::Int, trimesh->indices);
      }
      gotFaces = true;
    }
    if (gotVerts && gotFaces) {
      break;
    }
    reader.next_element();
  }
  if (!gotVerts || !gotFaces) {
    std::string vert1 = gotVerts ? "" : "vertices ";
    std::string face1 = gotFaces ? "" : "faces";
    Rcpp::Rcout << "Failed to load: " << vert1 << face1 << "\n";
    delete trimesh;
    return nullptr;
  }
  
  return trimesh;
}

// [[Rcpp::export]]
List load_ply(std::string inputfile, std::string basedir) {
  TriMesh* tri = parse_file_with_miniply(inputfile.c_str(), false);
  
  if(tri == nullptr) {
    std::string err = inputfile;
    throw std::runtime_error("No mesh loaded: " + err);
  }
  bool has_normals = false;
  if(tri->normal != nullptr) {
    has_normals = true;
  }
  
  bool has_uv = false;
  if(tri->uv != nullptr) {
    has_uv = true;
  }
  unsigned int numIndices = tri->numIndices;
  unsigned int numVerts = tri->numVerts;
  unsigned int nv_face = 3;
  
  List shape_list(1);
  List material_list(1);
  List single_shape;
  std::vector<float> verts;
  std::vector<float> norms;
  std::vector<float> texs;
  
  
  for(unsigned int j = 0; j < 3*numVerts; j++) {
    verts.push_back(tri->pos[j]);
  }
  
  if(has_uv) {
    for(unsigned int j = 0; j < 2*numVerts; j++) {
      texs.push_back(tri->uv[j]);
    }
  }
  
  if(has_normals) {
    for(unsigned int j = 0; j < 3*numVerts; j++) {
      norms.push_back(tri->normal[j]);
    }
  }
  
  std::vector<int> inds;
  std::vector<int> tex_inds;
  std::vector<int> norm_inds;
  
  for(unsigned int j = 0; j < numIndices; j++) {
    inds.push_back(tri->indices[j]);
  }
  
  if(has_uv) {
    for(unsigned int j = 0; j < numIndices; j++) {
      tex_inds.push_back(tri->indices[j]);
    }
  }
  if(has_normals) {
    for(unsigned int j = 0; j < numIndices; j++) {
      norm_inds.push_back(tri->indices[j]);
    }
  }
  
  single_shape["indices"]            = Rcpp::transpose(IntegerMatrix(nv_face, inds.size()/nv_face,      inds.begin()      ));
  single_shape["tex_indices"]        = Rcpp::transpose(IntegerMatrix(nv_face, tex_inds.size()/nv_face,  tex_inds.begin()  ));
  single_shape["norm_indices"]       = Rcpp::transpose(IntegerMatrix(nv_face, norm_inds.size()/nv_face, norm_inds.begin() ));
  single_shape["material_ids"]       = Rcpp::transpose(NumericMatrix(1L, inds.size()/nv_face));
  single_shape["has_vertex_tex"]     = LogicalVector(inds.size()/nv_face,inds.size() == tex_inds.size());
  single_shape["has_vertex_normals"] = LogicalVector(inds.size()/nv_face,inds.size() == norm_inds.size());
  shape_list[0]                = single_shape;
  
  const int num_items = 29;
  
  List out(num_items);
  CharacterVector names(num_items) ;
  int item = 0;
  
  set_item( out, item++, _["ambient"]  =  NumericVector::create(0,0,0), names) ;
  set_item( out, item++, _["diffuse"]  = NumericVector::create(1,1,1), names) ;
  set_item( out, item++, _["specular"]  = NumericVector::create(0,0,0), names) ;
  set_item( out, item++, _["transmittance"]  = NumericVector::create(0,0,0), names) ;
  set_item( out, item++, _["emission"]  = NumericVector::create(0,0,0), names) ;
  set_item( out, item++, _["shininess"]  = 1.0, names) ;
  set_item( out, item++, _["ior"]  = 1.0, names) ;
  set_item( out, item++, _["dissolve"]  = 1.0, names) ;
  set_item( out, item++, _["illum"]  = 1, names) ;
  set_item( out, item++, _["ambient_texname"]    = "" ,  names) ;
  set_item( out, item++, _["diffuse_texname"]    = "" ,  names) ;
  set_item( out, item++, _["emissive_texname"]   = "",  names) ;
  set_item( out, item++, _["specular_texname"]   = "",  names) ;
  set_item( out, item++, _["normal_texname"]     = ""  ,  names) ;
  set_item( out, item++, _["bump_texname"]    = "" ,  names) ;
  set_item( out, item++, _["diffuse_intensity"]  = 1.0  , names) ;
  set_item( out, item++, _["bump_intensity"]  = 1.0  , names) ;
  set_item( out, item++, _["emission_intensity"] = 1.0, names) ;
  set_item( out, item++, _["specular_intensity"] = 1.0, names) ;
  set_item( out, item++, _["ambient_intensity"] = 1.0, names) ;
  set_item( out, item++, _["culling"] = 1, names) ;
  set_item( out, item++, _["type"] = "diffuse", names) ;
  set_item( out, item++, _["translucent"] = true, names) ;
  set_item( out, item++, _["toon_levels"] = 5, names) ;
  set_item( out, item++, _["toon_outline_width"] = 0.05, names) ;
  set_item( out, item++, _["toon_outline_color"] = NumericVector::create(0,0,0), names) ;
  set_item( out, item++, _["reflection_intensity"] = 1.0, names) ;
  set_item( out, item++, _["reflection_sharpness"] = 1.0, names) ;
  set_item( out, item++, _["two_sided"] = false, names) ;
  
  if(item != num_items) {
    throw std::runtime_error("Number of items is not equal to specified material length.");
  }
  
  out.names() = names ;
  material_list[0] = out;
  List return_val;
  return_val["shapes"]    = shape_list;
  return_val["materials"] = List::create(material_list);
  return_val["vertices"]  = List::create(Rcpp::transpose(NumericMatrix(3L, verts.size()/3L, verts.begin())));
  return_val["texcoords"] = List::create(Rcpp::transpose(NumericMatrix(2L, texs.size()/2L, texs.begin())));
  return_val["normals"]   = List::create(Rcpp::transpose(NumericMatrix(3L, norms.size()/3L, norms.begin())));
  delete tri;
  return return_val;
};


