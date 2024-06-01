#include "loopsubdiv.h"
#include <set>
#include <map>
#include "glm.hpp"
#include "Rcpp.h"
#include <queue>

using namespace Rcpp;

struct SDFace;
struct SDVertex;

// LoopSubdiv Macros
#define NEXT(i) (((i) + 1) % 3)
#define PREV(i) (((i) + 2) % 3)

// LoopSubdiv Local Structures
struct SDVertex {
  // SDVertex Constructor
  SDVertex(const glm::vec3 &p = glm::vec3(0, 0, 0),
           const glm::vec2 &uv = glm::vec2(0, 0)) : 
    p(p), uv(uv) {}
  
  // SDVertex Methods
  int valence();
  void oneRing(glm::vec3 *p);
  void oneRing(glm::vec2 *p);
  glm::vec3 p;
  glm::vec2 uv;
  SDFace *startFace = nullptr;
  SDVertex *child = nullptr;
  bool regular = false, boundary = false;
  bool initialized = false; //This marks when the vertex has a startFace
};

struct SDFace {
  // SDFace Constructor
  SDFace() {
    for (int i = 0; i < 3; ++i) {
      v[i] = nullptr;
      f[i] = nullptr;
    }
    MatID = 0;
    vertices_initialized = false;
    faces_initialized = false;
    for (int i = 0; i < 4; ++i) children[i] = nullptr;
  }
  
  // SDFace Methods
  int vnum(SDVertex *vert) const {
    for (int i = 0; i < 3; ++i) {
      if(!vert) {
        Rcpp::stop("Vert not found");
      }
      if(!v[i]) {
        Rcpp::stop("Vert not initialized");
      }
      if (v[i] == vert) {
        return i; 
      }
    }
    Rcpp::stop( "Basic logic error in SDFace::vnum()");
    return -1;
  }
  SDFace *nextFace(SDVertex *vert) { return f[vnum(vert)]; }
  SDFace *prevFace(SDVertex *vert) { return f[PREV(vnum(vert))]; }
  SDVertex *nextVert(SDVertex *vert) { return v[NEXT(vnum(vert))]; }
  SDVertex *prevVert(SDVertex *vert) { return v[PREV(vnum(vert))]; }
  SDVertex *otherVert(SDVertex *v0, SDVertex *v1) {
    for (int i = 0; i < 3; ++i) {
      if (v[i] != v0 && v[i] != v1) {
        return v[i];
      }
    }
    Rcpp::stop( "Basic logic error in SDVertex::otherVert()");
    return nullptr;
  }
  SDVertex *v[3];
  SDFace *f[3];
  SDFace *children[4];
  int MatID;
  bool vertices_initialized; //This marks when the vertices are all initialized
  bool faces_initialized; //This marks when the vertices are all initialized
};

struct SDEdge {
  // SDEdge Constructor
  SDEdge(SDVertex *v0 = nullptr, SDVertex *v1 = nullptr) {
    v[0] = std::min(v0, v1);
    v[1] = std::max(v0, v1);
    f[0] = f[1] = nullptr;
    f0edgeNum = -1;
  }
  
  // SDEdge Comparison Function
  bool operator<(const SDEdge &e2) const {
    if (v[0] == e2.v[0]) return v[1] < e2.v[1];
    return v[0] < e2.v[0];
  }
  SDVertex *v[2];
  SDFace *f[2];
  int f0edgeNum;
};

// LoopSubdiv Local Declarations
template <typename T>
static T weightOneRing(SDVertex *vert, T vert_data, float beta);

template <typename T>
static T weightBoundary(SDVertex *vert, T vert_data, float beta);

// LoopSubdiv Inline Functions
inline int SDVertex::valence() {
  SDFace *f = startFace;
  if (!boundary) {
    // Compute valence of interior vertex
    int nf = 1;
    while ((f = f->nextFace(this)) != startFace) {
      Rcpp::checkUserInterrupt();
      ++nf;
    }
    return nf;
  } else {
    // Compute valence of boundary vertex
    int nf = 1;
    
    while ((f = f->nextFace(this)) != nullptr) {
      Rcpp::checkUserInterrupt();
      ++nf;
    }
    f = startFace;
    while ((f = f->prevFace(this)) != nullptr) {
      Rcpp::checkUserInterrupt();
      ++nf;
    }
    return nf + 1;
  }
}

inline float beta(int valence) {
  if (valence == 3) {
    return 3.f / 16.f;
  } else {
    return 3.f / (8.f * valence);
  }
}

inline float loopGamma(int valence) {
  return 1.f / (valence + 3.f / (8.f * beta(valence)));
}

// LoopSubdiv Function Definitions
// [[Rcpp::export]]
List LoopSubdivide(List mesh,
                   int shape_i,
                   const int nLevels,
                   bool verbose) {
  if(nLevels <= 1) {
    return(List());
  }
  List shapes_list = as<List>(mesh["shapes"])(shape_i);
  NumericMatrix vertex_matrix = as<NumericMatrix>(as<List>(mesh["vertices"])(shape_i));
  NumericMatrix texcoord_matrix = as<NumericMatrix>(as<List>(mesh["texcoords"])(shape_i));
  
  IntegerVector face_material_ids = as<IntegerVector>(shapes_list["material_ids"]);
  
  IntegerVector vertexIndices = as<IntegerVector>(Rcpp::transpose(as<IntegerMatrix>(shapes_list["indices"])));
  IntegerVector texIndices = as<IntegerVector>(Rcpp::transpose(as<IntegerMatrix>(shapes_list["tex_indices"])));
  
  int nIndices = vertexIndices.length();
  
  std::vector<std::unique_ptr<SDVertex> > vertex_storage;
  std::vector<std::unique_ptr<SDFace> > face_storage;

  std::vector<glm::vec3> p(vertex_matrix.nrow());
  for(size_t i = 0; i < vertex_matrix.nrow(); i++) {
    p[i] = glm::vec3(vertex_matrix(i,0),vertex_matrix(i,1),vertex_matrix(i,2));
  }
  std::vector<glm::vec2> uv(texcoord_matrix.nrow());
  for(size_t i = 0; i < texcoord_matrix.nrow(); i++) {
    uv[i] = glm::vec2(texcoord_matrix(i,0),texcoord_matrix(i,1));
  }
  bool has_uv = texcoord_matrix.nrow() > 0;
  int nVertices =  vertex_matrix.nrow();

  
  std::vector<SDVertex *> vertices;
  std::vector<SDFace *> faces;
  // Allocate _LoopSubdiv_ vertices and faces
  std::vector<glm::vec2> TexcoordsExpanded(nVertices);
  std::vector<int> vertexToTex(nVertices);
  if(has_uv) {
    for(int i = 0; i < vertexIndices.size(); ++i) {
      if(texIndices[i] != -1) {
        TexcoordsExpanded[vertexIndices[i]] = uv[texIndices[i]];
      }
    }
  }

  std::unique_ptr<SDVertex[]> verts(new SDVertex[nVertices]);
  for (int i = 0; i < nVertices; ++i) {
    verts[i] = SDVertex(p[i]);
    if(has_uv) {
      verts[i].uv = TexcoordsExpanded[i];
    }
    vertices.push_back(&verts[i]);
  }

  int nFaces = nIndices / 3;
  std::unique_ptr<SDFace[]> fs(new SDFace[nFaces]);
  for (int i = 0; i < nFaces; ++i) {
    fs[i].MatID = face_material_ids[i];
    faces.push_back(&fs[i]);
  }
  
  // Set face to vertex pointers
  // rayrender addition: This also marks the vertices/faces as initialized
  const int *vp = vertexIndices.begin();
  
  for (int i = 0; i < nFaces; ++i, vp += 3) {
    SDFace *f = faces[i];
    for (int j = 0; j < 3; ++j) {
      SDVertex *v = vertices[vp[j]];
      f->v[j] = v;
      v->initialized = true;
      v->startFace = f;
    }
    f->vertices_initialized = true;
  }

  // Set neighbor pointers in _faces_
  std::set<SDEdge> edges;
  for (int i = 0; i < nFaces; ++i) {
    SDFace *f = faces[i];
    for (int edgeNum = 0; edgeNum < 3; ++edgeNum) {
      // Update neighbor pointer for _edgeNum_
      int v0 = edgeNum, v1 = NEXT(edgeNum);
      SDEdge e(f->v[v0], f->v[v1]);
      if (edges.find(e) == edges.end()) {
        // Handle new edge
        e.f[0] = f;
        e.f0edgeNum = edgeNum;
        edges.insert(e);
      } else {
        // Handle previously seen edge
        e = *edges.find(e);
        e.f[0]->f[e.f0edgeNum] = f;
        f->f[edgeNum] = e.f[0];
        edges.erase(e);
      }
    }
    f->faces_initialized = true;
  }

  // Finish vertex initialization
  for (int i = 0; i < nVertices; ++i) {
    SDVertex *v = vertices[i];
    SDFace *f = v->startFace;
    // SDFace *prevf = nullptr;
    if(!f || !f->faces_initialized) {
      continue;
    }
    do {
      f = f->nextFace(v);
      Rcpp::checkUserInterrupt(); //Add logic here to auto detect loops due to flipped edges
    } while (f && f != v->startFace);
    v->boundary = (f == nullptr);
    if (!v->boundary && v->valence() == 6) {
      v->regular = true;
    } else if (v->boundary && v->valence() == 4) {
      v->regular = true;
    } else {
      v->regular = false;
    }
    v->initialized = true;
  }

  // Refine _LoopSubdiv_ into triangles
  
  std::vector<SDFace *> f = faces;
  std::vector<SDVertex *> v = vertices;
  std::vector<SDFace *> newFaces;
  std::vector<SDVertex *> newVertices;
  for (int i = 0; i < nLevels; ++i) {
    if(verbose) {
      Rprintf("Subdividing mesh level %i/%i \n", (int)i+1, nLevels);
    }
    Rcpp::checkUserInterrupt();
    // Update _f_ and _v_ for next level of subdivision
    newFaces.clear();
    newVertices.clear();
    
    // Allocate next level of children in mesh tree
    for (SDVertex *vertex : v) {
      if(vertex->initialized) {
        vertex_storage.push_back(std::make_unique<SDVertex>());
        vertex->child = vertex_storage.back().get();
        vertex->child->regular = vertex->regular;
        vertex->child->boundary = vertex->boundary;
        newVertices.push_back(vertex->child);
      }
    }
    for (SDFace *face : f) {
      for (int k = 0; k < 4; ++k) {
        face_storage.push_back(std::make_unique<SDFace>());
        face->children[k] = face_storage.back().get();
        face->children[k]->MatID = face->MatID;
        newFaces.push_back(face->children[k]);
      }
    }


    // Update vertex positions and create new edge vertices
    // Update vertex positions for even vertices
    for (SDVertex *vertex : v) {
      if(!vertex->initialized) {
        continue;
      }

      if (!vertex->boundary) {
        // Apply one-ring rule for even vertex
        if (vertex->regular) {
          vertex->child->p = weightOneRing<glm::vec3>(vertex, vertex->p, 1.f / 16.f);
        } else {
          vertex->child->p = weightOneRing<glm::vec3>(vertex, vertex->p, beta(vertex->valence()));
        }
        if(has_uv) {
          if (vertex->regular) {
            vertex->child->uv = weightOneRing<glm::vec2>(vertex, vertex->uv, 1.f / 16.f);
          } else {
            vertex->child->uv = weightOneRing<glm::vec2>(vertex, vertex->uv, beta(vertex->valence()));
          }
        }
      } else {
        // Apply boundary rule for even vertex
        vertex->child->p = weightBoundary<glm::vec3>(vertex, vertex->p, 1.f / 8.f);
        if(has_uv) {
          vertex->child->uv = weightBoundary<glm::vec2>(vertex, vertex->uv, 1.f / 8.f);
        }
      }
    }    


    // Compute new odd edge vertices
    std::map<SDEdge, SDVertex *> edgeVerts;
    for (SDFace *face : f) {
      for (int k = 0; k < 3; ++k) {
        // Compute odd vertex on _k_th edge
        SDEdge edge(face->v[k], face->v[NEXT(k)]);
        SDVertex *vert = edgeVerts[edge];
        if (!vert) {
          // Create and initialize new odd vertex
          vertex_storage.push_back(std::make_unique<SDVertex>());
          vert = vertex_storage.back().get();
          newVertices.push_back(vert);
          vert->regular = true;
          vert->boundary = (face->f[k] == nullptr);
          vert->startFace = face->children[3];
          vert->initialized = true;
          
          // Apply edge rules to compute new vertex position
          if (vert->boundary) {
            vert->p = 0.5f * edge.v[0]->p;
            vert->p += 0.5f * edge.v[1]->p;
            if(has_uv) {
              vert->uv = 0.5f * edge.v[0]->uv;
              vert->uv += 0.5f * edge.v[1]->uv;
            }
          } else {
            vert->p = 3.f / 8.f * edge.v[0]->p;
            vert->p += 3.f / 8.f * edge.v[1]->p;
            vert->p += 1.f / 8.f * face->otherVert(edge.v[0], edge.v[1])->p;
            vert->p += 1.f / 8.f * face->f[k]->otherVert(edge.v[0], edge.v[1])->p;
            if(has_uv) {
              vert->uv = 3.f / 8.f * edge.v[0]->uv;
              vert->uv += 3.f / 8.f * edge.v[1]->uv;
              vert->uv += 1.f / 8.f * face->otherVert(edge.v[0], edge.v[1])->uv;
              vert->uv += 1.f / 8.f * face->f[k]->otherVert(edge.v[0], edge.v[1])->uv;
            }
          }
          edgeVerts[edge] = vert;
        }
      }
    }

    // Update new mesh topology
    
    // Update even vertex face pointers
    for (SDVertex *vertex : v) {
      if(vertex->initialized) {
        int vertNum = vertex->startFace->vnum(vertex);
        vertex->child->startFace = vertex->startFace->children[vertNum];
        vertex->child->initialized = true;
      }
    }
    
    // Update face neighbor pointers
    for (SDFace *face : f) {
      for (int j = 0; j < 3; ++j) {
        // Update children _f_ pointers for siblings
        face->children[3]->f[j] = face->children[NEXT(j)];
        face->children[j]->f[NEXT(j)] = face->children[3];
        
        // Update children _f_ pointers for neighbor children
        SDFace *f2 = face->f[j];
        face->children[j]->f[j] =
          f2 ? f2->children[f2->vnum(face->v[j])] : nullptr;
        f2 = face->f[PREV(j)];
        face->children[j]->f[PREV(j)] =
          f2 ? f2->children[f2->vnum(face->v[j])] : nullptr;
        //This helps avoid accessing floating triangles (e.g. which have no adjacent faces)
        if(!face->children[j]->f[j]) {
          continue;
        }
        
        face->children[j]->f[j]->faces_initialized = true;
      }
    }
    
    // Update face vertex pointers
    for (SDFace *face : f) {
      for (int j = 0; j < 3; ++j) {
        // Update child vertex pointer to new even vertex
        face->children[j]->v[j] = face->v[j]->child;
        
        // Update child vertex pointer to new odd vertex
        SDVertex *vert = edgeVerts[SDEdge(face->v[j], face->v[NEXT(j)])];
        face->children[j]->v[NEXT(j)] = vert;
        face->children[NEXT(j)]->v[j] = vert;
        face->children[3]->v[j] = vert;
        face->vertices_initialized = true;
      }
    }
    // Prepare for next level of subdivision
    f = newFaces;
    v = newVertices;
  }
  // Push vertices to limit surface
  NumericMatrix final_vertices(v.size(), 3);
  for (size_t i = 0; i < v.size(); ++i) {

    if(v[i]->initialized) {
      if (v[i]->boundary) {
        glm::vec3 tmp_v = weightBoundary<glm::vec3>(v[i], v[i]->p, 1.f / 5.f);
        final_vertices(i,0) = tmp_v[0];
        final_vertices(i,1) = tmp_v[1];
        final_vertices(i,2) = tmp_v[2];
      } else {
        glm::vec3 tmp_v = weightOneRing<glm::vec3>(v[i], v[i]->p, loopGamma(v[i]->valence()));
        final_vertices(i,0) = tmp_v[0];
        final_vertices(i,1) = tmp_v[1];
        final_vertices(i,2) = tmp_v[2];
      }
    }
  }

  for (size_t i = 0; i < v.size(); ++i) {
    if(v[i]->initialized) {
      v[i]->p = glm::vec3(final_vertices(i,0),
                          final_vertices(i,1),
                          final_vertices(i,2));
    }
  }
  
  // Compute UVs on limit surface
 NumericMatrix final_texcoords(v.size(),2);
  
  if(has_uv) {
    for (size_t i = 0; i < v.size(); ++i) {
      if(v[i]->initialized) {
        if (v[i]->boundary) {
          glm::vec2 tmp_uv = weightBoundary<glm::vec2>(v[i], v[i]->uv, 1.f / 5.f);
          final_texcoords(i,0) = tmp_uv[0];
          final_texcoords(i,1) = tmp_uv[1];
        } else {
          glm::vec2 tmp_uv = weightOneRing<glm::vec2>(v[i], v[i]->uv, loopGamma(v[i]->valence()));
          final_texcoords(i,0) = tmp_uv[0];
          final_texcoords(i,1) = tmp_uv[1];
        }
      }
    }
    
    for (size_t i = 0; i < v.size(); ++i) {
      if(v[i]->initialized) {
        v[i]->uv = glm::vec2(final_texcoords(i,0),
                             final_texcoords(i,1));
      }
    }
  }
  
  
  //Visualize the faces as colors related to topology
  // Compute vertex tangents on limit surface
  Rcpp::NumericMatrix final_normals(v.size(), 3);
  std::vector<glm::vec3> pRing(16, glm::vec3());
  int cntr = 0;
  for (SDVertex *vertex : v) {
    if(vertex->initialized) {
      glm::vec3 S(0, 0, 0), T(0, 0, 0);
      int valence = vertex->valence();
      if (valence > (int)pRing.size()) {
        pRing.resize(valence);
      }
      vertex->oneRing(&pRing[0]);
      if (!vertex->boundary) {
        // Compute tangents of interior face
        for (int j = 0; j < valence; ++j) {
          S += (float)std::cos(2 * M_PI * j / valence) * glm::vec3(pRing[j]);
          T += (float)std::sin(2 * M_PI * j / valence) * glm::vec3(pRing[j]);
        }
      } else {
        // Compute tangents of boundary face
        S = pRing[valence - 1] - pRing[0];
        if (valence == 2) {
          T = glm::vec3(pRing[0] + pRing[1] - 2.f * vertex->p);
        } else if (valence == 3) {
          T = pRing[1] - vertex->p;
        } else if (valence == 4) {   // regular
          T = glm::vec3(-1.f * pRing[0] + 2.0f * pRing[1] + 2.0f * pRing[2] +
            -1.f * pRing[3] + -2.f * vertex->p);
        } else {
          float theta = M_PI / float(valence - 1);
          T = glm::vec3(std::sin(theta) * (pRing[0] + pRing[valence - 1]));
          for (int k = 1; k < valence - 1; ++k) {
            float wt = (2 * std::cos(theta) - 2) * std::sin((k)*theta);
            T += glm::vec3(wt * pRing[k]);
          }
          T = -T;
        }
      }
      glm::vec3 norm = (glm::vec3(-cross(S, T)));
      norm = glm::normalize(norm);
      final_normals(cntr, 0) = norm[0];
      final_normals(cntr, 1) = norm[1];
      final_normals(cntr, 2) = norm[2];
    }
    cntr++;
  }
  // Create triangle mesh from subdivision mesh
  size_t ntris = f.size();
  std::unique_ptr<int[]> indices(new int[3 * ntris]);
  IntegerVector face_material_id(ntris);
  int *vp2 = indices.get();
  size_t totVerts = v.size();
  std::map<SDVertex *, int> usedVerts;
  for (size_t i = 0; i < totVerts; ++i) {
    if(v[i]->initialized) {
      usedVerts[v[i]] = i;
    }
  }

  for (size_t i = 0; i < ntris; ++i) {
    face_material_id[i] = f[i]->MatID;
    for (int j = 0; j < 3; ++j) {
      *vp2 = usedVerts[f[i]->v[j]];
      ++vp2;
    }
  }

  IntegerMatrix final_shape_indices(ntris, 3);
  IntegerMatrix final_shape_normals(ntris, 3);
  IntegerMatrix final_shape_uvs(ntris, 3);

  for (size_t s = 0; s < ntris * 3; s += 3) {
    final_shape_indices(s / 3, 0) = (indices[s]);
    final_shape_indices(s / 3, 1) = (indices[s+1]);
    final_shape_indices(s / 3, 2) = (indices[s+2]);
    // if(base_mesh->has_normals) {
    final_shape_normals(s / 3, 0) = (indices[s]);
    final_shape_normals(s / 3, 1) = (indices[s+1]);
    final_shape_normals(s / 3, 2) = (indices[s+2]);
    if(has_uv) {
      final_shape_uvs(s / 3, 0) = (indices[s]);
      final_shape_uvs(s / 3, 1) = (indices[s+1]);
      final_shape_uvs(s / 3, 2) = (indices[s+2]);
    } else {
      final_shape_uvs(s / 3, 0) = -1;
      final_shape_uvs(s / 3, 1) = -1;
      final_shape_uvs(s / 3, 2) = -1;
    }
  }
  // shapes_list["indices"] = Rcpp::clone(final_shape_indices);
  // if(has_uv) {
  //   shapes_list["tex_indices"] = Rcpp::clone(final_shape_indices);
  // }
  // shapes_list["norm_indices"] = Rcpp::clone(final_shape_indices);
  // shapes_list["material_ids"] = Rcpp::clone(face_material_id);
  // shapes_list["has_vertex_tex"] = Rcpp::clone(LogicalVector(face_material_id.size(),has_uv));
  // shapes_list["has_vertex_normals"] = Rcpp::clone(LogicalVector(face_material_id.size(),true));
  // 
  // Rcpp::List mesh_vertices = mesh["vertices"];
  // Rcpp::List mesh_texcoords = mesh["texcoords"];
  // Rcpp::List mesh_normals = mesh["normals"];
  // 
  // mesh_vertices(shape_i) = Rcpp::clone(final_vertices);
  // mesh_normals(shape_i) = Rcpp::clone(final_normals);
  // if(has_uv) {
  //   mesh_texcoords(shape_i) = Rcpp::clone(final_texcoords);
  // }
  return(List::create(_["indices"] = final_shape_indices,
                      _["face_material_id"] = face_material_id,
                      _["vertices"] = final_vertices,
                      _["texcoords"] = final_texcoords,
                      _["normals"] = final_normals,
                      _["has_vertex_tex"] = LogicalVector(face_material_id.size(),has_uv),
                      _["has_vertex_normals"] = LogicalVector(face_material_id.size(),true)
  ));
}

#define ALLOCA(TYPE, COUNT) (TYPE *) alloca((COUNT) * sizeof(TYPE))

template <typename T>
static T weightOneRing(SDVertex *vert, T vert_data, float beta) {
  // Put _vert_ one-ring in _pRing_
  int valence = vert->valence();
  T *pRing = ALLOCA(T, valence);
  vert->oneRing(pRing);
  T p = (1 - valence * beta) * vert_data;
  for (int i = 0; i < valence; ++i) {
    p += beta * pRing[i];
  }
  return p;
}

//Need methods for each data type
void SDVertex::oneRing(glm::vec3 *p) {
  if (!boundary) {
    // Get one-ring vertices for interior vertex
    SDFace *face = startFace;
    do {
      *p++ = face->nextVert(this)->p;
      face = face->nextFace(this);
    } while (face != startFace);
  } else {
    // Get one-ring vertices for boundary vertex
    SDFace *face = startFace, *f2;
    while ((f2 = face->nextFace(this)) != nullptr) face = f2;
    *p++ = face->nextVert(this)->p;
    do {
      *p++ = face->prevVert(this)->p;
      face = face->prevFace(this);
    } while (face != nullptr);
  }
}

void SDVertex::oneRing(glm::vec2 *p) {
  if (!boundary) {
    // Get one-ring vertices for interior vertex
    SDFace *face = startFace;
    do {
      *p++ = face->nextVert(this)->uv;
      face = face->nextFace(this);
    } while (face != startFace);
  } else {
    // Get one-ring vertices for boundary vertex
    SDFace *face = startFace, *f2;
    while ((f2 = face->nextFace(this)) != nullptr) face = f2;
    *p++ = face->nextVert(this)->uv;
    do {
      *p++ = face->prevVert(this)->uv;
      face = face->prevFace(this);
    } while (face != nullptr);
  }
}

template<typename T>
static T weightBoundary(SDVertex *vert, T vert_data, float beta) {
  // Put _vert_ one-ring in _pRing_
  int valence = vert->valence();
  T *pRing = ALLOCA(T, valence); //allocate on the stack
  vert->oneRing(pRing);
  T newValue = (1 - 2 * beta) * vert_data;
  newValue += beta * pRing[0];
  newValue += beta * pRing[valence - 1];
  return newValue;
}