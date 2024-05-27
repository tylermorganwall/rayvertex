// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppThread.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// load_obj
List load_obj(std::string inputfile, std::string basedir);
RcppExport SEXP _rayvertex_load_obj(SEXP inputfileSEXP, SEXP basedirSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type inputfile(inputfileSEXP);
    Rcpp::traits::input_parameter< std::string >::type basedir(basedirSEXP);
    rcpp_result_gen = Rcpp::wrap(load_obj(inputfile, basedir));
    return rcpp_result_gen;
END_RCPP
}
// load_ply
List load_ply(std::string inputfile, std::string basedir);
RcppExport SEXP _rayvertex_load_ply(SEXP inputfileSEXP, SEXP basedirSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type inputfile(inputfileSEXP);
    Rcpp::traits::input_parameter< std::string >::type basedir(basedirSEXP);
    rcpp_result_gen = Rcpp::wrap(load_ply(inputfile, basedir));
    return rcpp_result_gen;
END_RCPP
}
// LoopSubdivide
List LoopSubdivide(List mesh, int shape_i, const int nLevels, bool verbose);
RcppExport SEXP _rayvertex_LoopSubdivide(SEXP meshSEXP, SEXP shape_iSEXP, SEXP nLevelsSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mesh(meshSEXP);
    Rcpp::traits::input_parameter< int >::type shape_i(shape_iSEXP);
    Rcpp::traits::input_parameter< const int >::type nLevels(nLevelsSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(LoopSubdivide(mesh, shape_i, nLevels, verbose));
    return rcpp_result_gen;
END_RCPP
}
// rasterize_lines_rcpp
List rasterize_lines_rcpp(NumericMatrix line_mat, int nx, int ny, NumericVector model_color, NumericVector lookfrom, NumericVector lookat, double fov, double near_clip, double far_clip, NumericVector bounds, NumericVector camera_up, double alpha_line, double line_offset, NumericVector ortho_dims, bool aa_lines);
RcppExport SEXP _rayvertex_rasterize_lines_rcpp(SEXP line_matSEXP, SEXP nxSEXP, SEXP nySEXP, SEXP model_colorSEXP, SEXP lookfromSEXP, SEXP lookatSEXP, SEXP fovSEXP, SEXP near_clipSEXP, SEXP far_clipSEXP, SEXP boundsSEXP, SEXP camera_upSEXP, SEXP alpha_lineSEXP, SEXP line_offsetSEXP, SEXP ortho_dimsSEXP, SEXP aa_linesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type line_mat(line_matSEXP);
    Rcpp::traits::input_parameter< int >::type nx(nxSEXP);
    Rcpp::traits::input_parameter< int >::type ny(nySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type model_color(model_colorSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type lookfrom(lookfromSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type lookat(lookatSEXP);
    Rcpp::traits::input_parameter< double >::type fov(fovSEXP);
    Rcpp::traits::input_parameter< double >::type near_clip(near_clipSEXP);
    Rcpp::traits::input_parameter< double >::type far_clip(far_clipSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type bounds(boundsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type camera_up(camera_upSEXP);
    Rcpp::traits::input_parameter< double >::type alpha_line(alpha_lineSEXP);
    Rcpp::traits::input_parameter< double >::type line_offset(line_offsetSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type ortho_dims(ortho_dimsSEXP);
    Rcpp::traits::input_parameter< bool >::type aa_lines(aa_linesSEXP);
    rcpp_result_gen = Rcpp::wrap(rasterize_lines_rcpp(line_mat, nx, ny, model_color, lookfrom, lookat, fov, near_clip, far_clip, bounds, camera_up, alpha_line, line_offset, ortho_dims, aa_lines));
    return rcpp_result_gen;
END_RCPP
}
// rasterize
List rasterize(List mesh, NumericMatrix lightinfo, NumericMatrix line_mat, int nx, int ny, NumericVector model_color, NumericVector lookfrom, NumericVector lookat, double fov, IntegerVector typevals, bool has_shadow_map, bool calc_ambient, bool tbn, double ambient_radius, double shadow_map_bias, int numbercores, int max_indices, LogicalVector has_normals_vec, LogicalVector has_tex_vec, LogicalVector has_texture, LogicalVector has_ambient_texture, LogicalVector has_bump_texture, LogicalVector has_normal_texture, LogicalVector has_specular_texture, LogicalVector has_emissive_texture, int block_size, bool use_default_material, double near_clip, double far_clip, double shadow_map_intensity, NumericVector bounds, IntegerVector shadowdims, NumericVector camera_up, double alpha_line, double line_offset, NumericVector ortho_dims, LogicalVector is_dir_light, bool aa_lines, LogicalVector& has_vertex_tex, LogicalVector& has_vertex_normals, LogicalVector has_reflection_map, Rcpp::String reflection_map_file, double background_sharpness, LogicalVector has_refraction, bool environment_map_hdr, bool has_environment_map, NumericVector bg_color, bool verbose);
RcppExport SEXP _rayvertex_rasterize(SEXP meshSEXP, SEXP lightinfoSEXP, SEXP line_matSEXP, SEXP nxSEXP, SEXP nySEXP, SEXP model_colorSEXP, SEXP lookfromSEXP, SEXP lookatSEXP, SEXP fovSEXP, SEXP typevalsSEXP, SEXP has_shadow_mapSEXP, SEXP calc_ambientSEXP, SEXP tbnSEXP, SEXP ambient_radiusSEXP, SEXP shadow_map_biasSEXP, SEXP numbercoresSEXP, SEXP max_indicesSEXP, SEXP has_normals_vecSEXP, SEXP has_tex_vecSEXP, SEXP has_textureSEXP, SEXP has_ambient_textureSEXP, SEXP has_bump_textureSEXP, SEXP has_normal_textureSEXP, SEXP has_specular_textureSEXP, SEXP has_emissive_textureSEXP, SEXP block_sizeSEXP, SEXP use_default_materialSEXP, SEXP near_clipSEXP, SEXP far_clipSEXP, SEXP shadow_map_intensitySEXP, SEXP boundsSEXP, SEXP shadowdimsSEXP, SEXP camera_upSEXP, SEXP alpha_lineSEXP, SEXP line_offsetSEXP, SEXP ortho_dimsSEXP, SEXP is_dir_lightSEXP, SEXP aa_linesSEXP, SEXP has_vertex_texSEXP, SEXP has_vertex_normalsSEXP, SEXP has_reflection_mapSEXP, SEXP reflection_map_fileSEXP, SEXP background_sharpnessSEXP, SEXP has_refractionSEXP, SEXP environment_map_hdrSEXP, SEXP has_environment_mapSEXP, SEXP bg_colorSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mesh(meshSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type lightinfo(lightinfoSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type line_mat(line_matSEXP);
    Rcpp::traits::input_parameter< int >::type nx(nxSEXP);
    Rcpp::traits::input_parameter< int >::type ny(nySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type model_color(model_colorSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type lookfrom(lookfromSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type lookat(lookatSEXP);
    Rcpp::traits::input_parameter< double >::type fov(fovSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type typevals(typevalsSEXP);
    Rcpp::traits::input_parameter< bool >::type has_shadow_map(has_shadow_mapSEXP);
    Rcpp::traits::input_parameter< bool >::type calc_ambient(calc_ambientSEXP);
    Rcpp::traits::input_parameter< bool >::type tbn(tbnSEXP);
    Rcpp::traits::input_parameter< double >::type ambient_radius(ambient_radiusSEXP);
    Rcpp::traits::input_parameter< double >::type shadow_map_bias(shadow_map_biasSEXP);
    Rcpp::traits::input_parameter< int >::type numbercores(numbercoresSEXP);
    Rcpp::traits::input_parameter< int >::type max_indices(max_indicesSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type has_normals_vec(has_normals_vecSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type has_tex_vec(has_tex_vecSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type has_texture(has_textureSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type has_ambient_texture(has_ambient_textureSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type has_bump_texture(has_bump_textureSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type has_normal_texture(has_normal_textureSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type has_specular_texture(has_specular_textureSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type has_emissive_texture(has_emissive_textureSEXP);
    Rcpp::traits::input_parameter< int >::type block_size(block_sizeSEXP);
    Rcpp::traits::input_parameter< bool >::type use_default_material(use_default_materialSEXP);
    Rcpp::traits::input_parameter< double >::type near_clip(near_clipSEXP);
    Rcpp::traits::input_parameter< double >::type far_clip(far_clipSEXP);
    Rcpp::traits::input_parameter< double >::type shadow_map_intensity(shadow_map_intensitySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type bounds(boundsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type shadowdims(shadowdimsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type camera_up(camera_upSEXP);
    Rcpp::traits::input_parameter< double >::type alpha_line(alpha_lineSEXP);
    Rcpp::traits::input_parameter< double >::type line_offset(line_offsetSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type ortho_dims(ortho_dimsSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type is_dir_light(is_dir_lightSEXP);
    Rcpp::traits::input_parameter< bool >::type aa_lines(aa_linesSEXP);
    Rcpp::traits::input_parameter< LogicalVector& >::type has_vertex_tex(has_vertex_texSEXP);
    Rcpp::traits::input_parameter< LogicalVector& >::type has_vertex_normals(has_vertex_normalsSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type has_reflection_map(has_reflection_mapSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type reflection_map_file(reflection_map_fileSEXP);
    Rcpp::traits::input_parameter< double >::type background_sharpness(background_sharpnessSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type has_refraction(has_refractionSEXP);
    Rcpp::traits::input_parameter< bool >::type environment_map_hdr(environment_map_hdrSEXP);
    Rcpp::traits::input_parameter< bool >::type has_environment_map(has_environment_mapSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type bg_color(bg_colorSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(rasterize(mesh, lightinfo, line_mat, nx, ny, model_color, lookfrom, lookat, fov, typevals, has_shadow_map, calc_ambient, tbn, ambient_radius, shadow_map_bias, numbercores, max_indices, has_normals_vec, has_tex_vec, has_texture, has_ambient_texture, has_bump_texture, has_normal_texture, has_specular_texture, has_emissive_texture, block_size, use_default_material, near_clip, far_clip, shadow_map_intensity, bounds, shadowdims, camera_up, alpha_line, line_offset, ortho_dims, is_dir_light, aa_lines, has_vertex_tex, has_vertex_normals, has_reflection_map, reflection_map_file, background_sharpness, has_refraction, environment_map_hdr, has_environment_map, bg_color, verbose));
    return rcpp_result_gen;
END_RCPP
}
// smooth_normals_mesh_rcpp
List smooth_normals_mesh_rcpp(List mesh_input, bool override_existing);
RcppExport SEXP _rayvertex_smooth_normals_mesh_rcpp(SEXP mesh_inputSEXP, SEXP override_existingSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mesh_input(mesh_inputSEXP);
    Rcpp::traits::input_parameter< bool >::type override_existing(override_existingSEXP);
    rcpp_result_gen = Rcpp::wrap(smooth_normals_mesh_rcpp(mesh_input, override_existing));
    return rcpp_result_gen;
END_RCPP
}
// tonemap_image
Rcpp::List tonemap_image(Rcpp::NumericMatrix routput, Rcpp::NumericMatrix goutput, Rcpp::NumericMatrix boutput, int toneval);
RcppExport SEXP _rayvertex_tonemap_image(SEXP routputSEXP, SEXP goutputSEXP, SEXP boutputSEXP, SEXP tonevalSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type routput(routputSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type goutput(goutputSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type boutput(boutputSEXP);
    Rcpp::traits::input_parameter< int >::type toneval(tonevalSEXP);
    rcpp_result_gen = Rcpp::wrap(tonemap_image(routput, goutput, boutput, toneval));
    return rcpp_result_gen;
END_RCPP
}
// wireframe
List wireframe(NumericMatrix verts, IntegerMatrix inds, int nx, int ny);
RcppExport SEXP _rayvertex_wireframe(SEXP vertsSEXP, SEXP indsSEXP, SEXP nxSEXP, SEXP nySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type verts(vertsSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type inds(indsSEXP);
    Rcpp::traits::input_parameter< int >::type nx(nxSEXP);
    Rcpp::traits::input_parameter< int >::type ny(nySEXP);
    rcpp_result_gen = Rcpp::wrap(wireframe(verts, inds, nx, ny));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rayvertex_load_obj", (DL_FUNC) &_rayvertex_load_obj, 2},
    {"_rayvertex_load_ply", (DL_FUNC) &_rayvertex_load_ply, 2},
    {"_rayvertex_LoopSubdivide", (DL_FUNC) &_rayvertex_LoopSubdivide, 4},
    {"_rayvertex_rasterize_lines_rcpp", (DL_FUNC) &_rayvertex_rasterize_lines_rcpp, 15},
    {"_rayvertex_rasterize", (DL_FUNC) &_rayvertex_rasterize, 48},
    {"_rayvertex_smooth_normals_mesh_rcpp", (DL_FUNC) &_rayvertex_smooth_normals_mesh_rcpp, 2},
    {"_rayvertex_tonemap_image", (DL_FUNC) &_rayvertex_tonemap_image, 4},
    {"_rayvertex_wireframe", (DL_FUNC) &_rayvertex_wireframe, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_rayvertex(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
