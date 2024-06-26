% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_mesh_functions.R
\name{scale_unit_mesh}
\alias{scale_unit_mesh}
\title{Scale Mesh to Unit Bounding Box}
\usage{
scale_unit_mesh(mesh, center_mesh = FALSE)
}
\arguments{
\item{mesh}{The mesh.}

\item{center_mesh}{Default \code{FALSE}. Whether to center the mesh at the origin after scaling.}
}
\value{
Scaled mesh
}
\description{
Scale Mesh to Unit Bounding Box
}
\examples{
if(run_documentation()) {
#Scale the Cornell box (and contents) down to the unit box.
robj = obj_mesh(r_obj(), scale=150,angle=c(0,180,0))

generate_cornell_mesh() |>
add_shape(scale_mesh(translate_mesh(robj,c(400,100,155)),0.5, center=c(400,100,155))) |>
add_shape(scale_mesh(translate_mesh(robj,c(555/2,200,555/2)),1.5, center=c(555/2,200,555/2))) |>
add_shape(scale_mesh(translate_mesh(robj,c(55,300,400)),c(0.5,2,0.5), center=c(155,300,400))) |>
scale_unit_mesh(center_mesh = TRUE) |> 
rasterize_scene(light_info=directional_light(direction=c(0.1,0.6,-1)), 
                lookfrom = c(0,0,-2), lookat=c(0,0,0))
}
}
