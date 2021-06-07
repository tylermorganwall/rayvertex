% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rasterize_scene.R
\name{rasterize_scene}
\alias{rasterize_scene}
\title{Rasterize Scene}
\usage{
rasterize_scene(
  scene,
  filename = NA,
  width = 800,
  height = 800,
  line_info = NULL,
  alpha_line = 1,
  parallel = TRUE,
  fov = 20,
  lookfrom = c(0, 0, 10),
  lookat = NULL,
  camera_up = c(0, 1, 0),
  light_info = directional_light(),
  color = "red",
  type = "diffuse",
  background = "black",
  tangent_space_normals = TRUE,
  shadow_map = TRUE,
  shadow_map_bias = 0.003,
  shadow_map_intensity = 0,
  shadow_map_dims = NULL,
  ssao = FALSE,
  ssao_intensity = 10,
  ssao_radius = 0.1,
  tonemap = "none",
  debug = "none",
  near_plane = 0.1,
  far_plane = 100,
  shader = "default",
  block_size = 4,
  shape = NULL,
  line_offset = 1e-05,
  ortho_dimensions = c(1, 1),
  bloom = FALSE,
  antialias_lines = TRUE,
  environment_map = "",
  background_sharpness = 1
)
}
\arguments{
\item{scene}{The scene object.}

\item{filename}{Default `NULL`. Filename to save the image. If `NULL`, the image will be plotted.}

\item{width}{Default `400`. Width of the rendered image.}

\item{height}{Default `400`. Width of the rendered image.}

\item{line_info}{Default `NULL`. Matrix of line segments to add to the scene. Number of rows must be a multiple of 2.}

\item{alpha_line}{Default `1`. Line transparency.}

\item{parallel}{Default `TRUE`. Whether to use parallel processing.}

\item{fov}{Default `20`. Width of the rendered image.}

\item{lookfrom}{Default `c(0,0,10)`. Camera location.}

\item{lookat}{Default `NULL`. Camera focal position, defaults to the center of the model.}

\item{camera_up}{Default `c(0,1,0)`. Camera up vector.}

\item{light_info}{Default `directional_light()`. Description of scene lights, generated with the `point_light()` and
`directional_light()` functions.}

\item{color}{Default `darkred`. Color of model if no material file present (or for faces using the default material).}

\item{type}{Default `diffuse`. Shader type. Other options: `vertex` (Gouraud shading), `phong`, and `color` (no lighting).}

\item{background}{Default `white`. Background color.}

\item{tangent_space_normals}{Default `TRUE`.}

\item{shadow_map}{Default `FALSE`.}

\item{shadow_map_bias}{Default `0.005`.}

\item{shadow_map_intensity}{Default `0.5`.}

\item{shadow_map_dims}{Default `NULL`.}

\item{ssao}{Default `FALSE`. Whether to add screen-space ambient occlusion (SSAO) to the render.}

\item{ssao_intensity}{Default `10`. Intensity of the shadow map.}

\item{ssao_radius}{Default `0.1`. Radius to use when calculating the SSAO term.}

\item{tonemap}{Default `"none"`.}

\item{debug}{Default `"none"`.}

\item{near_plane}{Default `0.1`.}

\item{far_plane}{Default `100`.}

\item{shader}{Default `"default"`.}

\item{block_size}{Default `4`.}

\item{shape}{Default `NULL`. The shape to render in the OBJ mesh.}

\item{line_offset}{Default `0.0001`. Amount to offset lines towards camera to prevent z-fighting.}

\item{ortho_dimensions}{Default `c(1,1)`. Width and height of the orthographic camera. Will only be used if `fov = 0`.}

\item{bloom}{Default `FALSE`. Whether to apply bloom to the image. If `TRUE`,
this performs a convolution of the HDR image of the scene with a sharp, long-tailed
exponential kernel, which does not visibly affect dimly pixels, but does result in emitters light
slightly bleeding into adjacent pixels.}

\item{antialias_lines}{Default `TRUE`. Whether to anti-alias lines in the scene.}

\item{environment_map}{Default `""`. Image file to use as a texture for all reflective and refractive
materials in the scene, along with the background.}

\item{background_sharpness}{Default `1.0`. A number greater than zero but less than one indicating the sharpness
of the background image.}
}
\value{
Rasterized image.
}
\description{
Render a 3D scene with meshes, lights, and lines using a software rasterizer.
}
\examples{
#Let's load the cube OBJ file included with the package

\donttest{
rasterize_scene(cube_mesh(),lookfrom=c(2,4,10), 
              light_info = directional_light(direction=c(0.5,1,0.7)))
}
#Flatten the cube, translate downwards, and set to grey
base_model = cube_mesh() \%>\% 
 scale_mesh(scale=c(5,0.2,5)) \%>\%
 translate_mesh(c(0,-0.1,0)) \%>\% 
 set_material(diffuse="grey80") 

\donttest{
rasterize_scene(base_model, lookfrom=c(2,4,10), 
              light_info = directional_light(direction=c(0.5,1,0.7)))
}

#load the R OBJ file, scale it down, color it blue, and add it to the grey base
r_model = obj_mesh(r_obj()) \%>\% 
 scale_mesh(scale=0.5) \%>\% 
 set_material(diffuse="dodgerblue") \%>\% 
 add_shape(base_model)
 
\donttest{
rasterize_scene(r_model, lookfrom=c(2,4,10), 
              light_info = directional_light(direction=c(0.5,1,0.7)))
}
#Zoom in and reduce the shadow mapping intensity
\donttest{
rasterize_scene(r_model, lookfrom=c(2,4,10), fov=10,shadow_map = TRUE, shadow_map_intensity=0.3,
              light_info = directional_light(direction=c(0.5,1,0.7)))
}

#Include the resolution (4x) of the shadow map for less pixellation around the edges
#Also decrease the shadow_map_bias slightly to remove the "peter panning" floating shadow effect
\donttest{
rasterize_scene(r_model, lookfrom=c(2,4,10), fov=10,
              shadow_map_dims=4, 
              light_info = directional_light(direction=c(0.5,1,0.7)))
}

#Add some more directional lights and change their color
lights = directional_light(c(0.7,1.1,-0.9),color = "orange",intensity = 1) \%>\% 
           add_light(directional_light(c(0.7,1,1),color = "dodgerblue",intensity = 1)) \%>\% 
           add_light(directional_light(c(2,4,10),color = "white",intensity = 0.5))
\donttest{
rasterize_scene(r_model, lookfrom=c(2,4,10), fov=10,
              light_info = lights)
}
#Add some point lights
lights_p = lights \%>\% 
 add_light(point_light(position=c(-1,1,0),color="red", intensity=2)) \%>\% 
 add_light(point_light(position=c(1,1,0),color="purple", intensity=2)) 
\donttest{
rasterize_scene(r_model, lookfrom=c(2,4,10), fov=10,
              light_info = lights_p)
}
#change the camera position
\donttest{
rasterize_scene(r_model, lookfrom=c(-2,2,-10), fov=10,
              light_info = lights_p)
}
              
\donttest{        
#Add a spiral of lines around the model by generating a matrix of line segments
t = seq(0,8*pi,length.out=361)
line_mat = matrix(nrow=0,ncol=9)

for(i in 1:360) {
  line_mat = add_lines(line_mat,
                      generate_line(start = c(0.5*sin(t[i]), t[i]/(8*pi), 0.5*cos(t[i])),
                                    end  = c(0.5*sin(t[i+1]), t[i+1]/(8*pi), 0.5*cos(t[i+1]))))
}

rasterize_scene(r_model, lookfrom=c(2,4,10), fov=10, line_info = line_mat,
              light_info = lights)
}
}