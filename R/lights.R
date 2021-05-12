#'@title Point light
#'
#'@description The falloff of the point light intensity is given by the following equation (referenc:
#'
#'Intensity = intensity / (constant + falloff * distance + falloff_quad * (distance * distance));    
#'
#'@param position  A two-dimensional matrix, where each entry in the matrix is the elevation at that point. All points are assumed to be evenly spaced.
#'@param color Default `400`. Width of the rendered image.
#'@param intensity Default `1`. Intensity of the point light.
#'@param constant Default `1`. Constant term. See description for details.
#'@param falloff Default `1`. Linear falloff term. See description for details.
#'@param falloff_quad Default `1`. Quadratic falloff term. See description for details.

#'@return Rasterized image.
#'@export
#'@examples
#'#Add point lights and vary the intensity
#'lights_int = point_light(position=c(100,100,400), color="white", intensity=0.125,
#'                       falloff_quad = 0.0, constant = 0.0002, falloff = 0.005) %>% 
#'  add_light(point_light(position=c(100,455,400), color="white", intensity=0.25,
#'                        falloff_quad = 0.0, constant = 0.0002, falloff = 0.005)) %>% 
#'  add_light(point_light(position=c(455,100,400), color="white", intensity=0.5,
#'                        falloff_quad = 0.0, constant = 0.0002, falloff = 0.005)) %>% 
#'  add_light(point_light(position=c(455,455,400), color="white", intensity=1,
#'                        falloff_quad = 0.0, constant = 0.0002, falloff = 0.005))
#'                        
#'\donttest{
#'generate_cornell_mesh(light=FALSE) %>% 
#'  rasterize_scene(light_info = lights_int)
#'}
#'#Add point lights and vary the color
#'lights_c = point_light(position=c(100,100,500), color="red", 
#'                       falloff_quad = 0.0, constant = 0.0002, falloff = 0.005) %>% 
#'  add_light(point_light(position=c(100,455,500), color="blue",
#'                        falloff_quad = 0.0, constant = 0.0002, falloff = 0.005)) %>% 
#'  add_light(point_light(position=c(455,100,500), color="purple", 
#'                        falloff_quad = 0.0, constant = 0.0002, falloff = 0.005)) %>% 
#'  add_light(point_light(position=c(455,455,500), color="yellow", 
#'                        falloff_quad = 0.0, constant = 0.0002, falloff = 0.005))
#'                        
#'\donttest{
#'generate_cornell_mesh(light=FALSE) %>% 
#'  rasterize_scene(light_info = lights_c)
#'}
#'#Add point lights and vary the falloff term
#'lights_fo = point_light(position=c(100,100,500), color="white", 
#'                       falloff_quad = 0.0, constant = 0.0002, falloff = 0.005) %>% 
#'  add_light(point_light(position=c(100,455,500), color="white",
#'                        falloff_quad = 0.0, constant = 0.0002, falloff = 0.01)) %>% 
#'  add_light(point_light(position=c(455,100,500), color="white", 
#'                        falloff_quad = 0.0, constant = 0.0002, falloff = 0.02)) %>% 
#'  add_light(point_light(position=c(455,455,500), color="white", 
#'                        falloff_quad = 0.0, constant = 0.0002, falloff = 0.04))
#'\donttest{
#'generate_cornell_mesh(light=FALSE) %>% 
#'  rasterize_scene(light_info = lights_fo)
#'}
#'
#'#Add point lights and vary the quadradic falloff term
#'lights_quad = point_light(position=c(100,100,500), color="white", 
#'                       falloff_quad = 0.0001, constant = 0.0002, falloff = 0.005) %>% 
#'  add_light(point_light(position=c(100,455,500), color="white",
#'                        falloff_quad = 0.0002, constant = 0.0002, falloff = 0.005)) %>% 
#'  add_light(point_light(position=c(455,100,500), color="white", 
#'                        falloff_quad = 0.0004, constant = 0.0002, falloff = 0.005)) %>% 
#'  add_light(point_light(position=c(455,455,500), color="white", 
#'                        falloff_quad = 0.0008, constant = 0.0002, falloff = 0.005))
#'\donttest{
#'generate_cornell_mesh(light=FALSE) %>% 
#'  rasterize_scene(light_info = lights_quad)
#'}
point_light = function(position = c(0,0,0), color = "white",  intensity=1, 
                       constant = 1, falloff = 1, falloff_quad = 1) {
  color = convert_color(color)
  returnmat = matrix(c(position, color*intensity, constant, falloff,falloff_quad), nrow=1,ncol=9)
  colnames(returnmat) = c("x","y","z","r","g","b","constant","falloff","falloff_quad")
  returnmat
}

#'@title Generate Directional Lights
#'
#'@param direction Default `c(0,1,0)`. Direction of the light.
#'@param color Default `white`. COlor of the light.
#'@param intensity Default `1`. Intensity of the light.
#'
#'@return Light matrix.
#'@export
#'@examples
#'#Add a light to scene (manually specify the light automatically added to the Cornell Box
#'lights = point_light(position=c(555/2,450,555/2),
#'                     falloff_quad = 0.0, constant = 0.0002, falloff = 0.005)
#'\donttest{
#'generate_cornell_mesh(light=FALSE) %>% 
#'  rasterize_scene(light_info = lights)
#'}
#'#Add a directional light
#'lights_d = add_light(lights, directional_light(direction=c(1,1.5,-1)))
#'
#'\donttest{
#'generate_cornell_mesh(light=FALSE) %>% 
#'  rasterize_scene(light_info = lights_d)
#'}
#'#Change the intensity and color
#'lights_d = add_light(lights, 
#'                     directional_light(direction=c(1,1.5,-1),color="orange", intensity=0.5))
#'
#'\donttest{
#'generate_cornell_mesh(light=FALSE) %>% 
#'  rasterize_scene(light_info = lights_d)
#'}
directional_light = function(direction = c(0,1,0), color = "white", intensity=1) {
  color = convert_color(color)
  returnmat = matrix(c(direction, color*intensity, 0, 0, 0), nrow=1,ncol=9)
  colnames(returnmat) = c("x","y","z","r","g","b","constant","falloff","falloff_quad")
  returnmat
}

#'@title Add light
#'
#'@param lights Current light scene.
#'@param light New light to add.
#'
#'@return Light matrix.
#'@export
#'@examples
#'#Add a light to scene (manually specify the light automatically added to the Cornell Box
#'lights = point_light(position=c(555/2,450,555/2),
#'                     falloff_quad = 0.0, constant = 0.0002, falloff = 0.005)
#'\donttest{
#'generate_cornell_mesh(light=FALSE) %>% 
#'  rasterize_scene(light_info = lights)
#'}
#'#Add directional lights and a point light
#'lights_d = add_light(lights, directional_light(direction=c(1,1.5,-1), intensity=0.2)) %>% 
#'  add_light(directional_light(direction=c(-1,1.5,-1),color="red", intensity=0.2)) %>% 
#'  add_light(point_light(position=c(555/2,50,555/2), color="blue", intensity=0.3,
#'                        falloff_quad = 0.0, constant = 0.0002, falloff = 0.005))
#'\donttest{
#'generate_cornell_mesh(light=FALSE) %>% 
#'  rasterize_scene(light_info = lights_d)
#'}
add_light = function(lights, light) {
  rbind(lights,light)
}