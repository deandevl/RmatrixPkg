#' Create a new 3d device window.
#'
#' Function creates a new window with options for background color, size, camera angles, field of view angle,
#' and zoom.
#'
#' @importFrom rgl open3d
#' @importFrom rgl bg3d
#' @importFrom rgl par3d
#' @importFrom rgl view3d
#' @importFrom rgl cur3d
#' @importFrom rgl clear3d
#' @importFrom rgl close3d
#' @importFrom rgl rgl.dev.list
#' @importFrom rgl set3d
#'
#' @param bgrd_color The String background color of the window. Default is "white".
#' @param width The integer width of the window in pixels. The default is 640.
#' @param height The integer height of the window in pixels. The default is 640.
#' @param vp_theta The rotation of the camera. The default is 0.
#' @param vp_phi The rotation of the camera. The default is 15.
#' @param vp_fov The field of view angle. The default is 60.
#' @param vp_zoom The degree of zoom for the camera. The default is 1.
#'
#' @return The newly created integer id for the device window.
#'
#' @export
device_new <- function(bgrd_color="white", width=640, height=640, vp_theta=0, vp_phi=15, vp_fov=60, vp_zoom=1){
  rgl::open3d()
  rgl::bg3d(color = bgrd_color)
  rgl::par3d(windowRect = 50 + c(0, 0, width, height))
  rgl::view3d(theta = vp_theta, phi = vp_phi, fov = vp_fov, zoom = vp_zoom)
  rgl::cur3d()
}


#' Clear an object associated with the window.
#'
#' Function clears specific types of objects from the device window including "shapes", "bboxdeco", "lights", "background".
#'
#' @param clear_types A character vector that specifies the types of
#' objects to clear. Accepted values for the vector include: "shapes", "bboxdeco", "lights", "background".
#' The default is all 4 types are specified.
#'
#' @export
device_clear <- function(clear_types = c("shapes", "bboxdeco", "lights", "background")){
  if(is.vector(clear_types)){
    rgl::clear3d(type = clear_types)
  }
}


#' Close the device window.
#'
#' Function closes the current device window.
#' See 'device_current()' function for setting the current window.
#'
#'@return Returns the current list of device id's
#'
#' @export
device_close_current <- function(){
  rgl::close3d()
  return(rgl::rgl.dev.list())
}


#' Close all device windows
#'
#' Close all devices including the current device
#'
#' @export
device_close_all <- function(){
  for(id in rgl::rgl.dev.list()){
    rgl::close3d(dev = id)
  }
}


#' Get a list of all device window ids.
#'
#' Function returns a list of all created windows with their respective device id's.
#'
#' @return Returns a list of all the integer ids of the created windows.
#'
#' @export
device_list <- function(){
  return(rgl::rgl.dev.list())
}


#' Set or return the integer id of the current window.
#'
#' Function is used to set a specific window as 'current' or return the id of the 'current' window.
#' In setting the id, the value is checked for existence among the list of window id's.
#'
#' @param id If defined then sets the current device window to 'id'.
#'
#' @return If id is undefined, returns the current device id among one or more device windows.
#'
#' @export
device_current <- function(id){
  if(!missing(id)){
    if(id %in% rgl::rgl.dev.list()) {
      rgl::set3d(dev = id)
      return(rgl::cur3d())
    }
  }else {
    return(rgl::cur3d())
  }
}
