#' Get a list of objects in the device window.
#'
#' Function returns a list of all the object id's contained in the current window.
#'
#' @importFrom rgl rgl.ids
#' @importFrom rgl rgl.pop
#'
#' @return A dataframe containing all the object ids' for the current device window.
#'
#' @export
object_list <- function(){
  rgl::rgl.ids()
}

#' Remove an object from the window.
#'
#' Function removes a specific object from the window.  The submitted object id is checked for
#' existence among the windows listed id's.
#'
#' @param object_id An object's specific integer id that selects and removes it from the scene.
#'
#' @export
#'
object_remove <- function(object_id){
  if(object_id %in% rgl::rgl.ids()) {
    rgl::rgl.pop(id = object_id)
  }
}
