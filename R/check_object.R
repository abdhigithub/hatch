
#' Check_Object function
#' @description to verify whether NIFTI or path
#' @param object Image or path input
#' @importFrom neurobase readnii
#' @importFrom oro.nifti is.nifti
#' @return An image, either same as input or read from path
#' @export



##### Make examples

check_object <- function(object){
  if(!oro.nifti::is.nifti(object)) {
    if(file.exists(object)) use_image <- neurobase::readnii(object)
    if(!file.exists(object)) stop (paste0("File does not exist for ",object,"!!"))
  }


  if(oro.nifti::is.nifti(object)) { use_image<-object }

  return(use_image)
}


