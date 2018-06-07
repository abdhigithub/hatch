#' Create Landmarks
#' @description This function creates landmark points for piecewise histogram matching
#'
#' @param data image
#' @param i.min min for subj landmark
#' @param i.max max for subj landmark
#' @param i.s.min min for control landmark
#' @param i.s.max max for control landmark
#' @param h series or quantiles
#' @param mask this is corresponding control mask obatined from ctrl_vox_create
#' @importFrom stats quantile
#' @return A vector of landmarks to be used for Histogram Matching
#' @export
#'

get.landmarks <-function(data, i.min, i.max, i.s.min, i.s.max, h, mask) {
  scaled.data <- (data-quantile(data[data>0],i.s.min)+i.s.min)/quantile(data[data>0],i.s.max)*i.s.max
  return(quantile(scaled.data[mask>0], probs=h))
}
