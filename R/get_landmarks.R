#' Create Landmarks
#' @description This function creates landmark points for piecewise histogram matching
#'
#' @param rawdata
#' @param i.min
#' @param i.max
#' @param i.s.min
#' @param i.s.max
#' @param h
#' @param mask this is a test
#' @importFrom stats quantile
#' @return A vector of landmarks to be used for Histogram Matching
#' @export
#'

get.landmarks <- function(rawdata, i.min, i.max, i.s.min, i.s.max, h, mask) {
  scaled.data <- (rawdata-quantile(rawdata[mask>0],i.min)+i.s.min)/quantile(rawdata[mask>0],i.max)*i.s.max
  return(quantile(scaled.data[mask>0], probs=h))
}

