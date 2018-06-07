


#' Histogram Matching Piecewise Algorithm
#'
#' @param rawdata The subject image to be matched
#' @param i.min min for subj landmark
#' @param i.max max for subj landmark
#' @param i.s.min min for control landmark
#' @param i.s.max max for control landmark
#' @param h series or quantiles
#' @param m vector of landmarks obtained from control mask
#' @param ctrl_mask Mask created by subsampling to obtain clean landmarks
#' @param rangemax default = 1, Image intensities will range between 0 and 255
#'
#' @return Returns Normalized image
#' @export
#'

do.hist.norm <-
  function(rawdata,
           i.min,
           i.max,
           i.s.min,
           i.s.max,
           h,
           m,
           ctrl_mask,rangemax=1) {
    brain_mask <- niftiarr(rawdata,0)
    brain_mask[rawdata> min(rawdata)]<- 1
    m.obs <- c(0,quantile(rawdata[ctrl_mask ==1 ], probs = c(i.min, h, i.max)),max(rawdata))
    m.withends <- c(i.s.min, m, i.s.max)
    transformed.data <- rawdata
    #transformed.data2 <- rawdata[ctrl_mask>0]

    for (hist.section.i in 1:(length(h)+3)) {
      which.data <-
        (rawdata[brain_mask > 0] < m.obs[hist.section.i+1]) &
        (rawdata[brain_mask > 0] >= m.obs[hist.section.i])

      transformed.data[brain_mask > 0][which.data] <-
        (rawdata[brain_mask > 0][which.data] - m.obs[hist.section.i]) /
        (m.obs[hist.section.i + 1] - m.obs[hist.section.i]) *
        (m.withends[hist.section.i + 1] - m.withends[hist.section.i]) +
        m.withends[hist.section.i]


    }


    transformed.data[transformed.data==max(transformed.data)] <- 1


    return(transformed.data*rangemax)
    #return(list(long=transformed.data,short=transformed.data2))
  }
