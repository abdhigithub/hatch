

#' hatch
#' @description Takes in subject input file and healthy control files to produce matched intensity histograms
#' @param infile Patient Image Nifti file  or path (T1, T2, T1gad or FLAIR)
#' @param ctrl_vox Control Mask created using healthy control of subsampled voxles
#' @param ctrl_img Healthy Control image coreesponding to the modality of the subject
#' @param i.min min for subj landmark
#' @param i.max max for subj landmark
#' @param i.s.min min for control landmark
#' @param i.s.max max for control landmark
#' @param h series or quantiles
#' @param wm_pve White Matter partial volume probabilty map or path
#' @param gm_pve Grey Matter partial volume probabilty map or path
#' @param csf_pve CSF partial volume probabilty map or path
#' @param rangemax Range of intensities are between 0 and rangemax default=255
#' @param threshold cutoff probabilty value to choose good quality tissues, default = 0.99
#' @param samples vector of size 3 determining the subsample sizes for White Matter, Grey Matter and CSF respectively, default =c(1500,1200,1000)
#' @param outfile path to save output matched class to a particular location
#' @importFrom neurobase readnii niftiarr writenii
#' @importFrom oro.nifti is.nifti
#' @return The histogram matched normalized image
#' @export
#'

hatch <- function(infile,wm_pve,gm_pve,csf_pve,ctrl_vox,ctrl_img,i.min =0.01, i.max=0.99,i.s.min=0,i.s.max =1,h = seq(0.1, 0.9, by = 0.1),rangemax=255,threshold=0.99,samples= c(1500,1200,1000),outfile=NULL){

  infile <- check_object(infile)
  ctrl_vox <- check_object(ctrl_vox)
  ctrl_img <- check_object(ctrl_img)


  #### for incoming subject create control mask
  subj_ctrl_vox <- ctrl_vox_create(wm_pve,gm_pve,csf_pve,threshold=threshold,samples=samples)

  #### Getting landmarks and Hist Match

  m=get.landmarks(rawdata=ctrl_img,i.min, i.max, i.s.min, i.s.max, c(0.01,h,0.99),mask=ctrl_vox)
  norm_img  <- do.hist.norm(rawdata=infile,i.min, i.max, i.s.min, i.s.max, h,m,ctrl_mask=  subj_ctrl_vox,rangemax=rangemax)

  if(!is.null(outfile)){
    writenii(norm_img, filename=outfile) }

  return(norm_img)

}








