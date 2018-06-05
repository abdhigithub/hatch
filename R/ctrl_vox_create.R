

#' Control Voxel Create
#' @description  This function creates a mask to obtain ideal tissue to create landmarks for histogram matching
#' @param threshold cutoff probabilty value to choose good quality tissues, default = 0.99
#' @param samples vector of size 3 determining the subsample sizes for White Matter, Grey Matter and CSF respectively, default =c(1500,1200,1000)
#' @param outfile outputs a mask of selected controls voxels to create intensity landmarks
#' @param wm_pve White Matter partial volume probabilty map or path
#' @param gm_pve Grey Matter partial volume probabilty map or path
#' @param csf_pve CSF partial volume probabilty map or path
#' @importFrom neurobase readnii niftiarr writenii
#'
#' @return mask image of voxels where total number of 1's is sum(samples)
#' @export
#'

###### Add example later
ctrl_vox_create <- function(wm_pve,gm_pve,csf_pve,threshold=0.99,samples= c(1500,1200,1000),outfile=NULL){
  ##### bunch of if statements to check the format of prob_maps
  # if(length(p_map_infile)==1){
  #   if(!dir.exists(p_map_infile)) stop("Directory of Probability Maps does not exist!")
  #   prob_find <- list.files(p_map_infile,pattern="pve",full.names = T)[1:3]
  #   prob_list <- sapply(prob_find,check_object)
  #   if(length(prob_find)==0) stop("Probability Maps with suffix pve do not exist. Please enter a list of 3 prob map nifti images or enter a vector of paths corresponding to WM,GM and CSF probability maps in that order!")
  # }


    prob_list <- lapply(list(wm_pve,gm_pve,csf_pve),check_object)


  #### prob_maps_infile is a list wm,gm,csf.. however the order does not matter


  wm_dat <- which(prob_list[[1]]>threshold,arr.ind = T)
  sample_wm <-sample(1:nrow(wm_dat),samples[1])

  gm_dat <- which(prob_list[[2]]>threshold,arr.ind = T)
  sample_gm <- sample(1:nrow(gm_dat),samples[2])

  csf_dat <- which(prob_list[[3]]>threshold,arr.ind = T)
  sample_csf <- sample(1:nrow(csf_dat),samples[3])

  create_use_mask <- neurobase::niftiarr(prob_list[[1]],0)
  create_use_mask[rbind(as.matrix(wm_dat[sample_wm,]),as.matrix(gm_dat[sample_gm,]),as.matrix(csf_dat[sample_csf,]))]<-1
  if(!is.null(outfile)){
    neurobase::writenii(create_use_mask,file=outfile)
    print(paste0("Control mask is created at ",outfile))
  }
  return(create_use_mask)

}
