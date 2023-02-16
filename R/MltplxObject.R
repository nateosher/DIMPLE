#' Creates new `MltplxObject` object.
#' @param x Vector of x coordinates of cells
#' @param y Vector of y coordinates of cells
#' @param marks Vector of cell types
#' @param slide_id Vector of slide ids, i.e. how should cells be grouped
#' @param ps Optional; if you'd like to generate intensity estimates when
#' you first create these objects, this will determine the "pixel size," i.e.
#' the side-length of the squares the domain will be broken up into in order
#' for the estimation to occur. Required if `bw` is passed (and vice versa).
#' Intensities can also be generated after this object is created.
#' @param bw Optional; if you'd like to generate intensity estimates when
#' you first create these objects, this will determine the "bandwidth" of the
#' smoothing. Larger values of `bw` result in more smoothing; this is not
#' necessarily a good thing. Required if `ps` is passed (and vice versa).
#' Intensities can also be generated after this object is created.
#' @param dist_metric Optional; if you'd like to generate distance matrices
#' between different cell type intensities, this will be the distance metric
#' used to do so. Distance matrices can also be generated after this object
#' is created.
#' @param .dist_metric_name Optional; not required, even when a distance
#' matrix is passed. However, if you'd like to name the distance function
#' you use something specific, you can pass this name as a string using
#' this parameter. Otherwise, it defaults to the name of the function
#' @return `MltplxObject` object
#' @export
new_MltplxObject = function(x, y, marks, slide_id, ps = NULL, bw = NULL,
                            dist_metric = NULL, .dist_metric_name = NULL){

  if(!is.null(.dist_metric_name)){
    dist_metric_name = .dist_metric_name
  }else{
    dist_metric_name = substitute(dist_metric) %>% as.character()
  }

  # Make image
  mltplx_image = new_MltplxImage(x, y, marks)


  # Make intensities, if applicable
  if(!is.null(ps) && !is.null(bw)){
    mltplx_intensity = new_MltplxIntensity(mltplx_image, ps, bw)
  }else{
    mltplx_intensity = NULL
  }

  # Make distance matrices, if applicable
  if(!is.null(dist_metric)){
    mltplx_dist = new_MltplxDist(mltplx_intensity, dist_metric,
                                 dist_metric_name)
  }else{
    mltplx_dist = NULL
  }

  structure(
    list(
      slide_id = slide_id[1],
      mltplx_image = mltplx_image,
      mltplx_intensity = mltplx_intensity,
      mltplx_dist = mltplx_dist
    ),
    class = "MltplxObject"
  )
}

#' @export
print.MltplxObject = function(mltplx_object, ...){
  cat("MltplxObject \n")
  cat("Slide id:", mltplx_object$slide_id, "\n")
  cat("Image with", mltplx_object$mltplx_image$ppp$n, "cells across",
      length(mltplx_object$mltplx_image$cell_types), "cell types\n")
  cell_type_str = paste0(mltplx_object$mltplx_image$cell_types, collapse = ", ")
  cat("Cell types:", cell_type_str, "\n")

  if(!is.null(mltplx_object$mltplx_intensity)){
    cat("Intensity generated with pixel size",
        mltplx_object$mltplx_intensity$ps,
        "and bandwidth", mltplx_object$mltplx_intensity$bw, "\n")
  }else{
    cat("No intensity generated (yet)\n")
  }
  if(!is.null(mltplx_object$mltplx_dist)){
    cat("Distance matrix generated using", mltplx_object$mltplx_dist$metric,
        "\n")
  }else{
    cat("No distance matrix generated (yet)\n")
  }
  cat(length(mltplx_object$quantile_dist),"quantile distance arrays generated.",
      "\n")
}

#' @export
update_object_intensity = function(mltplx_object, ps, bw){
  mltplx_object$mltplx_intensity = new_MltplxIntensity(
    mltplx_object$mltplx_image, ps, bw
  )
  return(mltplx_object)
}

update_object_dist = function(mltplx_object, dist_metric,
                              .dist_metric_name = NULL){
  if(is.null(mltplx_object$mltplx_intensity))
    stop(paste("you have to generate intensities before",
                "distance matrices- see `update_intensity` function",
                "for details on how to do this"))

  if(is.null(.dist_metric_name)){
    .dist_metric_name = substitute(dist_metric) %>% as.character()
  }

  mltplx_object$mltplx_dist = new_MltplxDist(
    mltplx_object$mltplx_intensity, dist_metric,
    .dist_metric_name
  )
  return(mltplx_object)
}


#' Title
#'
#' @param mltplx_object
#' @param reduce_symmetric logical, whether to remove equivalent rows
#'
#' @return tibble with dist information
#' @export
dist_to_df.MltplxObject <- function(mltplx_object,reduce_symmetric = FALSE) {
  if(!is.null(mltplx_object$mltplx_dist)) {
    mat <- mltplx_object$mltplx_dist$dist
    
    if(reduce_symmetric) {
      mat[lower.tri(mat)] <- NA
    }

   df <- mat %>%
    as.data.frame.table() %>%
    rename(type1=Var1,
           type2=Var2,
           dist=Freq) %>%
    tidyr::drop_na(dist) %>%
    mutate(slide_id=mltplx_object$slide_id)

    return(df)
  } else {
    cat(paste0("Multiplex object corresponding to slide id ", mltplx_object$slide_id," does not contain a distance matrix."))
  }
}

#' Quantile dist to df
#'
#' @param mltplx_object
#' @param reduce_symmetric logical, whether to remove equivalent rows
#'
#' @return tibble with dist information
#' @export
qdist_to_df.MltplxObject <- function(mltplx_object,reduce_symmetric = FALSE) {

  has_quantile_dist = !is.null(mltplx_object$quantile_dist) &&
                        (length(mltplx_object$quantile_dist) > 1 ||
                         !is.na(mltplx_object$quantile_dist))
  if(has_quantile_dist) {
    arr <- mltplx_object$quantile_dist$quantile_dist_array

    nms3 <- mltplx_object$quantile_dist$quantiles %>%
      tidyr::unite("p1_p2",p1,p2,sep="-") %>%
      pull(p1_p2)

    dimnames(arr)[[3]] <- nms3

    df <- arr %>%
      as.data.frame.table() %>%
      rename(type1=Var1,
             type2=Var2,
             interval=Var3,
             dist=Freq) %>%
      mutate(slide_id=mltplx_object$slide_id) %>%
      as_tibble()

    if(reduce_symmetric) {
      df %>%
        select(type1,type2,slide_id,interval) %>%
        apply(.,1,sort) %>%
        t(.) %>%
        duplicated(.) -> dup_ix

      df <- df[dup_ix, ]
    }
    df
  } else {
    warning(paste0("Multiplex object corresponding to slide id ", mltplx_object$slide_id," does not contain a quantile distance array."))
    tibble::tibble()
  }
}

#' @export
add_QuantileDist.MltplxObject <- function(mltplx_object,
                                          dist_metric,
                                          mask_type,
                                          q_probs,
                                          .dist_metric_name = NULL) {

  if(!(mask_type %in% colnames(mltplx_object$mltplx_intensity$intensities))){
    warning(paste0("cell type ", mask_type, " not present in slide with id ",
                  mltplx_object$slide_id, "; returning NA"))
    mltplx_object$quantile_dist = NA
    return(mltplx_object)
  }

  if(!is.null(.dist_metric_name)){
    dist_metric_name = .dist_metric_name
  }else{
    dist_metric_name = substitute(dist_metric) %>% as.character()
  }

  q_dist <- mltplx_object$quantile_dist <- new_QuantileDist(mltplx_object$mltplx_intensity,
                                                       dist_metric=dist_metric,
                                                       mask_type=mask_type,
                                                       q_probs=q_probs,
                                                       dist_metric_name=dist_metric_name)
  if(length(mltplx_object$quantile_dist) == 0) {
    mltplx_object$quantile_dist <- list(q_dist)
  } else {
    mltplx_object$quantile_dist <- append(mltplx_object$quantile_dist,q_dist)
  }
  mltplx_object
}
