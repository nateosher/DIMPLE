#' Creates a new `MltplxExperiment` object. This class represents a collection
#' of multiplex images, which may or may not be from distinct patients. In
#' addition to storing the images themselves, this class can store estimates
#' of the intensities of various point types, distances between those
#' intensities, and metadata related to the patients that the slides belong to.
#'
#' @param x Vector of x coordinates of cells across all slides
#' @param y Vector of y coordinates of cells across all slides
#' @param marks Vector of cell types for each cell across all slides
#' @param slide_id Vector of ids indicating the slide to which each cell
#' belongs
#' @param ps Optional: "pixel size" of intensity estimations. If you'd
#' like to generate intensities while constructing the `MltplxExperiment`
#' object, it will be broken up into squares that are roughly `ps` by `ps`
#' units.
#' @param bw Required if `ps` is passed. This determines the bandwidth of the
#' smoothing of the values assigned to each square of the intensity
#' estimations. Larger values result in "smoother" intensities, while smaller
#' values result in "coarser" estimations.
#' @param dist_metric Optional: a function that computes the distance between
#' two intensity vectors. This can be any function that takes in two vectors
#' and returns a scalar.
#' @param metadata Optional: patient/slide level data. Must include *one* row
#' per unique slide id, and a column `slide_id` indicating the slide to which each row corresponds
#' @param window_sizes dataframe with columns `slide_id`, `min_x`, `max_x`, `min_y` and `max_y` indicating the window dimensions for each slide
#' @return `MltplxExperiment` object.
#' @export
#' @importFrom tibble tibble
#' @importFrom magrittr `%>%`
#' @import dplyr
#' @import purrr
new_MltplxExperiment = function(x, y, marks, slide_id, window_sizes = NULL,
                                ps = NULL, bw = NULL,
                                dist_metric = NULL, metadata = NULL){

  mltplx_experiment_check_inputs(x, y, marks, slide_id, ps, bw,window_sizes,
                                 dist_metric, metadata)

  full_tib = tibble(
    x = x,
    y = y,
    marks = marks,
    slide_id = slide_id
  )
  
  if(is.null(window_sizes)) {
    window_sizes <- full_tib %>%
      group_by(slide_id) %>%
      summarise(min_x = min(x),
                max_x = max(x),
                min_y = min(y),
                max_y = max(y))
  }
  
  full_tib <- left_join(full_tib,window_sizes,by = "slide_id")

  dist_metric_name = substitute(dist_metric) %>% as.character()
  if(length(dist_metric_name) == 0)
    dist_metric_name = NULL

  dfs <- full_tib %>%
    group_by(slide_id) %>%
    group_split()
  
  dfs <- lapply(dfs,\(df) {
    attr(df,"slide_id") <- unique(df$slide_id)
    attr(df,"xrange") <- c(unique(df$min_x),unique(df$max_x))
    attr(df,"yrange") <- c(unique(df$min_y),unique(df$max_y))
    df <- df %>%
      select(x,y,marks)
  })
  
  progressr::with_progress({
    prog <- progressr::progressor(steps = length(dfs))
    
    mltplx_objects <- furrr::future_map(dfs,\(df) {
        xrange <- attr(df,"xrange")
        yrange <- attr(df,"yrange")
        slide_id <- attr(df,"slide_id")
        obj <- new_MltplxObject(df$x,
                         df$y,
                         df$marks,
                         slide_id,
                         xrange = xrange,
                         yrange = yrange,
                         ps = ps,
                         bw = bw,
                         dist_metric = dist_metric,
                         .dist_metric_name = dist_metric_name)
        prog()
        
        return(obj)
      })
  })
  
  ids <- unlist(lapply(mltplx_objects,\(obj) obj$slide_id))
  ids_orig_order <- unique(slide_id)
  mltplx_objects <- mltplx_objects[order(match(ids,ids_orig_order))]

  if(!is.null(metadata)) {
    check_metadata(mltplx_objects,metadata)
  }

  slide_ids <- unlist(lapply(mltplx_objects,\(obj) obj$slide_id))
  structure(
    list(
      mltplx_objects = mltplx_objects,
      ps = ps,
      bw = bw,
      dist_metric_name = dist_metric_name,
      metadata = metadata,
      slide_ids = slide_ids
    ),
    class = "MltplxExperiment"
  )
}

mltplx_experiment_check_inputs = function(x, y, marks, slide_id, ps, bw,window_sizes,
                                          dist_metric, metadata){

  if(length(x) != length(y))
    stop("`x` must be the same length as `y`")

  if(length(marks) != length(x))
    stop("`marks` must be the same length as `x` and `y`")

  if(length(slide_id) != length(x))
    stop("`slide_id` must be the same length as `x` and `y`")
  
  if(!is.null(window_sizes)) stopifnot("window_sizes does not contain correct columns!"=all(c("slide_id","min_x","max_x","min_y","max_y") %in% colnames(window_sizes)))

  # TODO: typechecks
  # TODO: refactor into sub-functions that can be called when other
  #       components are added

  if((is.null(ps) && !is.null(bw)) || (!is.null(ps) && is.null(bw)))
    stop("`ps` and `bw` must both be provided to compute intensities")

}

#' @export
print.MltplxExperiment = function(mltplx_experiment, ...){
  cat("MltplxExperiment with", length(mltplx_experiment$mltplx_objects),
      "slides\n")
  if(!is.null(mltplx_experiment$ps) && !is.null(mltplx_experiment$bw)){
    cat("Intensities generated with pixel size",
        mltplx_experiment$ps, "and bandwidth",
        mltplx_experiment$bw, "\n")
  }else{
    cat("No intensities generated\n")
  }

  if(!is.null(mltplx_experiment$dist_metric_name)){
    cat("Distance matrices generated with",
        mltplx_experiment$dist_metric_name, "\n")
  }else{
    cat("No distance matrices generated\n")
  }

  if(!is.null(mltplx_experiment$metadata)){
    cat("Metadata has", ncol(mltplx_experiment$metadata), "columns\n")
  }else{
    cat("No attached metadata")
  }
  cat("\n\n")
}

#' @export
`[[.MltplxExperiment` = function(mltplx_experiment, i){
  return(mltplx_experiment$mltplx_objects[[i]])
}

#' @export
`[.MltplxExperiment` = function(mltplx_experiment, i){
  return(mltplx_experiment$mltplx_objects[i])
}

#' @export
dist_to_df.MltplxExperiment <- function(mltplx_experiment,reduce_symmetric = FALSE) {
  if(is.null(mltplx_experiment$metadata))
    warning("you have not attached any metadata")

  map_df(mltplx_experiment$mltplx_objects, dist_to_df, reduce_symmetric) %>%
    {
      if(!is.null(mltplx_experiment$metadata))
        left_join(.,mltplx_experiment$metadata,by="slide_id")
      else
        .
    } %>%
    mutate(type1 = factor(type1,levels = sort(levels(.$type1))),
           type2 = factor(type2,levels = sort(levels(.$type2))))
}

#' @export
qdist_to_df.MltplxExperiment <- function(mltplx_experiment,reduce_symmetric = FALSE) {
  if(is.null(mltplx_experiment$metadata))
    warning("you have not attached any metadata")
  map_df(mltplx_experiment$mltplx_objects, qdist_to_df, reduce_symmetric) %>%
    {
      if(!is.null(mltplx_experiment$metadata))
        left_join(.,mltplx_experiment$metadata,by="slide_id")
      else
        .
    } %>%
    mutate(type1 = factor(type1,levels = sort(levels(.$type1))),
           type2 = factor(type2,levels = sort(levels(.$type2))))
}

#' @export
add_QuantileDist.MltplxExperiment <- function(mltplx_experiment,
                                              dist_metric,
                                              mask_type,
                                              q_probs,
                                              .dist_metric_name = NULL) {
  slide_ids <- mltplx_experiment$slide_ids
  n_slides <- length(slide_ids)
  mltplx_experiment$mltplx_objects <- map(mltplx_experiment$mltplx_objects,
                                          \(obj,...) {
                                            ProgressBar(which(obj$slide_id == slide_ids), n_slides)
                                            add_QuantileDist(obj,...)
                                            },
                                          dist_metric,
                                          mask_type,
                                          q_probs,
                                          .dist_metric_name)
  ProgressBar(n_slides + 1, n_slides)

  mltplx_experiment
}
