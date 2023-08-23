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
#' @import tidyr
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
    cat("No attached metadata\n")
  }

  if(!is.null(mltplx_experiment$qdist_mask)){
    cat(mltplx_experiment$qdist_n_quantiles,
        "quantile distance arrays generated for mask",
        mltplx_experiment$qdist_mask,
        "\n")
  }
}

#' @export
as_MltplxExperiment.list = function(l, ...){
  list_classes = map(l, ~ class(.x))
  n_classes = map_dbl(list_classes, ~ length(.x))
  unique_classes = unlist(list_classes) %>% unique()
  if(any(n_classes > 1) ||
    length(unique_classes) > 1 ||
    unique_classes[1] != "MltplxObject"){
    stop(paste('to convert "list" object to "MltplxObject" object, all',
               'elements of list must be of class "MltplxObject"'))
  }

  x = map(l, ~ .x$mltplx_image$ppp$x) %>% unlist()
  y = map(l, ~ .x$mltplx_image$ppp$y) %>% unlist()
  marks = map(l, ~ .x$mltplx_image$ppp$marks) %>% unlist()
  slide_ids = map(l, ~ rep(.x$slide_id, .x$mltplx_image$ppp$n)) %>% unlist()
  window_sizes = tibble(
    slide_id = map_chr(l, ~ .x$slide_id),
    min_x = map_dbl(l, ~ .x$mltplx_image$ppp$window$xrange[1]),
    max_x = map_dbl(l, ~ .x$mltplx_image$ppp$window$xrange[2]),
    min_y = map_dbl(l, ~ .x$mltplx_image$ppp$window$yrange[1]),
    max_y = map_dbl(l, ~ .x$mltplx_image$ppp$window$yrange[2])
  )
  new_MltplxExperiment(x = x, y = y, marks = marks, slide_id = slide_ids,
                       window_sizes = window_sizes)
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
as_tibble.MltplxExperiment <- function(mltplx_experiment) {
  if(is.null(mltplx_experiment$dist_metric_name) &&
     is.null(mltplx_experiment$metadata)){
    stop("no metadata attached and no distance matrices generated")
  }else if(is.null(mltplx_experiment$dist_metric_name)){
    warning("no distance matrices generated; returning metadata")
    mltplx_experiment$metadata
  }else{
    dist_to_df(mltplx_experiment, reduce_symmetric = TRUE) %>%
      as_tibble() %>%
      mutate(
        cell_types = map2_chr(type1, type2, ~ paste0(.x, "~", .y))
      ) %>%
      select(-type1, -type2) %>%
      pivot_wider(names_from = "cell_types", values_from = "dist")
  }
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
update_qdist.MltplxExperiment <- function(mltplx_experiment,
                                              dist_metric,
                                              mask_type,
                                              q_probs,
                                              .dist_metric_name = NULL) {
  slide_ids <- mltplx_experiment$slide_ids
  n_slides <- length(slide_ids)

  mltplx_objects <- mltplx_experiment$mltplx_objects

  progressr::with_progress({
    prog <- progressr::progressor(steps = n_slides)
    mltplx_objects <- furrr::future_map(mltplx_objects, \(obj) {
                                              obj <- update_qdist(obj,
                                                               dist_metric,
                                                               mask_type,
                                                               q_probs,
                                                               .dist_metric_name)
                                              prog()
                                              return(obj)
                                              })
  })

  mltplx_experiment$mltplx_objects <- mltplx_objects
  mltplx_experiment$qdist_mask <- mask_type
  mltplx_experiment$qdist_n_quantiles = nrow(q_probs)

  mltplx_experiment
}

#'@export
cell_type_counts.MltplxExperiment <- function(mltplx_experiment) {
  map_df(mltplx_experiment$mltplx_objects,cell_type_counts)
}


