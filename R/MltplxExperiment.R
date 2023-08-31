#' Create a new MltplxExperiment object
#' 
#' This class represents a collection
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
#' @param windows Optional: list of `owin` objects representing the windows of each
#' biopsy. Windows will be assigned to slides in the order that they're passed
#' in accordance with how the slide IDs are ordered.
#' @return `MltplxExperiment` object.
#' @export
#' @importFrom tibble tibble
#' @importFrom magrittr `%>%`
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @examples
#' cell_x_values = runif(3000, 0, 600)
#' cell_y_values = runif(3000, 0, 600)
#' cell_marks = sample(c("Tumor", "Immune", "Other"), 3000, replace = TRUE)
#' slide_ids = rep(paste("Slide", 1:10), each = 300)
#' 
#' mltplx_exp = new_MltplxExperiment(x = cell_x_values,
#'                                   y = cell_y_values,
#'                                   marks = factor(cell_marks),
#'                                   slide_id = slide_ids)
#' print(mltplx_exp)
#' 
#' # Create an experiment with intensities
#' mltplx_exp_with_intensities = new_MltplxExperiment(x = cell_x_values,
#'                                   y = cell_y_values,
#'                                   marks = factor(cell_marks),
#'                                   slide_id = slide_ids,
#'                                   ps = 30, bw = 40)
#' print(mltplx_exp_with_intensities)
#' 
#' # Create an experiment with intensities and distance matrices
#' mltplx_exp_with_dist = new_MltplxExperiment(x = cell_x_values,
#'                                   y = cell_y_values,
#'                                   marks = factor(cell_marks),
#'                                   slide_id = slide_ids,
#'                                   ps = 30, bw = 40,
#'                                   dist_metric = cor)
#' print(mltplx_exp_with_dist)
#' 
#' # Create an experiment with intensities, distance matrices and patient metadata
#' set.seed(1234)
#' slides <- unique(slide_ids)
#' n_slides <- length(slides)
#' n_patients <- 4
#' n_groups <- 2
#' patient_ids <- rep_len(paste0("P",1:n_patients),n_slides)
#'
#' metadata <- tibble::tibble(slide_id=slides,
#'                    patient_id=patient_ids)
#'
#' groups <- sample(rep_len(sample(paste0("G",1:n_groups)),n_patients))
#'
#' ages <- runif(n_patients,min=0,max=100)
#'
#' gp_tb <- tibble::tibble(patient_id=unique(patient_ids),group=groups,age=ages)
#' metadata <- dplyr::left_join(metadata,gp_tb,by = "patient_id")
#' print(metadata)
#' mltplx_exp_with_pts = new_MltplxExperiment(x = cell_x_values,
#'                                   y = cell_y_values,
#'                                   marks = factor(cell_marks),
#'                                   slide_id = slide_ids,
#'                                   ps = 30, bw = 40,
#'                                   dist_metric = cor,
#'                                   metadata = metadata)
#' print(mltplx_exp_with_pts)
new_MltplxExperiment = function(x, y, marks, slide_id, window_sizes = NULL,
                                ps = NULL, bw = NULL,
                                dist_metric = NULL, metadata = NULL,
                                windows = NULL){

  mltplx_experiment_check_inputs(x, y, marks, slide_id, ps, bw, window_sizes,
                                 dist_metric, metadata, windows)

  full_tib = tibble(
    x = x,
    y = y,
    marks = marks,
    slide_id = slide_id
  )

  if(is.null(window_sizes) && is.null(windows)) {
    window_sizes <- full_tib %>%
      group_by(slide_id) %>%
      summarise(min_x = min(x),
                max_x = max(x),
                min_y = min(y),
                max_y = max(y))
  }else if(!is.null(windows)){
    window_sizes = tibble(
      slide_id = unique(slide_id),
      min_x = map_dbl(windows, ~ min(.x$x)),
      max_x = map_dbl(windows, ~ max(.x$x)),
      min_y = map_dbl(windows, ~ min(.x$y)),
      max_y = map_dbl(windows, ~ max(.x$y))
    )
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

  if(!is.null(windows) && length(dfs) != length(windows)){
    stop(paste0("length of `windows` argument must ",
                "be the same as the number of unique slide ids"))
  }

  if(is.null(windows))
    windows = rep(list(NULL), length(dfs))

  progressr::with_progress({
    prog <- progressr::progressor(steps = length(dfs))

    mltplx_objects <- furrr::future_map2(dfs, windows, \(df, window) {
        xrange <- attr(df,"xrange")
        yrange <- attr(df,"yrange")
        slide_id <- attr(df,"slide_id")
        obj <- new_MltplxObject(df$x,
                         df$y,
                         df$marks,
                         slide_id,
                         xrange = xrange,
                         yrange = yrange,
                         window = window,
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
                                          dist_metric, metadata, windows){

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

  if(!is.null(windows) && !is.null(window_sizes)){
    stop(paste0("pass either `windows` parameter or `window_sizes` ",
                "but not both"))
  }

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
    !(unique_classes[1] %in% c("MltplxObject", "ppp"))){
    stop(paste('to convert "list" object to "MltplxObject" object, all',
               'elements of list must either be of class "MltplxObject" or ',
               'ppp'))
  }

  if(unique_classes[1] == "ppp"){
    l = l %>%
      furrr::future_imap(\(p, i){
        new_MltplxObject(p$x, p$y, p$marks, paste("Slide", i),
                         window = p$window)
      })
  }

  # Check if list has smoothing/dist mats
  pixel_size = NULL
  unique_pixel_sizes = map_dbl(l, ~ ifelse(is.null(.x$mltplx_intensity$ps),
                                           NA,
                                           .x$mltplx_intensity$ps)) %>%
    unique()
  if(length(unique_pixel_sizes) > 1){
    warning(paste0("`MltplxObjects` have different pixel sizes; ",
                   "this information will be removed in the resulting ",
                   "`MltplxExperiment` object"))
  }else if(!is.na(unique_pixel_sizes[1])){
    pixel_size = unique_pixel_sizes[1]
  }

  bandwidth = NULL
  unique_bws = map_dbl(l, ~ ifelse(is.null(.x$mltplx_intensity$bw),
                                   NA,
                                   .x$mltplx_intensity$bw)) %>%
    unique()
  if(length(unique_bws) > 1){
    warning(paste0("`MltplxObjects` have different bandwidths; ",
                   "this information will be removed in the resulting ",
                   "`MltplxExperiment` object"))
  }else if(!is.na(unique_bws[1])){
    bandwidth = unique_bws[1]
  }

  dist_metric = NULL

  unique_dist_metrics = map_chr(l, ~ ifelse(is.null(.x$mltplx_dist$metric),
                                            NA,
                                            .x$mltplx_dist$metric)) %>%
    unique()
  if(length(unique_dist_metrics) > 1){
    warning(paste0("`MltplxObjects` have different distance metrics; ",
                   "this information will be removed in the resulting ",
                   "`MltplxExperiment` object"))

    l = map(l, \(mx_obj){
      mx_obj$mltplx_dist = NULL
      mx_obj
    })
  }else if(!is.na(unique_dist_metrics[1])){
    dist_metric = unique_dist_metrics[1]
  }

  # If the pixel sizes or bandwidths are different, need to nullify both
  if(any(is.null(c(pixel_size, bandwidth)))){
    pixel_size = NULL
    bandwidth = NULL

    l = map(l, \(mx_obj){
      mx_obj$mltplx_intensity = NULL
      mx_obj
    })
    # If same distance metric was used on different grid sizes/bandwidths,
    # need to nullify this as well
    if(!is.null(dist_metric)){
      warning(paste0("also removing distance matrices"))
      l = map(l, \(mx_obj){
        mx_obj$mltplx_dist = NULL
        mx_obj
      })
    }
  }


  structure(
    list(
      mltplx_objects = l,
      ps = pixel_size,
      bw = bandwidth,
      dist_metric_name = dist_metric,
      metadata = NULL,
      slide_ids = map_chr(l, ~ .x$slide_id)
    ),
    class = "MltplxExperiment"
  )
}

#' @export
`[[.MltplxExperiment` = function(mltplx_experiment, i){
  return(mltplx_experiment$mltplx_objects[[i]])
}

#' @export
`[.MltplxExperiment` = function(mltplx_experiment, i){
  return(mltplx_experiment$mltplx_objects[i])
}

#' Convert distance matrix to dataframe
#'
#' @param mltplx_experiment object of type `MltplxExperiment`
#' @param reduce_symmetric logical, whether to remove equivalent rows
#'
#' @return tibble with dist information
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

#' Convert quantile distance matrix to dataframe
#'
#' @param mltplx_experiment object of type `MltplxExperiment`
#' @param reduce_symmetric logical, whether to remove equivalent rows
#'
#' @return tibble with dist information
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

#' Update the quantile distance matrices generated for a `MltplxExperiment`
#' @param mltplx_experiment object of class `MltplxObject` to be updated
#' @param dist_metric distance metric to be used; can be any function that takes
#' in two vectors of the same length and produces a scalar
#' @param mask_type Name of cell type you want to make a QuantileDist of
#' @param q_probs Data frame with columns "from" and "to" with quantile ranges
#' as percentage points, i.e. 25 for first quartile, 50 for median, etc. Note
#' that these ranges *can* overlap.
#' @param .dist_metric_name The distance metric you want to use to compute
#' distances between distributions.
#' @return updated `MltplxExperiment` object
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

#' Get count of cell types in `MltplxExperiment`
#' @param mltplx_object `MltplxExperiment`
#' @return dataframe of cell type counts in boject
#' @export
cell_type_counts.MltplxExperiment <- function(mltplx_experiment) {
  map_df(mltplx_experiment$mltplx_objects,cell_type_counts)
}


