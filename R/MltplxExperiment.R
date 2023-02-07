#' Creates a new `MltplxExperiment` object. This class represents a collection
#' of multiplex images, which may or may not be from distinct patients. In
#' addition to storing the images themselves, this class can store estimates
#' of the intensities of various point types, distances between those
#' intensities, and metadata related to the patients that the slides belong to.
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
#' per unique slide id, and a column `slide_id` indicating the slide to which
#' each row corresponds.
#' @return `MltplxExperiment` object.
#' @export
#' @importFrom tibble tibble
#' @importFrom magrittr `%>%`
#' @import dplyr
#' @import purrr
new_MltplxExperiment = function(x, y, marks, slide_id, ps = NULL, bw = NULL,
                                dist_metric = NULL, metadata = NULL){
  full_tib = tibble(
    x = x,
    y = y,
    marks = marks,
    slide_id = slide_id,
    slide_id_num = slide_id %>% factor() %>% as.numeric(),
    total_slides = max(slide_id_num)
  )

  if(!is.null(dist_metric))
    dist_metric_name = substitute(dist_metric) %>% as.character()
  else
    dist_metric_name = NULL

  mltplx_objects = full_tib %>%
    group_by(slide_id, slide_id_num, total_slides) %>%
    group_map(~{
      ProgressBar(.y$slide_id_num, .y$total_slides)
      new_MltplxObject(.x$x, .x$y, .x$marks, .y$slide_id, ps, bw,
                                    dist_metric,
                                    dist_metric_name)
    })

  ProgressBar(max(full_tib$total_slides) + 1, max(full_tib$total_slides))

  if(!is.null(metadata)) {
    check_metadata(mltplx_objects,metadata)
  }

  structure(
    list(
      mltplx_objects = mltplx_objects,
      ps = ps,
      bw = bw,
      dist_metric_name = dist_metric_name,
      metadata = metadata
    ),
    class = "MltplxExperiment"
  )
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
dist_to_df.MltplxExperiment <- function(mltplx_experiment) {
  if(is.null(mltplx_experiment$metadata))
    warning("you have not attached any metadata")
  map_df(mltplx_experiment$mltplx_objects,dist_to_df) %>%
    {
      if(!is.null(mltplx_experiment$metadata)) left_join(.,mltplx_experiment$metadata)
      if(!is.null(mltplx_experiment$metadata))
        left_join(.,mltplx_experiment$metadata)
      else
        .
    }
}

#' @export
add_QuantileDist.MltplxExperiment <- function(mltplx_experiment,
                                              dist_metric,
                                              mask_type,
                                              q_probs,
                                              .dist_metric_name = NULL,
                                              verbose = FALSE) {
  mltplx_experiment$mltplx_objects <- map(mltplx_experiment$mltplx_objects,
                                          add_QuantileDist,
                                          dist_metric,
                                          mask_type,
                                          q_probs,
                                          .dist_metric_name,
                                          verbose)

  mltplx_experiment
}
