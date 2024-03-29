#' Linear model of between-group differences in dist
#'
#' @param mltplx_experiment MltplxExperiment object
#' @param group_factor currently only supports binary group_factor. Must be a string for a column name in metadata.
#' @param agg_fun function to aggregate slide-level measurements to patient level. Must have option to remove NA rows via `na.rm = T`.
#' @param covariates vector of metadata columns to adjust for in linear model
#' @param slide_ids vector of slide_ids
#' @param types vector of types
#' @param adjust_counts whether or not to adjust for the total counts of each cell type in the linear model
#' @import dplyr
#' @return tibble containing all pairwise type comparisons between the groups in the grouping factor, along with inferential statistics
#' @export
lm_dist <- function(mltplx_experiment,
                    group_factor,
                    agg_fun = median,
                    covariates = NULL,
                    slide_ids = NULL,
                    types = NULL,
                    adjust_counts = TRUE
) {
  stopifnot("Patient metadata must exist"=!is.null(mltplx_experiment$metadata))
  stopifnot("Group factor must be in patient metadata"=group_factor %in% colnames(mltplx_experiment$metadata))
  # stopifnot("na.rm must be an option in agg_fun"="na.rm" %in% formalArgs(agg_fun))
  if(!is.null(covariates)) {
    stopifnot("Covariates must be in patient metadata"=all(covariates %in% colnames(mltplx_experiment$metadata)))
  }

  df <- mltplx_experiment %>%
    dist_to_df(reduce_symmetric = TRUE)
  
  if(is.null(slide_ids)) slide_ids <- unique(df$slide_id)
  if(is.null(types)) types <- unique(df$type1)

  fm_string <- paste0("dist ~ ", group_factor)
  if(!is.null(covariates)) fm_string <- paste0(fm_string, " + ",paste0(covariates,collapse = " + "))
  
  if(adjust_counts) {
    ct_counts <- cell_type_counts(mltplx_experiment)
    colnames(ct_counts) <- make.names(colnames(ct_counts))
    df <- df %>%
      left_join(ct_counts, by = "slide_id")
  }
  fm_immutable <- as.formula(fm_string)
  fm <- fm_immutable

  result <- df %>%
    filter(slide_id %in% slide_ids,
           type1 %in% types | type2 %in% types) %>% # from reduce_symmetric
    group_by(patient_id,type1,type2) %>%
    mutate(.,dist = agg_fun(dist,na.rm = T)) %>%
    distinct(type1,type2,dist,patient_id,.keep_all = T) %>%
    group_by(type1,type2) %>%
    group_modify(~{
      if(adjust_counts) {
        fm_string <- format(fm_immutable)
        fm_string <- paste0(fm_string," + ",make.names(.y$type1)," + ",make.names(.y$type2))
        fm <- as.formula(fm_string)
      }      
      tryCatch({
        lm(fm,data=.x)
      },error=\(e) {
        NULL
      }
      ) %>%
        broom::tidy()
    }) %>%
    ungroup() %>%
    filter(!stringr::str_detect(term,paste0(c("Intercept",covariates,make.names(levels(df$type1))),collapse = "|"))) %>%
    mutate(p.adj = p.adjust(p.value,method="fdr"))

  return(result)
}

#' Linear model of between-group differences in quantile dist at specific quantile
#'
#' @param mltplx_experiment MltplxExperiment object
#' @param group_factor currently only supports binary group_factor. Must be a string for a column name in metadata.
#' @param interval string, which interval to condition on
#' @param agg_fun function to aggregate slide-level measurements to patient level. Must have option to remove NA rows via `na.rm = T`.
#' @param covariates vector of metadata columns to adjust for in linear model
#' @param slide_ids vector of slide_ids
#' @param types vector of types
#' @param adjust_counts whether or not to adjust for the total counts of each cell type in the linear model
#' @import dplyr
#' @return tibble containing all pairwise type comparisons between the groups in the grouping factor, along with inferential statistics
#' @export
lm_qdist <- function(mltplx_experiment,
                    group_factor,
                    interval,
                    agg_fun = median,
                    covariates = NULL,
                    slide_ids = NULL,
                    types = NULL,
                    adjust_counts = TRUE
) {
  stopifnot("Patient metadata must exist"=!is.null(mltplx_experiment$metadata))
  stopifnot("Group factor must be in patient metadata"=group_factor %in% colnames(mltplx_experiment$metadata))
  stopifnot("Quantile dist must be created first"=all(unlist(lapply(mltplx_experiment$mltplx_objects,\(obj) !is.null(obj$quantile_dist)))))


  df <- mltplx_experiment %>%
    qdist_to_df(reduce_symmetric = TRUE) %>%
    rename(intervals = interval)
  stopifnot("Interval must be in q_probs"=(interval %in% levels(df$intervals)))


  if(is.null(slide_ids)) slide_ids <- unique(df$slide_id)
  if(is.null(types)) types <- unique(df$type1)

  fm_string <- paste0("qdist ~ ", group_factor)
  if(!is.null(covariates)) fm_string <- paste0(fm_string, " + ",paste0(covariates,collapse = " + "))
  fm <- as.formula(fm_string)
  fm_immutable <- as.formula(fm_string)
  fm <- fm_immutable
  
  if(adjust_counts) {
    ct_counts <- cell_type_counts(mltplx_experiment)
    colnames(ct_counts) <- make.names(colnames(ct_counts))
    df <- df %>%
      left_join(ct_counts, by = "slide_id")
  }

  result <- df %>%
    filter(slide_id %in% slide_ids,
           intervals == interval,
           type1 %in% types | type2 %in% types) %>% # from reduce_symmetric
    group_by(patient_id,type1,type2) %>%
    mutate(.,qdist = agg_fun(qdist,na.rm = T)) %>%
    distinct(type1,type2,qdist,patient_id,.keep_all = T) %>%
    group_by(type1,type2) %>%
    group_modify(~{
      if(adjust_counts) {
        fm_string <- format(fm_immutable)
        fm_string <- paste0(fm_string," + ",make.names(.y$type1)," + ",make.names(.y$type2))
        fm <- as.formula(fm_string)
      }      
      tryCatch({
        lm(fm,data=.x)
      },error=\(e) {
        NULL
      }
      ) %>%
        broom::tidy()
    }) %>%
    ungroup() %>%
    filter(!stringr::str_detect(term,paste0(c("Intercept",covariates,make.names(levels(df$type1))),collapse = "|"))) %>%
    mutate(p.adj = p.adjust(p.value,method="fdr"))
  

  return(result)
}
