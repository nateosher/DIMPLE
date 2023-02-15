#' Linear model of between-group differences in dist
#'
#' @param mltplx_experiment MltplxExperiment object
#' @param group_factor currently only supports binary group_factor. Must be a string for a column name in metadata.
#' @param agg_fun function to aggregate slide-level measurements to patient level. Must have option to remove NA rows via `na.rm = T`.
#' @param covariates vector of metadata columns to adjust for in linear model
#' @param slide_ids vector of slide_ids
#' @param types vector of types
#' @import dplyr
#' @return tibble containing all pairwise type comparisons between the groups in the grouping factor, along with inferential statistics
#' @export
lm_dist <- function(mltplx_experiment,
                    group_factor,
                    agg_fun = median,
                    covariates = NULL,
                    slide_ids = NULL,
                    types = NULL
) {
  stopifnot("Patient metadata must exist"=!is.null(mltplx_experiment$metadata))
  stopifnot("Group factor must be in patient metadata"=group_factor %in% colnames(mltplx_experiment$metadata))
  
  df <- mltplx_experiment %>%
    dist_to_df(reduce_symmetric = TRUE)
  
  if(is.null(slide_ids)) slide_ids <- unique(df$slide_id)
  if(is.null(types)) types <- unique(df$type1)
  
  fm_string <- paste0("dist ~ ", group_factor)
  if(!is.null(covariates)) fm_string <- paste0(fm_string, " + ",paste0(covariates,collapse = " + "))
  fm <- as.formula(fm_string)
  
  result <- df %>%
    filter(slide_id %in% slide_ids,
           type1 %in% types,
           type2 %in% types) %>%
    group_by(patient_id,type1,type2) %>%
    mutate(.,dist = agg_fun(dist,na.rm = T)) %>%
    distinct(type1,type2,dist,patient_id,.keep_all = T) %>%
    group_by(type1,type2) %>%
    group_modify(~{
      tryCatch({
        lm(fm,data=.x)
      },error=\(e) {
        NULL
      }
      ) %>%
        broom::tidy()
    }) %>%
    ungroup() %>%
    filter(!stringr::str_detect(term,paste0(c("Intercept",covariates),collapse = "|"))) %>%
    mutate(p.adj = p.adjust(p.value,method="fdr"))
  
  return(result)
}
