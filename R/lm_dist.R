#' Linear model of between-group differences in dist
#'
#' @param mltplx_experiment MltplxExperiment object
#' @param group_factor currently only supports binary group_factor. Must be a string for a column name in metadata.
#' @param slide_ids vector of slide_ids
#' @param types vector of types
#'
#' @return tibble containing all pairwise type comparisons between the groups in the grouping factor, along with inferential statistics
#' @export
lm_dist <- function(mltplx_experiment,
                    group_factor,
                    slide_ids = NULL,
                    types = NULL
) {
  stopifnot("Patient metadata must exist"=!is.null(mltplx_experiment$metadata))
  stopifnot("Group factor must be in patient metadata"=group_factor %in% colnames(mltplx_experiment$metadata))
  
  df <- mltplx_experiment %>%
    dist_to_df(reduce_symmetric = TRUE)
  if(is.null(slide_ids)) slide_ids <- unique(df$slide_id)
  if(is.null(types)) types <- unique(df$type1)
  
  df <- df %>%
    filter(slide_id %in% slide_ids,
           type1 %in% types,
           type2 %in% types)
  
  fm <- as.formula(paste0("dist ~ ", group_factor))

  result <- df %>%
    group_by(type1,type2) %>%
    group_modify(~{
      tryCatch({
        lm(fm,data=.x)
      },error=\(e)
        NULL
      ) %>%
        broom::tidy()
    }) %>%
    ungroup() %>%
    filter(!stringr::str_detect(term,"Intercept")) %>%
    mutate(p.adj = p.adjust(p.value,method="fdr"))
  
  return(result)
  
}