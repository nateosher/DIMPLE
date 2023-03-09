#' Box plots of distances between cell type intensities `t1` and `t2` across
#' all slides in `mltplx_experiment`, grouped by patient.
#' @param mltplx_experiment `MltplxExperiment` object
#' @param t1 First cell type
#' @param t2 Second cell type
#' @param grouping_var Optional; string to indicate variable to use to further
#' group boxplots.
#' @param p_val_col can be either "p.adj" or "p.value" to show stars above boxplots that are significantly different than zero
#' @return ggplot2 plot
#' @importFrom magrittr `%>%`
#' @export
patient_boxplots <- function(mltplx_experiment,t1,t2,grouping_var="Group",p_val_col = "p.value") {
  stopifnot("Patient metadata must exist"=!is.null(mltplx_experiment$metadata))

  df <- mltplx_experiment %>%
    dist_to_df %>%
    filter(type1 == t1,
           type2 == t2)
  
  res <- df %>%
    group_by(patient_id) %>%
    group_modify(~{
      tryCatch({
        t.test(.x$dist)
      },error=\(e) NULL) %>%
        broom::tidy()
    }) %>%
    ungroup() %>%
    mutate(p.adj = p.adjust(p.value,method="fdr"))
  
  df %>%
    left_join(res,by="patient_id") %>%
    mutate(across(!!sym(grouping_var),factor) ) %>%
    {
      if(!is.null(grouping_var)) ggplot2::ggplot(.,aes(x=patient_id,y=dist,
                                              fill=!!sym(grouping_var)))
      else ggplot2::ggplot(.,aes(x=patient_id,y=dist))
    } +
    ggplot2::geom_boxplot() +
    ggplot2::geom_text(aes(label=ifelse(!!sym(p_val_col) < 0.05,"*","")), size = 20 / .pt, y = max(df$dist)) +
    anglex() +
    ggplot2::ylab("Distance") +
    ggplot2::ggtitle(paste0("Distance between ", t1,
                            " and ", t2, " in all patients"))
}
