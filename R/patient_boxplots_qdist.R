#' Box plots of distances between cell type intensities `t1` and `t2` across
#' all slides in `mltplx_experiment`, grouped by patient.
#'
#' @param mltplx_experiment `MltplxExperiment` object
#' @param t1 First cell type
#' @param t2 Second cell type
#' @param grouping_var Optional; string to indicate variable to use to further
#' group boxplots.
#' @param p_val_col can be either "p.adj" or "p.value" to show stars above boxplots that are significantly different than zero
#' @param mu0 numeric, true value of the null hypothesis
#' @return ggplot2 plot
#' @importFrom magrittr `%>%`
#' @export
patient_boxplots_qdist <- function(mltplx_experiment,t1,t2,grouping_var="Group",p_val_col = "p.value",mu0=0) {
  if(is.null(mltplx_experiment$metadata))
    stop("Patient metadata must exist")
  if(is.null(mltplx_experiment$mltplx_objects[[1]]$quantile_dist))
    stop("Quantile distances must exist")
  if(!(grouping_var %in% colnames(mltplx_experiment$metadata)))
    stop("Patient metadata must contain variable")
  
  df <- mltplx_experiment %>%
    qdist_to_df(reduce_symmetric = F) %>%
    filter((type1 == t1 & type2 == t2) |
             (type1 == t2 & type2 == t1)) 
  
  res <- df %>%
    group_by(patient_id,interval) %>%
    group_modify(~{
      tryCatch({
        t.test(.x$qdist,mu=mu0)
      },error=\(e) NULL) %>%
        broom::tidy()
    }) %>%
    ungroup() %>%
    mutate(p.adj = p.adjust(p.value,method="fdr"))
  
  
  
  df <- df %>%
    left_join(res,by=c("patient_id","interval"))
  if(is.null(grouping_var)) {
    df$gp <- 1
  } else {
    df$gp <- df %>% pull(!!sym(grouping_var))
  }
  df %>%
    mutate(across(gp,factor) ) %>%
    mutate(gp_bin = as.numeric(gp)) %>%
    mutate(patient_id= reorder(patient_id,gp_bin)) %>%
    ggplot2::ggplot(.,aes(x=patient_id,y=qdist,fill=gp)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_text(aes(label=ifelse(!!sym(p_val_col) < 0.05,"*","")), size = 20 / .pt, y = max(df$qdist)) +
    anglex() +
    ggplot2::ylab("Distance") +
    ggplot2::ggtitle(paste0("Distance between ", t1,
                            " and ", t2, " in all patients")) +
    scale_fill_manual(values=cbfp) +
    labs(fill=grouping_var) +
    ggplot2::guides(color="none") +
    ggplot2::facet_wrap(interval~.,ncol=1)
}
