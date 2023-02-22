#' Box plots of distances between cell type intensities `t1` and `t2` across
#' all slides in `mltplx_experiment`, grouped by patient.
#' @param mltplx_experiment `MltplxExperiment` object
#' @param t1 First cell type
#' @param t2 Second cell type
#' @param grouping_var Optional; string to indicate variable to use to further
#' group boxplots.
#' @param label_spots Whether or not boxplots should be labeled with slide ids
#' @return ggplot2 plot
#' @importFrom magrittr `%>%`
#' @export
patient_boxplots <- function(mltplx_experiment,t1,t2,grouping_var="Group",
                             label_spots=TRUE) {
  stopifnot("Patient metadata must exist"=!is.null(mltplx_experiment$metadata))

  mltplx_experiment %>%
    dist_to_df %>%
    filter(type1 == t1,
           type2 == t2) %>%
    mutate(across(!!sym(grouping_var),factor) )%>%
    {
      if(!is.null(grouping_var)) ggplot2::ggplot(.,aes(x=patient_id,y=dist,
                                              fill=!!sym(grouping_var)))
      else ggplot2::ggplot(.,aes(x=patient_id,y=dist))
    } +
    ggplot2::geom_boxplot() +
    {
      if(label_spots) ggplot2::geom_text(aes(label=slide_id),
                                position = position_jitter(seed = 1))
    } +
    anglex() +
    ggplot2::ylab("Distance") +
    ggplot2::ggtitle(paste0("Distance between ", t1,
                            " and ", t2, " in all patients"))
}
