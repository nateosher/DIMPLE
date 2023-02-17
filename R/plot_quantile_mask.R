#' Plots quantile of intensities for selected mask cell type and ppp for selected slides of
#' `MltplxExperiment` object.
#' @param mltplx_experiment `MltplxExperiment` object
#' @param mask_type Name of cell type you want to plot quantiles of intensities
#' @param q_probs Data frame with columns "from" and "to" with quantile ranges
#' as percentage points, i.e. 25 for first quartile, 50 for median, etc. Note
#' that these ranges *can* overlap.
#' @param slide_ids Vector of slide ids for which you would like to plot
#' @return NULL
#' @importFrom magrittr `%>%`
#' @importFrom fuzzyjoin fuzzy_join
#' @import ggplot2
#' @export

plot_quantile_mask <- function(mltplx_experiment,mask_type,q_probs,slide_ids) {
  objs <- filter_mltplx_objects(mltplx_experiment,slide_ids)
  
  df <- purrr::map_df(1:length(objs),\(i) {
    obj <- objs[[i]]
    intensities<-obj$mltplx_intensity$intensities%>%
      as.data.frame()
    mask_intensities <- intensities %>% pull(!!sym(mask_type))
    q <- q_probs %>%
      pmap_dfr(\(from,to) {
        as.vector(quantile(mask_intensities,probs=c(from,to)/100)) -> x
        x <- c(x,from,to)
        names(x) <- c("q1","q2","p1","p2")
        x
      }) %>%
      mutate(q_fac = factor(1:nrow(.)))
    q$q2[length(q$q2)]<-(q$q2[length(q$q2)]+.Machine$double.eps)
    
    intensities %>%
      fuzzyjoin::fuzzy_join(q,
                            by = setNames(c("q1","q2"),c(mask_type,mask_type)),
                            match_fun = list(`>=`, `<`)) -> joined_q
    joined_q$slide_id<-obj$slide_id
    joined_q
  })
  
  
  for(id in slide_ids) {
    ppp<-objs[[which(sapply(objs, "[[", 1)==id)]]$mltplx_image$ppp
    d<-df %>%dplyr::filter(slide_id == id)
    ggplot(d,aes(X,Y)) +
      geom_tile(aes(fill=q_fac)) +
      scale_fill_brewer(palette="YlGnBu",name=paste0("Quantile of ",mask_type)) +
      geom_point(aes(X,Y,color=type,shape=type),data=cbind.data.frame(X=ppp$x,Y=ppp$y,type=ppp$marks))+
      scale_shape_manual(name = "type",
                         label = levels(ppp$marks),
                         values=1:nlevels(ppp$marks),drop=FALSE) +
      scale_color_manual(name = "type",
                         label = levels(ppp$marks),
                         values=as.vector(pals::polychrome()),drop=FALSE) +
      ggtitle(paste0("Quantile intensity plot for slide id ", id)) -> p
    print(p)
  }
}
