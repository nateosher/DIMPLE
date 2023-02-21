jsd_unnormalized <- function(px,py,base=exp(1)){
  if (any(px <= 0))
    px <- exp(px)
  if (any(py <= 0))
    py <- exp(py)
  px[which(px < .Machine$double.xmin)] <- .Machine$double.xmin
  py[which(py < .Machine$double.xmin)] <- .Machine$double.xmin
  pmean = 1/2*(px + py)
  KLD.px.pmean <- px * (log(px, base = base) - log(pmean, base = base))
  KLD.py.pmean <- py * (log(py, base = base) - log(pmean, base = base))
  JSD = sqrt(sum(KLD.px.pmean) + sum(KLD.py.pmean))
  return(JSD)
}

jensen_shannon_dist <- function(px,py){
  px = px/sum(px+ .Machine$double.xmin)
  py = py/sum(py+ .Machine$double.xmin)
  px[which(px < .Machine$double.xmin)] <- .Machine$double.xmin
  py[which(py < .Machine$double.xmin)] <- .Machine$double.xmin
  pmean = 1/2*(px + py)
  JSD = sqrt(LaplacesDemon::KLD(px, pmean)$sum.KLD.px.py + LaplacesDemon::KLD(py, pmean)$sum.KLD.px.py)
  return(JSD)
}

KL_div <- function(px,py){
  px = px/(sum(px) + .Machine$double.eps)
  py = py/(sum(py) + .Machine$double.eps)
  px[which(px < .Machine$double.xmin)] <- .Machine$double.xmin
  py[which(py < .Machine$double.xmin)] <- .Machine$double.xmin
  KLD.res = LaplacesDemon::KLD(px, py)$sum.KLD.px.py
  
  return(KLD.res)
}
# 
# plot_dist <- function(mltplx_experiment, slide_ids, mode = "heatmap",plot_by_quantile=F) {
#   if(plot_by_quantile==F){
#     if(mode == "heatmap") {
#       df <- mltplx_experiment %>%
#         dist_to_df() %>%
#         tidyr::drop_na(dist)
#       
#       for(id in slide_ids) {
#         p <- df %>%
#           dplyr::filter(slide_id == id) %>%
#           ggplot(aes(type1,type2,fill=dist)) +
#           geom_tile() +
#           anglex() +
#           scale_fill_gradient2() +
#           ggtitle(paste0("Distance matrix for slide id ", id))
#         print(p)
#       }
#     } else if(mode == "network") {
#       filtered_exp <- filter_mltplx_objects(mltplx_experiment,slide_ids)
#       for(mltplx_object in filtered_exp) {
#         qgraph::qgraph(mltplx_object$mltplx_dist$dist,layout = "circle",threshold=0.1)
#       }
#     } else {
#       stop("Mode must be either heatmap or network")
#     } 
#   }else{
#     if(mode == "heatmap") {
# 
#       df <- qdist_to_df(mltplx_experiment) %>%
#         tidyr::drop_na(dist)
#       
#       for(id in slide_ids) {
#         p <- df %>%
#           dplyr::filter(slide_id == id) %>%
#           ggplot(aes(type1,type2,fill=dist)) +
#           geom_tile() +
#           anglex() +
#           scale_fill_gradient2() + facet_wrap(interval~.)+
#         ggtitle(paste0("Distance matrix for slide id ", id))
#         print(p)
#       }
#     } else if(mode == "network") {
#       stop("Mode not yet supported for network with quantiles")
#     } else {
#       stop("Mode must be either heatmap or network")
#     } 
#   }
#   
# }
# 
# plot_qdist<-function(mltplx_experiment, slide_ids, mode = "heatmap"){
#   if(mode == "heatmap") {
#     
#     df <- qdist_to_df(mltplx_experiment) %>%
#       tidyr::drop_na(dist)
#     
#     for(id in slide_ids) {
#       p <- df %>%
#         dplyr::filter(slide_id == id) %>%
#         ggplot(aes(type1,type2,fill=dist)) +
#         geom_tile() +
#         anglex() +
#         scale_fill_gradient2() + facet_wrap(interval~.)+
#         ggtitle(paste0("Distance matrix for slide id ", id))
#       print(p)
#     }
#   } else if(mode == "network") {
#     filtered_exp <- filter_mltplx_objects(mltplx_experiment,slide_ids)
#     for(mltplx_object in filtered_exp) {
#       for(i in 1:length(mltplx_object$quantile_dist$quantiles$q_fac))
#       arr<-mltplx_object$quantile_dist$quantile_dist_array
#       block.diag<-do.call("adiag", lapply(seq(dim(arr)[3]), function(x) arr[ , , x]))
#       intervals<-qdist_to_df(mltplx_object)%>%distinct(type1,interval)
#       qgraph::qgraph(block.diag,threshold=0.1,layout="groups",groups=as.factor(intervals$interval),title=paste0("Network for slide id ", mltplx_object$slide_id))
# 
#     }
#   } else {
#     stop("Mode must be either heatmap or network")
#   } 
# }
# 
# library(magic)
# plot_qintensities <- function(mltplx_experiment,mask_type,q_probs,slide_ids) {
#   objs <- filter_mltplx_objects(mltplx_experiment,slide_ids)
#   
#   df <- purrr::map_df(1:length(objs),\(i) {
#     obj <- objs[[i]]
#     intensities<-obj$mltplx_intensity$intensities%>%
#       as.data.frame()
#     mask_intensities <- intensities %>% pull(!!sym(mask_type))
#     q <- q_probs %>%
#       pmap_dfr(\(from,to) {
#         as.vector(quantile(mask_intensities,probs=c(from,to)/100)) -> x
#         x <- c(x,from,to)
#         names(x) <- c("q1","q2","p1","p2")
#         x
#       }) %>%
#       mutate(q_fac = factor(1:nrow(.)))
#     q$q2[length(q$q2)]<-(q$q2[length(q$q2)]+.Machine$double.eps)
#     
#     intensities %>%
#       fuzzyjoin::fuzzy_join(q,
#                             by = setNames(c("q1","q2"),c(mask_type,mask_type)),
#                             match_fun = list(`>=`, `<`)) -> joined_q
#     joined_q$slide_id<-obj$slide_id
#     joined_q
#   })
#   
#   
#   for(id in slide_ids) {
#     ppp<-objs[[which(sapply(objs, "[[", 1)==id)]]$mltplx_image$ppp
#     d<-df %>%dplyr::filter(slide_id == id)
#       ggplot(d,aes(X,Y)) +
#       geom_tile(aes(fill=q_fac)) +
#       scale_fill_brewer(palette="YlGnBu",name=paste0("Quantile of ",mask_type)) +
#       geom_point(aes(X,Y,color=type,shape=type),data=cbind.data.frame(X=ppp$x,Y=ppp$y,type=ppp$marks))+
#         scale_shape_manual(name = "type",
#                            label = levels(ppp$marks),
#                            values=1:nlevels(ppp$marks),drop=FALSE) +
#         scale_color_manual(name = "type",
#                            label = levels(ppp$marks),
#                            values=as.vector(pals::polychrome()),drop=FALSE) +
#       ggtitle(paste0("Quantile intensity plot for slide id ", id)) -> p
#     print(p)
#   }
# }
library(survival)
surv_dist <- function(mltplx_experiment,
                    surv_time, surv_event,
                    agg_fun = median,
                    covariates = NULL,
                    slide_ids = NULL,
                    types = NULL
) {
  stopifnot("Patient metadata must exist"=!is.null(mltplx_experiment$metadata))
  stopifnot("Survival time must be in patient metadata"=surv_time %in% colnames(mltplx_experiment$metadata))
  stopifnot("Survival event must be in patient metadata"=surv_event %in% colnames(mltplx_experiment$metadata))
  df <- mltplx_experiment %>%
    dist_to_df(reduce_symmetric = TRUE)
  
  if(is.null(slide_ids)) slide_ids <- unique(df$slide_id)
  if(is.null(types)) types <- unique(df$type1)
  fm_string <- paste0("Surv(",surv_time,",",surv_event,") ~ dist")
  #fm_string <- paste0("dist ~ ", group_factor)
  if(!is.null(covariates)) fm_string <- paste0(fm_string, " + ",paste0(covariates,collapse = " + "))
  fm <- as.formula(fm_string)
  
  result <- df %>%
    filter(slide_id %in% slide_ids,
           type1 %in% types | type2 %in% types) %>% # from reduce_symmetric
    group_by(patient_id,type1,type2) %>%
    mutate(.,dist = agg_fun(dist,na.rm = T)) %>%
    distinct(type1,type2,dist,patient_id,.keep_all = T) %>%
    group_by(type1,type2) %>%
    group_modify(~{
      tryCatch({
        coxph(fm,data=.x)
      },error=\(e) {
        NULL
      }
      ) %>%
        broom::tidy()
    }) %>%
    ungroup() %>%
    mutate(p.adj = p.adjust(p.value,method="fdr"))
  
  return(result)
}
