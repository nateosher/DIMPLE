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

plot_dist <- function(mltplx_experiment, slide_ids, mode = "heatmap",plot_by_quantile=F) {
  if(plot_by_quantile==F){
    if(mode == "heatmap") {
      df <- mltplx_experiment %>%
        dist_to_df() %>%
        tidyr::drop_na(dist)
      
      for(id in slide_ids) {
        p <- df %>%
          dplyr::filter(slide_id == id) %>%
          ggplot(aes(type1,type2,fill=dist)) +
          geom_tile() +
          anglex() +
          scale_fill_gradient2() +
          ggtitle(paste0("Distance matrix for slide id ", id))
        print(p)
      }
    } else if(mode == "network") {
      filtered_exp <- filter_mltplx_objects(mltplx_experiment,slide_ids)
      for(mltplx_object in filtered_exp) {
        qgraph::qgraph(mltplx_object$mltplx_dist$dist,layout = "circle",threshold=0.1)
      }
    } else {
      stop("Mode must be either heatmap or network")
    } 
  }else{
    if(mode == "heatmap") {

      df <- qdist_to_df(mltplx_experiment) %>%
        tidyr::drop_na(dist)
      
      for(id in slide_ids) {
        p <- df %>%
          dplyr::filter(slide_id == id) %>%
          ggplot(aes(type1,type2,fill=dist)) +
          geom_tile() +
          anglex() +
          scale_fill_gradient2() + facet_wrap(interval~.)+
        ggtitle(paste0("Distance matrix for slide id ", id))
        print(p)
      }
    } else if(mode == "network") {
      stop("Mode not yet supported for network with quantiles")
    } else {
      stop("Mode must be either heatmap or network")
    } 
  }
  
}

plot_qdist<-function(mltplx_experiment, slide_ids, mode = "heatmap"){
  if(mode == "heatmap") {
    
    df <- qdist_to_df(mltplx_experiment) %>%
      tidyr::drop_na(dist)
    
    for(id in slide_ids) {
      p <- df %>%
        dplyr::filter(slide_id == id) %>%
        ggplot(aes(type1,type2,fill=dist)) +
        geom_tile() +
        anglex() +
        scale_fill_gradient2() + facet_wrap(interval~.)+
        ggtitle(paste0("Distance matrix for slide id ", id))
      print(p)
    }
  } else if(mode == "network") {
    filtered_exp <- filter_mltplx_objects(mltplx_experiment,slide_ids)
    for(mltplx_object in filtered_exp) {
      for(i in 1:length(mltplx_object$quantile_dist$quantiles$q_fac))
      arr<-mltplx_object$quantile_dist$quantile_dist_array
      block.diag<-do.call("adiag", lapply(seq(dim(arr)[3]), function(x) arr[ , , x]))
      intervals<-qdist_to_df(mltplx_object)%>%distinct(type1,interval)
      qgraph::qgraph(block.diag,threshold=0.1,layout="groups",groups=as.factor(intervals$interval),title=paste0("Network for slide id ", mltplx_object$slide_id))

    }
  } else {
    stop("Mode must be either heatmap or network")
  } 
}

library(magic)
plot_qintensities <- function(mltplx_experiment,mask_type,q_probs,slide_ids) {
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

# 
# plot_qintensities(exp,"X1",q_probs,"A")
# df<-data.frame()%>%filter(image_id==input$quantile_to_plot)
# req(input$quantile_to_plot%in%df$image_id)
# intens_data<-new_MltplxObject(
#   x = df$x,
#   y = df$y,
#   marks = df$cell_type,
#   slide_id = df$image_id,ps=input$eps,bw=input$bw)
# from <- as.numeric(unlist(strsplit(input$quantiles_from,",")))
# to <- as.numeric(unlist(strsplit(input$quantiles_to,",")))
# q_probs<-cbind.data.frame(from,to)
# 
# intensities <- intens_data$mltplx_intensity$intensities %>%
#   as.data.frame()
# 
# mask_intensities <- intensities %>% pull(!!sym(input$quant_cell_type))
# q <- q_probs %>%
#   pmap_dfr(\(from,to) {
#     as.vector(quantile(mask_intensities,probs=c(from,to)/100)) -> x
#     x <- c(x,from,to)
#     names(x) <- c("q1","q2","p1","p2")
#     x
#   }) %>%
#   mutate(q_fac = factor(1:nrow(.)))
# q$q2[length(q$q2)]<-(q$q2[length(q$q2)]+.Machine$double.eps)
# 
# intensities %>%
#   fuzzyjoin::fuzzy_join(q,
#                         by = setNames(c("q1","q2"),c(input$quant_cell_type,input$quant_cell_type)),
#                         match_fun = list(`>=`, `<`)) -> joined_q
# 
# ggplot(joined_q,aes(X,Y))+geom_raster(aes(fill=q_fac))+geom_point(aes(X,Y,color=type),data=cbind.data.frame(X=intens_data$mltplx_image$ppp$x,Y=intens_data$mltplx_image$ppp$y,type=intens_data$mltplx_image$ppp$marks))+
#   scale_fill_brewer(palette="YlGnBu")


#' group_boxplots <- function(mltpx_experiment,t1,t2,grouping_var="Group") {
#'   stopifnot("Patient metadata must exist"=!is.null(mltplx_experiment$metadata))
#'   stopifnot("Patient metadata must contain grouping variable"= grouping_var %in% colnames(metadata))
#'   
#'   mltplx_experiment %>%
#'     dist_to_df %>%
#'     filter(type1 == t1,
#'            type2 == t2) %>%
#'     ggplot(aes(!!sym(grouping_var),dist)) +
#'     geom_boxplot() +
#'     geom_jitter(color="orange") +
#'     ggtitle(paste0("Distance between ", t1, " and ", t2)) +
#'     ylab("Distance")
#' }
#' 
#' patient_boxplots <- function(mltplx_experiment,t1,t2,grouping_var="Group",label_spots=TRUE) {
#'   stopifnot("Patient metadata must exist"=!is.null(mltplx_experiment$metadata))
#'   
#'   mltplx_experiment %>%
#'     dist_to_df %>%
#'     filter(type1 == t1,
#'            type2 == t2) %>%
#'     {
#'       if(!is.null(grouping_var)) ggplot(.,aes(x=patient_id,y=dist,fill=!!sym(grouping_var)))
#'       else ggplot(.,aes(x=patient_id,y=dist))
#'     } +
#'     geom_boxplot() +
#'     {
#'       if(label_spots) geom_text(aes(label=slide_id),position = position_jitter(seed = 1))
#'     } +
#'     anglex() +
#'     ylab("Distance") +
#'     ggtitle(paste0("Distance between ", t1, " and ", t2, " in all patients"))
#' }
#' 
#' plot_ppp <- function(mltplx_experiment,slide_ids) {
#'   objs <- filter_mltplx_objects(mltplx_experiment,slide_ids)
#'   pats <- lapply(objs,\(obj) obj$mltplx_image$ppp)
#'   
#'   df <- map_df(1:length(pats),\(i) {
#'     pat <- pats[[i]]
#'     slide_id <- slide_ids[[i]]
#'     tibble(X=pat$x,Y=pat$y,type=pat$marks,slide_id=slide_id)
#'   })
#'   
#'   for(id in slide_ids) {
#'     df %>%
#'       filter(slide_id == id) %>%
#'       ggplot(aes(X,Y,color=type,shape=type)) +
#'       geom_point() +
#'       scale_shape_manual(name = "type",
#'                          label = levels(df$type),
#'                          values=1:nlevels(df$type),drop=FALSE) +
#'       scale_color_manual(name = "type",
#'                          label = levels(df$type),
#'                          values=as.vector(pals::polychrome()),drop=FALSE) +
#'       ggtitle(paste0("Point pattern plot for slide id ", id))-> p
#'     print(p)
#'   }
#' }
#' 
#' plot_intensities <- function(mltplx_experiment,types,slide_ids) {
#'   objs <- filter_mltplx_objects(mltplx_experiment,slide_ids)
#'   
#'   df <- map_df(1:length(objs),\(i) {
#'     obj <- objs[[i]]
#'     intens <- obj$mltplx_intensity$intensities
#'     
#'     d <- intens %>%
#'       as_tibble() %>%
#'       select(all_of(types),X,Y) %>%
#'       pivot_longer(-c(X,Y),names_to = "type",values_to = "intensity")
#'     
#'     d$slide_id <- slide_ids[[i]]
#'     d
#'   })
#'   
#'   for(id in slide_ids) {
#'     df %>%
#'       filter(slide_id == id) %>%
#'       ggplot(aes(X,Y,fill=intensity)) +
#'       geom_tile() +
#'       facet_wrap(~type) +
#'       ggtitle(paste0("Intensity plot for slide id ", id)) -> p
#'     print(p)
#'   }
#' }
#' 
#' plot_dist <- function(mltplx_experiment, slide_ids, mode = "heatmap") {
#'   if(mode == "heatmap") {
#'     mltplx_experiment %>%
#'       dist_to_df() %>%
#'       drop_na(dist) -> df
#'     
#'     for(id in slide_ids) {
#'       df %>%
#'         filter(slide_id == id) %>%
#'         ggplot(aes(type1,type2,fill=dist)) +
#'         geom_tile() +
#'         anglex() +
#'         scale_fill_gradient2() +
#'         ggtitle(paste0("Distance matrix for slide id ", id)) -> p
#'       print(p)
#'     }
#'   } else if(mode == "network") {
#'     stop("Mode network currently unsupported")
#'   } else {
#'     stop("Mode must be either heatmap or network")
#'   }
#' }
#' 
#' ##### UTILS
#' 
#' #' GGplot helper to color heatmap tiles by whether or not they are significant according to an adjusted p-value
#' #'
#' #' @param alpha Significance level
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' sig_stars <- function(alpha=0.05,p_values="p.adj") {
#'   list(geom_point(aes(shape=ifelse(!!sym(p_values) < alpha, "dot", "no_dot"))),
#'        scale_shape_manual(values=c(dot=8, no_dot=NA), guide="none"))
#' }
#' 
#' #' GGplot helper to angle x-axis labels 45 degrees
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' anglex <- function() {
#'   list(theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1)))
#' }
#' 
#' dist_to_df.MltplxExperiment <- function(mltplx_experiment) {
#'   if(is.null(mltplx_experiment$metadata))
#'     warning("you have not attached any metadata")
#'   map_df(mltplx_experiment$mltplx_objects,dist_to_df) %>%
#'     {
#'       if(!is.null(mltplx_experiment$metadata)) left_join(.,mltplx_experiment$metadata)
#'       if(!is.null(mltplx_experiment$metadata))
#'         left_join(.,mltplx_experiment$metadata)
#'       else
#'         .
#'     }
#' }
#' 
#' dist_to_df.MltplxObject <- function(mltplx_object) {
#'   if(!is.null(mltplx_object$mltplx_dist)) {
#'     mat <- mltplx_object$mltplx_dist$dist
#'     
#'     mat %>%
#'       as.data.frame.table() %>%
#'       rename(type1=Var1,
#'              type2=Var2,
#'              dist=Freq) %>%
#'       mutate(slide_id=mltplx_object$slide_id)
#'   } else {
#'     cat(paste0("Multiplex object corresponding to slide id ", mltplx_object$slide_id," does not contain a distance matrix."))
#'   }
#' }
#' 
#' dist_to_df <- function(x,...) UseMethod("dist_to_df")
#' add_QuantileDist <- function(x,...) UseMethod("add_QuantileDist")


