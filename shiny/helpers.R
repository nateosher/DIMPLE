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
      mat <- mltplx_experiment$quantile_dist$quantile_dist_array
      quantiles<-mltplx_experiment$quantile_dist$quantiles
      df <- mat %>%
        as.data.frame.table() %>%
        rename(type1=Var1,
               type2=Var2,quantile=Var3,
               dist=Freq) %>% 
        mutate(slide_id=mltplx_experiment$slide_id,quantile=factor(quantile,labels=paste0(quantiles$p1,"-",quantiles$p2))) %>%
        tidyr::drop_na(dist)
      
      for(id in slide_ids) {
        p <- df %>%
          dplyr::filter(slide_id == id) %>%
          ggplot(aes(type1,type2,fill=dist)) +
          geom_tile() +
          anglex() +
          scale_fill_gradient2() + facet_wrap(quantile~.)
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


