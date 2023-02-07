plot_ppp <- function(mltplx_experiment,slide_ids) {
  objs <- filter_mltplx_objects(mltplx_experiment,slide_ids)
  pats <- lapply(objs,\(obj) obj$mltplx_image$ppp)

  df <- map_df(1:length(pats),\(i) {
    pat <- pats[[i]]
    slide_id <- slide_ids[[i]]
    tibble(X=pat$x,Y=pat$y,type=pat$marks,slide_id=slide_id)
  })

  for(id in slide_ids) {
    df %>%
      filter(slide_id == id) %>%
      ggplot(aes(X,Y,color=type,shape=type)) +
      geom_point() +
      scale_shape_manual(name = "type",
                         label = levels(df$type),
                         values=1:nlevels(df$type),drop=FALSE) +
      scale_color_manual(name = "type",
                         label = levels(df$type),
                         values=as.vector(pals::polychrome()),drop=FALSE) +
      ggtitle(paste0("Point pattern plot for slide id ", id))-> p
    print(p)
  }
}

plot_intensities <- function(mltplx_experiment,types,slide_ids) {
  objs <- filter_mltplx_objects(mltplx_experiment,slide_ids)

  df <- map_df(1:length(objs),\(i) {
    obj <- objs[[i]]
    intens <- obj$mltplx_intensity$intensities

    d <- intens %>%
      as_tibble() %>%
      select(all_of(types),X,Y) %>%
      pivot_longer(-c(X,Y),names_to = "type",values_to = "intensity")

    d$slide_id <- slide_ids[[i]]
    d
  })

  for(id in slide_ids) {
    df %>%
      filter(slide_id == id) %>%
      ggplot(aes(X,Y,fill=intensity)) +
      geom_tile() +
      facet_wrap(~type) +
      ggtitle(paste0("Intensity plot for slide id ", id)) -> p
    print(p)
  }
}

plot_dist <- function(mltplx_experiment, slide_ids, mode = "heatmap") {
  if(mode == "heatmap") {
    mltplx_experiment %>%
      dist_to_df() %>%
      drop_na(dist) -> df

    for(id in slide_ids) {
      df %>%
        filter(slide_id == id) %>%
        ggplot(aes(type1,type2,fill=dist)) +
          geom_tile() +
          anglex() +
          scale_fill_gradient2() +
          ggtitle(paste0("Distance matrix for slide id ", id)) -> p
      print(p)
    }
  } else if(mode == "network") {
    stop("Mode network currently unsupported")
  } else {
    stop("Mode must be either heatmap or network")
  }
}

##### UTILS

#' GGplot helper to color heatmap tiles by whether or not they are significant according to an adjusted p-value
#'
#' @param alpha Significance level
#'
#' @return
#' @export
#'
#' @examples
sig_stars <- function(alpha=0.05,p_values="p.adj") {
  list(geom_point(aes(shape=ifelse(!!sym(p_values) < alpha, "dot", "no_dot"))),
       scale_shape_manual(values=c(dot=8, no_dot=NA), guide="none"))
}

#' GGplot helper to angle x-axis labels 45 degrees
#'
#' @return
#' @export
#'
#' @examples
anglex <- function() {
  list(theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1)))
}

