#' Creates new `MltplxObject` object.
#'
#' @param x Vector of x coordinates of cells
#' @param y Vector of y coordinates of cells
#' @param marks Vector of cell types
#' @param slide_id Vector of slide ids, i.e. how should cells be grouped
#' @param ps Optional; if you'd like to generate intensity estimates when
#' you first create these objects, this will determine the "pixel size," i.e.
#' the side-length of the squares the domain will be broken up into in order
#' for the estimation to occur. Required if `bw` is passed (and vice versa).
#' Intensities can also be generated after this object is created.
#' @param bw Optional; if you'd like to generate intensity estimates when
#' you first create these objects, this will determine the "bandwidth" of the
#' smoothing. Larger values of `bw` result in more smoothing; this is not
#' necessarily a good thing. Required if `ps` is passed (and vice versa).
#' Intensities can also be generated after this object is created.
#' @param dist_metric Optional; if you'd like to generate distance matrices
#' between different cell type intensities, this will be the distance metric
#' used to do so. Distance matrices can also be generated after this object
#' is created.
#' @param .dist_metric_name Optional; not required, even when a distance
#' matrix is passed. However, if you'd like to name the distance function
#' you use something specific, you can pass this name as a string using
#' @param xrange vector of size 2 with range of x-coordinates. If NULL, will default to c(min(x),max(x))
#' @param yrange vector of size 2 with range of y-coordinates. If NULL, will default to c(min(y),max(y))
#' this parameter. Otherwise, it defaults to the name of the function
#' @param window Optional; object of class `owin` from spatstat.geom
#' representing window of point process
#' @return `MltplxObject` object
#' @export
#' @examples
#' library(dplyr)
#' set.seed(1234567)
#' cell_x_values = runif(3000, 0, 600)
#' cell_y_values = runif(3000, 0, 600)
#' cell_marks = sample(c("Tumor", "Immune", "Other"), 3000, replace = TRUE)
#' slide_ids = rep(paste("Slide", 1:10), each = 300)
#' raw_data_tibble = tibble(
#'   x = cell_x_values,
#'   y = cell_y_values,
#'   marks = cell_marks,
#'   id = slide_ids
#' )
#' # Construct `MltplxObject`
#'  obj_1 = new_MltplxObject(
#'    x = raw_data_tibble %>% filter(id == "Slide 1") %>% pull(x),
#'    y = raw_data_tibble %>% filter(id == "Slide 1") %>% pull(y),
#'    marks = raw_data_tibble %>% filter(id == "Slide 1") %>% pull(marks),
#'    slide_id = "Slide 1"
#' )
#' print(obj_1)
#'
#' # Construct `MltplxObject` with intensities
#'  obj_2 = new_MltplxObject(
#'    x = raw_data_tibble %>% filter(id == "Slide 1") %>% pull(x),
#'    y = raw_data_tibble %>% filter(id == "Slide 1") %>% pull(y),
#'    marks = raw_data_tibble %>% filter(id == "Slide 1") %>% pull(marks),
#'    slide_id = "Slide 1",
#'    ps = 10, bw = 30
#' )
#' print(obj_2)
#'
#' # Construct `MltplxObject` with intensities + dist matrices
#'  obj_3 = new_MltplxObject(
#'    x = raw_data_tibble %>% filter(id == "Slide 1") %>% pull(x),
#'    y = raw_data_tibble %>% filter(id == "Slide 1") %>% pull(y),
#'    marks = raw_data_tibble %>% filter(id == "Slide 1") %>% pull(marks),
#'    slide_id = "Slide 1",
#'    ps = 10, bw = 30,
#'    dist_metric = cor
#' )
#' print(obj_3)
#'
new_MltplxObject = function(x, y, marks,slide_id, xrange = NULL, yrange = NULL,
                            ps = NULL, bw = NULL,
                            dist_metric = NULL, .dist_metric_name = NULL,
                            symmetric = TRUE,
                            window = NULL){

  if(!is.null(.dist_metric_name)){
    dist_metric_name = .dist_metric_name
  }else{
    dist_metric_name = substitute(dist_metric) %>% as.character()
  }

  # Make image
  mltplx_image = new_MltplxImage(x, y, marks, xrange = xrange,
                                 yrange = yrange, window = window)


  # Make intensities, if applicable
  if(!is.null(ps) && !is.null(bw)){
    mltplx_intensity = new_MltplxIntensity(mltplx_image, ps, bw)
  }else{
    mltplx_intensity = NULL
  }

  # Make distance matrices, if applicable
  if(!is.null(dist_metric)){
    mltplx_dist = new_MltplxDist(mltplx_intensity, dist_metric,
                                 dist_metric_name, symmetric = symmetric)
  }else{
    mltplx_dist = NULL
  }

  structure(
    list(
      slide_id = slide_id[1],
      mltplx_image = mltplx_image,
      mltplx_intensity = mltplx_intensity,
      mltplx_dist = mltplx_dist
    ),
    class = "MltplxObject"
  )
}

#' @export
print.MltplxObject = function(mltplx_object, ...){
  cat("MltplxObject \n")
  cat("Slide id:", mltplx_object$slide_id, "\n")
  cat("Image with", mltplx_object$mltplx_image$ppp$n, "cells across",
      length(mltplx_object$mltplx_image$cell_types), "cell types\n")
  cell_type_str = paste0(mltplx_object$mltplx_image$cell_types, collapse = ", ")
  cat("Cell types:", cell_type_str, "\n")

  if(!is.null(mltplx_object$mltplx_intensity)){
    cat("Intensity generated with pixel size",
        mltplx_object$mltplx_intensity$ps,
        "and bandwidth", mltplx_object$mltplx_intensity$bw, "\n")
  }else{
    cat("No intensity generated (yet)\n")
  }
  if(!is.null(mltplx_object$mltplx_dist)){
    cat("Distance matrix generated using", mltplx_object$mltplx_dist$metric,
        "\n")
  }else{
    cat("No distance matrix generated (yet)\n")
  }
  if(!is.null(mltplx_object$quantile_dist) && length(mltplx_object$quantile_dist) > 1){
    cat(dim(mltplx_object$quantile_dist$quantile_dist_array)[3],
        "quantile distance arrays generated for mask",
        mltplx_object$quantile_dist$mask_type,
        "\n")
  }
}

#' Plots distance matrix of `MltplxObject`, if one has been generated.
#' @param mltplx_object Object of class `MltplxObject`
#' @param mode String indicating plot type, either "heatmap" or "network"
#' @return NULL
#' @importFrom magrittr `%>%`
#' @import ggplot2
#' @export
plot_dist_matrix.MltplxObject <- function(mltplx_object, mode = "heatmap",
                                   net_threshold = 0, invert_dist = TRUE,
                                   symmetric=TRUE) {
  if(is.null(mltplx_object$mltplx_dist))
    stop("no distance matrix has been generated for this `MltplxObject`; see `update_object_dist` function")
  if(mode == "heatmap") {
    df <- mltplx_object %>%
      dist_to_df(reduce_symmetric = symmetric) %>%
      tidyr::drop_na(dist)

    p <- df %>%
      ggplot(aes(type1,type2,fill=dist)) +
      geom_tile() +
      anglex() +
      viridis::scale_fill_viridis() +
      xlab("") + ylab("") +
      guides(fill = guide_colorbar(title = mltplx_object$mltplx_dist$metric)) +
      ggtitle(paste0("Distance matrix for slide id ", mltplx_object$slide_id)) +
      theme(
        axis.text.x = element_text(face="bold", colour = "black"),
        axis.text.y = element_text(face="bold", colour = "black")
      )

    print(p)
  } else if(mode == "network") {
    g <- igraph::graph_from_adjacency_matrix(mltplx_object$mltplx_dist$dist,weighted=TRUE)
    p <- g %>%
      ggnetwork::ggnetwork(.,layout = igraph::layout_in_circle(.)) %>%
      dplyr::mutate(sgn = factor(ifelse(sign(weight) < 0,"Negative","Positive"))) %>%
      dplyr::filter(abs(weight) >= net_threshold) %>%
      (\(d){
        if(invert_dist){
          d = d %>%
            mutate(
              weight = map_dbl(weight, \(w){
                if(w == 0) return(w)
                else if(w > 0) return(max(abs(weight)) - w)
                else return(-max(abs(weight)) - w)
              })
            )
        }
        d
      }) %>%
      ggplot2::ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
      ggnetwork::geom_edges(aes(color=sgn,linewidth=abs(weight))) +
      ggnetwork::geom_nodes(color="#E58601",size=10) +
      ggnetwork::geom_nodetext(aes( label = name),
                               fontface = "bold",color="#D3DDDC") +
      theme(legend.position = "none") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            panel.background = element_rect(fill = "#24281A"),
            panel.grid = element_blank()) +
      guides(linewidth="none",color="none") +
      ggplot2::scale_color_manual(values=c("Positive"="#46ACC8","Negative"="#B40F20"))
    suppressWarnings(print(p))
  } else {
    stop("Mode must be either heatmap or network")
  }
}

#' Plots quantile distance matrices for selected slides in a `MltplxExperiment` object
#' @param mltplx_object `MltplxObject` objects
#' @param mode String indicating plot type, either "heatmap" or "network"
#' @param net_threshold When mode is "network", edges below this absolute value are
#' excluded from the plot
#' @return ggplot2 object
#' @importFrom magrittr `%>%`
#' @import ggplot2
#' @export
plot_qdist_matrix.MltplxObject <- function(mltplx_object, mode = "heatmap",
                                           net_threshold = 0) {
  stopifnot("Quantile distances must exist"=!is.null(mltplx_object$quantile_dist))
  if(mode == "heatmap") {

    df <- qdist_to_df(mltplx_object) %>%
      tidyr::drop_na(qdist)

    p <- df %>%
      ggplot(aes(type1,type2,fill=qdist)) +
      geom_tile() +
      anglex() +
      viridis::scale_fill_viridis() + facet_wrap(interval~., nrow = 1)+
      ggtitle(paste0("Distance matrix by quantile of cell type ",
                     mltplx_object$quantile_dist$mask_type),
              subtitle = paste0("slide id: ", mltplx_object$slide_id)) +
      xlab("") + ylab("") +
      guides(fill = guide_colorbar(title = mltplx_object$mltplx_dist$metric)) +
      theme(
        axis.text.x = element_text(face="bold", colour = "black"),
        axis.text.y = element_text(face="bold", colour = "black")
      )

    p
  } else if(mode == "network") {
      arr <- mltplx_object$quantile_dist$quantile_dist_array
      intervals <- qdist_to_df(mltplx_object) %>%
        distinct(interval) %>%
        pull(interval)
      gf <- purrr::map_df(1:dim(arr)[3],\(i) {
        g <- igraph::graph_from_adjacency_matrix(arr[,,i],weighted=TRUE)
        d <- ggnetwork::ggnetwork(g,layout = igraph::layout_in_circle(g))
        d$interval <- intervals[i]
        d
      })
      p <- gf %>%
        dplyr::mutate(sgn = factor(ifelse(sign(weight) < 0,"Negative","Positive"))) %>%
        dplyr::filter(abs(weight) >= net_threshold) %>%
        ggplot2::ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
        ggnetwork::geom_edges(aes(color=sgn,linewidth=abs(weight))) +
        ggnetwork::geom_nodes(color="#E58601",size=10) +
        ggnetwork::geom_nodetext_repel(aes( label = name),
                                       fontface = "bold",color="#D3DDDC") +
        theme(legend.position = "none") +
        theme_minimal() +
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              panel.background = element_rect(fill = "#24281A"),
              panel.grid = element_blank()) +
        guides(linewidth="none",color="none") +
        ggplot2::scale_color_manual(values=c("Positive"="#46ACC8","Negative"="#B40F20")) +
        ggtitle(paste0("Distance matrix by quantile of cell type ",
                       mltplx_object$quantile_dist$mask_type),
                subtitle = paste0("slide id: ", mltplx_object$slide_id)) +
        facet_wrap(interval~.)
      p
  } else {
    stop("Mode must be either heatmap or network")
  }
}

#' Update the intensities generated for a specific `MltplxObject`
#' @param mltplx_object object of class `MltplxObject` to be updated
#' @param ps "Pixel size" of intensity estimations. Results in squares that are
#' roughly `ps` by `ps` units.
#' @param bw This determines the bandwidth of the
#' smoothing of the values assigned to each square of the intensity
#' estimations. Larger values result in "smoother" intensities, while smaller
#' values result in "coarser" estimations.
#' @return updated `MltplxObject` object
#' @export
update_object_intensity = function(mltplx_object, ps, bw){
  mltplx_object$mltplx_intensity = new_MltplxIntensity(
    mltplx_object$mltplx_image, ps, bw
  )
  return(mltplx_object)
}

#' Update the distance matrices generated for a specific `MltplxObject`
#' @param mltplx_object object of class `MltplxObject` to be updated
#' @param dist_metric distance metric to be used; can be any function that takes
#' in two vectors of the same length and produces a scalar
#' @param .dist_metric_name Optional; you can use this argument to specify the
#' name of the distance metric you use
#' @return updated `MltplxObject` object
#' @export
update_object_dist = function(mltplx_object, dist_metric,
                              .dist_metric_name = NULL,
                              symmetric = TRUE){
  if(is.null(mltplx_object$mltplx_intensity))
    stop(paste("you have to generate intensities before",
                "distance matrices- see `update_intensity` function",
                "for details on how to do this"))

  if(is.null(.dist_metric_name)){
    .dist_metric_name = substitute(dist_metric) %>% as.character()
  }

  mltplx_object$mltplx_dist = new_MltplxDist(
    mltplx_object$mltplx_intensity, dist_metric,
    .dist_metric_name, symmetric = symmetric
  )
  return(mltplx_object)
}

#' Plots intensities for selected cell types of `MltplxObject` object.
#' @param mltplx_object `MltplxObject` object
#' @param types Vector of cell types whose intensities you'd like to plot
#' @return NULL
#' @importFrom magrittr `%>%`
#' @import ggplot2
#' @export
plot_intensity_surface.MltplxObject <- function(mltplx_object,types = NULL) {
  if(is.null(mltplx_object$mltplx_intensity))
    stop("intensities have not been generated for this `MltplxObject`")

  all_cell_types = mltplx_object$mltplx_image$cell_types

  if(is.null(types))
    types = all_cell_types

  intens <- mltplx_object$mltplx_intensity$intensities

  df <- intens %>%
    tibble::as_tibble() %>%
    dplyr::select(all_of(types),X,Y) %>%
    tidyr::pivot_longer(-c(X,Y),names_to = "type",values_to = "intensity") %>%
    filter(type %in% types)

  df$slide_id <- mltplx_object$slide_id

  df %>%
    ggplot(aes(X,Y,fill=intensity)) +
    geom_tile() +
    facet_wrap(~type) +
    viridis::scale_fill_viridis() +
    xlab("") + ylab("") +
    guides(fill = guide_colorbar(title = "Intensity")) +
    theme(
      axis.text.x = element_text(face="bold", colour = "black"),
      axis.text.y = element_text(face="bold", colour = "black")
    ) +
    ggtitle(paste0("Intensity plot for slide id ", mltplx_object$slide_id)) -> p

  p
}


#' Convert distance matrix to dataframe
#'
#' @param mltplx_object object of type `MltplxObject`
#' @param reduce_symmetric logical, whether to remove equivalent rows
#'
#' @return tibble with dist information
#' @export
dist_to_df.MltplxObject <- function(mltplx_object,
                                    reduce_symmetric = FALSE) {
  if(!is.null(mltplx_object$mltplx_dist)) {
    mat <- mltplx_object$mltplx_dist$dist

    if(reduce_symmetric) {
      mat[lower.tri(mat)] <- NA
    }

   df <- mat %>%
    as.data.frame.table() %>%
    rename(type1=Var1,
           type2=Var2,
           dist=Freq) %>%
    tidyr::drop_na(dist) %>%
    mutate(slide_id=mltplx_object$slide_id)

    return(df)
  } else {
    cat(paste0("Multiplex object corresponding to slide id ", mltplx_object$slide_id," does not contain a distance matrix."))
  }
}

#' Quantile distance matrix to dataframe
#'
#' @param mltplx_object object of type `MltplxObject`
#' @param reduce_symmetric logical, whether to remove equivalent rows
#'
#' @return tibble with dist information
#' @export
qdist_to_df.MltplxObject <- function(mltplx_object,reduce_symmetric = TRUE) {

  has_quantile_dist = !is.null(mltplx_object$quantile_dist) &&
                        (length(mltplx_object$quantile_dist) > 1 ||
                         !is.na(mltplx_object$quantile_dist))
  if(has_quantile_dist) {
    arr <- mltplx_object$quantile_dist$quantile_dist_array

    nms3 <- mltplx_object$quantile_dist$quantiles %>%
      tidyr::unite("p1_p2",p1,p2,sep="-") %>%
      pull(p1_p2)

    dimnames(arr)[[3]] <- nms3

    df <- arr %>%
      as.data.frame.table() %>%
      rename(type1=Var1,
             type2=Var2,
             interval=Var3,
             qdist=Freq) %>%
      mutate(slide_id=mltplx_object$slide_id) %>%
      as_tibble()

    if(reduce_symmetric) {
      df %>%
        select(type1,type2,slide_id,interval) %>%
        apply(.,1,sort) %>%
        t(.) %>%
        duplicated(.) -> dup_ix

      df <- df[dup_ix, ]
    }
    df
  } else {
    warning(paste0("Multiplex object corresponding to slide id ", mltplx_object$slide_id," does not contain a quantile distance array."))
    tibble::tibble()
  }
}

#' @export
plot.MltplxObject = function(mltplx_object, ...){
  plot(mltplx_object$mltplx_image, id = mltplx_object$slide_id)
}

#' Update the quantile distance matrices generated for a specific `MltplxObject`
#' @param mltplx_object object of class `MltplxObject` to be updated
#' @param dist_metric distance metric to be used; can be any function that takes
#' in two vectors of the same length and produces a scalar
#' @param mask_type Name of cell type you want to make a QuantileDist of
#' @param q_probs Data frame with columns "from" and "to" with quantile ranges
#' as percentage points, i.e. 25 for first quartile, 50 for median, etc. Note
#' that these ranges *can* overlap.
#' @param .dist_metric_name The distance metric you want to use to compute
#' distances between distributions.
#' @return updated `MltplxObject` object
#' @export
update_qdist.MltplxObject <- function(mltplx_object,
                                          dist_metric,
                                          mask_type,
                                          q_probs,
                                          .dist_metric_name = NULL) {

  if(!(mask_type %in% colnames(mltplx_object$mltplx_intensity$intensities))){
    warning(paste0("cell type ", mask_type, " not present in slide with id ",
                  mltplx_object$slide_id, "; returning NA"))
    mltplx_object$quantile_dist = NA
    return(mltplx_object)
  }

  if(!is.null(.dist_metric_name)){
    dist_metric_name = .dist_metric_name
  }else{
    dist_metric_name = substitute(dist_metric) %>% as.character()
  }

  q_dist <- mltplx_object$quantile_dist <- new_QuantileDist(mltplx_object$mltplx_intensity,
                                                       dist_metric=dist_metric,
                                                       mask_type=mask_type,
                                                       q_probs=q_probs,
                                                       dist_metric_name=dist_metric_name)
  if(length(mltplx_object$quantile_dist) == 0) {
    mltplx_object$quantile_dist <- list(q_dist)
  } else {
    mltplx_object$quantile_dist <- append(mltplx_object$quantile_dist,q_dist)
  }
  mltplx_object
}

#' Get count of cell types in `MltplxObject`
#' @param mltplx_object `MltplxObject`
#' @return dataframe of cell type counts in boject
#' @export
cell_type_counts.MltplxObject <- function(mltplx_object) {
  mltplx_object$mltplx_image$ppp$marks %>%
    table() %>%
    as.data.frame() %>%
    tidyr::pivot_wider(names_from = ".",values_from = "Freq") %>%
    dplyr::mutate(slide_id = mltplx_object$slide_id)
}
