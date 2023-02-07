#' Replaces the metadata of a `MltplxExperiment` object with new metadata,
#' or creates it if there is currently none.
#' @param mltplx_experiment `MltplxExperiment` object to be updated
#' @param metadata New tibble or data.frame of metadata. Must include *one* row
#' per unique slide id, and a column `slide_id` indicating the slide to which
#' each row corresponds.
#' @return Updated `MltplxExperiment` object
#' @importFrom magrittr `%>%`
#' @export
update_metadata = function(mltplx_experiment, metadata){
  check_metadata(mltplx_experiment$mltplx_objects,metadata)
  metadata <- metadata %>%
    mutate(patient_id = factor(patient_id),
           slide_id = factor(slide_id))
  mltplx_experiment$metadata = metadata
  return(mltplx_experiment)
}

check_metadata <- function(mltplx_objects,metadata) {
  stopifnot("Metadata needs column named slide_id"="slide_id" %in%
              colnames(metadata))
  stopifnot("Metadata needs column named patient_id"="patient_id" %in%
              colnames(metadata))

  SameElements <- function(a, b) return(identical(sort(a), sort(b)))

  stopifnot("Metadata slide_id does not contain all slide_ids found in mltplx_objects" =
              SameElements(metadata$slide_id,
                             unlist(
                               lapply(mltplx_objects,\(obj) obj$slide_id)
                             )
                          )
            )
}


s1 = "testing testing \
        123"
