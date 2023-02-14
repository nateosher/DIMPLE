build_mltplx_exp <- function(n,seed,n_types=3,n_slides=4) {
  set.seed(seed)
  x <- runif(n,-50,50)
  y <- runif(n,-50,50)
  types <- sample(rep_len(sample(paste0("X",1:n_types)),n))
  slide_ids <- sample(rep_len(sample(paste0("S",1:n_slides)),n))
  
  exp <- new_MltplxExperiment(x,y,types,slide_ids)
}

add_mltplx_metadata <- function(exp,n_patients,seed=2024,n_groups=2) {
  set.seed(seed)
  slide_ids <- unlist(lapply(exp$mltplx_objects,\(e) e$slide_id))
  n_slides <- length(slide_ids)
  stopifnot("n_patients must be less than n_slides"=n_patients <= n_slides)
  patient_ids <- rep_len(paste0("P",1:n_patients),n_slides)
  
  metadata <- tibble(slide_id=slide_ids,
                     patient_id=patient_ids)
  
  groups <- sample(rep_len(sample(paste0("G",1:n_groups)),n_patients))
  
  gp_tb <- tibble(patient_id=unique(patient_ids),group=groups)
  
  metadata <- left_join(metadata,gp_tb)
  
  exp <- update_metadata(exp,metadata)
}
