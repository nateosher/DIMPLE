#' Generates a simulation of an arbitrary number of cell types with the specified
#' intensity pattern
#' @param intensity_list a list of matrices, each of which represents the
#' intensity pattern of a given cell type. Each matrix should have the same
#' dimensions.
#' @param square_side_length a double indicating the side length of each matrix
#' entry. Defaults to 1, i.e. each matrix entry is a 1 by 1 unit square.
SimulateGrid = function(intensity_list, square_side_length = 1){
  # Because spatstat indexes from bottom left corner and matrices index from
  # top left, reflecting over the horizontal gives the desired configuration
  intensity_list = map(intensity_list, function(m){
    apply(m, 2, rev)
  })
  square_area = square_side_length * square_side_length
  n = length(intensity_list[[1]])
  grid_nrow = nrow(intensity_list[[1]])
  grid_ncol = ncol(intensity_list[[1]])
  counts_lists = map(intensity_list, function(intensity_mat){
    # simulates by column, i.e. first column, then second, etc.
    counts = rpois(n, intensity_mat)
    # TODO: pipe
    counts_r = map2(counts, rep(1:grid_nrow, grid_ncol), function(counts, r){
      c(counts, r)
    })
    counts_r_c = map2(counts_r, rep(1:grid_ncol, each = grid_nrow), function(l, c){
      c(l, c)
    })
    counts_r_c = discard(counts_r_c, ~ .x[1] == 0)
    counts_r_c
  })

  cell_count_matrices = map(counts_lists, function(count_list){
    do.call(rbind, map(count_list, \(l){
      matrix(c(rep(l[2], l[1]), rep(l[3], l[1])), ncol = 2) - 1
    }))
  })

  final_sim_list = imap(cell_count_matrices, function(m, i){
    m = m * square_side_length
    m[,1] = m[,1] + runif(nrow(m)) * square_side_length
    m[,2] = m[,2] + runif(nrow(m)) * square_side_length
    m = cbind(m, i)
  })

  final_sim_mat = do.call(rbind, final_sim_list)

  final_pointproc = ppp(x = final_sim_mat[,2], y = final_sim_mat[,1],
                        marks = factor(final_sim_mat[,3]),
                        window = owin(c(0, square_side_length * grid_ncol),
                                      c(0, square_side_length * grid_nrow)))

}
