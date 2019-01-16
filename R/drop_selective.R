#' Drop and slice with controlled dimensionality reduction
#'
#' @author Michal Oleszak
#'
#' @param x A multi-dimensional array
#' @param dim_extract Which level in `dim_drop` to extract
#' @param dim_drop Index of the dimension to slice and drop.
#'
#' @return
#' @export
#'
#' @examples
#' my_array <- array(NA, dim = c(3, 1, 5, 10))
#' # We want to take the first out of ten levels in the 4th dimension
#' # and then drop this (and only this) dimension.
#' my_array[,,,1] %>% dim()
#' # This reduces the array to two dimensions!
#' my_array[,,,1, drop = FALSE] %>% dim()
#' # This keeps all the four dimensions!
#' drop_selective(my_array, 1, 4) %>% dim()
#' # Ta-daa!
drop_selective <- function(x, dim_extract, dim_drop) {
  dimx <- dim(x) %>% length()
  eval(
    parse(
      text = paste0(
        "x[",
        paste0(paste(rep(",", dim_drop - 1), collapse = ""),
               dim_extract,
               paste(rep(",", dimx - dim_drop), collapse = "")),
        ", drop = FALSE] %>% adrop(dim_drop)"
      )
    )
  )
}
