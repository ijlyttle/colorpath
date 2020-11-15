label_cols <- function(mat, labels) {
  dimnames(mat) <- list(NULL, labels)

  mat
}
