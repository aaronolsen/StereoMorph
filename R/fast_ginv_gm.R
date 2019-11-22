# Copied from geomorph function fast.ginv because I couldn't figure out how to call it in geomorph from within another package
# same as ginv, but without traps (faster)
# used in any function requiring a generalized inverse
fast_ginv_gm <- function(X, tol = sqrt(.Machine$double.eps)){
  k <- ncol(X)
  Xsvd <- La.svd(X, k, k)
  Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
  rtu <-((1/Xsvd$d[Positive]) * t(Xsvd$u[, Positive, drop = FALSE]))
  v <-t(Xsvd$vt)[, Positive, drop = FALSE]
  v%*%rtu
}
