# Copied from geomorph function fast.solve because I couldn't figure out how to call it in geomorph from within another package
# chooses between fast.ginv or qr.solve, when det might or might not be 0
# used in any function requiring a matrix inverse where the certainty of
# singular matrices is in doubt; mostly phylo. functions
fast_solve_gm <- function(x) if(det(x) > 1e-8) qr.solve(x) else fast_ginv_gm(x)
