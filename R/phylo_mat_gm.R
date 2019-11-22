# Copied from geomorph function phylo.mat because I couldn't figure out how to call it in geomorph from within another package
# estimate BM phylo.cov.matrix and transform from phylogeny
# used in: compare.evol.rates, compare.multi.evol.rates, phylo.integration, phylo.modularity, physignal
phylo_mat_gm<-function(x,phy){
  C<-vcv.phylo(phy,anc.nodes=FALSE)
  C<-C[rownames(x),rownames(x)]
  invC <-fast_solve_gm(C)
  eigC <- eigen(C)
  lambda <- zapsmall(eigC$values)
  if(any(lambda == 0)){
    warning("Singular phylogenetic covariance matrix. Proceed with caution")
    lambda = lambda[lambda > 0]
  }
  eigC.vect = eigC$vectors[,1:(length(lambda))]
  D.mat <- fast_solve_gm(eigC.vect%*% diag(sqrt(lambda)) %*% t(eigC.vect))
  rownames(D.mat) <- colnames(D.mat) <- colnames(C)
  rownames(invC) <- colnames(invC) <- colnames(C)
  list(invC = invC, D.mat = D.mat,C = C)
}