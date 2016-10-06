avectors <- function(u, v){
	if(anyNA(u) || anyNA(v)) return(NA)
	if(sum(abs(u - v)) < 1e-9) return(0)
	acos(sum(u*v) / (sqrt(sum(u*u)) * sqrt(sum(v*v))))
}