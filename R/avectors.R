avectors <- function(u, v){
	if(anyNA(u) || anyNA(v)) return(NA)
	#if(sum(abs(u - v)) < 1e-9) return(0)
	if(length(u) == 2){
		if(sum(abs(c(u[1]+v[1], u[2]+v[2]))) < 1e-9) return(0)
	}else{
		if(sum(abs(c(u[1]+v[1], u[2]+v[2], u[3]+v[3]))) < 1e-9) return(0)
	}
	acos(sum(u*v) / (sqrt(sum(u*u)) * sqrt(sum(v*v))))
}