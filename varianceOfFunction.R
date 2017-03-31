varf <-function(f, low, upp){
  suma <- polynom::integral(f, c(low,upp))
  meana <- suma / (upp-low)

  r.sq <- integrate(X.sq, low, upp, f)
  
  r.sq$value - meana^2
  
}


X.sq <- function(x, fun){
  as.function(fun)(x^2)
}