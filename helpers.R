# the figures require ggplot2 library and
# all packages it depends on
library(ggplot2)

simulateData <- function(Sample, Noise=NA, Model=NA, Polynom){
  x <- runif(Sample,-2,2)
  X <- cbind(intercept=1, poly(x, Polynom, simple=TRUE))
  y <- X %*% Model
  
  Noise <- 1/(1-Noise)
  y <- y + rnorm(Sample, sd=sd(y)*Noise)
  data.frame(x=x, y=y)
}

createNewDat <- function(Data, max.poly){
  # cretaing data.frame which will store model predictions

  x.new <- seq(min(Data$x), max(Data$x), by=0.05)
  degree <- rep(1:max.poly, each=length(x.new))
  predicted <- numeric(length=length(x.new)*max.poly)
  new.dat <- data.frame(x=rep(x.new, times=max.poly),
                        degree,
                        predicted)
  new.dat
}

# fitting lm() polynomials of increasing complexity
# (up to max.degree) and storing their predictions
# in the new.dat data.frame
fitModels <- function(Data, max.poly){
  new.dat <- createNewDat(Data, max.poly)


  x.new <- seq(min(Data$x), max(Data$x), by = 0.05)
  
  for(i in 1:max.poly)
  {
    sub.dat <- new.dat[new.dat$degree==i,]
    new.dat[new.dat$degree==i,3] <- predict(lm(y~poly(x, i), data=Data),
                                            newdata=data.frame(x=x.new)
                                            )
  }
  new.dat
}

plotModels <- function(Data, max.poly){
  new.dat <- fitModels(Data, max.poly)
  # plotting the data and the fitted models
  p <- ggplot()
  p <- p + geom_point(aes(x, y), Data, colour="darkgrey")
  p <- p + geom_line(aes(x, predicted,
                    colour=as.factor(degree)),
                new.dat)
  p <- p + scale_colour_discrete(name = "Degree")
  p
}

plotGenerativeModel <- function(polynom, model, noise, min=-2, max=2){
  x <- cbind(intercept=1, poly(seq(min, max, 0.01), polynom, simple = TRUE))

  y <- x %*% model
  var.y <- var(y)
  noise <- noise/(1-noise)
  upper <- as.vector(y) + sqrt(noise)
  lower <- as.vector(y) - sqrt(noise)
  d <- data.frame(x=x[,2], y=y, upper=upper, lower=lower)
  p <- ggplot()
  p <- p + geom_line(aes(x, y), d)
  p <- p + geom_ribbon(aes(x=x, ymax=upper, ymin=lower), d, alpha="0.5")
  
  p
}