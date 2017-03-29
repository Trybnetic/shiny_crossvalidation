# the figures require ggplot2 library and
# all packages it depends on
library(ggplot2)

simulateData <- function(Sample, Noise=1, Model=NA){
  # generate the x predictor
  x <- runif(Sample,-2,2)
  # generate the y response
  y <- 2*x^3 + x^2 - 2*x +5 + rnorm(Sample, sd = Noise)
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

validate <- function(data, n_bins) {
  n <- nrow(data)
  bins <- 1:n_bins

  k = ceiling(n / n_bins)

  data$bin <- sample(rep(bins, k)[1:n])
}
