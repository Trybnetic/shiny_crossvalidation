# the figures require ggplot2 library and
# all packages it depends on
library(ggplot2)
library(polynom)

# simulates data for a polynomial model and adds noise
#
# @param sample_size: number of simulated observations
# @param poly_vec: a vector containing the coefficients of a polynomial
# @param noise: the amount of noise which will be added to the expected
#               values of the polynomial
simulateData <- function(sample_size, poly_vec, noise){
  f <- as.function(polynomial(poly_vec))

  # generate the x predictor
  x <- runif(sample_size, -20, 20)

  # calculate y values
  y <- f(x)

  y <- y + rnorm(sample_size, sd=noise)
  data.frame(x=x, y=y)
}

# fitting lm() polynomials of increasing complexity
# (up to max.degree) and storing their predictions
# in the new.dat data.frame
fitModels <- function(dat, max.poly) {
  res <- lapply(1:max.poly, FUN=function(i) {as.function(polynomial(coef(lm(y ~ poly(x, i, raw=TRUE, simple=TRUE), data = dat))))})

  return(res)
}


plotModels <- function(Data, max.poly){
  estimated_functions <- fitModels(Data, max.poly)
  colors = c("red","blue","green","yellow","black","orange", "chocolate", "deeppink", "seagreen", "slategray")

  # plotting the data and the fitted models
  p <- ggplot()
  p <- p + geom_point(aes(x, y), Data, colour = "darkgrey")
  degree <- 0
  for (f in estimated_functions) {
    degree <- degree + 1
    text <- paste("x^", as.character(degree), sep="")
    p <- p + stat_function(data = data.frame(x = -20:20,
                                             polynomial = rep(text, 41)),
                           fun = f,
                           aes(colour = polynomial))
  }

  p
}

plotGenerativeModel <- function(poly_vec, noise=NA, min=-20, max=20){
  f <- as.function(polynomial(poly_vec))
  x <- data.frame(x=seq(min, max, 0.01))
  y <- f(x)
  
  upper <- as.vector(y) + noise
  lower <- as.vector(y) - noise
  d <- data.frame(x=x, y=y, upper=upper, lower=lower)
  p <- ggplot(data=x, aes(x=x))
  p <- p + stat_function(fun=f) #geom_line(aes(x, y), d)
  p <- p + geom_ribbon(aes(x=x, ymax=upper, ymin=lower), d, alpha="0.5")
  
  p
}

add_bins <- function(data, n_bins, seed) {
  n <- nrow(data)
  bins <- 1:n_bins
  k <- ceiling(n / n_bins)

  set.seed(seed)
  data$bin <- sample(rep(bins, k)[1:n])

  return(data)
}


split_data <- function(data, bin) {
  list(train = data[data$bin != bin, ],
       test = data[data$bin == bin, ])
}

estimate <- function(model, x) {
  x <- as.integer((2 + x) * 100) + 1
  return(model$predicted[x])
}

#' Function to cross validate a dataset
#'
#' @param data data.frame of x and y values, x values have to be divisible by .01
#' @param n_bins number of bins you want to split the data for cross validation
#' @param max.poly maximum degree of polynomial you want to validate
#' @param seed seed to control randomness
#'
#' @returns data.frame of all points of data with estimated y values
#' for each polynomial degree and the calculated MSEs
validate <- function(data, n_bins, max.poly = 2, seed = 1337) {
  data <- add_bins(data, n_bins, seed)
  result <- data.frame()

  for (bin in 1:n_bins) {
    split <- split_data(data, bin)

    model <- fitModels(split$train, max.poly, boundary=c(-2,2))
    test_df <- split$test
    test_df <- test_df[order(test_df$x),]
    rownames(test_df) <- 1:nrow(test_df)

    for (i in 1:max.poly) {
      sub.dat <- model[model$degree == i, ]

      new.dat <- test_df
      new.dat$estimate <- estimate(sub.dat, new.dat$x)
      new.dat$degree <- i

      result <- rbind(result, new.dat)
    }
  }

  result$mse <- (result$y - result$estimate)^2

  return(result[order(result$degree),])
}

validation_se <- function(data, n_bins, max.poly = 2, seed = 1337) {
  result <- validate(data, n_bins, max.poly=max.poly, seed=seed)

  res <- aggregate(cbind(estimate, mse) ~ degree, result, mean)
  res$n <- aggregate(mse ~ degree, result, length)$mse

  res$se <- sqrt(res$mse) / sqrt(res$n)

  return(res)
}
