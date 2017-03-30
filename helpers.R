# the figures require ggplot2 library and
# all packages it depends on
library(ggplot2)

simulateData <- function(Sample, Noise=NA, Model=NA, Polynom){
  # generate the x predictor
  x <- round(runif(Sample, -2, 2),2)
  # add minimal and maximal values for x
  x <- c(-2, x[2:(Sample -1)], 2)
  # generate the y response
  X <- cbind(intercept=1, poly(x, Polynom, simple=TRUE))
  y <- X %*% Model
  
  Noise <- 1/(1-Noise)
  y <- y + rnorm(Sample, sd=sd(y)*Noise)
  data.frame(x=x, y=y)
}

createNewDat <- function(Data, max.poly, boundary=NA){
  # cretaing data.frame which will store model predictions

  if (any(is.na(boundary))) {
    x.new <- seq(min(Data$x), max(Data$x), by = 0.01)
  } else {
    x.new <- seq(boundary[1], boundary[2], by = 0.01)
  }
  degree <- rep(1:max.poly, each = length(x.new))
  predicted <- numeric(length = length(x.new) * max.poly)
  new.dat <- data.frame(x = rep(x.new, times = max.poly),
                        degree,
                        predicted)
  new.dat
}

# fitting lm() polynomials of increasing complexity
# (up to max.degree) and storing their predictions
# in the new.dat data.frame
fitModels <- function(Data, max.poly, boundary=NA) {
  new.dat <- createNewDat(Data, max.poly, boundary=boundary)

  if (any(is.na(boundary))) {
    x.new <- seq(min(Data$x), max(Data$x), by = 0.01)
  } else {
    x.new <- seq(boundary[1], boundary[2], by = 0.01)
  }

  for (i in 1:max.poly) {
    sub.dat <- new.dat[new.dat$degree == i, ]
    new.dat[new.dat$degree == i, 3] <- predict(lm(y~poly(x, i, simple=TRUE), data = Data),
                                            newdata = data.frame(x = x.new)
                                            )
  }
  new.dat
}

plotModels <- function(Data, max.poly){
  new.dat <- fitModels(Data, max.poly)
  # plotting the data and the fitted models
  p <- ggplot()
  p <- p + geom_point(aes(x, y), Data, colour = "darkgrey")
  p <- p + geom_line(aes(x, predicted,
                    colour = as.factor(degree)),
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

