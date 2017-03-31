# the figures require ggplot2 library and
# all packages it depends on
library(ggplot2)
library(polynom)
library(RColorBrewer)

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
  #colors <- c(brewer.pal(9,"Blues"),"#000000")
  colors <- brewer.pal(10, "RdYlGn")
  names(colors) <- rep(1:10)
  lim_y_min <- -50000; lim_y_max <- 50000
  # plotting the data and the fitted models
  p <- ggplot()
  p <- p + geom_point(aes(x, y), Data, colour = "darkgrey")
  # Set the entire chart region to a light gray color
  p <- p + theme_bw()
  p <- p + theme(panel.background=element_rect(fill="#F0F0F0"),
                 plot.background=element_rect(fill="#F0F0F0"),
                 panel.border=element_rect(colour="#F0F0F0"))
  # Format the grid
  p <- p + theme(panel.grid.major=element_line(colour="#D0D0D0",size=1), axis.ticks=element_blank())
  # Drop axis text, color axis titles
  p <- p + theme(axis.text.x = element_blank(), 
                 axis.text.y = element_blank(), 
                 axis.title = element_text(size = 11,colour = "#535353"))
  # Set y limits to keep the grid fixed
  p <- p + ylim(lim_y_min,lim_y_max)
  # Add solid black line to bottom of the plot
  p <- p + geom_hline(yintercept=lim_y_min,size=.5,colour="#535353")
  # Work legend
  p <- p + theme(legend.background = element_rect(fill="#F0F0F0"), 
                 legend.title = element_text(size=8, colour = "#535353"),
                 legend.text = element_text(size = 9, colour = "#535353"),
                 legend.position = "center")
  degree <- 0
  for (f in estimated_functions) {
    degree <- degree + 1
    text <- as.character(degree)
    p <- p + stat_function(data = data.frame(x = -20:20,
                                             polynomial = rep(text, 41)),
                           fun = f,
                           aes(colour = polynomial))
  }
  p <- p + scale_color_manual(values=colors,
                              name="Degree",
                              position="right")
  p
}

plotGenerativeModel <- function(poly_vec, noise=NA, min=-20, max=20){
  f <- as.function(polynomial(poly_vec))
  pol <- as.character(polynomial(poly_vec))
  x <- data.frame(x=seq(min, max, 0.01))
  y <- f(x)
  upper <- as.vector(y) + noise
  lower <- as.vector(y) - noise
  d <- data.frame(x=x, y=y, upper=upper, lower=lower)
  p <- ggplot(data=x, aes(x=x))
  p <- p + stat_function(fun=f) #geom_line(aes(x, y), d)
  p <- p + geom_ribbon(aes(x=x, ymax=upper, ymin=lower), d, alpha="0.5")
  p <- p + annotate('text', x = 0, y = max(upper), label=pol, parse=TRUE)#ggtitle(tits)
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
  n <- length(n_bins) * max.poly * nrow(data)
  result <- data.frame()

  for (bin in 1:n_bins) {
    split <- split_data(data, bin)

    estimated_functions <- fitModels(split$train, max.poly)

    test_df <- split$test
    test_df <- test_df[order(test_df$x),]
    rownames(test_df) <- 1:nrow(test_df)

    degree <- 0
    for (f in estimated_functions) {
      degree <- degree + 1
      tmp <- test_df
      tmp$estimate <- f(tmp$x)
      tmp$degree <- degree

      result <- rbind(result, tmp)
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
