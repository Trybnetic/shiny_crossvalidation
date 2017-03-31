calc_aic_bic <- function(max.poly, data) {
  # df to store AIC + BIC values
  df <- data.frame(measure=c(rep("AIC",max.poly),rep("BIC",max.poly)),
                   value=numeric(max.poly * 2),
                   degree=rep(1:max.poly, 2))
  
  # AIC + BIC over the max.poly models
  for(i in 1:max.poly){
    df[i,2] <- AIC(lm(y~poly(x,i), data))
    df[i+max.poly,2] <- BIC(lm(y~poly(x,i), data))
  }
  return(df)
}

plot_aic_bic <- function(data) {
  p <- ggplot(data=data, aes(x=degree, y=value, group=measure, colour=measure))
  p <- p + geom_line()
  # p <- p + scale_color_manual(values=c(col1, col2))
  p <- p + xlab("Degree") + ylab("Value") + labs(colour="Criterion")
  return(p)
}