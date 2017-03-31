# calculate AIC + BIC
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

# plot AIC + BIC
plot_aic_bic <- function(data) {
  p <- ggplot(data=data, aes(x=degree, y=value, group=measure, colour=measure))
  p <- p + geom_line(size=.8)
  p <- p + geom_point(size=3)
  p <- p + ggtitle(label="AIC and BIC")
  p <- p + xlab("Number of polynomials in the model") + labs(colour="Criterion")
  p <- p + theme_bw()
  p <- p + theme(panel.background=element_rect(fill="#F0F0F0"), # Set the entire chart region to a light gray color
                 plot.background=element_rect(fill="#F0F0F0"),
                 panel.grid.major=element_line(colour="#D0D0D0",size=.5), # Format the grid
                 panel.border=element_rect(colour="#F0F0F0"),
                 #panel.border = element_rect(size=1.1, colour="#535353", fill=NA),
                 axis.ticks=element_blank(),
                 axis.text.x = element_text(), 
                 axis.text.y = element_blank(), # Drop y axis text
                 axis.title.x = element_text(size = 15,colour = "#535353"), # Color axis titles
                 axis.title.y = element_blank(),
                 legend.background = element_rect(fill="#F0F0F0"), # Work legend
                 legend.title = element_text(size=14, colour="#535353"),
                 legend.text = element_text(size=14, colour="#535353"),
                 plot.title = element_text(hjust = .5, size=20, colour="#535353"))
  p <- p + scale_x_discrete(limits=c(1:max(data$degree))) # Scale x axis according to number of polynomials in the model
  # Add solid black line to bottom of the plot (only needed if panel.border not set)
  p <- p + geom_hline(yintercept=min(data$value) - 1,size=1,colour="#535353")

  return(p)
}