###### Plots ######

plotCrossValidation <- function(seData){
  
  p <- ggplot(data=seData, aes(x=degree, y=mse, colour=degree))
  p <- p + geom_line()
  p <- p + geom_errorbar(aes(x=degree, ymin=mse-se, ymax=mse+se), data = seData, width=0.25)
  p <- p + xlab("Degree of polynomial") + ylab("Mean Squared error")
  p <- p + ggtitle(label="Results of crossvalidation")
  p <- p + theme(legend.position = "none")
  p
}