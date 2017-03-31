###### Plots ######

plotCrossValidation <- function(seData){
  
  p <- ggplot(data=seData, aes(x=degree, y=mse))
  p <- p + geom_line(size=1.2, colour="#999999")
  p <- p + geom_errorbar(aes(x=degree, ymin=mse-se, ymax=mse+se), data = seData, width=0.25)
  p <- p + xlab("Degree of polynomial") + ylab("Mean Squared error")
  p <- p + ggtitle(label="Results of crossvalidation")
  # Set the entire chart region to a light gray color
  p <- p + theme_bw()
  p <- p + theme(panel.background=element_rect(fill="#F0F0F0"),
                 plot.background=element_rect(fill="#F0F0F0"),
                 panel.border=element_rect(colour="#F0F0F0"),
                 #panel.border = element_rect(size=1.1, colour="#535353", fill=NA),
                 panel.grid.major=element_line(colour="#D0D0D0",size=.5), # Format grid
                 axis.ticks=element_blank(),
                 axis.text.x = element_text(), # Drop y axis text, color axis titles, add plot border
                 axis.text.y = element_blank(),
                 axis.title = element_text(size=15, colour="#535353"),
                 legend.position = "none", # Drop legend
                 plot.title = element_text(hjust = .5, size=20, colour="#535353")) # Work title
  p <- p + scale_x_discrete(limits=c(1:max(seData$degree)))
  # Add solid black line to bottom of the plot (only needed if panel.border not set)
  p <- p + geom_hline(yintercept=min(seData$mse) - 1, size=1, colour="#535353")
  
  return(p)
}