# Making multiple plots in one graph
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Extract Quantities of Interest for Plotting Zelig simulations
# For ranges, basicallly 
mv_extract <- function(s.out){
  myev <- s.out$get_qi(qi='ev', xvalue = 'range')
  myev2 <- as.data.frame(matrix(unlist(myev), nrow =1000)) # Default is 1000 sims
  #This step is to create quantiles
  a<- apply(myev2, 2, quantile, probs = c(0.025,0.975, 0.25, 0.75)) 
  low <- a[1,]
  high <- a[2,]
  qt.1 <- a[3,]
  qt.3 <- a[4,]
  mean <- apply(myev2, 2, mean) 
  plotdata <- as.data.frame(cbind(low, high, mean, qt.1, qt.3))
  return(plotdata)
}

# For Dummies, basically
mv_extract_single <- function(s.out){
  myev <- s.out$get_qi(qi='ev')
  myev2 <- as.data.frame(matrix(unlist(myev), nrow =1000)) # Default is 1000 sims
  #This step is to create quantiles
  a<- apply(myev2, 2, quantile, probs = c(0.025,0.975, 0.25, 0.75)) 
  low <- a[1,]
  high <- a[2,]
  qt.1 <- a[3,]
  qt.3 <- a[4,]
  mean <- apply(myev2, 2, mean) 
  plotdata <- as.data.frame(cbind(low, high, mean, qt.1, qt.3))
  return(plotdata)
}
