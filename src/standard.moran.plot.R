# standard.moran.plot
# function for Moran scatterplot using standardized variates
# specify variable = x
# specify spatial weights (listw object) = wfile
# all other options are hard coded (so far)
#
# Anselin, 2007, Spatial Regression Analysis in R - A Workbook, p.33
#

standard.moran.plot <- function(x,wfile)
{
  # get name of variable
  xname <- deparse(substitute(x))
  # standardized variable
  zx <- (x - mean(x))/sd(x)
  # spatial lag for the standardized variable
  wzx <- lag.listw(wfile,zx)
  # compute the intercept and slope (Moran's I) using lm
  morlm <- lm(wzx ~ zx)
  aa <- morlm$coefficients[1]
  mori <- morlm$coefficients[2]
  # scatterplot
  par(pty="s")
  plot(zx,wzx,xlab=xname,ylab=paste("Spatial Lag of ",xname))
  abline(aa,mori,col=2)
  abline(h=0,lty=2,col=4)
  abline(v=0,lty=2,col=4)
  title(paste("Moran Scatterplot I= ",format(round(mori,4))))
}