# Adam Griffin, 2019-03-18
# Code snippet for adding to shiny app to demonstrate growth curve at selected site.
# Uses precalculated parameters for Pearson Type III distribution
# Functions:
#   growthCurvePlot


#### SETUP ####
library(here)
library(lmom)
here()

growthCurvePlot <- function(x, type="gpa", conf=F,
                            pngfile=NULL, placename=NULL){
  
  # plots flood frequency curve for gpa or pe3
  # x       amax discharge data
  # type    gpa or pe3
  # conf    if TRUE, generate 95% confidence intervals. TO BE IMPLEMENTED
  # pngfile if not NULL, output to supplied path
  
  # require(lmom)
  yf <- function(f){ - log((1-f)/f) }  # Reduced logistic variate
  yfgum <- function(f){ - log(- log(f))} # Gumnbel variate
  
  x <- sort(x[!is.na(x)])
  n <- length(x)
  y <- -log(-log(((1:n) - 0.44)/(n + 0.12)))  
      # Gringorten plotting positions, as for GLO
  fx <- ((1:n) - 0.44)/(n + 0.12) # F_i = F(x_i)
  yfx <- yfgum(fx)  # y(F_i)
  
  # axes and labels
  xlm <- c(min(0,yfx), max(yfx))
  ylm <- c(min(0,x), max(x))
  ylb <- expression("Peak flow  " * (m^3/s))
  xlb <- expression("Gumbel reduced variate,  " * -log( -log(1 - (1/T))))
  
  fcurve <- seq(0,1, length.out=1000)[2:999]
  ycurve <- yfgum(fcurve)
  
  lx <- samlmu(x)
  #lx[1] <- median(x)
  
  #parameter estimation
  if(type=="gpa"){
    param <- lmom::pelgpa(lx)
    xcurve <- lmom::quagpa(fcurve,param)
  }else if(type=="pe3"){
    param <- lmom::pelpe3(lx)
    xcurve <- lmom::quape3(fcurve, param)
  }else if(type=="both"){
    param <- lmom::pelpe3(lx)
    paramGPA <- lmom::pelgpa(lx)
    xcurve <- lmom::quape3(fcurve, param)
    xcurveGPA <- lmom::quagpa(fcurve,paramGPA)
  }else{
    stop("Unknown distribution. Use gpa or pe3")
  }
  
  #graphing
  par(mar=c(4,4.3,1,1))
  plot(y, x, xlab = xlb, ylab = ylb, xlim = xlm, ylim = ylm, type='p', pch=4)
  lines(ycurve, xcurve)
  if(type=="both"){
    lines(ycurve, xcurveGPA, lty=3)
  }
  parusr <- par("usr")
  rp.lab <- c(2, 5, 10, 20, 50, 100, 200, 500, 1000, 10000, 
              1e+05, 1e+06, 1e+07, 1e+08)
  rp.tic <- -log(-log(1 - 1/rp.lab))
  crit <- (rp.tic >= parusr[1] & rp.tic <= parusr[2])
  rp.tic <- rp.tic[crit]
  rp.lab <- rp.lab[crit]
  rp.ypos <- parusr[3] + (parusr[4] - parusr[3]) * 0.05
  axis(side = 3, at = rp.tic, labels = rp.lab, pos = rp.ypos, cex.axis=0.8)
  text((min(rp.tic) + max(rp.tic)) * 0.5,
       rp.ypos + 0.9*(par("cxy")[2] * par("mgp")[1]),
       "Return period (years)",
       adj = c(0.5, 0), cex=0.8)
  legend("topleft", 
         legend=switch(type, "gpa"=c("GPA"),
                       "pe3"=c("PE3"),
                       "both"=c("GPA", "PE3")),
         col = 1,
         lty = if (type != "both") 1 else c(1,3),
         bty="n"
  )
  
  if (!is.null(pngfile)){
    dev.print(png, filename=pngfile, 
              width=100, height=100, 
              units='mm', res=300, 
              pointsize=9)
  }
  
  outlist <- list(place=placename,
                  amax=x,
                  pts=cbind(y,x),
                  curve=cbind(ycurve,fcurve))
  if (type == "gpa"){
    outlist$paramGPA <- param
  }else if (type == "pe3"){
    outlist$paramPE3 <- param
  }else{
    outlist$paramPE3 <- param
    outlist$paramGPA <- paramGPA
  }
  
  return(outlist)
}


## test of growthCurvePlot
i <- 10
amax_dir <- dir(here("data/AMAX_New"), full.names=T, pattern=".csv")
amax <- read.csv(amax_dir[i], header=T)
amax <- amax[,3]
place <- unlist(strsplit(amax_dir[i],split="_"))
place <- place[length(place)-1]
apath <- here("Graphs for reports/growth_curves2",
              paste0("shinytest", place, "_both_20190318.png"))

Z <- growthCurvePlot(amax, type='both', place=place)


## preprocessing for app to reduce computation overheads
amax_list <- lapply(1:length(amax_dir),
                function(i){read.csv(amax_dir[i], header=T)[,3]})
place_list <- lapply(1:length(amax_dir),
                function(i){tail(unlist(strsplit(amax_dir[i],split="_")),2)[1]})

param_list <- lapply(1:length(amax_dir),
                 function(i){
                   try(growthCurvePlot(x=amax_list[[i]],
                                       type='both',
                                       placename = place_list[[i]])
                       )
                   })

place_list <- unlist(place_list)
save(param_list, file="./data/param_list_app.RDa")
save(amax_list, param_list, place_list, file="./data/amax_list_app.RDa")
