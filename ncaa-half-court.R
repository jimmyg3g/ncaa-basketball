# https://gist.github.com/edkupfer/6354317
# NCAA courts are 50ft sideline-to-sideline
# and 94ft end to end. This will plot a court
# with the sidelines on the y-axis, centered 
# on the halfcourt circle. Measurement units
# are in feet. The basket at the bottom is
# centered on {x = 0, y = -41.75} and at the
# top  on {x = 0, y = 41.75}

library(lattice)
getHalfCourt <- function() { 
    xyplot(NA~NA,ylab='',xlab='',aspect='iso',xlim=c(-25,25),ylim=c(0,47),
           ### remove outside box and ticks, there must be a better way:
           par.settings = list(axis.line = list(col = "transparent")), par.box = c(col = "transparent"),
           scales=list(draw=F),
           panel=function(...){
               #outside box:
               panel.lines(x=c(-25,-25,25,25,-25),y=c(0,47,47,0,0))
               #halfcourt line
               panel.lines(x=c(-25,25),y=c(0,0))
               #solid FT semicircle above FT line:
               panel.lines(x=c(-6000:(-1)/1000,1:6000/1000),y=c(-28+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2)))
               panel.lines(x=c(-6000:(-1)/1000,1:6000/1000),y=c(28-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2)))
               #paint:
               panel.lines(x=c(-6,-6,6,6,-6),y=c(-47,-28,-28,-47,-47))
               panel.lines(x=c(-6,-6,6,6,-6),y=c(47,28,28,47,47))
               # large halfcourt semicircle:
               panel.lines(x=c(-6000:(-1)/1000,1:6000/1000),y=c(0-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2)))
               panel.lines(x=c(-6000:(-1)/1000,1:6000/1000),y=c(0+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2)))
               # small halfcourt semicircle:
               panel.lines(x=c(-2000:(-1)/1000,1:2000/1000),y=c(0-sqrt(2^2-c(-2000:(-1)/1000,1:2000/1000)^2)))
               panel.lines(x=c(-2000:(-1)/1000,1:2000/1000),y=c(0+sqrt(2^2-c(-2000:(-1)/1000,1:2000/1000)^2)))
               # rim:
               panel.lines(x=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000),y=c(c(1.25+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(1.25-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))-43)
               panel.lines(x=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000),y=c(c(1.25-sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(1.25+sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))+40.5)
               panel.rect(-5/24,-43,5/24,54/12-47)
               panel.rect(-5/24,43,5/24,47-54/12)
               #backboard:
               panel.lines(x=c(-3,3),y=c(-43,-43))
               panel.lines(x=c(-3,3),y=c(43,43))
               #three-point line:
               panel.lines(x=c(-20.75,-20.75,-20750:(-1)/1000,1:20750/1000,20.75,20.75),y=c(-47,-41.75,-41.75+sqrt(20.75^2-c(-20750:(-1)/1000,1:20750/1000)^2),-41.75,-47))
               panel.lines(x=c(-20.75,-20.75,-20750:(-1)/1000,1:20750/1000,20.75,20.75),y=-c(-47,-41.75,-41.75+sqrt(20.75^2-c(-20750:(-1)/1000,1:20750/1000)^2),-41.75,-47))
           })
}

