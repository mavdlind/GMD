####code for fig 3 - top - of https://doi.org/10.5194/gmd-2019-125#### 
####load libraries#####
library(rio)
library(rcarbon)
library(tidyverse)
####load data####
####original dataset available from https://doi.org/10.1016/j.quaint.2015.06.022####
radiocarbon_data<-import("https://github.com/mavdlind/GMD/raw/master/balsera_iberia.csv")
####14C Summed Probability Distributions####
# setup
ncores <- 1
nsim<-100
realstartBCAD <- -8500
realendBCAD <- -500
bracket <- 1000
workingstartBP <- abs(realstartBCAD-1950)+bracket
workingendBP <- abs(realendBCAD-1950)-bracket
if (workingendBP<0){ workingendBP <- 0 }
workingstartCRA <- uncalibrate(workingstartBP)$ccCRA
workingendCRA <- uncalibrate(workingendBP)$ccCRA
if (workingendCRA<0){ workingendCRA <- 0 }
#calibrate dates and create SPDs
radiocarbon_calib<-calibrate(x=radiocarbon_data$C14AGE,errors=radiocarbon_data$C14STD,calCurves='intcal13', normalised=FALSE,verbose=FALSE)
#plot SPD
tiff(file="spd.tiff",width=1200,height=765,units="px",pointsize=24)
ymax <- max(radiocarbon_calib$grid$PrDens)*1.1
layout(matrix(c(1,2), 2, 1, byrow=TRUE), widths=4, heights=c(1.5,1.5))
par(mar=c(4.2,4.5,1,1))
plot(radiocarbon_calib, calendar="BP",xlim=c(9000,3500), ylim=c(0,ymax), xaxt="n",yaxt="n")
abline(v=seq(4000,8500,500), lty="dotted", col="white")
abline(v=seq(4000,8500,500), lty="dotted", col="white")
segments(x0=7500, y0=0, x1=7500, y1=(0.3*ymax), lwd=2,col="red",lty="solid") #intro neo
segments(x0=6250, y0=(ymax*0.05), x1=5750, y1=(0.05*ymax), lwd=3,lty="dotted") #first window
segments(x0=4250, y0=(0.05*ymax), x1=3750, y1=(0.05*ymax), lwd=3,lty="dotted") #second window
text(x=7500,y=(0.40*ymax),labels="Regional start of\n early farming",family="sans",ps=9,col="red")
text(x=6000,y=(0.1*ymax),labels="6k window",family="sans",ps=9)
text(x=4000,y=(0.1*ymax),labels="4k window",family="sans", ps=9)
axis(side=1, at=seq(3500,9000,500), labels=seq(3500,9000,500), las=2, family="sans",ps=9)
axis(side=2, family="sans", ps=9, at=seq(0,1.2,0.2),labels=seq(0,1.2,0.2))
ylab.text<-expression(paste(""^"14"," C probability density"))
title(xlab="years calBP", ylab=ylab.text,family="sans", ps=9)
dev.off()

