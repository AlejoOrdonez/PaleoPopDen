# Get the estimates of the Mean Population size
rm(list=ls());gc()
require(raster)
require(mgcv)
# Load Ice Layers from ICE-6G-C
IceList <- readRDS(file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/IceMaps/ICE-6G-C/Ice0_5DD.RData")

PaleoPopDenEst50 <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/50Percent/PaleoPopDenEst50.RData")
## Estimate the Min PopDensity
PopDenFinalRast <- lapply(names(PaleoPopDenEst50), function(y){
  PopDen <- PaleoPopDenEst50[[y]] # Load pop density estimates
  # Remove MWM -  Might not be necessary latter on
  if(sum(names(PopDen)%in%"MWM")!=0){
    PopDen <- PopDen[[-which(names(PopDen)%in%"MWM")]]}
  Ice <- IceList[[y]]==0 # load Ice sheets
  PopDenCrop <- PopDen*Ice
  PopDenCrop[is.infinite(PopDenCrop[])] <- NA
  # calculate the Mim Popultion size the Limming factor Variable
  calc(PopDenCrop,function(x){min(x)})
})
PopDenFinalList <- lapply(PopDenFinalRast,function(x){
  mean(x[],na.rm=T)})
PopDen <- data.frame(density = do.call("rbind", PopDenFinalList),
                     time = seq(-21,-8,by=0.500))

# Estimate the smoothed trend in change in population density over time.
fit50 <- gam(density~s(time),
             data = PopDen)
PopDen$mean <- predict(fit50)
PopDen$UpCI <- predict(fit50)+(qnorm(0.975)*predict(fit50, se.fit=T)$se.fit)
PopDen$LowCI <- predict(fit50)-(qnorm(0.975)*predict(fit50, se.fit=T)$se.fit)

## Get the Population size estimates Based on Schmidt_et_al_2021
Schmidt <- read.csv("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Baseline - Proxy/PopDen_data_Schmidt_et_al_2021.csv")
Schmidt <- cbind(Time = round(apply(Schmidt[1:5,1:2],1,mean)/-1000,2),Schmidt [1:5,-c(1:2)])

## Number of records per period in the INQUA radio carbon dates dbs
#pnas <- read.csv("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Baseline - Proxy/pnas.1503784112.sd03.csv")
# Summ <- table(round(pnas$X14C_age/1000))
# Summ <- data.frame( age = -11:-27,
# Sum = as.numeric(Summ))

INQUA <- read.csv("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Baseline - Proxy/InquaData_V28.csv")
Summ <- table(round(INQUA$X14C_age/1000))
Summ <- Summ[as.numeric(names(Summ))<21 & as.numeric(names(Summ))>7]
Summ <- data.frame( age = -1*as.numeric(names(Summ)),
                    Sum = as.numeric(Summ))


pdf("~/Desktop/Fig_3.pdf",
    width=4.5,height=7.5)
#dev.new(width=6,height=10)
par(oma=rep(1,4),mar=c(4,4,1,1))
layout(rbind(matrix(1,3,3,byrow=T),matrix(2:10,3,3,byrow=T)))

### PLot the mean estimated Density
plot(density ~ time,data = PopDen,
     pch = 19,cex=0.8,
     type="b",
     axes = F,
     ylim = c(0.5,2),#round(range(do.call("rbind", PopDenFinalList))+c(-0.6,0.6)),
     xlim = c(-22,-7),
     ylab = NA,
     xlab=NA, cex.lab=1.5,
     xpd=NA)
# Add Axes and legends
axis(1,
     at=seq(-22,-7,by=3),
     labels = paste0(seq(-22,-7,by=3)*-1,"kaBP"))
axis(2,las=2)
mtext("Mean density [#people/100km2]",side=2,cex=0.8,line=2.5)

# Draw the smoothed changes in population density
lines(x = PopDen$time,
      y = predict(fit50, se.fit=T)$fit,
      lty = 1, col = "black", lwd = 1)
lines(x= PopDen$time,
      y= predict(fit50, se.fit=T)$fit-(qnorm(0.975)*predict(fit50, se.fit=T)$se.fit),
      lty = 2,col = "black", lwd = 1)
lines(x= PopDen$time,
      y= predict(fit50, se.fit=T)$fit+(qnorm(0.975)*predict(fit50, se.fit=T)$se.fit),
      lty = 2,col = "black", lwd = 1)

## LGM termination	  
abline(v=-20, lty=3)
text(x = -19,
     y = 2,
     cex=0.8,
     adj = 0.5,
     labels = "Greenland stadial 2\ntermination",     
     xpd=NA,
     srt = 45)

# Bølling–Allerød warming
abline(v=c(-14.69), lty=4)	
text(x = mean(c(-14.69,-12.89)),
     y = 2,#3,
     cex=0.8,
     adj = 0.5,
     srt = 45,
     labels = "Greenland\ninterstadial 1",
     xpd=NA)

# Younger Dryas cooling
abline(v=c(-12.9), lty=4)	
text(x = mean(c(-12.9,-11.7)),
     y = 2,#4,
     cex=0.8,
     adj = 0.5,
     srt = 45,
     labels = "Greenland\nstadial 1",
     xpd=NA)
# Holocene start
abline(v=-11.7, lty=5)	
text(x = -10.65,
     y = 2,
     cex=0.8,
     adj = 0.5,
     srt = 45,
     labels = "Holocene start",
     xpd=NA)

# plot Schmidt_et_al_2021 Core area Population desnisty
points(x=Schmidt$Time,
       y= scales::rescale(Schmidt$PopDenMEAN,to=c(1.3,1.8), from=range(Schmidt[-1])),
       pch=19,col="blue",
       xlim = c(-22,-8),
       ylim = c(-1,0.5),type="b")
polygon(x = c(Schmidt$Time,rev(Schmidt$Time)),
        y = c(scales::rescale(Schmidt$PopDenLOW,to=c(1.3,1.8), from=range(Schmidt[-1])), rev(scales::rescale(Schmidt$PopDenHIGH,to=c(1.3,1.8),from=range(Schmidt[-1])))),
        col = rgb(0,0,1,0.6), border = "blue")
#Add the axis and legend
axis(2,
     at = scales::rescale(c(0,0.2,0.4),to=c(1.3,1.8), from=range(Schmidt[-1])),
     labels = c(0,0.2,0.4),
     cex.axis = 0.8,
     col="blue",las=2,line=-2.5,col.axis="blue")
text(x=-20.4, y = scales::rescale(0.2,to=c(1.3,1.8), from=range(Schmidt[-1])),
     labels = "Core area\npopulation size estimates\n [#people/100km2]",
     srt=90,adj = 0.5,
     cex=0.8,
     xpd=NA,
     col="blue")

## PLot INQUA Number of radio carbon dates dbs
points(x = Summ[4:dim(Summ)[1],1],
       y = scales::rescale(Summ[4: dim(Summ)[1],2],to=c(0.5,1.1)),
       ylim = scales::rescale(c(0,1000),to=c(0.5,1.1)),
       pch = 19, col = "red",
       type = "b")
#Add the axis and legend
axis(4,
     at = scales::rescale(c(0,250,500,750,1000),to=c(0.5,1.1)),
     labels = F,col="red",
     line=-7,
     xpd=NA)
text(x=-9.5, y = mean(c(0.5,1.1)),
     cex=0.8,
     labels = "Archeological Population\nproxy (Relative units)",
     srt=90,adj = 0.5,
     xpd=NA,
     col="red")
	 
###############################################################################################
### 				    Plot changes in key Enviormnegtal variables    						###
###############################################################################################
EnvVarUse <- c("ET", "PET","NPP",
               "MCM",#"MWM",
               "TS",
               "Log10.TAP",
               "Log10.PDM", "Log10.PWM",
               "PS"
               )
ColUse <- c("#ff7f00",#"ET"
            "#fdbf6f",#"PET"
            "#33a02c",#"NPP",
            "#a6cee3",#"MCM"
            #"#fb9a99",#"MWM"
            "#6a3d9a",#"TS"
            "#e31a1c",#"TAP", 
            "#b2df8a",#"PDM",
            "#1f78b4",#"PWM",
            "#cab2d6")#"PS"               
EnvVarName <- c("Effective Temperature",
                "Potential Evapotraspiration",
                "Net Primary Productivity",
                "Mean Temp Coldest Month",
                #"Mean Temp Warmest Month",
                "Temperature Seasonality",
                "Total Annual Precipitation",
                "Precip. Driest Month",
                "Precip. Wettest Month",
                "Precip. Seasonality")
UnitsUse <- c("C",
				"mm/Yr",
				"gC/m2/year",
               "C",
               #"C",
               "C",
               "Log10 mm/yr",
               "Log10 mm/month",
               "Log10 mm/month",
               "No Units")
               
# Laod the Paleocliatic data
PaleoClimRst <- readRDS('~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Paleo Predictors/EU-Lorents dowscale/EnvVarUse.RData')
# Load Ice Layers from ICE-6G-C
IceList <- readRDS(file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/IceMaps/ICE-6G-C/Ice0_5DD.RData")

par(mar=c(2,4,0.5,1))
for (Var in EnvVarUse){#(Var <- "ET")Var <- "Log10.PDM" Var <-"Log10.PWM"
# Define the sumary acorss Europe 
	VarTbl <- lapply(1:length(PaleoClimRst),
				function(x){
					EnvRast <- PaleoClimRst[[x]][[match(Var,names(PaleoClimRst[[1]]))]] # Env raster
					IceRast <- IceList[[x]]==0 # Ice Rast The operation make the Ice loction Zero
					EnvRast <- EnvRast#/IceRast
					#EnvRast[is.infinite(EnvRast[])] <- NA
					mean(EnvRast[],na.rm=T)})
	VarSumm <- data.frame(Time = rev(seq(8,21,by=0.5))*-1,
	                      Var = do.call("c", VarTbl))
# Estimate the smoothed trend in change in population desity over time.	 
	fit50 <- gam(Var~s(Time),
	            data = VarSumm)
	VarSumm$mean <- predict(fit50)
	VarSumm $UpCI <- predict(fit50)+(qnorm(0.975)*predict(fit50, se.fit=T)$se.fit)
	VarSumm $LowCI <- predict(fit50)-(qnorm(0.975)*predict(fit50, se.fit=T)$se.fit)
# Plot the Relation
	plot(x=VarSumm$Time,
	     y=VarSumm$Var,
	     xlim=c(-22,-7),
	     ylim=range(VarSumm[,-1]),
	     type = "b",
	     cex=0.6,
	     pch=19,col= ColUse[match(Var,EnvVarUse)],
	     xlab=NA,ylab=NA,
	     axes=F)
mtext(	EnvVarName[match(Var,EnvVarUse)], font=2, cex=0.6,line=0.5)
# Correlation to Men pop Density
# legend("topleft",
	  # legend = paste0("r2 = ",round(cor(VarSumm$Var, PopDen$density, method = "spearman"),3)), bty= "n",cex=1.5)

#Add the axis and legend	 
	
	axis(1,
		 at=seq(-22,-7,by=3),
		 labels = NA)		 
	axis(2,
		 #col= ColUse[match(Var,EnvVarUse)], col.axis = ColUse[match(Var,EnvVarUse)],
		 las=2,xpd=NA,
		 cex.axis = 0.8)
	mtext(UnitsUse [match(Var,EnvVarUse)],
		  side = 2,
		  line = 2.5,
		  cex = 0.6)
		  #col = ColUse[match(Var,EnvVarUse)])

#Add the Smoothed trend 
	lines(x = VarSumm$Time,
	      y = VarSumm$mean,
	      lty = 1, col = ColUse[match(Var,EnvVarUse)], lwd = 0.8)
	lines(x= VarSumm$Time,
	      y= VarSumm$UpCI,
	      lty = 2,col = ColUse[match(Var,EnvVarUse)], lwd = 0.8)
	lines(x= VarSumm$Time,
	      y= VarSumm$LowCI,
	      lty = 2,col = ColUse[match(Var,EnvVarUse)], lwd = 0.8)

	if(Var%in%c("Log10.PDM", "Log10.PWM","PS")){
		plot.window(xlim=c(-22,-7),ylim=c(0,1))
		text(x = seq(-22,-7,by=3),
			 y = rep(-0.2,3),
		labels = paste0(seq(-22,-7,by=3)*-1,"kaBP"),
		srt=45,
		cex=0.7,
		xpd=NA)}
}
dev.off()