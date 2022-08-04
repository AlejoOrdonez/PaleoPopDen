rm(list = ls());gc()
require(raster)
BinfordNewEnvDta <-read.csv("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Contemp Predictors/BinfordNewEnvDta_WorldClim.csv")
names(BinfordNewEnvDta)

EnvVarUse <- c("ET", "PET","NPP",
               "MCM", "MWM",
               "TS",
               "Log10.TAP",
               "Log10.PDM", "Log10.PWM",
               "PS")

VarOrder <- EnvVarUse
VarName <- c("Effective Temperature",
             "Potential Evapotraspiration",
             "Net Primary Productivity",
             "Mean Temp Coldest Month",
             "Mean Temp Warmest Month",
             "Temperature Seasonality",
             "Total Annual Precipitation",
             "Precip. Driest Month",
             "Precip. Wettest Month",
             "Precip. Seasonality")
UnitsUse <- c("°C",
              "mm per Yr",
              "gC per m^2 per year",
              "°C",
              "°C",
              "°C",
              "mm per yr",
              "mm per month",
              "mm per month",
              "mm per month")

# Laod the Paleo data
PaleoData <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Paleo Predictors/EU-Lorents dowscale/EnvVarUse.RData")

#dev.new(width=12,height=9)
#pdf("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/ClimateHistograms/ClimHist.pdf", width=12,height=9)
pdf("~/Desktop/Fig_2.pdf", width=3.5,height=8)
par(mfrow=c(5,2), mar=c(2.5,0.5,1.5,1),oma=c(0,4,0,0))#mar=c(3,2,2,2), oma=c(1,3,0,0))

for (Var.Use in VarOrder){#(Var.Use <- "ET")
# Current Density
	BinfordNewtmp <- BinfordNewEnvDta[,c("density", Var.Use)]
	names(BinfordNewtmp)[2]<-"Var"
	BinfordNewtmp <- BinfordNewtmp[complete.cases(BinfordNewtmp),]
	Binford.d <- density(BinfordNewtmp$Var)
	Binford.d$y <- scales::rescale(Binford.d $y,to = c(0,1))

# Past Density
	LGM.d <- lapply(c("BP.2050",#LGM
	                  "BP.1450",#BA-strat
	                  "BP.1300",#BA-end YD-start
	                  "BP.1000"),# Holo
	                
					function(x){#(x<-"BP.2050")
					  Tmp <- PaleoData[[x]]
					  Tmp <- Tmp[[Var.Use]]
						d1 <- density(na.omit(Tmp[]))
						d1$y <- scales::rescale(d1$y,to = c(0,1))
						d1	
					})

	## plot the density
	plot(Binford.d,
	     main=NA,
	     las = 1,
	     xlab=NA,
	     ylab = NA,
	     xlim = range(c(sapply(LGM.d,function(x){min(x$x)}),
	                    sapply(LGM.d,function(x){max(x$x)}),
	                    range(Binford.d$x))),
	     ylim = c(0,1),
	     col="grey",
	     xpd=NA,
	     axes=F)
	# Add the Axes
	box()
	axis(1,labels = NA)
	axis(1,cex.axis=0.6,line = -0.5 ,tick=F)
	if(Var.Use %in% c("ET","NPP","MWM","Log10.TAP","Log10.PWM")){
	  axis(2,
	       las = 2,cex.axis=0.8)
	}
	else{
	  axis(2,
	       labels = NA)
	}
# Add the units
	mtext(UnitsUse[EnvVarUse%in%Var.Use],1,line=1.2,cex=0.5)
	mtext(VarName[EnvVarUse%in%Var.Use],3,line=0.5,cex=0.6,font =2)
	 # 
		#plot the density plots
		for(i in 1:length(LGM.d)){
		polygon(LGM.d[[i]],
				col = hcl.colors(length(LGM.d), palette = "viridis", alpha = 0.4)[i],
				border = hcl.colors(length(LGM.d), palette = "viridis", alpha = 0.6)[i])
	}
	polygon(Binford.d,
			col = "grey",
			density=40,
			border = "black")
# pannel title
	# mtext(VarName[which(EnvVarUse%in%Var.Use)],
	#       cex= 0.7,
	#       line=0.8)
# y-axis label
	# if(which(VarOrder%in%Var.Use)%%2==1){
	# mtext("Density",
	#       side=2,cex=0.6,
	#       line = 3)	}
# panel label
		plot.window(xlim=c(0,1),y=c(0,1))
		text(x = 0.02,
		     y = 0.95,
		     cex = 0.8,
		     font = 2,
		     labels = paste0(LETTERS[which(EnvVarUse%in%Var.Use)],")"),
		     xpd =NA)
		}
legend("topright",
		#inset=c(-1,0),
	fill= c("grey","#4B0055","#007094","#00BE7D","#FDE333"),
	legend=c("Current", "G. Stadial 2",
	                     "G. Interstadial 1",
	                     "G. Stadial 1",
	                     "Holocene"),
	xpd=NA,
	bty="n",cex=0.6)
mtext("Density",
      side=2,
      cex=0.8,
      line=2,
      outer=T)
dev.off()