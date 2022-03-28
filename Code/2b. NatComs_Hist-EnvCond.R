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
             "Mean temperature\nof the Coldest Month",
             "Mean temperature\nof the Warmest Month",
             "Temperature Seasonality",
             "Total Annual Precipitation",
             "Prec. Dryiest Month",
             "Prec. Wettest Month",
             "Precipitation Seasonality")


# Laod the Paleo data
PaleoData <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Paleo Predictors/EU-Lorents dowscale/EnvVarUse.RData")

#dev.new(width=12,height=9)
pdf("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/ClimateHistograms/ClimHist.pdf", width=12,height=9)
par(mar=c(4,5,3,3), mfrow=c(3,4),oma=c(0.5,0.5,1.5,0.5))

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
					  Tmp <- Tmp[[which(EnvVarUse%in%Var.Use)]]
						d1 <- density(na.omit(Tmp[]))
						d1$y <- scales::rescale(d1$y,to = c(0,1))
						d1	
					})

	## plot the density
	plot(Binford.d,

	     main=NA,
	     cex.main = 1.8,
	     cex.axis = 1.5,
	     #line.main = 2,
	     las = 1,
	     xlab=NA, ylab = NA,
	     xlim = range(c(sapply(LGM.d,function(x){min(x$x)}),
	                    sapply(LGM.d,function(x){max(x$x)}),
	                    range(Binford.d$x))),
	     ylim = c(0,1),
	     col="grey")
	mtext(VarName[which(EnvVarUse%in%Var.Use)], cex= 1.3, line=0.8, font=2)
		for(i in 1:length(LGM.d)){
		polygon(LGM.d[[i]],
				col = hcl.colors(length(LGM.d), palette = "viridis", alpha = 0.4)[i],
				border = hcl.colors(length(LGM.d), palette = "viridis", alpha = 0.6)[i])
	}
	polygon(Binford.d,
			col = "grey",
			density=40,
			border = "black")
		mtext("Density",
		  side=2,cex=1.1,
		  line = 3.5)	
}
legend("topright",
		inset=c(-1,0),
	fill= c("grey","#4B0055","#007094","#00BE7D","#FDE333"),
	legend=c("Current", "G. Stadial 2",
	                     "G. Interstadial 1",
	                     "G. Stadial 1",
	                     "Holocene"),
	xpd=NA,
	bty="n",cex=1.5)
dev.off()