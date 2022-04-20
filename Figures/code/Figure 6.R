####################################################################################
####    Step 7. Plot the temporal trends in changes in LImiting factors  ####
####################################################################################
# Vars use 
EnvVarUse <- c("ET",#ff7f00
               "PET",#fdbf6f
               "NPP",#33a02c
               "MCM",#a6cee3
               #               "MWM",#fb9a99
               "TS",#6a3d9a
               "TAP", #e31a1c
               "PDM",#b2df8a
               "PWM",#1f78b4
               "PS")#cab2d6

ColUse <- c("#ff7f00",#"ET"
            "#fdbf6f",#"PET"
            "#33a02c",#"NPP",
            "#a6cee3",#"MCM"
            #           "#fb9a99",#"MWM"
            "#6a3d9a",#"TS"
            "#e31a1c",#"TAP", 
            "#b2df8a",#"PDM",
            "#1f78b4",#"PWM",
            "#cab2d6")#"PS"
# Load Ice Layers from ICE-6G-C
IceList <- readRDS(file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/IceMaps/ICE-6G-C/Ice0_5DD.RData")
# .................................................................................#
#          Estimate the 50-percentile model limiting factor                        #
# .................................................................................#

## load the LimFact50 estimate
LimFact50 <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/50Percent/LimFact50.RData")

LimFact50Lst <- lapply(1:length(LimFact50), 
                       function(i){
                         # Name of the period to use
                         PerUse <- names(IceList)[i]
                         #Load the limiting factors Raster
                         LimFact <- LimFact50[[i]][]
                         #Turn the limiting factors Raster to a factors table
                         LimFact <- LimFact[!is.na(LimFact)]
                         LimFact <- factor(LimFact,
                                           levels=1:length(EnvVarUse),
                                           labels = EnvVarUse)
                         # table of proportions
                         Prop <- round((table(LimFact)/sum(table(LimFact)))*100,2)
                         Out <-data.frame(Time = PerUse)
                         for( i in EnvVarUse){Out[,i] <- Prop[i]}
                         return(Out)
                       })

###############################################################################################
### Step 8. Plot changes in population desnity under Avg Env Condistions                    ###
###############################################################################################
EnvVarUse <- c("ET", "PET","NPP",
               "MCM",#"MWM",
               "TS",
               "Log10.TAP",
               "Log10.PDM", "Log10.PWM",
               "PS"
)
# load the Paleoclimatic data
PaleoClimRst <- readRDS('~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Paleo Predictors/EU-Lorents dowscale/EnvVarUse.RData')

# Estimate the Pop Density under the Avg Env Conditions for the 50-percentile model
# Load Models
qGAM50Per <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CrosValMod50.RData")

EnvVarSumm <- lapply(EnvVarUse, function(VarUse){#(VarUse<-"ET")
  
  # Define the summary for the Env Variable across Europe 
  VarTbl <- lapply(1:length(PaleoClimRst),
                   function(x){
                     EnvRast <- PaleoClimRst[[x]][[match(VarUse,names(PaleoClimRst[[x]]))]] # Env raster
                     IceRast <- IceList[[x]]==0 # Ice Rast The operation make the Ice location Zero
                     EnvRast <- EnvRast/IceRast
                     EnvRast[is.infinite(EnvRast[])] <- NA
                     mean(EnvRast[],na.rm=T)
                   })
  do.call("c", VarTbl)
})
names(EnvVarSumm) <- EnvVarUse

# estimate the Mean population density predicted by each predictor over the 100 cross validation folds 
PopDenPred <- lapply(EnvVarUse,#(VarUse<-"ET")
                     function(VarUse){
                       VarData <- EnvVarSumm[[VarUse]]
                       out <- lapply(qGAM50Per,
                                     function(y){
                                       10^as.numeric(predict(y[[VarUse]],newdata=list(Var=VarData)))
                                       
                                     })
                       apply(do.call("rbind",out),2, mean)
                     })
names(PopDenPred) <- EnvVarUse

PopDenSumm <- data.frame(Time = rev(seq(8,21,by=0.5))*-1,
                         do.call("cbind", PopDenPred))


#### Make the plots
EnvVarUse <- c("ET",#ff7f00
               "PET",#fdbf6f
               "NPP",#33a02c
               "MCM",#a6cee3
               #               "MWM",#fb9a99
               "TS",#6a3d9a
               "TAP", #e31a1c
               "PDM",#b2df8a
               "PWM",#1f78b4
               "PS")#cab2d6


pdf("~/Desktop/Fig_6.pdf", width = 8, height = 10)
### Plot the Barplots
par(mar=c(1,4,1,4),mfrow=c(2,1),oma=c(0,0,0,5))
# Build teh final summary for the tables
LimFact50Tbl <- do.call("rbind",LimFact50Lst)
row.names(LimFact50Tbl) <- LimFact50Tbl[,1]
# make the BarPlot of proportion of Europe where each variable is the limiting factor

a<-barplot(as.matrix(t(LimFact50Tbl[,-1][,order(apply(LimFact50Tbl[,-1],2,mean))])),
           col=ColUse[order(apply(LimFact50Tbl[,-1],2,mean))],
           axes=F,
           names.arg = rep(NA,27))
# add the y axis
axis(2,las=2)
mtext("Proportion (%)",side=2,line=2.5,cex=1,font=2)
# add the x axis
axis(1,at=a,labels = NA)
text(x=a-0.7,
     y=rep(-6,length(a)),
     labels = sprintf("%.2f",as.numeric(gsub("BP.","",names(IceList)))/100),
     xpd=NA,
     srt=45,
     pos=1)
mtext("Thosands of Years Before Preent (kaBP)",
      side=1,line=3,cex=1,font=2)
plot.window(xlim=c(0,1),ylim=c(0,1))
text(x=-0.15,y=1,
     labels = "A)",
     font=2,
     xpd=NA)
### Plot the trends
par(mar=c(4,4,4,4))
plot.new()
plot.window(xlim = c(-22,-7),
            ylim = round(range(PopDenSumm[,-1]) + c(-0.5,0.5)))
axis(1,
     at = seq(-21,-8,by=1),
     labels = NA)
mtext("Thosands of Years Before Preent (kaBP)",
      side=1,line=3,cex=1,font=2)
axis(2,
     las = 2,
     xpd = NA)
mtext("Popultion density estimate",
      side = 2,
      line = 2,
      font=2)

for(VarUse  in EnvVarUse){#(VarUse<-"NPP")
  PopDen <- data.frame(density = PopDenSumm[,VarUse],
                       time = PopDenSumm[,1])
  # Estimate the smoothed trend in change in population density over time.
  fit50 <- gam(density~s(time),
               data = PopDen)
  PopDen$mean <- predict(fit50)
  PopDen$UpCI <- predict(fit50)+(qnorm(0.975)*predict(fit50, se.fit=T)$se.fit)
  PopDen$LowCI <- predict(fit50)-(qnorm(0.975)*predict(fit50, se.fit=T)$se.fit)
  #---------------------------------------------------------
  # Plot the change on Estimated popultion desnity over time  
  polygon(x = c(PopDen$time,rev(PopDen$time)),
          y = c(PopDen$UpCI,rev(PopDen$LowCI)),
          col= paste0(ColUse[match(VarUse,EnvVarUse)],"80"),
          border=NA)
}

plot.window(xlim=c(-22,-7),ylim=c(0,1))
text(x = seq(-21,-8,by=1),
     y = rep(-0.15,3),
     cex=0.8,
     labels = paste0(seq(-22,-7,by=3)*-1,"kaBP"),
     srt=45,
     xpd=NA)

# Add the legebd
VarName <- c("Effective Temperature",
             "Potential Evapotraspiration",
             "Net Primary Productivity",
             "Mean temperature\nof the Coldest Month",
             #"Mean temperature\nof the Warmest Month",
             "Temperature Seasonality",
             "Total Annual Precipitation",
             "Prec. Dryiest Month",
             "Prec. Wettest Month",
             "Precipitation Seasonality")
legend("topright",
       legend=VarName,#gsub("Log10.","",EnvVarUse),
       fill=ColUse,
       inset=c(-0.32,0),
       xpd=NA,
       cex=0.9,
       bty="n")
plot.window(xlim=c(0,1),ylim=c(0,1))
text(x=-0.15,y=1,
     labels = "B)",
     font=2,
     xpd=NA)
dev.off()