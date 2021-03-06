rm(list=ls());gc()
require(qgam)
require(mgcv)

####################################################################################
# Figure 1- visualize the q-gams
####################################################################################
# Load the Environmental data
#Binford_EnvDta_WorldClim <-read.csv("https://raw.githubusercontent.com/AlejoOrdonez/PaleoPopDen/main/Data/BindfordData/BinfordNewEnvDta_WorldClim.csv")
Binford_EnvDta_WorldClim <-read.csv("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Contemp Predictors/BinfordNewEnvDta_WorldClim.csv")


# Define the variables names to use 
EnvVarUse <- c("ET", "PET", #"MAT",
               "NPP",
               "MCM", "MWM",
               "TS",
               "Log10.TAP",
               "Log10.PDM", "Log10.PWM",
               "PS")

EnvVarName <- c("Effective Temperature",
                "Potential Evapotraspiration",
                "Net Primary Productivity",
                "Mean Temp Coldest Month",
                "Mean Temperture Warmest Month",
                "Temperture Sesonality",
                "Total Annual Precipitation",
                "Precip. Driest Month",
                "Precip. Wettest Month",
                "Precip. Sesonality")
expression(EnvVarName~mm/year)

UnitsUse <- c("°C",
              "mm per year",
              "gC per m^2 per year",
              "°C",
              "°C",
              "°C",
              "mm per year",
              "mm per month",
              "mm per month",
              "mm per month")

pdf("~/Desktop/Fig1.pdf",width=6,height=12)
par(mfrow=c(5,2),mar=c(3,1,2,2),oma=c(2,4,0,0))
for (Var.Use in EnvVarUse){#(Var.Use <- "ET")
  BinfordNewtmp <- Binford_EnvDta_WorldClim[,c("density", Var.Use)]
  names(BinfordNewtmp)[2]<-"Var"
  
  ## Build a quantile-GAM model to describe the change in density as a function of the predictor
  ### 90-percentile
  fit90 <- qgam(log10(density)~s(Var),
                data = BinfordNewtmp,
                qu = 0.90)
  ### 50-percentile
  fit50 <- gam(log10(density)~s(Var),
               data = BinfordNewtmp)
  ### 10-percentile
  fit10 <- qgam(log10(density)~s(Var),
                data = BinfordNewtmp,
                qu = 0.10)
  
  # Plot the relation of between a predictor a Population data
  plot(log10(density)~ Var,data = BinfordNewtmp,
       pch = 19,
       xlab = paste0(EnvVarName[EnvVarUse%in%Var.Use],"\n[",UnitsUse[EnvVarUse%in%Var.Use],"]"),
       main =  NA,
       cex.main = 1,
       axes = F,ylim = c(-1,3),
       ylab = NA,
       xpd=NA)
  if(Var.Use %in% c("ET","NPP","MWM","Log10.TAP","Log10.PWM")){
  # mtext(expression("People per 100"~km^2),
  #       side=2,
  #       cex=0.8,
  #       line=2.8)
    axis(2,
         at = -1:3,
         labels = 10^c(-1:3),
         las = 2)
  }
  else{
    axis(2,
         at = -1:3,
         labels = NA,
         las = 2)
    
  }
  box()
  axis(1)
  Var.predict <- seq(min(BinfordNewtmp$Var),
                     max(BinfordNewtmp$Var),
                     length.out = 100)
  # Predict and plot GAM model
  # MEan model
  fit50.Predict <- predict(fit50,
                           newdata = list(Var = Var.predict),
                           se.fit = T) 
  lines(x = Var.predict,
        y = fit50.Predict$fit,
        col = "red",lwd = 2)
  lines(x = Var.predict,
        y = fit50.Predict$fit+(qnorm(0.975) * fit50.Predict$se.fit),
        col = "red",lwd = 1,lty=2)
  lines(x = Var.predict,
        y = fit50.Predict$fit-(qnorm(0.975) * fit50.Predict$se.fit),
        col = "red",lwd = 1,lty=2)
  # Max model		 	
  fit90.Predict <- predict(fit90,
                           newdata = list(Var = Var.predict),
                           se.fit = T) 
  lines(x = Var.predict,
        y = fit90.Predict$fit,
        col = "blue",lwd = 2)
  lines(x = Var.predict,
        y = fit90.Predict$fit+(qnorm(0.975) * fit90.Predict$se.fit),
        col = "blue",lwd = 1,lty=2)
  lines(x = Var.predict,
        y = fit90.Predict$fit-(qnorm(0.975) * fit90.Predict$se.fit),
        col = "blue",lwd = 1,lty=2)
  
  #Min model
  fit10.Predict <- predict(fit10,
                           newdata = list(Var = Var.predict),
                           se.fit = T) 
  lines(x = Var.predict,
        y = fit10.Predict$fit,
        col = "purple",lwd = 2)
  lines(x = Var.predict,
        y = fit10.Predict$fit+(qnorm(0.975) * fit10.Predict$se.fit),
        col = "purple",lwd = 1,lty=2)
  lines(x = Var.predict,
        y = fit10.Predict$fit-(qnorm(0.975) * fit10.Predict$se.fit),
        col = "purple",lwd = 1,lty=2)
plot.window(xlim=c(0,1),ylim=c(0,1))
text(x = 0.05,
     y = 0.98,
     cex = 1.1,
     font = 2,
     labels = paste0(LETTERS[which(EnvVarUse%in%Var.Use)],")"),
     xpd =NA)
}
mtext(expression("People per 100"~km^2),
      side=2,
      cex=1.2,
      line=2,
      outer=T)

dev.off()