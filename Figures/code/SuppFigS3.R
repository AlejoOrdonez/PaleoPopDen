###############################################################################################
###     Map Mean Population density 
###############################################################################################
rm(list=ls());gc()
require(raster)
require(mgcv)
require(rnaturalearth)
require(rnaturalearthdata)
require(latticeExtra)
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]
# Get the estimates of the Mean Population size
PopDenFinalRast <- readRDS('~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/10Percent/MinPopPopDen10.RData')

PopDenMap <- stack(PopDenFinalRast[[1]],
                   mean(do.call("stack",lapply(14:17,function(x){PopDenFinalRast[[x]]})),na.rm=T),
                   mean(do.call("stack",lapply(17:19,function(x){PopDenFinalRast[[x]]})),na.rm=T),
                   mean(do.call("stack",lapply(20:23,function(x){PopDenFinalRast[[x]]})),na.rm=T),
                   mean(do.call("stack",lapply(23:27,function(x){PopDenFinalRast[[x]]})),na.rm=T))

# Load Ice Layers from ICE-6G-C
IceList <- readRDS(file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/IceMaps/ICE-6G-C/Ice0_5DD.RData")
IceList <- stack(IceList[[1]],
                 IceList[[14]],
                 IceList[[17]],
                 IceList[[20]],
                 IceList[[24]])
pdf("~/Desktop/SuppFigS3.pdf",width=9,height = 9)                
print(
  rasterVis::levelplot(PopDenMap,
                       margin=F,
                       main=list(expression(bold("People per 100"~km^2)),side=1,line=-0.5),
                       names.attr = c("Greenland Stadial 2",
                                      "Greenland Interstadial 1",
                                      "Greenland Stadial 1",
                                      "Holocene initiation",
                                      "Early Holocene"),
                       col.regions = rev(hcl.colors(100, palette = "RdYlBu")),
                       colorkey=list(at=seq(0,1,length.out=101),
                                     labels = list(at = seq(0,1,by=0.2)),
                                     col=rev(hcl.colors(100, palette = "RdYlBu")))) + 
    rasterVis::levelplot(IceList, col.regions =c(NA,"grey"),add=T, title="NA") + 
    layer(sp.polygons(as(Europe[,"adm0_dif"],Class = "Spatial"), lwd=1)))
dev.off()