# testing generate_figures

# R studio top right window
# Go to more>document
# Install and Restart


folder_i="C:/Users/thom927/Documents/Data/tethysDemeterOutputs"

out <- generate_figures(folder = folder_i,
                        temporal_scale = "annual")

mydata <- data.table::fread(paste0(folder_i, "/ssp1_rcp26_gfdl/wdtotal_km3peryr.csv"),
                            select=c(2,3,6,24), check.names = F)
mydata <- data.table::melt(mydata, id=1:2, variable.name="x")
mapx <- rmap::map(mydata, paletteDiff = "pal_div_GnBr", overLayer = rmap::mapIntersectGCAMBasin32Reg,
                  background = T, save=F,  xRef=2010, xDiff=c(2100), diffOnly = T, scaleRangeDiffPrcnt = c(0,1000))



