# testing generate_figures

# R studio top right window
# Go to more>document
# Install and Restart


folder_i="C:/Users/thom927/Documents/Data/tethysDemeterOutputs"
data_i="annual_data.rds"

out <- generate_figures(data = data_i,
                        folder = folder_i,
                        temporal_scale = "annual")

mydata <- data.table::fread(paste0(folder_i, "/ssp1_rcp26_gfdl/wdtotal_km3peryr.csv"),
                            select=c(2,3,6,24), check.names = F)
mydata <- data.table::melt(mydata, id=1:2, variable.name="x")
mapx <- rmap::map(mydata, paletteDiff = "pal_div_GnBr", overLayer = rmap::mapIntersectGCAMBasin32Reg,
                  background = T, save=F,  xRef=2010, xDiff=c(2100), diffOnly = T, scaleRangeDiffPrcnt = c(0,1000))

sector <- c(rep("Domestic", 5),
            rep("Electricity", 5),
            rep("Manufacturing", 5),
            rep("Mining", 5),
            rep("Livestock", 5),
            rep("Irrigation", 5))
year <- rep(seq(2010,2030,by=5), 6)
value <- runif(30)
dummy <- data.frame(sector, year, value)

sector_pal = c("Domestic"="royalblue4",
               "Electricity"="yellow3",
               "Manufacturing"="steelblue",
               "Mining"="cadetblue3",
               "Irrigation"="palegreen4",
               "Livestock"="yellowgreen")

p1 <- ggplot2::ggplot(dummy, ggplot2::aes(fill=sector, y=value, x=year)) +
  ggplot2::geom_bar(position="stack", stat="identity") +
  ggplot2::scale_fill_manual(values=sector_pal); p1

