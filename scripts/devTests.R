# testing generate_figures

# R studio top right window
# Go to more>document
# Install and Restart

library(rmap)
library(magrittr)
library(jgcricolors)

images = r"{C:\Users\thom927\Documents\metarepos\khan-etal_2022_tethysSSPRCP\webpage\images\}"
folder = "C:/Users/thom927/Documents/Data/tethysDemeterOutputs"
GCAM_withdrawals_csv = "C:/Users/thom927/Documents/Data/GrahamGCAM/water_withdrawals_by_mapping_source.csv"

out <- generate_figures(annual_rds = "annual_data.rds",
                        monthly_rds = "monthly_data.rds",
                        folder = "C:/Users/thom927/Documents/Data/tethysDemeterOutputs",
                        temporal_scale = "all")


# map testing


mydata <- get_data(folder = folder, scenarios = "ssp1_rcp26_gfdl",
                   sectors = c("total"), #regions=c("USA"),
                   years=2020, months=0)


rm <- rmap::map(mydata, save=F, show=F, background = T, legendType="continuous",
                crop_to_underLayer = T,
                underLayer = rmap::mapCountries,
                overLayer =  rmap::mapCountries)

a <- rm[[1]] + ggplot2::scale_fill_gradientn(
  colors = rev(jgcricol()$pal_spectral),
  rescaler = ~(.x/max(.x))^0.25); a


a <- rm[[1]] + ggplot2::scale_fill_viridis_c(
  rescaler = ~(.x/max(.x))^0.25); a


a <- rm[[1]] + ggplot2::scale_fill_gradientn(
  colors = rev(jgcricol()$pal_spectral),
  trans = scales::trans_new(name = '4th root',
                            transform = function(x){x^0.25},
                            inverse = function(x){x^4}),
  breaks = round(seq(0,max(mydata$value*0.99, na.rm = T)^0.25,length.out=5)^4, 2))


# make the png not bad
grDevices::png(paste0(images,"total.png"),width=13,height=7, units="in",res=300); print(a); grDevices::dev.off()


mydata <- get_data(folder = folder, scenarios = "ssp1_rcp26_gfdl",
                   sectors = names(crop_pal), #c("Rice", "Corn", "Wheat"),
                   years = c(2010,2050,2100), months=0)
mydata2 <- dplyr::select(mydata, lon, lat, Year=year, Sector=sector, value)
mydata2 <- dplyr::mutate(mydata2, value=dplyr::if_else(value <= 0, NA_real_, value))

rm <- rmap::map(mydata2, save=F, show=F, background=T, legendType="continuous",
                crop_to_overLayer = T,
                underLayer = rmap::mapGCAMReg32,
                overLayer = rmap::mapGCAMReg32,
                col="Year", row="Sector")
b <- rm[[1]] + ggplot2::scale_fill_gradientn(
  colors = rev(jgcricol()$pal_spectral),
  trans = scales::trans_new(name = '4th root',
                            transform = function(x){x^0.25},
                            inverse = function(x){x^4}),
  breaks = round(seq(0,max(mydata2$value*0.99, na.rm = T)^0.25,length.out=5)^4, 2))

grDevices::png(paste0(images,"test.png"),width=13,height=21,units="in",res=300); print(b); grDevices::dev.off()


#
mydata <- get_data(folder = folder, scenarios = "ssp1_rcp26_gfdl",
                   sectors = names(crop_pal), #c("Rice", "Corn", "Wheat"),
                   years = c(2049, 2050), months=NULL)
mydata <- dplyr::filter(mydata, (year==2049&month==12|year==2050&month!=12))
mydata1 <- dplyr::mutate(mydata, season = c("Dec, Jan, Feb",
                                            "Mar, Apr, May",
                                            "Jun, Jul, Aug",
                                            "Sep, Oct, Nov")[floor((month%%12)/3)+1]) # needs levels

mydata1 <- dplyr::group_by(mydata1, lon, lat, sector, season)
mydata2 <- dplyr::summarise(mydata1, value=sum(value))
mydata2 <- dplyr::select(mydata2, lon, lat, season, Sector=sector, value)
mydata2 <- dplyr::mutate(mydata2, value=dplyr::if_else(value <= 0, NA_real_, value))

rm <- rmap::map(mydata2, save=F, show=F, background=T, legendType="continuous",
                crop_to_overLayer = T,
                underLayer = rmap::mapGCAMReg32,
                overLayer = rmap::mapGCAMReg32,
                col="season", row="Sector")
b <- rm[[1]] + ggplot2::scale_fill_gradientn(
  colors = rev(jgcricol()$pal_spectral),
  trans = scales::trans_new(name = '4th root',
                            transform = function(x){x^0.25},
                            inverse = function(x){x^4}),
  breaks = round(seq(0,max(mydata2$value*0.99, na.rm = T)^0.25,length.out=5)^4, 2))

grDevices::png(paste0(images,"test.png"),width=15,height=21,units="in",res=100); print(b); grDevices::dev.off()


data1 <- data.table::fread(GCAM_withdrawals_csv)
data2 <- dplyr::filter(data1, endsWith(scenario, "woclimate"))
data3 <- dplyr::filter(data2, value < 0)
data4 <- dplyr::filter(data1, value < 0)
data5 <- dplyr::filter(data1, value < 0 & scenario == "ssp1_rcp60_woclimate")

####
indus_data1 <- get_data(folder = folder, scenarios = "ssp1_rcp26_gfdl",
                       sectors = "Rice", months = 0,
                       regions = "South Africa")

indus_data <- get_data(folder = folder, scenarios = "ssp1_rcp26_gfdl",
                       sectors = names(crop_pal),
                       basins = c("Indus"))

idp <- dplyr::filter(indus_data, value > 0)
idp <- dplyr::group_by(idp, sector, Grid_ID, year)
idp <- dplyr::mutate(idp, value = value/sum(value))
p <- ggplot2::ggplot(data = idp,
                       ggplot2::aes(x = month,
                                    y = value,
                                    group = interaction(year, Grid_ID))) +
  ggplot2::geom_line(ggplot2::aes(color=sector)) +
  ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p

