# testing generate_figures

# R studio top right window
# Go to more>document
# Install and Restart

library(rmap)
library(magrittr)
library(jgcricolors)
library(dplyr)

images = r"{C:\Users\thom927\Documents\metarepos\khan-etal_2022_tethysSSPRCP\webpage\images\}"
folder = "C:/Users/thom927/Documents/Data/tethysDemeterOutputs"
GCAM_withdrawals_csv = "C:/Users/thom927/Documents/Data/GrahamGCAM/water_withdrawals_by_mapping_source.csv"
GCAM_consumption_csv = "C:/Users/thom927/Documents/Data/GrahamGCAM/water_consumption_by_mapping_source.csv"


out <- generate_figures(annual_rds = "annual_data.rds",
                        monthly_rds = "monthly_data.rds",
                        folder = "C:/Users/thom927/Documents/Data/tethysDemeterOutputs",
                        temporal_scale = "all")


# map testing


mydata <- get_data(folder = folder, scenarios = "ssp1_rcp26_gfdl",
                   sectors = c("total"),
                   years=2010, months=0)

rm <- rmap::map(mydata, save=F, show=F, background = T, legendType="continuous",
                crop_to_underLayer = T,
                underLayer = rmap::mapCountries,
                overLayer =  rmap::mapCountries)

a <- rm[[1]] + ggplot2::scale_fill_gradientn(
  colors = rev(jgcricol()$pal_spectral),
  rescaler = ~(.x/max(.x))^0.25,
  guide="none")

grDevices::png(paste0(images,"total.png"),width=13,height=7,units="in",res=288); print(a); grDevices::dev.off()


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

###
file_data <- data.table::fread(r"{C:\Users\thom927\Documents\Data\tethysDemeterOutputs\ssp1_rcp26_gfdl\twddom_km3permonth.csv}", drop=2:5)

file_data <- dplyr::group_by(file_data, dplyr::across(c(-month, -value)))
file_data <- dplyr::summarise(file_data, value = sum(value))

##

annual_withdrawals <- readRDS("../data/annual_data.rds")
annual_withdrawals<- dplyr::filter(annual_withdrawals, Sector!="Total" & Sector!="Non-Agriculture")
annual_withdrawals$type <- "withdrawals"

annual_consumption <- readRDS("../data/consumption_sums.rds")
annual_consumption <- dplyr::filter(annual_consumption, sector %in% c("dom", "elec", "mfg", "min", "liv", "irr"))
annual_consumption <- dplyr::group_by(annual_consumption, dplyr::across(c(-region, -basin, -value)))
annual_consumption <- dplyr::summarise(annual_consumption, value = sum(value))
annual_consumption <- dplyr::ungroup(annual_consumption)
annual_consumption <- dplyr::mutate(annual_consumption, SSP = paste0("SSP ", substr(scenario, 4, 4)),
                                    RCP = paste0("RCP ", substr(scenario, 9, 9), ".", substr(scenario, 10, 10)),
                                    GCM = substr(scenario, 12, 17))
annual_consumption <- dplyr::mutate(annual_consumption, Sector = c("dom" = "Domestic",
                                                                   "elec" = "Electricity",
                                                                   "mfg" = "Manufacturing",
                                                                   "min" = "Mining",
                                                                   "irr" = "Irrigation",
                                                                   "liv" = "Livestock")[sector])
annual_consumption$type <- "consumption"
annual_consumption <- dplyr::select(annual_consumption, SSP, RCP, GCM, Sector, type, Year=year, Value=value)

annual_data <- dplyr::bind_rows(annual_consumption, annual_withdrawals)
annual_data2 <- dplyr::filter(annual_data, Sector != "Irrigation")

p2 <- ggplot2::ggplot(data = annual_data2,
                      ggplot2::aes(x = Year,
                                   y = Value,
                                   group = interaction(GCM, Sector, type))) +
  ggplot2::geom_line(ggplot2::aes(linetype=type, color=Sector)) +
  ggplot2::scale_color_manual(values=water_pal) +
  ggplot2::scale_linetype_manual(values=c("dashed", "solid")) +
  ggplot2::facet_grid(RCP~SSP,scales="fixed") +
  ggplot2::ggtitle("Global Annual Water Withdrawal/Consumption by SSP-RCP-GCM and Sector") +
  ggplot2::xlab("Year") +
  ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p2


scenario_names = c("ssp1_rcp26_gfdl"   = "SSP 1, RCP 2.6, gfdl",
                   "ssp2_rcp45_hadgem" = "SSP 2, RCP 4.5, hadgem",
                   "ssp3_rcp60_ipsl"   = "SSP 3, RCP 6.0, ipsl",
                   "ssp4_rcp45_miroc"  = "SSP 4, RCP 4.5, miroc",
                   "ssp5_rcp85_noresm" = "SSP 5, RCP 8.5, noresm")

#####


m1 = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
m2 = c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

a <- get_data(folder=folder, scenarios = "ssp1_rcp26_gfdl",
              sectors = "min", years = 2050:2075, months = 2:4)

a <- dplyr::arrange(a, scenario, sector, Grid_ID, month, year)
a <- dplyr::mutate(a, interp_value = dplyr::case_when(year %% 400 == 0 ~ value*366/m2[month],
                                                      year %% 100 == 0 ~ value*365/m1[month],
                                                      year %% 20 == 0 ~ value*366/m2[month],
                                                      year %% 5 == 0 ~ value*365/m1[month],
                                                      TRUE ~ NA_real_))
a <- dplyr::mutate(a, interp_value = zoo::na.approx(interp_value))
a <- dplyr::mutate(a, interp_value = dplyr::case_when(year %% 400 == 0 ~ interp_value*m2[month]/366,
                                                      year %% 100 == 0 ~ interp_value*m1[month]/365,
                                                      year %% 4 == 0 ~ interp_value*m2[month]/366,
                                                      TRUE ~ interp_value*m1[month]/365))
a <- dplyr::mutate(a, diff = interp_value - value)
a <- dplyr::mutate(a, pctdiff = (interp_value - value)/value)

##
b <- get_data(folder=folder, scenarios = "ssp1_rcp26_gfdl",
              sectors = "dom", years = 2055:2060, months = 2:4)

b <- dplyr::arrange(b, scenario, sector, Grid_ID, month, year)
b <- dplyr::mutate(b, interp_value = dplyr::case_when(year %% 5 == 0 ~ value,
                                                      TRUE ~ NA_real_))
b <- dplyr::mutate(b, interp_value = zoo::na.approx(interp_value))

b <- dplyr::mutate(b, diff = interp_value - value)
b <- dplyr::mutate(b, pctdiff = (interp_value - value)/value)

print(range(b$diff))

b2 <- dplyr::filter(b, diff != 0)


###

sheep1 <- data.table::fread(r"{C:\Users\thom927\Documents\Data\example_v1_3_0\Input\harmonized_inputs\livestock_sheep.csv}")
goat1 <- data.table::fread(r"{C:\Users\thom927\Documents\Data\example_v1_3_0\Input\harmonized_inputs\livestock_goat.csv}")

baa <- dplyr::bind_cols(rmap::mapping_tethys_grid_basin_region_country, sheep1, goat1)
baa <- dplyr::group_by(baa, regionName)
baa <- dplyr::summarise(baa, sheep=sum(sheep), goat=sum(goat))
baa <- dplyr::mutate(baa, gfrac = goat/(sheep+goat))


