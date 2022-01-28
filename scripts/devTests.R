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


# map debugging


mydata <- get_data(folder = folder, scenarios = "ssp1_rcp26_gfdl",
                   sectors = c("irr"), #regions=c("USA"),
                   years=2020, months=0)

#mydata <- dplyr::mutate(mydata, value=dplyr::if_else(value==0, NA_real_, value))

rm <- rmap::map(mydata, save=F, show=F, background = T, legendType="continuous",
                crop_to_underLayer = T,
                underLayer = rmap::mapCountries,
                overLayer =  rmap::mapCountries)

a <- rm[[1]] + ggplot2::scale_fill_gradientn(
  colors = rev(jgcricol()$pal_spectral),
  trans = scales::trans_new(name = '4th root',
                            transform = function(x){x^0.25},
                            inverse = function(x){x^4}),
  breaks = round(seq(0,max(mydata$value*0.99, na.rm = T)^0.25,length.out=5)^4, 2)); a

# make the png not bad
grDevices::png(paste0(images,"total.png"),width=13,height=7, units="in",res=300); print(a); grDevices::dev.off()


GCAM <- readRDS("region_data.rds")
GCAM <- dplyr::rename(GCAM, basin=subRegion_GCAMBasin, region=subRegion_GCAMReg32, sector=class, GCAM_value=value)

tethys <- readRDS("region_sums_wd.rds")
tethysa <- dplyr::filter(tethys, !sector %in% names(crop_pal))
tethysa <- dplyr::mutate(tethysa, sector = c(dom = "Domestic",
                                             elec= "Electricity",
                                             irr = "Irrigation",
                                             liv = "Livestock",
                                             mfg = "Manufacturing",
                                             min = "Mining")[sector])
a <- dplyr::full_join(GCAM, tethysa)

mydata <- get_data(folder = "C:/Users/thom927/Documents/Data/example_v1_3_0/Output", scenarios = "test_example",
                   sectors = c("irr"), basin = "Indus",
                   years=2010, months=0)

rm <- rmap::map(mydata, save=F, show=T, background = T, legendType="kmeans", zoom = -0.5,
                underLayer = rmap::mapCountries,
                overLayer =  rmap::mapCountries)
