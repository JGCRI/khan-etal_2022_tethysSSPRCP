# testing generate_figures

# R studio top right window
# Go to more>document
# Install and Restart

folder = "C:/Users/thom927/Documents/Data/tethysDemeterOutputs"
GCAM_withdrawals_csv = "C:/Users/thom927/Documents/Data/GrahamGCAM/water_withdrawals_by_mapping_source.csv"

out <- generate_figures(annual_rds = "annual_data.rds",
                        monthly_rds = "monthly_data.rds",
                        folder = "C:/Users/thom927/Documents/Data/tethysDemeterOutputs",
                        temporal_scale = "all")


# map debugging


mydata <- get_data(folder = folder, scenarios = "ssp1_rcp26_gfdl",
                   sectors = c("dom"), #basin=c("Indus"),
                   years=2010, months=0)



md2 <- dplyr::mutate(mydata, value = (value/max(mydata$value))^0.25)

rm <- rmap::map(mydata, save=F, show=T, background = T, zoom=-0.5, legendType="kmeans",
                underLayer = rmap::mapCountries,
                overLayer =  rmap::mapCountries)

filename <- paste0(folder, "/ssp1_rcp26_gfdl/crops_twdirr_Rice_km3permonth.csv")
file_data <- data.table::fread(cmd=paste0("grep ,.*,.*,.*,.*,.*[1-9] ", filename))
file_data <- data.table::fread(filename)

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
