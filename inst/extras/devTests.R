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
crops= c("biomass",
         "Corn",
         "FiberCrop",
         "FodderGrass",
         "FodderHerb",
         "MiscCrop",
         "OilCrop",
         "OtherGrain",
         "PalmFruit",
         "Rice",
         "RootTuber",
         "SugarCrop",
         "Wheat")

pal_all <- jgcricol()$pal_all
mypal <- pal_all[names(pal_all) %in% crops]
names(mypal)[names(mypal)=="RootTuber"] <- "Root_Tuber"

data_year <- 2100

files <- paste0(folder, "/", "ssp1_rcp26_gfdl", "/crops_wdirr_",
               names(crop_pal), "_km3peryr.csv")
grid_data <- data.table::rbindlist(lapply(files, function(x){ # stacks long
  return (data.table::fread(x, select=c(2, 3, 6+(data_year-2010)/5),
                            col.names=c("lon", "lat", "value"))) }))
grid_data <- dplyr::bind_cols(class=rep(names(crop_pal), each=67420), grid_data)

gd <- dplyr::filter(grid_data, class == "biomass" | class=="Rice")

rmap::map(data=gd, overLayer=rmap::mapCountries, background=T, save=F)


6+(data_year-2010)/5

rc <- dplyr::filter(region_crops, Crop=="Rice" & scenario=="ssp1_rcp26_gfdl" & Year==2100)

grid_rice <- data.table::fread("crops_wdirr_Rice_km3peryr.csv",
                               select=c(1,24), col.names=c("GRID_ID", "Value"))
gr <- dplyr::mutate(grid_rice, .keep ="unused",
                    subRegion_GCAMBasin = gridLookup$basinName[GRID_ID],
                    subRegion_GCAMReg32 = gridLookup$regionName[GRID_ID])
gr <- dplyr::group_by(gr, subRegion_GCAMBasin, subRegion_GCAMReg32)
gr <- dplyr::summarise(gr, Value=sum(Value))

fj <- dplyr::full_join(rc, gr)
fj <- dplyr::mutate(fj, diff=Value-GCAM_Value)

reg_crops <- dplyr::filter(region_crops, scenario=="ssp1_rcp26_gfdl"&Year==2100)
reg_crops <- dplyr::group_by(reg_crops, subRegion_GCAMBasin, subRegion_GCAMReg32, Crop, Year)
reg_crops <- dplyr::summarise(reg_crops, GCAM_Value=sum(GCAM_Value))

gridLookup <- readRDS("gridLookup.rds")
files <- paste0(folder, "/", "ssp1_rcp26_gfdl", "/crops_wdirr_",
                names(crop_pal), "_km3peryr.csv")
grid_data <- data.table::rbindlist(lapply(files, function(x){ # stacks long
  return (data.table::fread(x, select=c(1, 24),
                            col.names=c("GRID_ID", "value"))) }))
grid_data <- dplyr::bind_cols(Crop=rep(names(crop_pal), each=67420), grid_data)

grid_data <- dplyr::mutate(grid_data,
                           subRegion_GCAMBasin = gridLookup$basinName[GRID_ID],
                           subRegion_GCAMReg32 = gridLookup$regionName[GRID_ID])
grid_data <- dplyr::group_by(grid_data, Crop, subRegion_GCAMBasin, subRegion_GCAMReg32)
grid_data <- dplyr::summarise(grid_data, gridValue=sum(value))

reg_v_grid_crops <- dplyr::full_join(reg_crops, grid_data)
reg_v_grid_crops <- dplyr::mutate(reg_v_grid_crops, diff=gridValue-GCAM_Value)





ad <- readRDS("annual_crops.rds")
ad <- dplyr::filter(ad, GCM == "gfdl" & Year %in% c(2010, 2100))
ad <- dplyr::group_by(ad, Crop)
ad <- dplyr::summarise(ad, Value=sum(Value))
