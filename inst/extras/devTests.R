# testing generate_figures

# R studio top right window
# Go to more>document
# Install and Restart


folder_i="C:\\Users\\thom927\\Documents\\Data\\tethysDemeterOutputs"
temporal_scale_i = "annual"

out <- generate_figures(folder = folder_i,
                 temporal_scale = temporal_scale_i)


combos <- tidyr::crossing(SSP = c("SSP 1", "SSP 2", "SSP 3", "SSP 4", "SSP 5"),
                          RCP = c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5"),
                          GCM = c("gfdl", "hadgem", "ipsl", "miroc", "noresm")
)

combos <- dplyr::filter(combos, !(SSP=="SSP 3" & RCP=="RCP 2.6") & # if rcp26, exclude ssp3
                      !(SSP!="SSP 5" & RCP=="RCP 8.5") & # if rcp85, must use ssp5
                      !(RCP=="RCP 8.5" & GCM!="gfdl") # if rcp85, must use gfdl
)

annual <- tidyr::crossing(combos, year=seq(2010,2100,by=5))

folders <- dplyr::transmute(combos, name=tolower(
  (gsub(" |\\.", "",paste(SSP,RCP,GCM,sep="_"))))) # eg ssp1_rcp26_gfdl

paths <- dplyr::transmute(folders, path=paste0(
  folder_i, "\\", name, "\\wdtotal_km3peryr.csv"))[[1]]

#temp <- list.files(path=folder_i, full.names = TRUE, recursive = TRUE, pattern="*.csv")
DT <- do.call(rbind, lapply(paths, function(x){t(colSums(data.table::fread(x, drop=1:5)))}))
df <- cbind(annual, stack(data.frame(t(DT)))[1])
