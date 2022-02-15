#' generate_figures
#'
#' Generate package figures
#' @param data Default = NULL. rds file of preprocessed data, overrides folder
#' @param folder Default = NULL. Path to folder with tethys outputs
#' @param temporal_scale Default = "annual". Option between "annual", "monthly" or "all"
#' @keywords figures
#' @export
#' @importFrom magrittr %>%
#' @examples
#'\dontrun{
#' library(khanetal2022tethysSSPRCP)
#' khanetal2022tethysSSPRCP::generate_figures()
#'}

generate_figures <- function(folder=NULL,
                             annual_rds=NULL,
                             monthly_rds=NULL,
                             temporal_scale="annual") {
  NULL -> Year -> Value -> GCM -> Sector -> SSP -> RCP

  secabbs <- c("Domestic" = "dom", "Electricity" = "elec", "Manufacturing" = "mfg", 
               "Mining" = "min", "Irrigation" = "irr", "Livestock" = "liv")

  water_pal = c("Domestic"="dodgerblue",
                "Electricity"="darkslateblue",
                "Manufacturing"="#cef4d1",
                "Mining"="grey75",
                "Irrigation"="forestgreen",
                "Livestock"="goldenrod2")
  
  sec_pal = c("dom"="dodgerblue",
                "elec"="darkslateblue",
                "mfg"="#cef4d1",
                "min"="grey75",
                "irr"="forestgreen",
                "liv"="goldenrod2")

  gcm_pal = c("gfdl" = "gold",
              "hadgem" = "darkorange",
              "ipsl" = "red3",
              "miroc" = "deeppink",
              "noresm" = "darkviolet")

  crop_pal = c("Corn" = "gold3",
               "FiberCrop" = "gold4",
               "MiscCrop" = "darkorange4",
               "OilCrop" = "gray20",
               "OtherGrain" = "indianred2",
               "PalmFruit" = "firebrick3",
               "Rice" = "steelblue2",
               "Root_Tuber" = "mediumpurple",
               "SugarCrop" = "yellow2",
               "Wheat" = "burlywood",
               "FodderHerb" = "darkseagreen4",
               "FodderGrass" = "mediumseagreen",
               "biomass" = "#00931d")
  
  all_pal = c("dom"="dodgerblue",
              "elec"="darkslateblue",
              "mfg"="#cef4d1",
              "min"="grey75",
              #"irr"="forestgreen",
              "liv"="goldenrod2",
              "Corn" = "gold3",
              "FiberCrop" = "gold4",
              "MiscCrop" = "darkorange4",
              "OilCrop" = "gray20",
              "OtherGrain" = "indianred2",
              "PalmFruit" = "firebrick3",
              "Rice" = "steelblue2",
              "Root_Tuber" = "mediumpurple",
              "SugarCrop" = "yellow2",
              "Wheat" = "burlywood",
              "FodderHerb" = "darkseagreen4",
              "FodderGrass" = "mediumseagreen",
              "biomass" = "#00931d")

  # Initialize
  print("Starting generate_figures ...")


  # Check if folder exists
  # Check if required files exist (sectors total, irr etc.)


  if(grepl("annual|all",temporal_scale,ignore.case=T)){

    # get all annual data

    if (is.null(annual_rds)) {
      if (is.null(folder)) {
        print("Provide prepared .rds file or tethys outputs folder")
      } else {
        print(paste("Preparing raw tethys data from", folder))
        annual_data <- prepare_annual(folder)
        print("To speed up subsequent runs, use annual_rds='annual_data.rds'")
      }
    } else {
      print(paste("Reading data from", annual_rds))
      annual_data <- readRDS(annual_rds)
    }

    # Annual plots

    # Figure 1: Global Faceted line plot showing
    # Facets: SSP (cols), RCP (rows)
    # Lines: GCMs are colored lines
    # X axis: Years
    # Aggregated to all grid cells (sector = total)

    print("Building Figure 1")
    data1 <- dplyr::filter(annual_data, names(Sector) == "total")
    p1 <- ggplot2::ggplot(data = data1,
                          ggplot2::aes(x = Year,
                                       y = Value,
                                       group = GCM)) +
      ggplot2::geom_line(ggplot2::aes(color=GCM)) +
      ggplot2::scale_color_manual(values=gcm_pal) +
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Total Global Annual Water Withdrawal by SSP-RCP-GCM") +
      ggplot2::xlab("Year") +
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p1
    ggplot2::ggsave(filename =  "figure1.png",
                    plot = p1,
                    width = 13,
                    height = 10) # save plot

    # Figure 2: Global Faceted line plot showing
    # Facets: SSP (cols), RCP (rows)
    # Lines: Sectors are colored lines (multiple GCMS same color)
    # X axis: Years
    # Aggregated to all grid cells (sectors = dom,elec,mfg,min,irr,liv)
    print("Building Figure 2")
    data2 <-  dplyr::filter(annual_data, (names(Sector)!="total") &
                              (names(Sector)!="nonag"))
    p2 <- ggplot2::ggplot(data = data2,
                          ggplot2::aes(x = Year,
                                       y = Value,
                                       group = interaction(GCM, Sector))) +
      ggplot2::geom_line(ggplot2::aes(color=Sector)) +
      ggplot2::scale_color_manual(values=water_pal) +
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Global Annual Water Withdrawal by SSP-RCP-GCM and Sector") +
      ggplot2::xlab("Year") +
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p2
    ggplot2::ggsave(filename =  "figure2.png",
                    plot = p2,
                    width = 13,
                    height = 10) # save plot

    # Figures 3a-e: Global Faceted line plot showing
    # Facets: SSP (cols), RCP (rows)
    # Bars: Sectors are stacked colored bars (GCMs are figures 3a-e)
    # X axis: Years
    # Aggregated to all grid cells (sectors = dom,elec,mfg,min,irr,liv)

    # ggprotos common to all figures 3
    base_fig3 <- list(ggplot2::aes(x = Year, y = Value, fill = Sector),
      ggplot2::geom_bar(position="stack", stat="identity"),
      ggplot2::facet_grid(RCP~SSP,scales="fixed"),
      ggplot2::scale_fill_manual(values=water_pal),
      ggplot2::xlab("Year"),
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))),
      ggplot2::theme_bw(),
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5))
      )

    print("Building Figure 3a")
    data3a <-  dplyr::filter(annual_data, (GCM=="gfdl") &
                               (names(Sector)!="total") &
                               (names(Sector)!="nonag"))
    p3a <- ggplot2::ggplot(data = data3a) +
      ggplot2::ggtitle("Global Annual Water Withdrawal by SSP-RCP-Sector, GCM: gfdl") +
      base_fig3; p3a
    ggplot2::ggsave(filename = "figure3a.png",
                    plot = p3a,
                    width = 13,
                    height = 10) # save plot

    print("Building Figure 3b")
    data3b <-  dplyr::filter(annual_data, (GCM=="hadgem") &
                               (names(Sector)!="total") &
                               (names(Sector)!="nonag"))
    p3b <- ggplot2::ggplot(data = data3b) +
      ggplot2::ggtitle("Global Annual Water Withdrawal by SSP-RCP-Sector, GCM: hadgem") +
      base_fig3; p3b
    ggplot2::ggsave(filename = "figure3b.png",
                    plot = p3b,
                    width = 13,
                    height = 10) # save plot

    print("Building Figure 3c")
    data3c <-  dplyr::filter(annual_data, (GCM=="ipsl") &
                               (names(Sector)!="total") &
                               (names(Sector)!="nonag"))
    p3c <- ggplot2::ggplot(data = data3c) +
      ggplot2::ggtitle("Global Annual Water Withdrawal by SSP-RCP-Sector, GCM: ipsl") +
      base_fig3; p3c
    ggplot2::ggsave(filename = "figure3c.png",
                    plot = p3c,
                    width = 13,
                    height = 10) # save plot

    print("Building Figure 3d")
    data3d <-  dplyr::filter(annual_data, (GCM=="miroc") &
                               (names(Sector)!="total") &
                               (names(Sector)!="nonag"))
    p3d <- ggplot2::ggplot(data = data3d) +
      ggplot2::ggtitle("Global Annual Water Withdrawal by SSP-RCP-Sector, GCM: miroc") +
      base_fig3; p3d
    ggplot2::ggsave(filename = "figure3d.png",
                    plot = p3d,
                    width = 13,
                    height = 10) # save plot

    print("Building Figure 3e")
    data3e <-  dplyr::filter(annual_data, (GCM=="noresm") &
                               (names(Sector)!="total") &
                               (names(Sector)!="nonag"))
    p3e <- ggplot2::ggplot(data = data3e) +
      ggplot2::ggtitle("Global Annual Water Withdrawal by SSP-RCP-Sector, GCM: noresm") +
      base_fig3; p3e
    ggplot2::ggsave(filename = "figure3e.png",
                    plot = p3e,
                    width = 13,
                    height = 10) # save plot

    data6 <- readRDS("../data/annual_crops.rds")
    p6 <- ggplot2::ggplot(data = data6,
                          ggplot2::aes(x = Year,
                                       y = Value,
                                       group = interaction(GCM, Crop))) +
      ggplot2::geom_line(ggplot2::aes(color=Crop)) + #linetype=GCM
      ggplot2::scale_color_manual(values=crop_pal) +
      #ggplot2::scale_linetype_manual(values=c("solid", "longdash", "dashed", "dotdash", "dotted"))+
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Annual Crop Water Withdrawal by SSP-RCP-GCM") +
      ggplot2::xlab("Year") +
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p6
    ggplot2::ggsave(filename =  paste0(images, "figure6.png"),
                    plot = p6,
                    width = 13,
                    height = 10) # save plot
  }

  # Monthly plots
  if(grepl("monthly|all",temporal_scale,ignore.case=T)){
    # get all monthly data

    if (is.null(monthly_rds)) {
      if (is.null(folder)) {
        print("Provide prepared .rds file or tethys outputs folder")
      } else {
        print(paste("Preparing raw tethys data from", folder))
        monthly_data <- prepare_monthly(folder)
        print("To speed up subsequent runs, use monthly_rds='monthly_data.rds'")
      }
    } else {
      print(paste("Reading data from", monthly_rds))
      monthly_data <- readRDS(monthly_rds)
    }

    # all sectors
    print("Building Figure 4a")
    data4a <- dplyr::filter(monthly_data, (GCM=="gfdl")&(Year %in% c(2010, 2100)))
    data4a <- dplyr::mutate(data4a, Year=factor(Year), Month=factor(Month, labels=month.abb))
    p4a <- ggplot2::ggplot(data = data4a,
                          ggplot2::aes(x = Month,
                                       y = Value,
                                       group = interaction(GCM, Sector, Year))) +
      ggplot2::geom_line(ggplot2::aes(linetype=Year, color=Sector)) +
      ggplot2::scale_color_manual(values=water_pal) +
      ggplot2::scale_linetype_manual(values=c("solid", "longdash"))+
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Global Monthly Water Withdrawal by Year") +
      ggplot2::xlab("Month") +
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p4a
    ggplot2::ggsave(filename =  "figure4a.png",
                    plot = p4a,
                    width = 13,
                    height = 10) # save plot

    # non-irrigation
    print("Building Figure 4b")
    data4b <- dplyr::filter(monthly_data, (Sector!= "Irrigation")&(GCM=="gfdl")&(Year %in% c(2010, 2100)))
    data4b <- dplyr::mutate(data4b, Year=factor(Year), Month=factor(Month, labels=month.abb))
    p4b <- ggplot2::ggplot(data = data4b,
                           ggplot2::aes(x = Month,
                                        y = Value,
                                        group = interaction(GCM, Sector, Year))) +
      ggplot2::geom_line(ggplot2::aes(linetype=Year, color=Sector)) +
      ggplot2::scale_color_manual(values=water_pal) +
      ggplot2::scale_linetype_manual(values=c("solid", "longdash"))+
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Global Monthly Water Withdrawal by Year") +
      ggplot2::xlab("Month") +
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p4b
    ggplot2::ggsave(filename =  "figure4b.png",
                    plot = p4b,
                    width = 13,
                    height = 10) # save plot

    # Crops
    monthly_crops <- readRDS("../data/monthly_crops.rds")

    filter_out_crops <- c("PalmFruit", "FodderGrass", "Root_Tuber", "FodderHerb", "OtherGrain")
    filter_crops <- c("Rice", "Wheat", "FiberCrop", "SugarCrop", "biomass")
    data5 <- dplyr::filter(monthly_crops, GCM == "gfdl" & Year %in% c(2010, 2100) &
                             Crop %in% filter_crops)
    data5 <- dplyr::mutate(data5, Year=factor(Year), Month=factor(Month, labels=month.abb))
    p5 <- ggplot2::ggplot(data = data5,
                           ggplot2::aes(x = Month,
                                        y = Value,
                                        group = interaction(GCM, Crop, Year))) +
      ggplot2::geom_line(ggplot2::aes(linetype=Year, color=Crop)) +
      ggplot2::scale_color_manual(values=crop_pal[names(crop_pal) %in% filter_crops]) +
      ggplot2::scale_linetype_manual(values=c("solid", "longdash"))+
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Global Monthly Water Withdrawal by Year, Selected Crops") +
      ggplot2::xlab("Month") +
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p5
    ggplot2::ggsave(filename = paste0(images, "figure5.png"),
                    plot = p5,
                    width = 13,
                    height = 10)
  }

  # Workflow illustrations
  if(grepl("workflow|all",temporal_scale,ignore.case=T)){
    # using ssp1_rcp26_gfdl, 2010 to show spatial downscaling
    library(rmap)

    ssp_rcp_gcm <- "ssp1_rcp26_gfdl"
    data_year <- 2020
    secNames <- c("Domestic", "Electricity", "Irrigation", "Livestock", "Manufacturing", "Mining")

    print("Starting spatial downscaling example map ...")

    basinRegion <- dplyr::distinct(rmap::mapIntersectGCAMBasin32Reg@data[c("subRegion_GCAMBasin", "subRegion_GCAMReg32")])

    # In order to plot with rmap facets, need to use the same map for all sectors,
    # and mapIntersectGCAMBasin32Reg is the "common denominator"
    # Irrigation data is actually region intersect basin scale, all others are region scale
    # For non-irrigation, each region_X_basin value is SET to the region's total value,
    # so the value displayed does not represent the value of the basin
    region_data <- readRDS("../data/region_data.rds")
    region_data <- dplyr::filter(region_data, scenario==ssp_rcp_gcm, year==data_year)

    region_data <- dplyr::full_join(tidyr::crossing(basinRegion, class=secNames), region_data)
    region_data <- dplyr::arrange(region_data, subRegion_GCAMReg32, class) # sort so that below works
    region_data <- tidyr::fill(region_data, scenario, year, value, .direction = "up") # sets all basin intersections to region's total withdrawal
    region_data <- tidyr::drop_na(region_data) # get rid of region only rows

    region_data <- dplyr::mutate(region_data, .keep="unused", subRegion=paste0(
      subRegion_GCAMBasin, "_X_", subRegion_GCAMReg32))


    # read grid data

    print("Loading grid data")
    files <- paste0(folder, "/", ssp_rcp_gcm, "/wd",
                    c("dom", "elec", "irr", "liv", "mfg", "min"), "_km3peryr.csv")
    grid_data <- data.table::rbindlist(lapply(files, function(x){ # stacks long
      return (data.table::fread(x, select=c(2, 3, 6+(data_year-2010)/5),
                                col.names=c("lon", "lat", "value"))) }))
    grid_data <- dplyr::bind_cols(class=rep(secNames, each=67420), grid_data)

    # make the figures
    print("Building spatial workflow figure")
    region_map <- rmap::map(data=region_data, title="Before Spatial Downscaling",
                            ncol=3, legendType = "continuous",
                            underLayer = rmap::mapIntersectGCAMBasin32Reg,
                            overLayer = rmap::mapIntersectGCAMBasin32Reg,
                            crop_to_underLayer = T,
                            background = T, save=F, show=T)
    region_map2 <- region_map[[1]] + ggplot2::scale_fill_gradientn(
      colors = rev(jgcricol()$pal_spectral),
      trans = scales::trans_new(name = '4th root',
                                transform = function(x){x^0.25},
                                inverse = function(x){x^4}),
      breaks = round(seq(0,max(region_data$value*0.99, na.rm = T)^0.25,length.out=5)^4, 2))
    
    grid_map <- rmap::map(data=grid_data, title="After Spatial Downscaling",
                          ncol=3, legendType = "continuous",
                          underLayer = rmap::mapIntersectGCAMBasin32Reg,
                          overLayer = rmap::mapIntersectGCAMBasin32Reg,
                          crop_to_underLayer = T,
                          background = T, save=F, show=F)
    grid_map2 <- grid_map[[1]] + ggplot2::scale_fill_gradientn(
      colors = rev(jgcricol()$pal_spectral),
      trans = scales::trans_new(name = '4th root',
                                transform = function(x){x^0.25},
                                inverse = function(x){x^4}),
      breaks = round(seq(0,max(grid_data$value*0.99, na.rm = T)^0.25,length.out=5)^4, 2))
    
    spatial_wf <- region_map2/ grid_map2 # library(patchwork)
    grDevices::png(paste0(images,"spatialworkflow.png"),width=13,height=10,units="in",res=300); print(spatial_wf); grDevices::dev.off()
    

    ggplot2::ggsave(filename = "spatialworkflow.png",
                    plot = spatial_wf,
                    width = 13,
                    height = 10) # save plot
    print("Spatial workflow figure complete")


    # Temporal Workflow

    # Annual and monthly maps, using ssp1_rcp26_gfdl, 2010, irrigation
    annual_grid_data <- data.table::fread(paste0(folder,"/ssp1_rcp26_gfdl/wdirr_km3peryr.csv"),
                                          select=c(2, 3, 6), col.names=c("lon", "lat", "value"))
    monthly_grid_data <- data.table::fread(paste0(folder,"/ssp1_rcp26_gfdl/twdirr_km3permonth.csv"),
                                          select=c(2, 3, 6:17), col.names=c("lon", "lat", month.abb))
    monthly_grid_data <- data.table::melt(monthly_grid_data, measure.vars=month.abb, variable.name="scenario")

    annual_map <- rmap::map(annual_grid_data, legendType = "continuous", overLayer=rmap::mapCountries, background=T, save=F, show=F)
    annual_map1 <- annual_map[[1]] + ggplot2::scale_fill_gradientn(
      colors = rev(jgcricol()$pal_spectral),
      trans = scales::trans_new(name = '4th root',
                                transform = function(x){x^0.25},
                                inverse = function(x){x^4}),
      breaks = round(seq(0,max(annual_grid_data$value*0.99, na.rm = T)^0.25,length.out=5)^4, 2))
    
    monthly_map <- rmap::map(monthly_grid_data, legendType = "continuous", overLayer=rmap::mapCountries, background=T, save=F, show=F)
    monthly_map1 <- monthly_map[[1]] + ggplot2::scale_fill_gradientn(
      colors = rev(jgcricol()$pal_spectral),
      trans = scales::trans_new(name = '4th root',
                                transform = function(x){x^0.25},
                                inverse = function(x){x^4}),
      breaks = round(seq(0,max(monthly_grid_data$value*0.99, na.rm = T)^0.25,length.out=5)^4, 2))
    
    temporal_wf1 <- annual_map1 / monthly_map1
    grDevices::png(paste0(images,"temporalworkflow.png"),width=8,height=10,units="in",res=300); print(temporal_wf1); grDevices::dev.off()
    
    ggplot2::ggsave(filename = "temporalworkflow.png",
                    plot = temporal_wf1,
                    width = 13,
                    height = 10) # save plot


    # annual graph, monthly graph
    annual_data <- readRDS("annual_data.rds")
    dataA <- dplyr::filter(annual_data, (SSP=="SSP 1") & (RCP=="RCP 2.6") &
                             (GCM=="gfdl") &
                             (Year %in% c(2010)) &
                           (names(Sector)!="total") &
                             (names(Sector)!="nonag"))
    dataA <- dplyr::mutate(dataA, Year=factor(Year))
    pA <- ggplot2::ggplot(data=dataA,
                          ggplot2::aes(x = Year, y = Value, fill = Sector))+
      ggplot2::geom_bar(position="dodge", stat="identity")+
      ggplot2::scale_fill_manual(values=water_pal)+
      ggplot2::ggtitle("Annual Water Withdrawal by Sector (2010)")+
      ggplot2::xlab("Year")+
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year)))+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5));pA

    monthly_data <- readRDS("monthly_data.rds")
    dataB <- dplyr::filter(monthly_data, (SSP=="SSP 1") & (RCP=="RCP 2.6") &
                             (GCM=="gfdl") &
                             (Year %in% c(2010)))
    dataB <- dplyr::mutate(dataB, Year=factor(Year), Month=factor(Month, labels=month.abb))
    pB <- ggplot2::ggplot(data=dataB,
                          ggplot2::aes(x = Month, y = Value, fill = Sector))+
      ggplot2::geom_bar(position="dodge", stat="identity")+
      ggplot2::scale_fill_manual(values=water_pal)+
      ggplot2::ggtitle("Monthly Water Withdrawal by Sector (2010)")+
      ggplot2::xlab("Month")+
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ month)))+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5));pB
    temporal_wf <- cowplot::plot_grid(pA, pB)
    ggplot2::ggsave(filename = "temporalworkflow.png",
                    plot = temporal_wf,
                    width = 13,
                    height = 10) # save plot
  }

  # validation plots
  if(grepl("validation|all",temporal_scale,ignore.case=T)){
    
    # Validation  1: Spatial Downscaling
    # 1 point for each ssp-rcp-gcm (75) x GCAM Year (19) x [Sector (6) or Crop (13)]
    # x-axis: total water withdrawal for that point (sum over GCAM regions/basins)
    # y-axis: total water withdrawal for that point (sum over 67420 Tethys grids)
    
    GCAM_data <- readRDS("../data/gcam_data.rds")
    Tethys_data <- readRDS("../data/region_sums_wd.rds")
    Tethys_data <- dplyr::select(Tethys_data,-month)
    Tethys_data <- dplyr::rename(Tethys_data, tethys_value = value)
    comparison <- dplyr::full_join(GCAM_data, Tethys_data)
    comparison <- dplyr::mutate(comparison, tethys_value=tidyr::replace_na(tethys_value, 0))
    #comparison <- dplyr::group_by(comparison, scenario, sector, year)
    #comparison <- dplyr::summarise(comparison, value = sum(value), tethys_value=sum(tethys_value))
    
    # 6 sectors
    comparison_sectors <- dplyr::filter(comparison, sector %in% c("dom","elec","irr","liv","mfg","min"))
    v1 <- ggplot2::ggplot(comparison_sectors,
                          ggplot2::aes(x=value, y=tethys_value, color=sector)) +
      ggplot2::geom_point(shape=3) +
      ggplot2::coord_fixed() +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(values=sec_pal) +
      ggplot2::ggtitle("GCAM vs Spatially Downscaled") +
      ggplot2::xlab(bquote("GCAM Water Withdrawal " ~ (km^3 ~ per ~ year))) +
      ggplot2::ylab(bquote("Spatially Downscaled, Reaggregated " ~ (km^3 ~ per ~ year)))
    ggplot2::ggsave(filename = paste0(images, "validation1.png"),
                    plot = v1,
                    width = 13,
                    height = 10)
    
    # 13 crops
    comparison_crops <- dplyr::filter(comparison, !sector %in% c("dom","elec","irr","liv","mfg","min"))
    v2 <- ggplot2::ggplot(comparison_crops,
                          ggplot2::aes(x=value, y=tethys_value, color=sector)) +
      ggplot2::geom_point(shape=3) +
      ggplot2::coord_fixed() +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(values=crop_pal) +
      ggplot2::ggtitle("GCAM vs Spatially Downscaled") +
      ggplot2::xlab(bquote("GCAM Water Withdrawal " ~ (km^3 ~ per ~ year))) +
      ggplot2::ylab(bquote("Spatially Downscaled, Reaggregated " ~ (km^3 ~ per ~ year)))
    ggplot2::ggsave(filename = paste0(images, "validation2.png"),
                    plot = v2,
                    width = 13,
                    height = 10)
    
    
    ## all months
#    files <- list.files(path = "../data", pattern = "^region_sums_twd", full.names = TRUE)
#    monthly_data <- purrr::map_dfr(files, function(x){
#      data <- readRDS(x)
#      data <- dplyr::filter(data, year %% 5 == 0)
#      data <- dplyr::group_by(data, scenario, region, basin, sector, year)
#      data <- dplyr::summarise(data, value = sum(value))
#      return (data)
#    })
#    saveRDS(monthly_data, "../data/month_sums.rds")
    
    Tethys_spatial <- readRDS("../data/region_sums_wd.rds")
    Tethys_spatial <- dplyr::select(Tethys_spatial,-month)
    Tethys_temporal <- readRDS("../data/month_sums.rds")
    Tethys_temporal <- dplyr::rename(Tethys_temporal, temporal_value=value)
    comparison <- dplyr::full_join(Tethys_spatial, Tethys_temporal)
    
    comparison_sectors <- dplyr::filter(comparison, sector %in% c("dom","elec","irr","liv","mfg","min"))
    v3 <- ggplot2::ggplot(comparison_sectors,
                          ggplot2::aes(x=value, y=temporal_value, color = sector)) +
      ggplot2::geom_point(shape=3) +
      ggplot2::coord_fixed() +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(values=sec_pal) +
      ggplot2::ggtitle("Tethys Annual vs Tethys Monthly") +
      ggplot2::xlab(bquote("Tethys Annual Withdrawal" ~ (km^3 ~ per ~ year))) +
      ggplot2::ylab(bquote("Tethys Monthly Withdrawal, Reaggregated " ~ (km^3 ~ per ~ year)))
    ggplot2::ggsave(filename = paste0(images, "validation3.png"),
                    plot = v3,
                    width = 13,
                    height = 10) # save plot
    
    comparison_crops <- dplyr::filter(comparison, !sector %in% c("dom","elec","irr","liv","mfg","min"))
    v4 <- ggplot2::ggplot(comparison_crops,
                          ggplot2::aes(x=value, y=temporal_value, color = sector)) +
      ggplot2::geom_point(shape=3) +
      ggplot2::coord_fixed() +
      ggplot2::theme_bw() +
      ggplot2::scale_color_manual(values=crop_pal) +
      ggplot2::ggtitle("Tethys Annual vs Tethys Monthly") +
      ggplot2::xlab(bquote("Tethys Annual Withdrawal" ~ (km^3 ~ per ~ year))) +
      ggplot2::ylab(bquote("Tethys Monthly Withdrawal, Reaggregated " ~ (km^3 ~ per ~ year)))
    ggplot2::ggsave(filename = paste0(images, "validation4.png"),
                    plot = v4,
                    width = 13,
                    height = 10) # save plot
    }

  if(grepl("animations|all",temporal_scale,ignore.case=T)) {
    gridLookup <- readRDS("rmap_tethys_grid_basin_region_country.rds")
    files <- paste0(folder, "/", "ssp5_rcp85_gfdl", "/crops_wdirr_",
                    names(crop_pal), "_km3peryr.csv")

    all_years_raw <- data.table::rbindlist(lapply(files, function(x){
      return (data.table::fread(x, select=c(1, 6:24))) }))
    all_years_raw <- dplyr::bind_cols(Crop=rep(names(crop_pal), each=67420), all_years_raw)


    all_years <- data.table::melt(all_years_raw, id.vars=c(1,2), variable.name="year")
    all_years <- tibble::as_tibble(all_years)
    all_years <- dplyr::filter(all_years, Crop != "biomass")
    all_years <- dplyr::group_by(all_years, Grid_ID, year)
    all_years <- dplyr::summarise(all_years, Crop = Crop[which.max(value)],
                                  max_value=max(value))
    all_years <- dplyr::ungroup(all_years)
    all_years <- dplyr::filter(all_years, max_value > 0)

    ay <- dplyr::transmute(all_years,
                           lon = gridLookup$lon[Grid_ID],
                           lat = gridLookup$lat[Grid_ID],
                           value = match(Crop, names(crop_pal)),
                           year = year,
                           param="SSP 5, RCP 8.5, gfdl,")

    numeric2Cat_list <-list(numeric2Cat_param = list(c("SSP 5, RCP 8.5, gfdl,")),
                            numeric2Cat_breaks = list(c(-Inf,1:12+0.5,Inf)),
                            numeric2Cat_labels = list(names(crop_pal)),
                            numeric2Cat_palette = list(crop_pal),
                            numeric2Cat_legendTextSize = list(c(0.7)))

    rm <- rmap::map(ay, background=T, save=T, show =F, animate=T,
                    numeric2Cat_list = numeric2Cat_list,
                    underLayer= rmap::mapCountries,
                    overLayer = rmap::mapCountries,
                    crop_to_underLayer = T)
  }

  if(grepl("regions|all",temporal_scale,ignore.case=T)){

    # overview of locations (map)
    data <- data.frame(subRegion = c("Indus", "Nile", "Upper_Colorado_River"),
                       value = c(0,0,0))
    overview_map <- rmap::map(data, background=T, crop = F, save=F, legendShow = F,
                              labels = T, labelSize = 5, labelColor = "black",
                              labelRepel = 1,
                              underLayer = rmap::mapCountries,
                              overLayer = rmap::mapCountries)
    ggplot2::ggsave(filename =  paste0(images, "overview_map.png"),
                    plot = overview_map$map_param_KMEANS,
                    width = 13,
                    height = 7)


    # Maps for Indus basin
    
    # Total withdrawals (annual), selected years
    
    my_sectors <- c("total", "dom", "elec", "mfg", "min", "liv", "irr")
    my_sectors <- c(my_sectors, names(crop_pal))
    my_sectors <- c("total", "FiberCrop")
    
    indus_data <- get_data(folder = folder, scenarios = "ssp1_rcp26_gfdl",
                           sectors = my_sectors,
                           basins = "Indus",
                           years=c(2025,2050,2075,2100), months=0)
    for (my_sector in my_sectors) {
      sector_data <- dplyr::filter(indus_data, sector == my_sector)
      # Having a column named "year" makes rmap make a bunch of other maps
      sector_data <- dplyr::select(sector_data, lon, lat, category=year, value) 
      sector_map <- rmap::map(sector_data, save=F, show=F,
                              background=T, zoom=-0.5, legendType="continuous",
                              title = paste0(my_sector, ": Indus basin"),
                              underLayer = rmap::mapGCAMReg32,
                              overLayer = rmap::mapGCAMReg32,
                              col="category", ncol=4)
      if (my_sector == my_sectors[1]){
        target_dim <- patchwork::get_dim(sector_map[[1]])
      } else {
        sector_map[[1]] <- patchwork::set_dim(sector_map[[1]], target_dim)
      }
      grDevices::png(paste0(images,"indus_", my_sector, ".png"),width=13,height=4,units="in",res=300); print(sector_map[[1]]); grDevices::dev.off()
    }
    
    ### heatmap by sector
    my_sectors <- c("dom", "elec", "irr")
    my_sectors <- c("dom", "elec", "mfg", "min", "liv", "irr", names(crop_pal))

    indus_data <- get_data(folder = folder, scenarios = "ssp1_rcp26_gfdl",
                           sectors = my_sectors,
                           basins = "Indus")
    for (my_sector in my_sectors) {
      sector_data <- dplyr::filter(indus_data, sector == my_sector)
      sector_data <- dplyr::group_by(sector_data, sector, year, month)
      sector_data <- dplyr::summarise(sector_data, value=sum(value))
      sector_data$month <- factor(sector_data$month, labels = month.abb)
      
      heatmap <- ggplot2::ggplot(sector_data, ggplot2::aes(x=year,
                                                           y=month,
                                                           fill= value)) +
        ggplot2::geom_tile() +
        ggplot2::scale_x_continuous(expand=c(0,0)) +
        ggplot2::scale_y_discrete(limits=rev(levels(sector_data$month)), expand=c(0,0)) +
        ggplot2::scale_fill_gradientn(colors=jgcricol()$pal_hot) +
        ggplot2::theme(panel.background = ggplot2::element_blank(),
                       plot.background = ggplot2::element_blank())
      if (my_sector == my_sectors[1]){
        target_dim <- patchwork::get_dim(heatmap)
      } else {
        heatmap <- patchwork::set_dim(heatmap, target_dim)
      }
      ggplot2::ggsave(filename = paste0(images, "heatmap_indus_", my_sector, ".png"),
                      plot = heatmap,
                      width = 13,
                      height = 3)
      }
    

    
    indus_data2 <- get_data(folder = folder, scenarios = "ssp1_rcp26_gfdl",
                            sectors = c("irr", "biomass"),
                            basins = "Indus",
                            years=c(2020,2040,2060,2080,2100), months=0)
    indus_data2 <- dplyr::mutate(indus_data2, value=dplyr::case_when(
      sector=="biomass"~ -value,
      T~ value))
    indus_data2 <- dplyr::group_by(indus_data2, lon, lat, year)
    indus_data2 <- dplyr::summarise(indus_data2, value=sum(value))

    indus_data2 <- dplyr::select(indus_data2, lon, lat, Year=year, Month=month, value)
    indus_data2 <- dplyr::mutate(indus_data2, Month=factor(month.abb[Month], levels=month.abb))
    indus_data2 <- dplyr::mutate(indus_data2, value = (value/max(indus_data2$value))^0.2)

    rm <- rmap::map(indus_data2, save=T, show=F, background=T, zoom=-0.5, legendType="continuous",
                    underLayer = rmap::mapGCAMReg32,
                    overLayer = rmap::mapGCAMReg32,
                    col="Month", row="Year")

    # crop types
    indus_data_raw <- get_data(folder = folder, scenarios = "ssp1_rcp26_gfdl",
                               sectors = head(names(crop_pal), -1),
                               basins = "Indus",
                               years=c(2020,2040,2060,2080,2100))
    indus_data <- dplyr::group_by(indus_data_raw, Grid_ID, lon, lat, year, month)
    indus_data <- dplyr::summarise(indus_data, sector = sector[which.max(value)],
                                  max_value=max(value))
    indus_data <- dplyr::ungroup(indus_data)
    indus_data <- dplyr::filter(indus_data, max_value > 0)

    indus_data <- dplyr::mutate(indus_data, value=match(sector, names(crop_pal)),
                                month=factor(month.abb[month], levels=month.abb),
                                param="param")
    indus_data <- dplyr::select(indus_data, lon, lat, Year=year, Month=month, value, param)

    numeric2Cat_list <-list(numeric2Cat_param = list(c("param")),
                            numeric2Cat_breaks = list(c(-Inf,1:12+0.5,Inf)),
                            numeric2Cat_labels = list(names(crop_pal)),
                            numeric2Cat_palette = list(crop_pal),
                            numeric2Cat_legendTextSize = list(c(0.7)))

    rm <- rmap::map(indus_data, background=T, save=T, show =F,
                    numeric2Cat_list = numeric2Cat_list,
                    underLayer= rmap::mapGCAMReg32,
                    overLayer = rmap::mapGCAMReg32,
                    row="Month", col="Year", zoom = -0.5)


    # heatmap (year-month)
    indus_data <- get_data(folder = folder, scenarios = "ssp1_rcp26_gfdl",
                           sectors = names(crop_pal), #c("dom", "elec", "mfg", "min", "irr", "liv"),
                           basins = "Indus")
    indus_data <- dplyr::group_by(indus_data, sector, year, month)
    indus_summary <- dplyr::summarise(indus_data, value=sum(value))
    indus_summary$month <- factor(indus_summary$month, labels = month.abb)

    hm <- ggplot2::ggplot(indus_summary, ggplot2::aes(x=year,
                                                      y=month,
                                                      fill= value)) +
      ggplot2::geom_tile() +
      ggplot2::facet_wrap(ggplot2::vars(sector)) +
      ggplot2::scale_x_continuous(expand=c(0,0)) +
      ggplot2::scale_y_discrete(limits=rev(levels(indus_summary$month)), expand=c(0,0)) +
      ggplot2::scale_fill_gradientn(colors=jgcricol()$pal_hot) +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     plot.background = ggplot2::element_blank()); hm

    
    ggplot2::ggsave(filename = paste0(images, "heatmap.png"),
                    plot = hm,
                    width = 13,
                    height = 7)

  }

  # Initialize
  print("generate_figures completed succesfuly!")
} # Close generate_figures function


## helpers and other functions below

prepare_annual <- function(folder=NULL, outfile="annual_data.rds") {
  annual_data <- all_combos()
  files <- dplyr::transmute(annual_data, paste0(
    folder, "/", names(SSP), "_",names(RCP), "_", GCM,
    "/wd", names(Sector), "_km3peryr.csv"))[[1]]

  print(paste0("Preparing data from ", length(files), " files ..."))
  time_start <- Sys.time()
  values <- data.table::rbindlist(lapply(1:length(files), function(i){ # stacks long
    if (i %% 50 == 0) { # progress update at arbitrary interval
      time_remaining <- (length(files)/i-1)*(Sys.time()-time_start)
      print(paste0("Progress: ", round(100*i/length(files), digits=2),
                  "%, Estimated time remaining: ", format(time_remaining)))
    }
    return(data.frame(Value=colSums(data.table::fread(files[i], drop=1:5)))) # sums of year columns
    }))
  print("Data loaded")

  annual_data <- tidyr::crossing(annual_data, Year=seq(2010,2100,by=5)) # each year for each combo
  annual_data <- dplyr::bind_cols(annual_data, values) # SSP, RCP, GCM, Sector, Year; Value
  saveRDS(annual_data, file = outfile)
  print(paste("Annual totals saved to", outfile))
  return (annual_data)
}

prepare_monthly <- function(folder=NULL, outfile="monthly_data.rds") {
  monthly_data <- all_combos()
  monthly_data <- dplyr::filter(monthly_data, (names(Sector) != "total") & (names(Sector) != "nonag"))
  files <- dplyr::transmute(monthly_data, paste0(
    folder, "/", names(SSP), "_",names(RCP), "_", GCM,
    "/twd", names(Sector), "_km3permonth.csv"))[[1]]

  print(paste0("Preparing data from ", length(files), " files ..."))
  time_start <- Sys.time()
  values <- data.table::rbindlist(lapply(1:length(files), function(i){ # stacks long
    if (i %% 50 == 0) { # progress update at arbitrary interval
      time_remaining <- (length(files)/i-1)*(Sys.time()-time_start)
      print(paste0("Progress: ", round(100*i/length(files), digits=2),
                   "%, Estimated time remaining: ", format(time_remaining)))
    }
    return(data.frame(Value=colSums(data.table::fread(files[i], drop=1:5)))) # sums of month columns
  }))
  print("Data loaded")

  monthly_data <- tidyr::crossing(monthly_data, Year=2010:2100, Month=1:12) # each year and month for each combo
  monthly_data <- dplyr::bind_cols(monthly_data, values) # SSP, RCP, GCM, Sector, Year; Value
  saveRDS(monthly_data, file = outfile)
  print(paste("Monthly totals saved to", outfile))
  return (monthly_data)
}

prepare_annual_crops <- function(folder=NULL, outfile="annual_crops.rds") {
  crop_names = c("biomass",
                 "Corn",
                 "FiberCrop",
                 "FodderGrass",
                 "FodderHerb",
                 "MiscCrop",
                 "OilCrop",
                 "OtherGrain",
                 "PalmFruit",
                 "Rice",
                 "Root_Tuber",
                 "SugarCrop",
                 "Wheat")
  annual_crops <- dplyr::distinct(dplyr::select(all_combos(), -Sector)) # rewrite all_combos to handle this
  annual_crops <- tidyr::crossing(annual_crops, Crop = crop_names)

  # uncomment for local testing
  annual_crops <- dplyr::filter(annual_crops, SSP == "SSP 1" & RCP == "RCP 2.6" & GCM == "gfdl")

  files <- dplyr::transmute(annual_crops, paste0(
    folder, "/", names(SSP), "_",names(RCP), "_", GCM,
    "/crops_wdirr_", Crop, "_km3peryr.csv"))[[1]]

  print(paste0("Preparing data from ", length(files), " files ..."))
  time_start <- Sys.time()
  values <- data.table::rbindlist(lapply(1:length(files), function(i){ # stacks long
    if (i %% 13 == 0) { # progress update at arbitrary interval
      time_remaining <- (length(files)/i-1)*(Sys.time()-time_start)
      print(paste0("Progress: ", round(100*i/length(files), digits=2),
                   "%, Estimated time remaining: ", format(time_remaining)))
    }
    return(data.frame(Value=colSums(data.table::fread(files[i], drop=1:5)))) # sums of year columns
  }))
  print("Data loaded")

  annual_crops <- tidyr::crossing(annual_crops, Year=seq(2010,2100,by=5)) # each year for each combo
  annual_crops <- dplyr::bind_cols(annual_crops, values) # SSP, RCP, GCM, Crop, Year; Value
  saveRDS(annual_crops, file = outfile)
  print(paste("Annual crop totals saved to", outfile))
  return (annual_crops)
}

prepare_monthly_crops <- function(folder=NULL, outfile="monthly_crops.rds") {
  crop_names = c("biomass",
                 "Corn",
                 "FiberCrop",
                 "FodderGrass",
                 "FodderHerb",
                 "MiscCrop",
                 "OilCrop",
                 "OtherGrain",
                 "PalmFruit",
                 "Rice",
                 "Root_Tuber",
                 "SugarCrop",
                 "Wheat")
  monthly_crops <- dplyr::distinct(dplyr::select(all_combos(), -Sector)) # rewrite all_combos to handle this
  monthly_crops <- tidyr::crossing(monthly_crops, Crop = crop_names)

  # uncomment for local testing
  monthly_crops <- dplyr::filter(monthly_crops, SSP == "SSP 1" & RCP == "RCP 2.6" & GCM == "gfdl")

  files <- dplyr::transmute(monthly_crops, paste0(
    folder, "/", names(SSP), "_",names(RCP), "_", GCM,
    "/crops_twdirr_", Crop, "_km3permonth.csv"))[[1]]

  print(paste0("Preparing data from ", length(files), " files ..."))
  time_start <- Sys.time()
  values <- data.table::rbindlist(lapply(1:length(files), function(i){ # stacks long
    if (i %% 13 == 0) { # progress update at arbitrary interval
      time_remaining <- (length(files)/i-1)*(Sys.time()-time_start)
      print(paste0("Progress: ", round(100*i/length(files), digits=2),
                   "%, Estimated time remaining: ", format(time_remaining)))
    }
    return(data.frame(Value=colSums(data.table::fread(files[i], drop=1:5)))) # sums of year columns
  }))
  print("Data loaded")

  monthly_crops <- tidyr::crossing(monthly_crops, Year=2010:2100, Month=1:12) # each year and month for each combo
  monthly_crops <- dplyr::bind_cols(monthly_crops, values) # SSP, RCP, GCM, Crop, Year: Value
  saveRDS(monthly_crops, file = outfile)
  print(paste("Monthly crop totals saved to", outfile))
  return (monthly_crops)
}

prepare_GCAM <- function(GCAM_withdrawals_csv=NULL, outfile="region_data.rds") {
  subReg_to_map <- tibble::deframe(rmap::mappings("mappingGCAMBasins"))
  subReg_to_map <- setNames(names(subReg_to_map), subReg_to_map)

  scenarios <- dplyr::transmute(all_combos(), paste0(
    names(SSP), "_",names(RCP), "_", GCM))
  scenarios <- dplyr:::distinct(scenarios)[[1]]

  print("Loading GCAM data")
  region_data <- tibble::as_tibble(data.table::fread(GCAM_withdrawals_csv, select=c(2,3,5,6,7)))

  print("Filtering to dataset scenarios and years")
  region_data <- dplyr::filter(region_data, 2010<=year & year<=2100 & is.element(scenario, scenarios))

  # further filter (only need this scenario-year right now)
  #region_data <- dplyr::filter(region_data, scenario=="ssp1_rcp26_gfdl" & year == 2010)

  region_data <- dplyr::mutate(region_data, .keep="unused", .after="scenario",
                               subRegion_GCAMBasin = dplyr::case_when(
                                 grepl("water_td_irr_", input) ~ subReg_to_map[gsub("water_td_irr_|_W", "", input)],
                                 TRUE ~ NA_character_),
                               subRegion_GCAMReg32 = sub("-", "_", region), # need "EU_12", not "EU-12"
                               class = dplyr::case_when(
                                 grepl("water_td_dom_", input) ~ "Domestic",
                                 grepl("water_td_elec_", input) ~ "Electricity",
                                 grepl("water_td_irr_", input) ~ "Irrigation",
                                 grepl("water_td_an_", input) ~ "Livestock",
                                 grepl("water_td_ind_", input) ~ "Manufacturing",
                                 grepl("water_td_pri_", input) ~ "Mining"))
  region_data <- dplyr::group_by(region_data, scenario, year, subRegion_GCAMBasin, subRegion_GCAMReg32, class)
  region_data <- dplyr::summarise(region_data, value=sum(value))

  saveRDS(region_data, file = outfile)
  print(paste("GCAM data saved to", outfile))
  return (region_data)
}

prepare_GCAM_crops <- function(GCAM_withdrawals_csv=NULL, outfile="region_crops.rds") {
  subReg_to_map <- tibble::deframe(rmap::mappings("mappingGCAMBasins"))
  subReg_to_map <- setNames(names(subReg_to_map), subReg_to_map)

  scenarios <- dplyr::transmute(all_combos(), paste0(
    names(SSP), "_",names(RCP), "_", GCM))
  scenarios <- dplyr:::distinct(scenarios)[[1]]

  print("Loading GCAM data")
  region_crops <- tibble::as_tibble(data.table::fread(GCAM_withdrawals_csv, select=2:7))

  print("Filtering to dataset scenarios and years")
  region_crops <- dplyr::filter(region_crops, 2010<=year & year<=2100 & is.element(scenario, scenarios))

  # filter to irrigation only
  region_crops <- dplyr::filter(region_crops, grepl("water_td_irr_", input))

  region_crops <- dplyr::mutate(region_crops, .keep="unused", .after="scenario",
                               subRegion_GCAMBasin = subReg_to_map[gsub("water_td_irr_|_W", "", input)],
                               subRegion_GCAMReg32 = sub("-", "_", region), # need "EU_12", not "EU-12"
                               Crop = dplyr::case_when(
                                 grepl("biomass", subsector) ~ "biomass",
                                 TRUE ~ sub("_[^_]+$", "", subsector)) # only part before the final underscore
  )

  saveRDS(region_crops, file = outfile)
  print(paste("GCAM data saved to", outfile))
  return (region_crops)
}

build_gridLookup <- function() {
  basinIDs <- data.table::fread("data/basin.csv", col.names="basinID")
  regionIDs <- data.table::fread("data/region32_grids.csv", col.names="regionID")

  # some kind of off by 1 error?
  countryIDs <- data.table::fread("data/country.csv", col.names="countryID") -1

  coordinates <- data.table::fread("data/coordinates.csv",
                                   col.names=c("gridID",
                                               "lon",
                                               "lat",
                                               "ilon",
                                               "ilat"))


  # correct Arkansas_White_Red_Basin to Arkansas_White_Red, etc,
  basinID_to_basinName <- gsub("_Basin$", "", tibble::deframe(data.table::fread(
    "data/gcam_basin_lookup.csv", select=c(1,2))[order(basin_id)]))
  basinID_to_basinName["102"] <- "Hamun_i_Mashkel" # from "HamuniMashkel"
  basinID_to_basinName["109"] <- "Hong_Red_River" # from "Hong_(Red_River)"

  # correct EU-12 to EU_12,
  regionID_to_regionName <- gsub("-", "_", tibble::deframe(data.table::fread(
    "data/RgnNames.csv", select=c(2,1))[order(region_id)]))

  countryID_to_countryName <- tibble::deframe(data.table::fread(
    "data/country-names.csv"))


  gridLookup <- dplyr::bind_cols(coordinates, basinIDs, regionIDs, countryIDs)
  gridLookup <- dplyr::mutate(gridLookup,
                              basinName = basinID_to_basinName[as.character(basinID)],
                              regionName = regionID_to_regionName[as.character(regionID)],
                              countryName = countryID_to_countryName[as.character(countryID)])
  gridLookup <- dplyr::relocate(gridLookup,  basinName, .after=basinID)
  gridLookup <- dplyr::relocate(gridLookup,  regionName, .after=regionID)
  gridLookup <- dplyr::relocate(gridLookup,  countryName, .after=countryID)

  saveRDS(gridLookup, file = "rmap_tethys_grid_basin_region_country.rds")
  return(gridLookup)
}

get_data <- function(folder=NULL, scenarios=NULL, sectors=NULL, years=NULL, months=NULL, regions=NULL, basins=NULL, countries=NULL, aggregate=F) {
  lookup <- rmap::mapping_tethys_grid_basin_region_country
  crop_names = c("biomass", "Corn", "FiberCrop", "FodderGrass", "FodderHerb",
                 "MiscCrop", "OilCrop", "OtherGrain", "PalmFruit", "Rice",
                 "Root_Tuber", "SugarCrop", "Wheat")
  scenario_list <- rep(scenarios, each=length(sectors))
  sector_list <- rep(sectors, length(scenarios))
  if (is.null(years)) {years = 2010:2100}
  if (is.null(months)) {months = 1:12}
  if (months[1]==0) {
    years <- years[years %% 5 == 0]
    cols <- 6 + (years - 2010)/5
  } else {
    cols <- 5+(rep(years, each=length(months)) - 2010)*12 + rep(months, length(years))
  }
  filename_after = c("_km3permonth.csv", "_km3peryr.csv")[1 + (months[1] == 0)]

  all_data <- data.table::rbindlist(lapply(1:length(scenario_list), function(i){
    case <- 1 + (months[1] == 0)*2 + sector_list[i] %in% crop_names # abusing T=1, F=0
    filename_before = c("/twd", "/crops_twdirr_", "/wd", "/crops_wdirr_")[case]
    filename <- paste0(folder, "/", scenario_list[i], filename_before, sector_list[i], filename_after)
    print(filename)
    file_data <- data.table::fread(filename, select=c(1, cols))

    if (!is.null(regions)) {
      file_data <- dplyr::filter(file_data, lookup$regionName[Grid_ID] %in% regions)
    }
    if (!is.null(basins)) {
      file_data <- dplyr::filter(file_data, lookup$basinName[Grid_ID] %in% basins)
    }
    if (!is.null(countries)) {
      file_data <- dplyr::filter(file_data, lookup$countryName[Grid_ID] %in% countries)
    }

    file_data <- dplyr::mutate(file_data, .after="Grid_ID",
                               region = lookup$regionName[Grid_ID],
                               basin = lookup$basinName[Grid_ID])
    if (aggregate) {
      file_data[,Grid_ID:=NULL]
      if (sector_list[i] %in% c("dom", "elec", "mfg", "min", "liv")) {
        file_data$basin = NA_character_
      }
      file_data <- file_data[, lapply(.SD, sum), by=list(region, basin)]
      file_data <- data.table::melt(file_data, id.vars=1:2)

      file_data <- dplyr::filter(file_data, value != 0)
      file_data <- dplyr::mutate(file_data, .before="value",
                                 scenario = scenario_list[i],
                                 sector = sector_list[i],
                                 year  = as.integer(substr(variable, 1, 4)),
                                 month = as.integer(substr(variable, 5, 6)))
    } else {
      file_data <- data.table::melt(file_data, id.vars=1:3)
      file_data <- dplyr::mutate(file_data, .before="value",
                                 lon = lookup$lon[Grid_ID],
                                 lat = lookup$lat[Grid_ID],
                                 scenario = scenario_list[i],
                                 sector = sector_list[i],
                                 year  = as.integer(substr(variable, 1, 4)),
                                 month = as.integer(substr(variable, 5, 6)))
    }
    file_data <- dplyr::select(file_data, -variable)
    return (file_data)}))
  return (all_data)
}


# all valid ssp-rcp-gcm-sector (including total, nonag sectors)
all_combos <- function() {
  NULL -> SSP -> RCP -> GCM -> Sector
  combos <- tidyr::crossing(SSP = c(ssp1 = "SSP 1",
                                    ssp2 = "SSP 2",
                                    ssp3 = "SSP 3",
                                    ssp4 = "SSP 4",
                                    ssp5 = "SSP 5"),
                            RCP = c(rcp26 = "RCP 2.6",
                                    rcp45 = "RCP 4.5",
                                    rcp60 = "RCP 6.0",
                                    rcp85 = "RCP 8.5"),
                            GCM = c("gfdl",
                                    "hadgem",
                                    "ipsl",
                                    "miroc",
                                    "noresm"),
                            Sector = c(dom = "Domestic",
                                       elec= "Electricity",
                                       irr = "Irrigation",
                                       liv = "Livestock",
                                       mfg = "Manufacturing",
                                       min = "Mining",
                                       nonag = "Non-Agriculture",
                                       total = "Total")
  )
  combos <- dplyr::filter(combos, !(SSP=="SSP 3" & RCP=="RCP 2.6") & # if rcp26, exclude ssp3
                            !(SSP!="SSP 5" & RCP=="RCP 8.5") # if rcp85, must use ssp5
  )
  return(combos)
}
