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

  gcm_pal = c("gfdl" = "gold",
              "hadgem" = "darkorange",
              "ipsl" = "red3",
              "miroc" = "deeppink",
              "noresm" = "darkviolet")

  sector_pal = c("Domestic"="royalblue4",
                 "Electricity"="yellow3",
                 "Manufacturing"="steelblue",
                 "Mining"="cadetblue3",
                 "Irrigation"="palegreen4",
                 "Livestock"="yellowgreen")

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
      ggplot2::scale_color_manual(values=sector_pal) +
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
      ggplot2::scale_fill_manual(values=sector_pal),
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

    print("Building Figure 4")
    data4 <- dplyr::filter(monthly_data, Sector!="Irrigation"&(GCM=="gfdl")&(Year %in% c(2015, 2100)))
    data4 <- dplyr::mutate(data4, Year=factor(Year), Month=factor(Month, labels=month.abb))
    p4 <- ggplot2::ggplot(data = data4,
                          ggplot2::aes(x = Month,
                                       y = Value,
                                       group = interaction(GCM, Sector, Year))) +
      ggplot2::geom_line(ggplot2::aes(linetype=Year, color=Sector)) +
      ggplot2::scale_color_manual(values=sector_pal) +
      ggplot2::scale_linetype_manual(values=c("solid", "longdash"))+
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Global Monthly Water Withdrawal by SSP-RCP-GCM and Sector") +
      ggplot2::xlab("Month") +
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p4
    ggplot2::ggsave(filename =  "figure4.png",
                    plot = p4,
                    width = 13,
                    height = 10) # save plot

    # months x axis,
  }
  # Maps
  if(grepl("maps|all",temporal_scale,ignore.case=T)){
    # using ssp1_rcp26_gfdl, 2010 to show spatial downscaling

    ssp_rcp_gcm <- "ssp1_rcp26_gfdl"
    data_year <- 2010

    secAbbs <- c("dom", "elec", "irr", "liv", "mfg", "min")
    secNames <- c("Domestic", "Electricity", "Irrigation", "Livestock", "Manufacturing", "Mining")

    library(rmap)
    lookup <- tibble::deframe(rmap::mappings("mappingGCAMBasins"))
    lookup <- setNames(names(lookup), lookup)
    basinRegion <- dplyr::distinct(rmap::mapIntersectGCAMBasin32Reg@data[c("subRegion_GCAMBasin", "subRegion_GCAMReg32")])

    region_data <- tibble::as_tibble(data.table::fread(
      "C:/Users/thom927/Documents/Data/GrahamGCAM/water_withdrawals_by_mapping_source.csv",
      select=c(2,3,5,6,7)))
    region_data <- dplyr::filter(region_data, year==data_year & scenario==ssp_rcp_gcm)
    region_data <- dplyr::mutate(region_data, .keep="unused", .after="scenario",
                               subRegion_GCAMBasin = dplyr::case_when(
                                 grepl("water_td_irr_", input) ~ lookup[gsub("water_td_irr_|_W", "", input)],
                                 TRUE ~ NA_character_),
                               subRegion_GCAMReg32 = sub("-", "_", region), # need "EU_12", not "EU-12"
                               class = dplyr::case_when(
                                 grepl("water_td_dom_", input) ~ "Domestic",
                                 grepl("water_td_elec_", input) ~ "Electricity",
                                 grepl("water_td_irr_", input) ~ "Irrigation",
                                 grepl("water_td_an_", input) ~ "Livestock",
                                 grepl("water_td_ind_", input) ~ "Manufacturing",
                                 grepl("water_td_pri_", input) ~ "Mining"))
    region_data <- dplyr::group_by(region_data, subRegion_GCAMBasin, subRegion_GCAMReg32, class)
    region_data <- dplyr::summarise(region_data, value=sum(value))

    region_data <- dplyr::full_join(tidyr::crossing(basinRegion, class=secNames), region_data)
    region_data <- dplyr::arrange(region_data, subRegion_GCAMReg32, class)
    region_data <- tidyr::fill(region_data, value, .direction = "up")
    region_data <- tidyr::drop_na(region_data)
    region_data <- dplyr::mutate(region_data, subRegion=paste0(subRegion_GCAMBasin, "_X_", subRegion_GCAMReg32))



    files <- paste0(folder, "/", ssp_rcp_gcm, "/wd",
                    c("dom", "elec", "irr", "liv", "mfg", "min"), "_km3peryr.csv")
    grid_data <- data.table::rbindlist(lapply(files, function(x){ # stacks long
      return (data.table::fread(x, select=c(2, 3, 6+(data_year-2010)/5),
                                col.names=c("lon", "lat", "value"))) }))
    grid_data <- dplyr::bind_cols(class=rep(secNames, each=67420), grid_data)


    region_map <- rmap::map(data=region_data, ncol=3, shape=rmap::mapIntersectGCAMBasin32Reg, background = T, save=F, show=F)
    grid_map <- rmap::map(data=grid_data, ncol=3, overLayer = rmap::mapIntersectGCAMBasin32Reg, background = T, save=F, show=F)
    spatial_wf <- cowplot::plot_grid(region_map[[1]], grid_map[[1]], ncol=1)
    ggplot2::ggsave(filename =  "spatialworkflow.png",
                    plot = spatial_wf,
                    width = 13,
                    height = 10) # save plot

  }
  # Workflow illustrations
  if(grepl("workflow|all",temporal_scale,ignore.case=T)){
    # Temporal Workflow
    annual_data <- readRDS("annual_data.rds")
    dataA <- dplyr::filter(annual_data, (SSP=="SSP 2") & (RCP=="RCP 4.5") &
                             (GCM=="gfdl") &
                             (Year %in% c(2015,2100)) &
                           (names(Sector)!="total") &
                             (names(Sector)!="nonag"))
    dataA <- dplyr::mutate(dataA, Year=factor(Year))
    pA <- ggplot2::ggplot(data=dataA,
                          ggplot2::aes(x = Year, y = Value, fill = Sector))+
      ggplot2::geom_bar(position="dodge", stat="identity")+
      ggplot2::scale_fill_manual(values=sector_pal)+
      ggplot2::ggtitle("Global Annual Water Withdrawal by Sector, SSP 2, RCP 4.5, gfdl")+
      ggplot2::xlab("Year")+
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year)))+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5));pA

    monthly_data <- readRDS("monthly_data.rds")
    dataB <- dplyr::filter(monthly_data, (SSP=="SSP 2") & (RCP=="RCP 4.5") &
                             (GCM=="gfdl") &
                             (Year %in% c(2015)))
    dataB <- dplyr::mutate(dataB, Year=factor(Year), Month=factor(Month, labels=month.abb))
    pB <- ggplot2::ggplot(data=dataB,
                          ggplot2::aes(x = Month, y = Value, fill = Sector))+
      ggplot2::geom_bar(position="dodge", stat="identity")+
      ggplot2::scale_fill_manual(values=sector_pal)+
      ggplot2::ggtitle("Global Annual Water Withdrawal by Sector, SSP 2, RCP 4.5, gfdl")+
      ggplot2::xlab("Year")+
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year)))+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5));pB
  }

  # Initialize
  print("generate_figures completed succesfuly!")
} # Close generate_figures function


## helpers and other functions below

prepare_annual <- function(folder=NULL, outfile="annual_data.rds") {
  annual_data <- all_combos()
  #annual_data <- dplyr::filter(annual_data, (names(Sector) == "total")|(names(Sector) == "irr")) # uncomment for testing on my local files
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
    return(data.frame(Value=colSums(data.table::fread(files[i], drop=1:5)))) # sums of year columns
  }))
  print("Data loaded")

  monthly_data <- tidyr::crossing(monthly_data, Year=2010:2100, Month=1:12) # each year and month for each combo
  monthly_data <- dplyr::bind_cols(monthly_data, values) # SSP, RCP, GCM, Sector, Year; Value
  saveRDS(monthly_data, file = outfile)
  print(paste("Monthly totals saved to", outfile))
  return (monthly_data)
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
                            !(SSP!="SSP 5" & RCP=="RCP 8.5") & # if rcp85, must use ssp5
                            !(RCP=="RCP 8.5" & GCM!="gfdl") # if rcp85, must use gfdl
  )
  return(combos)
}
