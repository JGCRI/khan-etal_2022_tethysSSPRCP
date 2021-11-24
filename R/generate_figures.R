#' generate_figures
#'
#' Generate package figures
#'
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
                             temporal_scale="annual") {
  NULL -> Year -> Value -> GCM -> Sector -> SSP -> RCP

  # Initialize
  print("Starting generate_figures ...")


  # Check if folder exists
  # Check if required files exist (sectors total, irr etc.)



  if(grepl("annual|all",temporal_scale,ignore.case=T)){

    # get all annual data
    annual_data <- all_combos()
    #annual_data <- dplyr::filter(annual_data, (names(Sector) == "total")|(names(Sector) == "irr")) # uncomment for testing on my local files
    files <- dplyr::transmute(annual_data, paste0(
      folder, "/", names(SSP), "_",names(RCP), "_", GCM,
      "/wd", names(Sector), "_km3peryr.csv"))[[1]]

    print(paste0("Loading data from ", length(files), " files ..."))
    pb = utils::txtProgressBar(min=0, max=length(files), style=3)
    values <- data.table::rbindlist(lapply(1:length(files), function(i){ # stacks long
      utils::setTxtProgressBar(pb, i); return(data.frame(
        Value=colSums(data.table::fread(files[i], drop=1:5))) # sums of year columns
      )}))
    close(pb)
    print("Data loaded")

    annual_data <- tidyr::crossing(annual_data, Year=seq(2010,2100,by=5)) # each year for each combo
    annual_data <- dplyr::bind_cols(annual_data, values) # SSP, RCP, GCM, Sector, Year; Value

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

    print("Building Figure 3a")
    data3a <-  dplyr::filter(annual_data, (GCM=="gfdl") &
                               (names(Sector)!="total") &
                               (names(Sector)!="nonag"))
    p3a <- ggplot2::ggplot(data = data3a,
                          ggplot2::aes(x = Year,
                                       y = Value,
                                       fill = Sector)) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Global Annual Water Withdrawal by SSP-RCP-Sector, GCM: gfdl") +
      ggplot2::xlab("Year") +
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p3a
    ggplot2::ggsave(filename = "figure3a.png",
                    plot = p3a,
                    width = 13,
                    height = 10) # save plot

    print("Building Figure 3b")
    data3b <-  dplyr::filter(annual_data, (GCM=="hadgem") &
                               (names(Sector)!="total") &
                               (names(Sector)!="nonag"))
    p3b <- ggplot2::ggplot(data = data3b,
                           ggplot2::aes(x = Year,
                                        y = Value,
                                        fill = Sector)) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Global Annual Water Withdrawal by SSP-RCP-Sector, GCM: hadgem") +
      ggplot2::xlab("Year") +
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p3b
    ggplot2::ggsave(filename = "figure3b.png",
                    plot = p3b,
                    width = 13,
                    height = 10) # save plot

    print("Building Figure 3c")
    data3c <-  dplyr::filter(annual_data, (GCM=="ipsl") &
                               (names(Sector)!="total") &
                               (names(Sector)!="nonag"))
    p3c <- ggplot2::ggplot(data = data3c,
                           ggplot2::aes(x = Year,
                                        y = Value,
                                        fill = Sector)) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Global Annual Water Withdrawal by SSP-RCP-Sector, GCM: ipsl") +
      ggplot2::xlab("Year") +
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p3c
    ggplot2::ggsave(filename = "figure3c.png",
                    plot = p3c,
                    width = 13,
                    height = 10) # save plot

    print("Building Figure 3d")
    data3d <-  dplyr::filter(annual_data, (GCM=="miroc") &
                               (names(Sector)!="total") &
                               (names(Sector)!="nonag"))
    p3d <- ggplot2::ggplot(data = data3d,
                           ggplot2::aes(x = Year,
                                        y = Value,
                                        fill = Sector)) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Global Annual Water Withdrawal by SSP-RCP-Sector, GCM: miroc") +
      ggplot2::xlab("Year") +
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p3d
    ggplot2::ggsave(filename = "figure3d.png",
                    plot = p3d,
                    width = 13,
                    height = 10) # save plot

    print("Building Figure 3e")
    data3e <-  dplyr::filter(annual_data, (GCM=="noresm") &
                               (names(Sector)!="total") &
                               (names(Sector)!="nonag"))
    p3e <- ggplot2::ggplot(data = data3e,
                           ggplot2::aes(x = Year,
                                        y = Value,
                                        fill = Sector)) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Global Annual Water Withdrawal by SSP-RCP-Sector, GCM: noresm") +
      ggplot2::xlab("Year") +
      ggplot2::ylab(bquote(Water ~ Withdrawal ~ (km^3 ~ per ~ year))) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p3e
    ggplot2::ggsave(filename = "figure3e.png",
                    plot = p3e,
                    width = 13,
                    height = 10) # save plot
  }

  # Monthly plots
  if(grepl("monthly|all",temporal_scale,ignore.case=T)){

  }

  # Initialize
  print("generate_figures completed succesfuly!")
  return(annual_data)
} # Close generate_figures function


## helpers and other functions below

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

filter_combos <- function (combos, sector, gcm) {
  NULL -> Sector -> GCM
  # filter to only relevant sectors or gcms
  if (sector=="all") {
    combos <- dplyr::filter(combos, (names(Sector)!="total") &
                              (names(Sector)!="nonag"))
  } else if (is.element(sector, c("dom","elec","irr","liv","mfg","min","nonag","total"))) {
    combos <- dplyr::filter(combos, (names(Sector)==sector))
  }

  if (is.element(gcm, c("gfdl", "hadgem", "ipsl", "miroc", "noresm"))) {
    combos <- dplyr::filter(combos, (GCM==gcm))
  }
  return (combos)
}

# make list of file paths for given ssp-rcp-gcm-sector combos
get_paths <- function(folder=NULL, combos) {
  NULL -> SSP -> RCP -> GCM -> Sector
  files <- dplyr::transmute(combos, paste0(
    folder, "/", names(SSP), "_",names(RCP), "_", GCM,
    "/wd", names(Sector), "_km3peryr.csv"))[[1]]
  return(files)
}

# builds figure from arguments using given data or loaded from folder
build_fig <- function(folder=NULL,
                     data=NULL,
                     timeStep="annual",
                     gcm="all",
                     sector="all",
                     stacked=F,
                     color="gcm") {
  NULL -> SSP -> RCP -> Sector -> Year -> Value -> GCM
  if (is.null(data)) {
    if (is.null(folder)) {
      print("No data or folder supplied. Not good.")
    } else {
      print("No data supplied, loading from folder.")
      combos <- all_combos()
      combos <- filter_combos(combos, sector=sector, gcm=gcm)
      files <- get_paths(folder, combos) # list of file paths

      print(paste0("Loading data from ", length(files), " files ..."))
      values <- data.table::rbindlist(lapply(files, function(x){ # stacks long
        data.frame(Value=colSums(data.table::fread(x, drop=1:5)))})) # sums of year columns
      print("Data loaded")

      annual <- tidyr::crossing(combos, Year=seq(2010,2100,by=5)) # each year for each combo
      data <- cbind(annual, values) # SSP, RCP, GCM, Sector, Year; Value
    }
  } else {
    print("Filtering preexisting data")
    data <- filter_combos(data, sector=sector, gcm=gcm)
  }
  print("computing summary data")
  if (gcm=="avg") {
    print("Computing mean of 5 GCMs ...")
    data <- dplyr::group_by(data, SSP, RCP, Sector, Year)
    data <- dplyr::summarise(data, Value=mean(Value))
    data <- tibble::add_column(data, GCM=rep("avg", nrow(data)), .after="RCP")
  }
  #return(data)
  print("building figure")
  p1 <- ggplot2::ggplot(data = data,
                        ggplot2::aes(x = Year,
                                     y = Value,
                                     group = interaction(GCM, Sector))) +
    ggplot2::facet_grid(RCP~SSP,scales="fixed") +
    ggplot2::ggtitle("Global Annual Water Withdrawal by SSP-RCP-GCM") +
    ggplot2::theme_gray() +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5))

  if (timeStep=="annual") {
    p1 <- p1 + ggplot2::xlab("Year") +
      ggplot2::ylab(bquote(Water~ Withdrawal ~ (km^3 ~ per ~ year)))
  } else if (timeStep=="monthly") {
    p1 <- p1 + ggplot2::xlab("Month") +
      ggplot2::ylab(bquote(Water~ Withdrawal ~ (km^3 ~ per ~ month)))
  }

  if (stacked) {
    p1 <- p1 + ggplot2::geom_area()
  } else {
    p1 <- p1 + ggplot2::geom_line()
  }

  if (color=="gcm") {
    p1 <- p1 + ggplot2::aes(color=GCM)
  } else if (color=="sector") {
    p1 <- p1 + ggplot2::aes(color=Sector)
  }
  return (p1)
}
