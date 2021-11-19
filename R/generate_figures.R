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
                             sectors="total",
                             gcm="all",
                             temporal_scale="annual") {
  NULL -> Year -> Value -> GCM

  # Initialize
  print("Starting generate_figures ...")


  # Check if folder exists
  # Check if required files exist (sectors total, irr etc.)

  # Annual plots
  if(grepl("annual|all",temporal_scale,ignore.case=T)){

    # Figure 1: Global Faceted line plot showing
    # Facets: SSP (cols), RCP (rows)
    # Lines: GCMs are colored lines
    # X axis: Years
    # Aggregated to all grid cells (sector = total)

    # Example script

    datax <- get_annual_data(folder, sectors=sectors, gcm=gcm)

    p1 <- ggplot2::ggplot(data = datax,
                          ggplot2::aes(x = Year, y = Value, group = GCM)) +
      ggplot2::geom_line(ggplot2::aes(color=GCM)) +
      ggplot2::facet_grid(RCP~SSP,scales="fixed") +
      ggplot2::ggtitle("Global Annual Water Withdrawal by SSP-RCP-GCM") +
      ggplot2::xlab("Year") +
      ggplot2::ylab(bquote(Water~ Withdrawal ~ (km^3 ~ per ~ year)))
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)); p1

    ggplot2::ggsave(filename =  "my_file_name.png",
                    plot = p1,
                    width = 13,
                    height = 10) # save plot

    print(paste0("Figure saved as : my_file_name.png."))


  }

  # Monthly plots
  if(grepl("monthly|all",temporal_scale,ignore.case=T)){

  }

  # Initialize
  print("generate_figures completed succesfuly!")
  } # Close generate_figures function


# all valid ssp-rcp-gcm-sector (including total, nonag sectors)
all_combos <- function() {
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

get_paths <- function(folder=NULL, combos) {
  files <- dplyr::transmute(combos, paste0(
    folder, "/", names(SSP), "_",names(RCP), "_", GCM,
    "/wd", names(Sector), "_km3peryr.csv"))[[1]]
  return(files)
}


# get annual data for a single sector, all sectors, or totals
get_annual_data <- function(folder=NULL,
                            sectors="total",
                            gcm="all") {
  NULL -> SSP -> RCP -> GCM -> Year

  combos <- all_combos() # 71 valid ssp-rcp-gcm * 6+2 sectors/totals

  # filter to only relevant sectors or gcms
  if (sectors=="all") {
    print("Using all sectors (except 'total' and 'nonag')")
    combos <- dplyr::filter(combos, (names(Sector)!="total") &
                               (names(Sector)!="nonag"))
  }
  if (is.element(sectors, c("dom","elec","irr","liv","mfg","min","nonag","total"))) {
    print(paste0("Using sector: ", sectors))
    combos <- dplyr::filter(combos, (names(Sector)==sectors))
  }
  if (is.element(gcm, c("gfdl", "hadgem", "ipsl", "miroc", "noresm"))) {
    print(paste0("Using GCM: ", gcm))
    combos <- dplyr::filter(combos, (GCM==gcm))
  }

  files <- get_paths(folder, combos) # list of file paths

  print(paste0("Loading data from ", length(files), " files ..."))

  values <- data.table::rbindlist(lapply(files, function(x){ # stacks long
    data.frame(Value=colSums(data.table::fread(x, drop=1:5)))})) # sums of year columns

  print("Data loaded")

  annual <- tidyr::crossing(combos, Year=seq(2010,2100,by=5)) # each year for each combo
  df <- cbind(annual, values) # SSP,RCP,GCM,

  # compute mean of 5 GCMs if requested (instead of, not in addition to)
  if (gcm=="avg") {
    print("Computing mean of 5 GCMs ...")
    df <- dplyr::group_by(df, SSP, RCP, Sector, Year)
    df <- dplyr::summarise(df, Value=mean(Value))
    df <- tibble::add_column(df, GCM=rep("avg", nrow(df)), .after="RCP")
  }
  return(df)
}
