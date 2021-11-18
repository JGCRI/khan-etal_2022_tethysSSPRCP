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
                             temporal_scale = "annual") {
  NULL -> year -> value -> GCM

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

    datax <- build_df(folder)

    p1 <- ggplot2::ggplot(data = datax,
                          ggplot2::aes(x = year, y = value, group = GCM)) +
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



build_df <- function(folder=NULL, temporal_scale = "annual") {
  NULL -> SSP -> RCP -> GCM -> name -> temp -> n -> year
  combos <- tidyr::crossing(SSP = c("SSP 1", "SSP 2", "SSP 3", "SSP 4", "SSP 5"),
                            RCP = c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5"),
                            GCM = c("gfdl", "hadgem", "ipsl", "miroc", "noresm")
  )

  combos <- dplyr::filter(combos, !(SSP=="SSP 3" & RCP=="RCP 2.6") & # if rcp26, exclude ssp3
                            !(SSP!="SSP 5" & RCP=="RCP 8.5") & # if rcp85, must use ssp5
                            !(RCP=="RCP 8.5" & GCM!="gfdl") # if rcp85, must use gfdl
  )

  annual <- tidyr::crossing(combos, year=seq(2010,2100,by=5)) # each year for each combo

  folders <- dplyr::transmute(combos, name=tolower(
    (gsub(" |\\.", "",paste(SSP,RCP,GCM,sep="_"))))) # eg ssp1_rcp26_gfdl

  paths <- dplyr::transmute(folders, path=paste0(
    folder, "\\", name, "\\wdtotal_km3peryr.csv"))[[1]]

  #paths <- list.files(path=folder, full.names = TRUE, recursive = TRUE, pattern="*.csv")
  DT <- do.call(rbind, lapply(paths, function(x){colSums(data.table::fread(x, drop=1:5))}))
  dplyr::as_tibble(DT)%>%
    dplyr::mutate(n=1:dplyr::n())%>%
    tidyr::gather(key="year", value="value", -n)%>%
    dplyr::arrange(n)%>%
    dplyr::select(-n,-year)%>%
    dplyr::bind_cols(annual) ->
    df
  #df <- cbind(annual, stack(data.frame(t(DT)))[1])

  return(df)
}
