#' generate_figures
#'
#' Generate package figures
#'
#' @param folder Default = NULL. Path to folder with tethys outputs
#' @param temporal_scale Default = "annual". Option between "annual", "monthly" or "all"
#' @keywords figures
#' @export
#' @examples
#' library(khanetal2022tethysSSPRCP)
#' khanetal2022tethysSSPRCP::generate_figures()


generate_figures <- function(folder=NULL,
                             temporal_scale = "annual") {

  # Initialize
  print("Starting generate_figures ...")

  # Check if folder exists

  # Check if required files exist (sectors total, irr etc.)

  # Annual plots
  if(grepl("annual|all",temporal_scale,ignore.case=T)){

    # Figure 1: Global Faceted line plot showing
    # Facets: SSP (rows), RCP (cols)
    # Lines: GCMs are colored lines
    # X axis: Years
    # Aggregated to all grid cells (sector = total)

    # Example script
    datax <- data.frame(
      ssp = c(rep("ssp1",16),rep("ssp2",16)),
      gcm = rep(rep(c("gcm1","gcm2","gcm1","gcm2"),4),2),
      rcp = rep(rep(c("rcp1","rcp1","rcp2","rcp2"),4),2),
      x = rep(c(rep(2010,4),rep(2020,4),rep(2030,4),rep(2040,4)),2),
      value = runif(32)
    ); datax

    p1 <- ggplot2::ggplot(data = datax,
                          ggplot2::aes(x = x, y = value, group = gcm)) +
      ggplot2::geom_line(ggplot2::aes(color=gcm)) +
      ggplot2::facet_grid(ssp~rcp,scales="fixed") +
      ggplot2::ggtitle("my title") +
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
  print("generate_figures completed succesfuly.")

  } # Close generate_figures function
