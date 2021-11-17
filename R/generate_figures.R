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
    # Facets: SSP (cols), RCP (rows)
    # Lines: GCMs are colored lines
    # X axis: Years
    # Aggregated to all grid cells (sector = total)

    # Example script
    ssp_names = c("SSP 1", "SSP 2", "SSP 3", "SSP 4", "SSP 5")
    rcp_names = c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5")
    gcm_names = c("gfdl", "hadgem", "ipsl", "miroc", "noresm")

    forbidden = c("ssp3_rcp26",
                  "ssp1_rcp85",
                  "ssp2_rcp85",
                  "ssp3_rcp85",
                  "ssp4_rcp85",
                  "rcp85_hadgem",
                  "rcp85_ipsl",
                  "rcp85_miroc",
                  "rcp85_noresm")

    nvalid = 71*19 # 71 valid ssp-rcp-gcm combos x 19 years
    SSPS = character(nvalid)
    RCPS = character(nvalid)
    GCMS = character(nvalid)
    X = integer(nvalid)
    Y = double(nvalid)

    counter = 1
    for (ssp in ssp_names){
      for (rcp in rcp_names){
        for (gcm in gcm_names){
          subfolder = tolower((gsub(" |\\.", "",paste(ssp,rcp,gcm,sep="_"))))
          bad = FALSE
          for (name in forbidden){
            if (grepl(name,subfolder,fixed=TRUE)){
              bad = TRUE
            }
          }
          if (!bad){
            SSPS[counter:(counter+18)]=ssp
            RCPS[counter:(counter+18)]=rcp
            GCMS[counter:(counter+18)]=gcm
            X[counter:(counter+18)] = seq(2010, 2100, by=5)
            filename = paste0(folder,"\\",subfolder,"\\wdtotal_km3peryr.csv")
            csv_data = read.csv(filename)
            Y[counter:(counter+18)] = colSums(csv_data[6:ncol(csv_data)])
            counter = counter + 19
            print(paste(subfolder, "complete", (counter-1)/19))
          }
        }
      }
    }

    datax <- data.frame(
      ssps = SSPS,
      rcps = RCPS,
      gcms = GCMS,
      x = X,
      y = Y
    ); datax

    p1 <- ggplot2::ggplot(data = datax,
                          ggplot2::aes(x = x, y = y, group = gcms)) +
      ggplot2::geom_line(ggplot2::aes(color=gcms)) +
      ggplot2::facet_grid(rcps~ssps,scales="fixed") +
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
  return(datax)
  } # Close generate_figures function
