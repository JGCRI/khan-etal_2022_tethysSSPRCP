# testing generate_figures

# R studio top right window
# Go to more>document
# Install and Restart


folder_i="C:/Users/thom927/Documents/Data/tethysDemeterOutputs"

out <- generate_figures(folder = folder_i,
                        sectors="total",
                        gcm="all",
                        temporal_scale = "annual")

