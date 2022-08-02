library(rmap); library(jgcricolors)

# Generate distinct colors
# https://medialab.github.io/iwanthue/
my_palette = c("#ffbecf","#4ebe9f","#f3948c","#54e4e2","#ffa7a6",
               "#31d4ef","#eaa377","#2ab9e8","#dfac70","#92c9ff",
               "#dbcd81","#aba4e8","#e2f4a7","#ecc4ff","#b9cc81",
               "#c0c4ff","#bcab65","#a9dbff","#ffeda6","#78b6ba",
               "#ffb7a0","#77e6c7","#cda19e","#9aeebb","#ffe9ce",
               "#a7fffd","#96b574","#bce2d5","#d0ffc7","#a3baa3",
               "#f7ffd1","#85b799")
rmap::map(rmap::mapGCAMReg32, folder=getwd(), palette=my_palette, fileName = "gcam32")
rmap::map(rmap::mapGCAMLand, folder=getwd(), palette=my_palette, fileName = "gcamLand")
