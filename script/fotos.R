

### este script es para las fotos ###

install.packages("jpeg")
install.packages("grid")

library(jpeg)
library(grid)

foto <- function (a) {
  imagen <- readJPEG(paste0("datos/fotos microorganismos/",a,".jpeg"))
  grid.raster(imagen)
}

foto(9)


