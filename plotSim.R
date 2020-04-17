library(raster)
library(viridisLite)

load("sam.RDa")

plotSim <- function(sim, dates) {
    coordinates(sim) <- ~x+y
    coordinates(dates) <- ~x+y
    r <- raster(extent(sam))
    res(r) <- 0.25
    sim.r <- rasterize(sim, r, "bp")
    land <- list(sam, fill=NA, first=FALSE)
    datesIn <- list("sp.points", dates[dates$score > 0,], pch=21, col="black",
                    fill="white")
    datesOut <- list("sp.points", dates[dates$score == 0,], pch=21, col="white",
                     fill="black")
    spplot(sim.r, col.regions=magma(100), sp.layout=list(land, datesIn,
                                                         datesOut),
           scales=list(draw=TRUE), ylab.right="sim BP",
           par.settings = list(layout.widths = list(key.right = 1.5)))
}