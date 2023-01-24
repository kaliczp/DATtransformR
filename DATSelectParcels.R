DATSelectParcels.R <- function(x, ID, distance = 100) {
    ## Extract target parcel
    Directly.Selected <- DATSelectedExport(x, ID)
    ## Increase x,y limits with distance
    CoordsXY <- Directly.Selected$T_PONT[,c("x", "y")]
    RangeX <- range(CoordsXY$x)
    RangeY <- range(CoordsXY$y)
    RangeX <- RangeX + c(-distance, distance)
    RangeY <- RangeY + c(-distance, distance)
    ## Select parcels inside increased limits

    ## Results
    result <- list(
        Directly.Selected,
        RangeX,
        RangeY
    )
    result
}
