DATSelect <- function(x, ID = 139, obj = "BD") {
    DATtable.name <- paste0("T_OBJ_ATTR", obj)
    DATtable.row <- which(x[[DATtable.name]][, 4] == ID)
    area.ID <- x[[DATtable.name]][DATtable.row, 3]
    DATtable.row <- which(x[["T_FELULET"]][, 1] == area.ID)
    border.ID <- x[["T_FELULET"]][DATtable.row, 3]
    DATtable.row <- which(x[["T_HATAR"]][, 1] == border.ID)
    DAT.border.lines <- x[["T_HATAR"]][DATtable.row, 3]
    DAT.point.IDs <- c(x[["T_HATARVONAL"]][DAT.border.lines,3],
                       x[["T_HATARVONAL"]][DAT.border.lines,4])
    DAT.point.IDs <- unique(DAT.point.IDs)
    DATtable.rows = numeric()
    for(tti in 1:length(DAT.point.IDs))
        DATtable.rows <- c(DATtable.rows, which(x[["T_PONT"]][, 1] == DAT.point.IDs[tti]))
    x[["T_PONT"]][DATtable.rows,]
}


DATSelect(curr.list)[,c("x","y")]

const.shift <- rnorm(2, sd = 0.06)


Hmean <- mean(curr.list$T_PONT[curr.list$T_PONT$H > 1, "H"], na.rm = TRUE)
## foldr <- c(139,"207/2",40,42,"104/3",48,184,245,246,91,"54/9","54/8")
foldr <- c(140,"207/4",39,41,"103/2","49/1",183,"244/2",247,"92/1","54/8","54/9")

foldrok <- sub("/", "_", foldr)
KezdoPontszam <- 2000
for(tti in 1:10) {
    coo.sel <- DATSelect(curr.list, ID = foldr[tti])[,c("x","y")]
    coo.sel[,"x"] <- coo.sel[,"x"] + const.shift[1] + rnorm(nrow(coo.sel), sd = 0.02)
    coo.sel[,"y"] <- coo.sel[,"y"] + const.shift[2] + rnorm(nrow(coo.sel), sd = 0.02)
    coo.sel$H <- rnorm(nrow(coo.sel), Hmean, 0.1)
    coo.sel <- cbind(Name = paste0("GPS", KezdoPontszam + 0:(nrow(coo.sel)-1)), coo.sel)
    coo.ref <- coo.sel[1,]
    coo.ref[,1] <- paste("RTCM-Ref", 4000 + tti)
    coo.ref[,"x"] <- coo.ref[,"x"] + (1000 * rnorm(1))
    coo.ref[,"y"] <- coo.ref[,"y"] + (1000 * rnorm(1))
    coo.ref[,"H"] <- coo.ref[,"H"] + rnorm(1)
    coo.ok <- rbind(coo.ref, coo.sel)
    coo.ok$x <- round(coo.ok$x, 3)
    coo.ok$y <- round(coo.ok$y, 3)
    coo.ok$H <- round(coo.ok$H, 3)
    ## Zero padding?
    write.table(coo.ok, paste0("Meas", foldrok[tti], ".csv"), quot = FALSE, eol = "\r\n", sep = ",", row.names = FALSE, col.names = FALSE)
}