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

