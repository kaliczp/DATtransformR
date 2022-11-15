DATSelectedExport <- function(x, ID = c(139,140,"(204)")) {
    ## Parcel identification based on ID
    descript <- x$T_FELIRAT[x$T_FELIRAT$Text == ID[1],]
    if(length(ID) > 1) {
        for(id in 2:length(ID)) {
            descript <- rbind(descript, x$T_FELIRAT[x$T_FELIRAT$Text == ID[id],])
        }
    }
    parc.tab <- data.frame(DATtable = descript$Ref.tab, parcID = descript$Ref.tab.line)
### Area identification
    usedDATtables <- unique(descript$Ref.tab)
    ## Every table scanned
    parcel.ID  <- numeric()
    area.ID  <- numeric()
    for(DATtable in usedDATtables) {
        current.parcel.ID <- descript[descript$Ref.tab == DATtable, "Ref.tab.line"]
        parcel.ID <- c(parcel.ID, current.parcel.ID)
        ## Correct row number to parcel ID
        parcel.row.no <- x[[DATtable]][current.parcel.ID, 1]
        parcel.row.no <- as.numeric(parcel.row.no)
        current.parcel.ID <- as.numeric(current.parcel.ID)
        current.parcel.ID <- current.parcel.ID - (parcel.row.no - current.parcel.ID)
        assign(DATtable, x[[DATtable]][current.parcel.ID, ])
        area.ID <- c(area.ID, as.numeric(get(DATtable)[ ,3]))
    }
    ## Border identification
    DATtable.row <- which(x[["T_FELULET"]][, 1] == area.ID[1])
    area <- x[["T_FELULET"]][DATtable.row,]
    if(length(area.ID) > 1) {
        for(id in 2:length(ID)) {
            DATtable.row <- which(x[["T_FELULET"]][, 1] == area.ID[id])
            area <- rbind(area, x[["T_FELULET"]][DATtable.row, ])
        }
    }
    border.ID  <- as.numeric(area[,3])
    ## Borderline identification
    DATtable.row <- which(x[["T_HATAR"]][, 1] == border.ID[1])
    borders <- x[["T_HATAR"]][DATtable.row,]
    if(length(border.ID) > 1) {
        for(id in 2:length(border.ID)) {
            DATtable.row <- which(x[["T_HATAR"]][, 1] == border.ID[id])
            borders <- rbind(borders, x[["T_HATAR"]][DATtable.row,])
        }
    }
    borders <- borders[!duplicated(borders[, 3]),]
    border.IDs <- borders[, 3]
    borderlines <- x[["T_HATARVONAL"]][border.IDs,]
    ## Point identification
    point.IDs <- c(borderlines[,3],
                   borderlines[,4])
    point.IDs <- unique(point.IDs)
    points <- x[["T_PONT"]][point.IDs,]
    out.DAT <- list(Head = x$Head,
                    T_PONT = points,
                    T_VONAL = NULL,
                    T_HATARVONAL = borderlines,
                    T_HATAR = borders,
                    T_FELULET = area
                    )
    for(DATtable in usedDATtables)
        out.DAT[[DATtable]] <- get(DATtable)
    out.DAT$T_FELIRAT  <- descript
    out.DAT
}


selected.list <- DATSelectedExport(curr.list)
DAT_write(selected.list, "selecttesz.dat")
## recode u8..l2/cl selecttesz.dat
