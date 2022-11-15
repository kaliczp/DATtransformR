DATSelectedExport <- function(x, ID = c(139,140), obj = "BD") {
    ## Area identification
    DATtable.name <- paste0("T_OBJ_ATTR", obj)
    DATtable.row <- which(x[[DATtable.name]][, 4] == ID[1])
    parcel <- x[[DATtable.name]][DATtable.row, ]
    parcel.ID  <- parcel[,1]
    ## parcel id 
    descript <- x[["T_FELIRAT"]][x[["T_FELIRAT"]][,"Ref.tab"] == DATtable.name &
                                 x[["T_FELIRAT"]][,"Type"] == 11 &
                                 x[["T_FELIRAT"]][,"Ref.tab.line"] == parcel.ID,
                                 ]
    if(length(ID) > 1) {
        for(id in 2:length(ID)) {
            DATtable.row <- which(x[[DATtable.name]][, 4] == ID[id])
            parcel <- rbind(parcel, x[[DATtable.name]][DATtable.row, ])
            parcel.ID  <- c(parcel.ID, parcel[nrow(parcel), 1])
            descript <- rbind(descript, x[["T_FELIRAT"]][x[["T_FELIRAT"]][,"Ref.tab"] == DATtable.name &
                                                         x[["T_FELIRAT"]][,"Type"] == 11 &
                                                         x[["T_FELIRAT"]][,"Ref.tab.line"] == parcel.ID[length(parcel.ID)],
                                                         ]
                              )
        }
    }
    area.ID <- as.numeric(parcel[,3])
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
                    T_HATARVONAL = borderlines,
                    T_HATAR = borders,
                    T_FELULET = area,
                    tabla = parcel,
                    T_FELIRAT = descript
                    )
    out.names <- names(out.DAT)
    out.names[out.names == "tabla"] <-  DATtable.name
    names(out.DAT) <- out.names
    out.DAT
}


selected.list <- DATSelectedExport(curr.list)
DAT_write(selected.list, "selecttesz.dat")
## recode u8..l2/cl selecttesz.dat
