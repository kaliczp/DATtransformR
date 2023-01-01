DATSelectedExport <- function(x, ID = c(139,140,"(204)")) {
    if(!is.list(x))
        stop("x argument is not a list")
    ## Parcel identification based on ID
    descript <- x$T_FELIRAT[x$T_FELIRAT$Text == ID[1] &
                            x$T_FELIRAT$Type == 11
                           ,]
    if(length(ID) > 1) {
        for(id in 2:length(ID)) {
            descript <- rbind(descript, x$T_FELIRAT[x$T_FELIRAT$Text == ID[id] &
                                                    x$T_FELIRAT$Type == 11,
                                                    ]
                              )
        }
    }
    if(nrow(descript) == 0) stop(paste("No parcel identified!\n Name(s)",
                                      paste(ID, collapse = ", "),
                                      "are correct?"))
    ## Remove parentheses of public parcels
    ID <- sub("\\)","",sub("\\(","",ID))
### All tables except header listed
    NOTusedDATtables <- names(x)
    NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "Head"]
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
        current.parcel.ID <- current.parcel.ID - (parcel.row.no - current.parcel.ID)
        assign(DATtable, x[[DATtable]][current.parcel.ID, ])
        area.ID <- c(area.ID, as.numeric(get(DATtable)[ ,3]))
        ## Processed table removed
        NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == DATtable]
    }
    ## Create empty pointID object
    point.IDs <- numeric()
    ## Create empty line.IDs object
    line.IDs <- numeric()
    ## Get all referenced symbols
    if(nrow(x$T_SZIMBOLUM) > 1) {
        symbols <- x$T_SZIMBOLUM[1, ]
        for(DATtable in usedDATtables) {
            for(id in get(DATtable)[, 1]) {
                symbols <- rbind(symbols, x$T_SZIMBOLUM[x$T_SZIMBOLUM$Ref.tab == DATtable &
                                                      x$T_SZIMBOLUM$Ref.tab.line == id,
                                                      ]
                                 )
            }
        }
        symbols <- symbols[-1,]
        if(nrow(symbols) > 0) {
            point.IDs <- c(point.IDs, as.numeric(symbols$Pt.id))
            ## T_SZIMBOLUM is removed from NOTusedDATtables
            NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_SZIMBOLUM"]
        }
    }
    ## Check CA object group
    if(nrow(x$T_OBJ_ATTRCA) > 0) {
        TabCA.lines <- numeric()
        for(curr.tab in usedDATtables) {
            curr.column <- ifelse(curr.tab == "T_OBJ_ATTRBC", 5, 6)
            curr.parcels <- get(curr.tab)[,1]
            for(id in curr.parcels) {
                TabCA.lines <- c(TabCA.lines, which(x$T_OBJ_ATTRCA[, curr.column] == id))
            }
        }
    }
    if(length(TabCA.lines) > 0) {
        assign("T_OBJ_ATTRCA", x$T_OBJ_ATTRCA[TabCA.lines, ])
        area.ID <- c(area.ID, as.numeric(T_OBJ_ATTRCA[ ,3]))
        usedDATtables <- c(usedDATtables, "T_OBJ_ATTRCA")
        point.IDs <- c(point.IDs, as.numeric(T_OBJ_ATTRCA$V20))
        ## T_OBJ_ATTRCA is removed from NOTusedDATtables
        NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_OBJ_ATTRCA"]
    }
    ## If there are CA objects check CB
    if(length(TabCA.lines) > 0) {
        ## Check CB object group
        if(nrow(x$T_OBJ_ATTRCB) > 0) {
            TabCB.lines <- numeric()
            building.ID <- T_OBJ_ATTRCA$V1
            for(id in 1:length(building.ID))
                TabCB.lines <- c(TabCB.lines, which(x$T_OBJ_ATTRCB$V5 == building.ID[id]))
        }
        if(length(TabCB.lines) > 0) {
            assign("T_OBJ_ATTRCB", x$T_OBJ_ATTRCB[TabCB.lines, ])
            if(T_OBJ_ATTRCB$V3 == 3) {
                area.ID <- c(area.ID, as.numeric(T_OBJ_ATTRCB$V4))
            } else {
                warning("Not area-type building accessories!")
            }
            usedDATtables <- c(usedDATtables, "T_OBJ_ATTRCB")
            ## T_OBJ_ATTRCB is removed from NOTusedDATtables
            NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_OBJ_ATTRCB"]
        }
    }
    ## Check BE object group
    TabBE.lines <- numeric()
    for(id in 1:length(ID))
        TabBE.lines <- c(TabBE.lines, which(x$T_OBJ_ATTRBE$V5 == ID[id]))
    if(length(TabBE.lines) > 0) {
        assign("T_OBJ_ATTRBE", x$T_OBJ_ATTRBE[TabBE.lines, ])
        area.ID <- c(area.ID, as.numeric(T_OBJ_ATTRBE[ ,3]))
        usedDATtables <- c(usedDATtables, "T_OBJ_ATTRBE")
        ## T_OBJ_ATTRBE is removed from NOTusedDATtables
        NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_OBJ_ATTRBE"]
    }
    ## Check BF object group
    TabBF.lines <- numeric()
    for(id in 1:length(ID))
        TabBF.lines <- c(TabBF.lines, which(x$T_OBJ_ATTRBF$V13 == ID[id]))
    if(length(TabBF.lines) > 0) {
        assign("T_OBJ_ATTRBF", x$T_OBJ_ATTRBF[TabBF.lines, ])
        area.ID <- c(area.ID, as.numeric(T_OBJ_ATTRBF[ ,3]))
        usedDATtables <- c(usedDATtables, "T_OBJ_ATTRBF")
        ## T_OBJ_ATTRBF is removed from NOTusedDATtables
        NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_OBJ_ATTRBF"]
    }
    ## Select all descriptive text for all object groups
    for(DATtable in usedDATtables) {
        for(id in get(DATtable)[, 1]) {
            descript <- rbind(descript, x$T_FELIRAT[x$T_FELIRAT$Ref.tab == DATtable &
                                                    x$T_FELIRAT$Ref.tab.line == id,
                                                    ]
                              )
        }
    }
    descript <- unique(descript)
    ## T_FELIRAT is removed from NOTusedDATtables
    NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_FELIRAT"]
    ## Geometry selection
    area.ID <- unique(area.ID)
    ## Border identification
    DATtable.row <- which(x[["T_FELULET"]][, 1] == area.ID[1])
    if(length(area.ID) > 1) {
        for(id in area.ID[-1]) {
            DATtable.row <- c(DATtable.row, which(x[["T_FELULET"]][, 1] == id))
        }
    }
    area <- x[["T_FELULET"]][DATtable.row,]
    ## T_FELULET is removed from NOTusedDATtables
    NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_FELULET"]
    border.ID  <- as.numeric(area[,3])
    ## Borderline identification
    DATtable.row <- which(x[["T_HATAR"]][, 1] == border.ID[1])
    if(length(border.ID) > 1) {
        for(id in 2:length(border.ID)) {
            DATtable.row <- c(DATtable.row, which(x[["T_HATAR"]][, 1] == border.ID[id]))
        }
    }
    ## Check AD address coordinate objects
    if(nrow(x$T_OBJ_ATTRAD) > 0) {
        TabAD.lines  <- numeric()
        ## Select possible tables
        possibletab <- c("BC", "BD", "CA", "BG")
        for(currtabnum in 1:length(possibletab)) {
            currADtabcol <- 8 + currtabnum
            currtabname <- paste0("T_OBJ_ATTR", possibletab[currtabnum])
            if(any(usedDATtables == currtabname)){
                x$T_OBJ_ATTRAD[, currADtabcol] <- as.numeric(x$T_OBJ_ATTRAD[, currADtabcol])
                if(any(!is.na(x$T_OBJ_ATTRAD[, currADtabcol]))){
                    ## Select lines
                    curr.parcels <- get(currtabname)[,1]
                    for(id in curr.parcels) {
                        currADid <- which(x$T_OBJ_ATTRAD[, currADtabcol] == id)
                        TabAD.lines <- c(TabAD.lines, currADid)
                    }
                }
            }
        }
    }
    if(length(TabAD.lines) > 0) {
        assign("T_OBJ_ATTRAD", x$T_OBJ_ATTRAD[TabAD.lines, ])
        ## AD table added
        usedDATtables <- c(usedDATtables, "T_OBJ_ATTRAD")
        ## AD points selected
        point.IDs <- c(point.IDs, as.numeric(T_OBJ_ATTRAD$V5))
        ## T_OBJ_ATTRAD is removed from NOTusedDATtables
        NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_OBJ_ATTRAD"]
    }
    ## T_HATAR is removed from NOTusedDATtables
    NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_HATAR"]
    borders <- x[["T_HATAR"]][DATtable.row,]
    border.IDs <- unique(borders[, 3])
    borderlines <- x[["T_HATARVONAL"]][border.IDs,]
    ## T_HATARVONAL is removed from NOTusedDATtables
    NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_HATARVONAL"]
    ## Point identification
    point.IDs <- c(point.IDs,
                   borderlines[,3],
                   borderlines[,4])
    point.IDs <- c(point.IDs, descript[,3])
    point.IDs <- unique(point.IDs)
    points <- x[["T_PONT"]][point.IDs,]
    ## T_PONT is removed from NOTusedDATtables
    NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_PONT"]
    ## Order geometry tables
    points <- points[order(points[,1]),]
    borderlines <- borderlines[order(borderlines[,1]),]
    borders <- borders[order(borders[,1]),]
    area <- area[order(area[,1]),]
    usedDATtables <- usedDATtables[order(usedDATtables)]
    ## Ordinary points attributes
    pointattr.row <- numeric()
    for(p.id in 1:length(point.IDs))
        pointattr.row <- c(pointattr.row,which(x$T_OBJ_ATTRAC[,4] == point.IDs[p.id]))
    T_OBJ_ATTRAC <- x$T_OBJ_ATTRAC[pointattr.row, ]
    ## T_OBJ_ATTRAC is removed from NOTusedDATtables
    NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_OBJ_ATTRAC"]
    if(length(NOTusedDATtables) > 0) warning(paste("Tables are not checked!",
                                                 paste(NOTusedDATtables, collapse = ", ")
                                                 )
                                           )
    out.DAT <- list(Head = x$Head,
                    T_PONT = points)
    if(length(line.IDs) > 0) {
        out.DAT$T_VONAL <- lines
    }
    out.DAT$T_HATARVONAL <- borderlines
    out.DAT$T_HATAR <- borders
    out.DAT$T_FELULET <- area
    out.DAT$T_OBJ_ATTRAC  <-  T_OBJ_ATTRAC
    for(DATtable in usedDATtables)
        out.DAT[[DATtable]] <- get(DATtable)
    out.DAT$T_FELIRAT  <- descript
    if(nrow(symbols) > 0) {
        out.DAT$T_SZIMBOLUM <- symbols
    }
    out.DAT
}


##selected.list <- DATSelectedExport(curr.list, c("078/1", "078/2"))
selected.list <- DATSelectedExport(curr.list)
DAT_write(selected.list, "selecttesz.dat")
## recode u8..l2/cl selecttesz.dat
