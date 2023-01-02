DATSelectedExport <- function(x, ID = c(139,140,"(204)")) {
### Check input database
    if(!is.list(x))
        stop("x argument is not a list")
### T_FELIRAT checked for parcel names 
    ## Parcel identification based on first ID
    descript <- x$T_FELIRAT[x$T_FELIRAT$Text == ID[1] &
                            x$T_FELIRAT$Type == 11
                           ,]
    ## In case of multiple ID
    if(length(ID) > 1) {
        for(id in 2:length(ID)) {
            descript <- rbind(descript, x$T_FELIRAT[x$T_FELIRAT$Text == ID[id] &
                                                    x$T_FELIRAT$Type == 11,
                                                    ]
                              )
        }
    }
    ## If no rows selected stop!
    if(nrow(descript) == 0) stop(paste("No parcel identified!\n Name(s)",
                                      paste(ID, collapse = ", "),
                                      "are correct?"))
    ## Remove parentheses of public parcels
    ID <- sub("\\)","",sub("\\(","",ID))
### All tables except header listed
    NOTusedDATtables <- names(x)
    NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "Head"]
### Empty geometry and parcel id vectors created for collection
    ## Create empty pointID object
    point.IDs <- numeric()
    ## Create empty line.IDs object
    line.IDs <- numeric()
    ## Create empty vector for parcel collection
    parcel.ID  <- numeric()
    ## Finally create empty area.ID for areas
    area.ID  <- numeric()
### T_FELIRAT referenced attribute tables (typically BC and BD) identification
    usedDATtables <- unique(descript$Ref.tab)
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
### C objects class
    ## Check CA and if some examples are exits then CB object groups based on selected parcels public and owned (BC and BD)
    if(exists("T_OBJ_ATTRCA", where = x)) {
        TabCA.lines <- numeric()
        for(curr.tab in usedDATtables) {
            curr.column <- ifelse(curr.tab == "T_OBJ_ATTRBC", 5, 6)
            curr.parcels <- get(curr.tab)[,1]
            for(id in curr.parcels) {
                TabCA.lines <- c(TabCA.lines, which(x$T_OBJ_ATTRCA[, curr.column] == id))
            }
        }
        if(length(TabCA.lines) > 0) {
            assign("T_OBJ_ATTRCA", x$T_OBJ_ATTRCA[TabCA.lines, ])
            area.ID <- c(area.ID, as.numeric(T_OBJ_ATTRCA[ ,3]))
            usedDATtables <- c(usedDATtables, "T_OBJ_ATTRCA")
            point.IDs <- c(point.IDs, as.numeric(T_OBJ_ATTRCA$V20))
            ## If there are CA objects then check CB
            ## Check CB object group
            if(exists("T_OBJ_ATTRCB", where = x)) {
                TabCB.lines <- numeric()
                building.ID <- T_OBJ_ATTRCA$V1
                for(id in 1:length(building.ID))
                    TabCB.lines <- c(TabCB.lines, which(x$T_OBJ_ATTRCB$V5 == building.ID[id]))
                if(length(TabCB.lines) > 0) {
                    assign("T_OBJ_ATTRCB", x$T_OBJ_ATTRCB[TabCB.lines, ])
                    if(T_OBJ_ATTRCB$V3 == 3) {
                        area.ID <- c(area.ID, as.numeric(T_OBJ_ATTRCB$V4))
                    } else {
                        warning("Not area-type building accessories!")
                    }
                    usedDATtables <- c(usedDATtables, "T_OBJ_ATTRCB")
                }
                ## T_OBJ_ATTRCB is removed from NOTusedDATtables
                NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_OBJ_ATTRCB"]
            }
        }
        ## T_OBJ_ATTRCA is removed from NOTusedDATtables
        NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_OBJ_ATTRCA"]
    }
### T_SZIMBOLUM talbe check (BC BD CA CB mentioned in this table)
    ## Get all referenced symbols
    if(exists("T_SZIMBOLUM", where = x)) {
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
        }
        ## T_SZIMBOLUM is removed from NOTusedDATtables
        NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_SZIMBOLUM"]
    }
### Check other B class (BC BD already checked) based on parcel ID-s
    ## Check BE object group
    if(exists("T_OBJ_ATTRBE", where = x)) {
        TabBE.lines <- numeric()
        for(id in 1:length(ID))
            TabBE.lines <- c(TabBE.lines, which(x$T_OBJ_ATTRBE$V5 == ID[id]))
        if(length(TabBE.lines) > 0) {
            assign("T_OBJ_ATTRBE", x$T_OBJ_ATTRBE[TabBE.lines, ])
            area.ID <- c(area.ID, as.numeric(T_OBJ_ATTRBE[ ,3]))
            usedDATtables <- c(usedDATtables, "T_OBJ_ATTRBE")
        }
        ## T_OBJ_ATTRBE is removed from NOTusedDATtables
        NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_OBJ_ATTRBE"]
    }
    ## Check BF object group
    if(exists("T_OBJ_ATTRBF", where = x)) {
        TabBF.lines <- numeric()
        for(id in 1:length(ID))
            TabBF.lines <- c(TabBF.lines, which(x$T_OBJ_ATTRBF$V13 == ID[id]))
        if(length(TabBF.lines) > 0) {
            assign("T_OBJ_ATTRBF", x$T_OBJ_ATTRBF[TabBF.lines, ])
            area.ID <- c(area.ID, as.numeric(T_OBJ_ATTRBF[ ,3]))
            usedDATtables <- c(usedDATtables, "T_OBJ_ATTRBF")
        }
        ## T_OBJ_ATTRBF is removed from NOTusedDATtables
        NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_OBJ_ATTRBF"]
    }
    ## Check BG object group based on ID of enclosing parcels
    if(exists("T_OBJ_ATTRBG", where = x)) {
        TabBG.lines <- numeric()
        for(id in 1:length(ID))
            TabBG.lines <- c(TabBG.lines, which(x$T_OBJ_ATTRBG$V5 == ID[id]))
        if(length(TabBG.lines) > 0) {
            assign("T_OBJ_ATTRBG", x$T_OBJ_ATTRBG[TabBG.lines, ])
            area.ID <- c(area.ID, as.numeric(T_OBJ_ATTRBG[ ,3]))
            ## Are there any referring points
            T_OBJ_ATTRBG[, 28] <- as.numeric(T_OBJ_ATTRBG[, 28])
            if(any(!is.na(T_OBJ_ATTRBG[, 28])))
                ## If any georef point is added
                point.IDs <- c(point.IDs, T_OBJ_ATTRBG[!is.na(T_OBJ_ATTRBG[, 28]), 28])
            usedDATtables <- c(usedDATtables, "T_OBJ_ATTRBG")
        }
        ## T_OBJ_ATTRBF is removed from NOTusedDATtables
        NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_OBJ_ATTRBG"]
    }
### AD objects
    ## Check AD address coordinate objects
    if(exists("T_OBJ_ATTRAD", where = x)) {
        TabAD.lines  <- numeric()
        ## Select possible tables
        possibletab <- c("BC", "BD", "CA", "BG")
        for(currtabnum in 1:length(possibletab)) {
            ## Convert tabnum to column in AD table
            currADtabcol <- 8 + currtabnum
            ## Generate attr. table name
            currtabname <- paste0("T_OBJ_ATTR", possibletab[currtabnum])
            ## Extract all address coordinates to all selected objects
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
        if(length(TabAD.lines) > 0) {
            assign("T_OBJ_ATTRAD", x$T_OBJ_ATTRAD[TabAD.lines, ])
            ## AD table added
            usedDATtables <- c(usedDATtables, "T_OBJ_ATTRAD")
            ## AD points selected
            point.IDs <- c(point.IDs, as.numeric(T_OBJ_ATTRAD$V5))
        }
        ## T_OBJ_ATTRAD is removed from NOTusedDATtables
        NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_OBJ_ATTRAD"]
    }
### T_FELIRAT table completion
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
### Geometry completion 1 until T_HATARVONAL
    ## Area check for uniqueness
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
    ## T_HATAR is removed from NOTusedDATtables
    NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_HATAR"]
    borders <- x[["T_HATAR"]][DATtable.row,]
    border.IDs <- unique(borders[, 3])
    borderlines <- x[["T_HATARVONAL"]][border.IDs,]
    ## T_HATARVONAL is removed from NOTusedDATtables
    NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_HATARVONAL"]
### Geometry completion 2 with points
    ## Point identification
    point.IDs <- c(point.IDs,
                   borderlines[,3],
                   borderlines[,4])
    point.IDs <- c(point.IDs, descript[,3])
    point.IDs <- unique(point.IDs)
    points <- x[["T_PONT"]][point.IDs,]
    ## T_PONT is removed from NOTusedDATtables
    NOTusedDATtables  <- NOTusedDATtables[!NOTusedDATtables == "T_PONT"]
### A objects class
### AC tables collection
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
### Final geomtery completion
    ## Order geometry tables
    points <- points[order(points[,1]),]
    borderlines <- borderlines[order(borderlines[,1]),]
    borders <- borders[order(borders[,1]),]
    area <- area[order(area[,1]),]
### Output completion
    ## Collect extracted geometries
    out.DAT <- list(Head = x$Head,
                    T_PONT = points)
    if(length(line.IDs) > 0) {
        out.DAT$T_VONAL <- lines
    }
    out.DAT$T_HATARVONAL <- borderlines
    out.DAT$T_HATAR <- borders
    out.DAT$T_FELULET <- area
    ## Collect attribute tables
    ## Output point attributes
    out.DAT$T_OBJ_ATTRAC  <-  T_OBJ_ATTRAC
    ## Order and collect other used tables
    usedDATtables <- usedDATtables[order(usedDATtables)]
    for(DATtable in usedDATtables)
        out.DAT[[DATtable]] <- get(DATtable)
    ## T_FELIRAT and optional T_SZIMBOLUM tables completion
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
