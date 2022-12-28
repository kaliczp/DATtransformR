DAT_Renumbering <- function(x) {
    ## New PONTid
    PONT.df <- data.frame(Old = x$T_PONT$Nr, New = 1:length(x$T_PONT$Nr))
    ## Replace PONTid
    x$T_PONT$Nr <- PONT.df$New
    ## Replace PONTid in HATARVONAL, T_OBJ_ATTRAC, T_FELIRAT, T_SZIMBOLUM
    for(NrRow in 1:nrow(PONT.df)) {
        ptId1 <- x$T_HATARVONAL$V3 == PONT.df[NrRow, "Old"]
        if(any(ptId1))
            x$T_HATARVONAL[ptId1, "V3"] <- PONT.df[NrRow, "New"]
        ptId2 <- x$T_HATARVONAL$V4 == PONT.df[NrRow, "Old"]
        if(any(ptId2))
            x$T_HATARVONAL[ptId2, "V4"] <- PONT.df[NrRow, "New"]
        ## T_OBJ_ATTRAC
        ptIDAC <- x$T_OBJ_ATTRAC$V4 == PONT.df[NrRow, "Old"]
        if(any(ptIDAC))
            x$T_OBJ_ATTRAC[ptIDAC, "V4"] <- PONT.df[NrRow, "New"]
        ## T_FELIRAT
        ptIDfelir <- x$T_FELIRAT$Pt.id == PONT.df[NrRow, "Old"]
        if(any(ptIDfelir))
            x$T_FELIRAT[ptIDfelir, "Pt.id"] <- PONT.df[NrRow, "New"]
        ## T_SZIMBOLUM
        ptIDszimb <- x$T_SZIMBOLUM$Pt.id == PONT.df[NrRow, "Old"]
        if(any(ptIDszimb))
            x$T_SZIMBOLUM[ptIDszimb, "Pt.id"] <- PONT.df[NrRow, "New"]
    }
    ## AC points ordering and numbering
    x$T_OBJ_ATTRAC <- x$T_OBJ_ATTRAC[order(as.numeric(x$T_OBJ_ATTRAC$V4)),]
    x$T_OBJ_ATTRAC$V1 <- 1:nrow(x$T_OBJ_ATTRAC) 
    ## SZIMBOLUM points renumbering
    x$T_SZIMBOLUM$Nr <- 1:nrow(x$T_SZIMBOLUM) 
    ## FELIRAT points renumbering
    x$T_FELIRAT$Nr <- 1:nrow(x$T_FELIRAT) 
    ## New HATARVONALid
    HATARVONAL.df <- data.frame(Old = x$T_HATARVONAL$V1, New = 1:nrow(x$T_HATARVONAL))
    ## Replace HATARVONALid
    x$T_HATARVONAL$V1 <- HATARVONAL.df$New
    ## Replace HATARVONALid in HATAR
    for(NrRow in 1:nrow(HATARVONAL.df)) {
        htvId <- x$T_HATAR$V3 == HATARVONAL.df[NrRow, "Old"]
        if(any(htvId))
            x$T_HATAR[htvId, "V3"]  <- HATARVONAL.df[NrRow, "New"]
    }
    ## New HATARid
    OldHATAR <- unique(x$T_HATAR$V1)
    HATAR.df <- data.frame(Old = OldHATAR, New = 1:length(OldHATAR))
    ## Replace HATARid and HATARid in FELULET
    for(NrRow in 1:nrow(HATAR.df)) {
        x$T_HATAR[x$T_HATAR$V1 == HATAR.df[NrRow, "Old"], "V1"] <- HATAR.df[NrRow, "New"]
        x$T_FELULET[x$T_FELULET$V3 == HATAR.df[NrRow, "Old"], "V3"]  <- HATAR.df[NrRow, "New"]
    }
    FELULET.df <- data.frame(Old = x$T_FELULET$V1, New = 1:nrow(x$T_FELULET))
    ## Replace FELULETid and FELULETid in OBJ_ATTR
    for(NrRow in 1:nrow(FELULET.df)) {
        x$T_FELULET[x$T_FELULET$V1 == FELULET.df[NrRow, "Old"], "V1"] <- FELULET.df[NrRow, "New"]
        x$T_OBJ_ATTRBD[x$T_OBJ_ATTRBD$V3 == FELULET.df[NrRow, "Old"], "V3"]  <- FELULET.df[NrRow, "New"]
        x$T_OBJ_ATTRBF[x$T_OBJ_ATTRBF$V3 == FELULET.df[NrRow, "Old"], "V3"]  <- FELULET.df[NrRow, "New"]
        x$T_OBJ_ATTRCA[x$T_OBJ_ATTRCA$V3 == FELULET.df[NrRow, "Old"], "V3"]  <- FELULET.df[NrRow, "New"]
    }
    ### Renumber all OBJs
    ## Identify tables
    obj.names <- grep("OBJ", names(selected.list), value = TRUE)
    ## Remove already renumbered AC
    obj.names <- obj.names[-grep("AC", obj.names)]
    ## Edit given OBJs
    for(aktobj in obj.names) {
        old.objid <- x[[aktobj]][ ,1]
        new.objid <- 1:length(old.objid)
        ## Replace number with new
        x[[aktobj]][ ,1]  <-  new.objid
        ## Quick-and-dirty replacement in referring tables
        if(any(x$T_FELIRAT$Ref.tab == aktobj)) {
            for(akt.line in 1:length(old.objid)) {
                x$T_FELIRAT[x$T_FELIRAT$Ref.tab.line == old.objid, "Ref.tab.line"]  <- new.objid
            }
        }
        if(any(x$T_SZIMBOLUM$Ref.tab == aktobj)) {
            for(akt.line in 1:length(old.objid)) {
                x$T_SZIMBOLUM[x$T_SZIMBOLUM$Ref.tab.line == old.objid, "Ref.tab.line"]  <- new.objid
            }
        }
    }
    x
}
