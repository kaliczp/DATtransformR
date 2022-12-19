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
    x
}
