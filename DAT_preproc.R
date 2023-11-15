DAT_preproc <- function(x) {
    ## T_PONT
    names(x$T_PONT) <- c("Nr", "x", "y", "H", "errV", "errM")
    ## T_FELIRAT
    names(x$T_FELIRAT) <- c("Nr", "Text", "Pt.id", "Angle", "Font", "Valid", "Ref.tab", "Ref.tab.line", "Type")

    x$T_FELIRAT$Nr <- as.numeric(x$T_FELIRAT$Nr)
    x$T_FELIRAT$Pt.id <- as.numeric(x$T_FELIRAT$Pt.id)
    x$T_FELIRAT$Ref.tab.line <- as.numeric(x$T_FELIRAT$Ref.tab.line)
    x$T_FELIRAT$Type <- as.numeric(x$T_FELIRAT$Type)
    ## T_SZIMBOLUM
    if(exists("T_SZIMBOLUM", where = x)) {
    names(x$T_SZIMBOLUM) <- c("Nr", "Key", "Pt.id", "Angle", "Ref.tab", "Ref.tab.line", "Valid")
    x$T_SZIMBOLUM$Angle <- as.numeric(x$T_SZIMBOLUM$Angle)
    }
    x
}

curr.list <- DAT_preproc(curr.list)
