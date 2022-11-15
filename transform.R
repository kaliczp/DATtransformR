## cp ../original.dat originalutf8.dat
## recode l2/CR-LF..u8 originalutf8.dat
DATfiles <- dir(patt = "dat")

curr.list <- DAT_read(DATfiles[3])
names(curr.list$T_PONT) <- c("Nr", "x", "y", "H", "errV", "errM")

xshift <- 220000 - max(curr.list$T_PONT$x)
yshift  <- 840000- max(curr.list$T_PONT$y)

curr.list$T_PONT$x <- curr.list$T_PONT$x + xshift
curr.list$T_PONT$y <- curr.list$T_PONT$y + yshift
curr.list$T_PONT <- round(eovrotate(curr.list$T_PONT, -73), 2)

names(curr.list$T_FELIRAT) <- c("Nr", "Text", "Pt.id", "Angle", "Font", "Valid", "Ref.tab", "Ref.tab.line", "Type")

CurrTextAngle <- as.numeric(curr.list$T_FELIRAT$Angle)
CurrTextAngle[CurrTextAngle != 90] <- CurrTextAngle[CurrTextAngle != 90] - 73

CurrTextAngle[CurrTextAngle < 0] <- CurrTextAngle[CurrTextAngle < 0] + 180

curr.list$T_FELIRAT$Angle <- round(CurrTextAngle, 1)
curr.list$T_FELIRAT$Nr <- as.numeric(curr.list$T_FELIRAT$Nr)
curr.list$T_FELIRAT$Pt.id <- as.numeric(curr.list$T_FELIRAT$Pt.id)
curr.list$T_FELIRAT$Ref.tab.line <- as.numeric(curr.list$T_FELIRAT$Ref.tab.line)
curr.list$T_FELIRAT$Type <- as.numeric(curr.list$T_FELIRAT$Type)

names(curr.list$T_SZIMBOLUM) <- c("Nr", "Key", "Pt.id", "Angle", "Ref.tab", "Ref.tab.line", "Valid")
CurrTextAngle <- as.numeric(curr.list$T_SZIMBOLUM$Angle)
CurrTextAngle[CurrTextAngle != 90] <- CurrTextAngle[CurrTextAngle != 90] - 73
CurrTextAngle[CurrTextAngle < 0] <- CurrTextAngle[CurrTextAngle < 0] + 180
curr.list$T_SZIMBOLUM$Angle <- round(CurrTextAngle, 1)


## Public area parcel ID in parentheses
ttaktline <- curr.list$T_FELIRAT$Ref.tab == "T_OBJ_ATTRBC" & curr.list$T_FELIRAT$Type == 11
curr.list$T_FELIRAT[ttaktline, "Text"] <- paste0("(",curr.list$T_FELIRAT[ttaktline, "Text"], ")")


outfilename <- "teszt"
settlement <- "Seholsincs"
teacher <- "Kalicz Péter"
student <- "Nagyméretarányú 2"
act.date <- Sys.Date()
curr.list$Head <- init <- paste(outfilename, # filename without extension
                                paste0(settlement,"_jogerős"), # some title
                                "Soproni Egyetem", # source organisation
                                teacher, # resp. person
                                "-", # target organisation
                                student,
                                format(act.date, "%Y%m%d"), # Actual date
                                "", # software version
                                "20211108 1.1", # file version
                                sep = "*"
                                )

DAT_write(curr.list, paste0(outfilename, ".dat"))
## DAT_modify_table(curr.list, "teszt.dat", "teszt1.dat", table.name = "T_FELIRAT*")

## recode u8..l2/cl teszt.dat
## patch teszt.dat jav.patch
## patch teszt.dat betű.patch
## from Kapos to Kapus
