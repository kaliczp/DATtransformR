DATfiles <- dir(patt = "dat")

curr.list <- DAT_read(DATfiles[2])
names(curr.list$T_PONT) <- c("Nr", "x", "y", "H", "errV", "errM")

xshift <- 200000 - max(curr.list$T_PONT$x)
yshift  <- 860000- max(curr.list$T_PONT$y)

curr.list$T_PONT$x <- curr.list$T_PONT$x + xshift
curr.list$T_PONT$y <- curr.list$T_PONT$y + yshift
curr.list$T_PONT <- round(eovrotate(curr.list$T_PONT, -75), 2)

names(curr.list$T_FELIRAT) <- c("Nr", "Text", "Pt.id", "Angle", "Font", "Valid", "Ref.tab", "Ref.tab.line", "Type")

CurrTextAngle <- as.numeric(curr.list$T_FELIRAT$Angle)
CurrTextAngle[CurrTextAngle != 90] <- CurrTextAngle[CurrTextAngle != 90] - 75

CurrTextAngle[CurrTextAngle < 0] <- CurrTextAngle[CurrTextAngle < 0] + 180

curr.list$T_FELIRAT$Angle <- round(CurrTextAngle, 1)

outfilename <- "teszt"
settlement <- "Nekeresd"
teacher <- "Kalicz Péter"
student <- "Nagyméretarányú 1"
act.date <- Sys.Date()
curr.list$Head <- init <- paste(outfilename, # filename without extension
                                paste0(settlement,"_jogerős"), # some title
                                "Soproni Egyetem", # source organisation
                                teacher, # resp. person
                                "-", # target organisation
                                student,
                                format(act.date, "%Y%m%d"), # Actual date
                                "", # software version
                                "20211108 1.0", # file version
                                sep = "*"
                                )


DAT_write(curr.list, paste0(outfilename, ".dat"))
## DAT_modify_table(curr.list, "teszt.dat", "teszt1.dat", table.name = "T_FELIRAT*")
