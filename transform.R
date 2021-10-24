DATfiles <- dir(patt = "dat")

currpoints <- DAT_read(DATfiles[2], numeric.table = TRUE)
names(currpoints) <- c("Nr", "x", "y", "H", "errV", "errM")

currpoints$x <- currpoints$x + 10000
currpoints$y <- currpoints$y + 10000
currpoints <- eovrotate(currpoints, -75)

DAT_modify_table(currpoints, "BatéOld.dat", "teszt.dat")

currtext <- DAT_read(DATfiles[2], table.name = "T_FELIRAT*")
names(currtext) <- c("Nr", "Text", "Pt.id", "Angle", "Font", "Valid", "Ref.tab", "Ref.tab.line", "Type")

CurrTextAngle <- as.numeric(currtext$Angle)
CurrTextAngle[CurrTextAngle != 90] <- CurrTextAngle[CurrTextAngle != 90] - 75

CurrTextAngle[CurrTextAngle < 0] <- CurrTextAngle[CurrTextAngle < 0] + 180

currtext$Angle <- round(CurrTextAngle, 1)

DAT_modify_table(currtext, "teszt.dat", "teszt1.dat", table.name = "T_FELIRAT*")
