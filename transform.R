DATfiles <- dir(patt = "dat")

curr.list <- DAT_read(DATfiles[2], numeric.table = FALSE)
names(curr.list$T_PONT) <- c("Nr", "x", "y", "H", "errV", "errM")

curr.list$T_PONT$x <- curr.list$T_PONT$x + 10000
curr.list$T_PONT$y <- curr.list$T_PONT$y + 10000
curr.list$T_PONT <- eovrotate(curr.list$T_PONT, -75)

names(curr.list$T_FELIRAT) <- c("Nr", "Text", "Pt.id", "Angle", "Font", "Valid", "Ref.tab", "Ref.tab.line", "Type")

CurrTextAngle <- as.numeric(curr.list$T_FELIRAT$Angle)
CurrTextAngle[CurrTextAngle != 90] <- CurrTextAngle[CurrTextAngle != 90] - 75

CurrTextAngle[CurrTextAngle < 0] <- CurrTextAngle[CurrTextAngle < 0] + 180

curr.list$T_FELIRAT$Angle <- round(CurrTextAngle, 1)

DAT_write(curr.list, "teszt.dat")
## DAT_modify_table(curr.list, "teszt.dat", "teszt1.dat", table.name = "T_FELIRAT*")
