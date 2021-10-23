DATfiles <- dir(patt = "dat")

currpoints <- DAT_read(DATfiles[2], numeric.table = TRUE)
names(currpoints) <- c("Nr", "x", "y", "H", "errV", "errM")

currpoints$x <- currpoints$x + 10000
currpoints$y <- currpoints$y + 10000
currpoints <- eovrotate(currpoints, -75)

DAT_modify_table(currpoints, "BatéOld.dat", "teszt.dat")
