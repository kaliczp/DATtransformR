DATSelect <- function(x, ID = 139, obj = "BD") {
    DATtable.name <- paste0("T_OBJ_ATTR", obj)
    DATtable.row <- which(x[[DATtable.name]][, 4] == ID)
    area.ID <- x[[DATtable.name]][DATtable.row, 3]
    DATtable.row <- which(x[["T_FELULET"]][, 1] == area.ID)
    border.ID <- x[["T_FELULET"]][DATtable.row, 3]
    DATtable.row <- which(x[["T_HATAR"]][, 1] == border.ID)
    DAT.border.lines <- x[["T_HATAR"]][DATtable.row, 3]
    DAT.point.IDs <- c(x[["T_HATARVONAL"]][DAT.border.lines,3],
                       x[["T_HATARVONAL"]][DAT.border.lines,4])
    DAT.point.IDs <- unique(DAT.point.IDs)
    DATtable.rows = numeric()
    for(tti in 1:length(DAT.point.IDs))
        DATtable.rows <- c(DATtable.rows, which(x[["T_PONT"]][, 1] == DAT.point.IDs[tti]))
    x[["T_PONT"]][DATtable.rows,]
}


## DATSelectedExport(curr.list)$T_PONT[,c("x","y")]
selected.list$T_FELIRAT$Text
DATSelect <- DATSelect(selected.list, 140)

const.shift <- rnorm(2, sd = 0.06)


Hmean <- mean(curr.list$T_PONT[curr.list$T_PONT$H > 1, "H"], na.rm = TRUE)
## foldr <- c(139,"207/2",40,42,"104/3",48,184,245,246,91,"54/9","54/8")
## foldr <- c(140,"207/4",39,41,"103/2","49/1",183,"244/2",247,"92/1","54/8","54/9")

foldrok <- sub("/", "_", foldr)
KezdoPontszam <- 2000
for(tti in 1:12) {
    coo.sel <- DATSelect(curr.list, ID = foldr[tti])[,c("x","y")]
    coo.sel[,"x"] <- coo.sel[,"x"] + const.shift[1] + rnorm(nrow(coo.sel), sd = 0.02)
    coo.sel[,"y"] <- coo.sel[,"y"] + const.shift[2] + rnorm(nrow(coo.sel), sd = 0.02)
    coo.sel$H <- rnorm(nrow(coo.sel), Hmean, 0.1)
    coo.sel <- cbind(Name = paste0("GPS", KezdoPontszam + 0:(nrow(coo.sel)-1)), coo.sel)
    coo.ref <- coo.sel[1,]
    coo.ref[,1] <- paste("RTCM-Ref", 4000 + tti)
    coo.ref[,"x"] <- coo.ref[,"x"] + (1000 * rnorm(1))
    coo.ref[,"y"] <- coo.ref[,"y"] + (1000 * rnorm(1))
    coo.ref[,"H"] <- coo.ref[,"H"] + rnorm(1)
    coo.ok <- rbind(coo.ref, coo.sel)
    coo.ok$x <- round(coo.ok$x, 3)
    coo.ok$y <- round(coo.ok$y, 3)
    coo.ok$H <- round(coo.ok$H, 3)
    ## Zero padding?
    write.table(coo.ok, paste0("Meas", foldrok[tti], ".csv"), quot = FALSE, eol = "\r\n", sep = ",", row.names = FALSE, col.names = FALSE)
}

selected.list <- DATSelectedExport(curr.list, c(254,253,252,(255),"257/1","257/2",256,"093/4","(204)"))
DAT_write(selected.list, "Kivag/Nagy.dat")

## transform után
Seholse <- DAT_read("Seholseutf8.dat")
Seholse <- DAT_preproc(Seholse)

foldr <- 115:126

## Ép törlés utcanév nem volt
nevsor <- read.table("nevsor.csv", sep = "\t", head = TRUE)
foldr <- c(192:196,199,201,229:233)
for(ttnev in 1:nrow(nevsor)) {
    StudentFilename <- gsub(" ", "",nevsor[ttnev, "Név"])
    selected.list <- DATSelectedExport(Seholse, as.character(foldr[ttnev]))
    DAT_write(selected.list, paste0("Torl2023/Ori/",StudentFilename,".dat"))
}

szomszed <- c("(093/5)","092/12","(147)",148:159,189:197,"198/1","198/2",199:202,"203/1","203/2","(204)","224/1","224/2",227:236,"237/1","237/2",238:241,"(242)",243)
szomszedselected.list <- DATSelectedExport(Seholse, as.character(szomszed))
DAT_write(szomszedselected.list, paste0("Torl2023/Ori/szomszed.dat"))

## FreeTR megnyitás vagy:
## recode u8..l2/cl Torl2023/Ori/szomszed.dat

### Épület felt
## Telj ter kiválaszt
szomszed <- c(59:74,"75/1","75/2","75/3", # Sor
           "(027/3)", "(027/4)", # Nyugati út és szántó
           "(76)", # utca délről Hársfa
           "77/1", "77/5", "77/7", "78/1", "79/1", "80/4", "80/5", # Első alappontig délről
           "81/1", "82/1", "83/1", "84/1", "85/1", "86/2", "87/1", "88/2", # Második pontig délről
           "89/1", "90/1", 91, # plusz "92/2", "93/2", "94/1", "95/1", # Házak délről
                                        # "(96)", #Közút kelet
           "(026)", # út észak
           "029/29", "029/30", "029/31", "029/32", "029/33", "029/34", "025/3" # Földek észak
           )

szomszedselected.list <- DATSelectedExport(Seholse, as.character(szomszed))
DAT_write(szomszedselected.list, paste0("HazFel2023/Ori/szomszed.dat"))
## recode u8..l2/cl HazFel2023/Ori/szomszed.dat

## Mentés épület törlés FreeTRben
## Átmásolás, hogy legyen meg a weben
## cp szomszedok.dat 01szomszedok.dat
## Visszaolvasás és csak az aktuális
## recode l2/CR-LF..u8 szomszedok.dat
SzomNoHouse <- DAT_read("HazFel2023/OriNohouse/szomszedok.dat")
SzomNoHouse <- DAT_preproc(SzomNoHouse)

foldr <- c(63:74)
for(ttnev in 1:nrow(nevsor)) {
    StudentFilename <- gsub(" ", "",nevsor[ttnev, "Név"])
    selected.list <- DATSelectedExport(SzomNoHouse, as.character(foldr[ttnev]))
    DAT_write(selected.list, paste0("HazFel2023/OriNohouse/",StudentFilename,".dat"))
}


## Összevon
foldr <- c(63:74)
for(ttnev in 1:nrow(nevsor)) {
    StudentFilename <- gsub(" ", "",nevsor[ttnev, "Név"])
    selected.list <- DATSelectedExport(SzomNoHouse, as.character(c(foldr[ttnev],foldr[ttnev]+1)))
    DAT_write(selected.list, paste0("Összevon2023/",StudentFilename,".dat"))
}

## Külter oszt
foldr <- c("050/26","050/27","050/28","050/33","050/24","050/23","050/22","050/21","050/20","050/19","050/18","050/9")
for(ttnev in 1:nrow(nevsor)) {
    StudentFilename <- gsub(" ", "",nevsor[ttnev, "Név"])
    selected.list <- DATSelectedExport(Seholse, foldr[ttnev])
    DAT_write(selected.list, paste0("Küloszt2023/",StudentFilename,".dat"))
}

szomszed <- c("050/32","050/31","050/30", # Észak közúton belül
              "029/8", "029/9", "029/10","029/11", "029/2", "029/3", "029/42", # Észak úton kívül
              "029/4", "029/12", "029/17", "029/18", "029/19", "029/20", "029/21", "029/22", # Észak úton kívül
              foldr,
              "(050/17)", "(070)", "(066)",# K-i utak
              "069/3", "069/2", "069/5", "069/6","069/7", "069/9","050/15", "050/16", # K-i földek
               "069/11", "069/12", "069/13", "069/14",# K-i földek
              "049/1", "050/14", # Nyi földek
              "050/13", "(048)", # Nyi út
              "050/35", "050/7", "050/8", "(074)", "075", # D-i rész
              "050/2", "050/3", "050/4", "050/5", "050/6", # D-i rész
              "(044)" # Észak közút
              )

szomszedselected.list <- DATSelectedExport(Seholse, szomszed)
DAT_write(szomszedselected.list, paste0("Küloszt2023/szomszed.dat"))
## recode u8..l2/cl HazFel2023/Ori/szomszed.dat
