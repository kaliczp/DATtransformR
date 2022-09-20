DAT_write <- function(data, file) {
    connnew.file <- file(file, "w")
    write.table(t(as.data.frame(data[[1]])), connnew.file, sep = "*", eol = "*\r\n", na = "", row.names = FALSE, col.names= FALSE, quote = FALSE)
    table.names <- names(data)
    for(curr.table in 2:length(data)) {
        writeLines(text = paste0(table.names[[curr.table]], "*"), con = connnew.file, sep = "\r\n")
        write.table(data[[curr.table]], connnew.file, sep = "*", eol = "*\r\n", na = "", row.names = FALSE, col.names= FALSE, quote = FALSE, append = TRUE)
    }
    close(connnew.file)
}
