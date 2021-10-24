DAT_modify_table <- function(data, file, new.file, table.name = "T_PONT*") {
    connsource.file <- file(file, "r")
    connnew.file <- file(new.file, "w")
    ## Read source data
    source.data <- readLines(connsource.file, encoding = "latin1")
    ## Search indices of table titles
    tables.start <- grep("^T_", source.data)
    ## Vector of table titles
    tables.name <- source.data[tables.start]
    ## Title index of the selected table
    tables.name.idx <- which(tables.name == table.name)
    ## Table begins after the title of the selected table
    table.head <- tables.start[tables.name.idx]
    ## Table ends before the next title or at the last row of the file
    next.name.idx <- tables.name.idx + 1
    if(next.name.idx <= length(tables.start)) {
        table.tail <- tables.start[next.name.idx]
    } else {
        table.tail <- length(source.data)
    }
    writeLines(source.data[1:table.head], con = connnew.file, sep = "\r\n")
    write.table(data, connnew.file, sep = "*", eol = "*\r\n", na = "", row.names = FALSE, col.names= FALSE, quote = FALSE, append = TRUE)
    writeLines(source.data[table.tail:length(source.data)], con = connnew.file, sep = "\r\n")
    close(connsource.file)
    close(connnew.file)
}
