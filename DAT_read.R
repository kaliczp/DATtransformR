DAT_read <- function(file, table.name = "T_PONT*") {
    ## Read source data
    source.data <- readLines(file, encoding = "latin1")
    ## Search indices of table titles
    tables.start <- grep("^T_", source.data)
    ## Vector of table titles
    tables.name <- source.data[tables.start]
    ## Title index of the selected table
    tables.name.idx <- which(tables.name == table.name)
    ## Table begins after the title of the selected table
    table.head <- tables.start[tables.name.idx] + 1
    ## Table ends before the next title or at the last row of the file
    next.name.idx <- tables.name.idx + 1
    if(next.name.idx <= length(tables.start)) {
        table.tail <- tables.start[next.name.idx] - 1
    } else {
        table.tail <- length(source.data)
    }
    source.data[table.head: table.tail]
}
