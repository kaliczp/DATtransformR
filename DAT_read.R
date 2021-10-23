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
    ## Table ends before the next title
    table.tail <- tables.start[tables.name.idx + 1] - 1
    source.data[table.head: table.tail]
}
