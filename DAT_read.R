DAT_read <- function(file, table.name = "T_PONT*") {
    ## Read source data
    source.data <- readLines(file, encoding = "latin1")
    tables.start <- grep("^T_", source.data)
    tables.name <- source.data[tables.start]
    tables.name.idx <- which(tables.name == table.name)
    table.head <- tables.start[tables.name.idx] + 1
    table.tail <- tables.start[tables.name.idx + 1] - 1
    source.data[table.head: table.tail]
}
