DAT_read <- function(file, table.name = "T_PONT*") {
    ## Read source data
    source.data <- readLines(file, encoding = "latin1")
    table.head <- which(source.data == table.name)
    table.head
}
