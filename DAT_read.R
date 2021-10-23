DAT_read <- function(file, table.name = "T_PONT*", numeric.table = FALSE) {
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
    ## Substract table
    raw.table <- source.data[table.head: table.tail]
    ## Split information at * separator
    raw.table.atomic <- unlist(strsplit(raw.table, "\\*"))
    ## Numeric table is converted from character in one step if flag activated
    if(numeric.table) {
        raw.table.atomic <- as.numeric(raw.table.atomic)
    }
    ## Build data.frame from splitted data
    table.rows <- length(raw.table)
    as.data.frame(matrix(raw.table.atomic, nrow = table.rows, byrow=TRUE))
}
