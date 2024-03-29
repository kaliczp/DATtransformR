DAT_read <- function(file) {
    ## Read source data
    connsource.file <- file(file, "r")
    source.data <- readLines(connsource.file)
    close(connsource.file)
    ## Search indices of table titles
    tables.start <- grep("^T_", source.data)
    ## Vector of table titles
    tables.name <- source.data[tables.start]
    ## Empty list creation
    database.list <- list()
    ## Read header
    database.list[[1]] <- unlist(strsplit(source.data[1], "\\*"))
    for(tables.name.idx in 1:length(tables.start)) {
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
        ## Remove space after last asterix
        raw.table <- sub(" $","", raw.table)
        ## Split information at * separator
        raw.table.atomic <- unlist(strsplit(raw.table, "\\*"))
        ## Numeric table is converted from character in one step if flag activated
        curr.name <- tables.name[tables.name.idx]
        if(tables.name.idx < 6) {
            if(curr.name == "T_PONT*" || curr.name == "T_VONAL*" || curr.name == "T_HATARVONAL*") {
                raw.table.atomic <- as.numeric(raw.table.atomic)
            }
            if(curr.name == "T_HATAR*" || curr.name == "T_FELULET*") {
                around.fac.ind <- seq(4, length(raw.table.atomic), by = 4)
                raw.around.fac <- raw.table.atomic[around.fac.ind]
                raw.table.atomic <- as.numeric(raw.table.atomic[-around.fac.ind])
            }
        }
        ## Build data.frame from splitted data
        table.rows <- length(raw.table)
        database.list[[tables.name.idx + 1]] <- as.data.frame(matrix(raw.table.atomic, nrow = table.rows, byrow=TRUE))
        if(curr.name == "T_HATAR*" || curr.name == "T_FELULET*") {
            database.list[[tables.name.idx + 1]] <- cbind(database.list[[tables.name.idx + 1]],
                                                          as.factor(raw.around.fac)
                                                          )
        }
    }
    ## Remove closing asterix from names
    tables.name <- sub("\\*", "", tables.name)
    names(database.list) <- c("Head", tables.name)
    database.list
}
