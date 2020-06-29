P <- function(f, args) {
    function(x) {
        passed.args <- names(args)
        all.args <- formalArgs(f)

        first.arg <- all.args[!all.args %in% passed.args][[1]]

        all.args <- args

        all.args[first.arg] <- x

        do.call(f, all.args)
    }
}
