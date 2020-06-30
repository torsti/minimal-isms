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

integral <- function(f, a = -Inf, b = Inf, subdivisions = 1000, rel.tol = .Machine$double.eps^0.5, abs.tol = rel.tol) {
    function(f) { integrate(f, a, b, subdivisions = subdivisions, rel.tol = rel.tol, abs.tol = abs.tol) }
}
