P <- function(f, args) {
    function(x) {
        passed.args <- names(args)
        all.args <- formalArgs(f)
        first.arg <- all.args[!all.args %in% passed.args][[1]]
        all.args <- args
        all.args[[first.arg]] <- x

        do.call(f, all.args)
    }
}

integral <- function(f, a = -Inf, b = Inf, subdivisions = 1000, rel.tol = .Machine$double.eps^0.5, abs.tol = rel.tol) {
    y <- (integrate(f, a, b, subdivisions = subdivisions, rel.tol = rel.tol, abs.tol = abs.tol))
    v <- y$value
    attr(v, "integration") <- y

    return(v)
}

E <- function(p, a = -Inf, b = Inf, subdivisions = 1000, rel.tol = .Machine$double.eps^0.5, abs.tol = rel.tol) {
    y <- (integrate(function(x) { x * p(x) }, a, b, subdivisions = subdivisions, rel.tol = rel.tol, abs.tol = abs.tol))
    v <- y$value
    attr(v, "integration") <- y

    return(v)
}

Var <- function(p, a = -Inf, b = Inf, subdivisions = 10000, rel.tol = .Machine$double.eps^0.5, abs.tol = rel.tol) {
    mu <- (integrate(function(x) { x * p(x) }, a, b, subdivisions = subdivisions, rel.tol = rel.tol, abs.tol = abs.tol))$value
    y <- (integrate(function(x) { (x^2 -2 * x * mu + mu^2) * p(x) }, a, b, subdivisions = subdivisions, rel.tol = rel.tol, abs.tol = abs.tol))
    v <- y$value
    attr(v, "integration") <- y

    return(v)
}
