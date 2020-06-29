## pipe

`%|%` <- function(x, f) {
    f(x)
}

## pipe map

`%:%` <- function(x, f) {
    Map(f, x)
}

# pipe reduction

`%>%` <- function(x, f) {
    Reduce(f, x)
}

## pipe map with key-value output

`%::%` <- function(x, f) {
    Map(
        function(x) { list(x, f(x)) },
        x
    )
}

## pipe filter

`%?%` <- function(x, f) {
    Filter(f, x)
}

## pipe assertion

`%!%` <- function(x, f) {
    ifelse(
        f(x),
        x,
        warning(stop(paste("Condition false:", x)))
    )
}
