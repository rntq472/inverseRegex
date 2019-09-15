##' Identifies Infrequent inverseRegex Patterns in an R Object.
##' 
##' Calls \code{inverseRegex} on the input object and identifies values that
##' occur infrequently.
##' 
##' @param x Object to analyse for infrequent regex patterns.
##' @param fraction Fraction of the R object size; regex patterns that occur less
##' (or equal) often than this will be identified. For a vector this fraction will
##' be multiplied by the length of the object; for a matrix it will be multiplied by
##' the total number of entries; and for a data frame or tibble it will be multiplied
##' by the number of rows. Defaults to \code{0.05}.
##' @param n Alternative to the \code{fraction} argument which allows a literal
##' number of occurrences to be searched for. Defaults to NULL, in which case
##' \code{fraction} will be used.
##' @param ... Other arguments to be passed to \code{inverseRegex}.
##' 
##' @return A collection of logical values with \code{TRUE} indicating entries with
##' an infrequent regex pattern. The class of the return value will depend on the
##' input object; matrices, data frames, and tibbles will be returned in kind; all
##' others are returned as vectors.
##' 
##' @details This function is essentially a wrapper around calling \code{table()} on
##' the return value of \code{inverseRegex}. It can be used to identify the indices
##' of values that consist of a regex pattern different to others in the R object.
##' 
##' @note NA values are not considered and will need to be identified separately.
##' 
##' @examples
##' occurrencesLessThan(c(LETTERS, 1))
##' 
##' x <- iris
##' x$Species <- as.character(x$Species)
##' x[27, 'Species'] <- 'set0sa'
##' apply(occurrencesLessThan(x), 2, which)
##' 
##' @seealso inverseRegex, regex
##' 
##' @author Jasper Watson
##' 
##' @export
##' 
##
occurrencesLessThan <- function(x,
                                fraction = 0.05,
                                n = NULL,
                                ...
                                ){
    
    UseMethod('occurrencesLessThan')
    
}

##' @export
occurrencesLessThan.default <- function(...)
    stop('Input class not supported')

vectorOption <- function(x, fraction = 0.05, n = NULL, ...){
    
    out <- rep(FALSE, length(x))

    if (is.null(n))
        n <- fraction * length(x)
    
    y <- inverseRegex(x, ...)
    
    tab <- table(y)
    
    tmp <- y %in% names(tab)[tab <= n]
    
    if (length(tmp) > 0)
        out <- tmp
    
    out
    
}
##' @export
occurrencesLessThan.character <- function(x, fraction = 0.05, n = NULL,  ...)
    vectorOption(x, fraction, n, ...)

##' @export
occurrencesLessThan.logical <- function(x, fraction = 0.05, n = NULL, ...)
    vectorOption(x, fraction, n, ...)

##' @export
occurrencesLessThan.integer <- function(x, fraction = 0.05, n = NULL, ...)
    vectorOption(x, fraction, n, ...)

##' @export
occurrencesLessThan.numeric <- function(x, fraction = 0.05, n = NULL, ...)
    vectorOption(x, fraction, n, ...)

##' @export
occurrencesLessThan.Date <- function(x, fraction = 0.05, n = NULL, ...)
    vectorOption(x, fraction, n, ...)

##' @export
occurrencesLessThan.POSIXct <- function(x, fraction = 0.05, n = NULL, ...)
    vectorOption(x, fraction, n, ...)

##' @export
occurrencesLessThan.factor <- function(x, fraction = 0.05, n = NULL, ...)
    vectorOption(x, fraction, n, ...)

##' @export
occurrencesLessThan.matrix <- function(x, fraction = 0.05, n = NULL, ...){
    
    out <- matrix(FALSE, nrow = nrow(x), ncol = ncol(x), dimnames = dimnames(x))
    
    if (is.null(n))
        n <- nrow(x) * ncol(x) * fraction
     
    y <- inverseRegex(as.vector(x), ...)
    
    tab <- table(y)
    
    tmp <- y %in% names(tab)[tab <= n]
    
    if (length(tmp) > 0)
        out[] <- tmp
        
    out
    
}

##' @export
occurrencesLessThan.data.frame <- function(x, fraction = 0.05, n = NULL, ...){

    if (is.null(n))
        n <- nrow(x) * fraction
    
    out <- as.data.frame(matrix(FALSE, nrow = nrow(x), ncol = ncol(x),
                                dimnames = dimnames(x)),
                         stringsAsFactors = FALSE)
    
    for (jj in 1:ncol(x) ){
        
        y <- as.vector(unlist(inverseRegex(x[, jj, drop = FALSE], ...)))
        
        tab <- table(y)
        
        tmp <- y %in% names(tab)[tab <= n]
        
        if (length(tmp) > 0)
            out[, jj] <- tmp
        
    }
    
    out    
    
}

##' @export
occurrencesLessThan.tbl_df <- function(x, fraction = 0.05, n = NULL, ...){

    if (!requireNamespace('tibble', quietly = TRUE))
        stop('Package tibble not available. Install or provide input as a data.frame.')

    if (is.null(n))
        n <- nrow(x) * fraction
    
    out <- tibble::as_tibble(matrix(FALSE, nrow = nrow(x), ncol = ncol(x),
                                    dimnames = dimnames(x)),
                             stringsAsFactors = FALSE)
    
    for (jj in 1:ncol(x) ){
        
        y <- as.vector(unlist(inverseRegex(x[, jj, drop = FALSE], ...)))
        
        tab <- table(y)
        
        tmp <- y %in% names(tab)[tab <= n]
        
        if (length(tmp) > 0)
            out[, jj] <- tmp
        
    }
    
    out    
    
}
