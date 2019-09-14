


##' @export
inverseRegex.default <- function(...)
    stop('Input class not supported')

##' @export
inverseRegex.logical <- function(x, ...)
    x

##' @export
inverseRegex.integer <- function(x, ...)
    inverseRegex.character(as.character(x), ...)

##' @export
inverseRegex.numeric <- function(x, ...){

    tmp <- vapply(x, format, character(1), nsmall = 1)
    tmp[is.na(x)] <- NA_character_
    
    inverseRegex.character(tmp, ...)
    
}

##' @export
inverseRegex.Date <- function(x, ...)
    inverseRegex.character(as.character(x), ...)

##' @export
inverseRegex.POSIXct <- function(x, ...)
    inverseRegex.character(as.character(x), ...)

##' @export
inverseRegex.factor <- function(x, ...)
    inverseRegex.character(as.character(x), ...)

##' @export
inverseRegex.matrix <- function(x, ...){
    
    tmp <- x
    
    if (mode(tmp) == 'numeric')
        tmp <- trimws(format(tmp))
    
    tmp[is.na(x)] <- NA_character_
    
    out <- matrix(NA_character_, nrow = nrow(x), , ncol = ncol(x), dimnames = dimnames(x))
    
    for (ii in 1:nrow(out))
        out[ii, ] <- inverseRegex(x[ii, ], ...)
    
    out
    
}

##' @export
inverseRegex.data.frame <- function(x, ...){
    
    out <- as.data.frame(matrix(NA_character_, nrow = nrow(x), , ncol = ncol(x),
                                dimnames = dimnames(x)), stringsAsFactors = FALSE)
    
    for (jj in 1:ncol(out) ){
        
        if (inherits(x[[jj]], 'numeric') ){
            
            tmp <- vapply(x[[jj]], format, character(1), nsmall = 1)
            tmp <- trimws(format(tmp))
            tmp[is.na(x[[jj]])] <- NA_character_
            x[[jj]] <- tmp
            
        }
        
        out[[jj]] <- inverseRegex(x[[jj]], ...)
        
    }
    
    out
    
}

##' @export
inverseRegex.tibble <- function(x, ...){
    
    if (!requireNamespace('tibble', quietly = TRUE))
        stop('Package tibble not available. Install or provide input as a data.frame.')
    
    out <- tibble::as_tibble(matrix(NA_character_, nrow = nrow(x), ncol = ncol(x),
                                    dimnames = dimnames(x)), stringsAsFactors = FALSE)
    
    for (jj in 1:ncol(out) ){
        
        if (inherits(x[[jj]], 'numeric'))
            x[[jj]] <- trimws(format(x[[jj]]))
        
        out[[jj]] <- inverseRegex(x[[jj]], ...)
        
    }
    
    out
    
}
