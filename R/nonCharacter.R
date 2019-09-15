


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
    tmp[is.na(x) & !is.nan(x)] <- NA_character_
    
    out <- inverseRegex.character(tmp, ...)
    
    out[is.nan(x)] <- 'NaN'
    out[is.infinite(x)] <- as.character(x[is.infinite(x)])
    
    out
    
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
    
    out <- matrix(NA_character_, nrow = nrow(x), ncol = ncol(x), dimnames = dimnames(x))
    
    for (ii in 1:nrow(out))
        out[ii, ] <- inverseRegex(tmp[ii, ], ...)
    
    out[is.nan(x)] <- 'NaN'
    out[is.infinite(x)] <- as.character(x[is.infinite(x)])
    
    out
    
}

##' @export
inverseRegex.data.frame <- function(x, ...){
    
    out <- as.data.frame(matrix(NA_character_, nrow = nrow(x), ncol = ncol(x),
                                dimnames = dimnames(x)), stringsAsFactors = FALSE)
    
    for (jj in 1:ncol(out) ){
        
        if (inherits(x[[jj]], 'numeric') ){
            
            tmp <- trimws(format(x[[jj]]))
            tmp[is.na(x[[jj]]) & !is.nan(x[[jj]])] <- NA_character_
            
            tmp <- inverseRegex(tmp, ...)
            
            tmp[is.nan(x[[jj]])] <- 'NaN'
            tmp[is.infinite(x[[jj]])] <- as.character(x[[jj]][is.infinite(x[[jj]])])
            
            out[[jj]] <- tmp
            
        } else {
            
            out[[jj]] <- inverseRegex(x[[jj]], ...)
            
        }
        
    }
    
    out
    
}

##' @export
inverseRegex.tbl_df <- function(x, ...){
    
    if (!requireNamespace('tibble', quietly = TRUE))
        stop('Package tibble not available. Install or provide input as a data.frame.')
    
    out <- tibble::as_tibble(matrix(NA_character_, nrow = nrow(x), ncol = ncol(x),
                                    dimnames = dimnames(x)), stringsAsFactors = FALSE)
    
    for (jj in 1:ncol(out) ){
        
        if (inherits(x[[jj]], 'numeric') ){
            
            tmp <- trimws(format(x[[jj]]))
            tmp[is.na(x[[jj]]) & !is.nan(x[[jj]])] <- NA_character_
            
            tmp <- inverseRegex(tmp, ...)
            
            tmp[is.nan(x[[jj]])] <- 'NaN'
            tmp[is.infinite(x[[jj]])] <- as.character(x[[jj]][is.infinite(x[[jj]])])
            
            out[[jj]] <- tmp
            
        } else {
            
            out[[jj]] <- inverseRegex(x[[jj]], ...)
            
        }
        
    }
    
    out
    
}
