##' Reverse Engineers a Regular Expression Pattern to Represent the Input Object.
##' 
##' Deconstructs the input into collections of letters, digits, punctuation, and
##' spaces that represent a regex pattern consistent with that input.
##' 
##' @param x Object to derive a regex pattern for.
##' @param numbersToKeep Set of numbers giving the length for which elements
##' repeated that many times should be counted explicitly
##' (e.g. "[[:digit:]]\{5\}"). Repeat sequences not included in numbersToKeep
##' will be coded with a "+" (e.g. "[[:digit:]]+"). Defaults to c(2, 3, 4, 5, 10).
##' Set to NULL to have all runs coded as "+" and set to \code{2:maxCharacters}
##' to have the length specified for all repeated values. If one is included then
##' all unique patterns with be counted as "\{1\}"; if it is not then the "\{1\}"
##' is left off.
##' @param combineCases Logical indicating whether to treat upper and lower case
##' letters as a single entity. Defaults to FALSE.
##' @param combineAlphanumeric Logical indicating whether to treat alphabetic
##' characters and digits as a single entity. Defaults to FALSE.
##' @param combinePunctuation Logical indicating whether to treat all punctuation
##' characters as a single entity. Defaults to FALSE.
##' @param combineSpace Logical indicating whether to treat all space characters
##' as a single entity. Defaults to FALSE.
##' @param sep Value used to separate the regex patterns. Defaults to an empty
##' string.
##' @param escapePunctuation Logical indicating whether to escape any punctuation
##' characters. Defaults to FALSE. Set to TRUE if you want to use the returned
##' value as an argument to grep.
##' @param enclose Logical indicating whether to surround each returned value
##' with \code{'^'} and \code{'$'}. Defaults to FALSE.
##' 
##' @return A set of regex patterns that match the input data. These patterns will
##' either be character vectors or the same class as the input object if it was
##' a matrix, data frame, or tibble.
##' 
##' @details
##' 
##' The fundamental use of inverseRegex applies only to strings. Inputs of a class
##' other than character are treated as follows: 
##' \itemize{
##'     \item Integer: Converted using \code{as.character()}.
##'     \item Factor: Converted using \code{as.character()}.
##'     \item Logical: Left as is. Converting to character would not provide any
##'           simplification.
##'     \item Numeric: Converted to character by applying \code{format(..., nsmall = 1)}
##'           element by element. NA values will be returned as NA_character_, whilst
##'           \code{NaN}, \code{Inf}, and \code{-Inf} will be returned as literal
##'           strings: \code{"NaN"}, \code{"Inf"}, and \code{"-Inf"}.
##'     \item Date: Converted using \code{as.character()}.
##'     \item POSIXct: Converted using \code{as.character()}.
##'     \item Data frame: Each column is assessed individually and the results
##'           combined together so that the output is a data frame of regex patterns
##'           with the same dimensions as the input. The columns of class other than
##'           character will each be converted as described previously, with one
##'           exception: Unlike above where numerics are passed to
##'           \code{format(..., nsmall = 1)} element by element, here the entire
##'           column is passed to \code{trimws(format(...))}. This will lead to a
##'           common number of digits to the right of the decimal point and a variable
##'           number of digits with no padding on the left side.
##'     \item Matrix: Creates a matrix of regex patterns with the same dimensions
##'           as the input. If the matrix has a mode of numeric then it will first
##'           be passed to \code{trimws(format(...))}.
##'     \item Tibble: Same as a data frame except the returned object is a tibble.
##'           Requires the tibble package to be installed.
##'     \item Anything else: Not supported; an error will be thrown.
##' }
##' 
##' If these conversion methods are not appropriate then you can do the conversion
##' yourself so that the input is dispatched directly to inverseRegex.character.
##' 
##' The regex patterns are identified using the constructs "[:digit:]", "[:upper:]",
##' "[:lower:]", "[:alpha:]", "[:alnum:]", "[:punct:]", and "[:space:]" as described
##' in \code{?regex}. This will allow for non-ASCII characters to be identified, to
##' the extent supported by \code{grep}. Any characters not identified by these
##' search patterns will be left as is. Note that for characters from unicameral
##' alphabets the combineCases argument will need to be set to TRUE otherwise they
##' will not be detected by "lower" and "upper".
##' 
##' NA values in the input will remain as NA values in the output.
##' 
##' @examples
##' inverseRegex('Hello World!')
##' 
##' table(inverseRegex(c(rep('HELLO', 10), 'HELL0')))
##' 
##' unique(inverseRegex(iris, numbersToKeep = 1:10))
##'
##' inverseRegex(c(1, NA, 3.45, NaN, Inf, -Inf))
##' 
##' @seealso occurrencesLessThan, regex
##' 
##' @author Jasper Watson
##' 
##' @export
##' 
##
inverseRegex <- function(x,
                         numbersToKeep = c(2, 3, 4, 5, 10),
                         combineCases = FALSE,
                         combineAlphanumeric = FALSE,
                         combinePunctuation = FALSE,
                         combineSpace = FALSE,
                         sep = '',
                         escapePunctuation = FALSE,
                         enclose = FALSE
                         ){
    
    UseMethod('inverseRegex')
    
}

##' @export
inverseRegex.character <- function(x,
                                   numbersToKeep = c(2, 3, 4, 5, 10),
                                   combineCases = FALSE,
                                   combineAlphanumeric = FALSE,
                                   combinePunctuation = FALSE,
                                   combineSpace = FALSE,
                                   sep = '',
                                   escapePunctuation = FALSE,
                                   enclose = FALSE
                                   ){
    
    out <- rep(NA_character_, length(x))
    
    ## strsplit is vectorised so call it outside of the loop.
    splits <- strsplit(x, '')   
    
    for (ii in seq_along(x) ){
        
        if (is.na(x[ii]))
            next
        
        chars <- splits[[ii]]
        
        digit <- grepl('[[:digit:]]', chars)
        lower <- grepl('[[:lower:]]', chars)
        upper <- grepl('[[:upper:]]', chars)
        alpha <- grepl('[[:alpha:]]', chars)
        alnum <- grepl('[[:alnum:]]', chars)
        space <- grepl('[[:space:]]', chars)
        punct <- grepl('[[:punct:]]', chars)
        
        if (!combinePunctuation && escapePunctuation){
            
            ## chars are all length one so we can just grepl and paste. No need
            ## for complicated replace-all-occurrences regexing.
            use <- grepl('[[:punct:]]', chars)
            if (any(use))
                chars[use] <- paste0('\\', chars[use])
            
        }
        
        if (combineAlphanumeric){
            
            chars[alnum] <- '[[:alnum:]]'
            
        } else {
            
            if (combineCases){
                
                chars[digit] <- '[[:digit:]]'
                chars[alpha] <- '[[:alpha:]]'
                
            } else {
                
                chars[digit] <- '[[:digit:]]'
                chars[lower] <- '[[:lower:]]'
                chars[upper] <- '[[:upper:]]'
                
            }
            
        }
        
        if (combineSpace)
            chars[space] <- '[[:space:]]'
        
        if (combinePunctuation)
            chars[punct] <- '[[:punct:]]'
        
        rr <- rle(chars)

        tmp <- paste0('{', rr$lengths, '}')
        
        rr$lengths <- ifelse(rr$lengths %in% numbersToKeep, tmp,
                      ifelse(rr$lengths == 1, '', '+')
                      )

        if (enclose){
            result <- paste0('^', paste(paste0(rr$values, rr$lengths), collapse = sep), '$')
        } else {
            result <- paste(paste0(rr$values, rr$lengths), collapse = sep)
        }
        
        out[ii] <- result
        
    }
    
    out
    
}
