

context('Test inverseRegex')


test_that('Various characters all work', {
    
    expect_equal(inverseRegex('a'), '[[:lower:]]')
    expect_equal(inverseRegex('A'), '[[:upper:]]')
    expect_equal(inverseRegex('1'), '[[:digit:]]')
    expect_equal(inverseRegex('aA1'), '[[:lower:]][[:upper:]][[:digit:]]')
    expect_equal(inverseRegex('~!@#$%^&*()_+}{|":?><,./;[]-='),
                 '~!@#$%^&*()_+}{|":?><,./;[]-=')
    expect_equal(inverseRegex('\\'), '\\')
    expect_equal(inverseRegex(' \t\n\r'), ' \t\n\r')
    
    expect_equal(inverseRegex('a', enclose = TRUE), '^[[:lower:]]$')
    
})

test_that('The combineCases argument works', {
    
    expect_equal(inverseRegex('A', combineCases = TRUE), '[[:alpha:]]')
    expect_equal(inverseRegex('a', combineCases = TRUE), '[[:alpha:]]')
    expect_equal(inverseRegex('aA', combineCases = TRUE), '[[:alpha:]]{2}')
    
})

test_that('The combineAlphanumeric argument works', {
    
    expect_equal(inverseRegex('A', combineAlphanumeric = TRUE), '[[:alnum:]]')
    expect_equal(inverseRegex('1', combineAlphanumeric = TRUE), '[[:alnum:]]')
    expect_equal(inverseRegex('a1', combineAlphanumeric = TRUE), '[[:alnum:]]{2}')
    
})

test_that('The combinePunctuation argument works', {
    
    expect_equal(inverseRegex('~!@#$%^&*()_+}{|":?><,./;[]-=',
                              combinePunctuation = TRUE, numbersToKeep = 29),
                 '[[:punct:]]{29}')
    expect_equal(inverseRegex('\\', combinePunctuation = TRUE), '[[:punct:]]') 
    
})

test_that('The combineSpace argument works', {
    
    expect_equal(inverseRegex(' \t\n\r', combineSpace = TRUE), '[[:space:]]{4}')   
    
})

test_that('The numbersToKeep argument works', {
    
    expect_equal(inverseRegex('1', numbersToKeep = NULL), '[[:digit:]]')   
    expect_equal(inverseRegex('1', numbersToKeep = 1), '[[:digit:]]{1}')   
    expect_equal(inverseRegex('12', numbersToKeep = 2), '[[:digit:]]{2}')   
    expect_equal(inverseRegex('12', numbersToKeep = 1), '[[:digit:]]+')   
    expect_equal(inverseRegex('12345', numbersToKeep = 1:5), '[[:digit:]]{5}')   
    expect_equal(inverseRegex('12345', numbersToKeep = 10:100), '[[:digit:]]+')     
    
})

test_that('The escapePunctuation argument works', {
    
    expect_equal(inverseRegex('~!@#$%^&*()_+}{|":?><,./;[]-='),
                 '~!@#$%^&*()_+}{|":?><,./;[]-=')
    expect_equal(inverseRegex('\\'), '\\')
    
})

test_that('We get an error for a non-supported input class', {
    
    x <- 1:10
    class(x) <- 'Foo'
    
    expect_error(inverseRegex(x))
    
})

test_that('Inputs other than type character work as expected', {
    
    expect_equal(inverseRegex(1:10), inverseRegex(as.character(1:10)))
    
    expect_equal(inverseRegex(seq(0, 1, length.out = 10)),
                 inverseRegex(vapply(seq(0, 1, length.out = 10), format,
                                     character(1), nsmall = 1))
                 )
    
    expect_equal(inverseRegex(Sys.Date()), inverseRegex(as.character(Sys.Date())))
    
    expect_equal(inverseRegex(Sys.time()), inverseRegex(as.character(Sys.time())))
    
    expect_equal(inverseRegex(as.factor(letters)), inverseRegex(letters))
    
    expect_equal(inverseRegex(matrix(1:4, nrow = 2)),
                 matrix('[[:digit:]]', nrow = 2, ncol = 2)
                 )
    expect_equal(inverseRegex(matrix(letters[1:4], nrow = 2)),
                 matrix('[[:lower:]]', nrow = 2, ncol = 2)
                 )
    
    expect_equal(inverseRegex(data.frame(a = 1:26, b = LETTERS,
                                         stringsAsFactors = FALSE)),
                 data.frame(a = c(rep('[[:digit:]]', 9), rep('[[:digit:]]{2}', 17)),
                            b = rep('[[:upper:]]', 26),
                            stringsAsFactors = FALSE),
                 check.attributes = FALSE
                 )

    if (requireNamespace('tibble', quietly = TRUE) ){
        
        expect_equal(inverseRegex(tibble::tibble(a = 1:26, b = LETTERS,
                                                 stringsAsFactors = FALSE)),
                     tibble::tibble(a = c(rep('[[:digit:]]', 9), rep('[[:digit:]]{2}', 17)),
                                    b = rep('[[:upper:]]', 26),
                                    stringsAsFactors = FALSE),
                     check.attributes = FALSE
                     )
        
    }
    
})

test_that('The escapePunctuation argument works as intended', {
    
    expect_equal(inverseRegex('!', escapePunctuation = TRUE), '\\!')
    
})

test_that('NA values are passed through correctly', {
    
    expect_equal(inverseRegex(NA_character_), NA_character_)
    
    x <- c(TRUE, FALSE, NA)
    expect_equal(inverseRegex(x), x)
    
    expect_equal(inverseRegex(c(1L, NA)), c('[[:digit:]]', NA_character_))

    expect_equal(inverseRegex(c(1, NA)), c('[[:digit:]].[[:digit:]]', NA_character_))
    
    expect_equal(inverseRegex(c(Sys.Date(), NA)),
                 c('[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}', NA_character_))
    
    expect_equal(inverseRegex(c(Sys.time(), NA))[2], NA_character_)

    ## Can't test factor as concatenating a factor with an NA will convert it to an
    ## integer or character.
    
    x <- matrix(c(1:3, NA), nrow = 2)
    expect_equal(inverseRegex(x), matrix(c(rep('[[:digit:]]', 3), NA_character_), nrow = 2))
    
    x <- data.frame(a = c(1.1, NA), b = c('a', NA), stringsAsFactors = FALSE)
    expect_equal(inverseRegex(x),
                 data.frame(a = c('[[:digit:]].[[:digit:]]', NA_character_),
                            b = c('[[:lower:]]', NA_character_),
                            stringsAsFactors = FALSE),
                 check.attributes = FALSE
                 )
    
    if (requireNamespace('tibble', quietly = TRUE) ){
        
        x <- tibble::tibble(a = c(1.1, NA), b = c('a', NA), stringsAsFactors = FALSE)
        expect_equal(inverseRegex(x),
                     tibble::tibble(a = c('[[:digit:]].[[:digit:]]', NA_character_),
                                    b = c('[[:lower:]]', NA_character_),
                                    stringsAsFactors = FALSE),
                     check.attributes = FALSE
                     )
        expect_true(inherits(inverseRegex(tibble::tibble(a = 1:10)), 'tbl_df'))
        
    }
    
})

test_that('The special numeric characters work as expected', {

    x <- c(1, NA, 3.45, NaN, Inf, -Inf)
    out <- c('[[:digit:]].[[:digit:]]', NA, '[[:digit:]].[[:digit:]]{2}', 'NaN', 'Inf', '-Inf')
    
    expect_equal(inverseRegex(x), out)

    out <- c('[[:digit:]].[[:digit:]]{2}', NA, '[[:digit:]].[[:digit:]]{2}', 'NaN',
             'Inf', '-Inf')
    
    expect_equal(inverseRegex(as.matrix(x, nrow = 2)), as.matrix(out, nrow = 2))
    expect_equal(inverseRegex(data.frame(a = x)), data.frame(a = out, stringsAsFactors = FALSE),
                 check.attributes = FALSE)
    
})
