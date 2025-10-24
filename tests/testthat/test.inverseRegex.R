

context("Test inverseRegex")


test_that("Various characters all work", {

    expect_identical(inverseRegex("a"), "[[:lower:]]")
    expect_identical(inverseRegex("A"), "[[:upper:]]")
    expect_identical(inverseRegex("1"), "[[:digit:]]")
    expect_identical(inverseRegex("aA1"), "[[:lower:]][[:upper:]][[:digit:]]")
    expect_identical(inverseRegex('~!@#$%^&*()_+}{|":?><,./;[]-='),
                     '~!@#$%^&*()_+}{|":?><,./;[]-=')
    expect_identical(inverseRegex("\\"), "\\")
    expect_identical(inverseRegex(" \t\n\r"), " \t\n\r")

    expect_identical(inverseRegex("a", enclose = TRUE), "^[[:lower:]]$")

})

test_that("The combineCases argument works", {

    expect_identical(inverseRegex("A", combineCases = TRUE), "[[:alpha:]]")
    expect_identical(inverseRegex("a", combineCases = TRUE), "[[:alpha:]]")
    expect_identical(inverseRegex("aA", combineCases = TRUE), "[[:alpha:]]{2}")

})

test_that("The combineAlphanumeric argument works", {

    expect_identical(inverseRegex("A", combineAlphanumeric = TRUE), "[[:alnum:]]")
    expect_identical(inverseRegex("1", combineAlphanumeric = TRUE), "[[:alnum:]]")
    expect_identical(inverseRegex("a1", combineAlphanumeric = TRUE), "[[:alnum:]]{2}")

})

test_that("The combinePunctuation argument works", {

    expect_identical(inverseRegex('~!@#$%^&*()_+}{|":?><,./;[]-=',
                                  combinePunctuation = TRUE, numbersToKeep = 29),
                     "[[:punct:]]{29}")
    expect_identical(inverseRegex("\\", combinePunctuation = TRUE), "[[:punct:]]")

})

test_that("The combineSpace argument works", {

    expect_identical(inverseRegex(" \t\n\r", combineSpace = TRUE), "[[:space:]]{4}")

})

test_that("The numbersToKeep argument works", {

    expect_identical(inverseRegex("1", numbersToKeep = NULL), "[[:digit:]]")
    expect_identical(inverseRegex("1", numbersToKeep = 1), "[[:digit:]]{1}")
    expect_identical(inverseRegex("12", numbersToKeep = 2), "[[:digit:]]{2}")
    expect_identical(inverseRegex("12", numbersToKeep = 1), "[[:digit:]]+")
    expect_identical(inverseRegex("12345", numbersToKeep = 1:5), "[[:digit:]]{5}")
    expect_identical(inverseRegex("12345", numbersToKeep = 10:100), "[[:digit:]]+")

})

test_that("The escapePunctuation argument works", {

    expect_identical(inverseRegex('~!@#$%^&*()_+}{|":?><,./;[]-='),
                     '~!@#$%^&*()_+}{|":?><,./;[]-=')
    expect_identical(inverseRegex("\\"), "\\")

    expect_identical(inverseRegex('~!@#$%^&*()_+}{|":?><,./;[]-=', escapePunctuation = TRUE),
                     '\\~\\!\\@\\#\\$\\%\\^\\&\\*\\(\\)\\_\\+\\}\\{\\|\\"\\:\\?\\>\\<\\,\\.\\/\\;\\[\\]\\-\\=')
    expect_identical(inverseRegex("\\", escapePunctuation = TRUE), "\\\\")
})

test_that("The priority argument works", {

    expect_identical(inverseRegex("AB", priority = "A"), "A[[:upper:]]")
    expect_identical(inverseRegex("AB", priority = c("A", "[[:upper:]]")), "A[[:upper:]]")
    expect_identical(inverseRegex("AB", priority = c("A", "[[:upper:]]", "B")), "A[[:upper:]]")

    expect_identical(inverseRegex("abc123?!", priority = c("a", "1", "!", "[[:lower:]]", "[[:digit:]]", ".")), "a[[:lower:]]{2}1[[:digit:]]{2}.!")
})

test_that("We get an error for a non-supported input class", {

    x <- 1:10
    class(x) <- "Foo"

    expect_error(inverseRegex(x))

})

test_that("Inputs other than type character work as expected", {

    expect_identical(inverseRegex(1:10), inverseRegex(as.character(1:10)))

    expect_identical(inverseRegex(seq(0, 1, length.out = 10)),
                     inverseRegex(vapply(seq(0, 1, length.out = 10), format,
                                         character(1), nsmall = 1))
                     )

    expect_identical(inverseRegex(structure(19281, class = "Date")),
                     inverseRegex(as.character(structure(19281, class = "Date"))))

    expect_identical(inverseRegex(structure(1665840397.51796, class = c("POSIXct", "POSIXt"))),
                     inverseRegex(as.character(structure(1665840397.51796, class = c("POSIXct", "POSIXt")))))

    expect_identical(inverseRegex(as.factor(letters)), inverseRegex(letters))

    expect_identical(inverseRegex(matrix(1:4, nrow = 2)),
                     matrix("[[:digit:]]", nrow = 2, ncol = 2)
                     )
    expect_identical(inverseRegex(matrix(letters[1:4], nrow = 2)),
                     matrix("[[:lower:]]", nrow = 2, ncol = 2)
                     )

    expect_equal(inverseRegex(data.frame(a = 1:26, b = LETTERS,
                                         stringsAsFactors = FALSE)),
                 data.frame(a = c(rep("[[:digit:]]", 9), rep("[[:digit:]]{2}", 17)),
                            b = rep("[[:upper:]]", 26),
                            stringsAsFactors = FALSE),
                 check.attributes = FALSE
                 )

    if (requireNamespace("tibble", quietly = TRUE)) {

        expect_equal(inverseRegex(tibble::tibble(a = 1:26, b = LETTERS,
                                                 stringsAsFactors = FALSE)),
                     tibble::tibble(a = c(rep("[[:digit:]]", 9), rep("[[:digit:]]{2}", 17)),
                                    b = rep("[[:upper:]]", 26),
                                    stringsAsFactors = FALSE),
                     check.attributes = FALSE
                     )

    }

})

test_that("NA values are passed through correctly", {

    expect_identical(inverseRegex(NA_character_), NA_character_)

    x <- c(TRUE, FALSE, NA)
    expect_identical(inverseRegex(x), x)

    expect_identical(inverseRegex(c(1L, NA)), c("[[:digit:]]", NA_character_))

    expect_identical(inverseRegex(c(1, NA)), c("[[:digit:]].[[:digit:]]", NA_character_))

    expect_identical(inverseRegex(c(structure(19281, class = "Date"), NA)),
                     c("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}", NA_character_))

    expect_identical(inverseRegex(c(structure(1665840397.51796, class = c("POSIXct", "POSIXt")), NA))[2],
                     NA_character_)

    ## Can't test factor as concatenating a factor with an NA will convert it to an
    ## integer or character.

    x <- matrix(c(1:3, NA), nrow = 2)
    expect_identical(inverseRegex(x), matrix(c(rep("[[:digit:]]", 3), NA_character_), nrow = 2))

    x <- data.frame(a = c(1.1, NA), b = c("a", NA), stringsAsFactors = FALSE)
    expect_equal(inverseRegex(x),
                 data.frame(a = c("[[:digit:]].[[:digit:]]", NA_character_),
                            b = c("[[:lower:]]", NA_character_),
                            stringsAsFactors = FALSE),
                 check.attributes = FALSE
                 )

    if (requireNamespace("tibble", quietly = TRUE)) {

        x <- tibble::tibble(a = c(1.1, NA), b = c("a", NA), stringsAsFactors = FALSE)
        expect_equal(inverseRegex(x),
                     tibble::tibble(a = c("[[:digit:]].[[:digit:]]", NA_character_),
                                    b = c("[[:lower:]]", NA_character_),
                                    stringsAsFactors = FALSE),
                     check.attributes = FALSE
                     )
        expect_s3_class(inverseRegex(tibble::tibble(a = 1:10)), "tbl_df")

    }

})

test_that("The special numeric characters work as expected", {

    x <- c(1, NA, 3.45, NaN, Inf, -Inf)
    out <- c("[[:digit:]].[[:digit:]]", NA, "[[:digit:]].[[:digit:]]{2}", "NaN", "Inf", "-Inf")

    expect_identical(inverseRegex(x), out)

    out <- c("[[:digit:]].[[:digit:]]{2}", NA, "[[:digit:]].[[:digit:]]{2}", "NaN",
             "Inf", "-Inf")

    expect_identical(inverseRegex(as.matrix(x, nrow = 2)), as.matrix(out, nrow = 2))
    expect_equal(inverseRegex(data.frame(a = x)), data.frame(a = out, stringsAsFactors = FALSE),
                 check.attributes = FALSE)

})
