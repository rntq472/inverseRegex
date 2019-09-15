

context('Test inverseRegex')


test_that('Various inputs work as expected', {
    
    expect_equal(occurrencesLessThan(c(LETTERS, 1)), c(rep(FALSE, 26), TRUE))
    expect_equal(occurrencesLessThan(c(1, 2, 3, 'a'), 0.25), c(FALSE, FALSE, FALSE, TRUE))
    expect_equal(occurrencesLessThan(c(1, 2, 'a'), n = 1), c(FALSE, FALSE, TRUE))
    expect_equal(occurrencesLessThan(c('a', 'A', rep(1:9, 2))), c(TRUE, TRUE, rep(FALSE, 18)))
    
    expect_equal(occurrencesLessThan(c(rep(1, 99), 1.12)), c(rep(FALSE, 99), TRUE))
    
    expect_equal(FALSE, unique(occurrencesLessThan(rep(Sys.Date(), 200))))
    expect_equal(FALSE, unique(occurrencesLessThan(rep(Sys.time(), 200))))
    expect_equal(FALSE, unique(occurrencesLessThan(rep(1L, 200))))
    
    x <- c(rep(FALSE, 20), TRUE)
    expect_equal(occurrencesLessThan(x), x)
    
    expect_equal(FALSE, unique(occurrencesLessThan(iris$Species)))
    
    x <- iris
    x$Species <- as.character(x$Species)
    x[27, 'Species'] <- 'set0sa'
    expect_true(occurrencesLessThan(x)[27, 'Species'])
    
    x <- matrix(c(1:99, 'a'), nrow = 10, ncol = 10)
    expect_true(occurrencesLessThan(x)[10, 10])
    expect_equal(sum(occurrencesLessThan(x, 0.05)), 1)
    
})

test_that('We get an error for a non-supported input class', {
    
    x <- 1:10
    class(x) <- 'Foo'
    
    expect_error(occurrencesLessThan(x))
    
})
