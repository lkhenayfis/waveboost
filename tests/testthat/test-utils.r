
test_that("upsample", {
    v <- as.numeric(seq(5))
    up_v <- upsample(v, 2, "lin", TRUE)

    expect_equal(up_v, seq(1, 5.5, by = .5))
})