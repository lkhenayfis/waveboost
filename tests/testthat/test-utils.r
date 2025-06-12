
# RESAMPLE -----------------------------------------------------------------------------------------

test_that("resample", {
    v <- as.numeric(seq(5))
    up_v <- resample(v, 2, "lin", TRUE)

    expect_equal(up_v, seq(1, 5.5, by = .5))
})

# ESCALONAMENTO ------------------------------------------------------------------------------------

gen_dummy_dt_scale <- function(N = 10, seed = 1234) {
    set.seed(seed)
    data.table(
        date = as.Date("2020-01-01") + seq(N),
        datetime = as.POSIXct("2020-01-01 00:00:00") + seq(N),
        char1 = letters[seq(N)],
        int1 = seq(N),
        num1 = rnorm(N, 10, 2),
        num2 = rnorm(N, -3, .5),
        num3 = rnorm(N, 4, 1.2)
    )
}

test_that("scale2", {

    dummy <- gen_dummy_dt_scale()

    # somente dt

    scaled_dummy <- scale2(dummy)

    expect_true(inherits(scaled_dummy, "scaled_dt"))
    expect_equal(names(dummy), names(scaled_dummy))
    expect_equal(names(dummy), names(attr(scaled_dummy, "scales")))
    expect_equal(names(dummy), names(attr(scaled_dummy, "ignored")))

    expect_equal(dummy$date, scaled_dummy$date)
    expect_equal(dummy$datetime, scaled_dummy$datetime)
    expect_equal(dummy$char1, scaled_dummy$char1)
    expect_equal(dummy$int1, scaled_dummy$int1)

    expect_equal(as.numeric(scale(dummy$num1)), scaled_dummy$num1)
    expect_equal(as.numeric(scale(dummy$num2)), scaled_dummy$num2)
    expect_equal(as.numeric(scale(dummy$num3)), scaled_dummy$num3)

    # baseado em uma ref

    dummy2 <- gen_dummy_dt_scale(seed = 1233)
    scaled_dummy_2 <- scale2(dummy2)

    scaled_dummy_from_ref <- scale2(dummy, scaled_dummy_2)

    expect_true(inherits(scaled_dummy_from_ref, "scaled_dt"))
    expect_equal(names(dummy), names(scaled_dummy_from_ref))
    expect_equal(names(dummy), names(attr(scaled_dummy_from_ref, "scales")))
    expect_equal(names(dummy), names(attr(scaled_dummy_from_ref, "ignored")))

    expect_equal(dummy$date, scaled_dummy_from_ref$date)
    expect_equal(dummy$datetime, scaled_dummy_from_ref$datetime)
    expect_equal(dummy$char1, scaled_dummy_from_ref$char1)
    expect_equal(dummy$int1, scaled_dummy_from_ref$int1)

    expect_equal((dummy$num1 - mean(dummy2$num1)) / sd(dummy2$num1), scaled_dummy_from_ref$num1)
    expect_equal((dummy$num2 - mean(dummy2$num2)) / sd(dummy2$num2), scaled_dummy_from_ref$num2)
    expect_equal((dummy$num3 - mean(dummy2$num3)) / sd(dummy2$num3), scaled_dummy_from_ref$num3)
})

test_that("rescale2", {

    dummy <- gen_dummy_dt_scale()

    # somente dt

    scaled_dummy <- scale2(dummy)
    rescaled_dummy <- rescale2(scaled_dummy)

    expect_equal(dummy, rescaled_dummy)

    # baseado em uma ref

    dummy2 <- gen_dummy_dt_scale(seed = 1233)
    scaled_dummy_2 <- scale2(dummy2)
    rescaled_dummy_from_ref <- rescale2(dummy2, scaled_dummy_2)

    expect_equal(dummy2$date, rescaled_dummy_from_ref$date)
    expect_equal(dummy2$datetime, rescaled_dummy_from_ref$datetime)
    expect_equal(dummy2$char1, rescaled_dummy_from_ref$char1)
    expect_equal(dummy2$int1, rescaled_dummy_from_ref$int1)

    expect_equal((dummy2$num1 * sd(dummy2$num1)) + mean(dummy2$num1), rescaled_dummy_from_ref$num1)
    expect_equal((dummy2$num2 * sd(dummy2$num2)) + mean(dummy2$num2), rescaled_dummy_from_ref$num2)
    expect_equal((dummy2$num3 * sd(dummy2$num3)) + mean(dummy2$num3), rescaled_dummy_from_ref$num3)
})

test_that("recover_scale_ingored", {

    dt  <- gen_dummy_dt_scale()
    ref <- scale2(dt)
    dt2 <- gen_dummy_dt_scale(seed = 1233)[, 1:5]
    ref2 <- scale2(dt2)

    # erros simples de controle

    expect_error(recover_scale_ignored(dt))
    expect_error(recover_scale_ignored(dt, NULL))
    expect_error(recover_scale_ignored(dt, dt2))
    expect_error(recover_scale_ignored(dt, ref2))

    # checando escalas corretamente recuperadas

    rec1 <- recover_scale_ignored(ref, NULL)
    expect_equal(rec1[[1]], attr(ref, "scales"))
    expect_equal(rec1[[2]], attr(ref, "ignored"))

    rec2 <- recover_scale_ignored(dt2, ref2)
    expect_equal(rec2[[1]], attr(ref2, "scales"))
    expect_equal(rec2[[2]], attr(ref2, "ignored"))
})