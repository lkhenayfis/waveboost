
carga <- get_carga_observada("SECO", "2020-01-01/2020-01-03")

test_that("force_slice_hour", {
    slice <- slice(carga, "cargaglobalcons", "datahora", start = 16, L = seq(-7, 0), step = 48)

    forced_00 <- force_slice_hour(slice, "00:00:00")
    expect_equal(slice$cargaglobalcons, forced_00$cargaglobalcons)
    expect_true(all(format(attr(forced_00, "index"), "%H:%M") == "00:00"))

    forced_1030 <- force_slice_hour(slice, "10:30:00")
    expect_equal(slice$cargaglobalcons, forced_1030$cargaglobalcons)
    expect_true(all(format(attr(forced_1030, "index"), "%H:%M") == "10:30"))
})

test_that("offset_slice_time", {
    slice <- slice(carga, "cargaglobalcons", "datahora", start = 16, L = seq(-7, 0), step = 48)

    off_1day <- offset_slice_time(slice, "1 day")
    expect_equal(slice$cargaglobalcons, off_1day$cargaglobalcons)
    expect_true(all(attr(off_1day, "index") == attr(slice, "index") + 24 * 60 * 60))

    off_1day_2 <- offset_slice_time(slice, 24 * 60 * 60)
    expect_equal(slice$cargaglobalcons, off_1day_2$cargaglobalcons)
    expect_true(all(attr(off_1day_2, "index") == attr(slice, "index") + 24 * 60 * 60))

    off_40min <- offset_slice_time(slice, "40 min")
    expect_equal(slice$cargaglobalcons, off_40min$cargaglobalcons)
    expect_true(all(attr(off_40min, "index") == attr(slice, "index") + 40 * 60))
})

test_that("replicate_slice_day_ahead", {
    slice <- slice(carga, "cargaglobalcons", "datahora", start = 16, L = seq(-7, 0), step = 48)

    repl <- replicate_slice_day_ahead(slice)

    expect_equal(repl$cargaglobalcons, rep(slice$cargaglobalcons, 48))

    index_repl  <- attr(repl, "index")
    index_slice <- attr(slice, "index")

    expect_equal(length(index_repl), length(index_slice) * 48)
})

test_that("build_lagged_slice", {
    built <- build_lagged_slice(carga, "cargaglobalcons", 8)

    new_1 <- c(146.2389, -157.3065, -233.2180, -133.2041, -37.2370, -539.9685, -1155.6702,
        47553.4394, 47500.7784, 46753.2541, 45989.6233, 67213.4820, 65579.1175, 93898.5476)
    new_last <- c(-735.31681, -382.43163, -238.13942, 17.32058, -1574.39450, -305.53250,
        -2585.05016, 50945.30057, 48718.77051, 47463.02948, 47030.94127, 70473.14050,
        66817.32750, 97079.02092)
    expect_equal(unname(built$carga[[1]]), new_1)
    expect_equal(unname(tail(built$carga, 1)[[1]]), new_last)

    indices <- seq(as.POSIXct("2020-01-01 07:30:00", "GMT"), length.out = 3, by = "1 day")
    expect_equal(attr(built, "index"), indices)
})