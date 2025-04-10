library(data.table)


index <- seq(as.Date("2020-01-01"), as.Date("2024-12-31"), by ="day")
block_by <- yearmonth
split_in <- 4

blocked   <- block_samples(index, block_by, split_in)
all_paths <- block2paths(blocked)





















balanced_sample <- function(x, size) {
    N <- length(x)
    repeats  <- 1 + (size %/% N)
    samples  <- c(rep(N, repeats - 1), size %% N)

    sample <- lapply(seq_len(repeats), function(r) sample(x, samples[r]))
    sample <- unlist(sample)

    return(sample)
}
