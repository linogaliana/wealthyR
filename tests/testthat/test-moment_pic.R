context("moment_pic returns maximum median wealth")

fake_wave1 <- data.frame(Id = seq_len(100L),
                         wealth = exp(rnorm(100L)),
                         age = sample(20:80, size = 100L,
                                      replace = TRUE),
                         annee = 2015L)

m1 <- moment_pic(data.table::setDT(fake_wave1), wealth_var = "wealth",
                 age_var = "age",
                 moment_var = "m1",
                 normalize = FALSE,
                 scale = "level",
                 survey_year = 2015L,
                 exclude_negative = FALSE)
m1_log <- moment_pic(data.table::setDT(fake_wave1), wealth_var = "wealth",
                     age_var = "age",
                     moment_var = "m1",
                     normalize = FALSE,
                     scale = "log",
                     survey_year = 2015L,
                     exclude_negative = FALSE)

# CREATE AGE THRESHOLDS
fake_wave1b <- data.table::copy(fake_wave1)[,'age_tranche' := floor(get("age")/5)]
fake_wave1b <- fake_wave1b[get('age_tranche') %between% c(5, 15)]

# STATS IN LEVEL
stats_age1 <- fake_wave1b[,.('W' = median(get('wealth'))), by = "age_tranche"]
stats_age1 <- stats_age1[get('W') == max(get('W'))]            

# STATS IN LOG
stats_age2 <- fake_wave1b[,.('W' = median(log(get('wealth')))), by = "age_tranche"]
stats_age2 <- stats_age2[get('W') == max(get('W'))]


testthat::test_that(
  "Value returned is the one expected [scale = level]",
  testthat::expect_equal(stats_age1$W, m1$m1)
)

testthat::test_that(
  "Value returned is the one expected [scale = log]",
  testthat::expect_equal(stats_age2$W, m1_log$m1)
)
