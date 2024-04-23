context("test-create_moment")

fake_wave1 <- data.frame(Id = seq_len(100L),
                         wealth = exp(rnorm(100L)),
                         age = sample(20:80, size = 100L,
                                      replace = TRUE))

fake_wave2 <- data.frame(Id = c(seq_len(50L), 101:150),
                         wealth = exp(rnorm(100L)))


fake_longitudinal <- data.frame(
  'Id' = seq_len(50L),
  'age_2015' = sample(20:80, size = 50L,
                    replace = TRUE),
  'w_real_2015' = exp(rnorm(50L))
)


fake_longitudinal <- merge(
  fake_wave1, fake_wave2, by = "Id",
  suffixes = c("_2015","_2018")
)

data.table::setnames(
  fake_longitudinal, old = "age", new = "age_2015"
)

data.table::setDT(fake_longitudinal)[, 'age_2018' := get('age_2015')+3L]

data.table::setDT(fake_wave1)[,'annee' := 2015L]
data.table::setDT(fake_wave2)[,'annee' := 2018L]

data.table::setnames(
  fake_longitudinal, old = c('wealth_2015', 'wealth_2018'),
  new = c("w_real_2015","w_real_2018")
)


testthat::test_that("When Nmoments=1, log scale is automatically applied", {
  testthat::expect_equal(
    create_moment(EP_2015 = fake_wave1,
                EP_2018 = fake_wave2,
                EP_lon = fake_longitudinal,
                scale = "log",
                age_2015_var = "age_2015",
                age_var = "age",
                wealth_var = "wealth",
                N_moments = 1L,
                normalize = FALSE,
                exclude_negative = FALSE
  ),
  create_moment(EP_2015 = fake_wave1,
                EP_2018 = fake_wave2,
                EP_lon = fake_longitudinal,
                scale = "level",
                age_2015_var = "age_2015",
                age_var = "age",
                wealth_var = "wealth",
                N_moments = 1L,
                normalize = FALSE,
                exclude_negative = FALSE
  )
  )
})


# Nmoments = 2 --------------------


data_moment <- create_moment(EP_2015 = fake_wave1,
                             EP_2018 = fake_wave2,
                             EP_lon = fake_longitudinal,
                             scale = "log",
                             age_2015_var = "age_2015",
                             age_var = "age",
                             wealth_var = "wealth",
                             N_moments = 2L,
                             normalize = FALSE,
                             exclude_negative = FALSE
)


m1 <- moment_pic(fake_wave1,
                 wealth_var = "wealth",
                 age_var = "age",
                 moment_var = "m1",
                 normalize = FALSE,
                 scale = "log",
                 survey_year = 2015L,
                 exclude_negative = FALSE)


m2 <- moment_variation_K(fake_longitudinal, age_2015_var = "age_2015",
                         format = "wide",
                         stat = "difference", normalize = FALSE,
                         scale = "level")

testthat::test_that("When Nmoments=2, automatically log scale for first one, level scale for the second one", {
  testthat::expect_equal(
    data_moment$moment_data,
    c(m1$m1, m2$moment_data)
  )
})




