context("moment_variation_K returns median growth rate between two waves")

fake_longitudinal <- data.frame(
  'Id' = seq_len(50L),
  'age_2015' = sample(20:80, size = 50L,
                      replace = TRUE),
  'w_real_2015' = exp(rnorm(50L))
)

fake_longitudinal$age_2018 <- fake_longitudinal$age_2015 + 3L
fake_longitudinal$w_real_2018 <- fake_longitudinal$w_real_2015 + rnorm(50L)

data.table::setDT(fake_longitudinal)


m2 <- moment_variation_K(fake_longitudinal, age_2015_var = "age_2015",
                         format = "wide",
                         stat = "proportion", normalize = FALSE)

m2_theory <- fake_longitudinal[get('age_2015') %between% c(35,50), .('W' = median(get('w_real_2018')/get('w_real_2015')))]


testthat::test_that("We return median growth rate between two waves", {
  testthat::expect_equal(as.numeric(m2$moment_data), as.numeric(m2_theory$W))
})
