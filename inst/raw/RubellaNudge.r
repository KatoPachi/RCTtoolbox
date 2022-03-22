library(tidyverse)
RubellaNudge <- readr::read_csv("inst/raw/RubellaNudge.csv") %>%
  dplyr::filter(40 <= age & age <= 56 & follow == 1) %>%
  dplyr::filter(
    exp_antibody != 1 &
    exp_vaccine != 1 &
    act_test != 2 &
    act_vaccine != 2
  ) %>%
  dplyr::mutate(
    aw1_negative = if_else(act_vaccine != 4, 1, 0),
    aw1_testnega = aw1_test * aw1_negative
  ) %>%
  dplyr::select(
    itest = test_int,
    ivacc = vaccine_int,
    atest = aw1_test,
    avacc = aw1_testvaccine,
    anega = aw1_testnega,
    treat = nudge,
    coupon = coupon2019,
    age = age,
    educ = education
  )

usethis::use_data(RubellaNudge, overwrite = TRUE)

