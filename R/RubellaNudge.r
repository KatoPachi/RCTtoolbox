#' Online Survey Experiment of Text-Based Nudges
#' to Promote Rubella Antibody Test and Vaccination
#'
#' Online survey data which is used in Kato et al. (2022)
#'
#' @format A data frame with 2272 rows and 9 variables:
#' \describe{
#'   \item{itest}{A dummy of intention to take an antibody test}
#'   \item{ivacc}{A dummy of intention to take a vaccination}
#'   \item{atest}{A dummy of taking an antibody test}
#'   \item{avacc}{A dummy of taking a vaccination}
#'   \item{anega}{A dummy of negative test}
#'   \item{treat}{String of treatment assignment. Baseline is "A".}
#'   \item{coupon}{A dummy of receiving free coupons for test and vaccination.}
#'   \item{age}{Age}
#'   \item{educ}{Educational year}
#' }
"RubellaNudge"