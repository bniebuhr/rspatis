#' Movement data for Sturnira lilium bats
#'
#' Radiotelemetry movement data of \italic{S. lilium} frugivorous bats in the state of Sao Paulo.
#'
#' @usage
#' data(bats)
#' data(bats_sf)
#' data(bats_sp)
#'
#' @format Three objects based on the same data: \code{bats}, a data frame with the raw
#' movement data; \code{bats_sf}, a spatial data frame of the class \code{\link{sf}};
#' and \code{bats_sp}, a \code{\link{SpatialPointsDataFrame}}.
#' These tables contain 767 rows and 7 variables:
#' \describe{
#'   \item{Tag_ID}{ID of radio collar}
#'   \item{Animal_ID}{ID of the individual bat}
#'   \item{x, y}{longitude and latitute of the record (UTM 23S, WGS84)}
#'   \item{date}{date of the record}
#'   \item{time}{time of the day of the record}
#'   \item{position}{whether the animal was in the roost or was foraging (vertical)
#'   or flying (horizontal)}
#' }
#'
#' @example
#' examples/data_example.R
#'
#' @references
#' Kerches-Rogeri, P., Niebuhr, B.B., Muylaert, R.L, Mello, M.A.R.  Individual
#' specialization in the space use of frugivorous bats. Journal of Animal Ecology.
#'
#' @source What do we put here?
#' @keywords data
"bats"



