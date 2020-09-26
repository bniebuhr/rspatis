#' Calculation of Individual Specialization in the use of space from animal tracking
#'
#' This function calculates utilization distributions (UD) for both
#' the individuals and the whole (or rest of the) population, based on
#' telemetry location points, and calculates both the Spatial Individual Specialization
#' Index (`SpatIS`) and the Spatial Individual Complementary Specialization Index (SpatICS).
#' The individual SpatIS is calculated as the volume of the individual UD
#' (or a X% kernel density estimation area, X defined by the user) that do
#' not overlap with the populational UD (or a correspondent population KDE area).
#' The individual SpatICS is calculated as the volume of the individual UD
#' (or a X% KDE area, X defined by the user) that do
#' not overlap with the UD of the rest of the population, after excluding that individual
#' (or a correspondent KDE area of the rest of the population).
#' The population SpatIS and SpatICS are the average of the the individual
#' SpatIS and SpatICS values, averaged over all individuals.
#'
#' @param data a SpatialPointsDataFrame object containing the locations of
#' the individuals, their IDs, and other spatial or individual
#' information.
#' @param individuals.col name (character) or number of the column of the input
#' SpatialPointsDataFrame 'data' that represents the identity of individuals.
#' @param population.ID ID (character or number) that represents the population in the
#' column 'individuals.col' of the input dataset 'data'. In case
#' the locations of all individuals were not pooled and combined
#' to the original data, population.ID is set to NULL (the default).
#' In this case, the calculations for the whole population are
#' made inside the SpatIS function.
#' @param index character or vector of characters indicating the indexes to be calculated
#' (\code{"spatis"} or \code{"spatics"}).
#' @param method method to calculate the overlap between utilization distributions
#' or areas of use (the same options as the function `adehabitatHR::kerneloverlap`,
#' except for the "HD" method; see ?adehabitatHR::kerneloverlap for more information).
#' The default method is "VI", which computes the volume of the intersection between
#' the individual and populational UDs.
#' @param ... additional arguments to be passed to the function `adehabitatHR::kerneloverlap`
#' to calculate the overlap between UDs or to the function `adehabitat::kernelUD``
#' for the kernel estimation of the utilization distribution.
#'
#' @return The SpatIS function returns a list of six elements:
#'  \item{data}{the data used to calculate SpatIS (with locations corresponding
#'     to the whole population, independently of whether the population
#'     locations were already in the input data). It is a SpatialPointsDataFrame.}
#'  \item{parms}{a list with parameters used as input to call SpatIS function.}
#'  \item{SpatIS.individual}{} a vector of Spatial individual specialization indices for
#'     each individual of the population (the overlap between
#'     each individual and the population UDs).}
#'  \item{SpatIS.population}{a value of Spatial individual specialization for the population,
#'     calculated as the average of all SpatIS individual values.}
#'  \item{SpatICS.individual}{a vector of Spatial individual complementary specialization
#'     indices for each individual of the population (the overlap between
#'     each individual and the population UDs).}
#'  \item{SpatICS.population}{a value of Spatial individual complementary specialization
#'     for the population, calculated as the average of all SpatICS individual values.}
#'
#'  @author Bernardo B. Niebuhr <bernardo_brandaum@yahoo.com.br> and Patricia Kerches-Rogeri
#'  <parogeri@gmail.com>.
#'
#'  @references
#'  Kerches-Rogeri, P., Niebuhr, B.B., Muylaert, R.L, Mello, M.A.R.  Individual
#'  specialization in the space use of frugivorous bats. Journal of Animal Ecology.
#'
#'  @seealso [adehabitatHR::kernelUD()] for the estimation of utilization distributions, and
#'  [adehabitatHR::kerneloverlap()] for the calculation of the overlap between utilization distributions or
#'  areas of use.
#'
#'  @export
spatis <- function(data, individuals.col, population.ID = NULL, index = c("spatis", "spatics"),
                   method = c("VI", "HR", "PHR", "BA", "UDOI")[1], ...)
{
  # Check if the data are of the class SpatialPoints
  if (!inherits(data, "SpatialPoints"))
    stop("Data should inherit the class SpatialPoints.")

  # Check if the method is valid
  available.methods = c("VI", "HR", "PHR", "BA", "UDOI")
  if(!(method %in% available.methods))
    stop(paste0("Argument 'method' should be one of the following options: ",
                paste(available.methods, collapse = ", "), "."))

  # Copying data and transforming ID values in character variables
  data2 <- SpatialPointsDataFrame(data, data@data, match.ID = F)
  data2[[individuals.col]] <- as.character(data2[[individuals.col]])

  if("spatis" %in% tolower(index)) {
    # If population.ID is NULL, the points representing all the population were not
    #   calculated yet. They will be calculated then.
    if(is.null(population.ID)) {
      pop.ID <- "all"
      pop.dat <- data2 # Copying data
      pop.dat[[individuals.col]] <- pop.ID # We create a new "individual" with the ID "all"
      # which represents all population points
      data.aux <- rbind(data2, pop.dat) # Combining data from individuals and the population
    } else {
      # If the population.ID was furnished by the user, check if it really exists in the input data
      if(!(population.ID %in% unique(data2[[individuals.col]]))) {
        # If the population.ID does not exist, show an error message
        stop(paste("The population ID \"",population.ID,"\" is absent from the input data. Plase set the parameter population.ID to NULL or change its value..", sep = ""))
      } else {
        # If it exists, consider the input data itself and go on
        pop.ID <- population.ID
        data.aux <- data2
      }
    }

    # This is a matrix with the overlap of utilization distribution between each pair of individuals
    # and each individual and the whole population
    over <- kerneloverlap(data.aux[,1], method = method, ...)

    # Line in the overlap matrix that represents the population
    population.line <- which(rownames(over) == pop.ID)

    # Overlap of each individual with the whole population utilization distribution
    SpatIS.ind.aux <- over[-population.line,population.line]
    # SpatIS for individuals = 1 - overlap of the individual with the population
    SpatIS.ind <- 1 - SpatIS.ind.aux#spatis.calc(over = SpatIS.ind.aux, method = method)
    # SpatIS = average of individual SpatIS
    SpatIS.pop <- mean(SpatIS.ind)
  } else {
    SpatIS.ind <- SpatIS.pop <- NULL
  }

  if("spatics" %in% tolower(index)) {

    # function to calculate overlap not with the whole population, but with
    # each individual besides the one in question
    overlap.remaining.inds <- function(x, indiv, individuals.col, method = method, ...) {
      ind1 <- x[x[[individuals.col]] == indiv,]
      remaining.inds <- x[x[[individuals.col]] != indiv,]
      remaining.inds[[individuals.col]] <- "remaining"
      data.aux <- rbind(ind1, remaining.inds)

      over <- kerneloverlap(data.aux[,1], method = method, ...)[1,2]
      return(over)
    }

    # If population.ID is not NULL, remove the points that correspond to the population
    if(!is.null(population.ID)) {
      # If the population.ID was furnished by the user, check if it really exists in the input data
      if(!(population.ID %in% unique(data2[[individuals.col]]))) {
        # If the population.ID does not exist, show an error message
        stop(paste("The population ID \"",population.ID,"\" is absent from the input data. Plase set the parameter population.ID to NULL.", sep = ""))
      } else {
        # If it exists, remove it from the data for this analysis
        data.aux <- data2[data2[[individuals.col]] != population.ID,]
      }
    } else {
      data.aux <- data2[data2[[individuals.col]] != "all",]
    }

    # Here we calculate, for each individual, the overlap between each individual with the rest of the population
    all.inds <- sort(unique(data.aux[[individuals.col]]))
    over <- suppressWarnings(sapply(all.inds, overlap.remaining.inds, x = data.aux,
                                    individuals.col = individuals.col, method = method, ...))

    # SpatICS for individuals = 1 - overlap of the individual with the rest of the population
    SpatICS.ind <- 1 - over#spatis.calc(over = over, method = method)
    # SpatIS = average of individual SpatIS
    SpatICS.pop <- mean(SpatICS.ind)
  } else {
    SpatICS.ind <- SpatICS.pop <- NULL
  }

  # List of parameters
  parms <- list(individuals.col = individuals.col, population.ID = population.ID, index = index, method = method, ...)

  return( list(data = data2, parms = parms, SpatIS.individual = SpatIS.ind, SpatIS.population = SpatIS.pop,
               SpatICS.individual = SpatICS.ind, SpatICS.population = SpatICS.pop) )
}
