#' Title
#'
#' @param data
#' @param probabilities
#' @param sample
#' @param id.num
#' @param id.den
#'
#' @return
#' @export
#'
#' @examples
prob.to.dr <- function(data,
                       probabilities,
                       sample, # variable identifier
                       id.num, # sample
                       id.den){ # sample

  # total n
  total.n <- nrow(data)

  # Split data into numerator and denominator
  numerator <- data %>%
    filter({{ sample }} == id.num) # Naming numerator is appropriate?

  denominator <- data %>%
    filter({{ sample }} == id.den)

  # Priors
  prior.num <- nrow(numerator)/total.n
  prior.den <- nrow(denominator)/total.n

  # Density ratio

  probabilities <- as.vector(select(data, {{ probabilities }} ))[[1]]

  num <- prior.num*probabilities
  den <- prior.den*(1-probabilities)
  dr <- (num/den)

  return(dr)
}

#' Title
#'
#' @param model
#' @param data
#' @param sample
#' @param id.num
#' @param id.den
#'
#' @return
#' @export
#'
#' @examples
dr.via.log.model <- function(model, data, sample = sample, id.num = "numerator", id.den = "denominator"){


  data$predictions <- predict(model, type = "response", newdata = data)

  dr <- prob.to.dr(data = data, probabilities = predictions, sample = sample, id.num = "numerator", id.den = "denominator")
  return(dr)
}
