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
prob.to.dr <- function(data, probabilities, sample, id.num, id.den){

  # total n
  total.n <- nrow(data)

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
