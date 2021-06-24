
#' @title autoGAM_fit
#'
#' @import tidyverse magrittr gam splines fastDummies car future furrr AICcmodavg
#'
#' @description A wrapper function to fit final (best) Generalized Additive Model (GAM) from an object of class \code{autoGAM_frame}.
#'
#' @param object An object of class \code{autoGAM_frame}.
#'
#' @return Adds the final fitted model to the \code{autoGAM_frame} and prints out the summary of the final fit.
#'
#' @author Shahin Roshani
#'
#' @examples \dontrun{
#'
#' my_mtcars <- mtcars %>% mutate_at('vs',as.factor)
#'
#' carsGAM <- autoGAM_frame(mpg~disp+drat+vs,data=my_mtcars)
#'
#' autoGAM_fit(carsGAM)
#'
#' backward_select(carsGAM) %>% autoGAM_fit
#'
#' }
#'
#' @seealso \url{https://shahin-roshani.github.io/autoGAM/articles/autoGAM.html}
#'
#' @export

autoGAM_fit <- function(object){

  if (class(object)!='autoGAM_frame'){

    stop('Input must be an object of class autoGAM_frame!',call.=F)

  }

  data <- object$data

  resp <- names(data)[1]

  final_preds <- object$`final predictors`

  family <- object$`response family`

  final_formula <- str_c(resp,'~',str_c(final_preds,collapse='+')) %>%

    as.formula

  final_model <- glm(final_formula,data=data,family=family)

  object$`final fit` <- final_model

  return(object)

}
