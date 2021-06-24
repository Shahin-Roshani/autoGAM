
#' @title backward_select
#'
#' @import tidyverse magrittr gam splines fastDummies car future furrr AICcmodavg
#'
#' @description P-value based backward selection on Generalized Linear Models (GLMs).
#'
#' @param object An object of class \code{autoGAM_frame} or a formula of the full model to be processed via backward elimination.
#' @param data Data needed for creation of models. Default is \code{NULL} for objects of class \code{autoGAM_frame} where data will be automatically retrieved from the object but data needs to be specified when the \code{object} argument is a formula.
#' @param backward.test Test to obtain P-values of predictors during the backward elimination process. Valid values are \code{'LRT'}, \code{'Rao'}, \code{'Chisq'} & \code{'F'}. Default is \code{'LRT'}. See \code{?drop1} for more details.
#' @param backward.alpha The maximum alpha value that predictors with P-values greater than that will be sequentially removed. Default value is \emph{0.05}.
#' @param family The family of the response variable. Default is \code{gaussian(link='identity')} for situations where \code{object} argument is a formula (for formulas with responses from other classes, you need to specify this argument). When \code{object} is from class \code{autoGAM_frame}, default value will be automatically replaced by the true family coming from the object.
#'
#' @return If the object is from class \code{autoGAM_frame}, the \code{$`best forms`} & \code{$`final predictors`} parts of the object will be updated and a list of sequential results of backward elimination process will be added to the \code{autoGAM_frame} list. If the object is a formula, the mentioned sequential list of backward process will be returned.
#'
#' @author Shahin Roshani
#'
#' @examples \dontrun{
#'
#' my_mtcars <- mtcars %>% mutate_at('vs',as.factor)
#'
#' carsGAM <- autoGAM_frame(mpg~disp+drat+vs,data=my_mtcars)
#'
#' backward_select(carsGAM)
#'
#' #Or for example:
#'
#' backward_select(mpg~poly(disp,2)+I(drat^3)+vs,data=my_mtcars)
#'
#' my_iris <- iris %>% filter(Species!='setosa')
#'
#' backward_select(Species~ns(Sepal.Length,3)+cut(Petal.Width,2),data=my_iris,family=binomial(link='logit'))
#'
#' }
#'
#' @seealso \url{https://shahin-roshani.github.io/autoGAM/articles/autoGAM.html}
#'
#' @export

backward_select <- function(object,

                            data=NULL,

                            backward.test='LRT',

                            backward.alpha=.05,

                            family=gaussian(link='identity')){


  if (!(class(object) %in% c('formula','autoGAM_frame'))){

    stop('object argument must be either a formula or an object of class autoGAM_frame!',call.=F)

  }


  if (!(backward.test %in% c('LRT','Rao','Chisq','F'))){

    stop('Valid tests for GLMs are LRT, Rao, Chisq or F.',call.=F)

  }


  backward <- function(resp,preds,data,family,test,alpha){

    form <- str_c(resp,'~',str_c(preds,collapse='+')) %>%

      as.formula

    glm(form,data=data,family=family) -> fit

    x <- drop1(fit,test=test) %>% rownames_to_column() %>% as_tibble

    y <- x %>% na.omit

    l = v <- list() ; l[[1]] <- y ; i <- 1 ; j <- 0

    while (any(y[[6]]>alpha) & length(y[[6]])>1){

      i <- i + 1 ; j <- j + 1

      row <- y %>% filter(.[[6]]==max(.[[6]]))

      v[[j]] <- str_c(row %>% .$rowname,' [',names(row)[6],' = ',

                      row %>% .[[6]] %>% round(.,4),']')

      y %<>% filter(.[[6]]!=max(.[[6]])) %>% .$rowname

      fit %<>% update(str_c('~',str_c(y,collapse='+')) %>%

                        as.formula)

      y <- drop1(fit,test=test) %>% rownames_to_column() %>%

        as_tibble %>% na.omit

      l[[i]] <- y

    }


    l %<>% map(~column_to_rownames(.,'rowname'))

    names(l) <- str_c('step',0:(length(l)-1))

    if (!is_empty(v)){

      names(l) <- map2(.x=names(l),

                       .y=c('',str_c(': removed ',v)),

                       .f=~str_c(.x,.y))

    }

    if (nrow(l[[length(l)]])==1 && l[[length(l)]][,5]>alpha){

      warning('None of the predictors were significant. Returning the last remaining predictor!',call.=F)

    }

    return(l)

  }


  if (class(object)=='formula'){


    if (is_empty(data)){

      stop('data must be specified when object is a formula!',call.=F)

    }


    deparsed_formula <- deparse(object) %>% str_split('\\~') %>% .[[1]] %>%

      str_remove_all(' ')

    resp <- deparsed_formula[1]

    if (str_detect(resp,'(\\+|\\*|\\/|\\-)') | resp==''){

      stop('Please specify a single variable as response!',call.=F)

    }

    preds <- deparsed_formula[2] %>% str_split('\\+') %>% .[[1]]

    if (length(preds)==1 && preds=='.'){

      preds <- data %>% select(-any_of(resp)) %>% names

    }


    res <- backward(resp,preds,data,family,backward.test,backward.alpha)


  } else{

    data <- object$data

    resp <- names(object$data)[1]

    preds <- object$`final predictors`

    family <- object$`response family`

    backward_info <- backward(resp,preds,data,family,

                              backward.test,backward.alpha)

    object$`final predictors` <- backward_info[[length(backward_info)]] %>%

      row.names()

    object$`backward info` <- backward_info

    res <- object

  }

  return(res)

}
