
#' @title autoGAM_frame
#'
#' @import tidyverse magrittr gam splines fastDummies car future furrr AICcmodavg
#'
#' @description Automatic evaluation of different forms (functions) of predictors to obtain the best Generalized Additive Model (GAM).
#'
#' @param formula A simple formula object with one response and predictor(s).
#' @param resp.base Base level of the binary response variable. Default is \code{NULL}.
#' @param forms A named list of continuous predictor(s) form(s) with their respected degree(s) or degree(s) of freedom. Famous functions that one can think of are \code{identity()}, \code{log()}, \code{log2()}, \code{logb()}, \code{exp()}, \code{bs()} & \code{ns()} (from splines package), \code{s()} (from gam package) and \code{cut()}. Default is \code{list('identity','logb','exp','power'=2:3,'poly'=2:3)}.
#' The names of the list are the function(s) names and their respected value(s) are their degree(s)/degree(s) of freedom. Only functions with the form: \emph{f(x)} or \emph{f(x,degree)} with a vector or a matrix as their output should be included in this list.
#' Note: Functions with the \emph{f(x)} form can be passed in 2 ways: 1. \code{'f'=c()} or 2. \code{'f'}. Any function of the form \emph{f(vector,degree)} should be passed as \code{'f'=}desired degree(s).
#' If a function of the form \emph{f(vector,degree)} is passed as a single character, the default value for degree will be used and if there's no default value for degree, an error will occur.
#' @param data Dataset containing the response and all the predictors that were included in the formula.
#' @param ignore.outliers Logical indicating whether outliers should be ignored during the evaluation process of predictors forms or not. Default is \code{FALSE}. When \code{TRUE}, outliers in case of response are detected by \code{car::outlierTest()} which is based on studentized residuals of records and outliers in case of the predictor are determined as records that have \emph{hat-values > 2p} (where \emph{p} is the number of parameters inside the model).
#' @param family Family for the response variable in model fits. Default is \code{gaussian(link='identity')}.
#' @param metric The name of the metric to be used for evaluation of GLMs performances. Valid values are \code{'AIC'}, \code{'BIC'} & \code{'AICc'}. Default is \code{'AIC'}.
#' @param raw.poly Logical indicating whether raw forms of polynomials should be included when polynomial forms (\code{'poly'}) are being evaluated. Default is \code{FALSE}.
#' @param interval.alpha Numerical value of alpha for the creation of confidence intervals of predictions. Default value is \emph{0.05}.
#' @param parallel Logical indicating whether the evaluation process of different forms must be done in parallel mode or not. Default is \code{FALSE}.
#' @param core.nums Number of cores to be used in parallelization process. The default is \code{NULL} where automatically half of CPU cores will be used unless user specifies its value.
#'
#' @return A comprehensive list containing information of the whole evaluation process. By default, autoGAM shows final best forms of continuous predictors and the final set of categorical variables but they are part of a bigger list. You can access all items of the list via:
#'
#' \enumerate{
#'
#' \item \code{$data}: Dataset that was used to create different models in the evaluation process.
#'
#' \item \code{$`forms info`}: A nested data frame including full information of evaluation process. It includes values and predictions for all form(s) on all predictor(s).
#'
#' \item \code{$`best forms`}: Final best form of continuous predictors that were obtained from the evaluation process.
#'
#' \item \code{$`final predictors`}: Final predictors (best form of continuous predictors and categorical predictors) that are included in the best GAM model. If the backward argument was set to FALSE, best forms of continuous predictors (and possibly the categorical variables) are returned.
#'
#' \item \code{$`response family`}: The family of the model's response. This item is for internal use!
#'
#' }
#'
#' @author Shahin Roshani
#'
#' @examples \dontrun{ autoGAM_frame(mpg~disp+drat+vs,data=mtcars %>% mutate_at('vs',as.factor)) }
#'
#' @seealso \url{https://shahin-roshani.github.io/autoGAM/articles/autoGAM.html}
#'
#' @export

autoGAM_frame <- function(formula,

                          resp.base=NULL,

                          forms=list('identity',

                                     'logb',

                                     'exp',

                                     'power'=2:3,

                                     'poly'=2:3),

                          data,

                          ignore.outliers=F,

                          family=gaussian(link='identity'),

                          metric='AIC',

                          raw.poly=F,

                          interval.alpha=.05,

                          parallel=F,

                          core.nums=NULL){


  if (!(metric %in% c('AIC','BIC','AICc'))){

    stop('metric must be AIC, BIC or AICc!',call.=F)

  }


  if (class(formula)!='formula'){

    stop('Please specify a formula with correct format!',call.=F)

  }

  deparsed_formula <- deparse(formula) %>% str_split('\\~') %>% .[[1]] %>%

    str_remove_all(' ')

  resp <- deparsed_formula[1]

  if (str_detect(resp,'(\\+|\\*|\\/|\\-)') | resp==''){

    stop('Please specify a single variable as response!',call.=F)

  }

  preds <- deparsed_formula[2] %>% str_split('\\+') %>% .[[1]]

  if (length(preds)==1 && preds=='.'){

    preds <- data %>% select(-any_of(resp)) %>% names

  }


  preds_data <- data %>% select(-any_of(resp)) %>% select(all_of(preds))

  cont.vars <- preds_data %>% select(where(is.numeric)) %>% names

  cat.vars <- names(preds_data)[!(names(preds_data) %in% cont.vars)]


  if (!is_empty(cat.vars) & is_empty(cont.vars)){

    stop('Are you sure GAM is the model you are looking for? Looks like direct use of GLM is what you are looking for!',call.=F)

  }


  data %<>% select(all_of(c(resp,cont.vars,cat.vars))) %>%

    rownames_to_column() %>% as_tibble %>% column_to_rownames('rowname')


  if (family$family=='binomial' & !is.factor(data[[resp]])){

    data[[resp]] %<>% as.factor(.)

  }


  if (!is.null(resp.base)){

    data[[resp]] %<>% fct_relevel(.,resp.base,after=0)

  }


  autoGAM_frame_res <- list()

  autoGAM_frame_res$data <- data %>% as_tibble


  singles <- c('identity','exp','log2','log10','logb')

  if (any(is_empty(names(forms)))){

    forms %<>% list(.,'func_def_NA'=NA) %>% unlist(recursive=F)

  }

  if (any(names(forms)=='')){

    names(forms)[which(names(forms)=='')] <-

      forms[forms %>% map_lgl(is.character)] %>% unlist

    forms[forms %>% map_lgl(is.character)] <- NA

  }

  if ('func_def_NA' %in% names(forms)){

    forms %<>% .[-which(names(.)=='func_def_NA')]

  }

  n_sets <- forms

  n_sets[n_sets %>% map_lgl(is.null)] <- NA

  forms <- names(forms)


  defined_singles <- n_sets[is.na(n_sets)==T] %>% names

  singles <- c(singles,defined_singles) %>% unique


  f <- function(resp,my_x,var_name,label,data,

                family,ignore.outliers){

    base_fit <- glm(str_c(resp,'~my_x') %>% as.formula,

                    data=data %>% mutate(my_x=my_x) %>%

                      (function(x){

                        row.names(x) <- NULL

                        return(x)

                      }),family=family)

    dat <- data %>% mutate(my_x=my_x)

    fits_mat <- predict(base_fit,se.fit=T,type='response') %>%

      as.data.frame %>% select(1:2) %>%

      rename_at(1,function(x) 'yhat') %>%

      mutate(yhat_lwr=yhat-qnorm(1-interval.alpha/2)*se.fit,

             yhat_upr=yhat+qnorm(1-interval.alpha/2)*se.fit)


    metric_fun <- function(fit,metric){

      if (metric=='AICc'){

        metric <- 'AICcmodavg::AICc'

      }

      return(do.call(eval(parse(text=metric)),list(fit)))

    }


    if (ignore.outliers){

      res <- tibble(x=dat[[var_name]],

                    y=dat[[resp]]) %>% cbind(.,fits_mat) %>%

        nest(data=everything()) %>%

        mutate(var=var_name,

               form=label,

               metric=metric_fun(base_fit,metric))

    } else{

      if (is.matrix(my_x)){

        p <- ncol(my_x) + 1

      } else{

        p <- 2

      }

      outs <- c(car::outlierTest(base_fit) %>%

                  unlist %>% names %>%

                  str_subset('rstudent') %>%

                  str_remove('rstudent.') %>% as.numeric,

                hatvalues(base_fit) %>%

                  .[.>(2*p)/nrow(data)] %>% names %>%

                  as.numeric) %>% na.omit %>% as.vector %>%

        unique %>% sort

      dat %<>% slice(-outs)

      fits_mat <- predict(update(base_fit,data=dat),se.fit=T,

                          type='response') %>%

        as.data.frame %>% select(1:2) %>%

        rename_at(1,function(x) 'yhat') %>%

        mutate(yhat_lwr=yhat-qnorm(1-interval.alpha/2)*se.fit,

               yhat_upr=yhat+qnorm(1-interval.alpha/2)*se.fit)

      res <- tibble(x=dat[[var_name]],

                    y=dat[[resp]]) %>% cbind(.,fits_mat) %>%

        nest(data=everything()) %>%

        mutate(var=var_name,

               form=label,

               metric=metric_fun(update(base_fit,data=dat),metric))

    }

    return(res)

  }


  g <- function(var_name,form,n_set,data){

    power <- function(x,y) x^y

    x <- data[[var_name]]

    if (form %in% singles){

      base_form <- str_c(form,'(x)')

      labs <- str_c(form,'(',var_name,')')

    } else{

      if (form=='poly'){

        base_form <- str_c(form,'(','x',',',n_set,',',

                           'raw=',raw.poly,')')

        labs <- str_c(form,'(',var_name,',',n_set,',','raw=',

                      raw.poly,')')

      } else{

        base_form <- str_c(form,'(','x',',',n_set,')')

        labs <- str_c(form,'(',var_name,',',n_set,')')

      }

    }

    if (form=='power'){

      labs <- str_c('I(',var_name,'^',n_set,')')

    }

    base_form %>% as.list %>%

      map(~str2lang(.) %>% eval) -> list_objects

    return(

      tibble(var_name=var_name,my_x=list_objects,label=labs)

    )

  }



  expand.grid(var_name=cont.vars,form=forms) %>%

    left_join(tibble(form=forms,n_set=n_sets)) %>%

    suppressMessages %>% mutate_if(is.factor,as.character) -> combs


  pkg_envs <- c('tidyverse','magrittr','fastDummies','splines',

                'gam','car','AICcmodavg')


  start <- Sys.time()


  if(!parallel){

    preds_combs <- pmap(combs,~g(..1,..2,..3,data)) %>% reduce(rbind)

  } else{

    if (is.null(core.nums)){

      core.nums <- future::availableCores()/2

    }

    future::plan(future::multisession(),workers=core.nums)

    preds_combs <- furrr::future_pmap(combs,~g(..1,..2,..3,data),

                                      .options=furrr::furrr_options(packages=pkg_envs,globals=forms)) %>%

      reduce(rbind)

  }


  preds_combs %<>%

    mutate(check=my_x %>% map_lgl(function(x){

      any(x %in% c(NaN,Inf,-Inf))

    })) %>% split(.$check)


  if (!is_empty(preds_combs$`TRUE`)){

    warning(str_c('autoGAM removed ',

                  str_c(preds_combs$`TRUE`$label,collapse=', '),

                  ' due to the inclusion of NaN, Inf or -Inf.'),

            call.=F)

  }

  if (!parallel){

    pmap(.l=preds_combs$`FALSE`,.f=~f(resp=resp,

                                      my_x=..2,

                                      var_name=..1,

                                      label=..3,

                                      data=data,

                                      family=family,

                                      ignore.outliers=ignore.outliers

    )) %>% reduce(rbind) %>% relocate(data,.before=metric) -> info

  } else{

    furrr::future_pmap(.l=preds_combs$`FALSE`,

                       .f=~f(resp=resp,

                             my_x=..2,

                             var_name=..1,

                             label=..3,

                             data=data,

                             family=family,

                             ignore.outliers=ignore.outliers),

                       .options=furrr::furrr_options(packages=pkg_envs,globals=forms)) %>%

      reduce(rbind) %>% relocate(data,.before=metric) -> info

  }

  end <- Sys.time()


  cat('\nForms evaluation was done in:',(end-start) %>% as.character.POSIXt %>% str_c(.,'.\n\n'))


  if ('poly' %in% forms){

    info %<>% mutate_at('form',~str_replace(.,'TRUE','T') %>%

                          str_replace(.,'FALSE','F'))

  }


  best_forms <- info %>% split(.$var) %>%

    map(~filter(.,metric==min(metric)) %>% .$form) %>%

    map(~.[1]) %>% unlist %>% as.vector


  autoGAM_frame_res$`forms info` <- info %>%

    relocate(metric,.after=length(.)) %>%

    rename_at('metric',function(x) metric)


  autoGAM_frame_res$`best forms` <- best_forms


  autoGAM_frame_res$`final predictors` <- c(best_forms,cat.vars)


  autoGAM_frame_res$`response family` <- family


  class(autoGAM_frame_res) <- 'autoGAM_frame'

  print.autoGAM_frame <<- function(x){

    if ('final fit' %in% names(x)){

      print(x$`final fit` %>% summary)

    } else{

      print(x[c('best forms','final predictors')])

    }

  }

  return(autoGAM_frame_res)

}
