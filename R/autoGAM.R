
#' autoGAM
#'
#' @import tidyverse magrittr gam splines fastDummies recipes car
#'
#' @description Automatic evaluation of different forms (functions) of predictors to obtain the best Generalized Additive Model (GAM).
#'
#' @param resp Name of the response variable.
#' @param resp.base Base level of the binary response variable, default is NULL.
#' @param cont.vars Vector containing names of continuous variables, default is c(). Also see data argument.
#' @param cat.vars Vector containing names of categorical variables, default is c(). Also see data argument.
#' @param forms A named list of continuous predictor(s) form(s) with their respected degree(s) or degree(s) of freedom. Famous functions that one can think of are identity, log, log2, logb, exp, bs & ns (from splines package), s (from gam package) and cut,default is list('identity','logb','exp','power'=2:3,'poly'=2:3).
#' The names of the list are the function(s) names and their respected value(s) are their degree(s)/degree(s) of freedom. Only functions with the form: f(x) or f(x,degree) with a vector or a matrix as their output should be included in this list.
#' Note: Functions with the f(x) form can be passed in 2 ways: 1. 'f'=c() or 2. 'f'. Any function of the form f(vector,degree) should be passed as 'f'=desired degree(s).
#' If a function of the form f(vector,degree) is passed as a single character, the default value for degree will be used and if there's no default value for degree, an error will occur.
#' @param data Dataset containing the response and all the predictors.
#' Note: When the cont.vars & cat.vars arguments are BOTH left empty, variables and their respected types that are given in the dataset will be automatically used to distinguish continuous and categorical variables that are available in the whole dataset.
#' If only one of two cont.vars & cat.vars was left empty, remaining variables in the dataset other than the filled argument (either cont.vars or cat.vars) and the response will be discarded. It is recommended to let autoGAM decide only when your data object only consists of your desired response & predictors with right types.
#' @param ignore.outliers Logical indicating whether outliers should be ignored during the evaluation process of predictors forms or not, default is FALSE. When TRUE, outliers in case of response are detected by car::outlierTest which is based on studentized residuals of records and outliers in case of the predictor are determined as records that have hat-values > 2p (where p is the number of parameters inside the model).
#' @param family Family for the response variable in GLM fits, default is gaussian(link='identity').
#' @param raw.poly Logical indicating whether raw forms of polynomials should be included when polynomial forms ('poly') are being evaluated, default is FALSE.
#' @param forms.plot Logical indicating whether plots for all evaluated forms on all continuous predictors should be included in the output or not, default is FALSE.
#' @param backward Logical indicating whether backward elimination should be done on the obtained best forms of predictors, default is TRUE.
#' @param backward.test Name of the test to be used in the backward elimination process on GLM fits. Valid inputs are 'LRT', 'Rao', 'Chisq' and 'F', default is 'LRT'.
#' @param backward.alpha Numerical value of alpha for the backward elimination process, default is 0.05.
#' @param interval.alpha Numerical value of alpha for the creation of confidence intervals of predictions.
#'
#' @return A comprehensive list containing information of the whole evaluation process, backward elimination process and the final GAM model. Result includes:
#'
#' 1- $`Forms data`: A nested data frame including full information of evaluation process. It includes values and predictions for all form(s) on all predictor(s).
#'
#' 2- $`Forms plot(s)`: Plots of the dataset in $`Forms data` (Included if argument: forms.plot=TRUE).
#'
#' 3- $`Backward info`: Step by step list of information regarding the backward elimination process (elimination alpha = backward.alpha) (Included if argument: backward=TRUE).
#'
#' 4- $`Final predictors`: Final predictors (best form of continuous predictors and categorical predictors) that are included in the best GAM model. If the backward argument was set to FALSE, best forms of continuous predictors (and possibly the categorical variables) are returned.
#'
#' 5- $`Best forms plot(s)`: Plots with corresponding confidence intervals (alpha = interval.alpha) of predictions for final best model of each continuous predictor.
#'
#' 6- $`Final GAM`: Final fitted GAM based on final predictors in the $`Final predictors` part.
#'
#' @author Shahin Roshani
#'
#' @examples autoGAM(resp='mpg',cont.vars=names(mtcars)[3:6],cat.vars=names(mtcars)[8],
#' data=mtcars,forms.plot=TRUE)
#'
#' @export

autoGAM <- function(resp,resp.base=NULL,

                    cont.vars=c(),cat.vars=c(),

                    forms=list('identity',

                               'logb',

                               'exp',

                               'power'=2:3,

                               'poly'=2:3),

                    data,ignore.outliers=F,

                    family=gaussian(link='identity'),

                    raw.poly=F,forms.plot=F,

                    backward=T,

                    backward.test='LRT',

                    backward.alpha=.05,

                    interval.alpha=.05){


  if (is_empty(c(cont.vars,cat.vars))){

    preds_data <- data %>% select(-all_of(resp))

    cont.vars <- preds_data %>% select(where(is.numeric)) %>% names

    cat.vars <- names(preds_data)[!(names(preds_data) %in% cont.vars)]

  }


  if (!is_empty(cat.vars) & is_empty(cont.vars)){

    stop('Are you sure GAM is the model you are looking for? Looks like direct use of GLM is what you are looking for!',call.=F)

  }


  if (!(backward.test %in% c('LRT','Rao','Chisq','F'))){

    stop('Valid tests for GLMs are LRT, Rao, Chisq or F.',call.=F)

  }


  data %<>% select(one_of(c(resp,cont.vars,cat.vars))) %>%

    rownames_to_column() %>%

    mutate_at(cat.vars,function(x){

      if (!is.factor(x) & !is.character(x)){

        x %<>% as.factor(.)

      }

      return(x)

    }) %>% as_tibble %>% column_to_rownames('rowname')


  if (family$family=='binomial' & !is.factor(data[[resp]])){

    data[[resp]] %<>% as.factor(.)

  }


  if (!is.null(resp.base)){

    data[[resp]] %<>% fct_relevel(.,resp.base,after=0)

  }


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

    if (ignore.outliers){

      res <- tibble(x=dat[[var_name]],

                    y=dat[[resp]]) %>% cbind(.,fits_mat) %>%

        nest(data=everything()) %>%

        mutate(var=var_name,

               form=label,

               aic=summary(base_fit)$aic)

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

               aic=summary(update(base_fit,data=dat))$aic)

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

    suppressMessages %>%

    mutate_if(is.factor,as.character) -> combs


  preds_combs <- pmap(combs,~g(..1,..2,..3,data)) %>% reduce(rbind)

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


  pmap(.l=preds_combs$`FALSE`,.f=~f(resp=resp,

                                    my_x=..2,

                                    var_name=..1,

                                    label=..3,

                                    data=data,

                                    family=family,

                                    ignore.outliers=ignore.outliers

  )) %>% reduce(rbind) %>%

    relocate(data,.before=aic) -> info


  if ('poly' %in% forms){

    info %<>% mutate_at('form',~str_replace(.,'TRUE','T') %>%

                          str_replace(.,'FALSE','F'))

  }


  results <- list()

  results$`Forms data` <- info


  info_plot_data <- info

  info_plot_data$form %<>%

    str_replace_all(.,str_c('(',str_c(info$var %>% unique,

                                      collapse='|'),')'),'x')

  if ('poly' %in% forms){

    info_plot_data$form %<>%

      str_replace(.,'TRUE','T') %>% str_replace(.,'FALSE','F')

  }


  info_plot_data %<>% as_tibble %>% rename('f(x):'='form')


  cat_format <- str_c('P(',resp,'=',levels(data[[resp]])[2],')')


  if (forms.plot){

    ggplot() +

      geom_point(aes(x=x,y=y),color='grey55',size=2,

                 data=data %>% select(one_of(c(resp,cont.vars))) %>%

                   gather(key='var',value='x',-1) %>%

                   rename_at(1,function(x) 'y') %>%

                   (function(x){

                     if (!is.numeric(x$y)){

                       x <- dummy_cols(x,'y',

                                       remove_first_dummy=T,

                                       ignore_na=T,

                                       remove_selected_columns=T) %>%

                         rename_at(length(.),function(x) 'y')

                     }

                     return(x)

                   })

      ) +

      geom_line(aes(x=x,y=yhat,color=`f(x):`),size=.75,

                data=info_plot_data %>% select(-aic) %>%

                  unnest(cols=data)) +

      facet_wrap(~var,scales='free') + theme_minimal() +

      theme(panel.grid=element_line(color='grey85'),

            text=element_text(family='serif'),

            panel.spacing=unit(.8,'cm')) +

      labs(x='',

           y=ifelse(is.numeric(data[[resp]]),

                    resp,cat_format)) -> results$`Forms plot(s)`

  }


  info %>% split(.$var) %>%

    map(~filter(.,aic==min(aic)) %>% .$form) %>%

    map(~.[1]) %>% unlist %>% as.vector -> best_forms


  if (backward){

    backward <- function(resp,preds,data,family=family,

                         test=backward.test,

                         alpha=backward.alpha){

      form <- str_c(resp,'~',str_c(preds,collapse='+')) %>%

        as.formula

      glm(form,data=data,family=family) -> fit

      x <- drop1(fit,test=test) %>% rownames_to_column() %>% as_tibble

      y <- x %>% na.omit

      l = v <- list() ; l[[1]] <- y ; i <- 1 ; j <- 0

      while (any(y[[6]]>alpha)){

        i <- i + 1 ; j <- j + 1

        v[[j]] <- y %>% filter(.[[6]]==max(.[[6]])) %>%

          .$rowname

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

      return(l)

    }

    backward_info <- backward(resp=resp,

                              preds=c(best_forms,cat.vars),

                              data=data,

                              family=family)

    results$`Backward info` <- backward_info

    final_preds <-

      backward_info[[length(backward_info)]] %>% rownames

  } else{

    final_preds <- c(best_forms,cat.vars)

  }

  results$`Final predictors` <- final_preds


  ggplot(data=info %>% split(.$var) %>%

           map(~filter(.,aic==min(aic))) %>% reduce(rbind) %>%

           filter(form %in% (final_preds %>% str_remove_all(' '))) %>%

           unnest(cols=data) %>%

           mutate(var=str_c(form,': AIC=',round(aic,4))) %>%

           (function(x){

             if (!is.numeric(x$y)){

               x <- dummy_cols(x,'y',

                               remove_first_dummy=T,

                               ignore_na=T,

                               remove_selected_columns=T) %>%

                 rename_at(length(.),function(x) 'y')

             }

             return(x)

           })

  ) +

    geom_point(aes(x=x,y=y),color='grey50',size=2) +

    geom_line(aes(x=x,y=yhat),size=.75,color='steelblue3') +

    geom_ribbon(aes(x=x,y=yhat,ymin=yhat_lwr,ymax=yhat_upr),

                size=.75,fill='steelblue3',alpha=.35) +

    facet_wrap(~var,scales='free') + theme_minimal() +

    theme(panel.grid=element_line(color='grey85'),

          text=element_text(family='serif'),

          panel.spacing=unit(.8,'cm')) +

    labs(x='',

         y=ifelse(is.numeric(data[[resp]]),

                  resp,cat_format)) -> results$`Best forms plot(s)`



  final_model <- glm(str_c(resp,'~',

                           str_c(final_preds,collapse='+')) %>%

                       as.formula,data=data,family=family)

  results$`Final GAM` <- final_model


  return(results)

}

