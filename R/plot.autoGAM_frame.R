
#' @title plot.autoGAM_frame
#'
#' @import tidyverse magrittr gam splines fastDummies car future furrr AICcmodavg
#'
#' @description Plot method for an object of class \code{autoGAM_frame}.
#'
#' @param x An object of class \code{autoGAM_frame}.
#' @param type The type of the plot to be created. Valid inputs are \code{'forms'} that returns the whole plots of evaluated continuous predictors & \code{'final'} (default value) that returns the plots related to the best forms obtained from the evaluation process.
#' @param ... Other plot arguments (Not being used for objects of class \code{'forms'}).
#'
#' @return Desired plots that were requested by the \code{type} argument.
#'
#' @author Shahin Roshani
#'
#' @examples \dontrun{
#'
#' my_mtcars <- mtcars %>% mutate_at('vs',as.factor)
#'
#' carsGAM <- autoGAM_frame(mpg~disp+drat+vs,data=my_mtcars) %>% backward_select %>% autoGAM_fit
#'
#' plot(carsGAM,type='forms')
#'
#' plot(carsGAM)
#'
#' }
#'
#' @seealso \url{https://shahin-roshani.github.io/autoGAM/articles/autoGAM.html}
#'
#' @export

plot.autoGAM_frame <- function(x,type='final',...){


  if (!(type %in% c('forms','final'))){

    stop('type must be either forms or final!',call.=F)

  }

  object <- x

  info <- object$`forms info`

  data <- object$data

  resp <- names(data)[1]

  cat_format <- str_c('P(',resp,'=',levels(data[[resp]])[2],')')

  metric <- names(object$`forms info`) %>% .[length(.)]


  if (type=='forms'){

    info_plot_data <- info

    info_plot_data$form %<>%

      str_replace_all(.,str_c('(',str_c(info$var %>% unique,

                                        collapse='|'),')'),'x')

    forms <- object$`forms info`$form %>% unique

    if ('poly' %in% forms){

      info_plot_data$form %<>%

        str_replace(.,'TRUE','T') %>% str_replace(.,'FALSE','F')

    }

    info_plot_data %<>% as_tibble %>% rename('f(x):'='form')

    cont.vars <- data %>% select(-any_of(resp)) %>%

      select(where(is.numeric)) %>% names


    plot_output <- ggplot() +

      geom_point(aes(x=x,y=y),color='grey55',size=2,

                 data=data %>% select(all_of(c(resp,cont.vars))) %>%

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

                data=info_plot_data %>% select(-any_of(metric)) %>%

                  unnest(cols=data)) +

      facet_wrap(~var,scales='free') + theme_minimal() +

      theme(panel.grid=element_line(color='grey85'),

            text=element_text(family='serif'),

            panel.spacing=unit(.8,'cm')) +

      labs(x='',y=ifelse(is.numeric(data[[resp]]),resp,cat_format))

  } else{

    final_preds <- object$`final predictors`

    plot_output <- ggplot(data=info %>% split(.$var) %>%

                            map(~filter(.,get(metric)==min(get(metric)))) %>%

                            reduce(rbind) %>%

                            filter(form %in% (final_preds %>% str_remove_all(' '))) %>%

                            unnest(cols=data) %>%

                            mutate(var=str_c(form,': ',metric,'=',

                                             round(get(metric),4))) %>%

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

      labs(x='',y=ifelse(is.numeric(data[[resp]]),resp,cat_format))

  }

  return(plot_output)

}
