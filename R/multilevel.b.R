
# This file is a generated template, your changes will not be overwritten

#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom correlation correlation
#' @import correlation
#' @import ggplot2
#' @export


multilevelClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "multilevelClass",
    inherit = multilevelBase,
    private = list(
        
        #------------------------
        
        .init = function() {
            if (is.null(self$data) | is.null(self$options$vars)) {
                self$results$instructions$setVisible(visible = TRUE)
                
            }
            
            self$results$instructions$setContent(
                "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            
            <p> The rationale of Multilevel Correlation is described in the <a href='https://cran.r-project.org/web/packages/correlation/vignettes/multilevel.html' target = '_blank'>page.</a></p>
            <p> Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/seolmatrix/'  target = '_blank'>GitHub.</a></p>
            </div>
            </body>
            </html>"
            )
            
        },
        
        
        
        #---------------------------------
        
        .run = function() {

            # data <- data(iris)
            # res<- correlation::correlation(data, multilevel = TRUE)
            
            
            if(length(self$options$vars>2)){
            
            # read the option values into shorter variable names--
            
            vars  <- self$options$vars
            facs <- self$options$facs
            
            # get the data------
            
            data <- self$data
            
            
            # convert to appropriate data types
            
            for (i in seq_along(vars))
                data[[i]] <- jmvcore::toNumeric(data[[i]])
            
            
            #  data[[vars]] <- jmvcore::toNumeric(data[[vars]])
            
            for (fac in facs)
                data[[fac]] <- as.factor(data[[fac]])
            
            # data is now all of the appropriate type we can begin!
            
            data <- na.omit(data)
            
           # data <- jmvcore::select(data, self$options$vars)
            
            # multilevel correlation analysis---------
            
            res<- correlation::correlation(data, multilevel = TRUE)
            
            # get variables-------
            
            v1 <- res$Parameter1
            v2 <- res$Parameter2
            r <- res$r
            low <- res$CI_low
            high <- res$CI_high
            t <- res$t
            df <- res$df
            p <- res$p
            n <- res$n_Obs
            
            results <-
                list(
                    'v1' = v1,
                    'v2' = v2,
                    'r' = r,
                    'low' = low,
                    'high' = high,
                    't' = t,
                    'df' = df,
                    'p' = p,
                    'n' = n
                )
                   
            # populate multilevel correlation---
            
            table <- self$results$multi
            
            nrows <- ncol(combn(self$options$vars, 2))
            
            # for (i in seq_len(nrows)) {
            #     # add the rows in here
            # }
            
            
            for (i in seq_len(nrows)) {
            
            row <- list()
            
            row[['v1']] <- v1[i]
            row[['v2']] <- v2[i]
            row[['r']] <- r[i]
            row[['low']] <- low[i]
            row[['high']] <- high[i]
            row[['t']] <- t[i]
            row[['df']] <- df[i]
            row[['p']] <- p[i]
            row[['n']] <- n[i]
            
            table$addRow(rowKey =i, values = row)
            }
            
            }  
            
        })
)
