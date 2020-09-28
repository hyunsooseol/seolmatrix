# This file is a generated template, your changes will not be overwritten


#' Spearman Analysis
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import psych
#' @importFrom psych corr.test
#' @importFrom psych partial.r
#' @import qgraph
#' @export

# This file is a generated template, your changes will not be overwritten

rankClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "rankClass",
    inherit = rankBase,
    private = list(
        
        #==========================================================
        .init = function() {
            
            
            if(is.null(self$data) | is.null(self$options$vars)){
                self$results$instructions$setVisible(visible = TRUE)
                
            }
            
            self$results$instructions$setContent(
                "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p>Welcome to Spearman correlation for doing Gaussian graphical model</p>
            
            <p><b>To get started:</b></p>
            <p>- The input dataset require the measure type of <b>numeric-continuous</b> in jamovi.
            <p>- Just highlight the variables and click the arrow to move it across into the 'Variables' box.</p>
            
            <p>- Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/seolmatrix/'  target = '_blank'>GitHub</a></p>

            <p>If you have any questions, please e-mail me: snow@cau.ac.kr</a></p>
            </div>
            </body>
            </html>"
            )
            
            # get variables
            
            matrix <- self$results$get('matrix')
            vars <- self$options$get('vars')
            nVars <- length(vars)
            
            
            # add columns--------
            
            for (i in seq_along(vars)) {
                
                var <- vars[[i]]
                
                matrix$addColumn(
                    name = paste0(var, '[r]'),
                    title = var,
                    type = 'number',
                    format = 'zto'
                )
                
            }
            
            # empty cells above and put "-" in the main diagonal
            
            for (i in seq_along(vars)) {
                
                var <- vars[[i]]
                
                values <- list()
                
                for (j in seq(i, nVars)) {
                    
                    v <- vars[[j]]
                    
                    values[[paste0(v, '[r]')]]  <- ''
                    
                }
                values[[paste0(var, '[r]')]]  <- '\u2014'  
                matrix$setRow(rowKey = var, values)
                
            }
            
            if (length(self$options$vars) <= 1)
                self$setStatus('complete')
        },
        
        #==========================================================
        .run = function() {
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
            
            
            # get variables---------------------------------
            
            matrix <- self$results$get('matrix')
            vars <- self$options$get('vars')
            nVars <- length(vars)
            
            
            mydata <- self$data
            
            for(v in vars)
                mydata[[v]] <- jmvcore::toNumeric(mydata[[v]])
            
            
            # compute spearman correlation with psych package--------
            
           spear <- psych::corr.test(mydata, method="spearman")
           spearman <- spear$r
              
            # populate result----------------------------------------
            
            for (i in 2:nVars) {
                for (j in seq_len(i - 1)) {
                    values <- list()
                    
                    values[[paste0(vars[[j]], '[r]')]] <- spearman[i, j]
                    
                    matrix$setRow(rowNo = i, values)
                }
            }
            
            res<- psych::partial.r(spearman)
            
            # Prepare Data For Plot -------
            image <- self$results$plot
            image$setState(res)
            
        },
        
        #================================================================
        
        .plot = function(image, ...) {
            ggm <- self$options$ggm
            
            if (!ggm)
                return()
            
            
            res <- image$state
            
            plot <- qgraph(res, layout = "spring", details = TRUE)
            
            print(plot)
            TRUE
            
        }
    ))

        
        
        
        
        
        
        
        
        
      
