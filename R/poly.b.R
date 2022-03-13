
# This file is a generated template, your changes will not be overwritten

#' @importFrom R6 R6Class
#' @import jmvcore
#' @import psych
#' @importFrom psych polychoric
#' @importFrom psych partial.r
#' @importFrom qgraph EBICglasso
#' @import qgraph
#' @export



polyClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "polyClass",
    inherit = polyBase,
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
            <p><b>Instructions</b></p>
            <p>____________________________________________________________________________________</p>
            <p>The polychoric correlation coefficient is a measure of association for ordinal variables.</p>
            <p>Tetrachoric correlation is a special case of the polychoric correlation applicable when both variables are dichotomous.</p>
            <p>____________________________________________________________________________________</p>
            </div>
            </body>
            </html>"
            )
            
            
        },
        
      
        .run = function() {

            if (length(self$options$vars)<2) return() 
            
            
            if(length(self$options$vars>2)){
                
                # get variables---------------------------------
                
                
                vars <- self$options$vars
                nVars <- length(vars)
                
                
                mydata <- self$data
                
                mydata <- jmvcore::naOmit(mydata)
                
                for(v in vars)
                    mydata[[v]] <- jmvcore::toNumeric(mydata[[v]])
                
                # compute polychoric correlation-------
                
                poly <- psych::polychoric(mydata)$rho
                
                
                poly<- as.data.frame(poly)
                
                
                names <- dimnames(poly)[[1]] 
                dims <- dimnames(poly)[[2]]
                
                
                table <- self$results$matrix
                
                # creating table----------------
                
                for (dim in dims) {
                  
                  table$addColumn(name = paste0(dim),
                                  type = 'number')
                }
                
                for (name in names) {
                  
                  row <- list()
                  
                  for(j in seq_along(dims)){
                    
                    row[[dims[j]]] <- poly[name,j]
                    
                  }
                  
                  table$addRow(rowKey=name, values=row)
                  
                }
            
               # paritial----------------------------------------------
                
                res<- psych::partial.r(poly)
                
                # Prepare Data For Plot -------
                image <- self$results$plot1
                image$setState(res)
                
                
                # EBIC PLOT------------
                
                poly <- psych::polychoric(mydata)$rho
                
                # Compute graph with tuning = 0.5 (EBIC)
                EBICgraph <- qgraph::EBICglasso(poly,nrow(mydata), 0.5, threshold = TRUE)
                
                # Prepare Data For Plot -------
                image <- self$results$plot
                image$setState(EBICgraph)
                
                      }
                  },
        
        #================================================================
        .plot = function(image, ...) {
            ggm <- self$options$ggm
            
            if (!ggm)
                return()
            
            
            EBICgraph <- image$state
            
            plot <- qgraph(EBICgraph, layout = "spring", details = TRUE)
            
            print(plot)
            TRUE
            
        },
        
        # partial plot-------------
        
        
        .plot1 = function(image, ...) {
            par <- self$options$par
            
            if (!par)
                return()
            
            
            res <- image$state
            
            plot1 <- qgraph(res, layout = "spring", details = TRUE)
            
            print(plot1)
            TRUE
            
        }
    ))

            
            
            
            
            
           
        

