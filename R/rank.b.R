# This file is a generated template, your changes will not be overwritten


#' Spearman Analysis
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import psych
#' @importFrom psych corr.test
#' @importFrom psych partial.r
#' @importFrom qgraph EBICglasso
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
        },
            
        
        #==========================================================
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
            
            #  compute spearman correlation with psych package--------
             
            spear <- psych::corr.test(mydata, method="spearman")
            spearman <- spear$r
                
            spearman<- as.data.frame(spearman)
           
            
            names <- dimnames(spearman)[[1]] 
            dims <- dimnames(spearman)[[2]]
            
           
            table <- self$results$matrix
            
            # creating table----------------
            
            for (dim in dims) {
              
              table$addColumn(name = paste0(dim),
                              type = 'number')
            }
            
            for (name in names) {
              
              row <- list()
              
              for(j in seq_along(dims)){
                
                row[[dims[j]]] <- spearman[name,j]
                
              }
              
              table$addRow(rowKey=name, values=row)
              
            }
          }
            
           
           res<- psych::partial.r(spearman)
            
            # Prepare Data For Plot -------
            image <- self$results$plot1
            image$setState(res)
        
            
            ########## EBIC PLOT------------
            
            rank <- spear$r
            
            # Compute graph with tuning = 0.5 (EBIC)
            EBICgraph <- qgraph::EBICglasso(rank,nrow(mydata), 0.5, threshold = TRUE)
            
            # Prepare Data For Plot -------
            image <- self$results$plot
            image$setState(EBICgraph)
            
            
            
                
        },
        
        #================================================================
        .plot = function(image, ...) {
          # ggm <- self$options$ggm
          # 
          # if (!ggm)
          #   return()
          
          if (is.null(image$state))
            return(FALSE)
          
          EBICgraph <- image$state
          
          plot <- qgraph(EBICgraph, layout = "spring", details = TRUE)
          
          print(plot)
          TRUE
          
        },
        
        
        
        .plot1 = function(image, ...) {
            # par <- self$options$par
            # 
            # if (!par)
            #     return()
          if (is.null(image$state))
            return(FALSE)
            
            res <- image$state
            
            plot1 <- qgraph(res, layout = "spring", details = TRUE)
            
            print(plot1)
            TRUE
            
        }
    ))

        
        
        
        
        
        
        
        
        
      
