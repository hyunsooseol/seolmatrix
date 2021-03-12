# This file is a generated template, your changes will not be overwritten


#' Tetrachoric Analysis
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import psych
#' @importFrom psych tetrachoric
#' @importFrom psych partial.r
#' @importFrom qgraph EBICglasso
#' @import qgraph
#' @export

# This file is a generated template, your changes will not be overwritten

dichotomousClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "dichotomousClass",
    inherit = dichotomousBase,
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
            <p>Welcome to Tetrachoric Correlation for doing factor analysis as an input data.</p>
            <p>The input dataset require dichotomous data with the type of <b>numeric-continuous</b> in jamovi.</p>
            
            </div>
            </body>
            </html>"
        )


},
        
        # # get variables
        # 
        # matrix <- self$results$get('matrix')
        # vars <- self$options$get('vars')
        # nVars <- length(vars)
        # 
        
        # add columns--------
        # 
        # for (i in seq_along(vars)) {
        #   
        #    var <- vars[[i]]
        #   
        #   matrix$addColumn(
        #     name = paste0(var),
        #     title = var,
        #     type = 'number',
        #     format = 'zto'
        #   )
        #   
        # }
     
        # empty cells above and put "-" in the main diagonal
        
      #   for (i in seq_along(vars)) {
      #     
      #      var <- vars[[i]]
      #     
      #     values <- list()
      #     
      #     for (j in seq(i, nVars)) {
      #       
      #        v <- vars[[j]]
      #       
      #       values[[paste0(v)]]  <- ''
      #       
      #     }
      #    values[[paste0(var)]]  <- '\u2014'  
      #    matrix$setRow(rowKey = var, values)
      #     
      #   }
      #   
      #   if (length(self$options$vars) <= 1)
      #     self$setStatus('complete')
      # },
      
  
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
        
       
# compute tetrachoric correlation with psych package--------
        
        tetrarho <- psych::tetrachoric(mydata)$rho
        
        names <- dimnames(tetrarho)[[1]] 
        
        table <- self$results$matrix
        
        # add columns--------
        
        for (i in seq_along(vars)) {
          
          var <- vars[[i]]
          
          table$addColumn(
            name = paste0(var),
            title = var,
            type = 'number',
            format = 'zto'
          )
          
        }
        
        
        # populate result----------------------------------------
        
        for (name in names) {
          
          row <- list()
          
          
          for (j in seq_along(vars)) {
            
              var <- vars[[j]]
            
             row[[var]] <- tetrarho[name, j]
           
          }
          table$addRow(rowKey=name, values=row)
          
        }
        
        
        ### paritial correlation--------
        
        res<- psych::partial.r(tetrarho)
        
        # Prepare Data For Plot -------
        image <- self$results$plot1
        image$setState(res)
        
      
        # EBIC PLOT------------
        
        tetrarho <- psych::tetrachoric(mydata)$rho
        
        # Compute graph with tuning = 0.5 (EBIC)
        EBICgraph <- qgraph::EBICglasso(tetrarho,nrow(mydata), 0.5, threshold = TRUE)
        
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
