# This file is a generated template, your changes will not be overwritten


#' Tetrachoric Analysis
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import psych
#' @importFrom psych tetrachoric
#' @importFrom psych partial.r
#' @importFrom qgraph EBICglasso
#' @importFrom qgraph centralityPlot
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
            <p>____________________________________________________________________________________</p>
            <p>Welcome to Tetrachoric Correlation for doing factor analysis as an input data.</p>
            <p>Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/seolmatrix/issues'  target = '_blank'>GitHub</a>.</p>
            <p>____________________________________________________________________________________</p>
            </div>
            </body>
            </html>"
        )


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
        
       
# compute tetrachoric correlation with psych package--------
        
        tetrarho <- psych::tetrachoric(mydata)$rho
        
        tetrarho <- as.data.frame(tetrarho)
        
        
        names <- dimnames(tetrarho)[[1]] 
        dims <- dimnames(tetrarho)[[2]]
        
        
        table <- self$results$matrix
        
        # creating table----------------
        
        for (dim in dims) {
          
          table$addColumn(name = paste0(dim),
                          type = 'number')
        }
        
        for (name in names) {
          
          row <- list()
          
          for(j in seq_along(dims)){
            
            row[[dims[j]]] <- tetrarho[name,j]
            
          }
          
          table$addRow(rowKey=name, values=row)
          
        }
        
        ### partial correlation--------
        
        res<- psych::partial.r(tetrarho)
        
        # Prepare Data For Plot -------
        image1 <- self$results$plot1
        image1$setState(res)
        
        if(self$options$plot | self$options$plot2==TRUE){
      
        # EBIC PLOT------------
        
        tetrarho <- psych::tetrachoric(mydata)$rho
        
        # Compute graph with tuning = 0.5 (EBIC)
        EBICgraph <- qgraph::EBICglasso(tetrarho,nrow(mydata), 0.5, threshold = TRUE)
        
        # Prepare Data For Plot -------
        image <- self$results$plot
        image$setState(EBICgraph)
        
        
        # Centrality plot-------
        image2 <- self$results$plot2
        image2$setState(EBICgraph)
        
        }
        }
        },
      
#================================================================
.plot = function(image, ...) {
  
  if (is.null(image$state))
    return(FALSE)
  
 
  EBICgraph <- image$state
  
  plot <- qgraph(EBICgraph, layout = "spring", details = TRUE)
  
  print(plot)
  TRUE
  
},


# Centrality plot for EBIC------------

.plot2 = function(image2, ggtheme, theme,...) {
  
  if (is.null(image2$state))
    return(FALSE)
  
  scale <- self$options$scale
  
  EBICgraph <- image2$state
  
  plot2<- qgraph::centralityPlot(EBIC = EBICgraph,
                                 scale=scale)
  
  plot2 <- plot2+ggtheme
  
  print(plot2)
  TRUE
  
},



# partial plot-------------


      .plot1 = function(image1, ...) {
        
       
        if (is.null(image1$state))
          return(FALSE)
        
        
        res <- image1$state
        
        plot1 <- qgraph(res, layout = "spring", details = TRUE)
        
        print(plot1)
        TRUE
        
      }
    ))
