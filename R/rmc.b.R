
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import rmcorr
#' @importFrom rmcorr rmcorr
#' @import ggplot2
#' @export

rmcClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "rmcClass",
    inherit = rmcBase,
    private = list(
      .init = function() {
        
        if (is.null(self$data) |is.null(self$options$dep) | 
            is.null(self$options$dep)  | is.null(self$options$covs)) {
          
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(
          "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p>____________________________________________________________________________________</p>
            <p>1. The rationale of Repeated measures correlation is described in the <a href='https://r-bloggers.com/2020/01/concordance-correlation-coefficient/?fbclid=IwAR2Txi_QrFTuDB9jH8NiJW8dEde_lw2Td08XqxNzoWqut9m8E-bE5RHUDiI' target = '_blank'>page.</a></p>
            <p>2. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/seolmatrix/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
            </div>
            </body>
            </html>"
        )
        
        
        # if(isTRUE(self$options$ccp)){
        #   width <- self$options$width
        #   height <- self$options$height
        #   self$results$plot$setSize(width, height)
        # }
        # 
        # if(isTRUE(self$options$bap)){
        #   width <- self$options$width1
        #   height <- self$options$height1
        #   self$results$plot1$setSize(width, height)
        # }  
        
      },
      
#---------------------------------------------------------------      
  .run = function() {

    
    if (is.null(self$options$id) | 
        is.null(self$options$dep) | 
        is.null(self$options$covs)) return()
 
    # Example-----------
    # library(rmcorr)
    # bland1995 <- bland1995
    # ## Bland Altman 1995 data
    # rc<- rmcorr(Subject, PaCO2, pH, bland1995)
    
    
    #get the data--------
  
    data <- self$data
    
    id <- self$options$id
    dep <- self$options$dep
    covs <- self$options$covs

 
    # convert to appropriate data types
    
    data[[dep]] <- jmvcore::toNumeric(data[[dep]])
    data[[covs]] <- jmvcore::toNumeric(data[[covs]])
    
    
    data <- na.omit(data)
    
# Model: Correlation coefficient-----------------------------------------------------    
  
    cc<- rmcorr::rmcorr(id, data[[dep]], data[[covs]], data)
#-----------------------------------------          
    #self$results$text$setContent(cc)    
        
    if(isTRUE(self$options$cc)){
      
      table<- self$results$cc
      
      r <-  cc$r
      df <- cc$df
      p <- cc$p
      lower <-  cc$CI[[1]]
      upper<-   cc$CI[[2]]
      
      row <- list()
      
      row[['r']] <- r
      row[['df']] <- df
      row[['p']] <- p
      row[['lower']] <- lower
      row[['upper']] <- upper
      
      table$setRow(rowNo = 1, values = row)
      
    }
    
      
          
        })
)
