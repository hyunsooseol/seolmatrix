
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import rmcorr
#' @importFrom rmcorr rmcorr
#' @importFrom stats ccf
#' @import ggplot2
#' @export

rmcClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "rmcClass",
    inherit = rmcBase,
    private = list(
      .init = function() {
        
        if (is.null(self$data) |is.null(self$options$dep) | 
             is.null(self$options$covs)) {
          
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(
          "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p>____________________________________________________________________________________</p>
            <p>1. The rationale of Repeated measures correlation is described in the <a href='https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2017.00456/full' target = '_blank'>page.</a></p>
            <p>2. The rationale of Cross correlation is described in the <a href='https://www.r-bloggers.com/2021/08/how-to-calculate-cross-correlation-in-r/' target = '_blank'>page.</a></p>
            <p>3. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/seolmatrix/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
            </div>
            </body>
            </html>"
        )
        
        
        if(isTRUE(self$options$plot)){
          width <- self$options$width
          height <- self$options$height
          self$results$plot$setSize(width, height)
        }
       
        if(isTRUE(self$options$plot1)){
          width <- self$options$width1
          height <- self$options$height1
          self$results$plot1$setSize(width, height)
        }
        
        
      },
      
#---------------------------------------------------------------      
  .run = function() {

    
    if (is.null(self$options$dep) | 
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
   
    if(isTRUE(self$options$rc) | isTRUE(self$options$plot)){
     
   # Repeated correlation-----------------------------------------------------    
  
    res<- rmcorr::rmcorr(id, data[[dep]], data[[covs]], data)

    #-----------------------------------------          
  
    if(isTRUE(self$options$rc)){
      
      table<- self$results$rc
      
      r <-  res$r
      df <- res$df
      p <- res$p
      lower <-  res$CI[[1]]
      upper<-   res$CI[[2]]
      
      row <- list()
      
      row[['r']] <- r
      row[['df']] <- df
      row[['p']] <- p
      row[['lower']] <- lower
      row[['upper']] <- upper
      
      table$setRow(rowNo = 1, values = row)
      
    }
    
    image <- self$results$plot
    image$setState(res) 
     
    }
    
    # Cross correlation-----------------------
    
    if(isTRUE(self$options$cc) | isTRUE(self$options$plot1)){
    
    Measure1 <- as.vector(data[[dep]])
    Measure2 <- as.vector(data[[covs]])
    r<- stats::ccf(Measure1, Measure2, plot = FALSE)
    
    #self$results$text$setContent(res1)  
    
    table <- self$results$cc
    
    res1<- cbind(r[["lag"]], r[["acf"]])
    res1<- as.data.frame(res1)
    names(res1) <- c("Lag", "Values")
    
    names<- dimnames(res1)[[1]]
    
    for (name in names) {
      
      row <- list()
      
      row[["lag"]]   <-  res1[name, 1]
      row[["value"]] <-  res1[name, 2]
      
      table$addRow(rowKey=name, values=row)
      
    }
    
    image1 <- self$results$plot1
    image1$setState(r) 
    
    }
    
  },

.plot = function(image, ...) {


  if (is.null(image$state))
    return(FALSE)

 rc <- image$state

  #plot <- plot(cc)
 
 plot<- plot(rc, overall = FALSE, lty = 1,lwd=3, 
             xlab = self$options$dep,
             ylab = self$options$covs)
 

  print(plot)
  TRUE


},

.plot1 = function(image1, ...) {
  
  
  if (is.null(image1$state))
    return(FALSE)
  
  res11 <- image1$state
  
  plot1<- plot(res11)
  
  print(plot1)
  TRUE
  
  
}


        )
)
