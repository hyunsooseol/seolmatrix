
# This file is a generated template, your changes will not be overwritten

#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ahpsurvey
#' @importFrom ahpsurvey ahp
#' @export



ahpsurveyClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "ahpsurveyClass",
    inherit = ahpsurveyBase,
    private = list(

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
            <p>1. Each row and column represents an item and a decision maker for the input data format.</p>
            <p>2. More than 10 items or 20 decision makers are NOT allowed.</p>
            <p>3. Each item would be graded 1-9 score by each decision maker</p>
            <p>4. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/seolmatrix/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
            </div>
            </body>
            </html>"
      )
      
      # if (self$options$cir)
      #   self$results$cir$setNote(
      #     "Note",
      #     "value<0.1 is acceptable for consistency ratio."
      #   )
      # 
      
      },

    .run = function() {

          
      if (length(self$options$vars)<1) 
        return() 
      
      
        # get variables---------------------------------
        
        
        vars <- self$options$vars
        nVars <- length(vars)
        
        
        mydata <- self$data
        mydata <- jmvcore::naOmit(mydata)
        
        for(v in vars)
          mydata[[v]] <- jmvcore::toNumeric(mydata[[v]])

        
        #atts <- self$options$atts
        atts <- strsplit(self$options$atts, ',')[[1]]
       
        # compute AHP-------
        
        res <- ahpsurvey::ahp(df=mydata,
                              atts = atts,
                              negconvert = TRUE)
                              
        #self$results$text$setContent(res)       
         
        
               
                
              }
              
        )
)
