
# This file is a generated template, your changes will not be overwritten

#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ahpsurvey
#' @import ggplot2
#' @importFrom ahpsurvey ahp
#' @importFrom ahpsurvey ahp.mat
#' @importFrom ahpsurvey ahp.indpref
#' @importFrom ahpsurvey ahp.aggpref
#' @importFrom ahpsurvey ahp.aggjudge
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
        #nVars <- length(vars)
        method <- self$options$method
        method1 <- self$options$method1
        
        mydata <- self$data
        mydata <- jmvcore::naOmit(mydata)
        
        for(v in vars)
          mydata[[v]] <- jmvcore::toNumeric(mydata[[v]])

        
        #atts <- self$options$atts
        atts <- strsplit(self$options$atts, ',')[[1]]
       
        # compute AHP-------
        
        # res <- ahpsurvey::ahp(df=mydata,
        #                       atts = atts,
        #                       negconvert = TRUE)
                              
        #self$results$text$setContent(res)       
         
        matahp<- ahpsurvey::ahp.mat(mydata,atts, negconvert = T)
        
        ############################
        eigentrue <- ahpsurvey::ahp.indpref(matahp, atts, method = "eigen")
        geom <- ahpsurvey::ahp.indpref(matahp, atts, method = "arithmetic")
        error <- data.frame(id = 1:length(matahp), maxdiff = apply(abs(eigentrue - geom), 1, max))
        
        #self$results$text$setContent(error)   
        
        # Individual preference plot1----------
        
        image <- self$results$plot1
        image$setState(error)
        
        # 'Aggregated priorities'  table----------
        
        geo <- ahpsurvey::ahp.aggpref(matahp, 
                                      atts, 
                                      method = method)
        df <- data.frame(Value = geo)
        
        
        
        names<- dimnames(df)[[1]]
        
        table <- self$results$ap
       
        
        for (name in names) {
          
          row <- list()
          
          row[['value']] <- df[name,1]
          
          table$addRow(rowKey=name, values=row)
          
        }
        
        # Aggregated individual judgements table--------
        
        aj<- ahpsurvey::ahp.aggjudge(matahp, 
                                     atts, 
                                     aggmethod = method1)
        
        item<- as.matrix(aj)
        
        names <- dimnames(item)[[1]]
        dims <- dimnames(item)[[2]]
        
        table <- self$results$aj
        
        for (dim in dims) {
          
          table$addColumn(name = paste0(dim),
                          type = 'number')
        }
        
        
        for (name in names) {
          
          row <- list()
          
          for(j in seq_along(dims)){
            
            row[[dims[j]]] <- item[name,j]
            
          }
          
          table$addRow(rowKey=name, values=row)
          
        }
        
        
              },
    
    .plot1 = function(image,ggtheme, theme,...) {
      
      if (is.null(image$state))
        return(FALSE)
      
      error <- image$state
      
      plot1<- ggplot2::ggplot(data=error,ggplot2::aes(x = id, y = maxdiff)) +
        geom_point() +
        geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
        geom_hline(yintercept = 0, color = "gray50") +
        scale_x_continuous("Respondent ID") +
        scale_y_continuous("Maximum difference") +
        theme_minimal()
       
      plot1+ggtheme
      
      print(plot1)
      TRUE
          
    }
    
        )
)
