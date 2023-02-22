
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
            <p>1. The R package <b>ahpsurvey</b> is described in the <a href='https://cran.r-project.org/web/packages/ahpsurvey/vignettes/my-vignette.html' target = '_blank'>page</a>.</p>
            <p>2. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/seolmatrix/issues'  target = '_blank'>GitHub.</a></p>
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

      # Ready--------
      
      ready <- TRUE
      
      if (is.null(self$options$vars) ||
          length(self$options$vars) < 2)
        
        ready <- FALSE
      
      if (ready) {
        
        data <- private$.cleanData()
        results <- private$.compute(data)
        
        #populate 'Aggregated priorities'------------ 
        
        private$.populateApTable(results)
        
        
        # populate 'Aggregated individual judgements' table----
        
        private$.populateAjTable(results)
        
      }
    },
    
    
    .compute = function(data) {
      
       # get variables---------------------------------
        
        
        vars <- self$options$vars
        method <- self$options$method
        method1 <- self$options$method1
        
          ############################################################   
          atts <- strsplit(self$options$atts, ',')[[1]]
          matahp<- ahpsurvey::ahp.mat(df=data,
                                      atts=atts,
                                      negconvert = T)
          
          ##########################################################
       
        # 'Aggregated priorities' table----------
        
        geo <- ahpsurvey::ahp.aggpref(matahp, 
                                      atts, 
                                      method = method)
        df <- data.frame(Value = geo)
        
       # Aggregated individual judgements table--------
        
        aj<- ahpsurvey::ahp.aggjudge(matahp, 
                                     atts, 
                                     aggmethod = method1)
        
        item<- as.matrix(aj)
        
        if(self$options$plot1==TRUE){
        
        # Individual preference plot1----------     
        eigentrue <- ahpsurvey::ahp.indpref(matahp, atts, method = "eigen")
        geom <- ahpsurvey::ahp.indpref(matahp, atts, method = "arithmetic")
        error <- data.frame(id = 1:length(matahp), maxdiff = apply(abs(eigentrue - geom), 1, max))
        
        #self$results$text$setContent(error)   
        
        image <- self$results$plot1
        image$setState(error)
    
        }
        
        
        results <-
          list(
            'df' = df,
            'item' = item
          )
          
    },  
        
        #Populate table----------------------------
        
            .populateApTable = function(results) {
              
              table <- self$results$ap
              df <- results$df  
        
        
              names<- dimnames(df)[[1]]
              
              table <- self$results$ap
              
              
              for (name in names) {
                
                row <- list()
                
                row[['value']] <- df[name,1]
                
                table$addRow(rowKey=name, values=row)
                
              }
        
        
              },
        
        
    .populateAjTable = function(results) {
    
      table <- self$results$aj
      item <- results$item
    
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
          
    },
    
    ### Helper functions =================================
    
    .cleanData = function() {
      items <- self$options$vars
      
      data <- list()
      
      for (item in items)
        data[[item]] <-
        jmvcore::toNumeric(self$data[[item]])
      
      attr(data, 'row.names') <-
        seq_len(length(data[[1]]))
      attr(data, 'class') <- 'data.frame'
      data <- jmvcore::naOmit(data)
      
      return(data)
    }
    
    
    
        )
)
