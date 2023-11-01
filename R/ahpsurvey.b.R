
# This file is a generated template, your changes will not be overwritten

#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ahpsurvey
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom ahpsurvey ahp
#' @importFrom ahpsurvey ahp.mat
#' @importFrom ahpsurvey ahp.indpref
#' @importFrom ahpsurvey ahp.aggpref
#' @importFrom ahpsurvey ahp.aggjudge
#' @importFrom ahpsurvey ahp.cr
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom tidyr gather
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
      
      
      if(isTRUE(self$options$plot1)){
        width <- self$options$width1
        height <- self$options$height1
        self$results$plot1$setSize(width, height)
      }
      
      if(isTRUE(self$options$plot2)){
        width <- self$options$width2
        height <- self$options$height2
        self$results$plot2$setSize(width, height)
      }  
      
      
      
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
        
        # Consistency ratio of each decision-maker---------
        
        private$.populateCrOutputs(results)
        
        
        
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
        
        # Consistency ratio of each decision-maker-------
        
        cr <- ahpsurvey::ahp.cr(matahp, atts)
        
        #self$results$text$setContent(cr)  
       
        # Individual preference plot1----------   
        if(self$options$plot1==TRUE){
        
        eigentrue <- ahpsurvey::ahp.indpref(matahp, atts, method = "eigen")
        geom <- ahpsurvey::ahp.indpref(matahp, atts, method = "arithmetic")
        error <- data.frame(id = 1:length(matahp), maxdiff = apply(abs(eigentrue - geom), 1, max))
        
        #self$results$text$setContent(error)   
        
        image <- self$results$plot1
        image$setState(error)
    
        }
        
        if(self$options$plot2==TRUE){
          
          eigentrue <- ahpsurvey::ahp.indpref(matahp, atts, method = "eigen")
        
          cr <- data %>%
            ahpsurvey::ahp.mat(atts, negconvert = T) %>%
            ahpsurvey::ahp.cr(atts)
          
          thres <- 0.1
          
          cr.df <- data %>%
            ahp.mat(atts, negconvert = TRUE) %>% 
            ahp.cr(atts) %>% 
            data.frame() %>%
            dplyr::mutate(rowid = 1:length(cr), cr.dum = as.factor(ifelse(cr <= thres, 1, 0))) %>%
            dplyr::select(cr.dum, rowid)
          
          #self$results$text$setContent(cr.df) 
          
          
          d<- data %>%
            ahpsurvey::ahp.mat(atts = atts, negconvert = TRUE) %>% 
            ahpsurvey::ahp.indpref(atts, method = "eigen") %>% 
            dplyr::mutate(rowid = 1:nrow(eigentrue)) %>%
            dplyr::left_join(cr.df, by = 'rowid') %>%
            tidyr::gather(atts, key = "var", value = "pref")  
          
          #self$results$text$setContent(d) 
          
         state<-list(d, cr)
          
          
          image <- self$results$plot2
          image$setState(state)
        }
        
        results <-
          list(
            'df' = df,
            'item' = item,
            'cr'=cr
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
    
    # COnsistency ration of each decision-maker-----
    
    .populateCrOutputs= function(results) {
      
      
      cr <- results$cr
    
      if (self$options$cr
          && self$results$cr$isNotFilled()) {
        
        
        self$results$cr$setValues(cr)
        
        self$results$cr$setRowNums(rownames(data))
        
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
        scale_y_continuous("Maximum difference") 
        
      #theme_minimal()
       
      plot1+ggtheme
      
      print(plot1)
      TRUE
          
    },
    
    
    .plot2 = function(image,ggtheme, theme,...) {
      
      if (is.null(image$state))
        return(FALSE)
    
      atts <- strsplit(self$options$atts, ',')[[1]]
      thres <- 0.1
        
      d <- image$state[[1]]
      cr <- image$state[[2]]
    
      plot2<- ggplot(d,aes(x = var, y = pref)) + 
        geom_violin(alpha = 0.6, width = 0.8, color = "transparent", fill = "gray") +
        geom_jitter(alpha = 0.6, height = 0, width = 0.1, aes(color = cr.dum)) +
        geom_boxplot(alpha = 0, width = 0.3, color = "#808080") +
        scale_x_discrete("Attribute", label = atts) +
        scale_y_continuous("Weight (dominant eigenvalue)", 
                           labels = scales::percent, 
                           breaks = c(seq(0,0.7,0.1))) +
        guides(color=guide_legend(title=NULL))+
        scale_color_discrete(breaks = c(0,1), 
                             labels = c(paste("CR >", thres), 
                                        paste("CR <", thres))) +
        labs(NULL, caption = paste("n =", nrow(data), ",", "Mean CR =",
                                   round(mean(cr),3)))+
     
       # theme_minimal() + 
        theme(
          axis.text.x = element_text(size = 12, color = "black")  # adjust size and darkness of x-axis text
          #axis.text.x.bottom = element_text(angle = 90, hjust = 1)  # rotate x-axis text by 90 degrees
        )
      
      plot2+ggtheme
      
        if (self$options$angle > 0) {
        plot2 <- plot2 + ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = self$options$angle, hjust = 1
          )
        )
      }
      
      
      print(plot2)
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
