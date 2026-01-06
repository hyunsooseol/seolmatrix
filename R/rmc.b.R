rmcClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "rmcClass",
    inherit = rmcBase,
    private = list(
      .htmlwidget = NULL,
      # Add instance for HTMLWidget
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new() # Initialize the HTMLWidget instance
        
        if (is.null(self$data) | is.null(self$options$dep) |
            is.null(self$options$covs)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/seolmatrix/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
        ))
      },
      
      #---------------------------------------------------------------
      .run = function() {
        if (is.null(self$options$dep) |
            is.null(self$options$covs))
          return()
        
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
        
        if (isTRUE(self$options$rc)) {  
          # Repeated correlation-----------------------------------------------------
          set.seed(1234)
          res <- rmcorr::rmcorr(id, data[[dep]], data[[covs]], data)
          
          #-----------------------------------------
          
          
          table <- self$results$rc
          r <-  res$r
          df <- res$df
          p <- res$p
          lower <-  res$CI[[1]]
          upper <-   res$CI[[2]]
          
          row <- list()
          row[['r']] <- r
          row[['df']] <- df
          row[['p']] <- p
          row[['lower']] <- lower
          row[['upper']] <- upper
          table$setRow(rowNo = 1, values = row)
        }
        
        
        # Cross correlation-----------------------
        
        if (isTRUE(self$options$cc)){ 
          Measure1 <- as.vector(data[[dep]])
          Measure2 <- as.vector(data[[covs]])
          set.seed(1234)
          r <- stats::ccf(Measure1, Measure2, plot = FALSE)
          
          #self$results$text$setContent(res1)
          
          table <- self$results$cc
          
          res1 <- cbind(r[["lag"]], r[["acf"]])
          res1 <- as.data.frame(res1)
          names(res1) <- c("Lag", "Values")
          
          for (i in 1:nrow(res1)) {
            row <- list()
            row[['lag']] <- res1[i, 1]
            row[['value']] <- res1[i, 2]
            table$addRow(rowKey = i, values = row)
          }
          # image1 <- self$results$plot1
          # image1$setState(r)
        }
      },
      
      .plot = function(image, ...) {
        
        if (!self$options$plot)
          return(FALSE)
        
        data <- self$data
        id <- self$options$id
        dep <- self$options$dep
        covs <- self$options$covs
        # convert to appropriate data types
        data[[dep]] <- jmvcore::toNumeric(data[[dep]])
        data[[covs]] <- jmvcore::toNumeric(data[[covs]])
        data <- na.omit(data)
        
        # Repeated correlation-----------------------------------------------------
        set.seed(1234)
        res <- rmcorr::rmcorr(id, data[[dep]], data[[covs]], data)
        
        plot <- plot(
          res,
          overall = FALSE,
          lty = 1,
          lwd = 3,
          xlab = self$options$dep,
          ylab = self$options$covs
        )
        print(plot)
        TRUE
      },
      
      
      .plot1 = function(image1, ...) {
        
        data <- self$data
        id <- self$options$id
        dep <- self$options$dep
        covs <- self$options$covs
        # convert to appropriate data types
        data[[dep]] <- jmvcore::toNumeric(data[[dep]])
        data[[covs]] <- jmvcore::toNumeric(data[[covs]])
        data <- na.omit(data)
        
        Measure1 <- as.vector(data[[dep]])
        Measure2 <- as.vector(data[[covs]])
        
        set.seed(1234)
        plot1 <- stats::ccf(Measure1, Measure2)
        
        print(plot1)
        TRUE
      }
    )
  )
