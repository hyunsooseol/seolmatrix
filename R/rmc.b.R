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
        if (is.null(self$options$dep) || is.null(self$options$covs))
          return()
        
        id   <- self$options$id
        dep  <- self$options$dep
        covs <- self$options$covs
        
        # -----------------------------
        # Cross-correlation용 데이터
        # -----------------------------
        cols_cc <- unique(c(dep, covs))
        cols_cc <- cols_cc[!is.na(cols_cc) & nzchar(cols_cc)]
        
        data_cc <- self$data[, cols_cc, drop = FALSE]
        data_cc[[dep]]  <- jmvcore::toNumeric(data_cc[[dep]])
        data_cc[[covs]] <- jmvcore::toNumeric(data_cc[[covs]])
        data_cc <- na.omit(data_cc)
        
        # -----------------------------
        # Repeated-correlation용 데이터
        # -----------------------------
        data_rc <- NULL
        if (!is.null(id) && id != "") {
          cols_rc <- unique(c(id, dep, covs))
          cols_rc <- cols_rc[!is.na(cols_rc) & nzchar(cols_rc)]
          
          data_rc <- self$data[, cols_rc, drop = FALSE]
          data_rc[[dep]]  <- jmvcore::toNumeric(data_rc[[dep]])
          data_rc[[covs]] <- jmvcore::toNumeric(data_rc[[covs]])
          data_rc <- na.omit(data_rc)
        }
        
        # Repeated correlation
        if ((isTRUE(self$options$rc) || isTRUE(self$options$plot)) &&
            !is.null(data_rc)) {
          
          res <- rmcorr::rmcorr(id, data_rc[[dep]], data_rc[[covs]], data_rc)
          
          if (isTRUE(self$options$rc)) {
            table <- self$results$rc
            row <- list(
              r     = res$r,
              df    = res$df,
              p     = res$p,
              lower = res$CI[[1]],
              upper = res$CI[[2]]
            )
            table$setRow(rowNo = 1, values = row)
          }
          
          if (isTRUE(self$options$plot)) {
            image <- self$results$plot
            image$setState(list(
              res  = res,
              dep  = dep,
              covs = covs
            ))
          }
        }
        
        # Cross correlation
        if (isTRUE(self$options$cc) || isTRUE(self$options$plot1)) {
          
          Measure1 <- as.vector(data_cc[[dep]])
          Measure2 <- as.vector(data_cc[[covs]])
          ccf_res <- stats::ccf(Measure1, Measure2, plot = FALSE)
          
          if (isTRUE(self$options$cc)) {
            table <- self$results$cc
            res1 <- cbind(ccf_res[["lag"]], ccf_res[["acf"]])
            res1 <- as.data.frame(res1)
            names(res1) <- c("Lag", "Values")
            
            for (i in 1:nrow(res1)) {
              row <- list()
              row[['lag']] <- res1[i, 1]
              row[['value']] <- res1[i, 2]
              table$addRow(rowKey = i, values = row)
            }
          }
          
          if (isTRUE(self$options$plot1)) {
            image1 <- self$results$plot1
            image1$setState(ccf_res)
          }
        }
      },
      
      .plot = function(image, ...) {
        
        if (!isTRUE(self$options$plot))
          return(FALSE)
        
        if (is.null(image$state))
          return(FALSE)
        
        res <- image$state$res
        dep <- image$state$dep
        covs <- image$state$covs
        
        plot(
          res,
          overall = FALSE,
          lty = 1,
          lwd = 3,
          xlab = dep,
          ylab = covs
        )
        
        TRUE
      },
      
      
      .plot1 = function(image1, ...) {
        
        if (is.null(image1$state))
          return(FALSE)
        
        plot(image1$state)
        TRUE
      }
    )
  )
