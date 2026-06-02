
ahpClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "ahpClass",
    inherit = ahpBase,
    private = list(
      .htmlwidget = NULL,
      
      #==========================================================
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
        }
        
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Each row and column represents an item and a decision maker for the input data format.</li>',
            '<li>More than 10 items or 20 decision makers are NOT allowed.</li>',
            '<li>Each item would be graded 1-9 score by each decision maker.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/seolmatrix/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
        
        if (self$options$cir)
          self$results$cir$setNote("Note", "value<0.1 is acceptable for consistency ratio.")
      },
      
      .run = function() {
        
        if (length(self$options$vars) < 1)
          return()
        
        # get variables---------------------------------
        vars <- self$options$vars
        
        mydata <- self$data[, vars, drop = FALSE]
        
        for (v in vars)
          mydata[[v]] <- jmvcore::toNumeric(mydata[[v]])
        
        mydata <- jmvcore::naOmit(mydata)
        
        ahpRes <- tryCatch({
          easyAHP::easyAHP(mydata)
        }, error = function(e) {
          self$results$instructions$setVisible(TRUE)
          self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
            title = "Analysis error",
            content = paste0(
              '<div style="border: 1px solid #fecaca; border-radius: 12px; padding: 14px; background-color: #fef2f2; margin-top: 10px;">',
              '<strong>Analysis could not be completed.</strong><br>',
              'Please check whether the input data are valid for AHP analysis.<br><br>',
              '<strong>Possible causes:</strong>',
              '<ul>',
              '<li>The input data may contain too few items or decision makers.</li>',
              '<li>The matrix may not be suitable for AHP calculation.</li>',
              '<li>The data may contain invalid, missing, or non-numeric values.</li>',
              '</ul>',
              '<strong>Details:</strong> ',
              htmltools::htmlEscape(e$message),
              '</div>'
            )
          ))
          return(NULL)
        })
        
        if (is.null(ahpRes))
          return()
        
        res <- ahpRes$Makers
        
        # item matrix--------
        item <- res$Matrix
        item <- as.matrix(item)
        names <- dimnames(item)[[1]]
        dims <- dimnames(item)[[2]]
        table <- self$results$itemmat
        
        for (dim in dims) {
          table$addColumn(name = paste0(dim), type = 'number')
        }
        
        for (name in names) {
          row <- list()
          for (j in seq_along(dims)) {
            row[[dims[j]]] <- item[name, j]
          }
          table$addRow(rowKey = name, values = row)
        }
        
        # item weights----------------
        weights <- res$Weights
        weights <- as.data.frame(weights)
        names <- dimnames(weights)[[1]]
        
        # item weights table-----------
        table <- self$results$weights
        for (name in names) {
          row <- list()
          row[['value']] <- weights[name, 1]
          table$addRow(rowKey = name, values = row)
        }
        
        # Confidence Index and Confidence Ratio--------
        Index <- res$CI
        Ratio <- res$CR
        cir <- rbind(Index, Ratio)
        names <- dimnames(cir)[[1]]
        
        # Confidence index and ratio table-------
        table <- self$results$cir
        for (name in names) {
          row <- list()
          row[['value']] <- cir[name, 1]
          table$addRow(rowKey = name, values = row)
        }
      }
    )
  )

