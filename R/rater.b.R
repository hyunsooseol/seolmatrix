raterClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "raterClass",
    inherit = raterBase,
    private = list(
      .htmlwidget = NULL,
      
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
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/seolmatrix/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
        ))
        if (self$options$ftest)
          self$results$ftest$setNote("Note", "H\u2090: ICC = 0; H\u2081: ICC > 0")
        
        if (self$options$ic)
          self$results$ic$setNote("Note", "The analysis was performed by 'irr::icc' function.")
        
        if (self$options$icc)
          self$results$icc$setNote("Note", "The analysis was performed by 'psy::icc' function.")
        
        if (self$options$kend)
          self$results$kend$setNote("Note", "H\u2090: W=0")
        
      },
      
      .run = function() {
        if (is.null(self$options$vars) |
            length(self$options$vars) < 2) return()
        
        vars <- self$options$vars
        data <- self$data
        data <- jmvcore::naOmit(data)
        #------------------------------------
        if (self$options$t == "row") {
          data <- t(data)
          data <- as.matrix(data)
        }
        
        # compute Light's Kappa-----
        
        # res <- irr::kappam.light(ratings = data)
        # 
        # # get subjects-------
        # n <- res$subjects
        # # get raters--------
        # rater <- res$raters
        # # get statistic------------
        # statistic <- res$value
        # # z value----------------
        # z <- res$statistic
        # # p value-------------------
        # p <- res$p.value
        # 
        # if (isTRUE(self$options$interrater)) {
        #   table <- self$results$interrater
        #   row <- list()
        #   row[['N']] <- n
        #   row[['Raters']] <- rater
        #   row[['Kappa']] <- statistic
        #   row[['Z']] <- z
        #   row[['p']] <- p
        #   table$setRow(rowNo = 1, values = row)
        # }
        # 
        res <- irr::kappam.light(ratings = data)
        
        if (isTRUE(self$options$interrater)) {
          table <- self$results$interrater
          table$setRow(
            rowNo = 1,
            values = with(res, list(
              N = subjects,
              Raters = raters,
              Kappa = value,
              Z = statistic,
              p = p.value
            ))
          )
        }
        
        # Fleiss' kappa================
        
        kap <- irr::kappam.fleiss(ratings = data)
        
        if (isTRUE(self$options$fk)) {
          table <- self$results$fk
          table$setRow(
            rowNo = 1,
            values = with(kap, list(
              N = subjects,
              Raters = raters,
              Kappa = value,
              Z = statistic,
              p = p.value
            ))
          )
        }

        # bootstrap of Fleiss' kappa------------
        
        if (isTRUE(self$options$bt)) {
          
          library(irr)
          
          bt <- boot::boot(data, function(x, idx) {
            kappam.fleiss(x[idx, ])$value
          }, R = self$options$boot1)
          
          bootci <- boot::boot.ci(bt)
          bootci <- bootci$normal
          table <- self$results$bt
          row <- list()
          row[['lower']] <- bootci[2]
          row[['upper']] <- bootci[3]
          table$setRow(rowNo = 1, values = row)
        }
        
        #  Exact kappa-------------------
        
        if (isTRUE(self$options$ek)) {
          
          kae <- irr::kappam.fleiss(ratings = data, exact = TRUE)
            table <- self$results$ek
            table$setRow(
              rowNo = 1,
              values = list(
                N = kae$subjects,
                Raters = kae$raters,
                Kappa = kae$value  
              )
            )
              }
          
                 
        # Category-wise Kappas -----------------
        if (isTRUE(self$options$cw)) {
          
          cw <- irr::kappam.fleiss(ratings = data, detail = TRUE)
          c <- cw[["detail"]]
          names <- dimnames(c)[[1]]
          table <- self$results$cw
          for (name in names) {
            row <- list()
            row[['k']] <- c[name, 1]
            row[['z']] <- c[name, 2]
            row[['p']] <- c[name, 3]
            table$addRow(rowKey = name, values = row)
          }
        }

        # Simple Percentage agreement-------------------
        
        if (isTRUE(self$options$pa)) {
          pa <- irr::agree(data)

          table <- self$results$pa
          table$setRow(
            rowNo = 1,
            values = with(pa, list(
              Subjects = subjects,
              Raters = raters,
              Agreement = value
             ))
          )
          }
        
        # ICC TABLE--------------------
        
        if (isTRUE(self$options$icc)) {
          
          # compute icc table-------
          icc <- try(psy::icc(data = data))
         
          if (jmvcore::isError(icc)) {
            err_string <- stringr::str_interp("You can't perform this analysis with sentence-type data.")
            stop(err_string)
          }
          if (!jmvcore::isError(icc)) {
            table <- self$results$icc
            table$setRow(
              rowNo = 1,
              values = with(icc, list(
                Subjects = nb.subjects,
                Raters = nb.raters,
                sv = subject.variance,
                rv = rater.variance,
                rev= residual,
                Consistency= icc.consistency,
                Agreement= icc.agreement
              )))
    
              }
        }
        
        # Bootstrap of ICC agreement table---------
        
        if (isTRUE(self$options$bicc)) {
          
          library(psy)
          
          k <- try(boot::boot(data, function(x, idx) {
            icc(x[idx, ])$icc.agreement
          }, R = self$options$boot))

          if (jmvcore::isError(k)) {
            err_string <- stringr::str_interp("You can't perform this analysis with sentence-type data.")
            stop(err_string)
          }
          
          if (!jmvcore::isError(k)) {
            bootci <- boot::boot.ci(k)
            bicc <- bootci$normal
            
            table <- self$results$bicc
            # row <- list()
            # row[['lower']] <- bicc[2]
            # row[['upper']] <- bicc[3]
            # table$setRow(rowNo = 1, values = row)
            table$setRow(
              rowNo = 1,
              values = list(
                lower = bicc[2],
                upper = bicc[3]
              )
            )
          }
        }
        
        # Kendall's W--------------------------
        if (isTRUE(self$options$kend)) {
          ked <- irr::kendall(data, correct = TRUE)

          table <- self$results$kend
          table$setRow(
            rowNo = 1,
            values = with(ked, list(
              n = subjects,
              rater = raters,
              w = value,
              chi = statistic,
              p = p.value
            )))
          }
        
        ########### icc using oneway and twoway----------
        
        model <- self$options$model
        type <- self$options$type
        unit <- self$options$unit
        
        ###################################################
        out <- irr::icc(data,
                        model = model,
                        type = type,
                        unit = unit)

        # icc for oneway and twoway table--------------
        
        if (isTRUE(self$options$ic)) {
          
          table <- self$results$ic
          table$setRow(
            rowNo = 1,
            values = with(out, list(
              model = model,
              type = type,
              unit = unit,
              sub = subjects,
              raters = raters,
              icc=value
            )))
        }

      # F test--------------------------
        
        if (isTRUE(self$options$ftest)) {
          
          table <- self$results$ftest
          
          table$setRow(
            rowNo = 1,
            values = with(out, list(
              icc = value,
              f = Fvalue,
              df1 = df1,
              df2 = df2,
              p1 = p.value,
              lower = lbound,
              upper=ubound
            )))
        }
        
        
        if (isTRUE(self$options$krip)) {
          method <- self$options$method
          
          if (self$options$t != "row") {
            data <- t(data)
            data <- as.matrix(data)
          } else if (self$options$t == "row") {
            data <- t(data)
            data <- as.matrix(data)
          }
          #----------------------------------------------
          krip <- irr::kripp.alpha(data, method = method)

          table <- self$results$krip
          
          table$setRow(
            rowNo = 1,
            values = with(krip, list(
              Subjects = subjects,
              Raters = raters,
              alpha=value
            )))
        }
      }
    )
  )