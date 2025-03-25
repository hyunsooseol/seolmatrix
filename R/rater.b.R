

raterClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "raterClass",
    inherit = raterBase,
    private = list(
      .htmlwidget = NULL,
      # Add instance for HTMLWidget
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new() # Initialize the HTMLWidget instance
        
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
            length(self$options$vars) < 2)
          return()
        
        vars <- self$options$vars
        data <- self$data
        data <- jmvcore::naOmit(data)
        #------------------------------------
        if (self$options$t == "row") {
          data <- t(data)
          data <- as.matrix(data)
        }
        
        # compute Light's Kappa-----
        
        res <- irr::kappam.light(ratings = data)
        
        # get subjects-------
        n <- res$subjects
        # get raters--------
        rater <- res$raters
        # get statistic------------
        statistic <- res$value
        # z value----------------
        z <- res$statistic
        # p value-------------------
        p <- res$p.value
        
        if (isTRUE(self$options$interrater)) {
          table <- self$results$interrater
          row <- list()
          row[['N']] <- n
          row[['Raters']] <- rater
          row[['Kappa']] <- statistic
          row[['Z']] <- z
          row[['p']] <- p
          table$setRow(rowNo = 1, values = row)
        }
        # Fleiss' kappa================
        
        kap <- irr::kappam.fleiss(ratings = data)
        
        # get subjects-------
        nk <- kap$subjects
        # get raters--------
        raterk <- kap$raters
        # get statistic------------
        statistick <- kap$value
        # z value----------------
        zk <- kap$statistic
        # p value-------------------
        pk <- kap$p.value
        
        if (isTRUE(self$options$fk)) {
          table <- self$results$fk
          row <- list()
          row[['N']] <- nk
          row[['Raters']] <- raterk
          row[['Kappa']] <- statistick
          row[['Z']] <- zk
          row[['p']] <- pk
          table$setRow(rowNo = 1, values = row)
        }
        
        # bootstrap of Fleiss' kappa------------
        
        if (isTRUE(self$options$bt)) {
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
        # Fleiss' Exact kappa-------------------
        
        if (isTRUE(self$options$ek)) {
          kae <- irr::kappam.fleiss(ratings = data, exact = TRUE)
          
          # get subjects-------
          nke <- kae$subjects
          # get raters--------
          raterke <- kae$raters
          # get statistic------------
          statisticke <- kae$value
          
          table <- self$results$ek
          row <- list()
          row[['N']] <- nke
          row[['Raters']] <- raterke
          row[['Kappa']] <- statisticke
          table$setRow(rowNo = 1, values = row)
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
        #  Agreement Analysis------------
        # Simple Percentage agreement-------------------
        
        if (isTRUE(self$options$pa)) {
          #tol <- self$options$tol
          
          pa <- irr::agree(data)
          
          # get subjects-------
          subjects <- pa$subjects
          # get raters--------
          raters <- pa$raters
          # get statistic(%)------------
          percent <- pa$value
          
          table <- self$results$pa
          row <- list()
          row[['Subjects']] <- subjects
          row[['Raters']] <- raters
          row[['Agreement(%)']] <- percent
          table$setRow(rowNo = 1, values = row)
        }
        
        # ICC TABLE--------------------
        
        if (isTRUE(self$options$icc)) {
          ## compute icc table-------
          
          icc <- try(psy::icc(data = data))
          #----------------------------
          
          if (jmvcore::isError(icc)) {
            err_string <- stringr::str_interp("You can't perform this analysis with sentence-type data.")
            stop(err_string)
          }
          if (!jmvcore::isError(icc)) {
            #subjects
            ns <- icc$nb.subjects
            #raters
            nr <- icc$nb.raters
            #subject variance
            sv <- icc$subject.variance
            #rater variance
            rv <- icc$rater.variance
            # residual variance
            ev <- icc$residual
            # consistency
            ic <- icc$icc.consistency
            # agreement
            ia <- icc$icc.agreement
            
            # populate icc table-----
            
            table <- self$results$icc
            row <- list()
            row[['Subjects']] <- ns
            row[['Raters']] <- nr
            row[['Subject variance']] <- sv
            row[['Rater variance']] <- rv
            row[['Residual variance']] <- ev
            row[['Consistency']] <- ic
            row[['Agreement']] <- ia
            table$setRow(rowNo = 1, values = row)
          }
        }
        
        # Bootstrap of ICC agreement table---------
        
        if (isTRUE(self$options$bicc)) {
          k <- try(boot::boot(data, function(x, idx) {
            icc(x[idx, ])$icc.agreement
          }, R = self$options$boot))
          #
          #   ### To obtain 95% confidence interval---------
          #
          #   icc.boot <- function(data, x) {
          #     icc(data[x, ])[[7]]
          #   }
          #
          #   boot <- self$options$boot
          #
          #   bres <- try(boot::boot(data, icc.boot, boot))
          #
          if (jmvcore::isError(k)) {
            err_string <- stringr::str_interp("You can't perform this analysis with sentence-type data.")
            stop(err_string)
          }
          
          if (!jmvcore::isError(k)) {
            #
            #   # two-sided bootstrapped confidence interval of icc (agreement)
            #
            # bicc <- quantile(bres$t, c(0.025, 0.975))
            #
            bootci <- boot::boot.ci(k)
            bicc <- bootci$normal
            
            table <- self$results$bicc
            row <- list()
            row[['lower']] <- bicc[2]
            row[['upper']] <- bicc[3]
            table$setRow(rowNo = 1, values = row)
          }
        }
        
        # Kendall's W--------------------------
        
        if (isTRUE(self$options$kend)) {
          ked <- irr::kendall(data, correct = TRUE)
          
          # get subjects-------
          n <- ked$subjects
          # get raters--------
          rater <- ked$raters
          # W------------
          w <- ked$value
          # chi value----------------
          chi <- ked$statistic
          # p value-------------------
          p <- ked$p.value
          
          table <- self$results$kend
          row <- list()
          row[['n']] <- n
          row[['rater']] <- rater
          row[['w']] <- w
          row[['chi']] <- chi
          row[['p']] <- p
          table$setRow(rowNo = 1, values = row)
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
        
        #################################################
        model <- out$model
        type <- out$type
        unit <- out$unit
        sub <- out$subjects
        raters <- out$raters
        icc <- out$value
        
        ##############
        # icc <- out$value
        f <- out$Fvalue
        df1 <- out$df1
        df2 <- out$df2
        p1 <- out$p.value
        lower <- out$lbound
        upper <- out$ubound
        
        if (isTRUE(self$options$ic)) {
          # icc for oneway and twoway table--------------
          
          table <- self$results$ic
          
          row <- list()
          row[['model']] <- model
          row[['type']] <- type
          row[['unit']] <- unit
          row[['sub']] <- sub
          row[['raters']] <- raters
          row[['icc']] <- icc
          table$setRow(rowNo = 1, values = row)
        }
        if (isTRUE(self$options$ftest)) {
          # F test--------------------------
          table <- self$results$ftest
          
          row <- list()
          row[['icc']] <- icc
          row[['f']] <- f
          row[['df1']] <- df1
          row[['df2']] <- df2
          row[['p1']] <- p1
          row[['lower']] <- lower
          row[['upper']] <- upper
          
          table$setRow(rowNo = 1, values = row)
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
          
          # get subjects-------
          nkrip <- krip$subjects
          
          # get raters--------
          raterkrip <- krip$raters
          
          # get statistic------------
          statistickrip <- krip$value
          
          #-----------------------------------
          table <- self$results$krip
          
          row <- list()
          row[['Subjects']] <- nkrip
          row[['Raters']] <- raterkrip
          row[['alpha']] <- statistickrip
          table$setRow(rowNo = 1, values = row)
        }
      }
    )
  )
