
# This file is a generated template, your changes will not be overwritten

#' Interrater Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom irr kappam.light
#' @import irr
#' @import lpSolve
#' @import qgraph
#' @import psych
#' @importFrom psych polychoric
#' @importFrom  psych partial.r
#' @importFrom  psy icc
#' @import boot
#' @export


raterClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "raterClass",
    inherit = raterBase,
    private = list(
      #==========================================================
      .init = function() {
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(
          "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p>Welcome to Interrater reliability.</p>

            <p><b>To get started:</b></p>

            <p>- Just highlight the variables(raters) and click the arrow to move it across into the 'Variables' box.</p>
            <p>- To produce Gaussian Graphical Model, partial correlations are calculated from a input matrix of polychoric correlations.</p>

            <p>- Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/seolmatrix/'  target = '_blank'>GitHub</a></p>

            <p>If you have any questions, please e-mail me: snow@cau.ac.kr</a></p>
            </div>
            </body>
            </html>"
        )
        
      },
      
      #======================================++++++++++++++++++++++
      .run = function() {
        # get variables-------
        
        data <- self$data
        
        vars <- self$options$get('vars')
        
        
        # Ready--------
        
        ready <- TRUE
        
        if (is.null(self$options$vars) ||
            length(self$options$vars) < 2)
          
          ready <- FALSE
        
        if (ready) {
          data <- private$.cleanData()
          
          results <- private$.compute(data)
          
          # populate rater table-----
          
          private$.populateRaterTable(results)
          
          
          # prepare plot-----
          
          private$.prepareGgmPlot(data)
          
          # populate icc table-----
          
          private$.populateIccTable(results)
          
          # populate Boot icc table-----
          
          private$.populateBooticcTable(results)
          
          
        }
      },
      
      # compute results=====================================================
      
      .compute = function(data) {
        # get variables------
        
        data <- self$data
        
        vars <- self$options$get('vars')
        
        
         # for(v in vars)
         #   data[[v]] <- jmvcore::toNumeric(data[[v]])

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
        
        
        ### compute icc table-------
        
        icc <- psy::icc(data = data)
        
        #subjects
        
        ns <- icc$nb.subjects
        
        #raters
        
        nr <- icc$nb.raters
        
        #subject variance
        
        sv <- icc$subject.variance
        
        #rater variance
        
        rv <- icc$rater.variance
        
        # consistency
        
        ic <- icc$icc.consistency
        
        # agreement
        
        ia <- icc$icc.agreement
        
        
        ### To obtain 95% confidence interval---------
        
        
        icc.boot <- function(data, x) {
          icc(data[x, ])[[7]]
        }
        
        bres <- boot::boot(data, icc.boot, 1000)
        
        # two-sided bootstrapped confidence interval of icc (agreement)
        
        bicc <- quantile(bres$t, c(0.025, 0.975))
        
        
        results <-
          list(
            'n' = n,
            'rater' = rater,
            'statistic' = statistic,
            'z' = z,
            'p' = p,
            'ns' = ns,
            'nr' = nr,
            'sv' = sv,
            'rv' = rv,
            'ic' = ic,
            'ia' = ia,
            'bicc' = bicc
            
          )
        
      },
      
      
      # populate rater table-----
      
      .populateRaterTable = function(results) {
        table <- self$results$interrater
        
        n <- results$n
        rater <- results$rater
        statistic <- results$statistic
        z <- results$z
        p <- results$p
        
        
        row <- list()
        
        row[['n']] <- n
        row[['rater']] <- rater
        row[['statistic']] <- statistic
        row[['z']] <- z
        row[['p']] <- p
        
        table$setRow(rowNo = 1, values = row)
        
      },
      
      
      # populate icc table-----
      
      .populateIccTable = function(results) {
        table <- self$results$icc
        
        ns <- results$ns
        nr <- results$nr
        sv <- results$sv
        rv <- results$rv
        ic <- results$ic
        ia <- results$ia
        
        
        row <- list()
        
        row[['Subjects']] <- ns
        row[['Raters']] <- nr
        row[['Subject variance']] <- sv
        row[['Rater variance']] <- rv
        row[['Consistency']] <- ic
        row[['Agreement']] <- ia
        
        table$setRow(rowNo = 1, values = row)
        
        
      },
      
      
      # populate Boot ICC table-------
      
      .populateBooticcTable = function(results) {
        table <- self$results$bicc
        
        bicc <- results$bicc
        
        
        row <- list()
        
        row[['lower']] <- bicc[1]
        row[['upper']] <- bicc[2]
        
        
        table$setRow(rowNo = 1, values = row)
        
        
      },
      
      
      
      #### Plot functions ----
      
      .prepareGgmPlot = function(data) {
        # Gaussian Graphical Models with polychoric correlation----------------
        
        poly <- psych::polychoric(data)$rho
        partial <- psych::partial.r(poly)
        
        # Prepare Data For Plot -------
        image <- self$results$plot
        image$setState(partial)
        
        
        
      },
      
      
      #================================================================
      
      .plot = function(image, ...) {
        ggm <- self$options$ggm
        
        if (!ggm)
          return()
        
        
        partial <- image$state
        
        plot <- qgraph(partial, layout = "spring", details = TRUE)
        
        print(plot)
        TRUE
      },
      
      
      #### Helper functions =================================
      
      .cleanData = function() {
        items <- self$options$vars
        
        data <- list()
        
        for (item in items)
          data[[item]] <- jmvcore::toNumeric(self$data[[item]])
        
        attr(data, 'row.names') <- seq_len(length(data[[1]]))
        attr(data, 'class') <- 'data.frame'
        data <- jmvcore::naOmit(data)
        
        return(data)
      }
    )
  )
