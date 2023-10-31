
# This file is a generated template, your changes will not be overwritten

#' Interrater Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom irr kappam.light
#' @import irr
#' @importFrom irr icc
#' @import lpSolve
#' @import qgraph
#' @import psych
#' @importFrom  psy icc
#' @importFrom  qgraph cor_auto
#' @importFrom qgraph EBICglasso
#' @importFrom qgraph qgraph
#' @importFrom psych partial.r
#' @importFrom irr kappam.fleiss
#' @import boot
#' @importFrom irr kappam.fleiss
#' @importFrom irr kripp.alpha
#' @importFrom stringr str_interp
#' @export


raterClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "raterClass",
    inherit = raterBase,
    private = list(
     # ==========================================================
      .init = function() {
       

        if(self$options$mode=='simple'){
       
          if (is.null(self$data) | is.null(self$options$vars)) {
            self$results$instructions$setVisible(visible = TRUE)
            
          }
          
          
         self$results$instructions$setContent(
           "<html>
             <head>
             </head>
             <body>
             <div class='instructions'>
             <p>___________________________________________________________________________________
             <p>1. The R package <b>irr</b> is described in the <a href='https://cran.r-project.org/web/packages/irr/irr.pdf' target = '_blank'>page</a>.</p>
             <p>2. If the <b>Raters are rows</b> option is selected, the plots are not drawn.</p>
             <p>3. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/seolmatrix/issues'  target = '_blank'>GitHub</a>.</p>
             <p>___________________________________________________________________________________
             </div>
             </body>
             </html>"
         )

         if (self$options$ftest)
           self$results$ftest$setNote(
             "Note",
             "H0: ICC = 0; H1: ICC > 0"
             
           )
         
         if (self$options$ic)
           self$results$ic$setNote(
             "Note",
             "The analysis was performed by 'irr::icc' function."
             
           )
         
         if (self$options$icc)
           self$results$icc$setNote(
             "Note",
             "The analysis was performed by 'psy::icc' function."
             
           )

        }
        
        if(self$options$mode=="complex"){
  
          
          if (is.null(self$data) | is.null(self$options$vars1)) {
            self$results$instructions1$setVisible(visible = TRUE)
            
          }
          
          self$results$instructions1$setContent(
            "<html>
             <head>
             </head>
             <body>
             <div class='instructions'>
             <h2><b>Krippendorff Alpha</b></h2>
             <p>___________________________________________________________________________________
             <p>1. Fleiss' kappa can be used with binary or nominal-scale.</p> 
             <p>2. The <b>irr</b> R package is described in the <a href='https://cran.r-project.org/web/packages/irr/irr.pdf' target = '_blank'>page</a>.</p>
             <p>3. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/seolmatrix/issues'  target = '_blank'>GitHub</a>.</p>
             <p>___________________________________________________________________________________
             </div>
             </body>
             </html>"
          )
       
    
        }
      },

      #======================================++++++++++++++++++++++
      .run = function() {
        
       
        if(self$options$mode=='complex'){
          
          if(is.null(self$options$vars1)) return()
          
          
          if(!is.null(self$options$vars)){
            
            err_string <- stringr::str_interp(
              " Variables should be removed from Rater Reliability box."
            )
            stop(err_string)
            
          } 
          
          
          vars1 <- self$options$vars1
          data <- self$data
          data <- jmvcore::naOmit(data)
          
          
          
          if(isTRUE(self$options$krip)){
            
            method <- self$options$method
            
            # sample<- t(data)
            # dat<- reshape::melt(sample)
            # colnames(dat) <-c("n", "rater", "value")
            # dat1 <- tidyr::pivot_wider(dat, id_cols = rater, names_from = n, values_from = value)
            # dat2 <- select(dat1, -rater)
            # dat2<- as.matrix(dat2)
            # 
            
            if (self$options$t != "row"){
              
              # data <- read.csv("kripp.csv")
              # sample<- t(data)
              # sample1<- as.matrix(sample)
              # krip<- irr::kripp.alpha(sample, method="nominal")
              # krip
              
              data<- t(data)
              dat2 <- as.matrix(data)
              
            }
            
            dat2 <- as.matrix(data)
            
            krip<- irr::kripp.alpha(dat2, method=method)
            
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
        
        # get variables-------
        
        data <- self$data
        vars <- self$options$vars
        
        
        
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
          
          
          # populate Fleiss' kappa table-----
          
          private$.populateKapTable(results)
          
         
          # populate icc table-----
          
          private$.populateIccTable(results)
          
          # populate Boot icc table-----
          
          private$.populateBooticcTable(results)
          
          # populate icc for anova table----
          
          private$.populateIcTable(results)
          
          
          # populate ftest----
          
          private$.populateFtestTable(results)
          
          
        }
      
       
        
        
        
        
        },
      
      # compute results=====================================================
      
      .compute = function(data) {
        
        
      
        data <- self$data
        vars <- self$options$vars
        
        #------------------------------------
        if (self$options$t1 == "row"){
        
          data<- t(data)
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
        
     
        # Fleiss' kappa================
        
        kap<- irr::kappam.fleiss(ratings = data)
        
        #self$results$text$setContent(kap)
        
        
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


        # Fleiss' Exact kappa================
       
        
        if(isTRUE(self$options$ek)){
         
        kae<- irr::kappam.fleiss(ratings = data, exact = TRUE)
        
        # get subjects-------
        
        nke <- kae$subjects
        
        # get raters--------
        
        raterke <- kae$raters
        
        # get statistic------------
        
        statisticke <- kae$value
        
        #-----------------------------------
        table <- self$results$ek
        
        row <- list()
        
        row[['n']] <- nke
        row[['rater']] <- raterke
        row[['statistic']] <- statisticke
        
        table$setRow(rowNo = 1, values = row)
        
        }
     

       if(isTRUE(self$options$cw)){
  
  cw<- irr::kappam.fleiss(ratings=data, detail=TRUE)
  
  c<- cw[["detail"]]
  
  names<- dimnames(c)[[1]]

  table <- self$results$cw  
  
  #self$results$text$setContent(names)
  
    for (name in names) {

      row <- list()
      
      row[['k']] <- c[name,1]
      row[['z']] <- c[name,2]
      row[['p']] <- c[name,3]

      table$addRow(rowKey=name, values=row)

    }
}        
     
        
        
    ## compute icc table-------
        
        icc <- psy::icc(data = data)
        
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
        
        
        ### To obtain 95% confidence interval---------
        
        
        icc.boot <- function(data, x) {
          icc(data[x, ])[[7]]
        }
        
        bres <- boot::boot(data, icc.boot, 1000)
        
        # two-sided bootstrapped confidence interval of icc (agreement)
        
        bicc <- quantile(bres$t, c(0.025, 0.975))
        
        
        ########### icc using oneway and twoway----------
        
        model <- self$options$model
        type <- self$options$type
        unit <- self$options$unit
        
        ###################################################
        out<- irr::icc(data, model = model,
                       type = type, unit = unit)
        
        #################################################
        
        
        ####################
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
        
        if(isTRUE(self$options$ggm)){
        
          if(self$options$t1=='row') return()
          
          # Compute correlations:
          CorMat <- qgraph::cor_auto(data)
          
          # Compute graph with tuning = 0.5 (EBIC)
          EBICgraph <- qgraph::EBICglasso(CorMat, nrow(data), 0.5, threshold = TRUE)
          
          # Prepare Data For Plot -------
          image <- self$results$plot
          image$setState(EBICgraph)
          
        }
          
        if(isTRUE(self$options$par)){
          
          if(self$options$t1=='row') return()
        
          
          par <- psych::partial.r(data)
          
          # partial Plot -------
          
          image <- self$results$plot1
          image$setState(par)
          
        }
          
         
        
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
            'ev'= ev,
            'ic' = ic,
            'ia' = ia,
            'bicc' = bicc,
            'model'=model,
            'type'=type,
            'unit'=unit,
            'sub'=sub,
            'raters'=raters,
            'icc'=icc,
            'f'=f,
            'df1'=df1,
            'df2'=df2,
            'p1'=p1,
            'lower'=lower,
            'upper'=upper,
            'nk' = nk,
            'raterk' = raterk,
            'statistick' = statistick,
            'zk' = zk,
            'pk' = pk,
            'kap'=kap
           # 'cw'=cw
            )
        
      },
      
      
      # populate rater table-----
      
      .populateRaterTable = function(results) {
        
        if(!self$options$interrater) return()
        
        
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
      
      
     # populate Fleiss' kappa table-----
     
     .populateKapTable = function(results) {
       
       if(!self$options$fk) return()
       
       
       table <- self$results$fk
       
       if(is.null(self$options$fk))
       return()
       
       
       nk <- results$nk
       raterk <- results$raterk
       statistick <- results$statistick
       zk <- results$zk
       pk <- results$pk
       
       
       row <- list()
       
       row[['n']] <- nk
       row[['rater']] <- raterk
       row[['statistic']] <- statistick
       row[['z']] <- zk
       row[['p']] <- pk
       
       table$setRow(rowNo = 1, values = row)
       
     },
     

  # populate icc table-----
      
      .populateIccTable = function(results) {
        
        
        table <- self$results$icc
        
        ns <- results$ns
        nr <- results$nr
        sv <- results$sv
        rv <- results$rv
        ev <- results$ev
        ic <- results$ic
        ia <- results$ia
        
        
        row <- list()
        
        row[['Subjects']] <- ns
        row[['Raters']] <- nr
        row[['Subject variance']] <- sv
        row[['Rater variance']] <- rv
        row[['Residual variance']] <- ev
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
      
     # icc for oneway and twoway table-------------- 
     
     .populateIcTable=function(results){
       
       table <- self$results$ic
       
       model <- results$model
       type <- results$type
       unit <- results$unit
       sub <- results$sub
       raters <- results$raters
       icc <- results$icc
       
       row <- list()
       
       row[['model']] <- model
       row[['type']] <- type
       row[['unit']] <- unit
       row[['sub']] <- sub
       row[['raters']] <- raters
       row[['icc']] <- icc
      
       table$setRow(rowNo = 1, values = row)
       
       
     },
     
     # F test--------------------------
     
     .populateFtestTable=function(results){
       
       table <- self$results$ftest
       
       icc <- results$icc
       f <- results$f
       df1 <- results$df1
       df2 <- results$df2
       p1 <- results$p1
       lower <- results$lower
       upper <- results$upper
       
      
       row <- list()
       
       row[['icc']] <- icc
       row[['f']] <- f
       row[['df1']] <- df1
       row[['df2']] <- df2
       row[['p1']] <- p1
       row[['lower']] <- lower
       row[['upper']] <- upper
       
       
       table$setRow(rowNo = 1, values = row)
       
       
     },
   
      
      #### Plot functions ----
      # 
      # .prepareGgmPlot = function(data) {
      #   
      #   # Compute correlations:
      #   CorMat <- qgraph::cor_auto(data)
      #   
      #   # Compute graph with tuning = 0.5 (EBIC)
      #   EBICgraph <- qgraph::EBICglasso(CorMat, nrow(data), 0.5, threshold = TRUE)
      #   
      #   # Prepare Data For Plot -------
      #   image <- self$results$plot
      #   image$setState(EBICgraph)
      #   
      #  
      # },
      
      # .prepareParPlot=function(data) {
      #   
      #   # # Compute correlations:
      #   # CorMat <- qgraph::cor_auto(data)
      #   
      #   par <- psych::partial.r(data)
      #   
      #   # Prepare Data For Plot -------
      #  
      #   image <- self$results$plot1
      #   image$setState(par)
      #  
      # },
      # 
      
      #================================================================
      
      .plot = function(image, ...) {
        
       
        if (is.null(image$state))
          return(FALSE)
        
        EBICgraph <- image$state
        
        plot <- qgraph::qgraph(EBICgraph, layout = "spring", title = "EBIC", details = TRUE)
        
        print(plot)
        TRUE
      },
      
      .plot1 = function(image, ...) {
        
        
        if (is.null(image$state))
          return(FALSE)
        
        par <- image$state
        
        plot1 <- qgraph::qgraph(par, layout = "spring", details = TRUE)
        
        print(plot1)
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
