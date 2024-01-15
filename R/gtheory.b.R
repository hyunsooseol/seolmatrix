
# This file is a generated template, your changes will not be overwritten
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom jmvcore toB64
#' @importFrom gtheory gstudy
#' @importFrom gtheory dstudy
#' @import ggplot2
#' @export


gtheoryClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "gtheoryClass",
    inherit = gtheoryBase,
    private = list(
  
      .init = function() {
 
        if (is.null(self$data) | is.null(self$options$facet)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(
          "<html>
             <head>
             </head>
             <body>
             <div class='instructions'>
             <p>___________________________________________________________________________________
             <p> Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/seolmatrix/issues'  target = '_blank'>GitHub</a>.</p>
             <p>___________________________________________________________________________________
             </div>
             </body>
             </html>"
        )
        
        
      },
      
      
      .run = function() {
        
        # g theory with R-----------
        
        # library(gtheory)
        # data(Brennan.3.2)
        # data <- Brennan.3.2
        # 
        # formula <- "Score ~ (1 | Person) + (1 | Task) + 
        # (1 | Rater:Task) +
        # (1 | Person:Task)"
        # gstudy.out <- gtheory::gstudy(data = data, 
        #                               colname.objects = "Person",
        #                               formula = formula)
        # coef<- dstudy(gstudy.out, colname.objects = "Person", 
        #               data = data, colname.scores = "Score")
        # 
        # data(Rajaratnam.2)
        
        # Multivariate example----
        # library(gtheory)
        # data(Rajaratnam.2)
        # formula.Rajaratnam.2 <- "Score ~ (1 | Person) + (1 | Item)"
        # g<- gstudy(data = Rajaratnam.2, formula = formula.Rajaratnam.2, 
        #            colname.strata = "Subtest", 
        #            colname.objects = "Person")
        # 
        # ds<- dstudy(g, colname.objects = "Person", data = Rajaratnam.2, colname.scores = "Score", 
        #             colname.strata = "Subtest")
        
        #-------------------------
        
        if (is.null(self$options$dep) ||
            is.null(self$options$id) ||
            is.null(self$options$facet)) return()
        
        
        dep <- self$options$dep
        id <- self$options$id
        sub <- self$options$sub
        facets <- self$options$facet
        
        data <- self$data
        data <- na.omit(data)
        data <- as.data.frame(data)
        
        ###### Generalizability theory--------------------
        
          # facet <- self$options$facet
          
          # Formula Example----------------------
          # formula <- "value ~ (1 | subject) +(1 | rater) + (1 | task) + 
          # (1 | subject:rater) +
          # (1 | rater:task) + 
          # (1 | subject:task)"
          
          # Example---------- 
          # vars <- c('A', 'B', 'C')  # you'll populate this from self$options$...
          # response <- 'bruce'
          # fmla <- as.formula(paste0(jmvcore::composeTerm(response), '~', paste(jmvcore::composeTerms(vars), collapse='*')))
          # trms <- attr(terms(fmla), 'term.labels')
          # trms[1:6] #example---
          # funnyTerms <- paste0('(1|', trms, ')')
          # finalFmla <- paste0(jmvcore::composeTerm(response), '~', paste(funnyTerms, collapse='+'))
          # finalFmla
          # 
          # 
          # vars <- c(self$options$id, self$options$facet)  
          # response <- self$options$dep
          # fmla <- as.formula(paste0(jmvcore::composeTerm(response), '~', paste(jmvcore::composeTerms(vars), collapse='*')))
          # trms <- attr(terms(fmla), 'term.labels')
          # funnyTerms <- paste0('(1|', trms, ')')
          # formula <- paste0(jmvcore::composeTerm(response), '~', paste(funnyTerms, collapse='+'))
          # 
          # 
          # self$results$text1$setContent(formula)
          # 
          
        #------------------------------------------------
          
        if(self$options$t=='uni'){
        
          formula <- self$options$formula
          
          gstudy.out<- gtheory::gstudy(data = data, 
                                       formula = formula)
                                    
          ds<- gtheory::dstudy(gstudy.out, 
                               colname.objects = id,
                               data = data, 
                               colname.scores = dep)
        
        # Univariate analysis----------
        # G study table----------------
          
          table<- self$results$g
          
          gstudy<- as.data.frame(gstudy.out)
          
          var<- as.vector(gstudy[[2]])
          percent<- as.vector(gstudy[[3]])
          n <- as.vector(gstudy[[4]])
          
          items <- as.vector(gstudy[[1]])
          
          for (i in seq_along(items)) {
            
            row <- list()
            
            row[["var"]] <-var[i]
            row[["percent"]] <- percent[i]
            row[["n"]] <- n[i]
            
            table$addRow(rowKey = items[i], values = row)
          }
          
          # G study table(Variance components)----------------
          
          table<- self$results$d
          
          dstudy<- as.data.frame(ds$components)
          
          var<- as.vector(dstudy[[2]])
          percent<- as.vector(dstudy[[3]])
          n <- as.vector(dstudy[[4]])
          
          items <- as.vector(dstudy[[1]])
          
          for (i in seq_along(items)) {
            
            row <- list()
            
            row[["var"]] <-var[i]
            row[["percent"]] <- percent[i]
            row[["n"]] <- n[i]
            
            table$addRow(rowKey = items[i], values = row)
          }
          
          # self$results$text2$setContent(dstudy.out$generalizability)
          
          # Measures of D study---------------
          gen <- ds$generalizability
          depe <- ds$dependability
          uni <- ds$var.universe
          rel <- ds$var.error.rel
          abs <- ds$var.error.abs
          
          
          if(isTRUE(self$options$mea)){
            
            table<- self$results$mea
            
            row <- list()
            
            row[['generalizability']] <- gen
            row[['dependability']] <- depe
            row[['universe']] <- uni
            row[['relative']] <- rel
            row[['absolute']] <- abs
            
            table$setRow(rowNo = 1, values = row)
            
          }
          
         
}
              
          if(self$options$t=='mul'){
            
            #-------------------------------------------------------------
            formula1 <- self$options$formula1
            
            g1<- gtheory::gstudy(data = data, formula = formula1,
                                colname.strata = self$options$sub, 
                                colname.objects = self$options$id)  
            
            ds1<- gtheory::dstudy(g1,
                                 colname.objects = id,
                                 colname.strata = sub,
                                 data = data,
                                 colname.scores = dep)
            #-----------------------------------------------------
            self$results$text$setContent(ds1) 

            if(isTRUE(self$options$item)) {
              
              ng <- self$options$ng
              res <- list() 
              
              for (i in  seq_along(1:ng)) {
                res.df <- lapply(g1$within[[as.character(i)]], as.data.frame)
                res[[i]] <- res.df
              }
              
              tab <- NULL
              
              for(i in seq_along(1:ng)) {
                re<- as.data.frame.matrix(res[[i]][['components']])
                tab[[i]] <- re
              }
              
              tab <- tab
              
              #--------------------------------------------------------------         
              tables <- self$results$item
              
              for(i in seq_along(1:ng)){
                table <- tables[[i]]
                item <- tab[[i]]
                
                for(rNo in 1:3)
                  table$setRow(rowNo=rNo,
                               values=list('source'=item$source[[rNo]],
                                           'var'=item$var[[rNo]],
                                           'percent'=item$percent[[rNo]],
                                           'n'=item$n[[rNo]]))
              }
            }
           
  # D study: variance components--------
            
            if(isTRUE(self$options$itemd)) {
              
              ng <- self$options$ng
              res <- list() 
              
              for (i in  seq_along(1:ng)) {
                res.df <- lapply(ds1$within[[as.character(i)]], as.data.frame)
                res[[i]] <- res.df
              }
              
              tab <- NULL
              
              for(i in seq_along(1:ng)) {
                re<- as.data.frame.matrix(res[[i]][['components']])
                tab[[i]] <- re
              }
              
              tab <- tab
              
              #--------------------------------------------------------------         
              tables <- self$results$itemd
              
              for(i in seq_along(1:ng)){
                table <- tables[[i]]
                item <- tab[[i]]
                
                for(rNo in 1:3)
                  table$setRow(rowNo=rNo,
                               values=list('source'=item$source[[rNo]],
                                           'var'=item$var[[rNo]],
                                           'percent'=item$percent[[rNo]],
                                           'n'=item$n[[rNo]]))
              }
            }
            
            
            
          # D study (Composite table)------------
         
            if(isTRUE(self$options$comp)){
               
            table <- self$results$comp
             
            # Measures of D study---------------
            gen<- as.vector(ds1$composite$generalizability)
            depe<- as.vector(ds1$composite$dependability)
            uni<- as.vector(ds1$composite$var.universe)
            rel<- as.vector(ds1$composite$var.error.rel)
            abs<- as.vector(ds1$composite$var.error.abs)
            
              row <- list()
              
              row[['generalizability']] <- gen
              row[['dependability']] <- depe
              row[['universe']] <- uni
              row[['relative']] <- rel
              row[['absolute']] <- abs
              
              table$setRow(rowNo = 1, values = row)
              
            } 
   
            if(isTRUE(self$options$bm)){
              
              table <- self$results$bm
              
              # measure---
              gen<- ds1$between$generalizability
              Generalizability<- diag(gen)
              gen<- as.data.frame(Generalizability)
              
              dep<- ds1$between$dependability
              Dependability<- diag(dep)
              depe<- as.data.frame(Dependability)
              
              rel<- ds1$between$var.error.rel
              Relative<- diag(rel)
              rel<- as.data.frame(Relative)
              
              abs<- ds1$between$var.error.abs
              Absolute<- diag(abs)
              abs<- as.data.frame(Absolute)
              
              all<- data.frame(gen,depe,rel,abs)
              #-----------
              names<- dimnames(all)[[1]]
              
              for (name in names) {
                
                row <- list()
                
                row[['gen']] <- all[name,1]
                row[['depe']] <- all[name,2]
                row[['rel']] <- all[name,3]
                row[['abs']] <- all[name,4]
                
                table$addRow(rowKey=name, values=row)
                
              }
              
              
            }
            
            
            
            
                      
 }  
             

          
          
 
        })
)
