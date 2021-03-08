
# This file is a generated template, your changes will not be overwritten

#' @importFrom R6 R6Class
#' @import jmvcore
#' @import epiR
#' @importFrom epiR epi.ccc
#' @import ggplot2
#' @export

concordanceClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "concordanceClass",
    inherit = concordanceBase,
    private = list(

        
        .init = function() {
            
            if (is.null(self$data) | is.null(self$options$dep)  | is.null(self$options$covs)) {
                
                self$results$instructions$setVisible(visible = TRUE)
                
            }
            
            self$results$instructions$setContent(
                "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p> The rationale of Concordance correlation is described in the <a href='https://r-bloggers.com/2020/01/concordance-correlation-coefficient/?fbclid=IwAR2Txi_QrFTuDB9jH8NiJW8dEde_lw2Td08XqxNzoWqut9m8E-bE5RHUDiI' target = '_blank'>page.</a></p>
            <p> Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/seolmatrix/'  target = '_blank'>GitHub.</a></p>

            </div>
            </body>
            </html>"
            )
            
                   },
        
        
        .run = function() {

                    
                    if (length(self$options$dep)<1) return()

                    if (length(self$options$covs)<1) return()

                    
                    #get the data--------
                    
                    data <- self$data
                    
                    dep <- self$options$dep
                    
                    covs <- self$options$covs
                    
                    
                    # get the data
                    
                    data <- self$data
                    
                    # convert to appropriate data types
                    
                    data[[dep]] <- jmvcore::toNumeric(data[[dep]])
                    
                    data[[covs]] <- jmvcore::toNumeric(data[[covs]])
                    
                    
                    data <- na.omit(data)
                    
                    
                    tmp.ccc <- epiR::epi.ccc(data[[dep]], data[[covs]])
                    
                    
                    # epi.ccc(x, y, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE, 
                    #         subjectid)   
                    # 
                    
                    table<- self$results$table
                    
                    r <-  tmp.ccc$rho.c[[1]]
                    lower <-  tmp.ccc$rho.c[[2]]
                    upper<-  tmp.ccc$rho.c[[3]]  
                    
                    row <- list()
                    
                    row[['r']] <- r
                    row[['lower']] <- lower
                    row[['upper']] <- upper
                    
                    table$setRow(rowNo = 1, values = row)
                    
                    ## plot---------------------------------
                    
                    tmp <- data.frame(data[[dep]], data[[covs]])
                    
                    # tmp.lab <- data.frame(lab = paste("CCC: ",
                    #                                   round(tmp.ccc$rho.c[,1], digits = 2), " (95% CI ",
                    #                                   round(tmp.ccc$rho.c[,2], digits = 2), " - ",
                    #                                   round(tmp.ccc$rho.c[,3], digits = 2), ")", sep = ""))
                    # 
                    z <- lm( data[[covs]] ~ data[[dep]])
                    alpha <- summary(z)$coefficients[1,1]
                    beta <-  summary(z)$coefficients[2,1]
                    tmp.lm <- data.frame(alpha, beta)
                    
                    dep <- data[[dep]]
                    covs <- data[[covs]]
                    
                    
                    image <- self$results$plot
                    
                    state <- list(z, alpha, beta, tmp.lm, tmp, dep,covs )
                    
                    image$setState(state)
                    
                    ## plot1-------------------------
                    
                   
                    tmp1 <- data.frame(mean = tmp.ccc$blalt[,1], delta = tmp.ccc$blalt[,2])
                    
                    est <- tmp.ccc$sblalt$est
                    lower <- tmp.ccc$sblalt$lower
                    upper <- tmp.ccc$sblalt$upper     
                    
                    mean = tmp.ccc$blalt[,1] 
                    delta = tmp.ccc$blalt[,2]
                    
                    dat<- tmp.ccc$sblalt
                    
                    #########
                    
                    image1 <- self$results$plot1
                        
                    state <- list(tmp1, est, lower, upper, mean, delta,dat)
                    
                    image1$setState(state)    
                     
                    
                    },
        
        .plot1 = function(image1, ggtheme, theme, ...) {
            
            if (length(self$options$dep)<1) return()
            
            if (length(self$options$covs)<1) return()
        
            tmp1 <- image1$state[[1]]
            est     <- image1$state[[2]]
            lower  <- image1$state[[3]]
            upper  <- image1$state[[4]]
           mean <- image1$state[[5]]
            delta <- image1$state[[6]]
            dat <- image1$state[[7]]
            
            plot1<- ggplot(tmp1, aes(x = mean, y = delta)) + 
                geom_point() +
                geom_hline(data = dat, aes(yintercept = lower), linetype = 2) +
                geom_hline(data =  dat, aes(yintercept = upper), linetype = 2) +
                geom_hline(data =  dat, aes(yintercept = est), linetype = 1) +
                scale_x_continuous(name = "Average between measures") +
                scale_y_continuous(name = "Difference between measures")   
            
            plot1 <- plot1 + ggtheme
            print(plot1)
            TRUE
        },
        
        
        .plot = function(image, ggtheme, theme, ...) {
        
            if (length(self$options$dep)<1) return()
            
            if (length(self$options$covs)<1) return()
            
          #  tmp.lab <- image$state[[1]]
            z     <- image$state[[1]]
            alpha  <- image$state[[2]]
            beta  <- image$state[[3]]
            tmp.lm <- image$state[[4]]
            tmp <- image$state[[5]]
            dep <- image$state[[6]]
            covs <- image$state[[7]]
            
            
            
            plot <- ggplot(tmp,aes(x = dep, y = covs)) + 
                geom_point() +
                geom_abline(intercept = 0, slope = 1) +
                geom_abline(data = tmp.lm, aes(intercept = alpha, slope = beta), 
                            linetype = "dashed") +
                scale_x_continuous(name = self$options$dep) +
                scale_y_continuous(name = self$options$covs ) +
            #    geom_text(data = tmp.lab, x = 0.5, y = 2.95, label = tmp.lab$lab) + 
                coord_fixed(ratio = 1 / 1)
            
            plot <- plot+ggtheme
            print(plot)
            TRUE
        }
            
         
         )
)
