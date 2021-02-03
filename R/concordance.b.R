
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
                    
                    tmp <- data.frame(data[[dep]], data[[covs]])
                    
                    tmp.lab <- data.frame(lab = paste("CCC: ",
                                                      round(tmp.ccc$rho.c[,1], digits = 2), " (95% CI ",
                                                      round(tmp.ccc$rho.c[,2], digits = 2), " - ",
                                                      round(tmp.ccc$rho.c[,3], digits = 2), ")", sep = ""))
                    
                    z <- lm( data[[covs]] ~ data[[dep]])
                    alpha <- summary(z)$coefficients[1,1]
                    beta <-  summary(z)$coefficients[2,1]
                    tmp.lm <- data.frame(alpha, beta)
                    
                    dep <- data[[dep]]
                    covs <- data[[covs]]
                    
                    
                    image <- self$results$plot
                    
                    state <- list(tmp.lab, z, alpha, beta, tmp.lm, tmp, dep,covs )
                    
                    image$setState(state)
                    
                    
                    },
        
        .plot = function(image, ggtheme, theme, ...) {
        
            if (length(self$options$dep)<1) return()
            
            if (length(self$options$covs)<1) return()
            
            tmp.lab <- image$state[[1]]
            z     <- image$state[[2]]
            alpha  <- image$state[[3]]
            beta  <- image$state[[4]]
            tmp.lm <- image$state[[5]]
            tmp <- image$state[[6]]
            dep <- image$state[[7]]
            covs <- image$state[[8]]
            
            
            
            plot <- ggplot(tmp,aes(x = dep, y = covs)) + 
                geom_point() +
                geom_abline(intercept = 0, slope = 1) +
                geom_abline(data = tmp.lm, aes(intercept = alpha, slope = beta), 
                            linetype = "dashed") +
                scale_x_continuous(limits = c(0,3), name = "Measure 1") +
                scale_y_continuous(limits = c(0,3), name = "Measure 2") +
                geom_text(data = tmp.lab, x = 0.5, y = 2.95, label = tmp.lab$lab) + 
                coord_fixed(ratio = 1 / 1)
            
            plot <- plot+ggtheme
            print(plot)
            TRUE
        }
            
         
         )
)
