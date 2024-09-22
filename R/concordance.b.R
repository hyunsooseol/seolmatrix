
# This file is a generated template, your changes will not be overwritten

#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @export

concordanceClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "concordanceClass",
    inherit = concordanceBase,
    private = list(
      .htmlwidget = NULL,  # Add instance for HTMLWidget
        
        .init = function() {
            
          private$.htmlwidget <- HTMLWidget$new() # Initialize the HTMLWidget instance 
          
            if (is.null(self$data) | is.null(self$options$dep)  | is.null(self$options$covs)) {
                
                self$results$instructions$setVisible(visible = TRUE)
                
            }
            
            # self$results$instructions$setContent(
            #     "<html>
            # <head>
            # </head>
            # <body>
            # <div class='instructions'>
            # <p>____________________________________________________________________________________</p>
            # <p>1. The rationale of Concordance correlation is described in the <a href='https://r-bloggers.com/2020/01/concordance-correlation-coefficient/?fbclid=IwAR2Txi_QrFTuDB9jH8NiJW8dEde_lw2Td08XqxNzoWqut9m8E-bE5RHUDiI' target = '_blank'>page.</a></p>
            # <p>2. Feature requests and bug reports can be made on the <a href='https://github.com/hyunsooseol/seolmatrix/issues'  target = '_blank'>GitHub.</a></p>
            # <p>____________________________________________________________________________________</p>
            # </div>
            # </body>
            # </html>"
            # )
            
          self$results$instructions$setContent(
            private$.htmlwidget$generate_accordion(
              title="Instructions",
              content = paste(
                '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
                '<div style="text-align:justify;">',
                '<ul>',
                '<li>The rationale of Concordance correlation is described in the <a href="https://r-bloggers.com/2020/01/concordance-correlation-coefficient/?fbclid=IwAR2Txi_QrFTuDB9jH8NiJW8dEde_lw2Td08XqxNzoWqut9m8E-bE5RHUDiI" target = "_blank">page</a>.</li>',
                '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/seolmatrix/issues" target="_blank">GitHub</a>.</li>',
                '</ul></div></div>'
                
              )
              
            )
          )         
   
            if(isTRUE(self$options$ccp)){
              width <- self$options$width
              height <- self$options$height
              self$results$plot$setSize(width, height)
            }
            
            if(isTRUE(self$options$bap)){
              width <- self$options$width1
              height <- self$options$height1
              self$results$plot1$setSize(width, height)
            }  
            
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
                   
#To work in Linux####################################

#seolmatrix imports the epiR package, which in turn imports the sf package ... 
# we can't build sf under linux at this time                    
# Therefore, delete epiR R package and added epi.ccc function directly.                     

                    #epi.ccc(  ) source codes##################################
 
                    epi.ccc = function(x, y, ci = "z-transform", conf.level = 0.95, rep.measure = FALSE, subjectid){
                      
                      N. <- 1 - ((1 - conf.level) / 2)
                      zv <- qnorm(N., mean = 0, sd = 1)
                      
                      dat <- data.frame(x, y)
                      id <- complete.cases(dat)
                      nmissing <- sum(!complete.cases(dat))
                      dat <- dat[id,]
                      
                      k <- length(dat$y)
                      yb <- mean(dat$y)
                      sy2 <- var(dat$y) * (k - 1) / k
                      sd1 <- sd(dat$y)
                      
                      xb <- mean(dat$x)
                      sx2 <- var(dat$x) * (k - 1) / k
                      sd2 <- sd(dat$x)
                      
                      r <- stats::cor(dat$x, dat$y)
                      sl <- r * sd1 / sd2
                      
                      sxy <- r * sqrt(sx2 * sy2)
                      p <- 2 * sxy / (sx2 + sy2 + (yb - xb)^2)
                      
                      delta <- (dat$x - dat$y)
                      rmean <- apply(dat, MARGIN = 1, FUN = mean)
                      blalt <- data.frame(mean = rmean, delta)
                      
                      # Scale shift:
                      v <- sd1 / sd2
                      # Location shift relative to the scale:
                      u <- (yb - xb) / ((sx2 * sy2)^0.25)
                      # Variable C.b is a bias correction factor that measures how far the best-fit line deviates from a line at 45 degrees (a measure of accuracy). 
                      # No deviation from the 45 degree line occurs when C.b = 1. See Lin (1989 page 258).
                      # C.b <- (((v + 1) / (v + u^2)) / 2)^-1
                      
                      # The following taken from the Stata code for function "concord" (changed 290408):
                      C.b <- p / r
                      
                      # Variance, test, and CI for asymptotic normal approximation (per Lin [March 2000] Biometrics 56:325-5):
                      sep <- sqrt(((1 - ((r)^2)) * (p)^2 * (1 - ((p)^2)) / (r)^2 + (2 * (p)^3 * (1 - p) * (u)^2 / r) - 0.5 * (p)^4 * (u)^4 / (r)^2 ) / (k - 2))
                      ll <- p - (zv * sep)
                      ul <- p + (zv * sep)
                      
                      # Statistic, variance, test, and CI for inverse hyperbolic tangent transform to improve asymptotic normality:
                      t <- log((1 + p) / (1 - p)) / 2
                      set = sep / (1 - ((p)^2))
                      llt = t - (zv * set)
                      ult = t + (zv * set)
                      llt = (exp(2 * llt) - 1) / (exp(2 * llt) + 1)
                      ult = (exp(2 * ult) - 1) / (exp(2 * ult) + 1)
                      
                      # Calculate delta.sd if repeated measures:
                      if(rep.measure == TRUE){  
                        # Make sure subject is a factor:
                        dat$sub <- subjectid
                        if(!is.factor(dat$sub)) dat$sub <- as.factor(dat$sub)
                        
                        # Number of subjects:
                        nsub <- length(levels(dat$sub))      
                        
                        # One way analysis of variance:
                        model <- aov(delta ~ dat$sub)           
                        
                        # Degrees of freedom:
                        MSB <- anova(model)[[3]][1]       
                        
                        # Sums of squares:
                        MSW <- anova(model)[[3]][2]       
                        
                        # Calculate number of complete pairs for each subject:
                        pairs <- NULL
                        
                        for(i in 1:nsub){
                          pairs[i] <- sum(is.na(delta[dat$sub == levels(dat$sub)[i]]) == FALSE)
                        }
                        
                        sig.dl <- (MSB - MSW) / ((sum(pairs)^2 - sum(pairs^2)) / ((nsub - 1) * sum(pairs)))
                        delta.sd <- sqrt(sig.dl + MSW)
                      }
                      
                      # Calculate delta.sd if no repeated measures:
                      if(rep.measure == FALSE){
                        delta.sd <- sqrt(var(delta, na.rm = TRUE))
                      }
                      
                      # Upper and lower bounds for Bland Altmann plot:
                      ba.p <- mean(delta)
                      ba.l <- ba.p - (zv * delta.sd)
                      ba.u <- ba.p + (zv * delta.sd)
                      sblalt <- data.frame("est" = ba.p, "delta.sd" = delta.sd, "lower" = ba.l, "upper" = ba.u) 
                      
                      if(ci == "asymptotic"){
                        rho.c <- data.frame(p, ll, ul)
                        names(rho.c) <- c("est", "lower", "upper")
                        rval <- list(rho.c = rho.c, s.shift = v, l.shift = u, C.b = C.b, blalt = blalt, sblalt = sblalt, nmissing = nmissing)  
                      }
                      
                      else if(ci == "z-transform"){
                        rho.c <- data.frame(p, llt, ult)
                        names(rho.c) <- c("est", "lower", "upper")
                        rval <- list(rho.c = rho.c, s.shift = v, l.shift = u, C.b = C.b, blalt = blalt, sblalt = sblalt, nmissing = nmissing)
                      }
                      
                      return(rval)
                    }  
                    
                    # Example-------------
                    
                    # x <- c(494,395,516,434,476,557,413,442,650,433,417,656,267,
                    #        478,178,423,427)
                    # 
                    # y <- c(512,430,520,428,500,600,364,380,658,445,432,626,260,
                    #        477,259,350,451)
                    # 
                    # res<- epi.ccc(x,y)
                    # res
                    
                    
                     
                    #######################################################
                    tmp.ccc <- epi.ccc(data[[dep]], data[[covs]])
                    ######################################################
                   
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
            
          if (is.null(image1$state))
            return(FALSE)
        
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
          
          if (is.null(image$state))
            return(FALSE)
            
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
