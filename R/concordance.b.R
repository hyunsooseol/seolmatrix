
concordanceClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "concordanceClass",
    inherit = concordanceBase,
    private = list(
      .htmlwidget = NULL,
      # Add instance for HTMLWidget
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new() # Initialize the HTMLWidget instance
        
        if (is.null(self$data) |
            is.null(self$options$dep)  |
            is.null(self$options$covs)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>The rationale of Concordance correlation is described in the <a href="https://r-bloggers.com/2020/01/concordance-correlation-coefficient/?fbclid=IwAR2Txi_QrFTuDB9jH8NiJW8dEde_lw2Td08XqxNzoWqut9m8E-bE5RHUDiI" target = "_blank">page</a>.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/seolmatrix/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
        ))
        
        if (isTRUE(self$options$ccp)) {
          width <- self$options$width
          height <- self$options$height
          self$results$plot$setSize(width, height)
        }
        
        if (isTRUE(self$options$bap)) {
          width <- self$options$width1
          height <- self$options$height1
          self$results$plot1$setSize(width, height)
        }
        
      },
      
      .run = function() {
        data <- self$data
        if (isTRUE(self$options$cc) ||
            isTRUE(self$options$ccp) ||
            isTRUE(self$options$bap)) {
          if (length(self$options$dep) >= 1 &&
              length(self$options$covs) >= 1) {
            #get the data--------
            #data <- self$data
            dep <- self$options$dep
            covs <- self$options$covs
            
            # convert to appropriate data types
            data[[dep]] <- jmvcore::toNumeric(data[[dep]])
            data[[covs]] <- jmvcore::toNumeric(data[[covs]])
            
            data <- na.omit(data)
            
            #To work in Linux####################################
            
            #seolmatrix imports the epiR package, which in turn imports the sf package ...
            # we can't build sf under linux at this time
            # Therefore, delete epiR R package and added epi.ccc function directly.
            tmp.ccc <- private$.computeCCC(data[[dep]], data[[covs]])
            
            ######################################################
            
            if (isTRUE(self$options$cc)) {
              table <- self$results$cc
              r <-  tmp.ccc$rho.c[[1]]
              lower <-  tmp.ccc$rho.c[[2]]
              upper <-  tmp.ccc$rho.c[[3]]
              row <- list()
              row[['r']] <- r
              row[['lower']] <- lower
              row[['upper']] <- upper
              table$setRow(rowNo = 1, values = row)
            }
            
            if (isTRUE(self$options$ccp)) {
              image <- self$results$plot
              image$setState(list(
                dep = dep,
                covs = covs,
                data_dep = data[[dep]],
                data_covs = data[[covs]]
              ))
            }
            
            if (isTRUE(self$options$bap)) {
              image1 <- self$results$plot1
              image1$setState(
                list(
                  est = tmp.ccc$sblalt$est,
                  lower = tmp.ccc$sblalt$lower,
                  upper = tmp.ccc$sblalt$upper,
                  blalt = tmp.ccc$blalt
                )
              )
            }
            
          }
        }
        if (isTRUE(self$options$mat)) {
          variables <- self$options$vars
          
          if (length(variables) < 2) return()
          
          data_subset <- as.data.frame(lapply(variables, function(var)
            data[[var]]))
          colnames(data_subset) <- variables
          n_vars <- length(variables)
          
          ccc_mat <- matrix(NA, nrow = n_vars, ncol = n_vars)
          rownames(ccc_mat) <- variables
          colnames(ccc_mat) <- variables
          
          for (i in 1:n_vars) {
            for (j in 1:n_vars) {
              if (i == j) {
                ccc_mat[i, j] <- 1
              } else {
                x <- data_subset[[i]]
                y <- data_subset[[j]]
                
                valid_indices <- !is.na(x) & !is.na(y)
                if (sum(valid_indices) < 2) {
                  ccc_mat[i, j] <- NA
                } else {
                  x <- x[valid_indices]
                  y <- y[valid_indices]
                  # 누락된 CCC 계산 부분 추가
                  tmp_ccc <- private$.computeCCC(x, y)
                  ccc_mat[i, j] <- tmp_ccc$rho.c[[1]]  # CCC 추정값만 저장
                }
              }
            }
          }
          
          res <- as.data.frame(ccc_mat)
          names <- dimnames(res)[[1]]
          dims <- dimnames(res)[[2]]
          table <- self$results$mat
          
           # creating table----------------
          for (dim in dims) {
            table$addColumn(name = paste0(dim), type = 'number')
          }
          
           for (name in names) {
            row <- list()
            for (j in seq_along(dims)) {
              row[[dims[j]]] <- res[name, j]
            }
            table$addRow(rowKey = name, values = row)
          }
          
        }
        #self$results$text$setContent(ccc_text)
      },
      .computeCCC = function(x, y, rep.measure = FALSE, subjectid = NULL) {
        #epi.ccc(  ) source codes##################################
        
        epi.ccc = function(x,
                           y,
                           ci = "z-transform",
                           conf.level = 0.95,
                           rep.measure = FALSE,
                           subjectid) {
          N. <- 1 - ((1 - conf.level) / 2)
          zv <- qnorm(N., mean = 0, sd = 1)
          
          dat <- data.frame(x, y)
          id <- complete.cases(dat)
          nmissing <- sum(!complete.cases(dat))
          dat <- dat[id, ]
          
          k <- length(dat$y)
          
          # 데이터가 충분하지 않은 경우 처리
          if (k < 2) {
            rho.c <- data.frame(NA, NA, NA)
            names(rho.c) <- c("est", "lower", "upper")
            return(list(
              rho.c = rho.c,
              s.shift = NA,
              l.shift = NA,
              C.b = NA,
              blalt = data.frame(mean = numeric(0), delta = numeric(0)),
              sblalt = data.frame(est = NA, delta.sd = NA, lower = NA, upper = NA),
              nmissing = nmissing
            ))
          }
          
          yb <- mean(dat$y)
          sy2 <- var(dat$y) * (k - 1) / k
          sd1 <- sd(dat$y)
          
          xb <- mean(dat$x)
          sx2 <- var(dat$x) * (k - 1) / k
          sd2 <- sd(dat$x)
          
          r <- stats::cor(dat$x, dat$y)
          sl <- r * sd1 / sd2
          
          sxy <- r * sqrt(sx2 * sy2)
          p <- 2 * sxy / (sx2 + sy2 + (yb - xb) ^ 2)
          
          delta <- (dat$x - dat$y)
          rmean <- apply(dat, MARGIN = 1, FUN = mean)
          blalt <- data.frame(mean = rmean, delta)
          
          # Scale shift:
          v <- sd1 / sd2
          # Location shift relative to the scale:
          u <- (yb - xb) / ((sx2 * sy2) ^ 0.25)
          # Variable C.b is a bias correction factor that measures how far the best-fit line deviates from a line at 45 degrees (a measure of accuracy).
          # No deviation from the 45 degree line occurs when C.b = 1. See Lin (1989 page 258).
          # C.b <- (((v + 1) / (v + u^2)) / 2)^-1
          
          # The following taken from the Stata code for function "concord" (changed 290408):
          C.b <- p / r
          
          # Variance, test, and CI for asymptotic normal approximation (per Lin [March 2000] Biometrics 56:325-5):
          sep <- sqrt(((1 - ((r) ^ 2)) * (p) ^ 2 * (1 - ((p) ^
                                                           2)) / (r) ^ 2 + (2 * (p) ^ 3 * (1 - p) * (u) ^ 2 / r) - 0.5 * (p) ^ 4 * (u) ^
                         4 / (r) ^ 2
          ) / (k - 2))
          ll <- p - (zv * sep)
          ul <- p + (zv * sep)
          
          # Statistic, variance, test, and CI for inverse hyperbolic tangent transform to improve asymptotic normality:
          t <- log((1 + p) / (1 - p)) / 2
          set = sep / (1 - ((p) ^ 2))
          llt = t - (zv * set)
          ult = t + (zv * set)
          llt = (exp(2 * llt) - 1) / (exp(2 * llt) + 1)
          ult = (exp(2 * ult) - 1) / (exp(2 * ult) + 1)
          
          delta.sd <- sqrt(var(delta, na.rm = TRUE))
          
          # Upper and lower bounds for Bland Altmann plot:
          ba.p <- mean(delta)
          ba.l <- ba.p - (zv * delta.sd)
          ba.u <- ba.p + (zv * delta.sd)
          sblalt <- data.frame(
            "est" = ba.p,
            "delta.sd" = delta.sd,
            "lower" = ba.l,
            "upper" = ba.u
          )
          
          if (ci == "asymptotic") {
            rho.c <- data.frame(p, ll, ul)
            names(rho.c) <- c("est", "lower", "upper")
            rval <- list(
              rho.c = rho.c,
              s.shift = v,
              l.shift = u,
              C.b = C.b,
              blalt = blalt,
              sblalt = sblalt,
              nmissing = nmissing
            )
          }
          
          else if (ci == "z-transform") {
            rho.c <- data.frame(p, llt, ult)
            names(rho.c) <- c("est", "lower", "upper")
            rval <- list(
              rho.c = rho.c,
              s.shift = v,
              l.shift = u,
              C.b = C.b,
              blalt = blalt,
              sblalt = sblalt,
              nmissing = nmissing
            )
          }
          return(rval)
        }
        return(epi.ccc(x, y))
      },
      
      .plot1 = function(image1, ggtheme, theme, ...) {
        if (is.null(image1$state))
          return(FALSE)
        
        est <- image1$state$est
        lower <- image1$state$lower
        upper <- image1$state$upper
        blalt <- image1$state$blalt
        
        tmp1 <- data.frame(mean = blalt[, 1], delta = blalt[, 2])
        dat <- data.frame(
          est = est,
          lower = lower,
          upper = upper
        )
        
        library(ggplot2)
        plot1 <- ggplot2::ggplot(tmp1, aes(x = mean, y = delta)) +
          geom_point() +
          geom_hline(data = dat,
                     aes(yintercept = lower),
                     linetype = 2) +
          geom_hline(data = dat,
                     aes(yintercept = upper),
                     linetype = 2) +
          geom_hline(data = dat, aes(yintercept = est), linetype = 1) +
          scale_x_continuous(name = "Average between measures") +
          scale_y_continuous(name = "Difference between measures")
        
        plot1 <- plot1 + ggtheme
        print(plot1)
        TRUE
      },
      
      .plot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        dep_name <- self$options$dep
        covs_name <- self$options$covs
        dep_data <- image$state$data_dep
        covs_data <- image$state$data_covs
        
        z <- lm(covs_data ~ dep_data)
        alpha <- summary(z)$coefficients[1, 1]
        beta <- summary(z)$coefficients[2, 1]
        
        tmp <- data.frame(dep = dep_data, covs = covs_data)
        tmp.lm <- data.frame(alpha = alpha, beta = beta)
        
        library(ggplot2)
        plot <- ggplot2::ggplot(tmp, aes(x = dep, y = covs)) +
          geom_point() +
          geom_abline(intercept = 0, slope = 1) +
          geom_abline(data = tmp.lm,
                      aes(intercept = alpha, slope = beta),
                      linetype = "dashed") +
          scale_x_continuous(name = dep_name) +
          scale_y_continuous(name = covs_name) +
          coord_fixed(ratio = 1 / 1)
        
        plot <- plot + ggtheme
        print(plot)
        TRUE
      }
    )
  )
