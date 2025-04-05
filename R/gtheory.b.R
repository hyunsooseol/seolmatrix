
gtheoryClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "gtheoryClass",
    inherit = gtheoryBase,
    private = list(
      .htmlwidget = NULL,
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) | is.null(self$options$facet)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Perform Univariate or Multivariate Generalizability theory based on <b>gtheory</b> R package.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/seolmatrix/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
          
        ))
        if (isTRUE(self$options$plot1)) {
          width <- self$options$width
          height <- self$options$height
          self$results$plot1$setSize(width, height)
        }
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
            is.null(self$options$facet))
          return()
        
        
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
        
        if (self$options$t == 'uni') {
          formula <- self$options$formula
          
          gstudy.out <- gtheory::gstudy(data = data, formula = formula)
          
          ds <- gtheory::dstudy(
            gstudy.out,
            colname.objects = id,
            data = data,
            colname.scores = dep
          )
          
          # Univariate analysis(G study)----------
        if(isTRUE(self$options$g)){         
          
          table <- self$results$g
          gstudy <- as.data.frame(gstudy.out)
          
          for (i in seq_len(nrow(gstudy))) {
            table$addRow(
              rowKey = gstudy[i, 1],
              values = list(
                var = gstudy[[2]][i],
                percent = gstudy[[3]][i],
                n = gstudy[[4]][i]
              )
            )
          }
          # var <- as.vector(gstudy[[2]])
          # percent <- as.vector(gstudy[[3]])
          # n <- as.vector(gstudy[[4]])
          # 
          # items <- as.vector(gstudy[[1]])
          # 
          # for (i in seq_along(items)) {
          #   row <- list()
          #   
          #   row[["var"]] <- var[i]
          #   row[["percent"]] <- percent[i]
          #   row[["n"]] <- n[i]
          #   
          #   table$addRow(rowKey = items[i], values = row)
          # }
        }  
          #  Single observation study----------
          # how many raters do I need?
          # Example----------
          #source: https://rpubs.com/YanweXu/672524
          
          # library(gtheory)
          # Rating <- read.csv("univariate.csv")
          # varComp <- gstudy(Rating, score ~ (1|ID) + (1|rater))
          #
          # r1 <- dstudy(varComp, colname.objects = "ID")
          # r1$components
          # r1$generalizability
          #
          # how many rater do I need ?
          
          # k <- 1
          # r1 <- r1$var.universe/(r1$var.universe + (r1$var.error.rel/k))
          # r1
          # k<-12
          # 0.62/(0.62 +2.787/12)
          
          if (isTRUE(self$options$gmea)) {
            gmea <- gtheory::dstudy(gstudy.out, colname.objects = self$options$id)

            table <- self$results$gmea
            table$setRow(
              rowNo = 1,
              values = with(gmea, list(
                generalizability = generalizability,
                dependability = dependability,
                universe = var.universe,
                relative = var.error.rel,
                absolute = var.error.abs
              )))
          }
            
          # D study table(Variance components)----------------
        if (isTRUE(self$options$d)){
          table <- self$results$d
          dstudy <- as.data.frame(ds$components)
          
          for (i in seq_len(nrow(dstudy))) {
            table$addRow(
              rowKey = dstudy[i, 1],
              values = list(
                var = dstudy[[2]][i],
                percent = dstudy[[3]][i],
                n = dstudy[[4]][i]
              )
            )
            }}

        if (isTRUE(self$options$mea)) {
            table <- self$results$mea
            
            table$setRow(
              rowNo = 1,
              values = with(ds, list(
                generalizability = generalizability,
                dependability = dependability,
                universe = var.universe,
                relative = var.error.rel,
                absolute = var.error.abs
              )))
          }
          
          # D study plot(n=1)----------------
          
          # if(isTRUE(self$options$plot1)){
          
          if (length(self$options$facet) == 1) {
            if (length(self$options$facet) > 1) return()
            
            # m<- lme4::lmer(self$options$formula, data = data)
            # gmodel <- hemp::gstudy(m)
            # #self$results$text$setContent(gmodel)
            # image <- self$results$plot1
            #
            # nvars <- length(1:self$options$nf)
            # width <- 400 + nvars * 15
            # image$setSize(width, 400)
            #
            # image$setState(gmodel)
            #
            nf <- self$options$nf
            gco <- self$options$gco
            gmea <- gtheory::dstudy(gstudy.out, colname.objects = self$options$id)
            tex <- gmea$var.universe / (gmea$var.universe + (gmea$var.error.rel / nf))
            self$results$text$setContent(tex)
            
            if (!gco == FALSE) {
              tex <- gmea$var.universe / (gmea$var.universe + (gmea$var.error.abs / nf))
              self$results$text$setContent(tex)
            }
          }
        }
        
        if (self$options$t == 'mul') {
          #-------------------------------------------------------------
          formula1 <- self$options$formula1
          
          g1 <- gtheory::gstudy(
            data = data,
            formula = formula1,
            colname.strata = self$options$sub,
            colname.objects = self$options$id
          )
          
          ds1 <- gtheory::dstudy(
            g1,
            colname.objects = id,
            colname.strata = sub,
            data = data,
            colname.scores = dep
          )
          #-----------------------------------------------------
          # self$results$text$setContent(ds1)
          
          if (isTRUE(self$options$item)) {
            ng <- self$options$ng
            res <- list()
            
            for (i in  seq_along(1:ng)) {
              res.df <- lapply(g1$within[[as.character(i)]], as.data.frame)
              res[[i]] <- res.df
            }
            tab <- NULL
            for (i in seq_along(1:ng)) {
              re <- as.data.frame.matrix(res[[i]][['components']])
              tab[[i]] <- re
            }
            tab <- tab
            tables <- self$results$item
            for (i in seq_along(1:ng)) {
              table <- tables[[i]]
              item <- tab[[i]]
              
              for (rNo in 1:3)
                table$setRow(
                  rowNo = rNo,
                  values = list(
                    'source' = item$source[[rNo]],
                    'var' = item$var[[rNo]],
                    'percent' = item$percent[[rNo]],
                    'n' = item$n[[rNo]]
                  )
                )
            }
       
          }
          # G study: Observed variance and covariance matrix----------
          
          if (isTRUE(self$options$mat)) {
            mat <- g1$between$var.obs
            mat <- as.data.frame(mat)
            names <- dimnames(mat)[[1]]
            dims <- dimnames(mat)[[2]]
            
            table <- self$results$mat

            for (dim in dims) {
              table$addColumn(name = paste0(dim), type = 'number')
            }
            
            for (name in names) {
              row <- list()
              for (j in seq_along(dims)) {
                row[[dims[j]]] <- mat[name, j]
              }
              table$addRow(rowKey = name, values = row)
            }
          }
          
          # D study: variance components--------
          
          if (isTRUE(self$options$itemd)) {
            ng <- self$options$ng
            res <- list()
            
            for (i in  seq_along(1:ng)) {
              res.df <- lapply(ds1$within[[as.character(i)]], as.data.frame)
              res[[i]] <- res.df
            }
            tab <- NULL
            for (i in seq_along(1:ng)) {
              re <- as.data.frame.matrix(res[[i]][['components']])
              tab[[i]] <- re
            }
            tab <- tab
            tables <- self$results$itemd
            
            for (i in seq_along(1:ng)) {
              table <- tables[[i]]
              item <- tab[[i]]
              
              for (rNo in 1:3)
                table$setRow(
                  rowNo = rNo,
                  values = list(
                    'source' = item$source[[rNo]],
                    'var' = item$var[[rNo]],
                    'percent' = item$percent[[rNo]],
                    'n' = item$n[[rNo]]
                  )
                )
            }
          }
          
          # D study: Between universe score variance matrix----------
          
          if (isTRUE(self$options$bmat)) {
            bmat <- ds1$between$var.universe
            bmat <- as.data.frame(mat)
            
            names <- dimnames(bmat)[[1]]
            dims <- dimnames(bmat)[[2]]
            table <- self$results$bmat

            for (dim in dims) {
              table$addColumn(name = paste0(dim), type = 'number')
            }
            
            for (name in names) {
              row <- list()
              for (j in seq_along(dims)) {
                row[[dims[j]]] <- bmat[name, j]
              }
              table$addRow(rowKey = name, values = row)
            }
          }

          # D study (Composite table)------------
          
          if (isTRUE(self$options$comp)) {
            gen <- as.vector(ds1$composite$generalizability)
            depe <- as.vector(ds1$composite$dependability)
            uni <- as.vector(ds1$composite$var.universe)
            rel <- as.vector(ds1$composite$var.error.rel)
            abs <- as.vector(ds1$composite$var.error.abs)
            
            table <- self$results$comp
            table$setRow(
              rowNo = 1,
              values = list(
                generalizability = gen,
                dependability = depe,
                universe = uni,
                relative = rel,
                absolute = abs
              ))
          }

          if (isTRUE(self$options$bm)) {
              table <- self$results$bm
              all <- data.frame(
                gen = diag(ds1$between$generalizability),
                depe = diag(ds1$between$dependability),
                rel = diag(ds1$between$var.error.rel),
                abs = diag(ds1$between$var.error.abs)
              )
              
              for (i in seq_len(nrow(all))) {
                table$addRow(
                  rowKey = rownames(all)[i],
                  values = as.list(all[i, ])
                )
              }
            }
        }
      },
      ####################################################
      .plot1 = function(image, ...) {
        # if (is.null(image$state))
        #   return(FALSE)
        #
        # gmodel <- image$state
        
        # dstudy_plot(one_facet_gstudy, unit = "Participants",
        #             facets = list(Items = c(10, 20, 30, 40, 50, 60)),
        #             g_coef = FALSE)
        
        if (length(self$options$facet) > 1) return()
        
        # hemp package---------
        
        dep <- self$options$dep
        id <- self$options$id
        sub <- self$options$sub
        facets <- self$options$facet
        
        data <- self$data
        data <- na.omit(data)
        data <- as.data.frame(data)
        
        one_facet <- lme4::lmer(self$options$formula, data = data)
        
        # gstudy function---
        gstudy <- function(x, fixed = NULL) {
          tmp <- as.data.frame(lme4::VarCorr(x))
          tmp <- tmp[c(1, 4)]
          no.match <- function(x) {
            x[-match(fixed, x)]
          }
          if (!is.null(fixed)) {
            n_adj <- length(unique(x@frame[, grep(fixed, names(x@frame))]))
            fixed_vars <- tmp[grep(fixed, tmp$grp), ]
            fixed_vars <- fixed_vars[-match(fixed, fixed_vars$grp), ]
            fixed_vars$adj_vcov <- fixed_vars$vcov / n_adj
            fixed_vars$vcov <- NULL
            add_back <- strsplit(fixed_vars$grp, ":")
            fixed_vars$grp <- sapply(add_back, no.match)
            two.way <- data.frame(grp = paste(fixed_vars$grp, collapse = ":"),
                                  adj_vcov = tmp[nrow(tmp), 2] / n_adj)
            fixed_vars <- rbind(fixed_vars, two.way)
            tmp <- merge(tmp, fixed_vars)
            tmp[, 2] <- tmp[, 2] + tmp[, 3]
            tmp[, 3] <- NULL
            tmp$grp[length(tmp$grp)] <- "Residual"
          }
          colnames(tmp) <- c("Source", "Est.Variance")
          tmp$Percent.Variance <- tmp$Est.Variance / sum(tmp$Est.Variance)
          tmp[, 2] <- round(tmp[, 2], 4)
          tmp[, 3] <- paste0(round(tmp[, 3] * 100, 1), "%")
          N <- length(x@resp$y)
          output <- list(gstudy.out = tmp, nobs = N)
          class(output) <- "gStudy"
          return(output)
        }
        
        gmodel <- gstudy(one_facet)
        
        
        #d study function---
        
        dstudy <- function(x, n, unit) {
          tmp <- x$gstudy.out
          tmp <- tmp[c(1, 2)]
          us.var <- tmp[tmp$Source %in% unit, 2]
          n.matrix <- matrix(nrow = nrow(tmp), ncol = length(n))
          for (i in 1:length(n))
            n.matrix[grep(names(n)[i], tmp$Source), i] <- n[i]
          n.matrix[nrow(n.matrix), ] <- n
          tmp$n <- apply(n.matrix, 1, prod, na.rm = T)
          tmp[match(unit, tmp$Source), "n"] <- x$nobs
          tmp$vcov.n <- tmp$Est.Variance / tmp$n
          tmp[match(unit, tmp$Source), "vcov.n"] <- tmp[match(unit, tmp$Source), "Est.Variance"]
          
          # relative variance ----
          rel.var <- tmp$vcov.n[nrow(tmp)]
          if (length(n) > 1) {
            for (i in 1:length(n)) {
              tmp.names <- c(paste0(unit, ":", names(n)[i]), paste0(names(n)[i], ":", unit))
              tmp.var <- tmp[tmp$Source %in% tmp.names, "vcov.n"]
              rel.var <- sum(rel.var, tmp.var)
            }
          }
          
          # absolute variance ----
          tmp.abs <- tmp[-nrow(tmp), ]
          abs.var <- sum(tmp.abs[-grep(unit, tmp.abs$Source), "vcov.n"], rel.var)
          
          # generalizability coefficient
          g.coef <- us.var / (us.var + rel.var)
          
          # dependability coefficient
          d.coef <- us.var / (us.var + abs.var)
          
          output <- list(
            ds.df = tmp,
            relvar = rel.var,
            absvar = abs.var,
            gcoef = g.coef,
            dcoef = d.coef
          )
          class(output) <- "dStudy"
          return(output)
        }
        
        library(lattice)
        
        # d_study plot---
        
        dstudy_plot <- function(x,
                                unit,
                                facets,
                                g_coef = T,
                                bw = F) {
          if (length(facets) == 1) {
            conds <- facets[[1]]
            coefs <- matrix(NA, nrow = length(conds), ncol = 2)
            for (i in 1:length(conds)) {
              n <- conds[i]
              names(n) <- names(facets)
              tmp <- dstudy(x, n = n, unit = unit)
              coefs[i, ] <- c(tmp$gcoef, tmp$dcoef)
            }
            data.df <- data.frame(conds, coefs)
            names(data.df)[1] <- names(facets)
            names(data.df)[2:3] <- c("Generalizability", "Dependability")
            if (g_coef) {
              if (bw) {
                xyplot(
                  data.df[, 2] ~ data.df[, 1],
                  type = c("p", "l"),
                  xlab = paste(names(data.df[1])),
                  ylab = paste(names(data.df[2])),
                  scales = list(x = list(at = unique(data.df[, 1]))),
                  col = "black"
                )
              } else {
                xyplot(
                  data.df[, 2] ~ data.df[, 1],
                  type = c("p", "l"),
                  xlab = paste(names(data.df[1])),
                  ylab = paste(names(data.df[2])),
                  scales = list(x = list(at = unique(data.df[, 1])))
                )
              }
            } else {
              if (bw) {
                xyplot(
                  data.df[, 3] ~ data.df[, 1],
                  type = c("p", "l"),
                  xlab = paste(names(data.df[1])),
                  ylab = paste(names(data.df[3])),
                  scales = list(x = list(at = unique(data.df[, 1]))),
                  col = "black"
                )
              } else {
                xyplot(
                  data.df[, 3] ~ data.df[, 1],
                  type = c("p", "l"),
                  xlab = paste(names(data.df[1])),
                  ylab = paste(names(data.df[3])),
                  scales = list(x = list(at = unique(data.df[, 1])))
                )
              }
            }
          } else {
            conds <- expand.grid(facets[[1]], facets[[2]])
            coefs <- matrix(NA, nrow = nrow(conds), ncol = 2)
            names(conds) <- names(facets)
            for (i in 1:nrow(conds)) {
              n <- c(conds[i, 1], conds[i, 2])
              names(n) <- names(conds)
              tmp <- dstudy(x, n = n, unit = unit)
              coefs[i, ] <- c(tmp$gcoef, tmp$dcoef)
            }
            data.df <- data.frame(conds, coefs)
            names(data.df)[1:2] <- names(conds)
            names(data.df)[3:4] <- c("Generalizability", "Dependability")
            if (bw) {
              par.settings <- simpleTheme(
                lty = seq(1, length(unique(data.df[, 2]))),
                pch = seq(1, length(unique(data.df[, 2]))),
                col = "black"
              )
            } else {
              par.settings <- simpleTheme(lty = seq(1, length(unique(data.df[, 2]))), pch = 1)
            }
            if (g_coef) {
              xyplot(
                data.df[, 3] ~ data.df[, 1],
                group = data.df[, 2],
                type = "b",
                xlab = paste(names(data.df[1])),
                ylab = paste(names(data.df[3])),
                par.settings = par.settings,
                auto.key = list(
                  title = paste(names(data.df[2])),
                  space = "right",
                  cex.title = 1,
                  lines = T,
                  points = F,
                  type = "b"
                ),
                scales = list(x = list(at = unique(data.df[, 1])))
              )
            } else {
              xyplot(
                data.df[, 4] ~ data.df[, 1],
                group = data.df[, 2],
                type = "b",
                xlab = paste(names(data.df[1])),
                ylab = paste(names(data.df[4])),
                par.settings = par.settings,
                auto.key = list(
                  title = paste(names(data.df[2])),
                  space = "right",
                  cex.title = 1,
                  lines = T,
                  points = F,
                  type = "b"
                ),
                scales = list(x = list(at = unique(data.df[, 1])))
              )
            }
          }
        }
        
        
        #############################################
        
        facet <- self$options$facet
        nf <- self$options$nf
        gco <- self$options$gco
        
        plot1 <- dstudy_plot(
          gmodel,
          unit = self$options$id,
          facet = list(facet = c(1:nf)),
          g_coef = gco
        )
        
        print(plot1)
        TRUE
        
      }
      
    )
  )
