# This file is a generated template, your changes will not be overwritten
#' @importFrom magrittr %>%
#' @importFrom EFA.dimensions MAP

mapClass <- if (requireNamespace("jmvcore", quietly = TRUE))
  R6::R6Class(
    "mapClass",
    inherit = mapBase,
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
        
        if (isTRUE(self$options$screePlot)) {
          width  <- self$options$width
          height <- self$options$height
          self$results$screePlot$setSize(width, height)
        }
        
        if (isTRUE(self$options$MapCurvePlot)) {
          width  <- self$options$width1
          height <- self$options$height1
          self$results$MapCurvePlot$setSize(width, height)
        }        
      },
      
      #---------------------------------
      
      .run = function() {
        
        if (is.null(self$options$vars) || length(self$options$vars) < 3) return()
        
        dat <- jmvcore::select(self$data, self$options$vars)
        dat <- jmvcore::naOmit(dat)
        dat <- jmvcore::toNumeric(dat)
        
        corType <- switch(
          self$options$type,
          pearson    = "pearson",
          kendall    = "kendall",
          spearman   = "spearman",
          gamma      = "gamma",
          polychoric = "polychoric",
          default    = "pearson"
        )
        
        res <- EFA.dimensions::MAP(
          data    = dat,
          corkind = corType
        )
        
        # table-------------------------------------------
        
        mat <- NULL
        if (!is.null(res$totvarexplNOROT)) mat <- res$totvarexplNOROT
        if (is.null(mat) && !is.null(res$totvarexpl)) mat <- res$totvarexpl
        if (is.null(mat))
          stop("Unable to find eigenvalue table in MAP result.")
        
        cn <- colnames(mat)
        idxEigen <- grep("eigen",      cn, ignore.case = TRUE)[1]
        idxProp  <- grep("proportion", cn, ignore.case = TRUE)[1]
        idxCum   <- grep("cumulative", cn, ignore.case = TRUE)[1]
        if (any(is.na(c(idxEigen, idxProp, idxCum))))
          stop("Unexpected column names in MAP eigen table.")
        
        tbl <- self$results$eigen  
        k <- nrow(mat)
        for (i in seq_len(k)) {
          tbl$addRow(
            rowKey = as.character(i),
            values = list(
              factor  = i,
              eigen   = unname(mat[i, idxEigen]),
              propVar = unname(mat[i, idxProp]),
              cumVar  = unname(mat[i, idxCum])
            )
          )
        }
        
        # ---- Fill "Velicer's Average Squared Correlations" table --------------
        av <- NULL
        if (!is.null(res$avgsqrs))
          av <- res$avgsqrs
        
        if (!is.null(av)) {
          av <- as.data.frame(av, stringsAsFactors = FALSE)
          
          cn <- colnames(av)
          idxRoot  <- grep("^root$", cn, ignore.case = TRUE)[1]
          idxAvgSq <- grep("avg.*corr.*sq", cn, ignore.case = TRUE)[1]
          idxPow4  <- grep("avg.*corr.*power4|power4", cn, ignore.case = TRUE)[1]
          
          # if no explicit root column, create 0..(k-1)
          if (is.na(idxRoot)) {
            rootVals <- seq.int(0, nrow(av) - 1)
          } else {
            rootVals <- av[[idxRoot]]
          }
          
          tbl2 <- self$results$avgCorr  # <- r.yaml: items$name: avgCorr
          k2 <- nrow(av)
          for (i in seq_len(k2)) {
            tbl2$addRow(
              rowKey = paste0("r", i),
              values = list(
                root    = rootVals[i],
                avgSq   = unname(av[i, idxAvgSq]),
                avgPow4 = unname(av[i, idxPow4])
              )
            )
          }
          
          dfMap <- data.frame(
            root    = rootVals,
            avgSq   = av[[idxAvgSq]],
            avgPow4 = av[[idxPow4]]
          )
          minSq   <- which.min(dfMap$avgSq)
          minPow4 <- which.min(dfMap$avgPow4)
          
          self$results$MapCurvePlot$setState(list(
            df     = dfMap,
            minSq  = minSq,
            minPow4= minPow4
          ))
        }
        
        # Scree plot state + Parallel Analysis -------------------------------
        pa_ncomp <- NULL  # PA 
        if (!is.null(mat)) {
          dfScree <- data.frame(
            factor = seq_len(nrow(mat)),
            eigen  = as.numeric(mat[, idxEigen])
          )
          
          pa_try <- try(psych::fa.parallel(dat, fa = "pc", n.iter = 100,
                                           show.legend = FALSE, plot = FALSE),
                        silent = TRUE)
          if (!inherits(pa_try, "try-error")) {
            simVals <- NULL
            if (!is.null(pa_try$pc.sim)) simVals <- pa_try$pc.sim
            if (is.null(simVals) && !is.null(pa_try$fa.sim)) simVals <- pa_try$fa.sim  # fallback
            if (!is.null(simVals)) {
              simVals <- as.numeric(simVals)
              simVals <- simVals[seq_len(nrow(dfScree))]     
              dfScree$sim <- simVals
              
              pa_ncomp <- if (!is.null(pa_try$ncomp)) pa_try$ncomp
              else sum(dfScree$eigen > dfScree$sim, na.rm = TRUE)
            }
          }

          self$results$screePlot$setState(dfScree)
        }
        
        txt_lines <- character(0)
        
        # Original MAP
        if (!is.null(res$NfactorsMAP)) {
          txt_lines <- c(
            txt_lines,
            sprintf("The number of components according to the Original (1976) MAP Test is = %d",
                    res[["NfactorsMAP"]])
          )
        }
        # Revised MAP
        if (!is.null(res$NfactorsMAP4)) {
          txt_lines <- c(
            txt_lines,
            sprintf("The number of components according to the Revised (2000) MAP Test is = %d",
                    res[["NfactorsMAP4"]])
          )
        }
        # minima
        if (exists("dfMap") && !is.null(dfMap)) {
          min_avgSq   <- dfMap$avgSq[minSq]
          min_avgPow4 <- dfMap$avgPow4[minPow4]
          txt_lines <- c(
            txt_lines,
            sprintf("The smallest average squared correlation is %.5f",   min_avgSq),
            sprintf("The smallest average 4rth power correlation is %.5f", min_avgPow4)
          )
        }
        # PA 제안 요인 수
        if (!is.null(pa_ncomp)) {
          txt_lines <- c(
            txt_lines,
            sprintf("Parallel Analysis suggests retaining %d components.", pa_ncomp)
          )
        }
        if (length(txt_lines) > 0)
          self$results$text$setContent(paste(txt_lines, collapse = "\n"))
      },
      
      .MapCurvePlot = function(image, ggtheme, theme, ...) {
        st <- image$state
        if (is.null(st) || is.null(st$df)) return(FALSE)
        
        df <- st$df
        library(ggplot2)
        library(tidyr)
        
        # wide → long 
        df_long <- tidyr::pivot_longer(
          df,
          cols = c("avgSq", "avgPow4"),
          names_to = "series", values_to = "value"
        )
        df_long$series <- factor(
          df_long$series,
          levels = c("avgSq", "avgPow4"),
          labels = c("Avg.Corr.Sq.", "Avg.Corr.power4")
        )
        
        p <- ggplot(df_long, aes(x = root, y = value,
                                 color = series, linetype = series)) +
          geom_line() +
          geom_point() +
          geom_point(
            data = data.frame(
              root   = df$root[c(st$minSq, st$minPow4)],
              value  = c(df$avgSq[st$minSq], df$avgPow4[st$minPow4]),
              series = c("Avg.Corr.Sq.", "Avg.Corr.power4")
            ),
            aes(x = root, y = value, color = series), inherit.aes = FALSE,
            size = 3, shape = 16
          ) +
          labs(x = "Root", y = "Avg.Corr.") +
          theme_bw() + ggtheme +
          theme(legend.title = element_blank(),
                legend.position = "right",
                plot.title = element_blank())
        
        print(p)
        TRUE
      },
      
      # ------------------------- Scree Plot ------------------------------
      .screePlot = function(image, ggtheme, theme, ...) {
        df <- image$state
        if (is.null(df)) return(FALSE)
        
        library(ggplot2)
        
        if ("sim" %in% names(df)) {
          df_long <- data.frame(
            factor = rep(df$factor, 2),
            series = factor(rep(c("Data", "Simulations"), each = nrow(df)),
                            levels = c("Data", "Simulations")),
            value  = c(df$eigen, df$sim)
          )
          p <- ggplot(df_long, aes(x = factor, y = value,
                                   color = series, linetype = series)) +
            geom_line() +
            geom_point() +
            labs(x = "Component", y = "Eigenvalue") +
            theme_bw() + ggtheme +
            theme(legend.title = element_blank(),
                  plot.title = element_blank())
        } else {
          p <- ggplot(df, aes(x = factor, y = eigen)) +
            geom_line() +
            geom_point() +
            labs(x = "Component", y = "Eigenvalue") +
            theme_bw() + ggtheme +
            theme(legend.title = element_blank(),
                  plot.title = element_blank())
        }
        
        print(p)
        TRUE
      }
    )
  )
