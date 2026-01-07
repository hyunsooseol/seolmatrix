
multilevelClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "multilevelClass",
    inherit = multilevelBase,
    private = list(
      .htmlwidget = NULL,
      # Add instance for HTMLWidget
      
      #------------------------
      
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
            '<li>More than two dependent variables should be specified for Multilevel correlation.</li>',
            '<li>The rationale of Multilevel Correlation is described in the <a href="https://cran.r-project.org/web/packages/correlation/vignettes/multilevel.html" target = "_blank">page</a>.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/seolmatrix/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
        ))
        if (self$options$icc)
          self$results$icc$setNote(
            "Note",
            "ICC1= Individual-level variance that can be explained by group membership; ICC2= The reliability of the group means."
          )
        
        
      },
      #---------------------------------
      .run = function() {
        if (length(self$options$facs) < 1)
          return()
        data <- self$data
        vars  <- self$options$vars
        facs <- self$options$facs
        
        res <- NULL  # ✅ 항상 정의 (multi OFF에서도 res not found 방지)
        
        # 결과요소 참조
        table_multi <- self$results$multi
        image <- self$results$plot
        
        # 기본: multi 꺼져 있으면 multi table/plot 숨김 + plot state 비움
        if (!isTRUE(self$options$multi)) {
          
          # ✅ multi 결과 숨김(가장 안전)
          table_multi$setVisible(FALSE)
          
          if (isTRUE(self$options$plot)) {
            image$setVisible(FALSE)
            image$setState(NULL)
          }
          
        } else {
          
          # ✅ multi 결과 보이기
          table_multi$setVisible(TRUE)
          if (isTRUE(self$options$plot))
            image$setVisible(TRUE)
          
          # convert to appropriate data types
          for (i in seq_along(vars))
            data[[i]] <- jmvcore::toNumeric(data[[i]])
          for (fac in facs)
            data[[fac]] <- as.factor(data[[fac]])
          
          #############################################
          res <- correlation::correlation(data, multilevel = TRUE)
          #############################################
          
          v1 <- res$Parameter1
          v2 <- res$Parameter2
          r <- res$r
          low <- res$CI_low
          high <- res$CI_high
          t <- res$t
          df <- res$df
          p <- res$p
          n <- res$n_Obs
          
          # populate multilevel table---
          nrows <- ncol(combn(self$options$vars, 2))
          
          for (i in seq_len(nrows)) {
            
            row <- list(
              v1   = v1[i],
              v2   = v2[i],
              r    = r[i],
              low  = low[i],
              high = high[i],
              t    = t[i],
              df   = df[i],
              p    = p[i],
              n    = n[i]
            )
            
            # ✅ row가 없으면 addRow로 만들고, 있으면 setRow로 업데이트
            tryCatch(
              {
                table_multi$addRow(rowKey = i, values = row)
              },
              error = function(e) {
                table_multi$setRow(rowKey = i, values = row)
              }
            )
          }
          
          # ✅ plot state 설정은 res 있을 때만
          if (isTRUE(self$options$plot)) {
            image$setState(res)
          }
        }
        
        ##Computing ICC###############################
        
        out <- NULL
        for (var in self$options$vars) {
          dataA <- data.frame(dep = data[[var]], group = data[[facs]])
          model <- stats::aov(dep ~ group, data = dataA)
          icc1 <- multilevel::ICC1(model)
          icc2 <- multilevel::ICC2(model)
          df2 <- data.frame(icc1, icc2)
          if (is.null(out)) {
            out <- df2
          } else {
            out <- rbind(out, df2)
          }
        }
        out <- out #data frame
        
        # Creating table----------
        table_icc <- self$results$icc
        icc <- as.list(out)
        for (i in seq_along(self$options$vars)) {
          row <- list()
          row[["icc1"]] <-  icc[[1]][i]
          row[["icc2"]] <-  icc[[2]][i]
          table_icc$setRow(rowNo = i, values = row)
        }
      },
      
      .plot = function(image, ggtheme, theme, ...) {
        
        if (!isTRUE(self$options$multi)) return(FALSE)
        if (is.null(self$options$facs)) return(FALSE)
        
        res <- image$state
        if (is.null(res)) return(FALSE)
        
        library(ggplot2)
        plot <- ggplot(res, aes(x = Parameter1, y = Parameter2, fill = r)) +
          geom_tile(color = "grey80", size = 0.7) +
          scale_fill_gradient2(
            low = "#4575b4",
            mid = "white",
            high = "#d73027",
            limits = c(-1, 1),
            midpoint = 0,
            name = "Correlation"
          ) +
          geom_text(aes(label = sprintf("%.2f", r)), color = "black", size = 5, fontface = "bold") +
          labs(
            title = "",
            subtitle = "",
            x = NULL, y = NULL
          ) +
          theme_minimal(base_size = 15) +
          theme(
            axis.text.x = element_text(face = "bold"),
            axis.text.y = element_text(face = "bold"),
            panel.grid = element_blank(),
            plot.title = element_text(face = "bold", size = 18)
          )
        
        if (self$options$angle > 0) {
          plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        
        # plot <- plot + ggtheme
        print(plot)
        TRUE
      }
    )
  )
