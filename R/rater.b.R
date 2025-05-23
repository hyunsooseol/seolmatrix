raterClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "raterClass",
    inherit = raterBase,
    private = list(
      .htmlwidget = NULL,
      .cachedResults = NULL,
      .transformedData = NULL,

      .init = function() {
        # Initialize cache
        private$.cachedResults <- list()

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

        # Set notes once at initialization
        if (self$options$ftest)
          self$results$ftest$setNote("Note", "H\u2090: ICC = 0; H\u2081: ICC > 0")

        if (self$options$ic)
          self$results$ic$setNote("Note", "The analysis was performed by 'irr::icc' function.")

        if (self$options$icc)
          self$results$icc$setNote("Note", "The analysis was performed by 'psy::icc' function.")

        if (self$options$kend)
          self$results$kend$setNote("Note", "H\u2090: W=0")
      },

      # Helper function to get and prepare data only once
      .getData = function() {
        if (!is.null(private$.transformedData))
          return(private$.transformedData)

        if (is.null(self$options$vars) || length(self$options$vars) < 2)
          return(NULL)

        data <- jmvcore::naOmit(self$data)

        # Transform data if needed
        if (self$options$t == "row") {
          data <- t(data)
        }

        # Convert to matrix once
        private$.transformedData <- as.matrix(data)
        return(private$.transformedData)
      },

      # Cache-aware computation function
      .compute = function(key, computeFn) {
        if (!is.null(private$.cachedResults[[key]])) {
          return(private$.cachedResults[[key]])
        }

        result <- computeFn()
        private$.cachedResults[[key]] <- result
        return(result)
      },

      .run = function() {

        data <- private$.getData()
        if (is.null(data)) return()

        # Load required libraries at the beginning
        if (any(c(self$options$bt, self$options$bicc))) {
          requireNamespace("boot", quietly = TRUE)
        }

        if (self$options$icc) {
          requireNamespace("psy", quietly = TRUE)
        }

        if (isTRUE(self$options$pair)) {

          res<- private$.pairwise()
          res <- res$matrix
        #self$results$text$setContent(res$matrix)
          res <- as.data.frame(res)
          names <- dimnames(res)[[1]]
          dims <- dimnames(res)[[2]]
          table <- self$results$matrix

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

        # Compute Light's Kappa
        if (isTRUE(self$options$interrater)) {
          res <- private$.compute("light_kappa", function() {
            irr::kappam.light(ratings = data)
          })

          table <- self$results$interrater
          table$setRow(
            rowNo = 1,
            values = with(res, list(
              N = subjects,
              Raters = raters,
              Kappa = value,
              Z = statistic,
              p = p.value
            ))
          )
        }

        # Fleiss' kappa
        if (isTRUE(self$options$fk)) {
          kap <- private$.compute("fleiss_kappa", function() {
            irr::kappam.fleiss(ratings = data)
          })

          table <- self$results$fk
          table$setRow(
            rowNo = 1,
            values = with(kap, list(
              N = subjects,
              Raters = raters,
              Kappa = value,
              Z = statistic,
              p = p.value
            ))
          )
        }

        # Bootstrap of Fleiss' kappa
        if (isTRUE(self$options$bt)) {
          bt_results <- private$.compute(paste0("fleiss_bootstrap_", self$options$boot1), function() {
            bt <- boot::boot(data, function(x, idx) {
              irr::kappam.fleiss(x[idx, ])$value
            }, R = self$options$boot1)

            bootci <- boot::boot.ci(bt)
            return(bootci$normal)
          })

          table <- self$results$bt
          table$setRow(
            rowNo = 1,
            values = list(
              lower = bt_results[2],
              upper = bt_results[3]
            )
          )
        }

        # Exact kappa
        if (isTRUE(self$options$ek)) {
          kae <- private$.compute("exact_kappa", function() {
            irr::kappam.fleiss(ratings = data, exact = TRUE)
          })

          table <- self$results$ek
          table$setRow(
            rowNo = 1,
            values = list(
              N = kae$subjects,
              Raters = kae$raters,
              Kappa = kae$value
            )
          )
        }

        # Category-wise Kappas
        if (isTRUE(self$options$cw)) {
          cw_results <- private$.compute("category_kappa", function() {
            irr::kappam.fleiss(ratings = data, detail = TRUE)
          })

          c <- cw_results[["detail"]]
          names <- dimnames(c)[[1]]
          table <- self$results$cw

          for (name in names) {
            row <- list()
            row[['k']] <- c[name, 1]
            row[['z']] <- c[name, 2]
            row[['p']] <- c[name, 3]
            table$addRow(rowKey = name, values = row)
          }
        }

        # Simple Percentage agreement
        if (isTRUE(self$options$pa)) {
          pa <- private$.compute("percent_agreement", function() {
            irr::agree(data)
          })

          table <- self$results$pa
          table$setRow(
            rowNo = 1,
            values = with(pa, list(
              Subjects = subjects,
              Raters = raters,
              Agreement = value
            ))
          )
        }

        # ICC TABLE
        if (isTRUE(self$options$icc)) {
          # Only try to compute if not already cached
          icc_key <- "psy_icc"
          if (is.null(private$.cachedResults[[icc_key]])) {
            icc <- try(psy::icc(data = data))

            if (jmvcore::isError(icc)) {
              err_string <- "You can't perform this analysis with sentence-type data."
              stop(err_string)
            }

            private$.cachedResults[[icc_key]] <- icc
          }

          icc <- private$.cachedResults[[icc_key]]

          table <- self$results$icc
          table$setRow(
            rowNo = 1,
            values = with(icc, list(
              Subjects = nb.subjects,
              Raters = nb.raters,
              sv = subject.variance,
              rv = rater.variance,
              rev = residual,
              Consistency = icc.consistency,
              Agreement = icc.agreement
            ))
          )
        }

        # Bootstrap of ICC agreement table
        if (isTRUE(self$options$bicc)) {
          bicc_key <- paste0("icc_bootstrap_", self$options$boot)

          if (is.null(private$.cachedResults[[bicc_key]])) {
            k <- try(boot::boot(data, function(x, idx) {
              psy::icc(x[idx, ])$icc.agreement
            }, R = self$options$boot))

            if (jmvcore::isError(k)) {
              err_string <- "You can't perform this analysis with sentence-type data."
              stop(err_string)
            }

            bootci <- boot::boot.ci(k)
            bicc <- bootci$normal
            private$.cachedResults[[bicc_key]] <- bicc
          }

          bicc <- private$.cachedResults[[bicc_key]]

          table <- self$results$bicc
          table$setRow(
            rowNo = 1,
            values = list(
              lower = bicc[2],
              upper = bicc[3]
            )
          )
        }

        # Kendall's W
        if (isTRUE(self$options$kend)) {
          ked <- private$.compute("kendall", function() {
            irr::kendall(data, correct = TRUE)
          })

          table <- self$results$kend
          table$setRow(
            rowNo = 1,
            values = with(ked, list(
              n = subjects,
              rater = raters,
              w = value,
              chi = statistic,
              p = p.value
            ))
          )
        }

        # ICC using oneway and twoway
        if (isTRUE(self$options$ic) || isTRUE(self$options$ftest)) {
          model <- self$options$model
          type <- self$options$type
          unit <- self$options$unit

          icc_key <- paste("irr_icc", model, type, unit, sep = "_")
          out <- private$.compute(icc_key, function() {
            irr::icc(data, model = model, type = type, unit = unit)
          })

          # ICC for oneway and twoway table
          if (isTRUE(self$options$ic)) {
            table <- self$results$ic
            table$setRow(
              rowNo = 1,
              values = with(out, list(
                model = model,
                type = type,
                unit = unit,
                sub = subjects,
                raters = raters,
                icc = value
              ))
            )
          }

          # F test
          if (isTRUE(self$options$ftest)) {
            table <- self$results$ftest
            table$setRow(
              rowNo = 1,
              values = with(out, list(
                icc = value,
                f = Fvalue,
                df1 = df1,
                df2 = df2,
                p1 = p.value,
                lower = lbound,
                upper = ubound
              ))
            )
          }
        }

        # Krippendorff's alpha
        if (isTRUE(self$options$krip)) {
          method <- self$options$method

          # Prepare data for Krippendorff's alpha
          krip_data <- data
          if (self$options$t != "row") {
            krip_data <- t(krip_data)
          } else if (self$options$t == "row") {
            krip_data <- t(krip_data)
          }

          krip_key <- paste("krippendorff", method, sep = "_")
          krip <- private$.compute(krip_key, function() {
            irr::kripp.alpha(as.matrix(krip_data), method = method)
          })

          table <- self$results$krip
          table$setRow(
            rowNo = 1,
            values = with(krip, list(
              Subjects = subjects,
              Raters = raters,
              alpha = value
            ))
          )
        }
      },

      # Add a cleanup method to clear caches when needed
      .clearCache = function() {
        private$.cachedResults <- list()
        private$.transformedData <- NULL
      },

      .pairwise = function() {
        # Get data using the cached method
        data <- private$.getData()
        if (is.null(data)) {
          return(NULL)
        }
        
        # Use cache-aware computation
        return(private$.compute("pairwise_kappa", function() {
          requireNamespace("irr", quietly = TRUE)
          
          # Get rater names
          raters <- colnames(data)
          n_raters <- length(raters)
          
          # Create dataframe to store results (removed SE, Lower, Upper)
          results <- data.frame(
            Ratings = character(),
            Unweighted_kappa = numeric(),
            stringsAsFactors = FALSE
          )
          
          # Variable to store kappa values
          kappa_values <- numeric()
          
          # Calculate for all rater pairs
          for (i in 1:(n_raters-1)) {
            for (j in (i+1):n_raters) {
              # Extract data for current rater pair
              pair_data <- data[, c(raters[i], raters[j])]
              
              # Remove rows with missing values
              pair_data <- pair_data[complete.cases(pair_data), ]
              
              # Add error handling with try-catch block
              kappa_result <- tryCatch({
                irr::kappa2(pair_data)
              }, error = function(e) {
                # Return NULL if error occurs
                message(paste("Error in pair", raters[i], "-", raters[j], ":", e$message))
                return(NULL)
              })
              
              # Process only if kappa_result is not NULL
              if (!is.null(kappa_result)) {
                # Save results
                pair_name <- paste(raters[i], "-", raters[j])
                
                results[nrow(results) + 1, ] <- c(
                  pair_name,
                  round(kappa_result$value, 3)
                )
                
                # Store kappa value
                kappa_values <- c(kappa_values, kappa_result$value)
              }
            }
          }
          
          # Check if results exist
          if (length(kappa_values) > 0) {
            # Calculate average kappa
            avg_kappa <- mean(kappa_values, na.rm = TRUE)
            
            # Add average kappa to results dataframe
            results <- rbind(data.frame(
              Ratings = "Average kappa",
              Unweighted_kappa = round(avg_kappa, 3),
              stringsAsFactors = FALSE
            ), results)
          } else {
            message("No valid kappa values were calculated.")
          }
          
          # Convert results to matrix format
          # Exclude "Average kappa" row
          pair_results <- results[results$Ratings != "Average kappa", ]
          
          # Extract rater names
          rater_pairs <- pair_results$Ratings
          unique_raters <- unique(unlist(strsplit(rater_pairs, " - ")))
          
          # Create matrix to store results
          n_unique_raters <- length(unique_raters)
          result_matrix <- matrix(NA, nrow = n_unique_raters, ncol = n_unique_raters)
          rownames(result_matrix) <- unique_raters
          colnames(result_matrix) <- unique_raters
          
          # Set diagonal to 1 (perfect agreement with self)
          diag(result_matrix) <- 1
          
          # Fill matrix with pairwise results
          for (i in 1:nrow(pair_results)) {
            raters <- unlist(strsplit(pair_results$Ratings[i], " - "))
            rater1 <- raters[1]
            rater2 <- raters[2]
            
            # Assign kappa value
            result_matrix[rater1, rater2] <- as.numeric(pair_results$Unweighted_kappa[i])
            result_matrix[rater2, rater1] <- as.numeric(pair_results$Unweighted_kappa[i])
          }
          
          # Return results (matrix and dataframe as list)
          return(list(
            matrix = result_matrix,
            details = results
          ))
        }))
      }
    ))