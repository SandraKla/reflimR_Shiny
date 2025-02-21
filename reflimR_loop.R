# Loops for reflim() and zlog()
#
# The following example demonstrates the use of the reflim.loop() and zlog.loop() functions:
#
# reflim.loop(): Computes reference limits for each column of the dataset.
# zlog.loop(): Calculates the zlog values for each individual value in the dataset using the reference limits from reflim.loop.

### EXAMPLE ###

# # Load the required library
# library(reflimR)
# 
# # Step 1: Extract relevant columns from the dataset
# xx <- livertests[, 4:11]
# # Step 2: Compute reference limits for each column
# reflim.loop.results <- reflim.loop(xx)
# # Step 3: Calculate zlog values for each individual value
# zlog.loop.results <- zlog.loop(xx, reflim.loop.results)
# 
# # Use targets
# targets <- targetvalues[,c(1,3,4)]
# colnames(targets) <- c("analyte", "ll", "ul")
# reflim.loop.results <- reflim.loop(xx, targets = targets)

### FUNCTIONS ###

#' Apply reflim() to each column of a dataframe
#'
#' This function applies the `reflim` function from the `reflimR` package to each column of a given dataframe.
#'
#' @param xx A dataframe where each column will be processed.
#' @param lognormal Boolean indicating whether a lognormal distribution should be assumed (NULL means that the distribution type is defined automatically).
#' @param targets A dataframe with the analyte name must be corresponding to xx and lower and upper limit.
#' @param perc.trunc Percentage of presumably normal values to be removed from each side.
#' @param n.min Minimum number of observations needed for a robust estimate of reference intervals.
#' @param apply.rounding Boolean indicating whether the estimated reference limits should be rounded.
#' @param plot.it Boolean indicating whether graphics should be created.
#' @param plot.all Boolean indicating whether graphics of all process steps should be created.
#' @param print.n Boolean indicating whether the number of cases after truncation should be printed on the graph.
#' @param main Title of the graphic.
#' @param xlab X-axis label of the graphic.
#'
#' @return A list where each element contains the results from `reflim` for the corresponding column of the input dataframe.
#' The structure of the results includes:
#' \describe{
#'   \item{\code{stats}}{Mean and SD (or meanlog and sdlog) of the truncated vector, number of cases before and after truncation.}
#'   \item{\code{lognormal}}{Boolean indicating whether a lognormal distribution has been assumed.}
#'   \item{\code{limits}}{Estimated reference limits with tolerance intervals.}
#'   \item{\code{targets}}{Target values with tolerance intervals.}
#'   \item{\code{perc.norm}}{Estimated percentage of non-pathological values.}
#'   \item{\code{confidence.int}}{95 percent confidence intervals for the estimated reference limits (depends on n).}
#'   \item{\code{interpretation}}{Short text describing the deviation of observed limits from target values.}
#'   \item{\code{remarks}}{Short text describing potential reasons why the reflim function could not be executed.}
#'   \item{\code{error}}{Error message if the function fails for a specific column.}
#' }
reflim.loop <- function(xx,
                        lognormal = NULL,
                        targets = NULL,
                        perc.trunc = 2.5,
                        n.min = 200,
                        apply.rounding = TRUE,
                        plot.it = TRUE,
                        plot.all = FALSE,
                        print.n = TRUE,
                        main = "reference limits",
                        xlab = "x") {
    # Check if input is a dataframe
    start_time <- Sys.time()
    if (!is.data.frame(xx))
        stop("Input xx must be a dataframe.")
    
    # Initialize an empty list to store results
    results <- list()
    targets_reflim <- targets
    
    # Loop through each column
    for (col_name in names(xx)) {
        
        # If targets are provided, extract the corresponding ll and ul
        if (!is.null(targets)) {
            target_row <- targets[targets$analyte == col_name, ]
            
            # Extract ll and ul from the appropriate columns
            if (nrow(target_row) > 0) {
                ll <- target_row[1, 2]  # Lower limit
                ul <- target_row[1, 3]  # Upper limit
                targets_reflim <- c(ll, ul) 
            } else {
                stop("No target values found for analyte: ", col_name)
            }
        } 
        
        main <- paste0("Reference Limits for ", col_name)
        
        tryCatch({
            # Apply reflim to the column
            results[[col_name]] <- reflimR::reflim(
                xx[[col_name]],
                lognormal = lognormal,
                targets = targets_reflim,
                perc.trunc = perc.trunc,
                n.min = n.min,
                apply.rounding = apply.rounding,
                plot.it = plot.it,
                plot.all = plot.all,
                print.n = print.n,
                main = main,
                xlab = xlab
            )
        },
        error = function(e) {
            # Store the error message in case of failure
            results[[col_name]] <-
                list(error = paste("Error:", e$message))
        })
    }
    
    end_time <- Sys.time()
    message("Total processing time: ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds")

    return(results)
}

#' Calculate Zlog Values for each column of a dataframe
#'
#' This function computes `zlog` values for each column of a dataframe `xx` using
#' the reference limits obtained from the results of `reflimR.expand`.
#'
#' @param xx A dataframe containing the original data for which zlog values are calculated.
#' @param reflim_results The output of `reflimR.expand`, containing the reference limits for each column.
#' @return A dataframe with `xx` and the calculated zlog values.
zlog.loop <- function(xx, reflim_results) {
    start_time <- Sys.time()
    # Check if input is a dataframe
    if (!is.data.frame(xx))
        stop("Input xx must be a dataframe.")
    
    # Initialize a list to store columns in the desired order
    sorted_results <- list()
    
    # Loop through each column
    for (col_name in names(xx)) {
        tryCatch({
            # Extract lower and upper limits from reflim_results
            limits <- reflim_results[[col_name]]$limits
            if (is.null(limits))
                stop(paste("No limits found for column:", col_name))
            
            lower.lim <- limits[[1]]
            upper.lim <- limits[[2]]
            
            # Initialize a vector to store zlog values for this column
            zlog_column <- numeric(nrow(xx))
            
            # Loop through each value in the column
            for (i in seq_len(nrow(xx))) {
                tryCatch({
                    # Apply zlog function for the current value
                    zlog_column[i] <-
                        zlog(xx[[col_name]][i], lower.lim, upper.lim)
                },
                error = function(e) {
                    # Handle errors for individual values
                    zlog_column[i] <- NA
                    message(
                        paste(
                            "Error processing value in column:",
                            col_name,
                            "row:",
                            i,
                            "-",
                            e$message
                        )
                    )
                })
            }
            
            # Add the original column and its corresponding _zlog column to the results list
            sorted_results[[col_name]] <- xx[[col_name]]
            sorted_results[[paste0(col_name, "_zlog")]] <-
                zlog_column
        },
        error = function(e) {
            # Handle errors for the entire column
            message(paste(
                "Error processing column:",
                col_name,
                "-",
                e$message
            ))
            sorted_results[[col_name]] <- xx[[col_name]]
            sorted_results[[paste0(col_name, "_zlog")]] <- NA
        })
    }
    
    end_time <- Sys.time()
    message("Total processing time: ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds")
    
    # Combine the sorted columns into a dataframe
    return(as.data.frame(sorted_results))
}
