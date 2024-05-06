#' Split by ID
#'
#' Helper function to split data by ID and make it regular for computing the test statistic.
#'
#' @import data.table
#'
#' @inheritParams make_regular_return_file
#' @inheritParams remove_bounceback

#' @export
split_by_id <- function(DATA,
                        IMPUTATION = FALSE,
                        T_large = 1,
                        full_day = 0.95*86400,
                        delts = c(1, 5, 10, 15),
                        FREQ = 1,
                        ALIGN = TRUE){
  if (FREQ == 1) {
    print("Warning: data frequency defaulted to 1. Is this intended behavior?")
  }
  ## split data.table ##
  # by id
  if (!"h" %in% names(DATA)) DATA[, "h" := hour(t)] # if no h available
  if (!"q" %in% names(DATA)) DATA[, "q" := NA] # if no q reported

  if (nrow(DATA) == 0 || !all(c("t", "s", "p", "q", "d") %in% names(DATA))) {
    stop("DATA is empty or missing required columns")
  }

  print(Sys.time());print("Splitting data")
  print(paste("Rows in DATA:", nrow(DATA)))
  print(paste("Unique 's' values:", DATA[,length(unique(s))]))

  # Determine chunk indices for splitting the dataset
  chunk_size <- 500000000
  chunk_indices <- seq(1, nrow(DATA), by = chunk_size)  # Chunk size can be adjusted

  # Split the data into chunks and process each chunk using lapply
  chunks <- lapply(chunk_indices, function(i) {
    end_row <- min(i + chunk_size - 1, nrow(DATA))  # Calculate end row of the chunk
    DATA[i:end_row, .(d, h, "p" = mean(p), "q" = sum(q)), by = .(t, s)]
  })

  # Combine processed chunks
  DT_split <- split(rbindlist(chunks), by = c("d", "s"))
  print(paste("Rows in cleaned DATA:", nrow(DT_split)))

  full_length_days <- unlist(lapply(seq_along(DT_split), function(x) {
    DT_tmp <- DT_split[[x]]
    sufficient_length <- nrow(DT_tmp) >= (full_day/max(delts))
    unique_h <- DT_tmp[,sort(unique(h))]
    complete_hours <- all(0:23 %in% unique_h)
    return(sufficient_length & complete_hours)
  })
  )

  # only keep time series that can be aggregated to at least x mins intervals
  DT_split <- DT_split[full_length_days]
  ##

  ##
  progress <- round(quantile(1:length(DT_split), probs = seq(0,1,0.05)))
  ##

  ## transform tick data files to regularly spaced return series w.r.t. number of observations##
  print("Making returns")

  DT_split <- lapply(1:length(DT_split), function(x) {
    if (x %in% progress) {print(Sys.time()); print(progress[which(progress == x)])}
    DT_tmp <- DT_split[[x]]
    return(make_regular_return_file(DATA = DT_tmp,
                                    delts = delts,
                                    full_day =  full_day,
                                    IMPUTATION = IMPUTATION,
                                    FREQ = FREQ,
                                    ALIGN = ALIGN))
  })
  ##

  print(Sys.time());print("Removing bounceback outliers")

  ## remove bouncebacks ##
  DT_split <- split(remove_bounceback(DT_split, IMPUTATION = IMPUTATION), by = c("d", "s"))
  return(DT_split)
}
