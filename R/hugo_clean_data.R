#' Clean your data
#'
#' This function fills missing values - a median for numeric variables and a mode for categorical variables (factors).
#' Additionally, the outliers from numeric variables are replaced according to the IQR rule for outliers.
#' In factors rare levels are merged into 'Other' level.
#'
#' @param data \code{data.frame} to clean
#' @param prop proportion of occurence of the level in a categorical variable which decides which levels are rare
#'
#' @return \code{data.frame} that has been cleaned
#'
#' @author Eliza Kaczorek
#' @examples
#' \dontrun{
#' # Dataset in base R: airquality
#' # There are 44 missing values
#' sum(is.na(airquality))
#'
#' hugo_clean_data(airquality)
#' # The data was cleaned.
#'
#' # Two original rows from data:
#'
#' # Ozone Solar.R  Wind Temp Month Day
#' #     8      19  20.1   61     5   9
#' #     NA      NA 14.3   56     5   5
#'
#' # After cleaning:
#'
#' # Ozone Solar.R  Wind Temp Month Day
#' #     8      19 17.65   61     5   9
#' #  31.5     205 14.30   56     5   5
#'
#' # We can see that the outlier in 'Wind' was
#' # replaced by the value Q3+1.5*IGR for this column.
#' # Missing values were replaced with medians.
#' }
#'
#' @export

hugo_clean_data <- function(data, prop = 0.05) {
  .hugoEnv$history[length(.hugoEnv$history)+1] <- deparse(match.call())

  stopifnot(is.data.frame(data))

  if(!(is.numeric(prop) & prop<=1 & prop>=0)) {
    cat("Try choose 'prop' parameter again.\nThe proportion should be the number between 0 and 1 (less than 0.2 recommended).")
    while(TRUE) {
      prop <- readline(prompt = "prop = ")
      if(prop>=0 & prop<=1) {
        break
      } else {
        cat("Try again:")
      }
    }
  }
  cat(paste0("Data cleaning will be done with 'prop' parameter equal to ",prop,".\n"))

  columns_num <- ncol(data)

  factor_columns <- which(sapply(1:columns_num, function(column) is.factor(data[,column])))
  numeric_columns <- setdiff(1:columns_num, factor_columns)

  for(col in numeric_columns) {
    data[is.na(data[,col]),col] <- stats::median(data[,col], na.rm=TRUE)

    q1 <- stats::quantile(data[,col], 0.25)
    q3 <- stats::quantile(data[,col], 0.75)
    iqr <- stats::IQR(data[,col])

    left_boundary <- q1 - 1.5*iqr
    right_boundary <- q3 + 1.5*iqr

    data[,col] <- replace(data[,col], data[,col] < left_boundary, left_boundary)
    data[,col] <- replace(data[,col], data[,col] > right_boundary, right_boundary)
  }

  for(col in factor_columns) {
    temp <- table(data[!is.na(data[,col]),col])
    modes_of_column <- names(temp)[temp == max(temp)]

    if(length(modes_of_column) > 1) {
      data[is.na(data[,col]),col] <- sample(modes_of_column, sum(is.na(data[,col])), replace = TRUE)
    } else {
      data[is.na(data[,col]),col] <- modes_of_column
    }
    data[,col] <- forcats::fct_lump(data[,col], prop = prop, other_level = "Other")
  }

  cat("\nThe data has been cleaned.")
  data
}
