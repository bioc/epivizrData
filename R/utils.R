#' Generic as.data.frame method for EpivizData objects
#' @export
setMethod ("as.data.frame", signature("EpivizData"),
  function (x, query=NULL, ...) {
    rows <- x$get_rows(query, metadata=x$get_metadata_columns())
    metadata <- as.data.frame(lapply(rows$values$metadata, unlist), ...)

    rows$values$id <- NULL
    rows$values$metadata <- NULL

    df <- as.data.frame(rows$values, ...)
    if (nrow(metadata) != 0) df <- data.frame(df, metadata, ...)

    cols <- x$.get_col_data(query)
    cols_df <- as.data.frame(lapply(cols, function(col) col$values))
    if (nrow(cols_df) != 0) df <- data.frame(df, cols_df, ...)

    df
  }
)
