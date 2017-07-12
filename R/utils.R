
#' @export
setMethod ("as.data.frame", signature("EpivizData"),
  function (x, query=NULL, row.names=NULL, optional=FALSE, ...) {
    rows <- x$get_rows(query, metadata=x$get_metadata_columns())

    metadata <- as.data.frame(lapply(rows$values$metadata, unlist), ...)
    rows$values$metadata <- NULL
    df <- as.data.frame(rows$values)
    if (nrow(metadata) != 0) df <- data.frame(df, metadata, stringsAsFactors=FALSE)

    cols<- x$.get_col_data(query)
    cols_df <- as.data.frame(lapply(cols, function(col) col$values))
    if (nrow(cols_df) != 0) df <- data.frame(df, cols_df, stringsAsFactors=FALSE)

    df
  }
)
