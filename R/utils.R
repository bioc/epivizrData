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

#' Utility function to import data to a MySQL database from Annotation Hub
#' @param hub_ids List of Annotation Hub record ids
#' @param hub Annotation Hub object
#' @param connection DBIConnection to a MySQL database
#' @param db_name Name of the database in MySQL database you are importing to
#' @param ... arguments for toMySQL (annotation, batch)
#' @export
ahToMySQL <-  function (hub_ids, hub, connection, db_name, ...) {
  
  # if (!is.list(hub_ids)) {
  #   stop(hub_ids, " must be a list of record ids")
  # }
  
  if (!is(hub, "AnnotationHub")) {
    stop(hub, " must be an 'AnnotationHub' object")
  }
  
  for (id in hub_ids) {
    record <- NULL
    try({
      record <- hub[[id]]
      title <- hub[id]$title
    })
    
    if (!is.null(record)) {
      # TODO: Cleaner way to handle errors for unsupported objects
      # is it better toc  ms_obj <- NULL
      try({
        ms_obj <- epivizrData::register(record)
        ms_obj$set_name(title)
      })
      
      if (!is.null(ms_obj)) {
        ms_obj$toMySQL(connection, db_name, ...) 
      }
    }
  }
}