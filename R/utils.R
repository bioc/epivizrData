#' Generic as.data.frame method for EpivizData objects
#' @param x \code{\link{EpivizData}} object to coerce.
#' @param query GRanges object
#' @param ... other param to send to data.frame
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
#' @param ah \code{\link{AnnotationHub}} object with records to add to
#'  database.
#' @param annotations A named list of lists (key/value pairs). Keys must 
#' be the AH ID for the corresponding record and the value is a named list
#' representing an annotation. An annotation is automatically inferred by
#' the record's metadata. Any annotation that is passed for a particular
#' record is concatenated to its inferred annotation. If the annotation has
#' a subtype column, it is used to name the data object being added to the db, 
#' otherwise the record's tags is used. 
#' @param ... arguments for toMySQL (connection, db_name, batch, index)
#' @examples
#' library(epivizrData)
#' library(AnnotationHub)
#' library(DBI)
#' library(RMySQL)
#' 
#' ah <- AnnotationHub()
#' db_annotations <- list()
#' 
#' # Query Patterns
#' roadmap <- "EpigenomeRoadMap"
#' bisulphite <- "bisulphite"
#' 
#' esophagus <- query(ah, c("esophagus", roadmap, bisulphite))
#' eso_anno <- list(tissue="Digestive", subtype="Esophagus")
#' eso_id <- names(esophagus)
#' db_annotations[[eso_id]] <- eso_anno
#' 
#' gastric <- query(ah, c("gastric", roadmap, bisulphite))
#' gas_anno <- list(tissue="Digestive", subtype="Gastric")
#' gas_id <- names(gastric)
#' db_annotations[[gas_id]] <- gas_anno
#' 
#' sigmoid_colon <- query(ah, c("sigmoid colon", roadmap, bisulphite))
#' colon_anno <- list(tissue="Digestive", subtype="Sigmoid Colon")
#' colon_id <- names(sigmoid_colon)
#' db_annotations[[colon_id]] <- colon_anno
#' 
#' small_intestine <- query(ah, c("small intestine", roadmap, bisulphite))
#' intestine_anno <- list(tissue="Digestive", subtype="Small Intestine")
#' intestine_id <- names(small_intestine)
#' db_annotations[[intestine_id]] <- intestine_anno
#' 
#' # This collapses our 4 records into one AnnotationHub object.
#' records <- c(esophagus, gastric, sigmoid_colon,  small_intestine)
#' 
#' connection <- dbConnect(MySQL(), host=host, user=user, password=pass)
#' db_name="my_database"
#' 
#' ahToMySQL(ah=records, annotations=db_annotations,
#'  connection=connection, db_name=db_name)
#'
#' @export
ahToMySQL <-  function (ah, annotations=list(), ...) {
  stopifnot(is(ah, "AnnotationHub"))

  ah_ids <- names(ah)
  for (id in ah_ids) {
    record <- ah[id]
    
    tryCatch({
      data_obj <- record[[id]]
    }, error=function(e) {
      message(e)
      next
    })

    try({
      # try to convert to GRanges if type is supported
      data_obj <- rtracklayer::import(data_obj)
    }, silent=TRUE)
    
    tryCatch({
      ms_obj <- epivizrData::register(data_obj)
    }, error=function(e) {
      # if we made it here the object type is not yet supported
      stop(e)
    })

    ms_obj$set_id(id)

    anno <- annotations[[id]]
    name <- NULL
    
    if (!is.null(anno)) name <- anno$subtype
    if (is.null(name)) name <- record$tags
    
    ms_obj$set_name(name)

    db_annotation <- .make_db_annotation(record, id, annotations)
    ms_obj$toMySQL(annotation=db_annotation, ...)
  }
  
  invisible()
}

.make_db_annotation <- function(record, id, annotations) {
  rec_metadata <- as.list(AnnotationHub:::.resource_table(record))
  annotation <- c(rec_metadata, annotations[[id]])
  
  # tidy for json format
  annotation$tags <- unlist(annotation$tags, use.names=FALSE)
  # annotation is wrapped in single quotes when added to db
  # this is a safety measure to avoid sql syntax errors 
  annotation <- lapply(annotation,
    function(anno) gsub("\'", '\\"', anno))
  
  annotation
}
