EpivizTrackData <- setRefClass("EpivizTrackData",
  contains="EpivizData",
  methods=list(
  	initialize = function(object=GNCList(GRanges()), ...) {
	  	callSuper(object=object, ...)
	  },
  	.check_class = function(object) { is(object, "GenomicRanges") }
  )
)

.valid.EpivizTrackData.object <- function(x) {
	if(!is(x$.object, "GNCList"))
		return("'object' is not a 'GNCList' object")
	NULL
}

.valid.EpivizTrackData <- function(x) {
	c(.valid.EpivizTrackData.object(x))
}

S4Vectors::setValidity2("EpivizTrackData", .valid.EpivizTrackData)

