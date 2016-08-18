#' @export
setMethod('typeof', signature(x="amdMatrix"),
          function(x) {
            switch(class(x),
                   "iamdMatrix" = "integer",
                   "famdMatrix" = "float",
                   "damdMatrix" = "double",
                   stop("unrecognized class"))
          })