
# need code to reshape if dimensions differ from input

#' @title Construct an amdMatrix
#' @description Construct a amdMatrix of a class that inherits
#' from \code{amdMatrix}
#' @param data An object that is or can be converted to a 
#' \code{matrix} object
#' @param ncol An integer specifying the number of columns
#' @param nrow An integer specifying the number of rows
#' @param type A character string specifying the type of amdMatrix.  Default
#' is NULL where type is inherited from the source data type.
#' @param ... Additional method to pass to amdMatrix methods
#' @return A amdMatrix object
#' @docType methods
#' @rdname amdMatrix-methods
#' @author Charles Determan Jr.
#' @export
setGeneric("amdMatrix", function(data = NA, ncol=NA, nrow=NA, type=NULL, ...){
  standardGeneric("amdMatrix")
})

#' @rdname amdMatrix-methods
#' @aliases amdMatrix,matrix
setMethod('amdMatrix', 
          signature(data = 'matrix'),
          function(data, type=NULL){
            
            if (is.null(type)) type <- typeof(data)
            
            data = switch(type,
                          integer = {
                            new("iamdMatrix", 
                                x=data,
                                type=type)
                          },
                          float = {
                            new("famdMatrix", 
                                x=data,
                                type=type)
                          },
                          double = {
                            new("damdMatrix",
                                x = data, 
                                type=type)
                          },
                          stop("this is an unrecognized 
                               or unimplemented data type")
                          )
            
            return(data)
          },
          valueClass = "amdMatrix"
)


#' @rdname amdMatrix-methods
#' @aliases amdMatrix,matrix
setMethod('amdMatrix', 
          signature(data = 'gpuMatrix'),
          function(data, type=NULL){
            
            if (is.null(type)) type <- typeof(data)
            
            data = switch(type,
                          integer = {
                            new("iamdMatrix", 
                                x=data@x,
                                type=type)
                          },
                          float = {
                            new("famdMatrix", 
                                x=data@x,
                                type=type)
                          },
                          double = {
                            new("damdMatrix",
                                x = data@x, 
                                type=type)
                          },
                          stop("this is an unrecognized 
                                 or unimplemented data type")
            )
            
            return(data)
          },
          valueClass = "amdMatrix"
)