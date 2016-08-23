
#' @import gpuR

# need code to reshape if dimensions differ from input

#' @title Construct an amdMatrix
#' @description Construct a amdMatrix of a class that inherits
#' from \code{amdMatrix}
#' @param data An object that is or can be converted to a 
#' \code{matrix} object
#' @param nrow An integer specifying the number of rows
#' @param ncol An integer specifying the number of columns
#' @param type A character string specifying the type of amdMatrix.  Default
#' is NULL where type is inherited from the source data type.
#' @param ... Additional method to pass to amdMatrix methods
#' @return A amdMatrix object
#' @docType methods
#' @rdname amdMatrix-methods
#' @author Charles Determan Jr.
#' @export
setGeneric("amdMatrix", function(data = NA, nrow=NA, ncol=NA, type=NULL, ...){
  standardGeneric("amdMatrix")
})

#' @rdname amdMatrix-methods
#' @aliases amdMatrix,matrix
setMethod('amdMatrix', 
          signature(data = 'matrix'),
          function(data, type=NULL, ctx_id = NULL){
            
            if (is.null(type)) type <- typeof(data)
            
            # just create the base object and then change class
            tmp <- gpuMatrix(data = data, 
                             nrow = nrow(data), ncol = ncol(data), 
                             type = type, 
                             ctx_id = ctx_id)
            
            data = switch(type,
                          integer = {
                              new("iamdMatrix", 
                                  address=tmp@address,
                                  .context_index = tmp@.context_index,
                                  .platform_index = tmp@.platform_index,
                                  .platform = tmp@.platform,
                                  .device_index = tmp@.device_index,
                                  .device = tmp@.device)
                          },
                          float = {
                              new("famdMatrix", 
                                  address=tmp@address,
                                  .context_index = tmp@.context_index,
                                  .platform_index = tmp@.platform_index,
                                  .platform = tmp@.platform,
                                  .device_index = tmp@.device_index,
                                  .device = tmp@.device)
                          },
                          double = {
                              new("damdMatrix",
                                  address = tmp@address,
                                  .context_index = tmp@.context_index,
                                  .platform_index = tmp@.platform_index,
                                  .platform = tmp@.platform,
                                  .device_index = tmp@.device_index,
                                  .device = tmp@.device)
                          },
                          stop("this is an unrecognized 
                                 or unimplemented data type")
            )
            
            return(data)
          },
          valueClass = "amdMatrix"
)

#' @rdname amdMatrix-methods
#' @aliases amdMatrix,missing
setMethod('amdMatrix', 
          signature(data = 'missing'),
          function(data, nrow=NA, ncol=NA, type=NULL, ctx_id = NULL){
            
              # just create the base object and then change class
              tmp <- gpuMatrix(nrow = nrow, ncol = ncol, 
                               type = type, 
                               ctx_id = ctx_id)
              
              data = switch(type,
                            integer = {
                                new("iamdMatrix", 
                                    address=tmp@address,
                                    .context_index = tmp@.context_index,
                                    .platform_index = tmp@.platform_index,
                                    .platform = tmp@.platform,
                                    .device_index = tmp@.device_index,
                                    .device = tmp@.device)
                            },
                            float = {
                                new("famdMatrix", 
                                    address=tmp@address,
                                    .context_index = tmp@.context_index,
                                    .platform_index = tmp@.platform_index,
                                    .platform = tmp@.platform,
                                    .device_index = tmp@.device_index,
                                    .device = tmp@.device)
                            },
                            double = {
                                new("damdMatrix",
                                    address = tmp@address,
                                    .context_index = tmp@.context_index,
                                    .platform_index = tmp@.platform_index,
                                    .platform = tmp@.platform,
                                    .device_index = tmp@.device_index,
                                    .device = tmp@.device)
                            },
                            stop("this is an unrecognized 
                                 or unimplemented data type")
              )
            
            return(data)
          },
          valueClass = "amdMatrix")


#' @rdname amdMatrix-methods
#' @aliases amdMatrix,matrix
setMethod('amdMatrix', 
          signature(data = 'gpuMatrix'),
          function(data, type=NULL){
            
            if (is.null(type)) type <- typeof(data)
            
            data = switch(type,
                          integer = {
                            new("iamdMatrix",
                                address = data@address,
                                .context_index = data@.context_index,
                                .platform_index = data@.platform_index,
                                .platform = data@.platform,
                                .device_index = data@.device_index,
                                .device = data@.device)
                          },
                          float = {
                            new("famdMatrix",
                                address = data@address,
                                .context_index = data@.context_index,
                                .platform_index = data@.platform_index,
                                .platform = data@.platform,
                                .device_index = data@.device_index,
                                .device = data@.device)
                          },
                          double = {
                            new("damdMatrix",
                                address = data@address,
                                .context_index = data@.context_index,
                                .platform_index = data@.platform_index,
                                .platform = data@.platform,
                                .device_index = data@.device_index,
                                .device = data@.device)
                          },
                          stop("this is an unrecognized 
                                 or unimplemented data type")
            )
            
            return(data)
          },
          valueClass = "amdMatrix"
)

#' @rdname amdMatrix-methods
#' @aliases amdMatrix,numeric
setMethod('amdMatrix', 
          signature(data = 'numeric'),
          function(data, nrow, ncol, type=NULL, ctx_id = NULL){
              
              # just create the base object and then change class
              tmp <- gpuMatrix(data = data, 
                               nrow = nrow, ncol = ncol, 
                               type = type, 
                               ctx_id = ctx_id)
              
              data = switch(type,
                            integer = {
                                new("iamdMatrix", 
                                    address=tmp@address,
                                    .context_index = tmp@.context_index,
                                    .platform_index = tmp@.platform_index,
                                    .platform = tmp@.platform,
                                    .device_index = tmp@.device_index,
                                    .device = tmp@.device)
                            },
                            float = {
                                new("famdMatrix", 
                                    address=tmp@address,
                                    .context_index = tmp@.context_index,
                                    .platform_index = tmp@.platform_index,
                                    .platform = tmp@.platform,
                                    .device_index = tmp@.device_index,
                                    .device = tmp@.device)
                            },
                            double = {
                                new("damdMatrix",
                                    address = tmp@address,
                                    .context_index = tmp@.context_index,
                                    .platform_index = tmp@.platform_index,
                                    .platform = tmp@.platform,
                                    .device_index = tmp@.device_index,
                                    .device = tmp@.device)
                            },
                            stop("this is an unrecognized 
                                 or unimplemented data type")
              )
              
              return(data)
          },
          valueClass = "amdMatrix")

