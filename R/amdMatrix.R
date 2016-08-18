
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
            
            device <- currentDevice()
            
            context_index <- ifelse(is.null(ctx_id), currentContext(), as.integer(ctx_id))
            device_index <- as.integer(device$device_index)
            device_type <- device$device_type
            device_name <- switch(device_type,
                                  "gpu" = gpuInfo(device_idx = as.integer(device_index))$deviceName,
                                  "cpu" = cpuInfo(device_idx = as.integer(device_index))$deviceName,
                                  stop("Unrecognized device type")
            )
            platform_index <- currentPlatform()$platform_index
            platform_name <- platformInfo(platform_index)$platformName
            
            data = switch(type,
                          integer = {
                            new("iamdMatrix", 
                                address=getRmatEigenAddress(data, 
                                                        nrow(data),
                                                        ncol(data), 
                                                        4L),
                                .context_index = context_index,
                                .platform_index = platform_index,
                                .platform = platform_name,
                                .device_index = device_index,
                                .device = device_name)
                          },
                          float = {
                            new("famdMatrix", 
                                address=getRmatEigenAddress(data, 
                                                        nrow(data),
                                                        ncol(data), 
                                                        6L),
                                .context_index = context_index,
                                .platform_index = platform_index,
                                .platform = platform_name,
                                .device_index = device_index,
                                .device = device_name)
                          },
                          double = {
                            assert_has_double(platform_index, device_index)
                            new("damdMatrix",
                                address = getRmatEigenAddress(data, 
                                                          nrow(data),
                                                          ncol(data), 
                                                          8L),
                                .context_index = context_index,
                                .platform_index = platform_index,
                                .platform = platform_name,
                                .device_index = device_index,
                                .device = device_name)
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
            
            if (is.null(type)) type <- getOption("gpuR.default.type")
            
            assert_is_numeric(nrow)
            assert_is_numeric(ncol)
            
            device <- currentDevice()
            
            context_index <- ifelse(is.null(ctx_id), currentContext(), as.integer(ctx_id))
            device_index <- as.integer(device$device_index)
            device_type <- device$device_type
            device_name <- switch(device_type,
                                  "gpu" = gpuInfo(device_idx = as.integer(device_index))$deviceName,
                                  "cpu" = cpuInfo(device_idx = as.integer(device_index))$deviceName,
                                  stop("Unrecognized device type")
            )
            platform_index <- currentPlatform()$platform_index
            platform_name <- platformInfo(platform_index)$platformName
            
            data = switch(type,
                          integer = {
                            new("iamdMatrix", 
                                address=emptyEigenXptr(nrow, ncol, 4L),
                                .context_index = context_index,
                                .platform_index = platform_index,
                                .platform = platform_name,
                                .device_index = device_index,
                                .device = device_name)
                          },
                          float = {
                            new("famdMatrix", 
                                address=emptyEigenXptr(nrow, ncol, 6L),
                                .context_index = context_index,
                                .platform_index = platform_index,
                                .platform = platform_name,
                                .device_index = device_index,
                                .device = device_name)
                          },
                          double = {
                            assert_has_double(platform_index, device_index)
                            new("damdMatrix",
                                address = emptyEigenXptr(nrow, ncol, 8L),
                                .context_index = context_index,
                                .platform_index = platform_index,
                                .platform = platform_name,
                                .device_index = device_index,
                                .device = device_name)
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
              
              if (is.null(type)) type <- "double"
              
              assert_is_numeric(nrow)
              assert_is_numeric(ncol)
              
              device <- currentDevice()
              
              context_index <- ifelse(is.null(ctx_id), currentContext(), as.integer(ctx_id))
              device_index <- as.integer(device$device_index)
              device_type <- device$device_type
              device_name <- switch(device_type,
                                    "gpu" = gpuInfo(device_idx = as.integer(device_index))$deviceName,
                                    "cpu" = cpuInfo(device_idx = as.integer(device_index))$deviceName,
                                    stop("Unrecognized device type")
              )
              platform_index <- currentPlatform()$platform_index
              platform_name <- platformInfo(platform_index)$platformName
              
              if(length(data) > 1){
                  data = switch(type,
                                float = {
                                    new("famdMatrix", 
                                        address=sexpVecToEigenXptr(data, nrow, ncol, 6L),
                                        .context_index = context_index,
                                        .platform_index = platform_index,
                                        .platform = platform_name,
                                        .device_index = device_index,
                                        .device = device_name)
                                },
                                double = {
                                    assert_has_double(platform_index, device_index)
                                    new("damdMatrix",
                                        address = sexpVecToEigenXptr(data, nrow, ncol, 8L),
                                        .context_index = context_index,
                                        .platform_index = platform_index,
                                        .platform = platform_name,
                                        .device_index = device_index,
                                        .device = device_name)
                                },
                                stop("this is an unrecognized 
                                     or unimplemented data type")
                                )
              }else{
                  data = switch(type,
                                float = {
                                    new("famdMatrix", 
                                        address=initScalarEigenXptr(data, nrow, ncol, 6L),
                                        .context_index = context_index,
                                        .platform_index = platform_index,
                                        .platform = platform_name,
                                        .device_index = device_index,
                                        .device = device_name)
                                },
                                double = {
                                    assert_has_double(platform_index, device_index)
                                    new("damdMatrix",
                                        address = initScalarEigenXptr(data, nrow, ncol, 8L),
                                        .context_index = context_index,
                                        .platform_index = platform_index,
                                        .platform = platform_name,
                                        .device_index = device_index,
                                        .device = device_name)
                                },
                                stop("this is an unrecognized 
                                 or unimplemented data type")
                  )
              }
              
              return(data)
          },
          valueClass = "amdMatrix")

