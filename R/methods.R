
#' @export
setMethod("%*%", signature(x="amdMatrix", y = "amdMatrix"),
          function(x,y)
          {
              if( dim(x)[2] != dim(y)[1]){
                  stop("Non-conformant matrices")
              }
              
              # type <- typeof(x)
              # 
              # C <- amdMatrix(nrow=nrow(x), ncol=ncol(y), type=type, ctx_id = x@.context_index)
              # 
              # if(!deviceHasDouble()){
              #   stop("Selected GPU does not support double precision")
              # }else{
              #   cpp_amdMatrix_dgemm(x@address, y@address, C@address, x@.context_index)
              # }
              
              return(amd_Mat_mult(x, y))
          },
          valueClass = "amdMatrix"
)



#' @export
setMethod("Arith", c(e1="amdMatrix", e2="amdMatrix"),
          function(e1, e2)
          {
              op = .Generic[[1]]
              switch(op,
                     `+` = amd_Mat_axpy(1, e1, e2),
                     `-` = amd_Mat_axpy(-1, e2, e1),
                     stop("undefined operation")
              )
          },
          valueClass = "damdMatrix"
)

#' @title Copy a "gpuRclblas" object
#' @description This is needed to make a duplicate of a gpuRclblas object 
#' @param object A gpuRclblas object
#' @details This is needed to make a duplicate of a gpuRclblas object 
#' (i.e. \code{\link{amdMatrix}}, \code{\link{amdVector}}, 
#' \code{\link{vclAMDMatrix}}, \code{\link{vclAMDVector}} because
#' the traditional syntax would only copy the pointer of the object.
#' @return A gpuRclblas object
#' @author Charles Determan Jr.
#' @docType methods
#' @rdname gpuRclblas-deepcopy
setMethod("deepcopy", signature(object ="amdMatrix"),
          function(object){
              
              out <- switch(typeof(object),
                            "float" = new("famdMatrix", 
                                          address = cpp_deepcopy_gpuMatrix(object@address, 6L),
                                          .context_index = object@.context_index,
                                          .platform_index = object@.platform_index,
                                          .platform = object@.platform,
                                          .device_index = object@.device_index,
                                          .device = object@.device),
                            "double" = new("damdMatrix", 
                                           address = cpp_deepcopy_gpuMatrix(object@address, 8L),
                                           .context_index = object@.context_index,
                                           .platform_index = object@.platform_index,
                                           .platform = object@.platform,
                                           .device_index = object@.device_index,
                                           .device = object@.device),
                            stop("unrecognized type")
              )
              return(out)
              
          })
