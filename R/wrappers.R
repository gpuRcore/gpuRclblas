
#' @useDynLib gpuRclblas
#' @importFrom Rcpp evalCpp

#' @title AMD GPU Matrix Multiplication
#' @description matrix multiplication
#' @param A A amdMatrix object
#' @param B A amdMatrix object
#' @return A amdMatrix object
#' @note Please note that for integer matrices there are no defined BLAS 
#' routines.  As such, any 'integer' type matrices will currently deprecate
#' to the \link[gpuR]{gpuMatrix} class.
#' @author Charles Determan Jr.
#' @importFrom gpuR deviceHasDouble
amdMatMult <- function(A, B){
  
  type <- typeof(A)
  
  out <- switch(type,
                integer = {
                  # this is not ideal, would prefer to redirect instead
                  # of using ':::' but currently only way I can think of
                  # with the use of the switch statement
                  class(A) <- "igpuMatrix"
                  class(B) <- "igpuMatrix"
                  A %*% B
#                   new("igpuMatrix", 
#                       x=gpuR:::cpp_gpuMatrix_igemm(A@x,B@x, kernel, "iMatMult"),
#                       type="integer"
#                   )
                },
                float = {
                    new("famdMatrix", 
                        x=cpp_amdMatrix_sgemm(A@x,B@x),
                        type="float"
                    )
                },
                double = {
                  if(!deviceHasDouble()){
                    stop("Selected GPU does not support double precision")
                  }else{
                    new("damdMatrix", 
                        x=cpp_amdMatrix_dgemm(A@x,B@x),
                        type="double"
                    )
                  }
                },
{
  stop("type not recognized")
})
return(out)
}


#' @title AMD GPU Matrix Multiplication
#' @description matrix multiplication
#' @param alpha Numeric value to multiply the A matrix
#' @param A A amdMatrix object
#' @param B A amdMatrix object
#' @return A amdMatrix or gpuMatrix (if data type is 'integer') object
#' @import bigalgebra 
#' @import bigmemory
#' @author Charles Determan Jr.
#' @export
amdMataxpy <- function(alpha, A, B){
  
  nrA = nrow(A)
  ncA = ncol(A)
  nrB = nrow(B)
  ncB = ncol(B)
  
  type <- typeof(A)
  
  Z <- matrix(0, nrow=nrB, ncol=ncA)
  if(!missing(B))
  {
    if(length(B@x) != length(A@x)) stop("Lengths of matrices must match")
    Z <- B@x
  }
  
  out <- switch(type,
                integer = {
                  class(A) <- "igpuMatrix"
                  class(B) <- "igpuMatrix"
                  
                  switch(as.character(alpha),
                         "1" = A + B,
                         "-1" = B - A,
                         stop("alpha not 0 or 1 not implemented yet")
                  )
#                   new("igpuMatrix", 
#                       x=cpp_gpuMatrix_iaxpy(alpha, A@x,Z, kernel, "iaxpy"),
#                       type="integer"
#                   )
                },
                float = {
                  new("famdMatrix", 
                      x=cpp_amdMatrix_saxpy(alpha, A@x, Z),
                      type="float"
                  )
                },
                double = {
                  new("damdMatrix", 
                      x=cpp_amdMatrix_daxpy(alpha, A@x,Z),
                      type="double"
                  )
                },
{
  stop("type not recognized")
}
  )

return(out)
}