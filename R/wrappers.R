
#' @useDynLib gpuRclblas
#' @importFrom Rcpp evalCpp
NULL


#' @import assertive

# GPU Matrix Multiplication
amd_Mat_mult <- function(A, B){
  
  assert_are_identical(A@.context_index, B@.context_index)
  
  type <- typeof(A)
  
  C <- amdMatrix(nrow=nrow(A), ncol=ncol(B), type=type, ctx_id = A@.context_index)
  
  switch(type,
         integer = {
           stop("integer not currently implemented")
           
           # file <- system.file("CL", "basic_gemm.cl", package = "gpuR")
           # 
           # if(!file_test("-f", file)){
           #   stop("kernel file does not exist")
           # }
           # kernel <- readChar(file, file.info(file)$size)
           # 
           # maxWorkGroupSize <- 
           #   switch(deviceType(C@.platform_index, C@.device_index),
           #          "gpu" = gpuInfo(C@.platform_index, C@.device_index)$maxWorkGroupSize,
           #          "cpu" = cpuInfo(C@.platform_index, C@.device_index)$maxWorkGroupSize,
           #          stop("unrecognized device type")
           #   )
           # 
           # cpp_gpuMatrix_custom_igemm(A@address,
           #                            B@address,
           #                            C@address,
           #                            kernel,
           #                            sqrt(maxWorkGroupSize),
           #                            A@.context_index - 1)
         },
         float = {cpp_amdMatrix_gemm(A@address,
                                     B@address,
                                     C@address,
                                     6L,
                                     A@.context_index - 1)
         },
         double = {
           cpp_amdMatrix_gemm(A@address,
                              B@address,
                              C@address,
                              8L,
                              A@.context_index - 1)
         },
         stop("type not recognized")
  )
  
  return(C)
}


# GPU axpy wrapper
amd_Mat_axpy <- function(alpha, A, B){
    
    assert_are_identical(A@.context_index, B@.context_index)
    
    nrA = nrow(A)
    ncA = ncol(A)
    nrB = nrow(B)
    ncB = ncol(B)
    
    type <- typeof(A)
    
    Z <- amdMatrix(nrow=nrB, ncol=ncA, type=type, ctx_id = A@.context_index)
    if(!missing(B))
    {
        if(length(B) != length(A)) stop("Lengths of matrices must match")
        Z <- deepcopy(B)
    }
    
    switch(type,
           float = {cpp_amdMatrix_axpy(alpha, 
                                       A@address, 
                                       Z@address, 
                                       6L,
                                       A@.context_index - 1)
           },
           double = {cpp_amdMatrix_axpy(alpha, 
                                        A@address,
                                        Z@address,
                                        8L,
                                        A@.context_index - 1)
           },
           stop("type not recognized")
    )
    
    return(Z)
}
