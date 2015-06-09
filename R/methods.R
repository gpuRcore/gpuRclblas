
#' @export
setMethod("%*%", signature(x="famdMatrix", y = "famdMatrix"),
          function(x,y)
          {
            if( dim(x)[2] != dim(y)[1]){
              stop("Non-conformant matrices")
            }
            new("famdMatrix", 
                x=cpp_amdMatrix_sgemm(x@x,y@x),
                type="float"
            )
          },
          valueClass = "famdMatrix"
)

#' @export
setMethod("%*%", signature(x="damdMatrix", y = "damdMatrix"),
          function(x,y)
          {
            if( dim(x)[2] != dim(y)[1]){
              stop("Non-conformant matrices")
            }
            if(!deviceHasDouble()){
              stop("Selected GPU does not support double precision")
            }else{
              new("damdMatrix", 
                  x=cpp_amdMatrix_dgemm(x@x,y@x),
                  type="double"
              )
            }
          },
          valueClass = "damdMatrix"
)


#' @export
setMethod("Arith", c(e1="famdMatrix", e2="famdMatrix"),
          function(e1, e2)
          {
            nrA = nrow(e1)
            ncA = ncol(e1)
            nrB = nrow(e2)
            ncB = ncol(e2)
            
            Z <- matrix(0, nrow=nrB, ncol=ncA)
            if(!missing(e2))
            {
              if(length(e2@x) != length(e1@x)) stop("Lengths of matrices must match")
              Z <- e2@x
            }
            
            op = .Generic[[1]]
            switch(op,
                   `+` = new("famdMatrix", 
                             x=cpp_amdMatrix_saxpy(1, e1@x, Z),
                             type="float"
                   ),
                   `-` = new("famdMatrix", 
                             x=cpp_amdMatrix_saxpy(-1, Z, e1@x),
                             type="float"
                   ),
                  {
                    stop("undefined operation")
                  }
            )
          },
valueClass = "famdMatrix"
)

#' @export
setMethod("Arith", c(e1="damdMatrix", e2="damdMatrix"),
          function(e1, e2)
          {
            if(!deviceHasDouble()){
              stop("Selected GPU does not support double precision")
            }else{
              nrA = nrow(e1)
              ncA = ncol(e1)
              nrB = nrow(e2)
              ncB = ncol(e2)
              
              Z <- matrix(0, nrow=nrB, ncol=ncA)
              if(!missing(e2))
              {
                if(length(e2@x) != length(e1@x)) stop("Lengths of matrices must match")
                Z <- e2@x
              }
              
              op = .Generic[[1]]
              switch(op,
                     `+` = new("damdMatrix", 
                               x=cpp_amdMatrix_daxpy(1, e1@x, Z),
                               type="double"
                     ),
                     `-` = new("damdMatrix", 
                               x=cpp_amdMatrix_daxpy(-1, Z, e1@x),
                               type="double"
                     ),
                    {
                      stop("undefined operation")
                    }
              )
            }
          },
valueClass = "damdMatrix"
)

