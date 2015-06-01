
#' @export
setMethod("%*%", signature(x="amdMatrix", y = "amdMatrix"),
          function(x,y)
          {
            if( dim(x)[2] != dim(y)[1]){
              stop("Non-conformant matrices")
            }
            return(amdMatMult(x, y))
          },
          valueClass = "gpuMatrix"
)


#' @export
setMethod("Arith", c(e1="amdMatrix", e2="amdMatrix"),
          function(e1, e2)
          {
            op = .Generic[[1]]
            switch(op,
                   `+` = amdMataxpy(1, e1, e2),
                   `-` = amdMataxpy(-1, e2, e1),
                  {
                    stop("undefined operation")
                  }
            )
          },
valueClass = "gpuMatrix"
)


#' @export
setMethod('typeof', signature(x="amdMatrix"),
          function(x) return(x@type))