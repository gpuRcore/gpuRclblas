#' @title amdMatrix Class
#' @description This is the 'mother' class for all
#' amdMatrix objects.  It is a child class from the
#' gpuMatrix class from gpuR.  All other 
#' amdMatrix classes inherit from this class but 
#' there are no current circumstances where this class 
#' is used directly.
#' 
#' There are multiple child classes that correspond
#' to the particular data type contained.  These include
#' \code{iamdMatrix}, \code{famdMatrix}, and 
#' \code{damdMatrix} corresponding to integer, float, and
#' double data types respectively.
#' @section Slots:
#'  Common to all amdMatrix objects in the package
#'  \describe{
#'      \item{\code{x}:}{An R matrix object}
#'      \item{\code{type}:}{Character object specifying
#'      the type the matrix data will be interpreted as}
#'  }
#' @note R does not contain a native float type.  As such,
#' the matrix data within a \code{\link{famdMatrix-class}} 
#' will be represented as double but downcast when any 
#' amdMatrix methods are used.
#' 
#' May also remove the type slot
#' 
#' @name amdMatrix-class
#' @rdname amdMatrix-class
#' @author Charles Determan Jr.
#' @seealso \code{\link{iamdMatrix-class}}, 
#' \code{\link{famdMatrix-class}},
#' \code{\link{damdMatrix-class}}
#' @importClassesFrom gpuR gpuMatrix
#' @export
setClass('amdMatrix', 
         contains = 'gpuMatrix')

#' @title iamdMatrix Class
#' @description An integer type matrix in the S4 \code{amdMatrix}
#' representation.
#' @section Slots:
#'  \describe{
#'      \item{\code{x}:}{A integer typed R matrix}
#'      \item{\code{type}:}{Character object specifying
#'      the type the matrix data is integer}
#'  }
#' @name iamdMatrix-class
#' @rdname iamdMatrix-class
#' @author Charles Determan Jr.
#' @seealso \code{\link{amdMatrix-class}}, 
#' \code{\link{iamdMatrix-class}},
#' \code{\link{damdMatrix-class}}
#' @export
setClass("iamdMatrix",
         contains = "amdMatrix",
         validity = function(object) {
           if( typeof(object) != "integer"){
             return("iamdMatrix must be of type 'integer'")
           }
           TRUE
         })

#' @title famdMatrix Class
#' @description An integer type matrix in the S4 \code{amdMatrix}
#' representation.
#' @section Slots:
#'  \describe{
#'      \item{\code{x}:}{A numeric R matrix.}
#'      \item{\code{type}:}{Character object specifying
#'      the type the matrix data is intepreted as float}
#'  }
#' @name famdMatrix-class
#' @rdname famdMatrix-class
#' @author Charles Determan Jr.
#' @seealso \code{\link{amdMatrix-class}}, 
#' \code{\link{iamdMatrix-class}},
#' \code{\link{damdMatrix-class}}
#' @export
setClass("famdMatrix",
         contains = "amdMatrix",
         validity = function(object) {
           if( typeof(object) != "float"){
             return("famdMatrix must be of type 'float'")
           }
           TRUE
         })


#' @title damdMatrix Class
#' @description An integer type matrix in the S4 \code{amdMatrix}
#' representation.
#' @section Slots:
#'  \describe{
#'      \item{\code{x}:}{A numeric R matrix}
#'      \item{\code{type}:}{Character object specifying
#'      the type the matrix data is double}
#'  }
#' @name damdMatrix-class
#' @rdname damdMatrix-class
#' @author Charles Determan Jr.
#' @seealso \code{\link{amdMatrix-class}}, 
#' \code{\link{iamdMatrix-class}},
#' \code{\link{famdMatrix-class}}
#' @export
setClass("damdMatrix",
         contains = "amdMatrix",
         validity = function(object) {
           if( typeof(object) != "double"){
             return("damdMatrix must be of type 'double'")
           }
           TRUE
         })
