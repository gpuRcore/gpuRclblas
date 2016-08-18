#define __CL_ENABLE_EXCEPTIONS

#include "gpuR/dynEigenMat.hpp"

// clBLAS and OpenCL headers
#include <clBLAS.h>

#include <string>

using namespace Rcpp;

void 
cpp_amdMatrix_daxpy(SEXP alpha_, SEXP ptrA_, SEXP ptrB_,
                    int ctx_id)
{
    
    const cl_double alpha = as<cl_double>(alpha_);
    const int incx = 1;
    const int incy = 1;

    XPtr<dynEigenMat<double> > ptrA(ptrA_);
    XPtr<dynEigenMat<double> > ptrB(ptrB_);
    
    // move data to device
    viennacl::matrix<double> vcl_A = ptrA->device_data(ctx_id);
    viennacl::matrix<double> vcl_B = ptrB->device_data(ctx_id);
                              
    // total number of elements
    // const int N = Am.n_elem;
    const int N = vcl_A.internal_size1() * vcl_A.internal_size2();
    
    // declare OpenCL objects
    cl_int err;
    
    // Create a command queue and use the first device
    // CommandQueue queue = CommandQueue(context, devices[0], 0, &err);
    cl_command_queue queue = vcl_A.handle().opencl_handle().context().get_queue().handle().get();
    
    /* Setup clblas. */
    err = clblasSetup();
    if (err != CL_SUCCESS) {
        stop("clblasSetup() failed with " + std::to_string(err));
    }
    
    /* Prepare OpenCL memory objects and place matrices inside them. */
    // Get memory buffers
    const cl_mem *bufA = &vcl_A.handle().opencl_handle().get();
    const cl_mem *bufB = &vcl_B.handle().opencl_handle().get();
    
    /* Call clblas extended function. Perform gemm */
    err = clblasDaxpy(N, alpha, *bufA, 0, incx,
                         *bufB, 0, incy, 1,
                         &queue, 0, NULL, 0);
    if (err != CL_SUCCESS) {
        stop("clblasDaxpy() failed");
    }
    
    /* Finalize work with clblas. */
    clblasTeardown();
    
    // Copy back to host
    ptrB->to_host(vcl_B);
}


void 
cpp_amdMatrix_saxpy(SEXP alpha_, SEXP ptrA_, SEXP ptrB_,
                    int ctx_id)
{
    
    const cl_float alpha = as<cl_double>(alpha_);
    const int incx = 1;
    const int incy = 1;
    
    XPtr<dynEigenMat<float> > ptrA(ptrA_);
    XPtr<dynEigenMat<float> > ptrB(ptrB_);
    
    // move data to device
    viennacl::matrix<float> vcl_A = ptrA->device_data(ctx_id);
    viennacl::matrix<float> vcl_B = ptrB->device_data(ctx_id);
    
    // total number of elements
    // const int N = Am.n_elem;
    const int N = vcl_A.internal_size1() * vcl_A.internal_size2();
    
    // declare OpenCL objects
    cl_int err;
    
    // Create a command queue and use the first device
    // CommandQueue queue = CommandQueue(context, devices[0], 0, &err);
    cl_command_queue queue = vcl_A.handle().opencl_handle().context().get_queue().handle().get();
    
    /* Setup clblas. */
    err = clblasSetup();
    if (err != CL_SUCCESS) {
        stop("clblasSetup() failed with " + std::to_string(err));
    }
    
    /* Prepare OpenCL memory objects and place matrices inside them. */
    // Get memory buffers
    const cl_mem *bufA = &vcl_A.handle().opencl_handle().get();
    const cl_mem *bufB = &vcl_B.handle().opencl_handle().get();
    
    /* Call clblas extended function. Perform gemm */
    err = clblasSaxpy(N, alpha, *bufA, 0, incx,
                      *bufB, 0, incy, 1,
                      &queue, 0, NULL, 0);
                      if (err != CL_SUCCESS) {
                          stop("clblasDaxpy() failed");
                      }
                      
                      /* Finalize work with clblas. */
                      clblasTeardown();
                      
                      // Copy back to host
                      ptrB->to_host(vcl_B);
}


//[[Rcpp::export]]
void
cpp_amdMatrix_axpy(
    SEXP alpha,
    SEXP ptrA, SEXP ptrB,
    const int type_flag,
    int ctx_id)
{
    
    switch(type_flag) {
        case 6:
            cpp_amdMatrix_saxpy(alpha, ptrA, ptrB, ctx_id);
            return;
        case 8:
            cpp_amdMatrix_daxpy(alpha, ptrA, ptrB, ctx_id);
            return;
        default:
            throw Rcpp::exception("unknown type detected for amdMatrix object!");
    }
}
