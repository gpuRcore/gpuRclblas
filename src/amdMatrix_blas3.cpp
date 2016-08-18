#define __CL_ENABLE_EXCEPTIONS

// #define VIENNACL_DEBUG_ALL 1

#include "gpuR/dynEigenMat.hpp"

// clBLAS and OpenCL headers
#include <clBLAS.h>

#include <string>

using namespace Rcpp;

// can add more arguments for more control of sgemm call
// e.g. if transpose needed?

void cpp_amdMatrix_dgemm(SEXP ptrA_, SEXP ptrB_, SEXP ptrC_,
                         int ctx_id)
{
    
    // probably need to set to clblasRowMajor
    static const clblasOrder order = clblasRowMajor;
    static const cl_double alpha = 1;
    static const clblasTranspose transA = clblasNoTrans;
    
    XPtr<dynEigenMat<double> > ptrA(ptrA_);
    XPtr<dynEigenMat<double> > ptrB(ptrB_);
    XPtr<dynEigenMat<double> > ptrC(ptrC_);
    
    // move data to device
    viennacl::matrix<double> vcl_A = ptrA->device_data(ctx_id);
    viennacl::matrix<double> vcl_B = ptrB->device_data(ctx_id);
    viennacl::matrix<double> vcl_C = ptrC->device_data(ctx_id);
    
    const unsigned int lda = vcl_A.internal_size1();        /* i.e. lda = K */
    const clblasTranspose transB = clblasNoTrans;
    
    const unsigned int ldb = vcl_B.internal_size1();        /* i.e. ldb = N */
    const cl_double beta = 0;
    
    const unsigned int ldc = vcl_B.internal_size1();        /* i.e. ldc = N */
    
    // declare OpenCL objects
    cl_int err;
    
    // Get command queue and use the first device
    // cl_command_queue queue = viennacl::ocl::current_context().get_queue().handle().get();
    cl_command_queue queue = vcl_A.handle().opencl_handle().context().get_queue().handle().get();
    
    // 
    /* Setup clblas. */
    err = clblasSetup();
    if (err != CL_SUCCESS) {
        stop("clblasSetup() failed with " + std::to_string(err));
    }
    
    // Get memory buffers
    const cl_mem *bufA = &vcl_A.handle().opencl_handle().get();
    const cl_mem *bufB = &vcl_B.handle().opencl_handle().get();
    const cl_mem *bufC = &vcl_C.handle().opencl_handle().get();
    
    /* Call clblas extended function. Perform gemm */
    err = clblasDgemm(order, transA, transB, 
                      vcl_A.size2(), 
                      vcl_B.size1(), vcl_A.size1(),
                      alpha, *bufA, 0, lda,
                      *bufB, 0, ldb, beta,
                      *bufC, 0, ldc,
                      1, 
                      &queue,
                      0, NULL, 0);
    
    if (err != CL_SUCCESS) {
        stop("clblasDgemmEx() failed with ", std::to_string(err));
    }
    
    /* Finalize work with clblas. */
    clblasTeardown();
    
    ptrC->to_host(vcl_C);
}


void cpp_amdMatrix_sgemm(SEXP ptrA_, SEXP ptrB_, SEXP ptrC_,
                         int ctx_id)
{
    
    // probably need to set to clblasRowMajor
    static const clblasOrder order = clblasRowMajor;
    static const cl_float alpha = 1;
    static const clblasTranspose transA = clblasNoTrans;
    
    XPtr<dynEigenMat<float> > ptrA(ptrA_);
    XPtr<dynEigenMat<float> > ptrB(ptrB_);
    XPtr<dynEigenMat<float> > ptrC(ptrC_);
    
    // move data to device
    viennacl::matrix<float> vcl_A = ptrA->device_data(ctx_id);
    viennacl::matrix<float> vcl_B = ptrB->device_data(ctx_id);
    viennacl::matrix<float> vcl_C = ptrC->device_data(ctx_id);
    
    const unsigned int lda = vcl_A.internal_size1();        /* i.e. lda = K */
    const clblasTranspose transB = clblasNoTrans;
    
    const unsigned int ldb = vcl_B.internal_size1();        /* i.e. ldb = N */
    const cl_float beta = 0;
    
    const unsigned int ldc = vcl_B.internal_size1();        /* i.e. ldc = N */
    
    // declare OpenCL objects
    cl_int err;
    
    // Get command queue and use the first device
    // cl_command_queue queue = viennacl::ocl::current_context().get_queue().handle().get();
    cl_command_queue queue = vcl_A.handle().opencl_handle().context().get_queue().handle().get();
    
    // 
    /* Setup clblas. */
    err = clblasSetup();
    if (err != CL_SUCCESS) {
        stop("clblasSetup() failed with " + std::to_string(err));
    }
    
    // Get memory buffers
    const cl_mem *bufA = &vcl_A.handle().opencl_handle().get();
    const cl_mem *bufB = &vcl_B.handle().opencl_handle().get();
    const cl_mem *bufC = &vcl_C.handle().opencl_handle().get();
    
    /* Call clblas extended function. Perform gemm */
    err = clblasSgemm(order, transA, transB, 
                      vcl_A.size2(), 
                      vcl_B.size1(), vcl_A.size1(),
                      alpha, *bufA, 0, lda,
                      *bufB, 0, ldb, beta,
                      *bufC, 0, ldc,
                      1, 
                      &queue,
                      0, NULL, 0);
    
    if (err != CL_SUCCESS) {
        stop("clblasSgemmEx() failed with ", std::to_string(err));
    }
    
    /* Finalize work with clblas. */
    clblasTeardown();
    
    ptrC->to_host(vcl_C);
}


//[[Rcpp::export]]
void
cpp_amdMatrix_gemm(
    SEXP ptrA, SEXP ptrB, SEXP ptrC,
    const int type_flag,
    int ctx_id)
{
    
    switch(type_flag) {
        case 6:
            cpp_amdMatrix_sgemm(ptrA, ptrB, ptrC, ctx_id);
            return;
        case 8:
            cpp_amdMatrix_dgemm(ptrA, ptrB, ptrC, ctx_id);
            return;
        default:
            throw Rcpp::exception("unknown type detected for amdMatrix object!");
    }
}

