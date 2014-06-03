
#include <R.h>
#include <Rinternals.h>

SEXP  residualcokurtosisMF(SEXP NN, SEXP sstockM2, SEXP sstockM4, SEXP bbetacov){
    /*
     arguments
     NN       : integer, number of assets
     sstockM2 : vector of length NN, 2nd moment of the model residuals
     sstockM4 : vector of length NN, 4th moment of the model residuals
     bbetacov : vector of length NN * NN, beta and factor covariance
     
     Note that this function was orignally written in C++ (using Rcpp) by
     Joshua Ulrich and re-written using the C API by Ross Bennett
     */
    
    // // declare pointers for the vectors
    double *stockM2, *stockM4, *betacov;
    
    // Do I need to protect these if they are function arguments?
    // use REAL() to access the C array inside the numeric vector passed in from R
    stockM2 = REAL(sstockM2);
    stockM4 = REAL(sstockM4);
    betacov = REAL(bbetacov);
    
    // Coerce length one R vector into C scalar
    int N = asInteger(NN);
    
    // Allocate and initialize matrix values to 0
    // Do I need to fill the matrix with zeros here?
    SEXP M4MF = PROTECT(allocMatrix(REALSXP, N, N*N*N));
    double *rM4MF = REAL(M4MF);
    // Note that the matrix is stored in column-major order and is represented
    // as a 1-dimensional array. Access the element in X(row, col) with:
    // element = row + column * nrows
    // outer loop over the columns and inner loop down the row
    for (int jj = 0; jj < N*N*N; jj++) {
      for (int ii = 0; ii < N; ii++) {
        rM4MF[ii + jj * N] = 0.0;
      }
    }
    
    // Rcpp declarations
    // NumericVector stockM2(sstockM2);
    // NumericVector stockM4(sstockM4);
    // NumericVector betacov(bbetacov);
    // int N = as<int>(NN);
    // NumericMatrix M4MF(N, pow(N,3));
    
    double kijkl = 0.0;
    int pos = 0;
    
    for(int i=0; i<N; i++) {
        for(int j=0; j<N; j++) {
            for(int k=0; k<N; k++) {
                for(int l=0; l<N; l++) {
                    // in the general case: i, j , k and l are all different
                    kijkl = 0;
                    // if at least one of them are equal:
                    if( (i==j) || (i==k) || (i==l) || (j==k) || (j==l) || (k==l) ) {
                        if( (i==j) && (i==k) && (i==l) ) {
                            // These are the kurtosis estimates of the individual assets: 6 bi S bi E[ei^2] + E[ei^4]
                            // this is element i,i in the cov, therefore in the vectorized form it is i*N+i
                            pos = i*N+i;
                            kijkl = 6*betacov[pos]*stockM2[i]+stockM4[i];
                        } else {
                            if( ((i==j) && (i==k)) || ((i==j) && (i==l)) || ((i==k) && (i==l)) || ((j==k) && (j==l)) ) {
                                // 3 indices are identical
                                if( (i==j) && (i==k) ) {
                                    // all indices are the same except l: 3 biSbl E[ei^2]
                                    pos = i*N+l;
                                    kijkl = 3*betacov[pos]*stockM2[i];
                                } else
                                    if( (i==j) && (i==l) ) {
                                        // all indices are the same except k: 3 biSbk E[ei^2]
                                        pos = i*N+k;
                                        kijkl = 3*betacov[pos]*stockM2[i];
                                    } else
                                        if( (i==k) && (i==l) ) {
                                            // all indices are the same except j: 3 biSbj E[ei^2]
                                            pos =i*N+j;
                                            kijkl = 3*betacov[pos]*stockM2[i];
                                        } else
                                            if( (j==k) && (j==l) ) {
                                                // all indices are the same except i: 3 biSbi E[ei^2]
                                                pos =j*N+i;
                                                kijkl = 3*betacov[pos]*stockM2[j];
                                            }
                            }
                            else {
                                if( ((i==j) && (k==l)) || ((i==k) && (j==l)) || ((i==l) && (j==k)) ) {
                                    // kiijj = E[ U[,i]^2*U[,j]^2 ] = r5*sqrt( vm4[i]*vm4[j]  )
                                    if( (i==j) && (k==l) ) {
                                        // biSbi E[ek^2] + bkSbk E[ei^2]	+ E[ei^2]E[ek^2]
                                        pos = i*N+i;
                                        kijkl = betacov[pos]*stockM2[k];
                                        pos = k*N+k;
                                        kijkl = kijkl + betacov[pos]*stockM2[i]+stockM2[i]*stockM2[k];
                                    } else
                                        if( (i==k) && (j==l) ) {
                                            // biSbi E[ej^2] + bjSbj E[ei^2] + E[ei^2]E[ej^2]
                                            pos =i*N+i;
                                            kijkl = betacov[pos]*stockM2[j];
                                            pos = j*N+j;
                                            kijkl = betacov[pos]*stockM2[i]+stockM2[i]*stockM2[j];
                                        } else
                                            if( (i==l) && (j==k) ) {
                                                // biSbi E[ej^2] + bjSbj E[ei^2] + E[ei^2]E[ej^2]
                                                pos =i*N+i;
                                                kijkl = betacov[pos]*stockM2[j];
                                                pos = j*N+j;
                                                kijkl = kijkl + betacov[pos]*stockM2[i]+stockM2[i]*stockM2[j];
                                            }
                                } else {
                                    if( i==j ) {
                                        // bkSbl E[ei^2]
                                        pos = k*N+l;
                                        kijkl = betacov[pos]*stockM2[i];
                                    } else
                                        if( i==k ) {
                                            // bjSbl E[ei^2]
                                            pos = j*N+l;
                                            kijkl = betacov[pos]*stockM2[i];
                                        } else
                                            if( i==l ) {
                                                // bjSbk E[ei^2]
                                                pos = j*N+k;
                                                kijkl = betacov[pos]*stockM2[i];
                                            } else
                                                if( j==k ) {
                                                    // biSbl E[ej^2]
                                                    pos = i*N+l;
                                                    kijkl = betacov[pos]*stockM2[j];
                                                } else
                                                    if( j==l ) {
                                                        // biSbk E[ej^2]
                                                        pos = i*N+k;
                                                        kijkl = betacov[pos]*stockM2[j];
                                                    } else
                                                        if( k==l ) {
                                                            // biSbj E[ek^2]
                                                            pos = i*N+j;
                                                            kijkl = betacov[pos]*stockM2[k];
                                                        }
                                }
                            }
                        }  // no kurtosis estimates of individual assets
                    }  // at least one of them are not equal
                    // i denotes the subcomoment matrix
                    //M4MF(k,i*pow(N,2)+j*N+l) = kijkl;
                    // Element = row + column * nrows
                    rM4MF[k + (i * N * N + j * N + l) * N] = kijkl;
                } // loop l
            } // loop k
        } // loop j
    } // loop i
    UNPROTECT(1);
    return M4MF;
}