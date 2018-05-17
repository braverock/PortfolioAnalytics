#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP residualcokurtosisMF(SEXP, SEXP, SEXP, SEXP);
extern SEXP residualcokurtosisSF(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"residualcokurtosisMF", (DL_FUNC) &residualcokurtosisMF, 4},
    {"residualcokurtosisSF", (DL_FUNC) &residualcokurtosisSF, 5},
    {NULL, NULL, 0}
};

void R_init_PortfolioAnalytics(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}