#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP ggraph_circlePackLayout(SEXP, SEXP);
extern SEXP ggraph_cut_lines(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP ggraph_pack(SEXP);
extern SEXP ggraph_partitionTree(SEXP, SEXP, SEXP, SEXP);
extern SEXP ggraph_pathAttr(SEXP, SEXP);
extern SEXP ggraph_splitTreemap(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"ggraph_circlePackLayout", (DL_FUNC) &ggraph_circlePackLayout, 2},
    {"ggraph_cut_lines",        (DL_FUNC) &ggraph_cut_lines,        9},
    {"ggraph_pack",             (DL_FUNC) &ggraph_pack,             1},
    {"ggraph_partitionTree",    (DL_FUNC) &ggraph_partitionTree,    4},
    {"ggraph_pathAttr",         (DL_FUNC) &ggraph_pathAttr,         2},
    {"ggraph_splitTreemap",     (DL_FUNC) &ggraph_splitTreemap,     5},
    {NULL, NULL, 0}
};

void R_init_ggraph(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
