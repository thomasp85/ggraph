#include <Rcpp.h>

using namespace Rcpp;

//[[Rcpp::export]]
DataFrame pathAttr(DataFrame paths, int ngroups) {
    LogicalVector solid(ngroups, true);
    LogicalVector constant(ngroups, true);

    int currentGroup, currentIndex, i;
    IntegerVector group = paths["group"];
    NumericVector alpha = paths["edge_alpha"];
    NumericVector width = paths["edge_width"];
    IntegerVector lty = paths["edge_linetype"];
    CharacterVector colour = paths["edge_colour"];

    currentGroup = group[0];
    currentIndex = 0;

    for (i = 1; i < group.size(); ++i) {
        if (group[i] == currentGroup) {
            if (solid[currentIndex]) {
                solid[currentIndex] = lty[i] == 1 && lty[i] == lty[i-1];
            }
            if (constant[currentIndex]) {
                constant[currentIndex] = alpha[i] == alpha[i-1] &&
                    width[i] == width[i-1] &&
                    lty[i] == lty[i-1] &&
                    colour[i] == colour[i-1];
            }
        } else {
            currentGroup = group[i];
            ++currentIndex;
        }
    }

    return DataFrame::create(
        Named("solid") = solid,
        Named("constant") = constant
    );
}
