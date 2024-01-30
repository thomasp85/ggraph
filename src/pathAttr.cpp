#include <cpp11/data_frame.hpp>
#include <cpp11/logicals.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/doubles.hpp>
#include <cpp11/strings.hpp>
#include <cpp11/r_bool.hpp>

using namespace cpp11::literals;

[[cpp11::register]]
cpp11::writable::data_frame pathAttr(cpp11::integers group, cpp11::doubles alpha,
                                     cpp11::doubles width, cpp11::strings lty,
                                     cpp11::strings colour, int ngroups) {
  cpp11::writable::logicals solid(ngroups);
  std::fill(solid.begin(), solid.end(), true);
  cpp11::writable::logicals constant(ngroups);
  std::fill(constant.begin(), constant.end(), true);

  int currentGroup, currentIndex, i;

  currentGroup = group[0];
  currentIndex = 0;

  for (i = 1; i < group.size(); ++i) {
    if (group[i] == currentGroup) {
      if (solid[currentIndex] == TRUE) {
        solid[currentIndex] = lty[i] == "solid" && lty[i] == lty[i-1];
      }
      if (constant[currentIndex] == TRUE) {
        constant[currentIndex] =
          ((alpha[i] == alpha[i-1]) || (cpp11::is_na(alpha[i]) && cpp11::is_na(alpha[i-1]))) &&
          ((width[i] == width[i-1]) || (cpp11::is_na(width[i]) && cpp11::is_na(width[i-1]))) &&
          ((lty[i] == lty[i-1]) || (cpp11::is_na(lty[i]) && cpp11::is_na(lty[i-1]))) &&
          ((colour[i] == colour[i-1]) || (cpp11::is_na(colour[i]) && cpp11::is_na(colour[i-1])));
      }
    } else {
      currentGroup = group[i];
      ++currentIndex;
    }
  }

  return cpp11::writable::data_frame({
    "solid"_nm = solid,
    "constant"_nm = constant
  });
}
