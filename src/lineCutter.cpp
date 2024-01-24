#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/strings.hpp>
#include <cpp11/data_frame.hpp>

using namespace cpp11::literals;

double copy_sign(double from, double to) {
  if (from > 0) {
    return to > 0 ? to : -1 * to;
  } else if (from < 0) {
    return to < 0 ? to : -1 * to;
  }
  return to;
}

struct Point {
  double x;
  double y;
  bool REAL;
};
Point bad_point() {
  Point p = {0, 0, false};
  return p;
}
Point point(double x, double y) {
  Point p = {x, y, true};
  return p;
}
Point segment_intersect(Point p, Point p0, Point k, Point k0) {
  double pX = p0.x - p.x;
  double pY = p0.y - p.y;
  double kX = k0.x - k.x;
  double kY = k0.y - k.y;
  double delta = pX*kY - kX*pY;
  if (delta == 0) return bad_point();
  double s = (-pY * (p.x - k.x) + pX * (p.y - k.y)) / (-kX * pY + pX * kY);
  double t = (kX * (p.y - k.y) - kY * (p.x - k.x)) / (-kX * pY + pX * kY);
  if (s >= 0 && s <= 1 && t >= 0 && t <= 1) {
    return point(p.x + t * pX, p.y + t * pY);
  } else {
    return bad_point();
  }
}

Point rect_intersection(Point p, Point p0, double width, double height) {
  double xmin, xmax, ymin, ymax;
  xmin = p0.x - width;
  xmax = p0.x + width;
  ymin = p0.y - height;
  ymax = p0.y + height;

  if (p.x < xmin) {
    if (p.y > ymin && p.y < ymax) {
      return segment_intersect(p, p0, point(xmin, ymin), point(xmin, ymax));
    } else if (p.y < ymin) {
      Point p_tmp = segment_intersect(p, p0, point(xmin, ymin), point(xmax, ymin));
      if (p_tmp.REAL) {
        return p_tmp;
      } else {
        return segment_intersect(p, p0, point(xmin, ymin), point(xmin, ymax));
      }
    } else {
      Point p_tmp = segment_intersect(p, p0, point(xmin, ymax), point(xmax, ymax));
      if (p_tmp.REAL) {
        return p_tmp;
      } else {
        return segment_intersect(p, p0, point(xmin, ymin), point(xmin, ymax));
      }
    }
  } else if (p.x > xmax) {
    if (p.y > ymin && p.y < ymax) {
      return segment_intersect(p, p0, point(xmax, ymin), point(xmax, ymax));
    } else if (p.y < ymin) {
      Point p_tmp = segment_intersect(p, p0, point(xmin, ymin), point(xmax, ymin));
      if (p_tmp.REAL) {
        return p_tmp;
      } else {
        return segment_intersect(p, p0, point(xmax, ymin), point(xmax, ymax));
      }
    } else {
      Point p_tmp = segment_intersect(p, p0, point(xmin, ymax), point(xmax, ymax));
      if (p_tmp.REAL) {
        return p_tmp;
      } else {
        return segment_intersect(p, p0, point(xmax, ymin), point(xmax, ymax));
      }
    }
  } else {
    if (p.y < ymin) {
      return segment_intersect(p, p0, point(xmin, ymin), point(xmax, ymin));
    } else {
      return segment_intersect(p, p0, point(xmin, ymax), point(xmax, ymax));
    }
  }
}
Point ellipsis_intersection(Point p, Point p0, double width, double height) {
  double pX = p.x - p0.x;
  double pY = p.y - p0.y;
  double mod = (width * height) / std::sqrt(float(width*width*pY*pY + height*height*pX*pX));
  double x = mod * pX;
  double y = mod * pY;
  x = copy_sign(pX, x);
  y = copy_sign(pY, y);
  return point(x + p0.x, y + p0.y);
}
bool inside_ellipsis(Point p, Point p0, double width, double height) {
  double pX = p.x - p0.x;
  double pY = p.y - p0.y;
  return (pX * pX) / (width * width) + (pY * pY) / (height * height) < 1;
}
void capRectStart(cpp11::writable::doubles &x, cpp11::writable::doubles &y, int from, int to, double width, double height) {
  int i;
  Point p;
  Point p0 = point(x[from], y[from]);
  width /= 2;
  height /= 2;
  for (i = from; i < to; ++i) {
    p.x = x[i];
    p.y = y[i];
    if (std::abs(p.x - p0.x) <= width && std::abs(p.y - p0.y) <= height) {
      x[i] = NA_REAL;
      y[i] = NA_REAL;
    } else {
      Point intersect = rect_intersection(p, p0, width, height);
      if (intersect.REAL) {
        x[i-1] = intersect.x;
        y[i-1] = intersect.y;
      }
      break;
    }
  }
}
void capRectEnd(cpp11::writable::doubles &x, cpp11::writable::doubles &y, int from, int to, double width, double height) {
  int i = to - 1;
  Point p;
  Point p0 = point(x[i], y[i]);
  width /= 2;
  height /= 2;
  for (; i >= from; --i) {
    p.x = x[i];
    p.y = y[i];
    if (std::abs(p.x - p0.x) <= width && std::abs(p.y - p0.y) <= height) {
      x[i] = NA_REAL;
      y[i] = NA_REAL;
    } else {
      Point intersect = rect_intersection(p, p0, width, height);
      if (intersect.REAL) {
        x[i+1] = intersect.x;
        y[i+1] = intersect.y;
      }
      break;
    }
  }
}
void capEllipStart(cpp11::writable::doubles &x, cpp11::writable::doubles &y, int from, int to, double width, double height) {
  int i;
  Point p;
  Point p0 = point(x[from], y[from]);
  width /= 2;
  height /= 2;
  for (i = from; i < to; ++i) {
    p.x = x[i];
    p.y = y[i];
    if (inside_ellipsis(p, p0, width, height)) {
      x[i] = NA_REAL;
      y[i] = NA_REAL;
    } else {
      Point intersect = ellipsis_intersection(p, p0, width, height);
      if (intersect.REAL) {
        x[i-1] = intersect.x;
        y[i-1] = intersect.y;
      }
      break;
    }
  }
}
void capEllipEnd(cpp11::writable::doubles &x, cpp11::writable::doubles &y, int from, int to, double width, double height) {
  int i = to - 1;
  Point p;
  Point p0 = point(x[i], y[i]);
  width /= 2;
  height /= 2;
  for (; i >= from; --i) {
    p.x = x[i];
    p.y = y[i];
    if (inside_ellipsis(p, p0, width, height)) {
      x[i] = NA_REAL;
      y[i] = NA_REAL;
    } else {
      Point intersect = ellipsis_intersection(p, p0, width, height);
      if (intersect.REAL) {
        x[i+1] = intersect.x;
        y[i+1] = intersect.y;
      }
      break;
    }
  }
}

[[cpp11::register]]
cpp11::writable::data_frame cut_lines(cpp11::doubles x, cpp11::doubles y, cpp11::integers id, cpp11::doubles start_width, cpp11::doubles start_height, cpp11::doubles end_width, cpp11::doubles end_height, cpp11::strings start_type, cpp11::strings end_type) {
  cpp11::writable::doubles new_x(x);
  cpp11::writable::doubles new_y(y);
  int i, j, group, group_ind;
  group_ind = j = 0;
  group = id[group_ind];

  for (i = 0; i < id.size(); ++i) {
    if (group != id[i]) {
      if (start_width[group_ind] != 0 && start_height[group_ind] != 0) {
        if (start_type[group_ind] == "circle") {
          capEllipStart(new_x, new_y, j, i, start_width[group_ind], start_height[group_ind]);
        } else if (start_type[group_ind] == "rect") {
          capRectStart(new_x, new_y, j, i, start_width[group_ind], start_height[group_ind]);
        }
      }
      if (end_width[group_ind] != 0 && end_height[group_ind] != 0) {
        if (end_type[group_ind] == "circle") {
          capEllipEnd(new_x, new_y, j, i, end_width[group_ind], end_height[group_ind]);
        } else if (end_type[group_ind] == "rect") {
          capRectEnd(new_x, new_y, j, i, end_width[group_ind], end_height[group_ind]);
        }
      }
      group = id[i];
      ++group_ind;
      j = i;
    }
  }
  if (start_width[group_ind] != 0 && start_height[group_ind] != 0) {
    if (start_type[group_ind] == "circle") {
      capEllipStart(new_x, new_y, j, i, start_width[group_ind], start_height[group_ind]);
    } else if (start_type[group_ind] == "rect") {
      capRectStart(new_x, new_y, j, i, start_width[group_ind], start_height[group_ind]);
    }
  }
  if (end_width[group_ind] != 0 && end_height[group_ind] != 0) {
    if (end_type[group_ind] == "circle") {
      capEllipEnd(new_x, new_y, j, i, end_width[group_ind], end_height[group_ind]);
    } else if (end_type[group_ind] == "rect") {
      capRectEnd(new_x, new_y, j, i, end_width[group_ind], end_height[group_ind]);
    }
  }

  return cpp11::writable::data_frame({
    "x"_nm = x,
    "y"_nm = y
  });
}
