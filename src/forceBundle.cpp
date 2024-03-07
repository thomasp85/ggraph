// Re-implementation of https://github.com/upphiminn/d3.ForceBundle which
// implements Holten, Danny, and Jarke J. Van Wijk. "Force‐Directed Edge
// Bundling for Graph Visualization." Computer Graphics Forum (Blackwell
// Publishing Ltd) 28, no. 3 (2009): 983-990.

#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/matrix.hpp>
#include <cpp11/data_frame.hpp>
#include <cpp11/list_of.hpp>

#include <vector>

using namespace cpp11::literals;

class Point2 {
public:
  double x;
  double y;

  Point2() : x(0.0), y(0.0) {};
  Point2(double _x, double _y) : x(_x), y(_y) {};
  Point2(const Point2& p) : x(p.x), y(p.y) {};

  Point2 operator+(const Point2& p) const {
    return {x + p.x, y + p.y};
  };

  Point2& operator+=(const Point2& p) {
    x += p.x;
    y += p.y;
    return *this;
  };

  Point2 operator-(const Point2& p) const {
    return {x - p.x, y - p.y};
  };

  Point2& operator-=(const Point2& p) {
    x -= p.x;
    y -= p.y;
    return *this;
  };

  Point2 operator*(double fac) const {
    return {x * fac, y * fac};
  };

  Point2& operator*=(double fac) {
    x *= fac;
    y *= fac;
    return *this;
  };

  double operator*(const Point2& p) const {
    return dot(p);
  };

  Point2 operator/(double fac) const {
    return {x / fac, y / fac};
  };

  Point2& operator/=(double fac) {
    x /= fac;
    y /= fac;
    return *this;
  };

  bool operator==(const Point2& p) const {
    return x == p.x && y == p.y;
  };

  bool operator!=(const Point2& p) const {
    return x != p.x || y != p.y;
  };

  double distance_to(const Point2& p, double eps = 0) const {
    double dx = x - p.x;
    double dy = y - p.y;
    if (std::abs(dx) < eps && std::abs(dy) < eps) return eps;

    return std::sqrt(dx * dx + dy * dy);
  }

  double dot(const Point2& p) const {
    return x * p.x + y * p.y;
  }
};
class Segment {
public:
  Point2 a;
  Point2 b;

  Segment() : a(), b() {};
  Segment(Point2 _a, Point2 _b) : a(_a), b(_b) {};
  Segment(double x0, double y0, double x1, double y1) : a(x0, y0), b(x1, y1) {};
  Segment(const Segment& s) : a(s.a), b(s.b) {};

  double length(double eps = 0.0) const {
    return a.distance_to(b, eps);
  }

  Point2 as_vec() const {
    return b - a;
  }

  Point2 project(const Point2& p, double eps = 0.0) const {
    Point2 vec = as_vec();

    double l2 = vec * vec;
    double u = ((p.x - a.x) * vec.x + (p.y - a.y) * vec.y) / l2;

    return a + vec * u;
  }

  Segment project(const Segment& s, double eps = 0.0) const {
    return {project(s.a, eps), project(s.b, eps)};
  }

  Point2 midPoint2() const {
    return (a + b) / 2.0;
  }

  bool are_compatible(const Segment& Q, double compatibility_threshold, double eps = 0.0) const {
    return compatibility_score(Q, eps) >= compatibility_threshold;
  }

private:
  double visibility(const Segment& s, double eps = 0.0) const {
    Segment proj_s = project(s, eps);
    return std::max(1.0 - 2.0 * midPoint2().distance_to(proj_s.midPoint2(), eps) / proj_s.length(eps), 0.0);
  }
  double angle_comp(const Segment& Q, double eps = 0.0) const {
    double dot_PQ = as_vec() * Q.as_vec();
    double euc_PQ = length(eps) * Q.length(eps);
    return std::abs(dot_PQ / euc_PQ);
  }
  double scale_comp(const Segment& Q, double eps = 0.0) const {
    double euc_P = length(eps);
    double euc_Q = Q.length(eps);

    double lavg = (euc_P + euc_Q) / 2.0;
    return 2.0 / (lavg / std::min(euc_P, euc_Q) + std::max(euc_P, euc_Q) / lavg);
  }
  double position_comp(const Segment& Q, double eps = 0.0) const {
    double euc_P = length(eps);
    double euc_Q = Q.length(eps);

    double lavg = (euc_P + euc_Q) / 2.0;

    double euc_mid = midPoint2().distance_to(Q.midPoint2(), eps);
    return lavg / (lavg + euc_mid);
  }
  double visibility_comp(const Segment& Q, double eps = 0.0) const {
    return std::min(visibility(Q, eps), Q.visibility(*this, eps));
  }
  double compatibility_score(const Segment& Q, double eps = 0.0) const {
    return angle_comp(Q, eps) * scale_comp(Q, eps) * position_comp(Q, eps) * visibility_comp(Q, eps);
  }
};

typedef std::vector<Point2> Path;


double compute_divided_edge_length(Path& edge) {
  int segments = edge.size() - 1;
  double length = 0.0;
  for (int i = 0; i < segments; ++i) {
    length += edge[i].distance_to(edge[i + 1]);
  }
  return length;
}

void update_edge_divisions(std::vector<Path>& elist, int P) {
  for (size_t e_idx = 0; e_idx < elist.size(); ++e_idx) {
    if (P == 1) {
      elist[e_idx].insert(elist[e_idx].begin() + 1, Segment(elist[e_idx][0], elist[e_idx][1]).midPoint2());
    } else {
      Path edge = elist[e_idx];
      double current_edge_length = compute_divided_edge_length(edge);
      double new_segment_length = current_edge_length / (P + 1);
      double current_segment_length = new_segment_length;
      elist[e_idx].clear();
      elist[e_idx].reserve(P + 2);
      elist[e_idx].push_back(edge[0]);
      for (size_t i = 1; i < edge.size(); ++i) {
        Point2& start_Point2 = edge[i - 1];
        Point2 vec = edge[i] - start_Point2;
        double segment_length = start_Point2.distance_to(edge[i]);
        while (segment_length > current_segment_length) {
          double percent_position = current_segment_length / segment_length;

          elist[e_idx].push_back(start_Point2 + vec * percent_position);

          current_segment_length += new_segment_length;
        }
        current_segment_length -= segment_length;
      }
      while (elist[e_idx].size() > size_t(P + 1)) {
        elist[e_idx].pop_back();
      }
      elist[e_idx].push_back(edge.back());
    }
  }
}

std::vector< std::vector<int> > compute_compatibility_lists(std::vector<Path>& edges, double compatibility_threshold) {
  size_t m = edges.size();
  std::vector< std::vector<int> > comp(m);
  for (size_t e = 0; e < (m - 1); ++e) {
    Segment P(edges[e][0], edges[e][1]);
    for (size_t oe = (e + 1); oe < m; ++oe) {
      Segment Q(edges[oe][0], edges[oe][1]);
      if (P.are_compatible(Q, compatibility_threshold)) {
        comp[e].push_back(oe);
        comp[oe].push_back(e);
      }
    }
  }
  return comp;
}

Point2 apply_spring_force(const std::vector<Path>& edges, int e_idx, int i, double kP) {
  const Path& edge = edges[e_idx];

  return (edge[i - 1] + edge[i + 1] - edge[i] * 2) * kP;
}

Point2 apply_electrostatic_force(const std::vector<Path>& edges,
                                const std::vector< std::vector<int> >& comp,
                                int e_idx, int i, double eps) {
  Point2 sum_of_forces;
  if (comp[e_idx].empty()) {
    return sum_of_forces;
  }
  const std::vector<int>& compatible_edges = comp[e_idx];
  const Path& edge = edges[e_idx];

  for (size_t oe = 0; oe < compatible_edges.size(); ++oe) {
    const Path& edge2 = edges[compatible_edges[oe]];
    Point2 force = edge2[i] - edge[i];
    if ((std::abs(force.x) > eps) || (std::abs(force.y) > eps)) {
      double euc = edge2[i].distance_to(edge[i]);
      double diff = std::pow(euc, -1.0);

      sum_of_forces += force * diff;
    }
  }
  return sum_of_forces;
}

std::vector<Point2> apply_resulting_forces_on_subdivision_Point2s(const std::vector<Path>& edges,
                                                                const std::vector< std::vector<int> >& comp,
                                                                int e_idx, int P,
                                                                double S, double K,
                                                                double eps) {
  const Path& edge = edges[e_idx];

  double kP = K / (edge[0].distance_to(edge[P + 1], eps) * (P + 1));

  std::vector<Point2> resulting_forces_for_subdivision_Point2s(P + 2);
  for (int i = 1; i < (P + 1); ++i) {
    Point2 spring_force = apply_spring_force(edges, e_idx, i, kP);

    Point2 electrostatic_force = apply_electrostatic_force(edges, comp, e_idx, i, eps);

    resulting_forces_for_subdivision_Point2s[i] = (spring_force + electrostatic_force) * S;
  }

  return resulting_forces_for_subdivision_Point2s;
}


[[cpp11::register]]
cpp11::writable::data_frame force_bundle_iter(cpp11::doubles_matrix<> edges_xy, double K, int C,
                       int P, int P_rate, double S, int I, double I_rate,
                       double compatibility_threshold, double eps) {
  R_xlen_t m = edges_xy.nrow();
  // first division
  std::vector<Path> edges(m);
  for (R_xlen_t i = 0; i < m; ++i) {
    edges[i].push_back(Point2(edges_xy(i, 0), edges_xy(i, 1)));
    edges[i].push_back(Point2(edges_xy(i, 2), edges_xy(i, 3)));
  }

  // compute compatibility list
  auto comp = compute_compatibility_lists(edges, compatibility_threshold);

  update_edge_divisions(edges, P);

  // main loop
  for (int cycle = 0; cycle < C; ++cycle) {
    for (int iteration = 0; iteration < I; ++iteration) {
      std::vector< std::vector<Point2> > forces(m);
      for (R_xlen_t e = 0; e < m; ++e) {
        forces[e] = apply_resulting_forces_on_subdivision_Point2s(edges, comp, e, P, S, K, eps);
      }
      for (int e = 0; e < m; ++e) {
        for (int i = 0; i < (P + 1); ++i) {
          edges[e][i] += forces[e][i];
        }
      }
    }
    if (cycle != (C - 1)) {
      S = S / 2.0;
      P = P * P_rate;
      I = I * I_rate;
      update_edge_divisions(edges, P);
    }
  }

  cpp11::writable::doubles x;
  x.reserve(m * edges[0].size());
  cpp11::writable::doubles y;
  y.reserve(m * edges[0].size());
  cpp11::writable::integers id;
  id.reserve(m * edges[0].size());
  for (size_t i = 0; i < edges.size(); ++i) {
    for (size_t j = 0; j < edges[i].size(); ++j) {
      x.push_back(edges[i][j].x);
      y.push_back(edges[i][j].y);
      id.push_back(i + 1);
    }
  }
  return cpp11::writable::data_frame({
    "x"_nm = x,
    "y"_nm = y,
    "group"_nm = id
  });
}
