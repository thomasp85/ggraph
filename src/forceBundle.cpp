// Re-implementation of https://github.com/upphiminn/d3.ForceBundle which
// implements Holten, Danny, and Jarke J. Van Wijk. "Force‐Directed Edge
// Bundling for Graph Visualization." Computer Graphics Forum (Blackwell
// Publishing Ltd) 28, no. 3 (2009): 983-990.
#include <Rcpp.h>
using namespace Rcpp;

double euclidean_distance(NumericVector P, NumericVector Q) {
  return sqrt((P[0] - Q[0]) * (P[0] - Q[0]) + (P[1] - Q[1]) * (P[1] - Q[1]));
}

double edge_length(NumericVector P, NumericVector Q, double eps) {
  if ((std::abs(P[0] - Q[0]) < eps) & (std::abs(P[1] - Q[1]) < eps)) {
    return eps;
  } else {
    return euclidean_distance(P, Q);
  }
}

double vector_dot_product(NumericVector P, NumericVector Q) {
  return P[0] * Q[0] + P[1] * Q[1];
}

NumericVector edge_as_vector(NumericVector P) {
  NumericVector vec = {P[2] - P[0], P[3] - P[1]};
  return vec;
}

NumericVector project_point_on_line(NumericVector p, NumericVector Q) {
  double Q_target_x = Q[2];
  double Q_source_x = Q[0];
  double Q_target_y = Q[3];
  double Q_source_y = Q[1];
  double p_x = p[0];
  double p_y = p[1];

  double L = sqrt((Q_target_x - Q_source_x) * (Q_target_x - Q_source_x) +
                  (Q_target_y - Q_source_y) * (Q_target_y - Q_source_y));
  double r = ((Q_source_y - p_y) * (Q_source_y - Q_target_y) -
              (Q_source_x - p_x) * (Q_target_x - Q_source_x)) /
             (L * L);

  NumericVector vec = {(Q_source_x + r * (Q_target_x - Q_source_x)),
                       (Q_source_y + r * (Q_target_y - Q_source_y))};

  return vec;
}

double edge_visibility(NumericVector P, NumericVector Q) {
  NumericVector qs = {Q[0], Q[1]};
  NumericVector qt = {Q[2], Q[3]};

  NumericVector I0 = project_point_on_line(qs, P);
  NumericVector I1 = project_point_on_line(qt, P);

  NumericVector midI = {(I0[0] + I1[0]) / 2.0, (I0[1] + I1[1]) / 2.0};
  NumericVector midP = {(P[0] + P[2]) / 2.0, (P[1] + P[3]) / 2.0};

  double tmp =
      1.0 - 2.0 * euclidean_distance(midP, midI) / euclidean_distance(I0, I1);
  if (tmp > 0) {
    return tmp;
  } else {
    return 0;
  }
}

double compute_divided_edge_length(NumericMatrix emat) {
  int segments = emat.rows() - 1;
  double length = 0.0;
  for (int i = 0; i < segments; ++i) {
    double segment_length = euclidean_distance(emat(i, _), emat(i + 1, _));
    length += segment_length;
  }
  return length;
}

List update_edge_divisions(List elist, int P) {
  for (int e_idx = 0; e_idx < elist.length(); ++e_idx) {
    NumericMatrix emat = elist[e_idx];
    if (P == 1) {
      NumericMatrix emat_new(3, 2);
      emat_new(0, 0) = emat(0, 0);
      emat_new(0, 1) = emat(0, 1);
      emat_new(1, 0) = (emat(0, 0) + emat(1, 0)) / 2.0;
      emat_new(1, 1) = (emat(0, 1) + emat(1, 1)) / 2.0;
      emat_new(2, 0) = emat(1, 0);
      emat_new(2, 1) = emat(1, 1);
      elist[e_idx] = emat_new;
    } else {
      double divided_edge_length = compute_divided_edge_length(emat);
      double segment_length = divided_edge_length / (P + 1);
      double current_segment_length = segment_length;
      NumericMatrix emat_new(P + 2, 2);
      emat_new(0, _) = emat(0, _);
      emat_new((emat_new.rows() - 1), _) = emat((emat.rows() - 1), _);
      int cur = 1;
      for (int i = 1; i < emat.rows(); ++i) {
        double old_segment_length =
            euclidean_distance(emat(i - 1, _), emat(i, _));
        while (old_segment_length > current_segment_length) {
          double percent_position = current_segment_length / old_segment_length;
          double new_subdivision_point_x = emat(i - 1, 0);
          double new_subdivision_point_y = emat(i - 1, 1);

          new_subdivision_point_x +=
              percent_position * (emat(i, 0) - emat(i - 1, 0));
          new_subdivision_point_y +=
              percent_position * (emat(i, 1) - emat(i - 1, 1));

          emat_new(cur, 0) = new_subdivision_point_x;
          emat_new(cur, 1) = new_subdivision_point_y;
          cur += 1;
          old_segment_length = old_segment_length - current_segment_length;
          current_segment_length = segment_length;
        }
        current_segment_length -= old_segment_length;
      }
      elist[e_idx] = emat_new;
    }
  }

  return elist;
}

double angle_compatibility(NumericVector P, NumericVector Q) {
  NumericVector P_source = {P[0], P[1]};
  NumericVector P_target = {P[2], P[3]};
  NumericVector Q_source = {Q[0], Q[1]};
  NumericVector Q_target = {Q[2], Q[3]};

  double dot_PQ = vector_dot_product(edge_as_vector(P), edge_as_vector(Q));
  double euc_PQ = euclidean_distance(P_source, P_target) *
                  euclidean_distance(Q_source, Q_target);
  double frac_PQ = dot_PQ / euc_PQ;
  return std::abs(frac_PQ);
}

double scale_compatibility(NumericVector P, NumericVector Q) {
  NumericVector P_source = {P[0], P[1]};
  NumericVector P_target = {P[2], P[3]};
  NumericVector Q_source = {Q[0], Q[1]};
  NumericVector Q_target = {Q[2], Q[3]};

  double euc_P = euclidean_distance(P_source, P_target);
  double euc_Q = euclidean_distance(Q_source, Q_target);

  double lavg = (euc_P + euc_Q) / 2.0;
  return 2.0 / (lavg / std::min(euc_P, euc_Q) + std::max(euc_P, euc_Q) / lavg);
}

double position_compatibility(NumericVector P, NumericVector Q) {
  NumericVector P_source = {P[0], P[1]};
  NumericVector P_target = {P[2], P[3]};
  NumericVector Q_source = {Q[0], Q[1]};
  NumericVector Q_target = {Q[2], Q[3]};

  double euc_P = euclidean_distance(P_source, P_target);
  double euc_Q = euclidean_distance(Q_source, Q_target);

  double lavg = (euc_P + euc_Q) / 2.0;

  NumericVector midP = {(P_source[0] + P_target[0]) / 2.0,
                        (P_source[1] + P_target[1]) / 2.0};

  NumericVector midQ = {(Q_source[0] + Q_target[0]) / 2.0,
                        (Q_source[1] + Q_target[1]) / 2.0};

  double euc_mid = euclidean_distance(midP, midQ);
  return lavg / (lavg + euc_mid);
}

double visibility_compatibility(NumericVector P, NumericVector Q) {
  return std::min(edge_visibility(P, Q), edge_visibility(Q, P));
}

double compatibility_score(NumericVector P, NumericVector Q) {
  return angle_compatibility(P, Q) * scale_compatibility(P, Q) *
         position_compatibility(P, Q) * visibility_compatibility(P, Q);
}

bool are_compatible(NumericVector P, NumericVector Q,
                    double compatibility_threshold) {
  return compatibility_score(P, Q) >= compatibility_threshold;
}

List compute_compatibility_lists(NumericMatrix edges_xy,
                                 double compatibility_threshold) {
  int m = edges_xy.rows();
  List elist_comp(m);
  for (int e = 0; e < (m - 1); ++e) {
    NumericVector P = edges_xy(e, _);
    for (int oe = (e + 1); oe < m; ++oe) {
      NumericVector Q = edges_xy(oe, _);
      if (are_compatible(P, Q, compatibility_threshold)) {
        if (elist_comp[e] == R_NilValue) {
          IntegerVector ecomp = {oe};
          elist_comp[e] = ecomp;
        } else {
          IntegerVector ecomp = elist_comp[e];
          ecomp.push_back(oe);
          elist_comp[e] = ecomp;
        }
        if (elist_comp[oe] == R_NilValue) {
          IntegerVector oecomp = {e};
          elist_comp[oe] = oecomp;
        } else {
          IntegerVector oecomp = elist_comp[oe];
          oecomp.push_back(e);
          elist_comp[oe] = oecomp;
        }
      }
    }
  }
  return elist_comp;
}

NumericVector apply_spring_force(List elist, int e_idx, int i, double kP) {
  NumericMatrix emat = elist[e_idx];
  NumericVector prec = emat(i - 1, _);
  NumericVector succ = emat(i + 1, _);
  NumericVector crnt = emat(i, _);

  double x = prec[0] - crnt[0] + succ[0] - crnt[0];
  double y = prec[1] - crnt[1] + succ[1] - crnt[1];

  x *= kP;
  y *= kP;

  return {x, y};
}

NumericVector apply_electrostatic_force(List elist, List elist_comp, int e_idx,
                                        int i, double eps) {
  NumericVector sum_of_forces(2);
  if (elist_comp[e_idx] == R_NilValue) {
    return sum_of_forces;
  }
  IntegerVector ecomps = elist_comp[e_idx];
  NumericMatrix emat = elist[e_idx];

  for (int oe = 0; oe < ecomps.length(); ++oe) {
    NumericMatrix oemat = elist[ecomps[oe]];
    NumericVector force = {oemat(i, 0) - emat(i, 0), oemat(i, 1) - emat(i, 1)};
    if ((std::abs(force[0]) > eps) | (std::abs(force[1]) > eps)) {
      double euc = euclidean_distance(oemat(i, _), emat(i, _));
      double diff = std::pow(euc, -1.0);

      sum_of_forces[0] += force[0] * diff;
      sum_of_forces[1] += force[1] * diff;
    }
  }
  return sum_of_forces;
}

NumericMatrix apply_resulting_forces_on_subdivision_points(List elist,
                                                           List elist_comp,
                                                           int e_idx, int P,
                                                           double S, double K,
                                                           double eps) {
  NumericMatrix emat = elist[e_idx];

  double kP = K / (edge_length(emat(0, _), emat(P + 1, _), eps) * (P + 1));

  NumericMatrix resulting_forces_for_subdivision_points(P + 2, 2);
  for (int i = 1; i < (P + 1); ++i) {
    NumericMatrix resulting_force(2);
    NumericVector spring_force = apply_spring_force(elist, e_idx, i, kP);

    NumericVector electrostatic_force =
        apply_electrostatic_force(elist, elist_comp, e_idx, i, eps);

    resulting_force[0] = S * (spring_force[0] + electrostatic_force[0]);
    resulting_force[1] = S * (spring_force[1] + electrostatic_force[1]);

    resulting_forces_for_subdivision_points(i, _) = resulting_force;
  }

  return resulting_forces_for_subdivision_points;
}

// [[Rcpp::export]]
List force_bundle_iter(NumericMatrix edges_xy, List elist, double K, int C,
                       int P, int P_rate, double S, int I, double I_rate,
                       double compatibility_threshold, double eps) {
  int m = edges_xy.rows();
  // first division

  elist = update_edge_divisions(elist, P);

  // compute compatibility list
  List elist_comp =
      compute_compatibility_lists(edges_xy, compatibility_threshold);

  // main loop
  for (int cycle = 0; cycle < C; ++cycle) {
    for (int iteration = 0; iteration < I; ++iteration) {
      List forces(m);
      for (int e = 0; e < m; ++e) {
        forces[e] = apply_resulting_forces_on_subdivision_points(
            elist, elist_comp, e, P, S, K, eps);
      }
      for (int e = 0; e < m; ++e) {
        NumericMatrix emat = elist[e];
        NumericMatrix fmat = forces[e];
        for (int i = 0; i < (P + 1); ++i) {
          emat(i, 0) += fmat(i, 0);
          emat(i, 1) += fmat(i, 1);
        }
        elist[e] = emat;
      }
    }
    if (cycle != (C - 1)) {
      S = S / 2.0;
      P = P * P_rate;
      I = I * I_rate;
      elist = update_edge_divisions(elist, P);
    }
  }
  return elist;
}
