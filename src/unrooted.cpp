#include <Rcpp.h>
#include "nodes.h"
using namespace Rcpp;

const double PI2 = 6.28318;



void equalAngle(Node* node, double start, double angle) {
  if (node->leaf()) return;

  double x = node->bounds.x;
  double y = node->bounds.y;

  std::vector<Node*> children = node->getChildren();
  for (unsigned int i = 0; i < children.size(); ++i) {
    double span = children[i]->nLeafs() * angle;
    double mid = start + span / 2;
    children[i]->bounds.x = x + cos(mid) * children[i]->height();
    children[i]->bounds.y = y + sin(mid) * children[i]->height();
    equalAngle(children[i], start, angle);
    start += span;
  }
}
double equalDaylight(Node* node, double rotation_mod) {
  if (node->orphan() || node->getParent()->orphan()) return 0;
  if (node->nChildren() < 2) return 0;

  std::vector< std::vector<Node*> > leafs = node->getAllLeafs();
  std::vector<Node*> children = node->getChildren();

  std::vector<Node*> bounds_lower, bounds_upper;
  double x0 = node->bounds.x;
  double y0 = node->bounds.y;

  for (unsigned int i = 0; i < leafs.size(); ++i) {
    double x1, y1;
    if (i < children.size()) {
      x1 = children[i]->bounds.x - x0;
      y1 = children[i]->bounds.y - y0;
    } else {
      x1 = node->getParent()->bounds.x - x0;
      y1 = node->getParent()->bounds.y - y0;
    }
    int lower = 0;
    int upper = 0;
    double lower_angle = PI2;
    double upper_angle = -PI2;
    for (unsigned int j = 0; j < leafs[i].size(); ++j) {
      double x2 = leafs[i][j]->bounds.x - x0;
      double y2 = leafs[i][j]->bounds.y - y0;
      double dot = x1 * x2 + y1 * y2;
      double det = x1 * y2 - y1 * x2;
      double angle = atan2(det, dot);
      if (angle < lower_angle) {
        lower_angle = angle;
        lower = j;
      }
      if (angle > upper_angle) {
        upper_angle = angle;
        upper = j;
      }
    }
    bounds_lower.push_back(leafs[i][lower]);
    bounds_upper.push_back(leafs[i][upper]);
  }
  std::vector<double> daylight;
  double daylight_total = 0;
  for (unsigned int i = 0; i < leafs.size(); ++i) {
    int prev_i = i - 1;
    if (prev_i < 0) prev_i = leafs.size() - 1;
    double x1 = bounds_upper[prev_i]->bounds.x - x0;
    double y1 = bounds_upper[prev_i]->bounds.y - y0;
    double x2 = bounds_lower[i]->bounds.x - x0;
    double y2 = bounds_lower[i]->bounds.y - y0;
    double dot = x1 * x2 + y1 * y2;
    double det = x1 * y2 - y1 * x2;
    double d = atan2(det, dot);
    daylight.push_back(d);
    daylight_total += d;
  }
  double daylight_ave = daylight_total / leafs.size();
  double daylight_acc = 0;
  double daylight_change = 0;
  for (unsigned int i = 0; i < leafs.size() - 1; ++i) {
    daylight_acc = daylight_ave - daylight[i] + daylight_acc;
    children[i]->rotate(daylight_acc * rotation_mod, x0, y0);
    daylight_change += fabs(daylight_acc);
  }
  return daylight_change / leafs.size();
}

//[[Rcpp::export]]
NumericMatrix unrooted(IntegerVector parent, IntegerVector order, NumericVector length, bool daylight, double tol, double rotation_mod, int maxiter) {
  NumericMatrix rect(parent.size(), 2);

  std::vector<Node*> nodes = createUnrooted(as< std::vector<int> >(parent), as< std::vector<int> >(order), as< std::vector<double> >(length));
  Node* startNode = nodes[0]->getRoot();
  std::vector<Node*> children = startNode->getChildren();
  for (unsigned int i = 0; i < children.size(); ++i) {
    int n_leafs = children[i]->nLeafs();
    double split_angle = PI2 / n_leafs;
    children[i]->bounds.x = 0;
    children[i]->bounds.y = 0;
    equalAngle(children[i], 0, split_angle);
  }

  if (daylight) {
    double change = 1e6;
    double last_change = 0;
    int count = 0;
    do {
      for (unsigned int i = 0; i < nodes.size(); ++i) {
        rect(i, 0) = nodes[i]->bounds.x;
        rect(i, 1) = nodes[i]->bounds.y;
      }
      last_change = change;
      change = 0;
      for (unsigned int i = 0; i < nodes.size(); ++i) {
        double current_change = equalDaylight(nodes[i], rotation_mod);
        if (current_change > change) change = current_change;
      }
      count++;
    } while (change < last_change && count < maxiter && change > tol);
  } else {
    for (unsigned int i = 0; i < nodes.size(); ++i) {
      rect(i, 0) = nodes[i]->bounds.x;
      rect(i, 1) = nodes[i]->bounds.y;
    }
  }
  for (unsigned int i = 0; i < nodes.size(); ++i) {
    delete nodes[i];
  }
  return rect;
}
