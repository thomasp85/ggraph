#include <Rcpp.h>
#include <algorithm>

using namespace Rcpp;

inline int randWrapper(const int n) {
  return std::floor(float(unif_rand()*n));
}
struct Circle {
  double x;
  double y;
  double r;
  int i;
  Circle* next;
  Circle* prev;
};
class FrontChain {
  double center_x;
  double center_y;
  double total_weight;
  Circle enclosure;
  Circle* next_circle;

  void place(Circle* c, Circle* c1, Circle* c2) {
    double r0 = c1->r + c->r;
    double r1 = c2->r + c->r;
    double dx = c2->x - c1->x;
    double dy = c2->y - c1->y;
    double d = std::sqrt(float(dx*dx + dy*dy));
    double a = (r0*r0 - r1*r1 + d*d) / (2*d);
    double h = std::sqrt(float(r0*r0 - a*a));
    c->x = c1->x + a*(c2->x - c1->x)/d - h*(c2->y - c1->y)/d;
    c->y = c1->y + a*(c2->y - c1->y)/d + h*(c2->x - c1->x)/d;
  };
  double dist_to_center(Circle* c) {
    double dx = c->x - center_x / total_weight;
    double dy = c->y - center_y / total_weight;
    return c->r + std::sqrt(float(dx*dx + dy*dy));
  }
  bool circles_intersect(Circle* c1, Circle* c2) {
    double dx = c2->x - c1->x;
    double dy = c2->y - c1->y;
    double dr = c2->r + c1-> r;
    return dr*dr > dx*dx + dy*dy;
  };
  void update_closest_circle() {
    Circle* circle_it = next_circle;
    Circle* current_close = circle_it;
    double dist = dist_to_center(circle_it);
    circle_it = circle_it->next;
    while(circle_it != next_circle) {
      double temp_dist = dist_to_center(circle_it);
      if (temp_dist < dist) {
        dist = temp_dist;
        current_close = circle_it;
      }
      circle_it = circle_it->next;
    }
    next_circle = current_close;
  };
  void update_chain(Circle* from, Circle* to) {
    from->next = to;
    to->prev = from;
  };
  bool encloses(Circle enc, Circle* c) {
    double dx = c->x - enc.x;
    double dy = c->y - enc.y;
    double dr = c->r - enc.r;
    return dr * dr + 1e-6 > dx * dx + dy * dy;
  };
  Circle enclose1(Circle* c1) {
    Circle enc = {c1->x, c1->y, c1->r};
    if (enc.r > 1e10) {
      stop("enc1 error");
    }
    return enc;
  };
  Circle enclose2(Circle* c1, Circle* c2) {
    double dx = c2->x - c1->x;
    double dy = c2->y - c1->y;
    double dr = c2->r - c1->r;
    double l = std::sqrt(float(dx*dx + dy*dy));
    Circle enc = {
      (c1->x + c2->x + dx / l * dr) / 2,
      (c1->y + c2->y + dy / l * dr) / 2,
      (l + c1->r + c2->r) / 2
    };
    if (enc.r > 1e10) {
      stop("enc2 error");
    }
    return enc;
  };
  Circle enclose3(Circle* c1, Circle* c2, Circle* c3) {
    double A2 = 2 * (c1->x - c2->x);
    double B2 = 2 * (c1->y - c2->y);
    double C2 = 2 * (c2->r - c1->r);
    double D2 = c1->x * c1->x + c1->y * c1->y - c1->r * c1->r - c2->x * c2->x - c2->y * c2->y + c2->r * c2->r;
    double A3 = 2 * (c1->x - c3->x);
    double B3 = 2 * (c1->y - c3->y);
    double C3 = 2 * (c3->r - c1->r);
    double D3 = c1->x * c1->x + c1->y * c1->y - c1->r * c1->r - c3->x * c3->x - c3->y * c3->y + c3->r * c3->r;
    double AB = A3 * B2 - A2 * B3;
    double XA = (B2 * D3 - B3 * D2) / AB - c1->x;
    double XB = (B3 * C2 - B2 * C3) / AB;
    double YA = (A3 * D2 - A2 * D3) / AB - c1->y;
    double YB = (A2 * C3 - A3 * C2) / AB;
    double A = XB * XB + YB * YB - 1;
    double B = 2 * (XA * XB + YA * YB + c1->r);
    double C = XA * XA + YA * YA - c1->r * c1->r;
    double r = (-B - std::sqrt(float(B * B - 4 * A * C))) / (2 * A);
    Circle enc = {
      XA + XB * r + c1->x,
      YA + YB * r + c1->y,
      r
    };
    if (enc.r > 1e10) {
      stop("enc3 error");
    }
    return enc;
  };
  Circle encloseN(std::deque<Circle*>::iterator begin_S, std::deque<Circle*>::iterator end_S, std::deque<Circle*> Q) {
    Circle enc = {0, 0, 0};
    bool has_enc = false;
    switch (Q.size()) {
    case 1: enc = enclose1(Q[0]);
      has_enc = true;
      break;
    case 2: enc = enclose2(Q[0], Q[1]);
      has_enc = true;
      break;
    case 3: enc = enclose3(Q[0], Q[1], Q[2]);
      has_enc = true;
      break;
    }
    std::deque<Circle*>::iterator S_iter = begin_S;
    while (S_iter != end_S) {
      if (!has_enc || !encloses(enc, *S_iter)) {
        Q.push_back(*S_iter);
        enc = encloseN(begin_S, S_iter, Q);
        has_enc = true;
        Q.pop_back();
      }
      S_iter++;
    }
    return enc;
  };
public:
  FrontChain(Circle* c1, Circle* c2, Circle* c3) {
    // Set location of first circle to coord center and second at random angle
    c1->x = 0;
    c1->y = 0;
    double d12 = c1->r + c2->r;
    double angle2 = (2 * M_PI) * R::runif(0, 1);
    c2->x = std::cos(float(angle2)) * d12;
    c2->y = std::sin(float(angle2)) * d12;

    // Place third circle according to the first two
    place(c3, c2, c1);

    // Set weighted center
    double a1 = c1->r*c1->r;
    double a2 = c2->r*c2->r;
    double a3 = c3->r*c3->r;
    total_weight = a1 + a2 + a3;
    center_x = a1*c1->x + a2*c2->x + a3*c3->x;
    center_y = a1*c1->y + a2*c2->y + a3*c3->y;

    // Inititalize the front chain
    c1->next = c2;
    c1->prev = c3;
    c2->next = c3;
    c2->prev = c1;
    c3->next = c1;
    c3->prev = c2;
    next_circle = c1;
  };
  FrontChain(Circle* c1, Circle* c2) {
    // Set location of first circle to coord center and second at random angle
    c1->x = 0;
    c1->y = 0;
    double d12 = c1->r + c2->r;
    double angle2 = (2 * M_PI) * R::runif(0, 1);
    c2->x = std::cos(float(angle2)) * d12;
    c2->y = std::sin(float(angle2)) * d12;

    // Inititalize the front chain
    c1->next = c2;
    c1->prev = c2;
    c2->next = c1;
    c2->prev = c1;
    next_circle = c1;
  };
  FrontChain(Circle* c1) {
    // Set location of first circle to coord center and second at random angle
    c1->x = 0;
    c1->y = 0;

    // Inititalize the front chain
    c1->next = c1;
    c1->prev = c1;
    next_circle = c1;
  };
  void add(Circle* c) {
    Circle* n = next_circle;
    Circle* m = next_circle->next;
    place(c, n, m);
    bool at_m = false;
    bool at_n = false;
    while(true) {
      n = n->prev;
      if (n == m) break;
      if (circles_intersect(c, n)) {
        at_n = true;
        break;
      }
      m = m->next;
      if (n == m) break;
      if (circles_intersect(c, m)) {
        at_m = true;
        break;
      }
    }
    if (at_m) {
      update_chain(next_circle, m);
      add(c);
    } else if (at_n) {
      update_chain(n, next_circle->next);
      next_circle = n;
      add(c);
    } else {
      c->next = next_circle->next;
      c->prev = next_circle;
      next_circle->next->prev = c;
      next_circle->next = c;
      double a = c->r*c->r;
      total_weight += a;
      center_x += a*c->x;
      center_y += a*c->y;
      update_closest_circle();
    }
  };
  void center(std::deque<Circle>::iterator first, std::deque<Circle>::iterator last) {
    while (first != last) {
      first->x -= enclosure.x;
      first->y -= enclosure.y;
      first++;
    }
    enclosure.x = 0;
    enclosure.y = 0;
  };
  double enclose_radius() {
    return enclosure.r;
  };
  std::deque<int> chain_ind() {
    std::deque<int> index;
    Circle* circle_it = next_circle;
    index.push_back(circle_it->i);
    circle_it = circle_it->next;
    while(circle_it != next_circle) {
      index.push_back(circle_it->i);
      circle_it = circle_it->next;
    }
    return index;
  };
  void enclose() {
    std::deque<Circle*> fc;
    Circle* circle_it = next_circle;
    fc.push_back(circle_it);
    circle_it = circle_it->next;
    while(circle_it != next_circle) {
      fc.push_back(circle_it);
      circle_it = circle_it->next;
    }
    if (fc.size() == 1) {
      enclosure = enclose1(fc[0]);
    } else if (fc.size() == 2) {
      enclosure = enclose2(fc[0], fc[1]);
    } else {
      std::random_shuffle(fc.begin(), fc.end(), randWrapper);
      std::deque<Circle*> Q;
      enclosure = encloseN(fc.begin(), fc.end(), Q);
    }
  }
};

FrontChain pack_circles(std::deque<Circle> &circles) {
  if (circles.size() == 0) {
    stop("Cannot pack an empty set of circles");
  }

  std::deque<Circle>::iterator itc;
  FrontChain fc(&circles[0]);

  if (circles.size() == 2) {
    fc = FrontChain(&circles[0], &circles[1]);
  } else if (circles.size() > 2){
    fc = FrontChain(&circles[0], &circles[1], &circles[2]);

    for (itc = circles.begin() + 3; itc != circles.end(); itc++) {
      fc.add(&(*itc));
    }
  }
  fc.enclose();
  fc.center(circles.begin(), circles.end());

  return fc;
}
class NodePack {
  std::vector<NodePack*> children;
  NodePack* parent;
  bool hasParent;
public:
  int Id;
  double x;
  double y;
  double r;

  NodePack() {
    hasParent = false;
  };
  NodePack(int id, double weight) {
    hasParent = false;
    Id = id;
    x = 0.0;
    y = 0.0;
    r = std::sqrt(float( weight / M_PI));
  };
  int nChildren() {
    return children.size();
  };
  void addNode(NodePack* n) {
    n->setParent(this);
    children.push_back(n);
  };
  void setParent(NodePack* n) {
    hasParent = true;
    parent = n;
  };
  bool orphan() {
    return !hasParent;
  };
  void packChildren() {
    if (nChildren() != 0) {
      std::deque<Circle> circles;
      int i;
      double max_r = 0;
      int max_r_i = 0;
      for (i = 0; i != nChildren(); i++) {
        children[i]->packChildren();
        if (children[i]->r > max_r) {
          max_r = children[i]->r;
          max_r_i = i;
        }
        Circle circ = {0, 0, children[i]->r, children[i]->Id};
        circles.push_back(circ);
      }
      std::swap(children[0], children[max_r_i]);
      std::swap(circles[0], circles[max_r_i]);
      FrontChain fc = pack_circles(circles);
      for (i = 0; i != nChildren(); i++) {
        children[i]->x = circles[i].x;
        children[i]->y = circles[i].y;
      }
      r = fc.enclose_radius();
    }
  };
  void placeChildren(double center_x, double center_y) {
    x += center_x;
    y += center_y;
    if (nChildren() != 0) {
      int i;
      for (i = 0; i != nChildren(); i++) {
        children[i]->placeChildren(x, y);
      }
    }
  }
};
std::vector<NodePack*> createHierarchy(std::vector<int> parent, std::vector<double> weight) {
  std::vector<NodePack*> nodes;
  unsigned int i;
  for (i = 0; i < parent.size(); ++i) {
    NodePack* node = new NodePack(i + 1, weight[i]);
    nodes.push_back(node);
  }
  for (i = 0; i < parent.size(); ++i) {
    if (parent[i] >= 0) {
      nodes[parent[i]]->addNode(nodes[i]);
    }
  }
  return nodes;
}
int findTopNode(std::vector<NodePack*>& nodes) {
  unsigned int i;
  bool found = false;
  for (i = 0; i < nodes.size(); ++i) {
    if (nodes[i]->orphan()) {
      found = true;
      break;
    }
  }
  if (!found) {
    stop("No top node. Is this a tree structure?");
  }
  return (int) i;
}
//' Pack circles together
//'
//' This function is a direct interface to the circle packing algorithm used by
//' \code{\link{layout_tbl_graph_circlepack}}. It takes a vector of sizes and
//' returns the x and y position of each circle as a two-column matrix.
//'
//' @param areas A vector of circle areas
//'
//' @return A matrix with two columns and the same number of rows as the length
//' of the "areas" vector. The matrix has the following attributes added:
//' "enclosing_radius" giving the radius of the smallest enclosing circle, and
//' "front_chain" giving the terminating members of the front chain (see
//' Wang \emph{et al}. 2006).
//'
//' @references
//' Wang, W., Wang, H. H., Dai, G., & Wang, H. (2006). \emph{Visualization of
//' large hierarchical data by circle packing}. Chi, 517-520.
//'
//' @export
//'
//' @examples
//' library(ggforce)
//' sizes <- sample(10, 100, TRUE)
//'
//' position <- pack_circles(sizes)
//' data <- data.frame(x = position[,1], y = position[,2], r = sqrt(sizes/pi))
//'
//' ggplot() +
//'   geom_circle(aes(x0 = x, y0 = y, r = r), data = data, fill = 'steelblue') +
//'   geom_circle(aes(x0 = 0, y0 = 0, r = attr(position, 'enclosing_radius'))) +
//'   geom_polygon(aes(x = x, y = y),
//'                data = data[attr(position, 'front_chain'), ],
//'                fill = NA,
//'                colour = 'black')
//'
//[[Rcpp::export(name = "pack_circles")]]
NumericMatrix pack(NumericVector areas) {
  NumericVector::iterator itr;
  std::deque<Circle> circles;
  NumericMatrix res(areas.size(), 2);
  for (itr = areas.begin(); itr != areas.end(); itr++) {
    Circle c = {0, 0, std::sqrt(float(*itr / M_PI)), static_cast<int>(circles.size()) + 1};
    circles.push_back(c);
  }
  if (circles.size() == 0) {
    res.attr("enclosing_radius") = 0;
    res.attr("front_chain") = IntegerVector(0);
  } else {
    FrontChain fc = pack_circles(circles);

    for (int i = 0; i < areas.size(); i++) {
      res(i, 0) = circles[i].x;
      res(i, 1) = circles[i].y;
    }

    res.attr("enclosing_radius") = fc.enclose_radius();
    res.attr("front_chain") = wrap(fc.chain_ind());
  }

  return res;
}

//[[Rcpp::export]]
NumericMatrix circlePackLayout(IntegerVector parent, NumericVector weight) {
  NumericMatrix res(parent.size(), 3);
  unsigned int i;
  std::vector<NodePack*> nodes = createHierarchy(as< std::vector<int> >(parent), as< std::vector<double> >(weight));

  int startNode = findTopNode(nodes);

  nodes[startNode]->packChildren();
  nodes[startNode]->placeChildren(0.0, 0.0);

  for (i = 0; i < nodes.size(); ++i) {
    res(i, 0) = nodes[i]->x;
    res(i, 1) = nodes[i]->y;
    res(i, 2) = nodes[i]->r;
    delete nodes[i];
  }

  return res;
}
