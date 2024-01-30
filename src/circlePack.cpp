#include <algorithm>

#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/matrix.hpp>
#include <cpp11/R.hpp>
#include <R_ext/Random.h>

#include <deque>
#include <vector>

struct randWrapper {
  using result_type = unsigned int;

  static constexpr result_type min() {
    return 0;
  }

  static constexpr result_type max() {
    return RAND_MAX;
  }

  result_type operator()() {
    return unif_rand()*RAND_MAX;
  }
};

struct Circle {
  double x;
  double y;
  double r;
  int i;
  Circle* next;
  Circle* prev;
};

void place(Circle* c, Circle* b, Circle* a) {
  double dx = b->x - a->x;
  double dy = b->y - a->y;
  double d2 = dx * dx + dy * dy;
  if (d2 != 0.0) {
    double a2 = a->r + c->r;
    a2 *= a2;
    double b2 = b->r + c->r;
    b2 *= b2;
    if (a2 > b2) {
      double x = (d2 + b2 - a2) / (2 * d2);
      double y = std::sqrt(std::max(0.0, b2 / d2 - x * x));
      c->x = b->x - x * dx - y * dy;
      c->y = b->y - x * dy + y * dx;
    } else {
      double x = (d2 + a2 - b2) / (2 * d2);
      double y = std::sqrt(std::max(0.0, a2 / d2 - x * x));
      c->x = a->x + x * dx - y * dy;
      c->y = a->y + x * dy + y * dx;
    }
  } else {
    c->x = a->x + c->r;
    c->y = a->y;
  }
}
bool circles_intersect(Circle* c1, Circle* c2) {
  double dx = c2->x - c1->x;
  double dy = c2->y - c1->y;
  double dr = c2->r + c1->r - 1e-6;
  return dr > 0 && dr*dr > dx*dx + dy*dy;
}
double score(Circle* c) {
  double ab = c->r + c->next->r;
  double dx = (c->x * c->next->r + c->next->x * c->r) / ab;
  double dy = (c->y * c->next->r + c->next->y * c->r) / ab;
  return dx * dx + dy * dy;
}

class FrontChain {
  Circle enclosure;
  Circle* next_circle;

  void update_closest_circle() {
    Circle* a = next_circle;
    double aa = score(a);
    Circle* b = a->next;
    while (b != next_circle) {
      double ba = score(b);
      if (ba < aa) {
        a = b;
        aa = ba;
      }
      b = b->next;
    }
    next_circle = a;
  };
  void update_chain(Circle* c, Circle* after) {
    Circle* before = after->next;
    c->prev = after;
    c->next = before;
    after->next = before->prev = c;
  };
  bool encloses(Circle enc, Circle* c) {
    double dx = c->x - enc.x;
    double dy = c->y - enc.y;
    double dr = c->r - enc.r;
    return dr * dr + 1e-6 > dx * dx + dy * dy;
  };
  Circle enclose1(Circle* c1) {
    Circle enc = {c1->x, c1->y, c1->r};
    if (enc.r > 1e10 || enc.r < 0) {
      cpp11::stop("enc1 error");
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
    if (enc.r > 1e10 || enc.r < 0) {
      cpp11::stop("enc2 error");
    }
    return enc;
  };
  Circle enclose3(Circle* c1, Circle* c2, Circle* c3) {
    double A2 = c1->x - c2->x;
    double A3 = c1->x - c3->x;
    double B2 = c1->y - c2->y;
    double B3 = c1->y - c3->y;
    double C2 = c2->r - c1->r;
    double C3 = c3->r - c1->r;
    double D1 = c1->x * c1->x + c1->y * c1->y - c1->r * c1->r;
    double D2 = D1 - c2->x * c2->x - c2->y * c2->y + c2->r * c2->r;
    double D3 = D1 - c3->x * c3->x - c3->y * c3->y + c3->r * c3->r;
    double AB = A3 * B2 - A2 * B3;
    double XA = (B2 * D3 - B3 * D2) / (AB * 2) - c1->x;
    double XB = (B3 * C2 - B2 * C3) / AB;
    double YA = (A3 * D2 - A2 * D3) / (AB * 2) - c1->y;
    double YB = (A2 * C3 - A3 * C2) / AB;
    double A = XB * XB + YB * YB - 1;
    double B = 2 * (XA * XB + YA * YB + c1->r);
    double C = XA * XA + YA * YA - c1->r * c1->r;
    double r = -(std::abs(A) > 1e-6 ? (B + std::sqrt(B * B - 4 * A * C)) / (2 * A) : C / B);
    Circle enc = {
      XA + XB * r + c1->x,
      YA + YB * r + c1->y,
      r
    };
    if (enc.r > 1e10 || enc.r < 0) {
      cpp11::stop("enc3 error");
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
    double angle2 = (2 * M_PI) * unif_rand();
    c2->x = std::cos(float(angle2)) * d12;
    c2->y = std::sin(float(angle2)) * d12;

    // Place third circle according to the first two
    place(c3, c2, c1);

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
    double angle2 = (2 * M_PI) * unif_rand();
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
    Circle* a = next_circle;
    Circle* b = next_circle->next;

    place(c, a, b);

    Circle* j = b->next;
    Circle* k = a->prev;
    double sj = b->r;
    double sk = a->r;
    bool did_intersect = false;

    do {
      if (sj <= sk) {
        if (circles_intersect(j, c)) {
          b = j;
          a->next = b;
          b->prev = a;
          next_circle = a;
          did_intersect = true;
          add(c);
          break;
        }
        sj += j->r;
        j = j->next;
      } else {
        if (circles_intersect(k, c)) {
          a = k;
          a->next = b;
          b->prev = a;
          next_circle = a;
          did_intersect = true;
          add(c);
          break;
        }
        sk += k->r;
        k = k->prev;
      }
    } while (j != k->next);

    if (!did_intersect) {
      update_chain(c, a);

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
      std::shuffle(fc.begin(), fc.end(), randWrapper());
      std::deque<Circle*> Q;
      enclosure = encloseN(fc.begin(), fc.end(), Q);
    }
  }
};

FrontChain pack_circles(std::deque<Circle> &circles) {
  if (circles.size() == 0) {
    cpp11::stop("Cannot pack an empty set of circles");
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
std::vector<NodePack*> createHierarchy(cpp11::integers &parent, cpp11::doubles &weight) {
  std::vector<NodePack*> nodes;
  for (R_xlen_t i = 0; i < parent.size(); ++i) {
    NodePack* node = new NodePack(i + 1, weight[i]);
    nodes.push_back(node);
  }
  for (R_xlen_t i = 0; i < parent.size(); ++i) {
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
    cpp11::stop("No top node. Is this a tree structure?");
  }
  return (int) i;
}

[[cpp11::register]]
cpp11::writable::doubles_matrix<> pack(cpp11::doubles areas) {
  GetRNGstate();
  std::deque<Circle> circles;
  cpp11::writable::doubles_matrix<> res(areas.size(), 2);
  for (R_xlen_t i = 0; i < areas.size(); ++i) {
    Circle c = {0, 0, std::sqrt(areas[i] / M_PI), static_cast<int>(circles.size()) + 1};
    circles.push_back(c);
  }
  if (circles.size() == 0) {
    res.attr("enclosing_radius") = 0;
    res.attr("front_chain") = cpp11::writable::integers();
  } else {
    FrontChain fc = pack_circles(circles);

    for (int i = 0; i < areas.size(); i++) {
      res(i, 0) = circles[i].x;
      res(i, 1) = circles[i].y;
    }

    res.attr("enclosing_radius") = Rf_ScalarReal(fc.enclose_radius());
    auto chain_ind = fc.chain_ind();
    res.attr("front_chain") = cpp11::writable::integers(chain_ind.begin(), chain_ind.end());
  }
  PutRNGstate();
  return res;
}

[[cpp11::register]]
cpp11::writable::doubles_matrix<> circlePackLayout(cpp11::integers parent, cpp11::doubles weight) {
  GetRNGstate();
  cpp11::writable::doubles_matrix<> res(parent.size(), 3);
  unsigned int i;
  std::vector<NodePack*> nodes = createHierarchy(parent, weight);

  int startNode = findTopNode(nodes);

  nodes[startNode]->packChildren();
  nodes[startNode]->placeChildren(0.0, 0.0);

  for (i = 0; i < nodes.size(); ++i) {
    res(i, 0) = nodes[i]->x;
    res(i, 1) = nodes[i]->y;
    res(i, 2) = nodes[i]->r;
    delete nodes[i];
  }
  PutRNGstate();
  return res;
}
