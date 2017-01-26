#include <Rcpp.h>
using namespace Rcpp;

struct Rectangle {
    double x;
    double y;
    double width;
    double height;
};
class Node {
    std::vector<Node*> children;
    Node* parent;
    bool hasParent;
    double Weight;
    double Height;
    int Id;
    int Order;

    static bool comparePtrToNode(Node* a, Node* b) {
        return (a->order() < b->order());
    }
public:
    Node() {
        hasParent = false;
        Weight = 0.0;
        Height = 0.0;
        Id = 0;
        Order = 0;
    };
    Node(int id, int order, double weight) {
        hasParent = false;
        Id = id;
        Order = order;
        Weight = weight;
    };
    Node(int id, int order, double weight, double height) {
        hasParent = false;
        Id = id;
        Order = order;
        Weight = weight;
        Height = height;
    };
    std::vector<Node*> getChildren() {
        std::vector<Node*> childVec;
        for (int i = 0; i < children.size(); ++i) {
            childVec.push_back(children[i]);
        }
        return childVec;
    };
    int nChildren() {
        return children.size();
    };
    double weight() {
        return Weight;
    };
    double height() {
        return Height;
    };
    void addNode(Node* n) {
        double w = n->weight();
        Weight += w;
        if (hasParent) {
            parent->addWeight(w);
        }
        n->setParent(this);
        children.push_back(n);
    };
    void addWeight(double w) {
        if (hasParent) {
            parent->addWeight(w);
        }
        Weight += w;
    };
    void setParent(Node* n) {
        hasParent = true;
        parent = n;
    };
    bool orphan() {
        return !hasParent;
    };
    int order() {
        return Order;
    };
    void sortChildren() {
        std::sort(children.begin(), children.end(), comparePtrToNode);
    };

    Rectangle bounds;
};
double w(std::vector<Node*>& nodes) {
    double w = 0;
    for (int i = 0; i < nodes.size(); ++i) {
        w += nodes[i]->weight();
    }
    return w;
}
void splitLayout(std::vector<Node*> items, Rectangle r) {
    if (items.size() == 0) {
        return;
    }
    if (items.size() == 1) {
        items[0]->bounds = r;
        splitLayout(items[0]->getChildren(), r); // Layout the children within
    } else {
        Rectangle r1, r2;
        std::vector<Node*> s1, s2;
        if (items.size() == 2) {
            s1.push_back(items[0]);
            s2.push_back(items[1]);
        } else {
            double halfSize = w(items) / 2;
            double wH = 0;
            double tmp = 0;
            bool completed = false;
            // Pick out half the weight into l1, half into l2
            for (int i = 0; i < items.size(); ++i) {
                if (completed) {
                    s2.push_back(items[i]);
                } else {
                    tmp = wH + items[i]->weight();
                    // Test if it got worse by picking another item
                    if (std::abs(halfSize - tmp) > std::abs(halfSize - wH)) {
                        s2.push_back(items[i]);
                        completed = true;
                    } else {
                        s1.push_back(items[i]);
                        wH = tmp;
                    }
                }
            }
        }
        double w1 = w(s1);
        double w2 = w(s2);
        if (r.width > r.height) {
            r1.x = r.x;
            r1.y = r.y;
            r1.width = r.width * w1/(w1 + w2);
            r1.height = r.height;
            r2.x = r.x + r1.width;
            r2.y = r.y;
            r2.width = r.width - r1.width;
            r2.height = r.height;
        } else {
            r1.x = r.x;
            r1.y = r.y;
            r1.width = r.width;
            r1.height = r.height * w1/(w1 + w2);
            r2.x = r.x;
            r2.y = r.y + r1.height;
            r2.width = r.width;
            r2.height = r.height - r1.height;
        }
        splitLayout(s1, r1);
        splitLayout(s2, r2);
    }
}
std::vector<Node*> createHierarchy(std::vector<int> parent, std::vector<int> order, std::vector<double> weight) {
    std::vector<Node*> nodes;
    int i;
    for (i = 0; i < parent.size(); ++i) {
        Node* node = new Node(i, order[i], weight[i]);
        nodes.push_back(node);
    }
    for (i = 0; i < parent.size(); ++i) {
        if (parent[i] >= 0) {
            nodes[parent[i]]->addNode(nodes[i]);
        }
    }
    return nodes;
}
int findTopNode(std::vector<Node*>& nodes) {
    int i;
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
    return i;
}

//[[Rcpp::export]]
NumericMatrix splitTreemap(IntegerVector parent, IntegerVector order, NumericVector weight, double width, double height) {
    NumericMatrix rect(parent.size(), 4);
    int i;

    std::vector<Node*> nodes = createHierarchy(as< std::vector<int> >(parent), as< std::vector<int> >(order), as< std::vector<double> >(weight));

    for (i = 0; i < nodes.size(); ++i) {
        nodes[i]->sortChildren();
    }

    int startNode = findTopNode(nodes);
    Rectangle r = {
        0,
        0,
        width,
        height
    };
    nodes[startNode]->bounds = r;
    splitLayout(nodes[startNode]->getChildren(), r);

    for (i = 0; i < nodes.size(); ++i) {
        rect(i, 0) = nodes[i]->bounds.x;
        rect(i, 1) = nodes[i]->bounds.y;
        rect(i, 2) = nodes[i]->bounds.width;
        rect(i, 3) = nodes[i]->bounds.height;
        delete nodes[i];
    }

    return rect;
}
