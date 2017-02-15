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
    Node* getRoot() {
        if (orphan()) {
            return this;
        } else {
            return parent->getRoot();
        }
    }

    Rectangle bounds;
};
