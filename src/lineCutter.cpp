#include <Rcpp.h>
using namespace Rcpp;

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

}

Point intersection(Point p, Point p0, double width, double height) {
    double xmin, xmax, ymin, ymax;
    xmin = p0.x - width;
    xmax = p0.x + width;
    ymin = p0.y - height;
    ymax = p0.y + height;

    if (p.x < xmin) {
        if (p.y > ymin && p.y < ymax) {
            return segment_intersect(p, p0, point(xmin, ymin), point(xmin, ymax));
        } else if (p.y < ymin) {
            Point p_tmp = segment_intersect(p, p0, point(xmin, ymin), point(xmin, ymax));
            if (p_tmp.REAL) {
                return p_tmp;
            } else {
                return segment_intersect(p, p0, point(xmin, ymin), point(xmax, ymin));
            }
        } else {
            Point p_tmp = segment_intersect(p, p0, point(xmin, ymax), point(xmax, ymax));
            if (p_tmp.REAL) {
                return p_tmp;
            } else {
                return segment_intersect(p, p0, point(xmin, ymax), point(xmin, ymin));
            }
        }
    } else if (p.x > xmax) {
        if (p.y > ymin && p.y < ymax) {
            return segment_intersect(p, p0, point(xmax, ymin), point(xmax, ymax));
        } else if (p.y < ymin) {
            Point p_tmp = segment_intersect(p, p0, point(xmax, ymin), point(xmin, ymin));
            if (p_tmp.REAL) {
                return p_tmp;
            } else {
                return segment_intersect(p, p0, point(xmax, ymin), point(xmax, ymax));
            }
        } else {
            Point p_tmp = segment_intersect(p, p0, point(xmax, ymax), point(xmin, ymax));
            if (p_tmp.REAL) {
                return p_tmp;
            } else {
                return segment_intersect(p, p0, point(xmax, ymax), point(xmax, ymin));
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

NumericMatrix capSquareStart(NumericMatrix coord, double width, double height) {
    int i;
    Point p;
    Point p0 = point(coord(0, 0), coord(0, 1));
    width /= 2;
    height /= 2;
    for (i = 0; i < coord.nrow(); ++i) {
        p.x = coord(i, 0);
        p.y = coord(i, 1);
        if (std::abs(p.x - p0.x) <= width && std::abs(p.y - p0.y) <= height) {
            coord(i, 0) = NA_REAL;
            coord(i, 1) = NA_REAL;
        } else {
            Point intersect = intersection(p, p0, width, height);
            coord(i-1, 0) = intersect.x;
            coord(i-1, 0) = intersect.y;
            break;
        }
    }
}
/*
 * Copyright (c) 1970-2003, Wm. Randolph Franklin
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimers.
 * 2. Redistributions in binary form must reproduce the above copyright notice
 *    in the documentation and/or other materials provided with the
 *    distribution.
 * 3. The name of W. Randolph Franklin may not be used to endorse or promote
 *    products derived from this Software without specific prior written
 *    permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
int pnpoly(int nvert, float *vertx, float *verty, float testx, float testy) {
    int i, j, c = 0;
    for (i = 0, j = nvert-1; i < nvert; j = i++) {
        if ( ((verty[i]>testy) != (verty[j]>testy)) &&
             (testx < (vertx[j]-vertx[i]) * (testy-verty[i]) / (verty[j]-verty[i]) + vertx[i]) )
            c = !c;
    }
    return c;
}
int pnellipsis(float x, float y, float rx, float ry, float testx, float testy) {
    float xdir = (testx - x)*(testx - x) / (rx*rx);
    float ydir = (testy - y)*(testy - y) / (ry*ry);
    if (xdir + ydir <= 1) {
        return 1;
    } else {
        return 0;
    }
}
