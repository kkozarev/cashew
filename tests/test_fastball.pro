pro fastball, ptarray, ball

;    Point C;                            // Center of ball
;    float rad, rad2;                    // radius and radius squared
;    float xmin, xmax, ymin, ymax;       // bounding box extremes
;    int   Pxmin, Pxmax, Pymin, Pymax;   // index of  P[] at box extreme

    ; find a large diameter to start with
    ; first get the bounding box and P[] extreme points for it
    xmin = xmax = P[0].x;
    ymin = ymax = P[0].y;
    Pxmin = Pxmax = Pymin = Pymax = 0;
    for (int i=1; i<n; i++) {
        if (P[i].x < xmin) {
            xmin = P[i].x;
            Pxmin = i;
        }
        else if (P[i].x > xmax) {
            xmax = P[i].x;
            Pxmax = i;
        }
        if (P[i].y < ymin) {
            ymin = P[i].y;
            Pymin = i;
        }
        else if (P[i].y > ymax) {
            ymax = P[i].y;
            Pymax = i;
        }
    }
    // select the largest extent as an initial diameter for the  ball
    Vector dPx = P[Pxmax] - P[Pxmin]; // diff of Px max and min
    Vector dPy = P[Pymax] - P[Pymin]; // diff of Py max and min
    float dx2 = norm2(dPx); // Px diff squared
    float dy2 = norm2(dPy); // Py diff squared
    if (dx2 >= dy2) {                      // x direction is largest extent
        C = P[Pxmin] + (dPx / 2.0);          // Center = midpoint of extremes
        rad2 = norm2(P[Pxmax] - C);          // radius squared
    }
    else {                                 // y direction is largest extent
        C = P[Pymin] + (dPy / 2.0);          // Center = midpoint of extremes
        rad2 = norm2(P[Pymax] - C);          // radius squared
    }
    rad = sqrt(rad2);

    // now check that all points P[i] are in the ball
    // and if not, expand the ball just enough to include them
    Vector dP;
    float dist, dist2;
    for (int i=0; i<n; i++) {
        dP = P[i] - C;
        dist2 = norm2(dP);
        if (dist2 <= rad2)     // P[i] is inside the ball already
            continue;
        // P[i] not in ball, so expand ball  to include it
        dist = sqrt(dist2);
        rad = (rad + dist) / 2.0;          ; enlarge radius just enough
        rad2 = rad * rad;
        C = C + ((dist-rad)/dist) * dP;    ; shift Center toward P[i]
    }
    B->center = C;
    B->radius = rad;
    return;
}


end
