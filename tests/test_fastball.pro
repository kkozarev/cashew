pro test_fastball
;Test the fastball algorithm
;The function fastball takes an array of point structures, and returns
;the ball structure, which holds the info of the smallest bounding sphere.

  ;Create the array of point structures
  pt={x:0.,y:0.}
  npts=300
  xsize=500
  ysize=xsize
  pts=replicate(pt,npts)
  ;Generate the random point coordinates
  rx=randomn(seed,npts)
  ry=randomn(seed,npts)
  xc=(rx-min(rx))/max(rx-min(rx))*xsize/1.5
  xc+=mean(xc)/2.
  yc=(ry-min(ry))/max(ry-min(ry))*ysize/1.5
  yc+=mean(yc)/2.
  ;Assign the point coordinates
  for i=0,n_elements(pts)-1 do begin
     pts[i].x=xc[i]
     pts[i].y=yc[i]
  endfor

  ;Plot the points
  !P.background=255
  !P.color=0
  wdef,0,xsize,ysize
  for i=0,npts-1 do plots,pts[i].x,pts[i].y,/device,psym=sym(1),symsize=1.5

  ;Run the fastball algorithm
  ball=fastball(pts)
  circle=circle(ball.center.x, ball.center.y, ball.radius)
  plots,circle,/device,color=120
    
end

FUNCTION CIRCLE, xcenter, ycenter, radius
  points = (2 * !PI / 99.0) * FINDGEN(100)
  x = xcenter + radius * COS(points)
  y = ycenter + radius * SIN(points)
  RETURN, TRANSPOSE([[x],[y]])
END

function dot, u, v
  ;Calculate the dot product of two vectors
  dotp=u.x * v.x + u.y * v.y
  return, dotp
end

function norm2, v
  ;Calculate the squared length of a vector
  return, dot(v,v)
end

function norm, v
  ;Calculate the length of a vector
  return, sqrt(norm2(v))
end

function d, u, v
  ;Calculate the distance - norm of difference
  return, norm(u-v)
end

function vdiff, u, v
  ;subtract two vectors
  vect={x:0.D,y:0.D}
  vect.x = u.x - v.x
  vect.y = u.y - v.y
  return, vect
end

function vsratio, u, n
  ; Divide the vector u's components by the scalar n.  
  vect={x:0.D,y:0.D}
  n*=1.D
  vect.x = u.x / n
  vect.y = u.y / n
  return, vect
end

function vsproduct, u, n
  ; Multiply the vector u's components by the scalar n.  
  vect={x:0.D,y:0.D}
  n*=1.D
  vect.x = u.x * n
  vect.y = u.y * n
  return, vect
end

function vsum, u, v
  ;Add two vectors by components
  vect={x:0.D,y:0.D}
  vect.x = u.x + v.x
  vect.y = u.y + v.y
  return, vect
end

function fastball, ptarray
;An array of {x,y} structures containing the points' coordinates
;    ptarray;                            // points to bound
;    npts                                   // the number of points in ptarray
;    float rad, rad2;                    // radius and radius squared
;    float xmin, xmax, ymin, ymax;       // bounding box extremes
;    int   Pxmin, Pxmax, Pymin, Pymax;   // index of  P[] at box extreme
  npts=n_elements(ptarray)
  
  
 ; find a large diameter to start with
 ; first get the bounding box and ptarray[] extreme points for it
  xmin = ptarray[0].x
  xmax=xmin
  ymin = ptarray[0].y
  ymax=ymin
  Pxmin = 0
  Pxmax = 0
  Pymin = 0
  Pymax = 0
  
    for i=0,npts-1 do begin
       if ptarray[i].x lt xmin then begin
          xmin = ptarray[i].x
          Pxmin = i
       endif else begin
          if ptarray[i].x > xmax then begin
             xmax = ptarray[i].x
             Pxmax = i
          endif
       endelse

       if ptarray[i].y lt ymin then begin
          ymin = ptarray[i].y
          Pymin = i
       endif else begin
          if ptarray[i].y > ymax then begin
             ymax = ptarray[i].y
             Pymax = i
          endif
       endelse
    endfor
            
   ; Select the largest extent as an initial diameter for the  ball
    ; diff of Px max and min
    dPx = vdiff(ptarray[Pxmax],ptarray[Pxmin])
    ;dPx = {x:ptarray[Pxmax].x - ptarray[Pxmin].x,$
    ;       y:ptarray[Pxmax].y - ptarray[Pxmin].y}
   ; diff of Py max and min
   ; dPy = {x:ptarray[Pymax].x - ptarray[Pymin].x,$
   ;        y:ptarray[Pymax].y - ptarray[Pymin].y}
    dPy = vdiff(ptarray[Pymax],ptarray[Pymin])
    dx2 = norm2(dPx); Px diff squared
    dy2 = norm2(dPy); Py diff squared   
    if dx2 ge dy2 then begin ; x direction is largest extent
        C = vsum(ptarray[Pxmin], vsratio(dPx,2.0)); Center = midpoint of extremes
        rad2 = norm2(vdiff(ptarray[Pxmax],C)); radius squared
     endif else begin ; y direction is largest extent
        C = vsum(ptarray[Pymin], vsratio(dPy,2.0)); Center = midpoint of extremes
        rad2 = norm2(vdiff(ptarray[Pymax],C)); radius squared
     endelse
    rad = sqrt(rad2)
    
    ; now check that all points P[i] are in the ball
    ; and if not, expand the ball just enough to include them
    for i=0., npts-1 do begin
        dP = vdiff(ptarray[i], C)
        dist2 = norm2(dP)
        if dist2 le rad2 then continue    ; P[i] is inside the ball already
        ; ptarray[i] not in ball, so expand ball  to include it
        dist = sqrt(dist2);
        rad = (rad + dist) / 2.0;          ; enlarge radius just enough
        rad2 = rad * rad;
        C = vsum(C,vsproduct(dP,(dist-rad)/dist));  shift Center toward P[i]
     endfor
    
    Ball={center:{x:0.D,y:0.D},radius:0.D}
    Ball.center = C;
    Ball.radius = rad;
    return, Ball
 end
