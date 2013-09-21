function line_sphere_intersection,p1,p2,sc,r
;Code adapted from the explanation/code examples by Paul Bourke
;http://paulbourke.net/geometry/sphereline/
;Kamen Kozarev, 11/11/11 (!)

; INPUT:
;      p1, p2 - 3-element arrays containing the X,Y,Z coordinates of
;               the two points that define the line.
;      sc - 3-element array with the XYZ position of the sphere
;                center.
;      r - a value for the radius of the sphere.
;
; OUTPUT:
;      pint - a 7-element array containing the XYZ coordinates of the
;             intersection point. The function returns 0 in the first
;             element if there is no intersection, 1 if there is one
;             intersection, or 2 for two intersections. The format of
;             the array is [i,x1,y1,z1,x2,y2,z2].
pint = fltarr(7)

;Find the XYZ distances between the two points.
  dx = p2[0] - p1[0]
  dy = p2[1] - p1[1]
  dz = p2[2] - p1[2]
  
  a = dx^2 + dy^2 + dz^2
  b = 2 * (dx * (p1[0] - sc[0]) + dy * (p1[1] - sc[1]) + dz * (p1[2] - sc[2]))
  c = sc[0]^2 + sc[1]^2 + sc[2]^2
  c += p1[0]^2 + p1[1]^2 + p1[2]^2
  c -= 2 * (sc[0] * p1[0] + sc[1] * p1[1] + sc[2] * p1[2])
  c -= r^2
  
  u = b^2 - 4 * a * c

;1. No intersection
  if u lt 0.0 then begin
     pint[0] = 0.0
     return,pint
  endif
  
;2. One intersection
  if u eq 0.0 then begin
     mu = -b / (2 * a)
     pint[0] = 1.0
     pint[1] = p1[0] + mu*(p2[0] - p1[0])
     pint[2] = p1[1] + mu*(p2[1] - p1[1])
     pint[3] = p1[2] + mu*(p2[2] - p1[2])

     return,pint
  endif
  
 ;3. Two intersections
  if u gt 0.0 then begin
     pint[0] = 2

     ;first intersection
     mu = (-b + sqrt( b^2 - 4 * a * c )) / (2 * a)
     pint[1] = p1[0] + mu*(p2[0] - p1[0])
     pint[2] = p1[1] + mu*(p2[1] - p1[1])
     pint[3] = p1[2] + mu*(p2[2] - p1[2])

     ;second intersection
     mu = (-b - sqrt( b^2 - 4 * a * c )) / (2 * a)
     pint[4] = p1[0] + mu*(p2[0] - p1[0])
     pint[5] = p1[1] + mu*(p2[1] - p1[1])
     pint[6] = p1[2] + mu*(p2[2] - p1[2])

     return,pint
  endif

end
