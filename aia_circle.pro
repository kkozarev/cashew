function aia_circle, xcenter, ycenter, rad, plot=plot

radius = rad + 10
points = (2 * !PI / 199.0) * FINDGEN(200)

x = xcenter + radius * cos(points)
y = ycenter + radius * sin(points)

if keyword_set(plot) then plots,x,y,/device,thick=3

return, transpose([[x],[y]])

end
