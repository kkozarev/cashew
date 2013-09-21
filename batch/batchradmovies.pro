pro getpixvalue

wait,0.4


more=1
while more eq 1 do begin
print,'click on a pixel:'
cursor,x,y
print, 'x: '+strtrim(string(x),2) + '   y: '+ strtrim(string(x),2)

read,'More? (Y/N)',answer
if answer eq 'N' or answer eq 'n' then more=0
endwhile

end
