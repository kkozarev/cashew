function hostname
;a small program to return the name of the local computer.

spawn,"hostname",/noshell,tmp
pcname=strsplit(tmp,'.',/extract)
pcname=pcname[0]
return, pcname
end
