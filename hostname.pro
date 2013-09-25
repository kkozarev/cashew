function hostname
;PURPOSE:
; a small program to return the name of the local computer.
;CATEGORY:
; General
;
;INPUTS:
;
;KEYWORDS:
; 
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev
;

spawn,"hostname",/noshell,tmp
pcname=strsplit(tmp,'.',/extract)
pcname=pcname[0]
return, pcname
end
