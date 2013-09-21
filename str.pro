function str, invar
;This function returns the string version of a numerical variable,
;with all white space removed, in the appropriate format so that
;no information is lost.
;02/2011 Kamen Kozarev

case size(invar,/type) of 
     2:  format='(I)';INT
     3:  format='(I)';LONG
     4:  format='(G)';FLOAT
     5:  format='(G)';DOUBLE 
     7:  format='';STRING 
     8:  ;STRUCT
     12: format='(I)';UINT Unsigned Integer 
     13: format='(I)';ULONG Unsigned Longword Integer 
     14: format='(I)';LONG64 64-bit Integer 
     15: format='(I)';ULONG64 Unsigned 64-bit Integer
endcase



return,strtrim(string(invar,format=format),2)

end
