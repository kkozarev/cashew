function find_latest_file,query
;PURPOSE:
;Return the latest modified file in a folder/search query.
;
;CATEGORY:
;GEN
;
;INPUTS:
;       query - a string containing the search query to pass to file_search
;
;KEYWORDS:
;
;OUTPUTS:
;
; 
;DEPENDENCIES:
;file_search,file_info
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 02/21/2014
;
  
  res=file_search(query)
  if res[0] eq '' then begin
     print,''
     print,'No files found for query '+query
     return,''
  endif
  
  tmp=file_info(res)
  tmp=max(tmp.mtime,ind)
  file=res[ind]
  
  return,file
end
