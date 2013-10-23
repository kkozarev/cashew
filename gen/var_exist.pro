function var_exist, var

YES = 1
NO  = 0

sz = size(var)

if (sz[1] eq 0) then return,NO   $
else return,YES
end
