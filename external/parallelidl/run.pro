if n_elements(bridges) eq 0 then bridges = build_bridges()

in = indgen(10,20)
pout = ptr_new(fltarr(10,20), /no_copy)

ncpus = n_elements(bridges)

for i=0,ncpus-1 do begin
	(bridges[i])->execute, '.r worker'
	(bridges[i])->execute, '.r callback'
	(bridges[i])->setproperty, callback='callback'
endfor

; loop over all work packages
for i=0l,9 do begin
	for j=0l,19 do begin
		ud = {i:i,j:j,pout:pout}
		bridge = get_idle_bridge(bridges)
		bridge->setproperty, userdata=ud
		bridge->setvar, 'in', in[i,j]
		bridge->execute, /nowait, 'worker, in, out'
	endfor
endfor
barrier_bridges, bridges

out = (*pout)

end

