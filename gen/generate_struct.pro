function generate_struct,struct_type
;PURPOSE:
;This function generates an instance of structures in use throughout
;CASHeW. It is useful since all the structure definitions are kept in
;one place.
;
;CATEGORY:
;General
;
;INPUTS:
;       struct_type - string, the structure type requested
;
;KEYWORDS:
;
;OUTPUTS:
;       struct_instance - an instance of the requested structure
; 
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Kamen Kozarev, 07/13/2017
;
  struct_instance = {nodata:-9999}

  if struct_type eq 'cashewtime' then begin
     struct_instance={cashew_time:'',date_obs:'',JD:0.D,relsec:0.D}
  endif
  
  if struct_type eq 'crosspoint' then begin
     cashewtime=generate_struct('cashewtime')
     help,cashewtime
     struct_instance={time:cashewtime,$
                      rpx:0.0D,$
                      rpy:0.0D,$
                      rpz:0.0D,$
                      px:0.0D,$
                      py:0.0D,$
                      pz:0.0D,$
                      thbn:0.0,$
                      linid:0L,$
                      bmag:0.0D,$
                      density:0.0D,$
                      temperature:0.0,$
                      open:0,$
                      shockjump:0.0,$
                      vshock:0.0}
  endif

  if struct_type eq 'plotinfo' then begin
     struct_instance={p:!P, x:!X, y:!Y, $
                      origin:[0,0], $
                      winsize:[1000,1000], $
                      multi:[0,0,0], $
                      winind:3,$
                      difforigin:[0,0],$
                      kinquantity:['R!D0!N','R!D1!N','V!DR,0!N','V!DR,1!N','a'],$
                      kinunit:[' R!DS!N',' R!DS!N',' km/s',' km/s',' km/s!U2!N'],$
                      xtitle:'',$
                      imgtit:'',$
                      savename:''}
  endif
  
  if struct_type eq 'raddata' then begin
     cashewtime=generate_struct('cashewtime')
     struct_instance={type:'radial',$
                      label:'',$
                      wav:'',$
                      time:cashewtime,$
                      y_rsun_array:0.,$
                      y_arcsec_array:0.,$
                      x_deg_array:0.,$
                      data:dblarr(10,1),$
                      origdata:dblarr(10,1),$
                      diffdata:dblarr(10,1),$
                      radfitrange:[0,0],$
                      timefitrange:[0,0],$
                      fitparams:replicate({front:0.0, peak:0.0, back:0.0},3),$
                      fitsigma:replicate({front:0.0, peak:0.0, back:0.0},3),$
                      kinfittimerange:{front:[0,0],peak:[0,0],back:[0,0]},$
                      savgolfits:{front:replicate({speed:0.0,accel:0.0},10),$
                                  peak:replicate({speed:0.0,accel:0.0},10),$
                                  back:replicate({speed:0.0,accel:0.0},10)},$                   
                      maxinds:intarr(1,10),$
                      wave_frontedge:replicate({rad:0.0D, stdv:0.0D, val:0.0D, yind:0L, xind:0L},10),$
                      wave_backedge:replicate({rad:0.0D, stdv:0.0D, val:0.0D, yind:0L, xind:0L},10),$
                      wave_peak:replicate({rad:0.0D, stdv:0.0D, val:0.0D, yind:0L, xind:0L},10),$
                      wavethick:fltarr(10),$
                      avgIntense:fltarr(10)}
  endif

  if struct_type eq 'latdata' then begin
     cashewtime=generate_struct('cashewtime')
     struct_instance={type:'lateral',$
                      status:-1,$
                      label:'',$
                      radius:0.0,$
                      latmeasind:0,$
                      wav:'',$
                      time:cashewtime,$
                      y_rsun_array:0,$
                      y_arcsec_array:0,$
                      x_deg_array:0,$
                      data:dblarr(10,1),$
                      origdata:dblarr(10,1),$
                      diffdata:dblarr(10,1),$
                      latfitrange:[0,0],$
                      timefitrange:[0,0],$
                      fitparams:replicate({front:0.0, peak:0.0, back:0.0},3),$
                      fitsigma:replicate({front:0.0, peak:0.0, back:0.0},3),$
                      kinfittimerange:{front:[0,0],peak:[0,0],back:[0,0]},$
                      savgolfits:{front:replicate({speed:0.0,accel:0.0},10),$
                                  peak:replicate({speed:0.0,accel:0.0},10),$
                                  back:replicate({speed:0.0,accel:0.0},10)},$
                      maxinds:intarr(1,10),$
                      wave_frontedge:replicate({lat:0.0D, stdv:0.0D, val:0.0D, yind:0L, xind:0L},10),$
                      wave_backedge:replicate({lat:0.0D, stdv:0.0D, val:0.0D, yind:0L, xind:0L},10),$
                      wave_peak:replicate({lat:0.0D, stdv:0.0D, val:0.0D, yind:0L, xind:0L},10),$
                      wavethick:fltarr(10),$
                      avgIntense:fltarr(10),$
                      mymaxima:replicate({val:0.0D,ind:0L,lat:0.0D,gfit:dblarr(5),nmax:0,metric:0.D},3,10)}
  endif
  
  return,struct_instance
end
