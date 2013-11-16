pro test_aia_make_movies

  ;You can run this for a single event, like so
  label='110125_01'
  label='131029_01'
  event=load_events_info(label=label)
  movie_type='run'
  wavelength='193'
  aia_make_movies, event, movie_type=movie_type, wav=wavelength, FRAMES_PER_SECOND = frames_per_second,/force
  stop
  stop
  
  ;Alternatively, run it for all/multiple events
  events=load_events_info()
  wavelengths=['193','211']
  movie_types=['raw','base','run']
  nevents=n_elements(events)
  for ev=0,nevents-1 do begin
     event=events[ev]
     for w=0,n_elements(wavelengths)-1 do begin
        wavelength=wavelengths[w]
        for mt=0,n_elements(movie_types)-1 do begin
           movie_type=movie_types[mt]
           aia_make_movies, event, movie_type=movie_type, wav=wavelength, FRAMES_PER_SECOND = frames_per_second,/force
        endfor
     endfor
  endfor
end


pro aia_make_movies, event, wav=wav, FRAMES_PER_SECOND = frames_per_second, PATH = path, force=force, movie_type=movie_type
;PURPOSE:
; This procdure will turn a set of numbered png files in the same
; directory into a movie with a specified name based on the the type
; of movie and the details of the event.
;
;CATEGORY:
; AIA/General
;
;INPUTS:
;	event - the event structure returned by load_events_info()
;       movie_type - 'raw', 'base', 'run'
;	wavelength - wavelength of the AIA channel, string - 94,131,171,193,211,304,335
;       force - force the program to overwrite movies
;
;KEYWORDS:
; Frames_per_second: The number of frames per second to be used when
; creating the movie. The default is 10.
; Path: If the path is not an expected path, include it as a keyword 
;
;OUTPUTS:
;
;DEPENDENCIES:
;
;
;MODIFICATION HISTORY:
;Written by Michael Hammer - 07/2013
;Kamen Kozarev, 11/2013 - Integrated into the framework
;

; Determine Frames Per Second
if not keyword_set(FRAMES_PER_SECOND) then frames_per_second = '10'

if not keyword_set(wav) then wavelength = '193' else wavelength=wav
if not keyword_set(movie_type) then movie_type = 'raw'

; Determine Path
if not keyword_set(PATH) then path=event.savepath
movie_path=path




;Prepare the date string
tmp=strsplit(event.date,'/',/extract)
date=tmp[0]+tmp[1]+tmp[2]

process = '/usr/local/bin/ffmpeg -y -f image2 -r '
fps = frames_per_second
path_call = ' -i '
full_path = path
filetype = movie_type + '_'

random1 = ' -an -pix_fmt "yuv420p" -vcodec "libx264" -level 41 -crf 18.0 -b "28311k" -r '
random2 = ' -bufsize "28311k" -maxrate "28311k" -g "100" -coder 1 -profile main -preset faster -qdiff 4 -qcomp 0.7 -directpred 3 -flags +loop+mv4 -cmp +chroma -partitions +parti4x4+partp8x8+partb8x8 -subq 7 -me_range 16 -keyint_min 1 -sc_threshold 40 -i_qfactor 0.71 -rc_eq ''blurCplx^(1-qComp)'' -b_strategy 1 -bidir_refine 1 -refs 6 -deblockalpha 0 -deblockbeta 0 -trellis 1 -x264opts keyint=10:min-keyint=1:bframes=1 -threads 2 '

savepath=path+'png/'
movie_path=path+'movies/'
pngfname='normalized_AIA_'+date + '_' + event.label + '_' + wavelength + "_subdata_" + movie_type

if movie_type eq 'araw' or movie_type eq 'arun' or movie_type eq 'abase' then begin
   savepath=path+'annulusplot/'
   movie_path=path+'movies/'
   tp=strmid(movie_type,1)
   pngfname='annplot_'+date+'_'+event.label+'_'+wavelength+'_'+tp
endif



;The png image files
imgfnames = savepath + movie_type + '/' + wavelength + '/' + pngfname + '_%03d.png'
imgsearch = savepath + movie_type + '/' + wavelength + '/' + pngfname + '_*.png'
moviefname = movie_path + movie_type + '_' + wavelength + '_' + event.label + '.mp4'


print,''
print,'----'
print,'Creating movie ' + moviefname
print,''
if not file_exist(imgsearch) then begin
   print,'Required PNG files do not exist: '+imgsearch
   print,'Quitting...'
   print,'----'
   return
endif

if file_exist(moviefname) and not keyword_set(force) then begin
   print,'This movie file exists. To overwrite, rerun with /force. Quitting...'
   print,'----'
   return
endif

command = process + fps + path_call + imgfnames + random1 + fps + random2 + moviefname
spawn, command

print,'Success.'
print,'----'
end                             ; EOF


; Sample Command Line Input:
; /usr/local/bin/ffmpeg -f image2 -r 10 -i /data/tokyo/kkozarev/2011events/e13/bin/193/normalized_AIA_20110211_13_193_subdata_base_%03d.png -an -pix_fmt "yuv420p" -vcodec "libx264" -level 41 -crf 18.0 -b "28311k" -r 10 -bufsize "28311k" -maxrate "28311k" -g "100" -coder 1 -profile main -preset faster -qdiff 4 -qcomp 0.7 -directpred 3 -flags +loop+mv4 -cmp +chroma -partitions +parti4x4+partp8x8+partb8x8 -subq 7 -me_range 16 -keyint_min 1 -sc_threshold 40 -i_qfactor 0.71 -rc_eq 'blurCplx^(1-qComp)' -b_strategy 1 -bidir_refine 1 -refs 6 -deblockalpha 0 -deblockbeta 0 -trellis 1 -x264opts keyint=10:min-keyint=1:bframes=1 -threads 2 /Volumes/Scratch/Users/mhammer/Movies/2011/e13/bin_193_e0211E.mp4


; Sample Link Setup Input
; # Create a temporary directory to house the symbolic links
; mkdir tmp
; # counter for incrementing the symbolic links
; @ argnum = 0
; # initialize the for-loop for png image files
; foreach i (`ls -1 | grep "\.png" | sort`)
;    # create a symbolic link for each item $i
;    ln -fs ../$i `printf "tmp/img_%03d.png" $argnum`
;    # increment the counter by one for each iteration
;    @ argnum += 1
; end
