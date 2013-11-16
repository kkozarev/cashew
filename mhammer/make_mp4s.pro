pro make_mp4s, event_num, movie_type, wavelength, year, date, location, FRAMES_PER_SECOND = frames_per_second, PATH = path, OLD = old, OUT_OF_SEQUENCE = out_of_sequence
; Parameters:
; Event_num: Each event has a unique number. Leave off the initial 'e'
; Movie_type: The type of the images in the movie (e.g. raw, run, or base)
; Wavelength: The wavelength of the event as a string (e.g. '193')
; Year: The year of the event as a string (e.g. '2011', '2012', or '2013')
; Date: Write the date in MMDD format (eg 0423 for April 23)
; Location: 'E' or 'W', namely east or west
; 
; Frames_per_second: The number of frames per second to be used when
; creating the movie. The default is 10.
; Path: If the path is not an expected path, include it as a keyword
; Old: In the old naming system, 'raw' was left off of the file_names
; if the .png files were regular images. This has been updated in the
; new naming system. However, if you are making a 'raw' movie with old
; files, include this keyword /OLD.
; Out_of_sequence: This is the most important keyword! If the movie
; contains frames with bad exposures (which is likely), the images
; must be renumbered in a tmp directory so that they count
; 1,2,3,4,etc. The program will not do this automatically. It must be
; keyworded /OUT_OF_SEQUENCE. If it is not and there are bad
; exposures, the program will crash.
;
; What This Program Does:
; This procdure will turn a set of numbered png files in the same
; directory into a movie with a specified name based on the the type
; of movie and the details of the event.


; Determine Frames Per Second
if not keyword_set(FRAMES_PER_SECOND) then frames_per_second = '10'

; Determine Path
if not keyword_set(PATH) then begin
   case year of
   '2011': path = '/data/tokyo/kkozarev/2011events/'
   '2012': path = '/Volumes/Scratch/Users/mhammer/2012events/'
   '2013': path = '/Volumes/Scratch/Users/mhammer/2013events/'
   endcase
endif

process = '/usr/local/bin/ffmpeg -f image2 -r '
fps = frames_per_second
path_call = ' -i '
full_path = path
e_event_num = 'e' + event_num
e_event_num_loc = 'e' + date + location
ext = '/normalized_AIA_'
subdata = '_subdata_'
filetype = movie_type + '_'
png = '%03d.png'
random1 = ' -an -pix_fmt "yuv420p" -vcodec "libx264" -level 41 -crf 18.0 -b "28311k" -r '
random2 = ' -bufsize "28311k" -maxrate "28311k" -g "100" -coder 1 -profile main -preset faster -qdiff 4 -qcomp 0.7 -directpred 3 -flags +loop+mv4 -cmp +chroma -partitions +parti4x4+partp8x8+partb8x8 -subq 7 -me_range 16 -keyint_min 1 -sc_threshold 40 -i_qfactor 0.71 -rc_eq ''blurCplx^(1-qComp)'' -b_strategy 1 -bidir_refine 1 -refs 6 -deblockalpha 0 -deblockbeta 0 -trellis 1 -x264opts keyint=10:min-keyint=1:bframes=1 -threads 2 '
movie_path = '/Volumes/Scratch/Users/mhammer/Movies/'

if keyword_set(OUT_OF_SEQUENCE) then begin
   ext = '/tmp' + ext ; Use a tmp directory for the images
   old_dir = full_path + e_event_num + '/' + movie_type + '/' + wavelength
   new_dir = old_dir + '/tmp/'
   file_mkdir, new_dir ; Create new tmp directory
   
   f = file_search(old_dir + '/*.png')
   for i = 0L, n_elements(f)-1 do begin
      file_link, f[i], $
                 old_dir + ext + year + date + '_' + event_num + '_' $
                 + wavelength + subdata + movie_type + '_' $
                 + string(i, FORMAT = '(I03)') + '.png', $
                 /verbose ; Loop through images and set links
   endfor
endif

; Note that the old format did not include 'raw' in the file name
; The new format (the else case) corrects this inconsistency.
if keyword_set(OLD) and strcmp(movie_type, 'raw') then begin
   command = process + fps + path_call + full_path + e_event_num + '/' $
             + movie_type + '/' + wavelength + ext + year + date $
             + '_'+ event_num $
             + '_' + wavelength + subdata + png + random1 $
             + fps + random2 + movie_path + year + '/' + e_event_num + '/' $
             + movie_type + '_' + wavelength + '_' + e_event_num_loc + '.mp4'
endif else begin
   command = process + fps + path_call + full_path + e_event_num + '/' $
             + movie_type + '/' + wavelength + ext + year + date $
             + '_' + event_num $
             + '_' + wavelength + subdata + filetype + png + random1 $
             + fps + random2 + movie_path + year + '/' + e_event_num + '/' $
             + movie_type + '_' + wavelength + '_' + e_event_num_loc + '.mp4'
endelse
spawn, command

end ; EOF


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
