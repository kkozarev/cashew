pro run_plot_rotated_wave
; What This Program Does
; This program shows several proper calls to the procedure
; plot_rotated_wave, which is used in conjunction with
; polyfill_process to make r-t (height-time) plots of the 'intensity'
; of a base difference movie of a shock wave as a function of radius
; and time in a particular direction (e.g. the radial direction)

; Insert Final Call Here
plot_rotated_wave, '042313', '0423W', 'W'

; Here are your options:
;
; (0)
; This call is the most basic. No keywords are used.
; plot_rotated_wave, date, evnum, location
; Sample 1:
; plot_rotated_wave, '050113', '0501E', 'E'
; Sample 2:
; plot_rotated_wave, '080911', '45', 'W'

; (1)
; This call is more practical if you are already familiar with the
; event and know approximately when the event starts and ends.
; plot_rotated_wave, date, evnum, location, start = *START*, end = *END*
; Sample 1:
; plot_rotated_wave, '042313', '0423W', 'W', start = 50, end = 100
; Sample 2:
; plot_rotated_wave, '080911', '45', 'W', start = 30, end = 90

; (2)
; This call is identical to the previous (1). However, in this case, you
; have aleady seen the movie and don't want to see it ever
; again out of anger and hatred. Choose the start and end frames to be
; similar so that when the movie plays initially, you don't
; have to watch too many frames.
; Note: With this option, when prompted to select the start and end
; frames while the program is running, do it. The default options won't 
; help you at all.
; Sample 1:
; plot_rotated_wave, '042313', '0423W', 'W', start = 88, end = 90
; Sample 2:
; plot_rotated_wave, '080911', '45', 'W', start = 39, end = 40

; (3)
; This call is basic, except that you don't want the wavelength
; to be 193. You probably want it to be 211, but there are other options.
; plot_rotated_wave, date, evnum, location, wavelength = *WAVELENGTH*
; Sample 1:
; plot_rotated_wave, '042313', '0423W', 'W', wavelength = 171
; Sample 2:
; plot_rotated_wave, '080911', '45', 'W', wavelength = 211

; (4)
; This call is basic, except that there is a problem with frame 0 in
; your data. Thus, you want to choose a different frame to subtract
; when calculating the base difference images.
; plot_rotated_wave, date, evnum, location, bas_index = *BAS_INDEX*
; Sample 1:
; plot_rotated_wave, '042313', '0423W', 'W', bas_index = 40
; Sample 2:
; plot_rotated_wave, '080911', '45', 'W', bas_index = 3

; (5)
; This call is basic, except that you don't like base
; difference images. You want run difference images.
; plot_rotated_wave, date, evnum, location, /RUN
; Sample 1:
; plot_rotated_wave, '042313', '0423W', 'W', /RUN
; Sample 2:
; plot_rotated_wave, '080911', '45', 'W', /RUN

; (6)
; This call is basic, except that you want a strong base image.
; Instead of frame 0, you want an average of 5 frames, 0-4.
; plot_rotated_wave, date, evnum, location, /BASE
; Sample 1:
; plot_rotated_wave, '042313', '0423W', 'W', /BASE
; Sample 2:
; plot_rotated_wave, '080911', '45', 'W', /BASE

; (7)
; This call is identical to the previous except that you want to
; choose a different set of 5 frames to average for the base.
; This can be done by setting bas_index to i. Then, the frames that
; will be averaged are i, i+1, i+2, i+3, & i+4
; plot_rotated_wave, date, evnum, location, bas_index = *BAS_INDEX*, /BASE
; Sample 1:
; plot_rotated_wave, '042313', '0423W', 'W', bas_index = 48, /BASE
; Sample 2:
; plot_rotated_wave, '080911', '45', 'W', bas_index = 5, /BASE

; (8)
; This call is basic, except that you are not on my computer. Thus,
; the path needs to be changed so that it actually exists.
; plot_rotated_wave, date, evnum, location, path = *PATH*
; Sample 1:
; plot_rotated_wave, '042313', '0423W', 'W', path = '~/Documents/events/'
; Sample 2:
; plot_rotated_wave, '080911', '45', 'W', path = '~/Documents/events/'

; (9)
; All of the calls above are really basic. I have more convoluted
; problems and want to use multiple keywords. Great. Then, do it... :-\
; Sample 1:
; In this example, you are on someone else's computer, want to
; use an averaged base image from 15-19, know that the start frame is
; 25, the end frame is 85, and the wavelength of choice is 211.
; plot_rotated_wave, '042313', '0423W', 'W', start = 25, finish = 85,
; path = '~/Documents/my_events/2013/something/', wavelength = 211,
; bas_index = 15, /BASE


end ; EOF
