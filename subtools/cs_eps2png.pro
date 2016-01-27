
; C. Schlundt, 2016-01-07
PRO CS_EPS2PNG, epsfile
    filbase = FSC_Base_Filename( epsfile, Directory=dir, Extension=ext )
    pngfile = dir + filbase + '.png'
    ;convert -density 300 ${file} -resize 25% ${base}.png
    SPAWN, 'convert -density 300 ' + epsfile + $
           ' -resize 25% ' + pngfile + ' >/dev/null 2>&1 '
    PRINT, 'Convert  Image: ' + pngfile
    SPAWN, 'rm -f ' + epsfile
    ;PRINT, '** Image removed: ', epsfile
END

