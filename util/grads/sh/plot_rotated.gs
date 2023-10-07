*
* This is a tmeplate to plot roatated grids without distortion or interpolation
* Recent GRADS versions interoplate fields on rotated grids before plotting.
* A workaround to plot data on th original grid is:
* - rewrite the grib, as if it was on geographical (non-rotated) coordinates
*   for GRIB1: grib_set -s dataRepresentationType=0 filein fileout
* - import the grib in grads: g2g filein ana/for
* - if required, maunally correct the .ctl (grid increments are often missing
*   and re-run gribmap -i file.ctl
* - to plot the map, the aspect ratio of the plot must be manually set.
*   GRADS graphics are drawn on the virtual page, which is a subset of the real
*   page. The real page is 8.5x11 inches; the virtual page can be set to any
*   size within the real page with the command "set vpage xmin xmax ymin ymax"
*   To get a squared grid (as is usually the case for rotated grids), the aspect
*   ratio of the virtual page must match that of the plotting area
*   more details at: http://cola.gmu.edu/grads/gadoc/pagecontrol.html)
*

'white'
'c'

* Set plot area and virtual page
'set lon 1.2 2'
'set lat 2 2.8'
'set mproj off'
'set vpage 2 9 1 8'

* Plot-specific commands (LSM)
'set gxout grfill'
'define_colors'
'set clevs 0.01 0.1 0.25 0.5 0.75 0.9 0.99'
'set ccols 48 46 44 42 72 74 76 78'
'd landsfc'
'cbarn_white'
'set line 2 1 10'
'draw_shape /home/eminguzzi@ARPA.EMR.NET/enr/git/ma_utils/data/shape_regitrot2.dat'
'draw title Cosmo-5M, Land Fraction'
'save_png LSM_Cosmo5M_rot med'
