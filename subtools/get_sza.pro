FUNCTION GET_SZA, year, month, day, hour, minute, second, lat, lon 
 
  x      = 2.*!dpi/365.25* (julday(MONTH,DAY,YEAR)-julday(12,31,YEAR-1)) - 2.72*2.*!dpi/360. 
  delta  = asin(0.39776*sin(x-77.51*2.*!dpi/360.+1.92*2.*!dpi/360.*sin(x))) 
  z      = (-7.66*sin(x)-9.87*sin(2.*x+24.99*2.*!dpi/360.+3.83*2.*!dpi/360.*sin(x)))*60. 
  h      = (2.*!dpi/86400.)*(HOUR*3600.+MINUTE*60. -43200. + lon*4.*60. + z) 
  azi    = 360./(2.*!DPI) * atan(sin(h) / (cos(h)*sin(2.*!dpi*lat/360.) - tan(delta)*cos(2.*!dpi*lat/360.) ) ) 
  cost   = sin(2.*!dpi*lat/360.)*sin(delta) + cos(2.*!dpi*lat/360.)*cos(delta)*cos(h) 
  SZA    = (360/(2*!dpi))*acos(cost) 
 
  IF [cos(h)*sin(2.*!dpi*lat/360.) - tan(delta)*cos(2.*!dpi*lat/360.)] LT 0 THEN azi = azi + 180. 
  IF azi LT 0 THEN azi=azi +360. 
  azi = azi+180. 
  IF azi GE 360 THEN azi = azi - 360. 
 
  RETURN, SZA
END 
