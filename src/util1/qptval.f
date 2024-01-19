
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qptval"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qptval.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "qptval.web"
      integer function qptval(TABLE,TCURSR)
      implicit none
      integer i
      integer TABLE,TCURSR
      dimension TABLE(*)
      
      TCURSR=TCURSR+1
      i=TABLE(TCURSR)
      qptval=i
      return
      
      end
C* :1 * 
      
