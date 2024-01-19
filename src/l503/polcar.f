
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 polcar"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "polcar.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "polcar.web"
      subroutine polcar(R,PHI,AR,AI)
      implicit none
      double precision a0,a1,AI,AR,gcos,gsin,PHI,R
      
      a0=R
      a1=PHI
      AR=a0*gcos(a1)
      AI=a0*gsin(a1)
      return
      
      end
C* :1 * 
      
