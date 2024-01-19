
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 aclear"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "aclear.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "aclear.web"
      subroutine aclear(N,A)
      implicit none
      double precision A,zero
      integer i,N
      
      
      dimension A(*)
      data zero/0.0D0/
      
      do 100 i=1,N
      A(i)=zero
100   continue
      
      return
      
      end
C* :1 * 
      
