
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dform"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dform.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "dform.web"
      subroutine dform(A,B,MD,NB,NE,X)
      implicit none
      double precision A,B,X,zero
      integer i,j,k,MD,NB,NE
      dimension A(MD,MD),B(MD,MD)
      data zero/0.0D0/
      
      
      
      do 100 i=1,NB
      do 50 j=1,NB
      B(i,j)=zero
      do 20 k=1,NE
      B(i,j)=B(i,j)+X*A(j,k)*A(i,k)
20    continue
50    continue
100   continue
      return
      
      end
C* :1 * 
      
