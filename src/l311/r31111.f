
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 r31111"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "r31111.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 15 "r31111.web"
      subroutine r31111
      implicit none
      double precision Ax,Ay,Az,Bx,By,Bz,Cx,Cy,Cz,Dx,Dy,Dz,P11,P12,P13,P
     &21,P22,P23,P31,P32
      double precision P33,Q11,Q12,Q13,Q21,Q22,Q23,Q31,Q32,Q33,Rab,Rabsq
     &,Rcd,Rcdsq,t1,t2,t3,X
      integer i,i1,i2,i3,ind,j,k,l
      common/cgeom/Ax,Ay,Az,Bx,By,Bz,Cx,Cy,Cz,Dx,Dy,Dz,Rab,Rabsq,Rcd,Rcd
     &sq,P11,P12,P13,P21,P22,P23,P31,P32,P33,Q11,Q12,Q13,Q21,Q22,Q23,Q31
     &,Q32,Q33
      common/gout/X(256)
      
      
      
      
      
      i1=64
      i2=128
      i3=192
      do 100 j=1,4
      do 50 k=1,4
      do 20 l=1,4
      i1=i1+1
      i2=i2+1
      i3=i3+1
      t1=X(i1)
      t2=X(i2)
      t3=X(i3)
      X(i1)=P11*t1+P21*t2+P31*t3
      X(i2)=P12*t1+P22*t2+P32*t3
      X(i3)=P13*t1+P23*t2+P33*t3
20    continue
50    continue
100   continue
      ind=-48
      do 200 i=1,4
      ind=ind+48
      do 150 k=1,4
      do 120 l=1,4
      ind=ind+1
      i1=16+ind
      i2=32+ind
      i3=48+ind
      t1=X(i1)
      t2=X(i2)
      t3=X(i3)
      X(i1)=P11*t1+P21*t2+P31*t3
      X(i2)=P12*t1+P22*t2+P32*t3
      X(i3)=P13*t1+P23*t2+P33*t3
120   continue
150   continue
200   continue
      ind=-12
      do 300 i=1,4
      do 250 j=1,4
      ind=ind+12
      do 220 l=1,4
      ind=ind+1
      i1=4+ind
      i2=8+ind
      i3=12+ind
      t1=X(i1)
      t2=X(i2)
      t3=X(i3)
      X(i1)=P11*t1+P21*t2+P31*t3
      X(i2)=P12*t1+P22*t2+P32*t3
      X(i3)=P13*t1+P23*t2+P33*t3
220   continue
250   continue
300   continue
      ind=-3
      do 400 i=1,4
      do 350 j=1,4
      do 320 k=1,4
      ind=ind+4
      i1=1+ind
      i2=2+ind
      i3=3+ind
      t1=X(i1)
      t2=X(i2)
      t3=X(i3)
      X(i1)=P11*t1+P21*t2+P31*t3
      X(i2)=P12*t1+P22*t2+P32*t3
      X(i3)=P13*t1+P23*t2+P33*t3
320   continue
350   continue
400   continue
      return
      
      end
C* :1 * 
      
