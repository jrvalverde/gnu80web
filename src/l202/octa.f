
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 octa"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "octa.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 28 "octa.web"
      subroutine octa(MAXAP3,NGRP,NATOMS,NOP,MAXOP,TRANS,NPERM,A,B,D)
      implicit none
      double precision A,abc,B,D,gatan,gfloat,halfpi,one,phi3,phi6,pi,pi
     &ovr4,q,r,s,t,theta,three,TRANS,two
      double precision v,zero
      integer i,i1,i2,iop,iord,ixyz,j,k,l,MAXAP3,MAXOP,NATOMS,NGRP,NOP,N
     &PERM
      dimension NGRP(4),TRANS(*),NPERM(*),A(*),B(*),D(*)
      dimension v(3),q(3,3),r(3,3),s(3,3),t(3,3)
      data zero,one,two,three/0.0D0,1.0D0,2.0D0,3.0D0/
      
      
      
      
      
      
      
      piovr4=gatan(one)
      halfpi=two*piovr4
      pi=two*halfpi
      phi6=pi/three
      phi3=two*phi6
      
      
      do 100 ixyz=1,3
      theta=zero
      do 50 iop=1,3
      theta=theta+halfpi
      call rotate(MAXAP3,A,B,NATOMS,t,ixyz,theta)
      call fill(MAXAP3,NATOMS,NOP,MAXOP,t,TRANS,NPERM,A,B)
50    continue
100   continue
      
      
      call move(MAXAP3,A,D,NATOMS+3)
      v(3)=one
      do 200 i=1,3,2
      v(1)=gfloat(2-i)*one
      do 150 j=1,3,2
      v(2)=gfloat(2-j)*one
      theta=zero
      do 120 iop=1,2
      call put(MAXAP3,A,B,q,v,NATOMS,3)
      theta=theta+phi3
      call rotate(MAXAP3,A,B,NATOMS,r,3,theta)
      call matpac(r,q,s,3,3,1)
      do 110 k=1,3
      do 105 l=1,k
      if(k.NE.l)then
      abc=q(k,l)
      q(k,l)=q(l,k)
      q(l,k)=abc
      endif
105   continue
110   continue
      call matpac(q,s,t,3,3,1)
      call tform(MAXAP3,q,B,A,NATOMS)
      call fill(MAXAP3,NATOMS,NOP,MAXOP,t,TRANS,NPERM,D,A)
      call move(MAXAP3,D,A,NATOMS)
120   continue
150   continue
200   continue
      
      
      do 300 i=1,3
      v(i)=zero
      j=1+mod(i,3)
      v(j)=one
      k=1+mod(j,3)
      do 250 l=1,3,2
      v(k)=gfloat(2-l)*one
      call put(MAXAP3,A,B,q,v,NATOMS,3)
      call rotate(MAXAP3,A,B,NATOMS,r,3,pi)
      call matpac(r,q,s,3,3,1)
      do 220 i1=1,3
      do 210 i2=1,i1
      if(i2.NE.i1)then
      abc=q(i1,i2)
      q(i1,i2)=q(i2,i1)
      q(i2,i1)=abc
      endif
210   continue
220   continue
      call matpac(q,s,t,3,3,1)
      call tform(MAXAP3,q,B,A,NATOMS)
      call fill(MAXAP3,NATOMS,NOP,MAXOP,t,TRANS,NPERM,D,A)
      call move(MAXAP3,D,A,NATOMS)
250   continue
300   continue
      
      
      if(NGRP(2).EQ.iord('H'))then
      call invert(MAXAP3,D,B,NATOMS,t)
      call fill(MAXAP3,NATOMS,NOP,MAXOP,t,TRANS,NPERM,D,B)
      
      
      do 350 ixyz=1,3
      call reflct(MAXAP3,D,B,NATOMS,t,ixyz)
      call fill(MAXAP3,NATOMS,NOP,MAXOP,t,TRANS,NPERM,D,B)
350   continue
      
      
      do 400 ixyz=1,3
      do 360 iop=1,3
      if(iop.NE.ixyz)then
      call rotate(MAXAP3,D,B,NATOMS,q,ixyz,piovr4)
      call reflct(MAXAP3,B,A,NATOMS,r,iop)
      call matpac(r,q,s,3,3,1)
      call rotate(MAXAP3,A,B,NATOMS,r,ixyz,-piovr4)
      call matpac(r,s,t,3,3,1)
      call fill(MAXAP3,NATOMS,NOP,MAXOP,t,TRANS,NPERM,D,B)
      endif
360   continue
400   continue
      
      
      do 450 ixyz=1,3
      theta=-halfpi
      do 420 iop=1,2
      theta=theta+pi
      call rotate(MAXAP3,D,B,NATOMS,t,ixyz,theta)
      call reflct(MAXAP3,B,A,NATOMS,s,ixyz)
      call matpac(t,s,r,3,3,1)
      call fill(MAXAP3,NATOMS,NOP,MAXOP,r,TRANS,NPERM,D,A)
420   continue
450   continue
      
      
      call move(MAXAP3,D,A,NATOMS)
      v(3)=one
      do 500 i=1,3,2
      v(1)=gfloat(2-i)*one
      do 480 j=1,3,2
      v(2)=gfloat(2-j)*one
      do 460 iop=1,3,2
      theta=gfloat(2-iop)*phi6
      call put(MAXAP3,A,B,q,v,NATOMS,3)
      call rotate(MAXAP3,A,B,NATOMS,r,3,theta)
      call matpac(r,q,t,3,3,1)
      call reflct(MAXAP3,B,A,NATOMS,r,3)
      call matpac(r,t,s,3,3,1)
      do 455 k=1,3
      do 452 l=1,k
      if(l.NE.k)then
      abc=q(k,l)
      q(k,l)=q(l,k)
      q(l,k)=abc
      endif
452   continue
455   continue
      call matpac(q,s,t,3,3,1)
      call tform(MAXAP3,q,A,B,NATOMS)
      call fill(MAXAP3,NATOMS,NOP,MAXOP,t,TRANS,NPERM,D,B)
      call move(MAXAP3,D,A,NATOMS)
460   continue
480   continue
500   continue
      endif
      
      call move(MAXAP3,D,A,NATOMS+3)
      return
      
      end
C* :1 * 
      
