
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 filrec"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "filrec.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "filrec.web"
      subroutine filrec(SS,S,AOSA,AOSB,ISDIM)
      implicit none
      integer i,iaosa,iaosb,Iend,Ildum,Imj,intc,Ipurd,Ipurf,Irange,ISDIM
     &,Istart,ix,j,Jend,Jrange,Jstart,jx,Lamax,Lbmax
      integer Lbound,Lentq,Lpmax,Maxdum,Ndat,Nordr
      double precision S,SS
      integer AOSA,AOSB
      integer Ubound,Ulpure
      dimension SS(*)
      dimension S(ISDIM,ISDIM)
      common/limit/Imj,Istart,Jstart,Iend,Jend,Irange,Jrange,Lentq,Ildum
     &(11)
      common/ipure/Ipurd,Ipurf
      common/order/Nordr(20),Ndat(36),Lbound(4,3),Ubound(4),Ulpure(4)
      common/max/Lamax,Lbmax,Lpmax,Maxdum(4)
      
      
      
      if(Ipurd*Ipurf.EQ.0)call purdf1(SS)
      intc=0
      iaosa=AOSA-1
      iaosb=AOSB-1
      do 100 i=Istart,Iend
      ix=Nordr(i)+iaosa
      do 50 j=Jstart,Jend
      jx=Nordr(j)+iaosb
      intc=intc+1
      S(ix,jx)=SS(intc)
50    continue
100   continue
      
      Iend=Ubound(Lamax)
      Irange=Iend-Istart+1
      return
      
      end
C* :1 * 
      
