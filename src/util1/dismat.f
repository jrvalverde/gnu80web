
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 dismat"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "dismat.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "dismat.web"
      subroutine dismat(NATOMS,IAN,C,IFLAG,NCOLX,IOUT,ERROR,KILL,CONVER)
      implicit none
      double precision C,CONVER,gsqrt,one,s,scale,temp,thresh,tooclo,zer
     &o
      integer i,ia,IAN,icol,iel,iend,IFLAG,IOUT,ir,irange,irow,isav,ista
     &rt,jcol,jrow,jsav,KILL,kount,m,NATOMS
      integer ncmax,ncol,NCOLX
      logical ERROR
      dimension C(*),s(11),iel(87),IAN(*),isav(20),jsav(20)
      data tooclo/0.5D0/
      data thresh/1.0D-6/,zero/0.0D0/,one/1.0D0/
      data ncmax/11/
      data iel/'  Bq','   H','  He','  Li','  Be','   B','   C','   N','
     &   O','   F','  Ne','  Na','  Mg','  Al','  Si','   P','   S','  C
     &l','  Ar','   K','  Ca','  Sc','  Ti','   V','  Cr','  Mn','  Fe',
     &'  Co','  Ni','  Cu','  Zn','  Ga','  Ge','  As','  Se','  Br','  
     &Kr','  Rb','  Sr','   Y','  Zr','  Nb','  Mo','  Tc','  Ru','  Rh'
     &,'  Pd','  Ag','  Cd','  In','  Sn','  Sb','  Te','   I','  Xe',' 
     & Cs','  Ba','  La','  Ce','  Pr','  Nd','  Pm','  Sm','  Eu','  Gd
     &','  Tb','  Dy','  Ho','  Er','  Tm','  Yb','  Lu','  Hf','  Ta','
     &   W','  Re','  Os','  Ir','  Pt','  Au','  Hg','  Tl','  Pb','  B
     &i','  Po','  At','  Rn'/
      
      
      
      
99001 format(4x,11(8x,i3))
99002 format(i3,a4,11F11.6)
99003 format(' SMALL INTERATOMIC DISTANCES ENCOUNTERED:',(/1x,2I5))
      
      ncol=NCOLX
      if(NCOLX.GT.ncmax)ncol=ncmax
      
      if(IFLAG.EQ.2)then
      
      scale=CONVER
      elseif(IFLAG.EQ.3)then
      
      scale=one/CONVER
      tooclo=tooclo*scale
      else
      
      scale=one
      endif
      istart=1
      kount=0
      ERROR=.FALSE.
100   m=0
      iend=min0(istart+ncol-1,NATOMS)
      if(IOUT.NE.0)write(IOUT,99001)(ir,ir=istart,iend)
      do 200 irow=istart,NATOMS
      ia=IAN(irow)+1
      m=m+1
      irange=min0(m,ncol)
      icol=istart
      do 150 ir=1,irange
      jrow=(irow-1)*3
      jcol=(icol-1)*3
      temp=(C(1+jrow)-C(1+jcol))**2+(C(2+jrow)-C(2+jcol))**2+(C(3+jrow)-
     &C(3+jcol))**2
      s(ir)=zero
      if(temp.GT.thresh)s(ir)=scale*gsqrt(temp)
      if(KILL.NE.1.AND.irow.NE.icol)then
      if(s(ir).LT.tooclo)then
      ERROR=.TRUE.
      if(kount.LT.20)then
      kount=kount+1
      isav(kount)=irow
      jsav(kount)=icol
      endif
      endif
      endif
      icol=icol+1
150   continue
      if(IOUT.NE.0)write(IOUT,99002)irow,iel(ia),(s(ir),ir=1,irange)
200   continue
      istart=istart+ncol
      if(istart.LE.NATOMS)goto 100
      if(ERROR.AND.IOUT.NE.0)write(IOUT,99003)(isav(i),jsav(i),i=1,kount
     &)
      return
      
      end
C* :1 * 
      
