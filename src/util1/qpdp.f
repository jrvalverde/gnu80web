
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 qpdp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "qpdp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 21 "qpdp.web"
      integer function qpdp(LINE,LENGTH,LCURSR,DP)
      implicit none
      integer chr,esign,getchr,intchr,iord,LCURSR,LENGTH,LINE,Maxdep,Max
     &key,mycrsr,nsign,Qpabrv,Qpambg,Qpdpth,Qpend,Qperr,Qpexit,Qpfail,Qp
     &ok
      integer Qprecr,Qpret,Stack,t,val
      dimension LINE(*)
      double precision DP,expnt,whole,fract,ten,one,gfloat
      double precision teni,zero,fact
      logical gotone,ifalph
      common/qpretc/Qpok,Qpret,Qpfail,Qpambg,Qperr,Qpexit,Qpabrv,Qpend,Q
     &precr,Qpdpth,Maxdep,Stack(6,10),Maxkey
      data ten/10.0D0/,one/1.0D0/,zero/0.0D0/
      
      gotone=.FALSE.
      nsign=0
      esign=0
      whole=zero
      fract=zero
      expnt=zero
      mycrsr=LCURSR
      val=Qpfail
      if(LCURSR.LT.LENGTH)then
      
      chr=getchr(LINE,mycrsr)
      nsign=0
      if(chr.EQ.iord('-'))nsign=-1
      if(chr.EQ.iord('+'))nsign=+1
      if(nsign.EQ.0)mycrsr=LCURSR
      
50    chr=getchr(LINE,mycrsr)
      t=intchr(chr,10)
      if(t.EQ.-1)then
      
      call captlz(chr,chr,1)
      if(chr.EQ.iord('.'))then
      
      
      teni=one/ten
      fact=teni
      if(mycrsr.GE.LENGTH)goto 100
      
60    chr=getchr(LINE,mycrsr)
      t=intchr(chr,10)
      if(t.EQ.-1)then
      
      call captlz(chr,chr,1)
      if(chr.NE.iord('E').AND.chr.NE.iord('D'))then
      if(.NOT.(chr.EQ.iord('.').OR.ifalph(chr)))goto 100
      goto 200
      endif
      else
      gotone=.TRUE.
      fract=fract+fact*gfloat(t)
      fact=fact*teni
      if(mycrsr.LT.LENGTH)goto 60
      goto 100
      endif
      elseif(chr.NE.iord('E').AND.chr.NE.iord('D'))then
      goto 200
      endif
      
      if(.NOT.gotone)goto 200
      if(mycrsr.GE.LENGTH)goto 200
      esign=0
      chr=getchr(LINE,mycrsr)
      if(chr.EQ.iord('+'))esign=+1
      if(chr.EQ.iord('-'))esign=-1
      if(esign.EQ.0)mycrsr=mycrsr-1
      
      gotone=.FALSE.
      if(mycrsr.GE.LENGTH)goto 200
80    chr=getchr(LINE,mycrsr)
      t=intchr(chr,10)
      if(t.NE.-1)then
      gotone=.TRUE.
      expnt=ten*expnt+gfloat(t)
      if(mycrsr.LT.LENGTH)goto 80
      
      elseif(ifalph(chr).OR.chr.EQ.iord('.'))then
      goto 200
      endif
      else
      whole=ten*whole+gfloat(t)
      gotone=.TRUE.
      if(mycrsr.LT.LENGTH)goto 50
      goto 200
      endif
      
100   if(gotone)then
      LCURSR=mycrsr-1
      val=Qpok
      if(nsign.EQ.0)nsign=1
      if(esign.EQ.0)esign=1
      DP=(gfloat(nsign))*(whole+fract)*(ten**(gfloat(esign)*expnt))
      endif
      endif
200   qpdp=val
      return
      
      end
C* :1 * 
      
