
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 hybcmp"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "hybcmp.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "hybcmp.web"
      subroutine hybcmp(XYZ,PCENT,IHYB,JCTR,HYB)
      implicit none
      double precision cutoff,hnorm,HYB,PCENT,thresh,tmax,XYZ,zero
      integer IHYB,inao,Iprin,Ispin,ix,JCTR,jmax,l,Lfnao,Lfnarc,Lfndaf,L
     &fndef,Lfndm,Lfnin,Lfnmo,Lfnnab,Lfnnao,Lfnnbo,Lfnnho,Lfnnlm
      integer Lfnpna,Lfnpnb,Lfnpnh,Lfnpnl,Lfnppa,Lfnpr,Ltyp,m,MAXATM,MAX
     &BAS,Munit,Mxao,Mxaolm,Mxbo,Naoa,Naoc,Natoms,Nbas,Ndim
      dimension XYZ(3),HYB(1)
      
      parameter(MAXATM=99,MAXBAS=500)
      common/nbnao/Naoc(MAXBAS),Naoa(MAXBAS),Ltyp(MAXBAS),Iprin(MAXBAS)
      common/nbinfo/Ispin,Natoms,Ndim,Nbas,Mxbo,Mxao,Mxaolm,Munit
      common/nbio/Lfnin,Lfnpr,Lfnao,Lfnpna,Lfnnao,Lfnpnh,Lfnnho,Lfnpnb,L
     &fnnbo,Lfnpnl,Lfnnlm,Lfnmo,Lfndm,Lfnnab,Lfnppa,Lfnarc,Lfndaf,Lfndef
      
      data zero,thresh,cutoff/0.0D0,1.0D-4,1.0D-8/
      
      
      XYZ(1)=zero
      XYZ(2)=zero
      XYZ(3)=zero
      PCENT=zero
      hnorm=zero
      
      
      jmax=1
      tmax=dabs(HYB(1))
      do 100 inao=2,Nbas
      if(dabs(HYB(inao)).GT.tmax)then
      jmax=inao
      tmax=dabs(HYB(inao))
      endif
100   continue
      if(Naoc(jmax).NE.JCTR)then
      write(Lfnpr,99001)IHYB,JCTR,Naoc(jmax)
      stop
      endif
      
      
      jmax=0
      tmax=zero
      do 200 inao=1,Nbas
      l=Naoa(inao)/100
      if(l.EQ.0.AND.dabs(HYB(inao)).GT.tmax)then
      jmax=inao
      tmax=dabs(HYB(inao))
      endif
200   continue
      
      
      if(jmax.NE.0.AND.HYB(jmax).LT.-thresh)then
      do 250 inao=1,Nbas
      HYB(inao)=-HYB(inao)
250   continue
      endif
      
      
      do 300 inao=1,Nbas
      if(Naoc(inao).EQ.JCTR)then
      l=Naoa(inao)/100
      if(l.EQ.1)then
      PCENT=PCENT+HYB(inao)*HYB(inao)
      m=mod(Naoa(inao),50)
      XYZ(m)=XYZ(m)+HYB(inao)
      endif
      hnorm=hnorm+HYB(inao)*HYB(inao)
      endif
300   continue
      if(hnorm.LT.thresh)then
      write(Lfnpr,99002)JCTR,IHYB
      stop
      endif
      PCENT=PCENT/hnorm*100.0
      
      
      hnorm=zero
      do 400 ix=1,3
      if(dabs(XYZ(ix)).LT.cutoff)XYZ(ix)=zero
      hnorm=hnorm+XYZ(ix)*XYZ(ix)
400   continue
      hnorm=dsqrt(hnorm)
      if(dabs(hnorm).LT.cutoff)then
      PCENT=zero
      else
      do 450 ix=1,3
      XYZ(ix)=XYZ(ix)/hnorm
450   continue
      endif
      return
      
99001 format(/1x,'Expected to find hybrid ',i3,' on nuclear center ',i2,
     &' rather than center ',i2,'.')
99002 format(/1x,'The atomic orbitals on nuclear center ',i2,' do not ',
     &'contribute to hybrid ',i3,'.')
      end
C* :1 * 
      
