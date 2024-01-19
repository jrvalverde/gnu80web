
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 gesopt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "gesopt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 47 "gesopt.web"
      subroutine gesopt
      implicit none
      double precision gfloat,pt4375,pt875,scale
      integer i,I56d,Ialt,Ibasis,Iblock,Icmp,Icmplt,iconst,Idgn,Idon1,Id
     &on2,Idump,Iguess,ii,Imix,In,Iop,Iopdmp,Iout,iphf
      integer Ipolh,Iprint,Iproj,Ipunch,ipurdf,ipurf,Iscale,Ismear,Itst,
     &Iuhf,Jdump,Jjdump,maxop
      dimension Iopdmp(19)
      common/iop/Iop(50)
      common/ops401/Iguess,Iproj,Iuhf,Icmp,Ialt,Imix,Idgn,Iscale,Ismear,
     &Iblock,Icmplt,Itst,Ibasis,Ipolh,Idon1,Idon2,Iprint,Idump,I56d
      common/dump/Jdump,Jjdump
      common/io/In,Iout,Ipunch
      equivalence(Iopdmp(1),Iguess)
      data pt875/0.875D0/
      data pt4375/0.4375D0/
      data maxop/19/
      
99001 format('  IOP(',i2,') OUT OF RANGE.')
99002 format('  /OPS401/:',25I4)
      
      Iguess=Iop(5)
      Iproj=Iop(6)+1
      iconst=Iop(7)-1
      Ialt=Iop(8)
      iphf=Iop(9)
      Imix=Iop(10)
      Iscale=Iop(12)
      Icmplt=Iop(16)
      Itst=Iop(18)
      Ibasis=Iop(22)-1
      ipurdf=Iop(23)-1
      Ipolh=Iop(24)-1
      Idon1=Iop(25)-1
      Idon2=Iop(26)-1
      Iprint=Iop(33)
      Idump=Iop(34)
      Jdump=Idump
      
      if(Idump.NE.0)Iprint=2
      I56d=0
      
      if(ipurdf.GE.0)then
      
      I56d=mod(ipurdf,2)
      ipurf=ipurdf/2
      call ilsw(1,2,I56d)
      call ilsw(1,16,ipurf)
      else
      call ilsw(2,2,I56d)
      call ilsw(2,16,ipurf)
      endif
      
      if(Ibasis.GE.0)then
      
      call ilsw(1,3,Ibasis)
      else
      call ilsw(2,3,Ibasis)
      endif
      if(Ipolh.GE.0)then
      
      call ilsw(1,10,Ipolh)
      else
      call ilsw(2,10,Ipolh)
      endif
      if(Idon1.GE.0)then
      
      call ilsw(1,11,Idon1)
      else
      call ilsw(2,11,Idon1)
      endif
      if(Idon2.GE.0)then
      
      call ilsw(1,12,Idon2)
      else
      call ilsw(2,12,Idon2)
      endif
      
      
      if(Iguess.EQ.0)then
      
      Iguess=1
      if(Ibasis.EQ.0)Iguess=2
      if((Ipolh+Idon1+Idon2).NE.0)Iguess=1
      else
      i=Iguess
      if(i.EQ.1)Iguess=4
      if(i.EQ.2)Iguess=3
      if(i.EQ.3)Iguess=2
      if(i.EQ.4)Iguess=1
      endif
      
      if(iconst.LT.0)then
      call ilsw(2,1,iconst)
      
      elseif(iconst.NE.4)then
      call ilsw(1,1,iconst)
      else
      call ilsw(2,1,Iuhf)
      Icmp=1
      iconst=2+Iuhf
      call ilsw(1,1,iconst)
      goto 100
      endif
      Iuhf=mod(iconst,2)
      Icmp=iconst/2
      
100   scale=pt875
      if(Iscale.NE.0)scale=pt4375*gfloat(Iscale)
      
      if(Ialt.EQ.2.AND.iphf.EQ.1)call geserr(4)
      
      Ialt=Ialt+1
      if(Ialt.GT.2)then
      i=8
      write(Iout,99001)i
      call lnk1e
      endif
      if(Iproj.GT.3)then
      i=6
      write(Iout,99001)i
      call lnk1e
      endif
      Icmplt=1-Icmplt
      if(Idump.NE.0)write(Iout,99002)(Iopdmp(ii),ii=1,maxop)
      
      return
      
      end
C* :1 * 
      
