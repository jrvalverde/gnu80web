
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 gesprt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "gesprt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "gesprt.web"
      subroutine gesprt(IPRT,A,INC,MDIM,NBASIS,MBASIS)
      implicit none
      double precision A
      integer I56d,Ialt,Ibasis,Iblock,Icmp,Icmplt,Idgn,Idon1,Idon2,Idump
     &,Iguess,Imix,In,INC,Iout,Ipolh,Iprint,Iproj,IPRT,Ipunch
      integer Irtcrd,irwlbl,Iscale,Ismear,Ititle,Itst,Iuhf,Label,lc,line
     &,lrwlbl,MBASIS,MDIM,NBASIS
      dimension A(*)
      dimension line(30)
      common/io/In,Iout,Ipunch
      common/ops401/Iguess,Iproj,Iuhf,Icmp,Ialt,Imix,Idgn,Iscale,Ismear,
     &Iblock,Icmplt,Itst,Ibasis,Ipolh,Idon1,Idon2,Iprint,Idump,I56d
      common/label/Label(1000),Ititle(100),Irtcrd(100)
      data irwlbl/502/,lrwlbl/600/
      
      
      
      
      
      
      
      
99001 format('  TRANSFORMATION MATRIX.')
99002 format('  MINIMAL BASIS OVERLAP.')
99003 format('  MINIMAL BASIS TRANSFORMATION MATRIX.')
99004 format('  PROJECTION MATRIX.')
99005 format('  HUCKEL MATRIX  IOS =',i5)
99006 format('  CORE HAMILTONIAN.')
99007 format('  MO COEFFICIENTS BEFORE PROJECTION.')
99008 format('  OVERLAP BETWEEN THE BASIS SETS.')
      
      
      call tread(irwlbl,Label,lrwlbl,1,lrwlbl,1,0)
      
      lc=0
      if(INC.EQ.0)call putbc('REAL ALPHA ',11,line,lc)
      if(INC.EQ.1)call putbc('IMAGINARY ALPHA ',16,line,lc)
      if(INC.EQ.2)call putbc('REAL BETA ',10,line,lc)
      if(INC.EQ.3)call putbc('IMAGINARY BETA ',15,line,lc)
      if(INC.EQ.4)call putbc('INITIAL GUESS ',14,line,lc)
      
      if(IPRT.EQ.2)then
      
      write(Iout,99002)
      elseif(IPRT.EQ.3)then
      
      write(Iout,99003)
      elseif(IPRT.EQ.4)then
      
      write(Iout,99004)
      call matprt(A,MDIM,MDIM,NBASIS,NBASIS,0,0,0,0,0,0.,0)
      return
      elseif(IPRT.EQ.5)then
      
      write(Iout,99005)INC
      if(Iguess.NE.1)goto 100
      elseif(IPRT.EQ.6)then
      
      write(Iout,99006)
      goto 100
      elseif(IPRT.EQ.7)then
      
      write(Iout,99007)
      elseif(IPRT.EQ.8)then
      
      call putbc('DENSITY MATRIX.',15,line,lc)
      call strout(Iout,line,lc,1)
      goto 100
      elseif(IPRT.EQ.9)then
      
      call putbc('MO COEFFICIENTS.',16,line,lc)
      call strout(Iout,line,lc,1)
      goto 100
      elseif(IPRT.EQ.10)then
      
      call putbc('MO COEFFICIENTS.',16,line,lc)
      call strout(Iout,line,lc,1)
      call matprt(A,MDIM,MDIM,NBASIS,NBASIS,2,0,Label,0,0,0.,0)
      return
      elseif(IPRT.EQ.11)then
      
      call putbc('MO COEFFICIENTS READ IN.',24,line,lc)
      call strout(Iout,line,lc,1)
      elseif(IPRT.EQ.12)then
      
      call putbc('DENSITY MATRIX READ IN.',23,line,lc)
      call strout(Iout,line,lc,1)
      elseif(IPRT.EQ.13)then
      
      write(Iout,99008)
      elseif(IPRT.EQ.14)then
      
      write(Iout,99008)
      goto 200
      else
      
      write(Iout,99001)
      
      
      call matprt(A,MDIM,MDIM,NBASIS,NBASIS,0,0,0,0,0,0.,0)
      return
      endif
      
      call matprt(A,MDIM,MDIM,MBASIS,MBASIS,0,0,0,0,0,0.,0)
      return
      
100   call matprt(A,MDIM,MDIM,NBASIS,MBASIS,2,0,Label,0,0,0.,0)
      return
      
200   call matprt(A,MDIM,MDIM,NBASIS,MBASIS,2,0,Label,0,0,0.,0)
      return
      
      end
C* :1 * 
      
