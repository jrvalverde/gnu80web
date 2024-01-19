
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 symasg"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "symasg.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 29 "symasg.web"
      subroutine symasg(A,B,AA,ORBSYM,TABLE,NBASIS,NCOL,NAE,NBE,NATOMS,I
     &FGUES)
      implicit none
      integer i,IFGUES,In,inc,Iout,Ipunch,irwc,irwca,irwcb,irwrep,irws,i
     &rwsy1,irwsya,irwsyb,iuhf,len,lenf,lrwrep,NAE,NATOMS
      integer NBASIS,NBE,NCOL,nelect,nosym
      double precision Symops,Chrtbl
      integer Nsymop,Nreps,Lblrep,Iprmut
      double precision A(*),AA(*),B(*),TABLE(*)
      integer ORBSYM(*)
      common/io/In,Iout,Ipunch
      common/repcom/Nsymop,Nreps,Lblrep(32),Chrtbl(10,16),Symops(9,10),I
     &prmut(100,10)
      data irwrep/562/,lrwrep/767/
      data irws/514/,irwca/524/,irwcb/526/
      data irwsya/563/,irwsyb/564/
      
      call tquery(irwrep,lenf)
      if(lenf.LE.0)return
      
      irwc=irwca
      irwsy1=irwsya
      inc=0
      nelect=NAE
      call ilsw(2,1,iuhf)
      iuhf=mod(iuhf,2)
      
99001 format(' ORBITAL SYMMETRIES.')
99002 format('   ALPHA ORBITALS')
99003 format('   BETA ORBITALS')
99004 format(' INITIAL GUESS ORBITAL SYMMETRIES.')
      
      call tread(irwrep,Nsymop,lrwrep,1,lrwrep,1,0)
      if(Nsymop.EQ.0)return
      if(Nreps.EQ.0)return
      if(IFGUES.EQ.0)write(Iout,99001)
      if(IFGUES.NE.0)write(Iout,99004)
      
      nosym=0
100   call bldtbl(A,B,TABLE,NBASIS,NCOL,AA,irwc,irws,NATOMS,nosym)
      
      call getrep(TABLE,ORBSYM,NCOL,0,nosym)
      
      if(iuhf.NE.0.AND.inc.EQ.0)write(Iout,99002)
      if(iuhf.NE.0.AND.inc.EQ.1)write(Iout,99003)
      call prtsym(ORBSYM,nelect,NCOL,Iout)
      
      do 200 i=1,NCOL
      ORBSYM(i)=0
200   continue
      if(nosym.EQ.0)call getrep(TABLE,ORBSYM,NCOL,1,nosym)
      len=(NBASIS-1)/2+1
      call twrite(irwsy1,AA,len,1,len,1,0)
      len=(NCOL-1)/2+1
      call twrite(irwsy1,ORBSYM,len,1,len,1,0)
      
      if(iuhf.EQ.0)return
      if(inc.EQ.1)return
      inc=1
      irwc=irwcb
      irwsy1=irwsyb
      nelect=NBE
      goto 100
      
      end
C* :1 * 
      
