
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 leevao"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "leevao.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 25 "leevao.web"
      subroutine leevao(NATOMS,IAN,C,NBASIS)
      implicit none
      double precision C,C1,C2,C3,C4,cp2,cp3,cs1,cs2,cs3,Dexpb,Dexpbe,De
     &xpli,e1,e2,e3,Elibeb,Exx,one,Pdexp
      double precision Pdexp1,Pdexp2,Ppexp,rone,Scal1,Scal1x,Scal2,Scal2
     &x,Scal3,Scal3x,Scal4,Scal4x,Scale,X,xcoord,Y,ycoord,Z,zcoord,zero
      integer i,I2edsc,I2esf,I5d6d,ia,IAN,Iao,Iaos,Ibasis,Ibmod,Ibpr,Ico
     &unt,idx,iflag,ind,iord,Iosc,Ipt,Irot,Iscal
      integer iscon,j,j1,Jan,Jpunch,Llink,MAXPRM,MAXS21,MAXSH1,MAXSHL,Ma
     &xtyp,mm,mmdf,NATOMS,NBASIS,ngauss,Ngic,ngin,ngout,nin
      integer nout,Nshell,nsplit,nstart,Numd,nup,nup1
      integer Shella,Shelln,Shellt,Shellc,Shladf,Aos,Aon
      dimension C(*),IAN(*)
      dimension Elibeb(3)
      dimension e1(6),e2(6),e3(6),cs1(6),cs2(6),cs3(6),cp2(6),cp3(6)
      dimension nin(5),nout(5)
      common/scalsp/Scal1(35),Scal1x(35),Scal2(35),Scal2x(35),Scal3(35),
     &Scal3x(35),Scal4(35),Scal4x(35),Iscal(35),Icount
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/scale/Scale(MAXSHL)
      common/iao/Iao(4)
      common/iaos/Iaos(MAXSHL)
      common/polexp/Ppexp,Pdexp,Pdexp1,Pdexp2
      common/numd/Numd
      common/libeb/Dexpli,Dexpbe,Dexpb
      common/ops301/Ibasis,Ngic,Ipt,I5d6d,Iosc,Ibmod,Ibpr,Llink,I2edsc,I
     &rot,Jpunch,I2esf
      equivalence(Dexpli,Elibeb(1))
      data nin/3,4,5,3,4/
      data nout/1,1,1,2,2/
      data zero/0.0D0/
      data one/1.0D0/
      data rone/1.0D0/
      
      
      
      
      
      
      
      
      
      mm=1
      mmdf=1
      Nshell=0
      nstart=1
      ngauss=Ngic
      if(ngauss.EQ.0)ngauss=3
      if(Ngic.EQ.8)iflag=1
      nsplit=1
      ngin=nin(nsplit)
      ngout=nout(nsplit)
      call putlbl(0,0,-1)
      
      do 200 i=1,NATOMS
      ia=IAN(i)
      idx=3*(i-1)
      xcoord=C(idx+1)
      ycoord=C(idx+2)
      zcoord=C(idx+3)
      call putlbl(i,ia,0)
      
      if(ia.LE.2)then
      call ezero(e1,cs1,ngauss,nsplit,ia)
      do 20 j=1,ngin
      j1=mm-1+j
      Exx(j1)=e1(j)*Scal1(i)*Scal1(i)
      C1(j1)=cs1(j)
      C2(j1)=zero
20    continue
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      Shelln(Nshell)=ngin
      Shellt(Nshell)=0
      Scale(Nshell)=Scal1(i)
      Aon(Nshell)=Iao(1)
      Aos(Nshell)=nstart
      call putlbl(1,0,2)
      Iaos(Nshell)=nstart
      nstart=nstart+1
      mm=mm+ngin
      do 40 j=1,ngout
      j1=mm-1+j
      Exx(j1)=e1(j+ngin)*Scal1x(i)*Scal1x(i)
      C1(j1)=cs1(j+ngin)
      C2(j1)=zero
40    continue
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      Shelln(Nshell)=ngout
      Shellt(Nshell)=0
      Scale(Nshell)=Scal1x(i)
      Aon(Nshell)=Iao(1)
      Aos(Nshell)=nstart
      call putlbl(1,0,4)
      Iaos(Nshell)=nstart
      nstart=nstart+1
      mm=mm+ngout
      if(Ipt.EQ.2)then
      Nshell=Nshell+1
      Shellc(Nshell)=1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      Shelln(Nshell)=1
      Shellt(Nshell)=1
      Aon(Nshell)=iord('   P')
      Scale(Nshell)=rone
      Exx(mm)=Ppexp
      C1(mm)=zero
      C2(mm)=one
      Iaos(Nshell)=nstart
      mm=mm+1
      Aos(Nshell)=nstart-1
      call putlbl(0,1,1)
      nstart=nstart+3
      endif
      else
      
      if(ia.LE.10)then
      
      call lpeone(e2,cs2,cp2,ia)
      nup=ngauss
      if(iflag.EQ.1)then
      if(ia.LE.5)then
      nup=3
      else
      
      nup=4
      endif
      endif
      do 50 j=1,nup
      j1=mm-1+j
      Exx(j1)=e2(j)*Scal2(i)*Scal2(i)
      C1(j1)=cs2(j)
      C2(j1)=cp2(j)
50    continue
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      Shelln(Nshell)=nup
      Shellt(Nshell)=1
      Scale(Nshell)=Scal2(i)
      Aon(Nshell)=Iao(2)
      Aos(Nshell)=nstart
      call putlbl(2,0,2)
      call putlbl(2,1,2)
      Iaos(Nshell)=nstart
      nstart=nstart+4
      mm=mm+nup
      nup1=nup
      nup=1
      do 60 j=1,nup
      j1=mm-1+j
      Exx(j1)=e2(j+nup1)*Scal2x(i)*Scal2x(i)
      C1(j1)=cs2(j+nup1)
      C2(j1)=cp2(j+nup1)
60    continue
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      Shelln(Nshell)=nup
      Shellt(Nshell)=1
      Scale(Nshell)=Scal2x(i)
      Aon(Nshell)=Iao(2)
      Aos(Nshell)=nstart
      call putlbl(2,0,4)
      call putlbl(2,1,4)
      Iaos(Nshell)=nstart
      nstart=nstart+4
      mm=mm+nup
      
      elseif(ia.LE.18)then
      
      call lpetwo(e3,cs3,cp3,ia)
      nup=ngauss
      if(iflag.EQ.1)then
      if(ia.LE.13)then
      nup=3
      else
      
      nup=4
      endif
      endif
      do 70 j=1,nup
      j1=mm-1+j
      Exx(j1)=e3(j)*Scal3(i)*Scal3(i)
      C1(j1)=cs3(j)
      C2(j1)=cp3(j)
70    continue
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      Shelln(Nshell)=nup
      Shellt(Nshell)=1
      Scale(Nshell)=Scal3(i)
      Aon(Nshell)=Iao(3)
      Aos(Nshell)=nstart
      call putlbl(3,0,2)
      call putlbl(3,1,2)
      Iaos(Nshell)=nstart
      nstart=nstart+4
      mm=mm+nup
      nup1=nup
      nup=1
      do 80 j=1,ngout
      j1=mm-1+j
      Exx(j1)=e3(j+nup1)*Scal3x(i)*Scal3x(i)
      C1(j1)=cs3(j+nup1)
      C2(j1)=cp3(j+nup1)
80    continue
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      Shelln(Nshell)=nup
      Shellt(Nshell)=1
      Scale(Nshell)=Scal3x(i)
      Aon(Nshell)=Iao(3)
      Aos(Nshell)=nstart
      call putlbl(3,0,4)
      call putlbl(3,1,4)
      Iaos(Nshell)=nstart
      nstart=nstart+4
      mm=mm+nup
      else
      stop
      endif
      if(Ipt.EQ.0)goto 200
      Nshell=Nshell+1
      Iaos(Nshell)=nstart
      iscon=Shellc(Nshell)+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=mmdf
      Shelln(Nshell)=1
      Shellt(Nshell)=2
      Scale(Nshell)=rone
      Aon(Nshell)=iord('D(6)')
      if(Numd.EQ.5)Aon(Nshell)=iord('D(5)')
      if(ia.LE.5)then
      if(Ibmod.LT.2)then
      ind=ia-2
      Exx(mm)=Elibeb(ind)
      goto 100
      endif
      endif
      
      if(ia.LT.11)then
      Exx(mm)=Pdexp
      
      elseif(ia.LE.12)then
      Exx(mm)=Pdexp1
      else
      
      Exx(mm)=Pdexp2
      endif
100   C1(mm)=one
      C2(mm)=one
      C3(mmdf)=one
      C4(mmdf)=zero
      mm=mm+1
      mmdf=mmdf+1
      if(iscon.EQ.2.OR.iscon.EQ.3)then
      C1(mm-1)=zero
      C2(mm-1)=zero
      Aos(Nshell)=nstart-4
      call putlbl(0,2,1)
      nstart=nstart+Numd
      else
      Aos(Nshell)=nstart
      call putlbl(3,0,1)
      call putlbl(3,1,1)
      call putlbl(3,2,1)
      nstart=nstart+Numd+4
      endif
      endif
      
200   continue
      Aos(Nshell+1)=nstart
      Iaos(Nshell+1)=nstart
      NBASIS=nstart-1
      return
      
      end
C* :1 * 
      
