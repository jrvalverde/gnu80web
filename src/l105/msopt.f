
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 msopt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "msopt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 71 "msopt.web"
      subroutine msopt(JUMP)
      implicit none
      double precision Alph,Alpha,Beta,Bl,Delta1,Delta2,dmax,Epsiln,F,Fc
     &onv,Flast,Flowb,fmax,Frcnst,G,gabs,gfloat,Gkm1p,Glast,gmax1
      double precision Gnorm,gsqrt,P,Pc,Pnorm,Q,rmsd,rmsf,S,Stpmin,Thres
     &h,Toang,X,xd,xdispl,xf,Xlast,Xname,Z,zero
      integer i,iad2,Ianz,Idnts,idx,Ierr,Igo,igrd,Iguess,ihdg,In,iogrd,i
     &oms,Iop,iotest,Iout,iozmat,Ipunch,is,Istep
      integer Istype,itype,Iz,j,JUMP,Lalpha,Lbeta,Lbl,lgrd,lms,ltest,lzm
     &at,Ndum,Ndum2,Ndumm,nel,Nstep,Nvar,Nvarrd,Nz
      logical contst,Calcfc
      dimension xdispl(50),itype(50)
      dimension is(100),iad2(50)
      common/iop/Iop(50)
      common/phycon/Toang,Pc(29)
      common/ctests/Alpha,Delta1,Delta2,Epsiln,Stpmin,Fconv,Flowb,Gkm1p,
     &Pnorm,Gnorm,Idnts,Iguess,Ierr,Igo,Istype,Ndum
      common/grdnt/F,G(50),Frcnst(1275),Nvar,Ndumm
      common/msinfo/Flast,X(50),Xlast(50),Xname(100),Glast(50),S(50,50),
     &P(50),Q(50),Z(50),Istep,Nstep,Thresh,Calcfc,Ndum2
      common/io/In,Iout,Ipunch
      common/zmat/Ianz(50),Iz(50,4),Bl(50),Alph(50),Beta(50),Lbl(50),Lal
     &pha(50),Lbeta(50),Nz,Nvarrd
      data zero/0.0D0/
      data itype/50*98/
      data iogrd,lgrd,ioms,lms,iotest,ltest,iozmat,lzmat/511,1327,574,29
     &04,576,13,507,351/
      
      
99001 format(1H0,36(2HMS)/1H0,'MURTAUGH-SARGENT OPTIMIZATION')
99002 format(1H0,'INITIALIZATION PASS')
99003 format(1H0,36(2HMS)/)
99004 format(1H0,'CONVERGANCE UNLIKELY -- IERR=+1'/)
      
      
      call drum
      write(Iout,99001)
      
      
      call ilsw(2,23,igrd)
      if(igrd.NE.0)then
      
      
      
      
      call tread(iogrd,F,lgrd,1,lgrd,1,0)
      call tread(ioms,Flast,lms,1,lms,1,0)
      call tread(iotest,Alpha,ltest,1,ltest,1,0)
      Istep=Istep+1
      do 50 i=1,Nvar
      G(i)=-G(i)
50    continue
      
      
      if(Calcfc)then
      Calcfc=.FALSE.
      call analfc(Nvar,Frcnst,S)
      endif
      
      call nextx(X,Xlast,G,Glast,S,P,Q,Z,F,Flast,Nvar,50)
      
      
      rmsf=zero
      fmax=zero
      rmsd=zero
      dmax=zero
      do 100 i=1,Nvar
      xdispl(i)=X(i)-Xlast(i)
      xd=gabs(xdispl(i))
      dmax=gmax1(dmax,xd)
      rmsd=rmsd+xd*xd
      xf=gabs(G(i))
      fmax=gmax1(fmax,xf)
      rmsf=rmsf+xf*xf
100   continue
      rmsf=gsqrt(rmsf/gfloat(Nvar))
      rmsd=gsqrt(rmsd/gfloat(Nvar))
      
      
      call msprnt(Nvar,Istep,Nstep,X,Xlast,xdispl,G,S,Xname)
      
      
      if(Ierr.EQ.+1)then
      write(Iout,99004)
      write(Iout,99003)
      call lnk1e
      endif
      
      
      if(.NOT.contst(Iop,Istep,Nvar,rmsf,fmax,rmsd,dmax,Nstep,Thresh,ihd
     &g))then
      
      
      
      do 120 i=1,Nvar
      G(i)=-G(i)
120   continue
      call twrite(ioms,Flast,lms,1,lms,1,0)
      call twrite(iotest,Alpha,ltest,1,ltest,1,0)
      call twrite(iogrd,F,lgrd,1,lgrd,1,0)
      call udms(X,Nvar)
      write(Iout,99003)
      JUMP=0
      else
      call tread(iozmat,Ianz,lzmat,1,lzmat,1,0)
      call prmtbl(ihdg,Xname,Xlast,itype,G,Nvar,Lbl,Nz,Toang)
      write(Iout,99003)
      
      
      if(Iop(32).NE.0)then
      nel=200
      call binwt(Xname,nel,16HVARIABLE NAMES  ,0)
      call inv(S,Nvar,is,itype,iad2,xdispl,50)
      idx=0
      do 130 i=1,Nvar
      do 125 j=1,i
      idx=idx+1
      Frcnst(idx)=S(i,j)
125   continue
130   continue
      nel=Nvar*(Nvar+1)
      call binwt(Frcnst,nel,16HFORCE CONSTANTS ,0)
      endif
      JUMP=1
      endif
      else
      write(Iout,99002)
      call ilsw(1,23,1)
      call initms(Iop,Xname,X,Nvar,Istep,Nstep,S,Thresh,Calcfc,Toang,Frc
     &nst)
      call tests
      call twrite(ioms,Flast,lms,1,lms,1,0)
      call twrite(iotest,Alpha,ltest,1,ltest,1,0)
      call twrite(iogrd,F,lgrd,1,lgrd,1,0)
      write(Iout,99003)
      JUMP=0
      endif
      
      
      return
      
      end
C* :1 * 
      
