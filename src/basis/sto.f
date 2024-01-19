      subroutine sto(IVALEN,NATOMS,IAN,C,NBASIS,NGIC,IPT)
      implicit none
      double precision C,C1,C2,C3,C4,cd,cp,cs,e,Exx,f4p95,one,onep2,onep
     &7,Pdexp,Pdexp1,Pdexp2,Ppexp,pt325,pt65
      double precision rone,sc1s,sc2sp,sc3d,sc3sp,sc4d,sc4sp,sc5sp,Scale
     &,shlscl,three,X,xcoord,Y,ycoord,Z,zcoord,zero
      integer i,ia,IAN,idx,ih1s,ih2sp,ih3d,ih3sp,ih4d,ih4sp,ih5d,ih5sp,i
     &h6d,ihspd,iout,IPT,IVALEN,j,j1,j2
      integer Jan,maxan,MAXPRM,MAXS21,MAXSH1,MAXSHL,maxsto,Maxtyp,mm,mmd
     &f,NATOMS,NBASIS,ngauss,NGIC,Nshell,nstart,Numd
      
      
      dimension IAN(*),C(*)
      integer Shella,Shelln,Shellt,Shellc,Shladf,Aos,Aon
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/scale/Scale(MAXSHL)
      common/numd/Numd
      common/polexp/Ppexp,Pdexp,Pdexp1,Pdexp2
      dimension e(6),cs(6),cp(6),cd(6),sc1s(54),sc2sp(54),sc3sp(54),sc3d
     &(54),sc4sp(54),sc4d(54),sc5sp(54)
      data one,zero,rone/1.D0,0.D0,1.D0/,ih5d/2H5D/,ih6d/2H6D/
      data onep2/1.2D0/,onep7/1.7D0/,pt325/0.325D0/,pt65/0.65D0/,f4p95/4
     &.95D0/,three/3.0D0/,ih4sp/3H4SP/,maxan/54/
      data ih3d/3H3D /,ihspd/3HSPD/,ih4d/3H4D /,ih5sp/3H5SP/,ih1s/3H1S /
     &,ih2sp/3H2SP/,ih3sp/3H3SP/
      
      
      data sc1s/1.24D0,1.69D0,2.69D0,3.68D0,4.68D0,5.67D0,6.67D0,7.66D0,
     &8.65D0,9.64D0,10.61D0,11.59D0,12.56D0,13.53D0,14.50D0,15.47D0,16.4
     &3D0,17.40D0,18.61D0,19.58D0,20.56D0,21.54D0,22.53D0,23.52D0,24.50D
     &0,25.49D0,26.44D0,27.46D0,28.44D0,29.43D0,30.42D0,31.40D0,32.39D0,
     &33.37D0,34.36D0,35.34D0,36.32D0,37.31D0,38.29D0,39.27D0,40.26D0,41
     &.24D0,42.22D0,43.21D0,44.19D0,45.17D0,46.15D0,47.14D0,48.12D0,49.1
     &0D0,50.08D0,51.07D0,52.05D0,53.03D0/
      
      data sc2sp/0.00D0,0.00D0,0.80D0,1.15D0,1.50D0,1.72D0,1.95D0,2.25D0
     &,2.55D0,2.88D0,3.48D0,3.90D0,4.36D0,4.83D0,5.31D0,5.79D0,6.26D0,6.
     &74D0,7.26D0,7.74D0,8.22D0,8.70D0,9.18D0,9.66D0,10.13D0,10.61D0,11.
     &07D0,11.56D0,12.04D0,12.52D0,12.99D0,13.47D0,13.94D0,14.40D0,14.87
     &D0,15.34D0,15.81D0,16.28D0,16.72D0,17.19D0,17.66D0,18.12D0,18.59D0
     &,19.05D0,19.51D0,19.97D0,20.43D0,20.88D0,21.33D0,21.79D0,22.25D0,2
     &2.71D0,23.17D0,23.63D0/
      
      data sc3sp/6*0.D0,0.00D0,0.00D0,0.00D0,0.00D0,1.75D0,1.70D0,1.70D0
     &,1.75D0,1.90D0,2.05D0,2.10D0,2.33D0,2.75D0,3.01D0,3.21D0,3.44D0,3.
     &67D0,3.89D0,4.11D0,4.33D0,4.56D0,4.76D0,4.98D0,5.19D0,5.26D0,5.58D
     &0,5.90D0,6.22D0,6.54D0,6.86D0,7.18D0,7.49D0,7.97D0,8.21D0,8.51D0,8
     &.82D0,9.14D0,9.45D0,9.77D0,10.09D0,10.41D0,10.74D0,11.08D0,11.39D0
     &,11.71D0,12.03D0,12.35D0,12.66D0/
      
      data sc3d/18*0.00D0,0.00D0,0.00D0,1.10D0,1.90D0,2.55D0,3.05D0,3.45
     &D0,3.75D0,4.10D0,4.35D0,4.60D0,4.90D0,5.26D0,5.58D0,5.90D0,6.22D0,
     &6.54D0,6.86D0,7.18D0,7.49D0,7.97D0,8.21D0,8.51D0,8.82D0,9.14D0,9.4
     &5D0,9.77D0,10.09D0,10.41D0,10.74D0,11.08D0,11.39D0,11.71D0,12.03D0
     &,12.35D0,12.66D0/
      
      data sc4sp/18*0.0D0,1.43D0,1.36D0,1.60D0,1.70D0,1.70D0,1.75D0,1.65
     &D0,1.55D0,1.55D0,1.60D0,1.60D0,1.90D0,1.80D0,2.00D0,2.12D0,2.22D0,
     &2.38D0,2.54D0,3.02D0,3.16D0,3.29D0,3.48D0,3.67D0,3.87D0,4.05D0,4.2
     &4D0,4.41D0,4.59D0,4.76D0,4.93D0,4.65D0,4.89D0,5.12D0,5.36D0,5.59D0
     &,5.82D0/
      
      data sc4d/36*0.0D0,0.00D0,0.00D0,1.40D0,1.95D0,2.40D0,2.70D0,3.00D
     &0,3.20D0,3.45D0,3.60D0,3.75D0,3.95D0,4.65D0,4.89D0,5.12D0,5.36D0,5
     &.59D0,5.82D0/
      
      data sc5sp/36*0.00D0,1.90D0,1.80D0,1.80D0,1.90D0,1.90D0,1.95D0,1.8
     &5D0,1.75D0,1.75D0,1.80D0,1.80D0,2.10D0,2.05D0,2.15D0,2.20D0,2.28D0
     &,2.42D0,2.57D0/
      
      data maxsto/54/
99001 format(' Atomic number',i4,' out of range in subroutine STO.')
99002 format(' Atomic number',i4,' exceeds maximum of',i4,' in STO ')
      iout=6
      mm=1
      mmdf=1
      Nshell=0
      nstart=1
      ngauss=NGIC
      if(ngauss.EQ.0)ngauss=3
      call putlbl(0,0,-1)
      do 700 i=1,NATOMS
      ia=IAN(i)
      if(ia.EQ.0)goto 700
      if(ia.GT.maxsto)then
      write(iout,99002)ia,maxsto
      call lnk1e
      endif
      call putlbl(i,ia,0)
      idx=3*(i-1)
      xcoord=C(idx+1)
      ycoord=C(idx+2)
      zcoord=C(idx+3)
      if(IVALEN.NE.0)then
      if(ia.GT.2)then
      if(ia.LE.10)goto 100
      if(ia.LE.18)goto 200
      if(ia.GE.20.AND.ia.LE.30)goto 300
      if(ia.LE.36)goto 400
      if(ia.LE.54)goto 500
      write(iout,99001)ia
      call lnk1e
      endif
      endif
      
      call s1s(e,cs,ngauss)
      shlscl=sc1s(ia)
      do 50 j=1,ngauss
      j1=mm-1+j
      Exx(j1)=e(j)*shlscl*shlscl
      C1(j1)=cs(j)
      C2(j1)=zero
50    continue
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      Shelln(Nshell)=ngauss
      Shellt(Nshell)=0
      Scale(Nshell)=shlscl
      Aon(Nshell)=ih1s
      Aos(Nshell)=nstart
      call putlbl(1,0,1)
      nstart=nstart+1
      mm=mm+ngauss
      if(ia.LE.2)goto 700
100   call s2sp(e,cs,cp,ngauss)
      shlscl=sc2sp(ia)
      do 150 j=1,ngauss
      j1=mm-1+j
      Exx(j1)=e(j)*shlscl*shlscl
      C1(j1)=cs(j)
      C2(j1)=cp(j)
150   continue
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      Shelln(Nshell)=ngauss
      Shellt(Nshell)=1
      Scale(Nshell)=shlscl
      Aon(Nshell)=ih2sp
      Aos(Nshell)=nstart
      call putlbl(2,0,1)
      call putlbl(2,1,1)
      nstart=nstart+4
      mm=mm+ngauss
      if(ia.LE.10)goto 700
200   call s3sp(e,cs,cp,ngauss)
      shlscl=sc3sp(ia)
      do 250 j=1,ngauss
      j1=mm-1+j
      Exx(j1)=e(j)*shlscl*shlscl
      C1(j1)=cs(j)
      C2(j1)=cp(j)
250   continue
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      j2=mmdf+j-1
      Shelln(Nshell)=ngauss
      Shellt(Nshell)=1
      Scale(Nshell)=shlscl
      Aon(Nshell)=ih3sp
      Aos(Nshell)=nstart
      call putlbl(3,0,1)
      call putlbl(3,1,1)
      nstart=nstart+4
      mm=mm+ngauss
      if(ia.LE.18)goto 600
      if(ia.LE.20)goto 400
300   call s3d(e,cd,ngauss)
      shlscl=sc3d(ia)
      do 350 j=1,ngauss
      j1=mm-1+j
      j2=mmdf+j-1
      Exx(j1)=e(j)*shlscl*shlscl
      C1(j1)=zero
      C2(j1)=zero
      C3(j2)=cd(j)
      C4(j2)=zero
350   continue
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=mmdf
      Shelln(Nshell)=ngauss
      Shellt(Nshell)=2
      Shellc(Nshell)=2
      Scale(Nshell)=shlscl
      Aon(Nshell)=ih3d
      Aos(Nshell)=nstart-4
      call putlbl(3,2,1)
      nstart=nstart+Numd
      mm=mm+ngauss
      mmdf=mmdf+ngauss
400   call s4sp(e,cs,cp,ngauss)
      shlscl=sc4sp(ia)
      do 450 j=1,ngauss
      j1=mm-1+j
      Exx(j1)=e(j)*shlscl*shlscl
      C1(j1)=cs(j)
      C2(j1)=cp(j)
450   continue
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      j2=mmdf+j-1
      Shelln(Nshell)=ngauss
      Shellt(Nshell)=1
      Scale(Nshell)=shlscl
      Aon(Nshell)=ih4sp
      Aos(Nshell)=nstart
      call putlbl(4,0,1)
      call putlbl(4,1,1)
      nstart=nstart+4
      mm=mm+ngauss
      if(ia.LE.36)goto 600
      if(ia.GT.38)then
      call s4d(e,cd,ngauss)
      shlscl=sc4d(ia)
      do 460 j=1,ngauss
      j1=mm-1+j
      j2=mmdf+j-1
      Exx(j1)=e(j)*shlscl*shlscl
      C1(j1)=zero
      C2(j1)=zero
      C3(j2)=cd(j)
      C4(j2)=zero
460   continue
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=mmdf
      Shelln(Nshell)=ngauss
      Shellt(Nshell)=2
      Shellc(Nshell)=2
      Scale(Nshell)=shlscl
      Aon(Nshell)=ih4d
      Aos(Nshell)=nstart-4
      call putlbl(4,2,1)
      nstart=nstart+Numd
      mm=mm+ngauss
      mmdf=mmdf+ngauss
      endif
500   call s5sp(e,cs,cp,ngauss)
      shlscl=sc5sp(ia)
      do 550 j=1,ngauss
      j1=mm-1+j
      Exx(j1)=e(j)*shlscl*shlscl
      C1(j1)=cs(j)
      C2(j1)=cp(j)
550   continue
      Nshell=Nshell+1
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=0
      j2=mmdf+j-1
      Shelln(Nshell)=ngauss
      Shellt(Nshell)=1
      Scale(Nshell)=shlscl
      Aon(Nshell)=ih5sp
      Aos(Nshell)=nstart
      call putlbl(5,0,1)
      call putlbl(5,1,1)
      nstart=nstart+4
      mm=mm+ngauss
600   if(IPT.GT.0)then
      Nshell=Nshell+1
      if(Shellc(Nshell).EQ.0)call berror(1)
      X(Nshell)=xcoord
      Y(Nshell)=ycoord
      Z(Nshell)=zcoord
      Jan(Nshell)=ia
      Shella(Nshell)=mm
      Shladf(Nshell)=mmdf
      Shelln(Nshell)=1
      Shellt(Nshell)=2
      Scale(Nshell)=rone
      Aon(Nshell)=ih5d
      if(Numd.EQ.6)Aon(Nshell)=ih6d
      Exx(mm)=Pdexp1
      if(ia.GE.13)Exx(mm)=Pdexp2
      C1(mm)=zero
      C2(mm)=zero
      C3(mmdf)=one
      C4(mmdf)=zero
      mm=mm+1
      mmdf=mmdf+1
      Aos(Nshell)=nstart-4
      call putlbl(0,2,1)
      if(ia.GT.13)call putlbl(3,2,1)
      nstart=nstart+Numd
      endif
700   continue
      Aos(Nshell+1)=nstart
      NBASIS=nstart-1
      return
      end
      
