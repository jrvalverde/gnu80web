
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cids4"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cids4.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 29 "cids4.web"
      subroutine cids4(JUMP)
      implicit none
      double precision A00,Anorm,Atmchg,C,Cuts,Dehf,Delmax,Den,den1,den2
     &,den3,Energy,F42,Four,Half,One,Onept5,scalp,Ten,Three
      double precision Two,V,W0,Zero
      integer i,Iad1,Iad2,Iad3,Ian,Ias1,Ias2,Ibckt,Icharg,Icivar,Idb1,Id
     &b10,Idb2,Idb3,Idb4,Idb5,Idb6,Idb7,Idb8,Idb9
      integer ideriv,Idmm,Idummy,Idump,Ieval,Iextrp,Iflag,Igeno,In,Infor
     &b,Ioab,Iop,Iopcl,Iout,Ipcyc,Iprint,Ipunch,Iscr1,Iscr2,Iscr3
      integer Iscr4,Iscr5,Iscr6,Iscrd,Isd,Ispect,Iw1sav,Iw2sav,Iwd1,Iwd2
     &,Iwd3,Iws1,Iws2,JUMP,Lcivar,Lextrp,Ligen,Lnforb,Loab,Lspect
      integer Maxbuc,Maxit,Mdv,Multip,Nae,Natoms,Nbasis,Nbe,Ne,Niter,Noa
     &,Noa2,Noa3,Noaob,Noava,Noavb,Nob,Nob2,Nob3,Nobuc
      integer nobuc1,nobuc9,Nobva,Nobvb,Norm,Novaa,Novab,Novbb,Nrorb,Nva
     &,Nva2,Nva3,Nvavb,Nvb,Nvb2,Nvb3
      logical Davail,Savail
      dimension Ibckt(50)
      common/iop/Iop(50)
      common/mol/Natoms,Icharg,Multip,Nae,Nbe,Ne,Nbasis,Ian(101),Atmchg(
     &100),C(300)
      common/v/V(20000),Mdv
      common/rwfp/Igeno,Ligen,Inforb,Lnforb,Icivar,Lcivar,Iextrp,Lextrp
      common/orb/Ispect,Lspect,Nrorb,Noa,Nva,Nob,Nvb,Noaob,Noava,Noavb,N
     &obva,Nobvb,Nvavb,Noa2,Noa3,Nob2,Nob3,Nva2,Nva3,Nvb2,Nvb3,Novaa,Nov
     &ab,Novbb,Maxbuc,Ieval,Ioab,Loab
      common/civar/A00,Anorm,W0,Den,Energy,Dehf,Cuts,Delmax,Maxit,Ipcyc,
     &Norm,Isd,Iflag,Davail,Savail,Niter
      common/nobuc/Nobuc
      common/bucknr/Idb1,Idb2,Idb3,Idb4,Idb5,Idb6,Idb7,Idb8,Idb9,Idb10,I
     &dmm(11),Iad1,Iad2,Iad3,Ias1,Ias2,Iwd1,Iwd2,Iwd3,Iws1,Iws2,Iscr1,Is
     &cr2,Iscr3,Iscr4,Iscr5,Iscr6,Iscrd(13)
      common/wsav/Iw1sav,Iw2sav
      common/const/Zero,Half,One,Onept5,Two,Three,Four,Ten,F42
      common/constr/Iopcl
      common/io/In,Iout,Ipunch
      common/print/Iprint
      common/dump/Idump,Idummy
      equivalence(Ibckt(1),Idb1)
      
      
      
      
      
      
      
      
99001 format(34x,'CONTRIBUTION FROM DD2=',d18.8)
99002 format(34x,'CONTRIBUTION FROM DD3=',d18.8)
99003 format(34x,'SINGLES ADDED:    SUM=',d18.8)
      
      
      ideriv=Iop(15)
      Iprint=Iop(33)
      Idump=Iop(34)
      
      call ilsw(2,1,Iopcl)
      
      call tread(Inforb,Ispect,Lnforb,1,Lnforb,1,0)
      call tread(Icivar,A00,Lcivar,1,Lcivar,1,0)
      den1=Den
      
      do 100 i=1,Nobuc
      Ibckt(i)=i
100   continue
      
      nobuc1=Nobuc+1
      nobuc9=Nobuc+9
      do 200 i=nobuc1,nobuc9
      Ibckt(i)=3000+i
200   continue
      
      if(Iopcl.EQ.0)then
      Ias2=Ias1
      Iws2=Iws1
      Iad3=Iad1
      Iwd3=Iwd1
      Idb3=Idb1
      endif
      
      if(Davail)then
      
      
      call dd2
      
      if(Iopcl.EQ.0)call aaclos(Iwd2,Iwd1,Noa,Nva)
      
      if(ideriv.GT.0)call trsfr(Novab,Iwd2,Iw2sav)
      if(ideriv.GT.0)call sumn(Iw1sav,Iw2sav,Novab,-One)
      
      if(Iopcl.EQ.0)den2=Two*scalp(Iad1,Iwd1,Novaa)+scalp(Iad2,Iwd2,Nova
     &b)
      if(Iopcl.NE.0)den2=scalp(Iad1,Iwd1,Novaa)+scalp(Iad2,Iwd2,Novab)+s
     &calp(Iad3,Iwd3,Novbb)
      den2=den2-den1
      if(Iprint.GT.0)write(Iout,99001)den2
      
      call dd3
      
      if(Iopcl.EQ.0)call aaclos(Iwd2,Iwd1,Noa,Nva)
      
      if(Iopcl.EQ.0)Den=Two*scalp(Iad1,Iwd1,Novaa)+scalp(Iad2,Iwd2,Novab
     &)
      if(Iopcl.NE.0)Den=scalp(Iad1,Iwd1,Novaa)+scalp(Iad2,Iwd2,Novab)+sc
     &alp(Iad3,Iwd3,Novbb)
      den3=Den-den1-den2
      if(Iprint.GT.0)write(Iout,99002)den3
      endif
      
      if(Savail)then
      if(Iopcl.EQ.0)Den=Den+Two*scalp(Ias1,Iws1,Noava)
      if(Iopcl.NE.0)Den=Den+scalp(Ias1,Iws1,Noava)+scalp(Ias2,Iws2,Nobvb
     &)
      endif
      if(Iprint.GT.0)write(Iout,99003)Den
      
      
      call twrite(Icivar,A00,Lcivar,1,Lcivar,1,0)
      
      if(Idump.GT.1)call fdump
      JUMP=101
      
      return
      
      end
C* :1 * 
      
