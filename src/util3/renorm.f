
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 renorm"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "renorm.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 34 "renorm.web"
      subroutine renorm
      implicit none
      double precision aplusb,axb,C1,C2,C3,C4,del,delfin,Exx,f15,fcon,fo
     &rpt5,four,gabs,gatan,gsqrt,one,onept5,onpt25,onpt75
      double precision ovrd,ovrf,ovrlap,ovrp,ovrs,pi,pito75,prtthr,pt75,
     &sqrt2,three,thrpt5,thrsh,toovpi,two,twopt5,twort3,twpt25,X,Y
      double precision Z,zero
      integer i,igauss,igbeg,igdf,igend,In,Iout,Ipunch,ishell,Jan,jgauss
     &,jgdf,MAXPRM,MAXS21,MAXSH1,MAXSHL,Maxtyp,Nshell
      integer Aos,Aon,Shella,Shellt,Shelln,Shellc
      integer Shladf
      dimension ovrlap(4)
      parameter(MAXSHL=100,MAXPRM=(3*MAXSHL),MAXSH1=(MAXSHL+1),MAXS21=(2
     &*MAXSHL+1))
      common/b/Exx(MAXPRM),C1(MAXPRM),C2(MAXPRM),C3(MAXPRM),X(MAXSHL),Y(
     &MAXSHL),Z(MAXSHL),Jan(MAXSHL),Shella(MAXSHL),Shelln(MAXSHL),Shellt
     &(MAXSHL),Shellc(MAXSHL),Aos(MAXSHL),Aon(MAXSHL),Nshell,Maxtyp
      dimension C4(MAXSHL),Shladf(MAXSHL)
      equivalence(C4(1),C3(MAXSH1)),(Shladf(1),C3(MAXS21))
      common/io/In,Iout,Ipunch
      equivalence(ovrf,ovrlap(4))
      equivalence(ovrs,ovrlap(1)),(ovrp,ovrlap(2)),(ovrd,ovrlap(3))
      data zero/0.0D0/,one/1.0D0/
      data two/2.0D0/,three/3.0D0/,pt75/0.75D0/,onpt75/1.75D0/
      data onpt25/1.25D0/
      data thrsh/0.1D-04/,onept5/1.5D0/,twopt5/2.5D0/,thrpt5/3.5D0/
      data four/4.0D0/
      data twpt25/2.25D0/,f15/15.0D0/,forpt5/4.5D0/
      data prtthr/1.0D-05/
      
      
99001 format(39H SIGNIFICANT RENORMALIZATION REQUIRED: ,d20.10)
      sqrt2=gsqrt(two)
      pi=four*gatan(one)
      toovpi=two/pi
      pito75=pi**pt75
      fcon=(two/pito75)*gsqrt(two/f15)
      twort3=two/gsqrt(three)
      delfin=zero
      do 200 ishell=1,Nshell
      igbeg=Shella(ishell)
      igend=igbeg+Shelln(ishell)-1
      ovrs=zero
      ovrp=zero
      ovrd=zero
      ovrf=zero
      igdf=Shladf(ishell)
      do 50 igauss=igbeg,igend
      jgdf=Shladf(ishell)
      do 20 jgauss=igbeg,igend
      axb=four*Exx(igauss)*Exx(jgauss)
      aplusb=Exx(igauss)+Exx(jgauss)
      ovrs=ovrs+(C1(igauss)*C1(jgauss))*((axb**pt75)/(aplusb**onept5))
      ovrp=ovrp+(C2(igauss)*C2(jgauss))*((axb**onpt25)/(aplusb**twopt5))
      if(igdf.GT.0)then
      ovrd=ovrd+(C3(igdf)*C3(jgdf))*((axb**onpt75)/(aplusb**thrpt5))
      ovrf=ovrf+(C4(igdf)*C4(jgdf))*((axb**twpt25)/(aplusb**forpt5))
      jgdf=jgdf+1
      endif
20    continue
      if(igdf.GT.0)igdf=igdf+1
50    continue
      do 100 i=1,4
      if(ovrlap(i).LE.thrsh)then
      ovrlap(i)=zero
      else
      
      del=gabs(one-ovrlap(i))
      if(del.GE.delfin)delfin=del
      ovrlap(i)=one/gsqrt(ovrlap(i))
      endif
100   continue
      igdf=Shladf(ishell)
      do 150 igauss=igbeg,igend
      C1(igauss)=C1(igauss)*ovrs
      C2(igauss)=C2(igauss)*ovrp
      if(igdf.GT.0)then
      C3(igdf)=C3(igdf)*ovrd
      C4(igdf)=C4(igdf)*ovrf
      igdf=igdf+1
      endif
150   continue
200   continue
      if(gabs(delfin).GT.prtthr)write(Iout,99001)delfin
      do 300 ishell=1,Nshell
      igbeg=Shella(ishell)
      igend=igbeg+Shelln(ishell)-1
      igdf=Shladf(ishell)
      do 250 igauss=igbeg,igend
      C1(igauss)=C1(igauss)*((toovpi*Exx(igauss))**pt75)
      C2(igauss)=C2(igauss)*sqrt2*((two*Exx(igauss))**onpt25)/pito75
      if(igdf.GT.0)then
      C3(igdf)=C3(igdf)*twort3*((two*Exx(igauss))**onpt75)/pito75
      C4(igdf)=C4(igdf)*fcon*(two*Exx(igauss))**twpt25
      igdf=igdf+1
      endif
250   continue
300   continue
      return
      
      end
C* :1 * 
      
