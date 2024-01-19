
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 cfill"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "cfill.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "cfill.web"
      subroutine cfill(IT,IGBEG,IGSP,IGDF,C1,C2,C3,C4,CA)
      implicit none
      double precision C1,C2,C3,C4,CA,Pt5,R1,R2,R3,R3ov2,R4,Root15,Root3
     &,Root5,temp,Z1,Z2,Z3
      integer IGBEG,IGDF,IGSP,In,inddf,Iout,Ipunch,IT
      dimension CA(*)
      dimension C1(*),C2(*),C3(*),C4(*)
      common/cfact/Pt5,R3ov2,Root3,Root5,Root15,R1,R2,R3,R4,Z1,Z2,Z3
      common/io/In,Iout,Ipunch
      
      if(IT.LT.3)then
      
      CA(1)=C1(IGSP)
      
      if(IT.GT.0)then
      
      CA(2)=C2(IGSP)
      CA(3)=C2(IGSP)
      CA(4)=C2(IGSP)
      
      if(IT.GT.1)then
      
      inddf=IGDF+(IGSP-IGBEG)
      CA(5)=C3(inddf)
      CA(6)=C3(inddf)
      CA(7)=C3(inddf)
      temp=C3(inddf)*Root3
      CA(8)=temp
      CA(9)=temp
      CA(10)=temp
      endif
      endif
      else
      
      inddf=IGDF+(IGSP-IGBEG)
      CA(11)=C4(inddf)
      CA(12)=C4(inddf)
      CA(13)=C4(inddf)
      temp=C4(inddf)*Root5
      CA(14)=temp
      CA(15)=temp
      CA(16)=temp
      CA(17)=temp
      CA(18)=temp
      CA(19)=temp
      CA(20)=C4(inddf)*Root15
      endif
      
      return
      
      end
C* :1 * 
      
