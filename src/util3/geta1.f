
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 geta1"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "geta1.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 20 "geta1.web"
      subroutine geta1(A,CONST,INCP)
      implicit none
      double precision A,CONST,Xint,Zero1
      integer In,INCP,Iout,Ipunch,Lamax,Lbmax,Lpmax,lpnew,Maxdum
      dimension A(45)
      common/max/Lamax,Lbmax,Lpmax,Maxdum(4)
      common/io/In,Iout,Ipunch
      common/int/Zero1,Xint(12)
      
      
      
      
      
      
      
      
      
      lpnew=Lpmax+INCP
      if(lpnew.LE.1)return
      A(3)=CONST
      if(lpnew.NE.2)then
      A(4)=A(3)
      A(6)=A(3)*A(3)
      if(lpnew.NE.3)then
      A(8)=A(3)*(Xint(2)*A(3)+A(4))
      A(10)=A(3)*A(6)
      if(lpnew.NE.4)then
      A(11)=Xint(3)*A(3)*A(4)
      A(13)=A(3)*(Xint(3)*A(6)+A(8))
      A(15)=A(3)*A(10)
      if(lpnew.NE.5)then
      A(17)=A(3)*(Xint(4)*A(8)+A(11))
      A(19)=A(3)*(Xint(4)*A(10)+A(13))
      A(21)=A(3)*A(15)
      if(lpnew.NE.6)then
      A(22)=Xint(5)*A(3)*A(11)
      A(24)=A(3)*(Xint(5)*A(13)+A(17))
      A(26)=A(3)*(Xint(5)*A(15)+A(19))
      A(28)=A(3)*A(21)
      if(lpnew.NE.7)then
      A(30)=A(3)*(Xint(6)*A(17)+A(22))
      A(32)=A(3)*(Xint(6)*A(19)+A(24))
      A(34)=A(3)*(Xint(6)*A(21)+A(26))
      A(36)=A(3)*A(28)
      if(lpnew.NE.8)then
      A(37)=Xint(7)*A(3)*A(22)
      A(39)=A(3)*(Xint(7)*A(24)+A(30))
      A(41)=A(3)*(Xint(7)*A(26)+A(32))
      A(43)=A(3)*(Xint(7)*A(28)+A(34))
      A(45)=A(3)*A(36)
      endif
      endif
      endif
      endif
      endif
      endif
      endif
      return
      
      end
C* :1 * 
      
