
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 twod3c"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "twod3c.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 18 "twod3c.web"
      subroutine twod3c(CCQ)
      implicit none
      double precision CCQ,G,Vali2p,Vali3p
      integer Lamax,Lbmax,Lcmax,Ldmax,Lpmax,Lpqmax,Lqmax
      dimension CCQ(*)
      common/int2d/G(13),Vali2p(49),Vali3p(112)
      common/max/Lamax,Lbmax,Lcmax,Ldmax,Lpmax,Lqmax,Lpqmax
      
      
      if(Lpmax.EQ.1)goto 4200
      if(Lpmax.EQ.2)goto 3500
      if(Lpmax.EQ.3)goto 2800
      if(Lpmax.EQ.4)goto 2100
      if(Lpmax.EQ.5)goto 1400
      if(Lpmax.EQ.6)goto 700
      
      if(Lcmax.EQ.1)goto 500
      if(Lcmax.EQ.2)goto 300
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      Vali3p(109)=CCQ(31)*Vali2p(43)+CCQ(32)*Vali2p(44)+CCQ(33)*Vali2p(4
     &5)+Vali2p(46)
      goto 100
      elseif(Ldmax.EQ.2)then
      goto 50
      elseif(Ldmax.NE.3)then
      
      Vali3p(112)=CCQ(43)*Vali2p(43)+CCQ(44)*Vali2p(44)+CCQ(45)*Vali2p(4
     &5)+CCQ(46)*Vali2p(46)+CCQ(47)*Vali2p(47)+CCQ(48)*Vali2p(48)+Vali2p
     &(49)
      endif
      Vali3p(111)=CCQ(38)*Vali2p(43)+CCQ(39)*Vali2p(44)+CCQ(40)*Vali2p(4
     &5)+CCQ(41)*Vali2p(46)+CCQ(42)*Vali2p(47)+Vali2p(48)
50    Vali3p(110)=CCQ(34)*Vali2p(43)+CCQ(35)*Vali2p(44)+CCQ(36)*Vali2p(4
     &5)+CCQ(37)*Vali2p(46)+Vali2p(47)
      Vali3p(109)=CCQ(31)*Vali2p(43)+CCQ(32)*Vali2p(44)+CCQ(33)*Vali2p(4
     &5)+Vali2p(46)
      endif
100   if(Ldmax.EQ.1)then
      Vali3p(105)=CCQ(17)*Vali2p(43)+CCQ(18)*Vali2p(44)+Vali2p(45)
      goto 300
      elseif(Ldmax.EQ.2)then
      goto 200
      elseif(Ldmax.NE.3)then
      
      Vali3p(108)=CCQ(26)*Vali2p(43)+CCQ(27)*Vali2p(44)+CCQ(28)*Vali2p(4
     &5)+CCQ(29)*Vali2p(46)+CCQ(30)*Vali2p(47)+Vali2p(48)
      endif
      Vali3p(107)=CCQ(22)*Vali2p(43)+CCQ(23)*Vali2p(44)+CCQ(24)*Vali2p(4
     &5)+CCQ(25)*Vali2p(46)+Vali2p(47)
200   Vali3p(106)=CCQ(19)*Vali2p(43)+CCQ(20)*Vali2p(44)+CCQ(21)*Vali2p(4
     &5)+Vali2p(46)
      Vali3p(105)=CCQ(17)*Vali2p(43)+CCQ(18)*Vali2p(44)+Vali2p(45)
300   if(Ldmax.EQ.1)then
      Vali3p(101)=CCQ(7)*Vali2p(43)+Vali2p(44)
      goto 500
      elseif(Ldmax.EQ.2)then
      goto 400
      elseif(Ldmax.NE.3)then
      
      Vali3p(104)=CCQ(13)*Vali2p(43)+CCQ(14)*Vali2p(44)+CCQ(15)*Vali2p(4
     &5)+CCQ(16)*Vali2p(46)+Vali2p(47)
      endif
      Vali3p(103)=CCQ(10)*Vali2p(43)+CCQ(11)*Vali2p(44)+CCQ(12)*Vali2p(4
     &5)+Vali2p(46)
400   Vali3p(102)=CCQ(8)*Vali2p(43)+CCQ(9)*Vali2p(44)+Vali2p(45)
      Vali3p(101)=CCQ(7)*Vali2p(43)+Vali2p(44)
500   if(Ldmax.EQ.1)then
      Vali3p(97)=Vali2p(43)
      goto 700
      elseif(Ldmax.EQ.2)then
      goto 600
      elseif(Ldmax.NE.3)then
      
      Vali3p(100)=CCQ(4)*Vali2p(43)+CCQ(5)*Vali2p(44)+CCQ(6)*Vali2p(45)+
     &Vali2p(46)
      endif
      Vali3p(99)=CCQ(2)*Vali2p(43)+CCQ(3)*Vali2p(44)+Vali2p(45)
600   Vali3p(98)=CCQ(1)*Vali2p(43)+Vali2p(44)
      Vali3p(97)=Vali2p(43)
700   if(Lcmax.EQ.1)goto 1200
      if(Lcmax.EQ.2)goto 1000
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      Vali3p(93)=CCQ(31)*Vali2p(36)+CCQ(32)*Vali2p(37)+CCQ(33)*Vali2p(38
     &)+Vali2p(39)
      goto 800
      elseif(Ldmax.EQ.2)then
      goto 750
      elseif(Ldmax.NE.3)then
      
      Vali3p(96)=CCQ(43)*Vali2p(36)+CCQ(44)*Vali2p(37)+CCQ(45)*Vali2p(38
     &)+CCQ(46)*Vali2p(39)+CCQ(47)*Vali2p(40)+CCQ(48)*Vali2p(41)+Vali2p(
     &42)
      endif
      Vali3p(95)=CCQ(38)*Vali2p(36)+CCQ(39)*Vali2p(37)+CCQ(40)*Vali2p(38
     &)+CCQ(41)*Vali2p(39)+CCQ(42)*Vali2p(40)+Vali2p(41)
750   Vali3p(94)=CCQ(34)*Vali2p(36)+CCQ(35)*Vali2p(37)+CCQ(36)*Vali2p(38
     &)+CCQ(37)*Vali2p(39)+Vali2p(40)
      Vali3p(93)=CCQ(31)*Vali2p(36)+CCQ(32)*Vali2p(37)+CCQ(33)*Vali2p(38
     &)+Vali2p(39)
      endif
800   if(Ldmax.EQ.1)then
      Vali3p(89)=CCQ(17)*Vali2p(36)+CCQ(18)*Vali2p(37)+Vali2p(38)
      goto 1000
      elseif(Ldmax.EQ.2)then
      goto 900
      elseif(Ldmax.NE.3)then
      
      Vali3p(92)=CCQ(26)*Vali2p(36)+CCQ(27)*Vali2p(37)+CCQ(28)*Vali2p(38
     &)+CCQ(29)*Vali2p(39)+CCQ(30)*Vali2p(40)+Vali2p(41)
      endif
      Vali3p(91)=CCQ(22)*Vali2p(36)+CCQ(23)*Vali2p(37)+CCQ(24)*Vali2p(38
     &)+CCQ(25)*Vali2p(39)+Vali2p(40)
900   Vali3p(90)=CCQ(19)*Vali2p(36)+CCQ(20)*Vali2p(37)+CCQ(21)*Vali2p(38
     &)+Vali2p(39)
      Vali3p(89)=CCQ(17)*Vali2p(36)+CCQ(18)*Vali2p(37)+Vali2p(38)
1000  if(Ldmax.EQ.1)then
      Vali3p(85)=CCQ(7)*Vali2p(36)+Vali2p(37)
      goto 1200
      elseif(Ldmax.EQ.2)then
      goto 1100
      elseif(Ldmax.NE.3)then
      
      Vali3p(88)=CCQ(13)*Vali2p(36)+CCQ(14)*Vali2p(37)+CCQ(15)*Vali2p(38
     &)+CCQ(16)*Vali2p(39)+Vali2p(40)
      endif
      Vali3p(87)=CCQ(10)*Vali2p(36)+CCQ(11)*Vali2p(37)+CCQ(12)*Vali2p(38
     &)+Vali2p(39)
1100  Vali3p(86)=CCQ(8)*Vali2p(36)+CCQ(9)*Vali2p(37)+Vali2p(38)
      Vali3p(85)=CCQ(7)*Vali2p(36)+Vali2p(37)
1200  if(Ldmax.EQ.1)then
      Vali3p(81)=Vali2p(36)
      goto 1400
      elseif(Ldmax.EQ.2)then
      goto 1300
      elseif(Ldmax.NE.3)then
      
      Vali3p(84)=CCQ(4)*Vali2p(36)+CCQ(5)*Vali2p(37)+CCQ(6)*Vali2p(38)+V
     &ali2p(39)
      endif
      Vali3p(83)=CCQ(2)*Vali2p(36)+CCQ(3)*Vali2p(37)+Vali2p(38)
1300  Vali3p(82)=CCQ(1)*Vali2p(36)+Vali2p(37)
      Vali3p(81)=Vali2p(36)
1400  if(Lcmax.EQ.1)goto 1900
      if(Lcmax.EQ.2)goto 1700
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      Vali3p(77)=CCQ(31)*Vali2p(29)+CCQ(32)*Vali2p(30)+CCQ(33)*Vali2p(31
     &)+Vali2p(32)
      goto 1500
      elseif(Ldmax.EQ.2)then
      goto 1450
      elseif(Ldmax.NE.3)then
      
      Vali3p(80)=CCQ(43)*Vali2p(29)+CCQ(44)*Vali2p(30)+CCQ(45)*Vali2p(31
     &)+CCQ(46)*Vali2p(32)+CCQ(47)*Vali2p(33)+CCQ(48)*Vali2p(34)+Vali2p(
     &35)
      endif
      Vali3p(79)=CCQ(38)*Vali2p(29)+CCQ(39)*Vali2p(30)+CCQ(40)*Vali2p(31
     &)+CCQ(41)*Vali2p(32)+CCQ(42)*Vali2p(33)+Vali2p(34)
1450  Vali3p(78)=CCQ(34)*Vali2p(29)+CCQ(35)*Vali2p(30)+CCQ(36)*Vali2p(31
     &)+CCQ(37)*Vali2p(32)+Vali2p(33)
      Vali3p(77)=CCQ(31)*Vali2p(29)+CCQ(32)*Vali2p(30)+CCQ(33)*Vali2p(31
     &)+Vali2p(32)
      endif
1500  if(Ldmax.EQ.1)then
      Vali3p(73)=CCQ(17)*Vali2p(29)+CCQ(18)*Vali2p(30)+Vali2p(31)
      goto 1700
      elseif(Ldmax.EQ.2)then
      goto 1600
      elseif(Ldmax.NE.3)then
      
      Vali3p(76)=CCQ(26)*Vali2p(29)+CCQ(27)*Vali2p(30)+CCQ(28)*Vali2p(31
     &)+CCQ(29)*Vali2p(32)+CCQ(30)*Vali2p(33)+Vali2p(34)
      endif
      Vali3p(75)=CCQ(22)*Vali2p(29)+CCQ(23)*Vali2p(30)+CCQ(24)*Vali2p(31
     &)+CCQ(25)*Vali2p(32)+Vali2p(33)
1600  Vali3p(74)=CCQ(19)*Vali2p(29)+CCQ(20)*Vali2p(30)+CCQ(21)*Vali2p(31
     &)+Vali2p(32)
      Vali3p(73)=CCQ(17)*Vali2p(29)+CCQ(18)*Vali2p(30)+Vali2p(31)
1700  if(Ldmax.EQ.1)then
      Vali3p(69)=CCQ(7)*Vali2p(29)+Vali2p(30)
      goto 1900
      elseif(Ldmax.EQ.2)then
      goto 1800
      elseif(Ldmax.NE.3)then
      
      Vali3p(72)=CCQ(13)*Vali2p(29)+CCQ(14)*Vali2p(30)+CCQ(15)*Vali2p(31
     &)+CCQ(16)*Vali2p(32)+Vali2p(33)
      endif
      Vali3p(71)=CCQ(10)*Vali2p(29)+CCQ(11)*Vali2p(30)+CCQ(12)*Vali2p(31
     &)+Vali2p(32)
1800  Vali3p(70)=CCQ(8)*Vali2p(29)+CCQ(9)*Vali2p(30)+Vali2p(31)
      Vali3p(69)=CCQ(7)*Vali2p(29)+Vali2p(30)
1900  if(Ldmax.EQ.1)then
      Vali3p(65)=Vali2p(29)
      goto 2100
      elseif(Ldmax.EQ.2)then
      goto 2000
      elseif(Ldmax.NE.3)then
      
      Vali3p(68)=CCQ(4)*Vali2p(29)+CCQ(5)*Vali2p(30)+CCQ(6)*Vali2p(31)+V
     &ali2p(32)
      endif
      Vali3p(67)=CCQ(2)*Vali2p(29)+CCQ(3)*Vali2p(30)+Vali2p(31)
2000  Vali3p(66)=CCQ(1)*Vali2p(29)+Vali2p(30)
      Vali3p(65)=Vali2p(29)
2100  if(Lcmax.EQ.1)goto 2600
      if(Lcmax.EQ.2)goto 2400
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      Vali3p(61)=CCQ(31)*Vali2p(22)+CCQ(32)*Vali2p(23)+CCQ(33)*Vali2p(24
     &)+Vali2p(25)
      goto 2200
      elseif(Ldmax.EQ.2)then
      goto 2150
      elseif(Ldmax.NE.3)then
      
      Vali3p(64)=CCQ(43)*Vali2p(22)+CCQ(44)*Vali2p(23)+CCQ(45)*Vali2p(24
     &)+CCQ(46)*Vali2p(25)+CCQ(47)*Vali2p(26)+CCQ(48)*Vali2p(27)+Vali2p(
     &28)
      endif
      Vali3p(63)=CCQ(38)*Vali2p(22)+CCQ(39)*Vali2p(23)+CCQ(40)*Vali2p(24
     &)+CCQ(41)*Vali2p(25)+CCQ(42)*Vali2p(26)+Vali2p(27)
2150  Vali3p(62)=CCQ(34)*Vali2p(22)+CCQ(35)*Vali2p(23)+CCQ(36)*Vali2p(24
     &)+CCQ(37)*Vali2p(25)+Vali2p(26)
      Vali3p(61)=CCQ(31)*Vali2p(22)+CCQ(32)*Vali2p(23)+CCQ(33)*Vali2p(24
     &)+Vali2p(25)
      endif
2200  if(Ldmax.EQ.1)then
      Vali3p(57)=CCQ(17)*Vali2p(22)+CCQ(18)*Vali2p(23)+Vali2p(24)
      goto 2400
      elseif(Ldmax.EQ.2)then
      goto 2300
      elseif(Ldmax.NE.3)then
      
      Vali3p(60)=CCQ(26)*Vali2p(22)+CCQ(27)*Vali2p(23)+CCQ(28)*Vali2p(24
     &)+CCQ(29)*Vali2p(25)+CCQ(30)*Vali2p(26)+Vali2p(27)
      endif
      Vali3p(59)=CCQ(22)*Vali2p(22)+CCQ(23)*Vali2p(23)+CCQ(24)*Vali2p(24
     &)+CCQ(25)*Vali2p(25)+Vali2p(26)
2300  Vali3p(58)=CCQ(19)*Vali2p(22)+CCQ(20)*Vali2p(23)+CCQ(21)*Vali2p(24
     &)+Vali2p(25)
      Vali3p(57)=CCQ(17)*Vali2p(22)+CCQ(18)*Vali2p(23)+Vali2p(24)
2400  if(Ldmax.EQ.1)then
      Vali3p(53)=CCQ(7)*Vali2p(22)+Vali2p(23)
      goto 2600
      elseif(Ldmax.EQ.2)then
      goto 2500
      elseif(Ldmax.NE.3)then
      
      Vali3p(56)=CCQ(13)*Vali2p(22)+CCQ(14)*Vali2p(23)+CCQ(15)*Vali2p(24
     &)+CCQ(16)*Vali2p(25)+Vali2p(26)
      endif
      Vali3p(55)=CCQ(10)*Vali2p(22)+CCQ(11)*Vali2p(23)+CCQ(12)*Vali2p(24
     &)+Vali2p(25)
2500  Vali3p(54)=CCQ(8)*Vali2p(22)+CCQ(9)*Vali2p(23)+Vali2p(24)
      Vali3p(53)=CCQ(7)*Vali2p(22)+Vali2p(23)
2600  if(Ldmax.EQ.1)then
      Vali3p(49)=Vali2p(22)
      goto 2800
      elseif(Ldmax.EQ.2)then
      goto 2700
      elseif(Ldmax.NE.3)then
      
      Vali3p(52)=CCQ(4)*Vali2p(22)+CCQ(5)*Vali2p(23)+CCQ(6)*Vali2p(24)+V
     &ali2p(25)
      endif
      Vali3p(51)=CCQ(2)*Vali2p(22)+CCQ(3)*Vali2p(23)+Vali2p(24)
2700  Vali3p(50)=CCQ(1)*Vali2p(22)+Vali2p(23)
      Vali3p(49)=Vali2p(22)
2800  if(Lcmax.EQ.1)goto 3300
      if(Lcmax.EQ.2)goto 3100
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      Vali3p(45)=CCQ(31)*Vali2p(15)+CCQ(32)*Vali2p(16)+CCQ(33)*Vali2p(17
     &)+Vali2p(18)
      goto 2900
      elseif(Ldmax.EQ.2)then
      goto 2850
      elseif(Ldmax.NE.3)then
      
      Vali3p(48)=CCQ(43)*Vali2p(15)+CCQ(44)*Vali2p(16)+CCQ(45)*Vali2p(17
     &)+CCQ(46)*Vali2p(18)+CCQ(47)*Vali2p(19)+CCQ(48)*Vali2p(20)+Vali2p(
     &21)
      endif
      Vali3p(47)=CCQ(38)*Vali2p(15)+CCQ(39)*Vali2p(16)+CCQ(40)*Vali2p(17
     &)+CCQ(41)*Vali2p(18)+CCQ(42)*Vali2p(19)+Vali2p(20)
2850  Vali3p(46)=CCQ(34)*Vali2p(15)+CCQ(35)*Vali2p(16)+CCQ(36)*Vali2p(17
     &)+CCQ(37)*Vali2p(18)+Vali2p(19)
      Vali3p(45)=CCQ(31)*Vali2p(15)+CCQ(32)*Vali2p(16)+CCQ(33)*Vali2p(17
     &)+Vali2p(18)
      endif
2900  if(Ldmax.EQ.1)then
      Vali3p(41)=CCQ(17)*Vali2p(15)+CCQ(18)*Vali2p(16)+Vali2p(17)
      goto 3100
      elseif(Ldmax.EQ.2)then
      goto 3000
      elseif(Ldmax.NE.3)then
      
      Vali3p(44)=CCQ(26)*Vali2p(15)+CCQ(27)*Vali2p(16)+CCQ(28)*Vali2p(17
     &)+CCQ(29)*Vali2p(18)+CCQ(30)*Vali2p(19)+Vali2p(20)
      endif
      Vali3p(43)=CCQ(22)*Vali2p(15)+CCQ(23)*Vali2p(16)+CCQ(24)*Vali2p(17
     &)+CCQ(25)*Vali2p(18)+Vali2p(19)
3000  Vali3p(42)=CCQ(19)*Vali2p(15)+CCQ(20)*Vali2p(16)+CCQ(21)*Vali2p(17
     &)+Vali2p(18)
      Vali3p(41)=CCQ(17)*Vali2p(15)+CCQ(18)*Vali2p(16)+Vali2p(17)
3100  if(Ldmax.EQ.1)then
      Vali3p(37)=CCQ(7)*Vali2p(15)+Vali2p(16)
      goto 3300
      elseif(Ldmax.EQ.2)then
      goto 3200
      elseif(Ldmax.NE.3)then
      
      Vali3p(40)=CCQ(13)*Vali2p(15)+CCQ(14)*Vali2p(16)+CCQ(15)*Vali2p(17
     &)+CCQ(16)*Vali2p(18)+Vali2p(19)
      endif
      Vali3p(39)=CCQ(10)*Vali2p(15)+CCQ(11)*Vali2p(16)+CCQ(12)*Vali2p(17
     &)+Vali2p(18)
3200  Vali3p(38)=CCQ(8)*Vali2p(15)+CCQ(9)*Vali2p(16)+Vali2p(17)
      Vali3p(37)=CCQ(7)*Vali2p(15)+Vali2p(16)
3300  if(Ldmax.EQ.1)then
      Vali3p(33)=Vali2p(15)
      goto 3500
      elseif(Ldmax.EQ.2)then
      goto 3400
      elseif(Ldmax.NE.3)then
      
      Vali3p(36)=CCQ(4)*Vali2p(15)+CCQ(5)*Vali2p(16)+CCQ(6)*Vali2p(17)+V
     &ali2p(18)
      endif
      Vali3p(35)=CCQ(2)*Vali2p(15)+CCQ(3)*Vali2p(16)+Vali2p(17)
3400  Vali3p(34)=CCQ(1)*Vali2p(15)+Vali2p(16)
      Vali3p(33)=Vali2p(15)
3500  if(Lcmax.EQ.1)goto 4000
      if(Lcmax.EQ.2)goto 3800
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      Vali3p(29)=CCQ(31)*Vali2p(8)+CCQ(32)*Vali2p(9)+CCQ(33)*Vali2p(10)+
     &Vali2p(11)
      goto 3600
      elseif(Ldmax.EQ.2)then
      goto 3550
      elseif(Ldmax.NE.3)then
      
      Vali3p(32)=CCQ(43)*Vali2p(8)+CCQ(44)*Vali2p(9)+CCQ(45)*Vali2p(10)+
     &CCQ(46)*Vali2p(11)+CCQ(47)*Vali2p(12)+CCQ(48)*Vali2p(13)+Vali2p(14
     &)
      endif
      Vali3p(31)=CCQ(38)*Vali2p(8)+CCQ(39)*Vali2p(9)+CCQ(40)*Vali2p(10)+
     &CCQ(41)*Vali2p(11)+CCQ(42)*Vali2p(12)+Vali2p(13)
3550  Vali3p(30)=CCQ(34)*Vali2p(8)+CCQ(35)*Vali2p(9)+CCQ(36)*Vali2p(10)+
     &CCQ(37)*Vali2p(11)+Vali2p(12)
      Vali3p(29)=CCQ(31)*Vali2p(8)+CCQ(32)*Vali2p(9)+CCQ(33)*Vali2p(10)+
     &Vali2p(11)
      endif
3600  if(Ldmax.EQ.1)then
      Vali3p(25)=CCQ(17)*Vali2p(8)+CCQ(18)*Vali2p(9)+Vali2p(10)
      goto 3800
      elseif(Ldmax.EQ.2)then
      goto 3700
      elseif(Ldmax.NE.3)then
      
      Vali3p(28)=CCQ(26)*Vali2p(8)+CCQ(27)*Vali2p(9)+CCQ(28)*Vali2p(10)+
     &CCQ(29)*Vali2p(11)+CCQ(30)*Vali2p(12)+Vali2p(13)
      endif
      Vali3p(27)=CCQ(22)*Vali2p(8)+CCQ(23)*Vali2p(9)+CCQ(24)*Vali2p(10)+
     &CCQ(25)*Vali2p(11)+Vali2p(12)
3700  Vali3p(26)=CCQ(19)*Vali2p(8)+CCQ(20)*Vali2p(9)+CCQ(21)*Vali2p(10)+
     &Vali2p(11)
      Vali3p(25)=CCQ(17)*Vali2p(8)+CCQ(18)*Vali2p(9)+Vali2p(10)
3800  if(Ldmax.EQ.1)then
      Vali3p(21)=CCQ(7)*Vali2p(8)+Vali2p(9)
      goto 4000
      elseif(Ldmax.EQ.2)then
      goto 3900
      elseif(Ldmax.NE.3)then
      
      Vali3p(24)=CCQ(13)*Vali2p(8)+CCQ(14)*Vali2p(9)+CCQ(15)*Vali2p(10)+
     &CCQ(16)*Vali2p(11)+Vali2p(12)
      endif
      Vali3p(23)=CCQ(10)*Vali2p(8)+CCQ(11)*Vali2p(9)+CCQ(12)*Vali2p(10)+
     &Vali2p(11)
3900  Vali3p(22)=CCQ(8)*Vali2p(8)+CCQ(9)*Vali2p(9)+Vali2p(10)
      Vali3p(21)=CCQ(7)*Vali2p(8)+Vali2p(9)
4000  if(Ldmax.EQ.1)then
      Vali3p(17)=Vali2p(8)
      goto 4200
      elseif(Ldmax.EQ.2)then
      goto 4100
      elseif(Ldmax.NE.3)then
      
      Vali3p(20)=CCQ(4)*Vali2p(8)+CCQ(5)*Vali2p(9)+CCQ(6)*Vali2p(10)+Val
     &i2p(11)
      endif
      Vali3p(19)=CCQ(2)*Vali2p(8)+CCQ(3)*Vali2p(9)+Vali2p(10)
4100  Vali3p(18)=CCQ(1)*Vali2p(8)+Vali2p(9)
      Vali3p(17)=Vali2p(8)
4200  if(Lcmax.EQ.1)goto 4700
      if(Lcmax.EQ.2)goto 4500
      if(Lcmax.NE.3)then
      
      if(Ldmax.EQ.1)then
      Vali3p(13)=CCQ(31)*Vali2p(1)+CCQ(32)*Vali2p(2)+CCQ(33)*Vali2p(3)+V
     &ali2p(4)
      goto 4300
      elseif(Ldmax.EQ.2)then
      goto 4250
      elseif(Ldmax.NE.3)then
      
      Vali3p(16)=CCQ(43)*Vali2p(1)+CCQ(44)*Vali2p(2)+CCQ(45)*Vali2p(3)+C
     &CQ(46)*Vali2p(4)+CCQ(47)*Vali2p(5)+CCQ(48)*Vali2p(6)+Vali2p(7)
      endif
      Vali3p(15)=CCQ(38)*Vali2p(1)+CCQ(39)*Vali2p(2)+CCQ(40)*Vali2p(3)+C
     &CQ(41)*Vali2p(4)+CCQ(42)*Vali2p(5)+Vali2p(6)
4250  Vali3p(14)=CCQ(34)*Vali2p(1)+CCQ(35)*Vali2p(2)+CCQ(36)*Vali2p(3)+C
     &CQ(37)*Vali2p(4)+Vali2p(5)
      Vali3p(13)=CCQ(31)*Vali2p(1)+CCQ(32)*Vali2p(2)+CCQ(33)*Vali2p(3)+V
     &ali2p(4)
      endif
4300  if(Ldmax.EQ.1)then
      Vali3p(9)=CCQ(17)*Vali2p(1)+CCQ(18)*Vali2p(2)+Vali2p(3)
      goto 4500
      elseif(Ldmax.EQ.2)then
      goto 4400
      elseif(Ldmax.NE.3)then
      
      Vali3p(12)=CCQ(26)*Vali2p(1)+CCQ(27)*Vali2p(2)+CCQ(28)*Vali2p(3)+C
     &CQ(29)*Vali2p(4)+CCQ(30)*Vali2p(5)+Vali2p(6)
      endif
      Vali3p(11)=CCQ(22)*Vali2p(1)+CCQ(23)*Vali2p(2)+CCQ(24)*Vali2p(3)+C
     &CQ(25)*Vali2p(4)+Vali2p(5)
4400  Vali3p(10)=CCQ(19)*Vali2p(1)+CCQ(20)*Vali2p(2)+CCQ(21)*Vali2p(3)+V
     &ali2p(4)
      Vali3p(9)=CCQ(17)*Vali2p(1)+CCQ(18)*Vali2p(2)+Vali2p(3)
4500  if(Ldmax.EQ.1)then
      Vali3p(5)=CCQ(7)*Vali2p(1)+Vali2p(2)
      goto 4700
      elseif(Ldmax.EQ.2)then
      goto 4600
      elseif(Ldmax.NE.3)then
      
      Vali3p(8)=CCQ(13)*Vali2p(1)+CCQ(14)*Vali2p(2)+CCQ(15)*Vali2p(3)+CC
     &Q(16)*Vali2p(4)+Vali2p(5)
      endif
      Vali3p(7)=CCQ(10)*Vali2p(1)+CCQ(11)*Vali2p(2)+CCQ(12)*Vali2p(3)+Va
     &li2p(4)
4600  Vali3p(6)=CCQ(8)*Vali2p(1)+CCQ(9)*Vali2p(2)+Vali2p(3)
      Vali3p(5)=CCQ(7)*Vali2p(1)+Vali2p(2)
4700  if(Ldmax.EQ.1)goto 4900
      if(Ldmax.EQ.2)goto 4800
      
      if(Ldmax.NE.3)Vali3p(4)=CCQ(4)*Vali2p(1)+CCQ(5)*Vali2p(2)+CCQ(6)*V
     &ali2p(3)+Vali2p(4)
      Vali3p(3)=CCQ(2)*Vali2p(1)+CCQ(3)*Vali2p(2)+Vali2p(3)
4800  Vali3p(2)=CCQ(1)*Vali2p(1)+Vali2p(2)
4900  Vali3p(1)=Vali2p(1)
      return
      
      end
C* :1 * 
      
