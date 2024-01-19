
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 eone"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "eone.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 26 "eone.web"
      subroutine eone(E1,E2,CS1,CS2,CP2,NGAUSS,NSPLIT,IA,IFLAG)
      implicit none
      double precision CP2,CS1,CS2,E1,E2
      integer IA,IFLAG,NGAUSS,NSPLIT
      dimension E1(6),E2(6),CS1(6),CS2(6),CP2(6)
      
      
      
      
      IFLAG=0
      if(NSPLIT.NE.1)call berror(2)
      if(IA.NE.10)then
      
      if(IA.EQ.1.OR.IA.EQ.2)goto 1200
      if(IA.EQ.4)then
      if(NGAUSS.EQ.1)then
      elseif(NGAUSS.EQ.2)then
      elseif(NGAUSS.EQ.4)then
      goto 20
      elseif(NGAUSS.EQ.5)then
      call berror(2)
      goto 200
      elseif(NGAUSS.EQ.6)then
      goto 200
      endif
      
      call berror(2)
20    IFLAG=1
      E1(1)=554.010D0
      E1(2)=83.2631D0
      E1(3)=18.8635D0
      E1(4)=5.17782D0
      E1(5)=1.55602D0
      CS1(1)=.00540997D0
      CS1(2)=.0402515D0
      CS1(3)=.176858D0
      CS1(4)=.452559D0
      CS1(5)=.470293D0
      E2(1)=1.35899D0
      E2(2)=.284533D0
      CS2(1)=-.477429D0
      CS2(2)=1.24745D0
      CP2(1)=.201142D0
      CP2(2)=.884483D0
      E2(3)=.0804858D0
      CS2(3)=.100000000D01
      CP2(3)=.100000000D01
      return
      elseif(IA.EQ.5)then
      if(NGAUSS.EQ.1)then
      elseif(NGAUSS.EQ.2)then
      elseif(NGAUSS.EQ.4)then
      goto 40
      elseif(NGAUSS.EQ.5)then
      
      call berror(2)
      goto 300
      elseif(NGAUSS.EQ.6)then
      goto 300
      endif
      
      call berror(2)
40    E1(1)=3.307528520D02
      CS1(1)=1.799417960D-02
      E1(2)=4.984386500D01
      CS1(2)=1.246937000D-01
      E1(3)=1.111705350D01
      CS1(3)=4.343353750D-01
      E1(4)=2.922724310D00
      CS1(4)=5.609793740D-01
      E2(1)=5.355136790D00
      CS2(1)=-1.303870780D-01
      CP2(1)=6.374292250D-02
      E2(2)=1.370915820D00
      CS2(2)=-2.514343900D-01
      CP2(2)=2.761330530D-01
      E2(3)=4.037878930D-01
      CS2(3)=1.205129200D00
      CP2(3)=7.773865960D-01
      E2(4)=1.149706420D-01
      CS2(4)=1.000000000D00
      CP2(4)=1.000000000D00
      return
      elseif(IA.EQ.6)then
      if(NGAUSS.EQ.1)then
      
      call berror(2)
      goto 400
      elseif(NGAUSS.EQ.2)then
      call berror(2)
      goto 400
      elseif(NGAUSS.EQ.4)then
      goto 400
      elseif(NGAUSS.EQ.5)then
      
      if(NSPLIT.EQ.2)then
      
      call berror(2)
      goto 500
      elseif(NSPLIT.EQ.3.OR.NSPLIT.EQ.5)then
      goto 1200
      elseif(NSPLIT.EQ.4)then
      call berror(2)
      goto 500
      else
      
      E1(1)=1.264250200D03
      CS1(1)=5.473495530D-03
      E1(2)=1.901442980D02
      CS1(2)=4.079115360D-02
      E1(3)=4.312858670D01
      CS1(3)=1.812203490D-01
      E1(4)=1.194438200D01
      CS1(4)=4.634824780D-01
      E1(5)=3.651484710D00
      CS1(5)=4.524711990D-01
      E2(1)=7.942730520D00
      CS2(1)=-1.207731490D-01
      CP2(1)=6.867749980D-02
      E2(2)=1.907237930D00
      CS2(2)=-1.697932090D-01
      CP2(2)=3.141028910D-01
      E2(3)=5.535773850D-01
      CS2(3)=1.149811710D00
      CP2(3)=7.459685180D-01
      E2(4)=1.585119750D-01
      CS2(4)=1.000000000D00
      CP2(4)=1.000000000D00
      return
      endif
      elseif(NGAUSS.EQ.6)then
      goto 500
      else
      call berror(2)
      goto 400
      endif
      elseif(IA.EQ.7)then
      goto 600
      elseif(IA.EQ.8)then
      goto 800
      elseif(IA.EQ.9)then
      goto 1000
      elseif(IA.EQ.10)then
      call berror(2)
      goto 1200
      else
      if(NGAUSS.EQ.1)then
      elseif(NGAUSS.EQ.2)then
      elseif(NGAUSS.EQ.4)then
      goto 60
      elseif(NGAUSS.EQ.5)then
      call berror(2)
      goto 100
      elseif(NGAUSS.EQ.6)then
      goto 100
      endif
      
      call berror(2)
60    IFLAG=1
      E1(1)=.275394444D03
      CS1(1)=.612184691D-02
      E1(2)=.414351754D02
      CS1(2)=.451129615D-01
      E1(3)=.936699378D01
      CS1(3)=.192694150D00
      E1(4)=.253772533D01
      CS1(4)=.468544208D00
      E1(5)=.746636540D00
      CS1(5)=.440607515D00
      E2(1)=.692397267D00
      CS2(1)=-.252536797D00
      CP2(1)=.143591732D00
      E2(2)=.821924442D-01
      CS2(2)=.109734080D01
      CP2(2)=.947803050D00
      E2(3)=.322374501D-01
      CS2(3)=.100000000D01
      CP2(3)=.100000000D01
      return
      endif
100   E1(1)=.642418915D03
      CS1(1)=.214260781D-02
      E1(2)=.967985153D02
      CS1(2)=.162088715D-01
      E1(3)=.220911212D02
      CS1(3)=.773155725D-01
      E1(4)=.620107025D01
      CS1(4)=.245786052D00
      E1(5)=.193511768D01
      CS1(5)=.470189004D00
      E1(6)=.636735789D00
      CS1(6)=.345470845D00
      E2(1)=.219145858D01
      CS2(1)=-.350917459D-01
      CP2(1)=.894150804D-02
      E2(2)=.596126266D00
      CS2(2)=-.191232844D00
      CP2(2)=.141009464D00
      E2(3)=.745154442D-01
      CS2(3)=.108398780D01
      CP2(3)=.945363695D00
      E2(4)=.286686637D-01
      CS2(4)=.100000000D01
      CP2(4)=.100000000D01
      return
      else
      call neon(E1,E2,CS1,CS2,CP2,NGAUSS,NSPLIT)
      return
      endif
200   E1(1)=1264.58569D0
      CS1(1)=1.94475759D-3
      E1(2)=189.936806D0
      CS1(2)=1.48350520D-2
      E1(3)=43.1590890D0
      CS1(3)=7.20905463D-2
      E1(4)=12.0986627D0
      CS1(4)=.237154150D0
      E1(5)=3.80632322D0
      CS1(5)=.469198652D0
      E1(6)=1.27289030D0
      CS1(6)=.356520228D0
      E2(1)=3.01297304D0
      CS2(1)=-.112648729D0
      CP2(1)=.0559801998D0
      E2(2)=.704885761D0
      CS2(2)=-.229506409D0
      CP2(2)=.261550611D0
      E2(3)=.207339363D0
      CS2(3)=1.18691677D0
      CP2(3)=.793972339D0
      E2(4)=.0656169489D0
      CS2(4)=.100000000D01
      CP2(4)=.100000000D01
      return
300   E1(1)=.206888225D04
      CS1(1)=.186627459D-02
      E1(2)=.310649570D03
      CS1(2)=.142514817D-01
      E1(3)=.706830330D02
      CS1(3)=.695516185D-01
      E1(4)=.198610803D02
      CS1(4)=.232572933D00
      E1(5)=.629930484D01
      CS1(5)=.467078712D00
      E1(6)=.212702697D01
      CS1(6)=.363431440D00
      E2(1)=.445656619D01
      CS2(1)=-.130393797D00
      CP2(1)=.745975799D-01
      E2(2)=.112200748D01
      CS2(2)=-.130788951D00
      CP2(2)=.307846677D00
      E2(3)=.338779982D00
      CS2(3)=.113094448D01
      CP2(3)=.743456834D00
      E2(4)=.101045318D00
      CS2(4)=.100000000D01
      CP2(4)=.100000000D01
      return
400   E1(1)=4.869669280D02
      CS1(1)=1.772582290D-02
      E1(2)=7.337109420D01
      CS1(2)=1.234778670D-01
      E1(3)=1.641345790D01
      CS1(3)=4.338754000D-01
      E1(4)=4.344983560D00
      CS1(4)=5.615041970D-01
      E2(1)=8.673525310D00
      CS2(1)=-1.213837490D-01
      CP2(1)=6.354538410D-02
      E2(2)=2.096619260D00
      CS2(2)=-2.273384980D-01
      CP2(2)=2.982677570D-01
      E2(3)=6.046513290D-01
      CS2(3)=1.185173920D00
      CP2(3)=7.621032280D-01
      E2(4)=1.697095320D-01
      CS2(4)=1.000000000D00
      CP2(4)=1.000000000D00
      return
500   if(NSPLIT.EQ.2)then
      
      call berror(2)
      elseif(NSPLIT.EQ.3)then
      call berror(2)
      elseif(NSPLIT.EQ.4)then
      call berror(2)
      elseif(NSPLIT.EQ.5)then
      call berror(2)
      else
      
      E1(1)=3.047524880D03
      CS1(1)=1.834737130D-03
      E1(2)=4.573695180D02
      CS1(2)=1.403732280D-02
      E1(3)=1.039486850D02
      CS1(3)=6.884262220D-02
      E1(4)=2.921015530D01
      CS1(4)=2.321844430D-01
      E1(5)=9.286662960D00
      CS1(5)=4.679413480D-01
      E1(6)=3.163926960D00
      CS1(6)=3.623119850D-01
      E2(1)=7.868272350D00
      CS2(1)=-1.193324200D-01
      CP2(1)=6.899906660D-02
      E2(2)=1.881288540D00
      CS2(2)=-1.608541520D-01
      CP2(2)=3.164239610D-01
      E2(3)=5.442492580D-01
      CS2(3)=1.143456440D00
      CP2(3)=7.443082910D-01
      E2(4)=1.559860190D-01
      CS2(4)=1.000000000D00
      CP2(4)=1.000000000D00
      return
      endif
600   if(NGAUSS.EQ.1)then
      
      call berror(2)
      elseif(NGAUSS.EQ.2)then
      call berror(2)
      elseif(NGAUSS.EQ.4)then
      elseif(NGAUSS.EQ.5)then
      
      if(NSPLIT.EQ.2)then
      
      call berror(2)
      goto 700
      elseif(NSPLIT.EQ.3.OR.NSPLIT.EQ.5)then
      goto 1200
      elseif(NSPLIT.EQ.4)then
      call berror(2)
      goto 700
      else
      
      E1(1)=1.745284990D03
      CS1(1)=5.421222340D-03
      E1(2)=2.625413270D02
      CS1(2)=4.043458220D-02
      E1(3)=5.958251390D01
      CS1(3)=1.804448930D-01
      E1(4)=1.654434660D01
      CS1(4)=4.634396240D-01
      E1(5)=5.084876110D00
      CS1(5)=4.526306780D-01
      E2(1)=1.194496010D01
      CS2(1)=-1.161469430D-01
      CP2(1)=6.732112530D-02
      E2(2)=2.800090140D00
      CS2(2)=-1.757313530D-01
      CP2(2)=3.221237760D-01
      E2(3)=7.981580700D-01
      CS2(3)=1.150529060D00
      CP2(3)=7.421561640D-01
      E2(4)=2.235978380D-01
      CS2(4)=1.000000000D00
      CP2(4)=1.000000000D00
      return
      endif
      elseif(NGAUSS.EQ.6)then
      goto 700
      else
      call berror(2)
      endif
      E1(1)=6.712795030D02
      CS1(1)=1.759825110D-02
      E1(2)=1.012016620D02
      CS1(2)=1.228462410D-01
      E1(3)=2.269996590D01
      CS1(3)=4.337821410D-01
      E1(4)=6.040609000D00
      CS1(4)=5.614182170D-01
      E2(1)=1.264524000D01
      CS2(1)=-1.174892990D-01
      CP2(1)=6.402034430D-02
      E2(2)=2.981719040D00
      CS2(2)=-2.139940160D-01
      CP2(2)=3.112025550D-01
      E2(3)=8.494317690D-01
      CS2(3)=1.174502110D00
      CP2(3)=7.527482390D-01
      E2(4)=2.352813130D-01
      CS2(4)=1.000000000D00
      CP2(4)=1.000000000D00
      return
700   if(NSPLIT.EQ.2)then
      
      call berror(2)
      elseif(NSPLIT.EQ.3)then
      call berror(2)
      elseif(NSPLIT.EQ.4)then
      call berror(2)
      elseif(NSPLIT.EQ.5)then
      call berror(2)
      else
      
      E1(1)=4.173511460D03
      CS1(1)=1.834772160D-03
      E1(2)=6.274579110D02
      CS1(2)=1.399462700D-02
      E1(3)=1.429020930D02
      CS1(3)=6.858655180D-02
      E1(4)=4.023432930D01
      CS1(4)=2.322408730D-01
      E1(5)=1.282021290D01
      CS1(5)=4.690699480D-01
      E1(6)=4.390437010D00
      CS1(6)=3.604551990D-01
      E2(1)=1.186242410D01
      CS2(1)=-1.149611820D-01
      CP2(1)=6.757974390D-02
      E2(2)=2.771431290D00
      CS2(2)=-1.691174790D-01
      CP2(2)=3.239072960D-01
      E2(3)=7.878975580D-01
      CS2(3)=1.145851950D00
      CP2(3)=7.408951400D-01
      E2(4)=2.207741540D-01
      CS2(4)=1.000000000D00
      CP2(4)=1.000000000D00
      return
      endif
800   if(NGAUSS.EQ.1)then
      
      call berror(2)
      elseif(NGAUSS.EQ.2)then
      call berror(2)
      elseif(NGAUSS.EQ.4)then
      elseif(NGAUSS.EQ.5)then
      
      if(NSPLIT.EQ.2)then
      
      call berror(2)
      goto 900
      elseif(NSPLIT.EQ.3.OR.NSPLIT.EQ.5)then
      goto 1200
      elseif(NSPLIT.EQ.4)then
      call berror(2)
      goto 900
      else
      
      E1(1)=2.296705350D03
      CS1(1)=5.402590830D-03
      E1(2)=3.454369370D02
      CS1(2)=4.033911540D-02
      E1(3)=7.840108180D01
      CS1(3)=1.805909170D-01
      E1(4)=2.181041640D01
      CS1(4)=4.643773710D-01
      E1(5)=6.721723480D00
      CS1(5)=4.511585830D-01
      E2(1)=1.591091840D01
      CS2(1)=-1.118074560D-01
      CP2(1)=7.079811160D-02
      E2(2)=3.695812810D00
      CS2(2)=-1.519256420D-01
      CP2(2)=3.386958530D-01
      E2(3)=1.043638440D00
      CS2(3)=1.133714340D00
      CP2(3)=7.277198890D-01
      E2(4)=2.838735860D-01
      CS2(4)=1.000000000D00
      CP2(4)=1.000000000D00
      return
      endif
      elseif(NGAUSS.EQ.6)then
      goto 900
      else
      call berror(2)
      endif
      E1(1)=8.832728600D02
      CS1(1)=1.755062800D-02
      E1(2)=1.331292800D02
      CS1(2)=1.228292230D-01
      E1(3)=2.990640790D01
      CS1(3)=4.348835840D-01
      E1(4)=7.978677160D00
      CS1(4)=5.600108040D-01
      E2(1)=1.652325950D01
      CS2(1)=-1.134010030D-01
      CP2(1)=6.854527470D-02
      E2(2)=3.856837080D00
      CS2(2)=-1.772864660D-01
      CP2(2)=3.312254350D-01
      E2(3)=1.092728880D00
      CS2(3)=1.150407930D00
      CP2(3)=7.346078780D-01
      E2(4)=2.955850070D-01
      CS2(4)=1.000000000D00
      CP2(4)=1.000000000D00
      return
900   if(NSPLIT.EQ.2)then
      
      call berror(2)
      elseif(NSPLIT.EQ.3)then
      call berror(2)
      elseif(NSPLIT.EQ.4)then
      call berror(2)
      elseif(NSPLIT.EQ.5)then
      call berror(2)
      else
      
      E1(1)=5.484671660D03
      CS1(1)=1.831074430D-03
      E1(2)=8.252349460D02
      CS1(2)=1.395017220D-02
      E1(3)=1.880469580D02
      CS1(3)=6.844507810D-02
      E1(4)=5.296450000D01
      CS1(4)=2.327143360D-01
      E1(5)=1.689757040D01
      CS1(5)=4.701928980D-01
      E1(6)=5.799635340D00
      CS1(6)=3.585208530D-01
      E2(1)=1.585513340D01
      CS2(1)=-1.107775490D-01
      CP2(1)=7.087426820D-02
      E2(2)=3.673026820D00
      CS2(2)=-1.480262620D-01
      CP2(2)=3.397528390D-01
      E2(3)=1.034345220D00
      CS2(3)=1.130767010D00
      CP2(3)=7.271585770D-01
      E2(4)=2.811389240D-01
      CS2(4)=1.000000000D00
      CP2(4)=1.000000000D00
      return
      endif
1000  if(NGAUSS.EQ.1)then
      
      call berror(2)
      elseif(NGAUSS.EQ.2)then
      call berror(2)
      elseif(NGAUSS.EQ.4)then
      elseif(NGAUSS.EQ.5)then
      
      if(NSPLIT.EQ.2)then
      
      call berror(2)
      goto 1100
      elseif(NSPLIT.EQ.3.OR.NSPLIT.EQ.5)then
      goto 1200
      elseif(NSPLIT.EQ.4)then
      call berror(2)
      goto 1100
      else
      
      E1(1)=2.927122920D03
      CS1(1)=5.380259070D-03
      E1(2)=4.402360610D02
      CS1(2)=4.020133150D-02
      E1(3)=9.993134980D01
      CS1(3)=1.804364600D-01
      E1(4)=2.783870100D01
      CS1(4)=4.647456520D-01
      E1(5)=8.602577920D00
      CS1(5)=4.506029000D-01
      E2(1)=2.090380370D01
      CS2(1)=-1.094195750D-01
      CP2(1)=7.156794460D-02
      E2(2)=4.831641890D00
      CS2(2)=-1.494113590D-01
      CP2(2)=3.450879300D-01
      E2(3)=1.353751870D00
      CS2(3)=1.130961860D00
      CP2(3)=7.228825230D-01
      E2(4)=3.610197530D-01
      CS2(4)=1.000000000D00
      CP2(4)=1.000000000D00
      return
      endif
      elseif(NGAUSS.EQ.6)then
      goto 1100
      else
      call berror(2)
      endif
      E1(1)=1.126162690D03
      CS1(1)=1.747576090D-02
      E1(2)=1.697431570D02
      CS1(2)=1.225230890D-01
      E1(3)=3.818151120D01
      CS1(3)=4.349985020D-01
      E1(4)=1.021203590D01
      CS1(4)=5.598121670D-01
      E2(1)=2.149536670D01
      CS2(1)=-1.110570790D-01
      CP2(1)=6.988875080D-02
      E2(2)=4.989777570D00
      CS2(2)=-1.683221010D-01
      CP2(2)=3.393875100D-01
      E2(3)=1.403573860D00
      CS2(3)=1.143625550D00
      CP2(3)=7.279589810D-01
      E2(4)=3.730318350D-01
      CS2(4)=1.000000000D00
      CP2(4)=1.000000000D00
      return
1100  if(NSPLIT.EQ.2)then
      
      call berror(2)
      call berror(2)
      elseif(NSPLIT.EQ.3)then
      call berror(2)
      call berror(2)
      elseif(NSPLIT.EQ.4)then
      call berror(2)
      call berror(2)
      elseif(NSPLIT.EQ.5)then
      call berror(2)
      call berror(2)
      else
      
      E1(1)=7.001713090D03
      CS1(1)=1.819616900D-03
      E1(2)=1.051366090D03
      CS1(2)=1.391607960D-02
      E1(3)=2.392856900D02
      CS1(3)=6.840532450D-02
      E1(4)=6.739744530D01
      CS1(4)=2.331857600D-01
      E1(5)=2.151995730D01
      CS1(5)=4.712674390D-01
      E1(6)=7.403101300D00
      CS1(6)=3.566185460D-01
      E2(1)=2.084795280D01
      CS2(1)=-1.085069750D-01
      CP2(1)=7.162872430D-02
      E2(2)=4.808308340D00
      CS2(2)=-1.464516580D-01
      CP2(2)=3.459121030D-01
      E2(3)=1.344069860D00
      CS2(3)=1.128688580D00
      CP2(3)=7.224699570D-01
      E2(4)=3.581513930D-01
      CS2(4)=1.000000000D00
      CP2(4)=1.000000000D00
      return
      endif
1200  return
      
      end
C* :1 * 
      
