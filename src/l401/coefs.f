
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 coefs"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "coefs.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 22 "coefs.web"
      subroutine coefs(EXX,CS,CP,CD,ISHELL)
      implicit none
      double precision CD,CP,CS,EXX
      integer ISHELL
      dimension EXX(*),CS(*),CP(*),CD(*)
      
      if(ISHELL.EQ.2)then
      
      
      EXX(1)=9.942027296D-01
      CS(1)=-9.996722919D-02
      CP(1)=1.559162750D-01
      EXX(2)=2.310313333D-01
      CS(2)=3.995128261D-01
      CP(2)=6.076837186D-01
      EXX(3)=7.513856000D-02
      CS(3)=7.001154689D-01
      CP(3)=3.919573931D-01
      elseif(ISHELL.EQ.3)then
      
      
      EXX(1)=4.828540806D-01
      CS(1)=-2.196203690D-01
      CP(1)=1.058760429D-02
      EXX(2)=1.347150629D-01
      CS(2)=2.255954336D-01
      CP(2)=5.951670053D-01
      EXX(3)=5.272656258D-02
      CS(3)=9.003984260D-01
      CP(3)=4.620010120D-01
      elseif(ISHELL.EQ.4)then
      
      EXX(1)=3.252866494D-01
      CS(1)=-1.930400113D-01
      CP(1)=-1.168492194D-01
      CD(1)=3.589955767D-01
      EXX(2)=1.059284796D-01
      CS(2)=-2.425847807D-01
      CP(2)=4.286810330D-01
      CD(2)=6.478922861D-01
      EXX(3)=4.332211470D-02
      CS(3)=1.271325613D00
      CP(3)=6.821760416D-01
      CD(3)=1.440122856D-01
      else
      
      
      
      EXX(1)=2.227660584D00
      CS(1)=1.543289673D-01
      EXX(2)=4.057711562D-01
      CS(2)=5.353281423D-01
      EXX(3)=1.098175104D-01
      CS(3)=4.446345422D-01
      endif
      return
      
      end
C* :1 * 
      
