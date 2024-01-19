
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 binwt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "binwt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 23 "binwt.web"
      subroutine binwt(IX,NEL,IDENT,NBASIS)
      implicit none
      integer i,idel,IDENT,iflag,ilower,In,iolbl,Iout,Ipunch,Ipureg,Irtc
     &rd,isilly,Ititle,Iu1,Iudum,iupper,j,kount,llabel,NBASIS
      integer NEL,numwpc
      double precision Label,iel
      double precision IX
      dimension IX(*)
      dimension IDENT(4)
      common/io/In,Iout,Ipureg
      common/label/Label(500),Ititle(100),Irtcrd(100)
      common/munit/Iu1(4),Ipunch,Iudum(15)
      data llabel/600/
      data numwpc/18/
      data iolbl/2/
      
      
      
      
      
      
      
      
      
      
99001 format(1x,4A4,2x,'NUMBER OF WORDS:',i5,', NBASIS =',i3)
99002 format(20A4)
      
      write(Ipunch,99001)(IDENT(i),i=1,4),NEL,NBASIS
      
      call tread(iolbl,Label,llabel,1,llabel,1,0)
      write(Ipunch,99002)(Ititle(i),i=1,20)
      
      NEL=NEL/2
      write(Ipunch,99003)(IX(i),i=1,NEL)
99003 format(4D20.12)
      
      isilly=0
      if(isilly.EQ.0)return
      
      j=0
      kount=0
      iflag=1
      ilower=1
100   iupper=ilower+numwpc-1
      idel=iupper-NEL
      if(idel.GT.0)then
      iupper=NEL
      iflag=0
      endif
      kount=kount+1
      if(iflag.EQ.1)write(Ipunch,99004)(IX(i),i=ilower,iupper),kount
      if(iflag.EQ.0)write(Ipunch,99004)(IX(i),i=ilower,iupper),(j,i=1,id
     &el),kount
      
99004 format(1x,18A4,i7)
      
      if(iupper.NE.NEL)then
      if(iflag.NE.0)then
      ilower=ilower+numwpc
      goto 100
      endif
      endif
      
      return
      
      end
C* :1 * 
      
