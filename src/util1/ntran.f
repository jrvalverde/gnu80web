
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 ntran"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "ntran.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 24 "ntran.web"
      subroutine ntran(UNIT,OP,NWRDS,X,IERR)
      implicit none
      integer bufpos,bufr,dif,hop,i,Idum,ier,IERR,iiiiii,In,ind,Io,Iout,
     &Ipunch,iq,iunit,j,len,Len18,Maxpos
      integer ndummy,nops,Nunits,NWRDS,OP,opos,ops,pos,qio,reclen,rpos,s
     &tat,UNIT,Units,X
      dimension X(*)
      real Wait
      logical Syncs,Print,asyn,async,sync
      real cputim
      dimension ops(14)
      dimension pos(4),stat(2,4)
      dimension bufr(4095)
      common/len18/Len18
      common/ntr/Wait(3,4),Io(3,4),Units(4),Nunits,Maxpos(4),Print(4),Sy
     &ncs(4),Idum
      common/io/In,Iout,Ipunch
      data async/.TRUE./,sync/.FALSE./
      data ops/1,2,6,9,10,21,22,23,24,25,26,27,28,29/,nops/14/
      data pos/4*0/,stat/8*0/
      data reclen/4095/,bufr/4095*0/
      
      Iout=6
      ier=0
      hop=iabs(OP)
      
      do 100 i=1,Nunits
      if(UNIT.EQ.Units(i))goto 200
100   continue
      if(hop.EQ.26.OR.hop.EQ.27)then
      
      
      iunit=Nunits+1
      if(iunit.GT.4)then
      write(Iout,99009)OP,UNIT,iunit
      goto 1100
      
      write(Iout,99010)OP,UNIT
      goto 1100
      
      write(Iout,99011)OP,UNIT,NWRDS,rpos
      goto 1100
      else
      Nunits=iunit
      Units(iunit)=UNIT
      Maxpos(iunit)=0
      Syncs(iunit)=async
      if(hop.EQ.26)Syncs(iunit)=.NOT.async
      
      goto 800
      endif
      else
      
      write(Iout,99001)UNIT
      goto 1200
      endif
      
200   iunit=i
      Io(1,iunit)=Io(1,iunit)+1
      if(Print(iunit))write(Iout,99012)UNIT,OP,NWRDS
      asyn=Syncs(iunit)
      do 300 i=1,nops
      if(ops(i).EQ.hop)goto 400
300   continue
      
      write(Iout,99002)OP
      goto 1100
      
400   if(i.EQ.1)then
      elseif(i.EQ.2)then
      elseif(i.EQ.3)then
      
      rpos=NWRDS
      qio=0
      if(asyn)then
      call nwait(stat,iunit,UNIT,ier)
      if(ier.LT.0)goto 1000
      rpos=NWRDS
      if(rpos.LT.1)then
      
      write(Iout,99005)OP,NWRDS,UNIT
      goto 1100
      else
      dif=rpos-pos(iunit)
      pos(iunit)=rpos
      if(pos(iunit).GT.Maxpos(iunit)+1)then
      
      write(Iout,99004)UNIT
      goto 1100
      else
      if(dif.GT.0)goto 500
      if(dif.EQ.0)goto 800
      dif=-dif
      if(dif.GT.rpos)then
      rewind UNIT
      if(Print(iunit))write(Iout,99015)UNIT
      dif=rpos-1
      qio=1
      goto 500
      else
      do 402 i=1,dif
      backspace UNIT
402   continue
      qio=dif
      if(Print(iunit))write(Iout,99018)dif
      goto 600
      endif
      endif
      endif
      else
      rpos=(NWRDS+reclen-1)/reclen
      bufpos=1+mod(NWRDS-1,reclen)
      if(pos(iunit).NE.rpos)then
      if(stat(2,iunit).NE.0)then
      if(Print(iunit))write(Iout,99016)pos(iunit)
      if(pos(iunit).GT.Maxpos(iunit))Maxpos(iunit)=pos(iunit)
      write(UNIT,rec=pos(iunit))bufr
      Io(2,iunit)=Io(2,iunit)+1
      stat(2,iunit)=0
      endif
      pos(iunit)=rpos
      if(pos(iunit).LE.Maxpos(iunit))then
      Io(2,iunit)=Io(2,iunit)+1
      if(Print(iunit))write(Iout,99017)pos(iunit)
      read(UNIT,rec=pos(iunit))bufr
      endif
      endif
      goto 800
      endif
      elseif(i.EQ.5)then
      goto 700
      elseif(i.EQ.6)then
      
      if(asyn)goto 800
      Maxpos(iunit)=NWRDS
      goto 700
      elseif(i.EQ.7)then
      
      if(asyn)then
      
      call nwait(stat,iunit,UNIT,ier)
      if(ier.GE.0)goto 800
      goto 1000
      else
      if(stat(2,iunit).NE.0)then
      if(Print(iunit))write(Iout,99016)pos(iunit)
      if(pos(iunit).GT.Maxpos(iunit))Maxpos(iunit)=pos(iunit)
      write(UNIT,rec=pos(iunit))bufr
      stat(2,iunit)=0
      endif
      Io(2,iunit)=Io(2,iunit)+1
      goto 800
      endif
      elseif(i.EQ.8)then
      
      if(asyn)call nwait(stat,iunit,UNIT,ier)
      if(ier.LT.0)goto 1000
      IERR=ier
      goto 800
      elseif(i.EQ.9)then
      
      write(Iout,99003)OP
      goto 1100
      elseif(i.EQ.10)then
      write(Iout,99003)OP
      goto 1100
      elseif(i.EQ.11.OR.i.EQ.12)then
      goto 800
      elseif(i.EQ.13)then
      goto 800
      elseif(i.EQ.14)then
      
      Print(iunit)=NWRDS.NE.0
      goto 800
      else
      return
      endif
      
      IERR=NWRDS
      len=NWRDS
      ind=1
      Io(3,iunit)=Io(3,iunit)+len
      if(asyn)then
      call nwait(stat,iunit,UNIT,ier)
      if(ier.LT.0)goto 1000
      Wait(3,iunit)=cputim(iq)
      opos=pos(iunit)
      pos(iunit)=opos+1
      if(hop.NE.2)then
      if(Print(iunit))write(Iout,99016)opos
      if(Maxpos(iunit).LT.opos)Maxpos(iunit)=opos
      ndummy=NWRDS
      write(UNIT)ndummy,(X(iiiiii),iiiiii=1,NWRDS)
      
      elseif(opos.GT.Maxpos(iunit))then
      write(Iout,99004)UNIT
      goto 1100
      else
      if(Print(iunit))write(Iout,99017)opos
      read(UNIT)ndummy,(X(iiiiii),iiiiii=1,ndummy)
      NWRDS=ndummy
      endif
      stat(1,iunit)=hop
      stat(2,iunit)=NWRDS
      Io(2,iunit)=Io(2,iunit)+1
      IERR=-1
      if(OP.GT.0)goto 800
      call nwait(stat,iunit,UNIT,ier)
      if(ier.LT.0)goto 1000
      IERR=ier
      goto 800
      else
450   j=reclen-bufpos+1
      if(j.GT.len)j=len
      if(j.GT.0)then
      if(hop.EQ.1)then
      
      stat(2,iunit)=1
      do 460 i=1,j
      bufr(bufpos)=X(ind)
      bufpos=bufpos+1
      ind=ind+1
460   continue
      else
      do 470 i=1,j
      X(ind)=bufr(bufpos)
      bufpos=bufpos+1
      ind=ind+1
470   continue
      endif
      endif
      len=len-j
      if(len.LE.0)goto 800
      if(stat(2,iunit).NE.0)then
      if(pos(iunit).GT.Maxpos(iunit))Maxpos(iunit)=pos(iunit)
      if(Print(iunit))write(Iout,99016)pos(iunit)
      write(UNIT,rec=pos(iunit))bufr
      stat(2,iunit)=0
      endif
      Io(2,iunit)=Io(2,iunit)+1
      pos(iunit)=pos(iunit)+1
      if(pos(iunit).GT.Len18)then
      write(Iout,99004)UNIT
      goto 1100
      elseif(pos(iunit).LE.Maxpos(iunit))then
      
      if(Print(iunit))write(Iout,99017)pos(iunit)
      read(UNIT,rec=pos(iunit),err=900)bufr
      Io(2,iunit)=Io(2,iunit)+1
      bufpos=1
      goto 450
      elseif(hop.EQ.2)then
      write(Iout,99004)UNIT
      goto 1100
      else
      bufpos=1
      goto 450
      endif
      endif
500   if(dif.NE.0)then
      qio=qio+dif
      do 550 i=1,dif
      call nwait(stat,iunit,UNIT,ier)
      if(ier.LT.0)goto 1000
      Wait(3,iunit)=cputim(iq)
      read(UNIT)
      stat(1,iunit)=3
550   continue
      endif
600   stat(2,iunit)=qio
      Io(2,iunit)=Io(2,iunit)+qio
      goto 800
      
700   bufpos=1
      if(pos(iunit).NE.1)then
      if(asyn)then
      
      Io(2,iunit)=Io(2,iunit)+1
      call nwait(stat,iunit,UNIT,ier)
      if(ier.LT.0)goto 1000
      if(Print(iunit))write(Iout,99015)UNIT
      rewind UNIT
      pos(iunit)=1
      else
      if(stat(2,iunit).NE.0)then
      Io(2,iunit)=Io(2,iunit)+1
      if(Print(iunit))write(Iout,99016)pos(iunit)
      if(pos(iunit).GT.Maxpos(iunit))Maxpos(iunit)=pos(iunit)
      write(UNIT,rec=pos(iunit))bufr
      stat(2,iunit)=0
      endif
      Io(2,iunit)=Io(2,iunit)+1
      pos(iunit)=1
      if(Maxpos(iunit).NE.0)then
      if(Print(iunit))write(Iout,99017)pos(iunit)
      read(UNIT,rec=pos(iunit))bufr
      endif
      endif
      endif
      
800   if(.NOT.Print(iunit))return
      if(asyn)write(Iout,99013)pos(iunit),Maxpos(iunit),stat(1,iunit),IE
     &RR
      if(asyn)return
      write(Iout,99014)pos(iunit),Maxpos(iunit),stat(2,iunit),bufpos
      return
      
900   write(Iout,99005)UNIT
      goto 1100
      
1000  write(Iout,99007)UNIT,ier
1100  if(iunit.NE.0)Units(iunit)=-1
      if(asyn)write(Iout,99013)pos(iunit),Maxpos(iunit),stat(1,iunit),IE
     &RR
      if(.NOT.asyn)write(Iout,99014)pos(iunit),Maxpos(iunit),stat(2,iuni
     &t),bufpos
1200  call lnk1e
      stop
      
99001 format(' ','NTRAN **** ERROR **** UNIT NOT DEFINED: ',i10)
99002 format(' ','NTRAN **** ERROR **** UNKNOWN OPERATION: ',i10)
99003 format(' ','NTRAN **** ERROR **** OPERATION NOT IMPLEMENTED:',i5)
99004 format(' ','NTRAN **** ERROR **** END OF DATA SET, UNIT =  ',i10)
99005 format(' ','NTRAN **** ERROR **** DURING READ, UNIT =  ',i10)
99006 format(' ','NTRAN **** ERROR, OP= ',i5,'NWRDS= ',i5,'UNIT= ',i5)
99007 format(' ','NTRAN **** ERROR **** PREVIOUS READ OR WRITE NOT ','SU
     &CCESFULLY COMPLETED ON UNIT NR ',i2,' IERR= ',i2)
99008 format(' ','NTRAN:WARNING REPOSITION ON ASYNCHRONOUS UNIT:',i5,' P
     &OS= ',i5)
99009 format(' ','NTRAN **** ERROR, OP= ',i5,'UNIT= ',i5,' UNIT CANNOT B
     &E DEFINED SYNCHRONOUS: ',i5)
99010 format(' ','NTRAN **** ERROR, OP= ',i5,'UNIT= ',i5,' UNIT CANNOT B
     &E DEFINED, TO MANY UNITS: ',i5)
99011 format(' ','NTRAN **** ERROR, OP= ',i5,'UNIT= ',i5,' UNIT DEFINED 
     &TO LARGE N WORDS= ',i10,' N. RECORDS= ',i5)
99012 format(' ','NTRAN: UNIT= ',i5,', OP = ',i5,', NWRDS  =',i5)
99013 format(' ','  "  : POS = ',i5,', EOF= ',i5,', STATUS =',i5,' IERR=
     & ',i5)
99014 format(' ','  "  : POS = ',i5,', EOF= ',i5,', STATUS =',i5,' BUFPO
     &S= ',i5)
99015 format(' ','NTRAN: REWIND, UNIT = ',i5)
99016 format(' ','NTRAN: WRITE,  BUF # =  ',i5)
99017 format(' ','NTRAN: READ,   BUF # =  ',i5)
99018 format(' ','NTRAN: BACKSPACE,      ',i5,' RECORDS')
      
      end
C* :1 * 
      
