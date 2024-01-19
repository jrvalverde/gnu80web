
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 fileio"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "fileio.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 31 "fileio.web"
      subroutine fileio(IOPER,IFILNO,LEN,Q,IPOS)
      implicit none
      integer async,base,end,fclose,fdef,fdefsb,fdelov,fdelr,fdelsp,ffba
     &se,ffend,ffnum,ffrptr,ffwptr,filno,finit,fopen,fread,free,fretln
      integer Fstat,fwait,fwrite,i,Idum,Idump,ierr,IFILNO,In,ind,indp,in
     &ew,Io,IOPER,Iout,IPOS,Ipunch,isav,j,k
      integer kff,knew,kp,l,LEN,Len18,length,lng,maxfil,Maxpos,Mpos,Nfil
     &es,nfnew,nfold,ntcl,ntop,ntpos,ntrd,ntreop,ntrwnd
      integer ntwait,ntwr,num,Nunits,op,oper,pos,remove,retjmp,rwptr,sta
     &rt,thrsh,unit,Units,Wait
      double precision Q(*)
      dimension IFILNO(2)
      character*8 flio
      logical Print,Syncs
      common/fstat/Nfiles,Mpos,Fstat(1000)
      common/io/In,Iout,Ipunch
      common/fidump/Idump
      common/len18/Len18
      common/ntr/Wait(3,4),Io(3,4),Units(4),Nunits,Maxpos(4),Print(4),Sy
     &ncs(4),Idum
      data fdef/0/,fread/2/,fwrite/1/,fwait/3/
      data fdefsb/4/,fdelsp/5/,fdelov/8/
      data fdelr/6/,finit/12/,fclose/10/,fretln/11/
      data fopen/9/
      data ffnum/1/,ffbase/2/,ffend/3/,ffwptr/4/,ffrptr/5/
      data ntrd/2/,ntwr/1/,ntrwnd/10/,ntpos/6/,ntwait/23/,ntcl/22/
      data ntreop/21/
      data free/0/,remove/-1/
      data flio/' FILEIO:'/
      data unit/18/
      data maxfil/200/
      
      
      
      if(Idump.NE.0)write(Iout,99012)flio,IOPER,IFILNO(1),LEN,IPOS
      
      oper=iabs(IOPER)
      async=0
      if(IOPER.LT.0)async=1
      
      
      if(oper.EQ.fwait)then
      call ntran(unit,ntwait,1,Q,l)
      return
      
      
      
      elseif(oper.EQ.finit)then
      
      
      Nfiles=1
      Fstat(1)=0
      Fstat(2)=3+3*maxfil
      Fstat(3)=Len18*4095
      Fstat(4)=0
      Fstat(5)=0
      call ntran(unit,26,1,Q,l)
      i=0
      if(Idump.GE.2)i=1
      call ntran(unit,29,i,Q,l)
      call ntran(unit,ntrwnd,1,Q,l)
      Mpos=Maxpos(1)
      call ntran(unit,-ntwr,3*maxfil+2,Nfiles,l)
      return
      elseif(oper.EQ.fclose)then
      
      
      nfnew=0
      nfold=0
      do 50 i=1,Nfiles
      do 20 j=1,3
      Fstat(nfnew+j)=Fstat(nfold+j)
20    continue
      nfnew=nfnew+3
      nfold=nfold+5
50    continue
      
      Mpos=Maxpos(1)
      call ntran(unit,ntrwnd,1,Q,l)
      call ntran(unit,-ntwr,3*maxfil+2,Nfiles,l)
      call ntran(unit,ntcl,0,Q,l)
      return
      elseif(oper.EQ.fretln)then
      
      
      LEN=0
      k=0
      filno=iabs(IFILNO(1))
      do 100 i=1,Nfiles
      kff=k+ffnum
      if(Fstat(k+ffnum).EQ.filno)goto 1700
      k=k+5
100   continue
      return
      elseif(oper.EQ.fopen)then
      
      
      call ntran(unit,26,0,Q,l)
      i=0
      if(Idump.GE.2)i=1
      call ntran(unit,29,i,Q,l)
      call ntran(unit,ntreop,1,Q,l)
      
      call ntran(unit,-ntrd,3*maxfil+2,Nfiles,l)
      call ntran(unit,ntreop,Mpos,Q,l)
      
      nfnew=5*Nfiles-1
      nfold=3*Nfiles+1
      do 150 i=1,Nfiles
      do 120 j=1,3
      Fstat(nfnew-j)=Fstat(nfold-j)
120   continue
      nfnew=nfnew-5
      nfold=nfold-3
150   continue
      
      
      num=0
      do 200 i=1,Nfiles
      Fstat(num+ffrptr)=Fstat(num+ffbase)
      Fstat(num+ffwptr)=Fstat(num+ffbase)
      num=num+5
200   continue
      return
      else
      if(oper.LE.fdelov.AND.oper.GE.fdelr)goto 1100
      
      
      if(IFILNO(1).NE.0)then
      
      filno=iabs(IFILNO(1))
      
      ind=-5+ffnum
      do 220 i=1,Nfiles
      ind=ind+5
      if(filno.EQ.Fstat(ind))then
      ind=ind-ffnum
      isav=i
      goto 240
      endif
      
220   continue
      
      
      if(oper.EQ.fdef)goto 400
      if(oper.EQ.fwrite)goto 400
      ierr=6
      goto 1800
      
      
      
240   if(oper.EQ.fdef)then
      ierr=7
      goto 1800
      
      elseif(oper.EQ.fwrite.OR.oper.EQ.fread)then
      
      length=LEN*2
      if(length.LT.0)then
      ierr=4
      goto 1800
      endif
      elseif(oper.EQ.fdefsb)then
      
      
      
      if(IFILNO(1).LT.0)Fstat(ind+ffwptr)=Fstat(ind+ffbase)
      pos=IPOS*2
      start=Fstat(ind+ffwptr)+pos
      if(start.GE.Fstat(ind+ffbase).AND.start.LE.Fstat(ind+ffend))then
      
      length=LEN*2
      if(length.GT.0)then
      
      end=start+length
      if(end.LE.Fstat(ind+ffend))then
      
      Fstat(ind+ffwptr)=end
      
      indp=5*Nfiles
      ind=indp-5
      do 242 i=1,5
      Fstat(indp+i)=Fstat(ind+i)
242   continue
      
      Fstat(ind+ffnum)=IFILNO(2)
      Fstat(ind+ffbase)=start
      Fstat(ind+ffend)=end
      Fstat(ind+ffwptr)=start
      Fstat(ind+ffrptr)=start
      
      Nfiles=Nfiles+1
      if(Nfiles.LT.maxfil)return
      ierr=9
      else
      ierr=5
      endif
      else
      ierr=4
      endif
      else
      ierr=3
      endif
      goto 1800
      else
      if(oper.NE.fdelsp)goto 1100
      
      
      
      retjmp=1
      goto 700
      endif
      else
      ierr=1
      goto 1800
      endif
      endif
      
      
      
300   rwptr=ffwptr
      if(oper.EQ.fread)rwptr=ffrptr
      
      pos=IPOS*2
      
      if(IFILNO(1).LT.0)Fstat(ind+rwptr)=Fstat(ind+ffbase)
      
      start=Fstat(ind+rwptr)+pos
      if(start.GE.Fstat(ind+ffbase).AND.start.LE.Fstat(ind+ffend))then
      
      end=start+length
      if(end.LE.Fstat(ind+ffend))then
      
      
      
      if(length.NE.0)then
      call ntran(unit,ntpos,start,Q,l)
      ntop=ntwr
      if(oper.EQ.fread)ntop=ntrd
      if(async.EQ.1)then
      
      write(Iout,99011)flio
      goto 1900
      else
      call ntran(unit,ntop,length,Q,l)
      if(Idump.GT.2)then
      length=LEN
      if(Idump.LE.3.AND.length.GT.500)length=500
      write(Iout,99001)(Q(i),i=1,length)
      
99001 format(1x,10D13.5)
      endif
      endif
      endif
      
      
      Fstat(ind+rwptr)=end
      
      return
      else
      ierr=5
      goto 1800
      endif
      else
      ierr=3
      goto 1800
      endif
      
      
      
400   length=2*LEN
      if(length.GE.0)then
      
      
      
      k=-5
      do 450 i=1,Nfiles
      k=k+5
      if(Fstat(k+ffnum).EQ.free)then
      lng=Fstat(k+ffend)-Fstat(k+ffbase)
      if(length.LE.lng)goto 500
      endif
      
450   continue
      ierr=8
      else
      ierr=4
      endif
      goto 1800
      
      
500   if(length.EQ.lng)Fstat(k+ffnum)=remove
      
      base=Fstat(k+ffbase)
      Fstat(k+ffbase)=base+length
      
      indp=5*Nfiles
      ind=indp-5
      do 600 i=1,5
      Fstat(indp+i)=Fstat(ind+i)
600   continue
      
      Fstat(ind+ffnum)=filno
      Fstat(ind+ffbase)=base
      Fstat(ind+ffend)=base+length
      
      Fstat(ind+ffrptr)=base
      Fstat(ind+ffwptr)=base
      
      Nfiles=Nfiles+1
      if(Nfiles.LT.maxfil)goto 1500
      ierr=9
      
      goto 1800
      
700   Fstat(ind+ffnum)=free
      
      
      k=-5
      start=Fstat(ind+ffbase)
      end=Fstat(ind+ffend)
      do 800 i=1,Nfiles
      k=k+5
      if(i.NE.isav)then
      if(start.GE.Fstat(k+ffbase).AND.end.LE.Fstat(k+ffend))then
      
      Fstat(ind+ffnum)=remove
      goto 900
      endif
      endif
      
800   continue
      
      
900   k=-5
      do 1000 i=1,Nfiles
      k=k+5
      if(i.NE.isav)then
      if(Fstat(k+ffnum).NE.free)then
      if(Fstat(k+ffnum).NE.remove)then
      
      if(start.LE.Fstat(k+ffbase).AND.end.GE.Fstat(k+ffend))Fstat(k+ffnu
     &m)=remove
      endif
      endif
      endif
1000  continue
      if(retjmp.EQ.1)goto 1300
      if(retjmp.EQ.2)goto 1200
      
      
1100  op=oper-7
      if(op.LT.0)then
      thrsh=3000
      elseif(op.EQ.0)then
      
      thrsh=2000
      else
      
      thrsh=1000
      endif
      
      retjmp=2
      isav=0
      ind=-5
1200  isav=isav+1
      ind=ind+5
      if(isav.LE.Nfiles)then
      if(Fstat(ind+ffnum).GE.thrsh)goto 700
      goto 1200
      endif
      
      
      
      
1300  k=0
      do 1400 i=2,Nfiles
      k=k+5
      if(Fstat(k+ffnum).EQ.free)then
      kp=-5
      do 1320 j=1,i
      kp=kp+5
      if(Fstat(kp+ffnum).EQ.free)then
      if(Fstat(kp+ffbase).NE.Fstat(k+ffend))then
      
      if(Fstat(kp+ffend).NE.Fstat(k+ffbase))goto 1320
      Fstat(k+ffbase)=Fstat(kp+ffbase)
      else
      Fstat(k+ffend)=Fstat(kp+ffend)
      endif
      Fstat(kp+ffnum)=remove
      endif
1320  continue
      endif
1400  continue
      
      
1500  k=-5
      knew=0
      inew=0
      
      do 1600 i=1,Nfiles
      k=k+5
      if(Fstat(k+ffnum).NE.remove)then
      inew=inew+1
      
      do 1520 j=1,5
      knew=knew+1
      Fstat(knew)=Fstat(k+j)
1520  continue
      endif
1600  continue
      ind=ind-5*(Nfiles-inew)
      Nfiles=inew
      if(oper.EQ.fwrite)goto 300
      return
      
1700  LEN=(Fstat(k+ffend)-Fstat(k+ffbase))/2
      return
      
      
      
      
1800  if(ierr.EQ.2)then
      
      write(Iout,99003)flio
      elseif(ierr.EQ.3)then
      
      write(Iout,99004)flio,start
      elseif(ierr.EQ.4)then
      
      write(Iout,99005)flio
      elseif(ierr.EQ.5)then
      
      write(Iout,99006)flio
      elseif(ierr.EQ.6)then
      
      write(Iout,99007)flio
      elseif(ierr.EQ.7)then
      
      write(Iout,99008)flio
      elseif(ierr.EQ.8)then
      
      write(Iout,99009)flio
      elseif(ierr.EQ.9)then
      
      write(Iout,99010)flio
      else
      
      write(Iout,99002)flio
      endif
      
1900  write(Iout,99012)flio,IOPER,IFILNO(1),LEN,IPOS
      call fdump
      call lnk1e
      stop
      
99002 format(a8,' CALLED WITH FILE NUMBER OF ZERO.')
99003 format('  ')
99004 format(a8,' START OF FILE OPERATION OUT OF RANGE:',i9)
99005 format(a8,' A NEGATIVE FILE LENGTH WAS DETECTED.')
99006 format(a8,' END OF FILE; OPERATION OUT OF RANGE.')
99007 format(a8,' ATTEMPT WAS MADE TO READ NON-EXISTING FILE.')
99008 format(a8,' ATTEMPT WAS MADE TO RE-DEFINE EXISTING FILE.')
99009 format(a8,' NO FREE SPACE FOUND FOR NEW FILE.')
99010 format(a8,' FSTATS OVERFLOW, TOO MANY FILES.')
99011 format(a8,' ATTEMPT WAS MADE TO USE UNIT NR 18 ASYNCHRONOUSLY')
      
99012 format(a8,' IOPER=',i2,', IFILNO(1)=',i5,', LEN=',i8,', IPOS=',i8)
      
      
      end
C* :1 * 
      
