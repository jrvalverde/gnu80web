
C FTANGLE v1.61,
C created with UNIX on "Friday, September 25, 1998 at 8:02." 
C  COMMAND LINE: "ftangle -ybs15000 matprt"
C  RUN TIME:     "Friday, June 5, 2009 at 15:05."
C  WEB FILE:     "matprt.web"
C  CHANGE FILE:  (none)
      C* 1: * 
*line 29 "matprt.web"
      subroutine matprt(A,MD,ND,NROWS,NCOLS,LROW,LCOL,ROWLAB,COLLAB,ISYM
     &,EIG,IFEIG)
      implicit none
      integer blank,colcur,coldat,COLLAB,collim,colmax,colmin,getchr,i,I
     &FEIG,In,Iout,ipass,Ipunch,ISYM,j,LCOL,lcur,len,line
      integer LROW,ltab,maxlen,MD,NCOLS,ND,npass,NROWS,row,rowcur,ROWLAB
     &,rowm1,rowmax,rowmin,rowwid,tcur,tmp
      double precision A(MD,ND),EIG(NCOLS)
      dimension tmp(16),line(80),ROWLAB(*),COLLAB(*)
      common/io/In,Iout,Ipunch
      data coldat/5/,blank/1H /
      
      
      
      
      
      
99001 format(22x,10(i6,4x))
99002 format(5x,15HEIGENVALUES -- ,1x,10F10.5)
99003 format(1x,80A1)
99004 format(1x,i9,11x,5F10.5)
99005 format(1x,10x,8A1,2x,5F10.5)
99006 format(1x,2x,16A1,2x,5F10.5)
99007 format(1x,4x,i3,2x,8A1,3x,5F10.5)
99008 format(1x,i3,1x,16A1,5F10.5)
      
      
      rowmin=1
      rowmax=NROWS
      colmin=0
      colmax=0
      colcur=0
      
      maxlen=0
      rowwid=8
      if(LROW.NE.0)then
      rowcur=0
      do 50 i=1,NROWS
      call getb(2,tmp,len,ROWLAB,rowcur)
      if(len.GT.maxlen)maxlen=len
50    continue
      if(maxlen.GT.8)rowwid=16
      rowcur=0
      endif
      
      npass=(NCOLS-1)/coldat+1
      if(ISYM.LT.0)npass=(NROWS-1)/coldat+1
      
      do 200 ipass=1,npass
      
      colmin=colmax+1
      colmax=colmax+coldat
      colmax=min0(NCOLS,colmax)
      
      if(ISYM.EQ.1)rowmin=colmin
      if(ISYM.LT.0)rowmin=-ISYM
      if(ISYM.LT.0)rowmax=-ISYM
      
      rowcur=0
      if(ISYM.NE.0.AND.LROW.NE.0)then
      if(rowmin.NE.1)then
      rowm1=rowmin-1
      do 60 j=1,rowm1
      call skip(2,ROWLAB,rowcur)
60    continue
      endif
      endif
      
      if(LCOL.NE.1)write(Iout,99001)(i,i=colmin,colmax)
      
      if(LCOL.NE.0)then
      do 80 i=1,80
      line(i)=blank
80    continue
      ltab=26
      do 100 j=colmin,colmax
      call getb(2,tmp,len,COLLAB,colcur)
      len=min0(len,8)
      lcur=ltab-len/2
      tcur=0
      do 90 i=1,len
      lcur=lcur+1
      line(lcur)=getchr(tmp,tcur)
90    continue
      ltab=ltab+10
100   continue
      
      write(Iout,99003)(line(i),i=1,lcur)
      endif
      
      if(IFEIG.NE.0)write(Iout,99002)(EIG(i),i=colmin,colmax)
      
      do 150 row=rowmin,rowmax
      
      if(LROW.NE.0)then
      if(rowcur.LE.4000)call getb(2,tmp,len,ROWLAB,rowcur)
      do 110 i=1,16
      line(i)=blank
110   continue
      len=min0(len,rowwid)
      tcur=0
      if(rowcur.LE.4000)then
      do 115 i=1,len
      line(i)=getchr(tmp,tcur)
115   continue
      endif
      endif
      
      collim=colmax
      if(ISYM.GT.0)collim=min0(row,colmax)
      
      
      
      if(LROW.EQ.0)write(Iout,99004)row,(A(row,j),j=colmin,collim)
      
      if(LROW.EQ.1.AND.rowwid.EQ.8)write(Iout,99005)(line(i),i=1,8),(A(r
     &ow,j),j=colmin,collim)
      if(LROW.EQ.1.AND.rowwid.EQ.16)write(Iout,99006)(line(i),i=1,16),(A
     &(row,j),j=colmin,collim)
      
      if(LROW.EQ.2.AND.rowwid.EQ.8)write(Iout,99007)row,(line(i),i=1,8),
     &(A(row,j),j=colmin,collim)
      
      if(LROW.EQ.2.AND.rowwid.EQ.16)write(Iout,99008)row,(line(i),i=1,16
     &),(A(row,j),j=colmin,collim)
      
150   continue
200   continue
      return
      
      end
C* :1 * 
      
