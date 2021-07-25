C     Arrival time data and initial values and From pickup data file 
C
C     Input file name: efile_in.dat
C                    : sfile_in.dat
C     Out put files  : IN2016_Kuma.ad
C                    : AT2016_Kuma.dat
C
      character*40 eqfl, comment
      character sta0*6, blnk*6
      character*1 rankp0,ranks0,php,phs
C
      blnk='      '
C
      open(7, file='efile_in.dat')
C
      open(11,file='IN2016_Kuma.dat')
      open(12,file='AT2016_Kuma.dat')
C
      read(7,*) num_e
C
      do 10 i=1,num_e
      read(7,700) eqfl
  700 format(a40)
C
      open(9, file=eqfl)
C
      read(9, 900) comment
  900 format(a40)
C
      read(9, *) iy0,imo0,id0,ih0,imi0,sec0,elat0,elon0,edep0
      write(11,1100) iy0,imo0,id0,ih0,imi0,sec0,elat0,elon0,edep0
 1100 format(
     1  i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,f5.2,1x,f7.4,1x,f8.4,1x,f5.1)
C
      read(9,*) iy1,imo1,id1,ih1,imi1,sec1
C
    1 continue
C
      read(9,*, end=2)sta0,atp0,rankp0,php,ats0,ranks0,phs
      if(sta0 .eq. blnk) go to 2
C
      atp0 = atp0 + sec1
      ats0 = ats0 + sec1
      call secmh(atp0,ihp,imip,secp)
      call secmh(ats0,ihs,imis,secs)
      ihp1=ih1+ihp
      imip1=imi1+imip
      if(imip1 .ge. 60) then
        ihp1=ihp1+1
        imip1=imip1-60
      endif
      ihs1=ih1+ihs
      imis1=imi1+imis
      if(imis1 .ge. 60) then
        ihs1=ihs1+1
        imis1=imis1-60
      endif
C
      write(12,1200) 
     1         sta0,ihp1,imip1,secp,rankp0,ihs1,imis1,secs,ranks0
 1200 format(a6,4x,2i2,f5.2,a1,5x,2i2,f5.2,a1)
C
      go to 1
    2 continue
C
      if( i .lt. num_e) then
      write(12,1202)
 1202 format(40x)
      endif
C
      if( i .eq. num_e) then
      write(12,1204)
 1204 format('  PPP')
      endif
C
      close(9)
C
   10 continue
C
      stop
      end
      SUBROUTINE SECMH( T , IH , IM , SEC )                                     
      IH  =  T / 3600.0                                                         
      IM  =  ( T - 3600. * FLOAT( IH ) ) / 60.                                  
      SEC  =  T - 3600. * FLOAT( IH ) - 60. * FLOAT( IM )                       
      RETURN                                                                    
      END                                                                       

