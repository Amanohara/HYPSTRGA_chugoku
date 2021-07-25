C     HYPOCENTER DETERMINATION                                                  
C     HORIZONTAL LAYERED STRUCTURE                                              
C     Hypocenter determination from selected data set
C           Revised on May 16, 2001.
C           Revised on June 27, 2011.
C           Revised on April 26, 2018.
C
C     Dimension declaration
C
      DIMENSION HL(20) , VP(20) , VS(20)                                        
      DIMENSION IA(100) , CORP(100) , CORS(100) , ALAT(100) , ALON(100)         
     1             , AHIGH(100) , STX(100) , STY(100)                           
      DIMENSION IAI(100) , IY(100) , IM(100) , ID(100) , IUD(100) ,             
     1             IP(100) , IHP(100) , IMP(100) , SECP(100) ,                  
     2             IVS(100) , IS(100) , IHS(100) , IMS(100) , SECS(100)         
     3             , MFP(100) , FP(100) , COM( 100 , 5 )                        
     4             ,AM(100) ,BM(100)                                            
      DIMENSION FMSHD( 9 ) , FMSHH( 9 ) , CRFSP( 9 , 9 ) , CRFSS( 9 , 9)        
      DIMENSION TP(100) , TP0(100) , TS(100) , TS0(100) , IL(100)               
      DIMENSION WP(100) , WS(100)                                               
      DIMENSION R(100) , AZ(100) , TPC(100) , TSC(100)                          
     1             , PDTDD(100) , SDTDD(100) , PDTDH(100) , SDTDH(100)          
      DIMENSION Y( 200 )                                                        
      DIMENSION A( 200 , 4 )                                                    
      DIMENSION W( 200 )                                                        
      DIMENSION WA( 4 )                                                         
      DIMENSION SOL( 4 ) , SIG( 4 ) , RSL( 4 , 4 ) , RSD( 200 )                 
      DIMENSION X0( 4 ) , DX( 4 )                                               
      DIMENSION ANGP( 100 ) , ANGS( 100 )    
C
C     Common statements
C                                   
      COMMON/V/ HL , VP , VS , NL , NLM                                         
      COMMON/S/ IA , CORP , CORS , ALAT , ALON , AHIGH , STX , STY , NST        
     1          ,AM ,BM                                                         
      COMMON/P/ IAI , IY , IM , ID , IUD , IP , IHP , IMP , SECP ,              
     1             IVS , IS , IHS , IMS , SECS , MFP , FP , COM , N             
      COMMON/M/ ALAT0 , ALON0 , NSTOP , FPS                                     
      COMMON/F/ FMSHD , FMSHH , CRFSP , CRFSS                                   
      COMMON/T/ TP , TP0 , TS , TS0 ,IHX, IL                                    
      COMMON/I/ IY0,IM0,ID0,IH0,IMI0 , SEC0 , EX , EY , ED , T0,FMW         
      COMMON/W/ WP , WS , NWP , NWS                                             
      COMMON/DTR/ R , AZ , TPC , TSC , PDTDD , SDTDD , PDTDH , SDTDH            
      COMMON/WV/ W , NW                                                         
      COMMON/A/ A , NA                                                          
      COMMON/PW/ WA , NPW                                                       
      COMMON/Y/ Y , NY , NP , NS                                                
      COMMON/LS/ SOL , SIG , RSD , RSL , VAR                                    
      COMMON/CV/ X0 , DX , ICNV                                                 
      COMMON/AG/ ANGP , ANGS                                                    
      COMMON/OT/ NEQ                                                            
      COMMON/O/ PKFN1, CMNT1
C 
C     Character statements
C                                                                              
      CHARACTER*80 DSNV, DSNS, DSNFL, DSNHP, DSPKF                          
      CHARACTER*40 CMNT1                  
      CHARACTER*23 PKFN,PKFN1   
      CHARACTER*4 COM                                                           
      CHARACTER*6 IA , IAI                                                      
      CHARACTER*1 IUD , IP , IVS , IS , MFP                                     
C                                                                               
C     FIXED DEPTH                                                               
C                                                                               
      ED1 = 0.2                                                                 
      ED2 = 3.0                                                                 
C                                                                               
C     ED1 --- FOR RETRIAL                                                       
C     ED2 --- FIXED                                                             
C                                                                               
C     LP OUTPUT (LWRT = 1 ) OR NOT                                              
C                                                                               
C     ICNV = 0 --- NOT CONVERGENCE                                              
C            1 --- CONVERGENCE                                                  
C            2 --- CONVERGENCE WITH SLIGHT OSCILLATION                          
C     
      LWRT = 1                                                                  
      LWRTE = 1                                                                 
      LWRTS = 1                                                                 
C                                                                               
c
   99 CONTINUE      
C                                                                               
C     OPEN FILE                                                                 
C                                                                               
      OPEN( 11 , FILE = "dsns_2018.dat"  )                                 
      OPEN( 12 , FILE = "fldt2018"      )                                
      OPEN( 13 , FILE = "dsnv2018"  )                                 
      OPEN( 15 , FILE = "hypout2018all.txt" )
      OPEN( 16 , FILE = "st_oc_2018.dat" )
      OPEN( 17 , FILE = "hypo_2018.dat" )

C
      open(20, FILE='PSDATA2018.dat')                                
C                                                                               
C     START   
C                                                                  
C     SETTING PARAMETERS                                                        
C                                                                               
      CALL SETPAR( 1 )                                                          
C                                                                               
C     SETTING WEIGHTS OF PARAMETERS                                             
C                                                                               
      CALL PARW( 1 )                                                            
C                                                                               
C     INPUT *** VELOCITY DATA                                                   
      CALL VDATA( 1 )                                                           
C                                                                               
C     INPUT *** STATION DATA                                                    
C                                                                               
      CALL STDATA( 1 )                                                          
C                                                                               
      IF( LWRT .EQ. 1 ) WRITE( 15 , 605 )                                        
  605 FORMAT(/,2('--------+--------+----------+----------+--------+'),/)        
C
C     IF multiple earthquake are to be located, MULTI_EQ to be not 0.
      MULTI_EQ=0
C                                                                               
      NEQ  =  0                                                                 
C                                                                               
    1 CONTINUE                                                                  
C
      CALL RESET                                                                
C
C                                                                               
C     P AND S ARRIVAL TIME DATA                                                 
C                                                                               
C     WRITE(15 , 605 )                                                          
C                                                                               
      CALL PSDAT2018( 1   , IDX )                                                 
      IF( IDX .EQ. 1 ) GO  TO  900                                              
      NEQ  =  NEQ  +  1                                                         
C                                                                               
C                                                                               
C                                                                               
C     ITERATION OF LOCATING EARTHQUAKE                                          
C                                                                               
C     FIRST --- FOUR PARAMETERS FREE                                            
C     SECOND -- IF DEPTH BECOMES LESS THAN 0,                                   
C               THEN PUT THE DEPTH AT 1KM DEPTH AND                             
C               RETRY OF ITERATION.                                             
C     THIRD --- IF DEPTH BECOMES LESS THAN 0 IN THE SECOND                      
C               TRIAL, THEN FIX THE DEPTH AT 5KM AND                            
C               SEARCH THE EPICENTER AND ORIGIN TIME.                           
C                                                                               
C     IDFXP  ---  0 : FIRST TRIAL                                               
C                 1 : SECOND TRIAL OR THIRD                                     
C     IDFX   ---  0 : DEPTH FREE                                                
C                 1 :       FIXED                                               
C                                                                               
C      IDFXP = 1                                                                 
C                                                                               
      IDFX  =  0                                                                
C                                                                               
      ED2 = 0.0                                                                 
C                                                                               
C                                                                               
      CALL SETDAT( 1  )    
C
C                                                     
      ITER  =  0                                                                
C                                                                               
    6 CONTINUE                                                                  
C                                                                               
C     CALL INITP(  1   )                                                        
C                                                                               
      X0( 1 ) = EX                                                              
      X0( 2 ) = EY                                                              
      X0( 3 ) = T0                                                              
      X0( 4 ) = ED                                                             
C 
    2 CONTINUE                                                                  
      ITER  =  ITER  +  1                                                       
      IF( ITER .GT. NSTOP ) GO  TO  5                                           
      CALL DTDRV                                                                
C                                                                               
C                                                                               
      CALL WEIP                                                                 
C                                                                               
      CALL SETVEC                                                               
C
C                                                                             
C                                                                               
    3 CONTINUE                                                                  
      CALL AMTRX( IDFX )                                                        
C                                                                               
C                                                                               
      CALL WVCTR                                                                
C                                                                               
C                                                                               
      CALL LSTSQ( IDFX )                                                        
C                                                                               
C                                                                               
C     SOLUTION VECTOR SOL( I )  ---  DX( I )                                    
      DO  10  I  =  1 , 4                                                       
      DX( I ) = SOL( I )                                                        
   10 CONTINUE
C
C     DX( 4 ) = 0.0                                                         
C                                                                               
      CALL CNVSOL(IDFX)                                                         
C                                                                               
      IF(IDFX .eq. 0) go to 4
C                                                                               
      IF( IDFX .EQ. 1 ) GO  TO  4                                               
      H  =  X0( 4 )                                                             
      IF( H .GE. 0 ) GO  TO  4                                                  
C                                                                               
C     PUT THE DEPTH AT 1KM AND RETRY LOCATING EARTHQUAKE.                       
C                                                                               
      IF( IDFXP .EQ. 0 ) THEN                                                   
          ED = ED1                                                              
          IDFXP = 1                                                             
          X0(4) = ED                                                            
          GO TO 3                                                               
      END IF                                                                    
C                                                                               
C     DEPTH IS CONSTRAINED.                                                     
c     IDFX  =  1                                                                
c     ED  =  ED2                                                                
c     X0( 4 ) = ED                                                              
c     GO  TO  3                                                                 
    4 CONTINUE                                                                  
      EX  =  X0( 1 )                                                            
      EY  =  X0( 2 )                                                            
C     ED  =  ED2                                                                
C     IF( IDFX .EQ. 1 ) ED = 5.0                                                
      T0  =  X0( 3 )                                                            
      ED = X0(4)
C                                                                               
    7 CONTINUE
      IF( LWRT .EQ. 1 .AND. ITER .GE. 95 ) CALL OUTPUT2018( 0 , 0 )                 
C                                                                               
      IF( ITER .GT. 100) go to 5
      IF( ITER .LT. 30 .AND. ICNV .EQ. 1 ) GO TO 5                              
      IF( ITER .GE. 30 .AND. ICNV .EQ. 2 ) GO TO 5                              
      GO TO 2                                                                   
    5 CONTINUE                                                                  
C                                                                               
C     OUTPUT                                                                    
C                                                                               
      CALL DTDRV                                                                
C                                                                               
      IF( LWRT .EQ. 1 ) WRITE(15 , 604 ) ITER                                   
  604 FORMAT(/,'====== ITERATION NUMBER=' , I3 , '   =====')               
C                                                                               
      CALL OUTPUT2018( LWRTE, 1    )                                                
C     ED2 = ED2 + 2.0                                                           
      IF( MULTI_EQ .NE. 0) GO  TO  1                                                                 
  900 CONTINUE                                                                  
      CLOSE( 11 )                                                               
      CLOSE( 12 )                                                               
      CLOSE( 13 )                                                               
      CLOSE( 15 )
      CLOSE( 16 ) 
      CLOSE( 20 )                                                              
      STOP                                                                      
      END
C
C     Subroutine VDATA
C                                                                       
      SUBROUTINE VDATA( IW )                                                    
C     READING OF VELOCITY DATA                                                  
      DIMENSION HL(20) , VP(20) , VS(20)                                        
      DIMENSION D( 20 )                                                         
      COMMON/V/ HL , VP , VS , NL , NLM                                         
      NL  =  0                                                                  
    1 CONTINUE                                                                  
      NL  =  NL  +  1                                                           
      READ( 13, *, end=200 ) D( NL ) , VP( NL ) , VS( NL )                             
C                                                       
      IF( VP( NL ) .GE. 0.0001 ) GO  TO  1                                      
  200 NL  =  NL  -  1                                                           
      NLM  =  NL  -  1                                                          
C                                                                               
      DO  10  I  =  1 , NLM                                                     
      I1  =  I  +  1                                                            
      HL( I ) = D( I1 ) - D( I )                                                
   10 CONTINUE                                                                  
      HL( NL ) = 9999.9                                                         
C     D    : DEPTH OF UPPER SURFACE OF LAYER                                    
C     HL   : THICKNESS OF LAYER                                                 
      IF( IW .NE. 1 ) RETURN                                                    
      WRITE( 15 , 600 )                                                          
  600 FORMAT( // , 12X , 'NO.' , 5X , 'DEPTH' , 1X , 'THICKNESS' ,         
     1          8X , 'VP' , 8X , 'VS' , 5X , 'VP/VS'  )                         
      DO  20  I  =  1 , NL                                                      
      VPVS  =  VP( I ) / VS( I )                                                
      WRITE( 15 , 601 ) I , D( I ) , HL( I ) , VP( I ) , VS( I ) , VPVS          
  601 FORMAT( 10X , I5 , 2F10.2 , 3F10.3  )                                
   20 CONTINUE                                                                  
      RETURN                                                                    
      END 
C
C     Subroutine STDATA
C                                                                      
      SUBROUTINE STDATA( IW )                                                   
C     READING OF STATION DATA                                                   
      DIMENSION IA(100) , CORP(100) , CORS(100) , ALAT(100) , ALON(100)         
     1             , AHIGH(100) , STX(100) , STY(100) ,AM(100),BM(100)          
C                                                                               
      COMMON/S/ IA , CORP , CORS , ALAT , ALON , AHIGH , STX , STY , NST        
     1          ,AM , BM                                                        
      COMMON/M/ ALAT0 , ALON0 , NSTOP , FPS                                     
      CHARACTER*6 IA                                                            
C                                                                               
C     F-P MAGNITUDE    --- MFP                                                  
C         MFP = AM + BM * LOG10( F-P )                                          
C                                                                               
      NST  =  0                                                                 
    1 NST  =  NST  +  1                                                         
      READ( 11, *, end=2 ) IA( NST ), ALAT(NST), ALON(NST),AHIGH(NST)
C
      write(6,604) IA( NST ), ALAT(NST), ALON(NST),AHIGH(NST)
  604 FORMAT( A6 , 4X , 3F10.4 ,  2F6.4 )
C
C    1               , CORP(NST), CORS(NST)                         
C  500 FORMAT( A6 , 4X , 3F10.4 ,  2F6.4 )                                  
C
      CORP(NST)=0.0
      CORS(NST)=0.0
C                                                                               
      IF( ALON( NST ) .GT. 0.0001 ) GO  TO  1                                   
    2 NST  =  NST  -  1                                                         
C                                                                               
      DO  10  I  =  1 , NST                                                     
      CALL PRTOXY( ALAT( I ) , ALON( I ) , ALAT0 , ALON0 , STX( I ) ,           
     1             STY( I ) , 0 )                                               
   10 CONTINUE                                                                  
      IF( IW .NE. 1 ) RETURN                                                    
      WRITE( 15 , 600 )                                                          
  600 FORMAT( // , ' STATION DATA '  )                                     
      WRITE( 15 , 601 )                                                          
  601 FORMAT(/, 6X , 'STA' , 2X , 'CORP' , 1X , 'CORS' , 15X ,              
     1             'LAT' , 7X , 'LON' ,7X ,'HIGHT' , 12X ,'X' ,9X,'Y')                                  
C                                                                               
      DO  20  I  =  1 , NST                                                     
      WRITE( 15 , 602 ) IA( I ) , CORP( I ) , CORS( I ) , ALAT( I ) ,            
     1             ALON( I ) , AHIGH( I ) , STX( I ) , STY( I )                
  602 FORMAT(  4X , A6 , 2F6.2 , 10X , 3F10.4 , 5X , 2F10.2 )              
   20 CONTINUE                                                                  
      RETURN                                                                    
      END 
C
C     Subroutine PSDAT2018
C                                                                      
      SUBROUTINE PSDAT2018(IW , IDX )                                             
      DIMENSION IAI(100) , IY(100) , IM(100) , ID(100) , IUD(100) ,             
     1             IP(100) , IHP(100) , IMP(100) , SECP(100) ,                  
     2             IVS(100) , IS(100) , IHS(100) , IMS(100) , SECS(100)         
     3             , MFP(100), FP(100), COM( 100 , 5 ),ERP(100),ERS(100)                        
      DIMENSION JP(100) , JS(100)                                               
      COMMON/P/ IAI , IY , IM , ID , IUD , IP , IHP , IMP , SECP ,              
     1             IVS , IS , IHS , IMS , SECS , MFP , FP , COM , N             
      COMMON/J/ JP , JS  , NN                                                   
      COMMON/I/ IY0,IM0,ID0,IH0, IMI0 , SEC0 , EX , EY , ED , T0,FMW         
      COMMON/M/ ALAT0 , ALON0 , NSTOP , FPS                                     
      COMMON/O/ PKFN1, CMNT1 
      CHARACTER*1 IP,IS,JS,JP,IUD,IVS,LX,BL,RA,RB,RC,RD,RS,MFP,BB
      CHARACTER*1 DMP,DMS                        
      CHARACTER*2 c1,c2                        
      CHARACTER*6 IAI, BL6                                                           
      CHARACTER*4 COM                                                           
      CHARACTER*23 PSFL , PKFN1                                                        
      CHARACTER*40 CMNT,CMNT1                                                         
C                                                                               
      DATA LX/'X'/ , BL/' '/, RD/'D'/, RS/'*'/, RA/'A'/, 
     1     RB/'B'/, RC/'C'/,BB/'B'/ ,BL6/'      '/                               
C                                                                               
C     IDX : OUTPUT
      IDX  =  0    
      READ(20, 2000, end=910) CMNT                                     
 2000 FORMAT( a40)
      IF(IW .EQ. 1 ) WRITE( 15 , 606 )  CMNT                           
  606 FORMAT(//, '**********',10x,A40,'**********') 
C                                 
      read(20, *, end=910 ) IY0,IM0,ID0,IH0,IMI0,SEC0,ELAT0,ELON0, ED,FMW  
C
      if(iy0 .eq. 0) go to 910
C
      T0=FLOAT(IMI0)*60.+SEC0
C
      CALL PRTOXY( ELAT0 , ELON0 , ALAT0 , ALON0 , EX , EY , 0 )                
      write(15, 607) IY0,IM0,ID0,IH0,IMI0,SEC0, ELAT0, ELON0, ED,FMW
  607 FORMAT( I4, 4(1x, I2), 1x, F7.2, 5x, 3(F8.4, 2x), f5.1)
      WRITE(15, 608) T0, EX, EY, ED
  608 FORMAT(15x, F9.2, 5x,3(F8.4, 2x))
C
      read(20,*) IYE,IME,IDE,IHE,IMIE,SECE
C
      N  =  0                                                                   
    1 CONTINUE                                                                  
      N  =  N  +  1                                                             
C     READ( 20, 2004,end=900 )                                                          
C    1             IAI(n), SECP(n), IP(n), SECS(n), IS(n)
C
C2004 format( a6, 2x, f6.3, 1x, a1, 2x, f6.3, 1x, a1) 
C
      READ( 20, *,end=900 )                                                          
     1             IAI(n), SECP(n), IP(n),DMP, SECS(n), IS(n),DMS
      SECP(n)=SECP(n)+SECE
      SECS(n)=SECS(n)+SECE
C
C2004 format( a6, 2x, f6.3, 1x, a1, 2x, f6.3, 1x, a1) 

      IF(IAI(n) .eq. BL6) THEN
       N=N-1
       GO TO 2
      ENDIF
C                                                                               
      IY(N)=IYE
      IM(N)=IME
      ID(N)=IDE
      IHP(N)=IHE
      IMP(N)=IMIE
      IHS(N)=IHE
      IMS(N)=IMIE
C
      go to 1
C
    2 continue
  900 continue
C 
      NN=N-1
C                                                                              
      DO  20  I  =  1 , NN                                                       
      JP( I ) = IP( I )                                                         
      JS( I ) = IS( I )                                                         
   20 CONTINUE                                                                  
      IF( IW .NE. 1 ) GO  TO  900                                               
      WRITE( 15 , 600 )                                                          
  600 FORMAT(  // , '       ARRIVAL TIME DATA '  )                          
      WRITE( 15 , 601 )                                                          
  601 FORMAT( 7X , 'STA' , 3X , 'DATE' , 9X ,'----P DATA-----',           
     1        3X , '----S DATA-----', 6X,'F-P' , 7X , 'COMMENT'  )              
      DO  10  I  =  1 , NN                                                       
      WRITE( 15 , 602 )                                                          
     1             IAI(I) , IY(I) , IM(I) , ID(I) ,  IP(I) ,            
     2             IHP(I) , IMP(I) , SECP(I) ,  IS(I) ,                 
     3             IHS(I) , IMS(I) , SECS(I)                  
  602 FORMAT(      4X , A6 , 2X , I4,2I3 , 2X ,   A1 , 2X ,                
     1             2I3 , F6.2 , 2X , A1 , 2X , 2I3 , F6.2 )                
   10 CONTINUE                                                                  
      RETURN                                                                    
  910 CONTINUE                                                                  
      IDX  =  1                                                                 
      RETURN                                                                    
      END     
C
C     Subroutine SETPAR
C                                                                  
      SUBROUTINE SETPAR( IW )                                                   
      DIMENSION FMSHD( 9 ), FMSHH( 9 ), CRFSP( 9 , 9 ), CRFSS( 9 ,9)
      COMMON/F/ FMSHD , FMSHH , CRFSP , CRFSS                                     
      COMMON/M/ ALAT0 , ALON0 , NSTOP , FPS                                     
      COMMON/WR/ WA , WB , WC , WD                                              
      COMMON/MW/ W1 , W2 , D1 , D2 , FD1 , FD2
C
      CALL FLSP2( FMSHD , FMSHH , CRFSP , 0 )                                     
      CALL FLSP2( FMSHD , FMSHH , CRFSS , 1 )                                     
      ALAT0  =  36.0                                                            
      ALON0  =  140.0                                                          
      NSTOP  =  100                                                             
      FPS  =  1.0  /  3.0                                                       
      SIGA  =  0.1                                                              
      SIGB = 0.3                                                                
      SIGC = 1.0                                                                
      SIGD = 5.0                                                                
      SIGA2 = SIGA ** 2                                                         
      SIGB2 = SIGB ** 2                                                         
      SIGC2 = SIGC ** 2                                                         
      SIGD2 = SIGD ** 2                                                         
      WA  =  1.0 / SIGA2                                                        
      WB  =  1.0 / SIGB2                                                        
      WC  =  1.0 / SIGC2                                                        
      WD  =  1.0 / SIGD2                                                        
      D1  =  50.0                                                               
      D2  =  200.0                                                              
      W1  =  1.0                                                                
      W2  =  0.2                                                                
      FD2  =  ( W2 - W1 ) / ( D2 - D1 )                                         
      FD1  =  W1 - FD2 * D1                                                     
      IF( IW .NE. 1 ) GO  TO  900                                               
      WRITE( 15 , 600 )                                                          
  600 FORMAT(  // , 30( '====' ) )                                          
      WRITE( 15 , 610 ) ALAT0 , ALON0                                            
  610 FORMAT( 10X , 'LAT0=' , F10.3 , 5X , 'LON0=' , F10.3 )              
      WRITE( 15 , 612 ) NSTOP                                                    
  612 FORMAT( 10X , 'NUMBER OF ITERATION =' , I5 )                        
      WRITE( 15 , 614 ) FPS                                                      
  614 FORMAT( 10X , 'WS/WP=' , F7.4 )                                     
      WRITE( 15 , 630 )                                                          
  630 FORMAT( '   WEIGHT FOR READING RANK '  )                            
      WRITE( 15 , 632 ) WA , WB , WC , WD                                        
  632 FORMAT(  5X , 'WA=' , F7.2 , 5X , 'WB=' , F7.2 , 5X ,                 
     1             'WC=' , F7.2 , 5X , 'WD=' , F7.2    )                        
      WRITE( 15 , 620 )                                                          
  620 FORMAT( /, '  WEIGHT FOR EPICENTER DISTANCE'  )                        
      WRITE( 15 , 622 ) D1 , W1                                                  
      WRITE( 15 , 624 ) D1 , D2 , FD1 , FD2                                      
      WRITE( 15 , 626 ) D2 , W2                                                  
  622 FORMAT( '    0KM---D---' , F5.1 , 'KM' , 3X , 'W=' , F5.2 )          
  624 FORMAT(  F5.1 , 'KM---D---' , F5.1 , 'KM' , 3X ,                      
     1                   'W=' , F10.4 , '+' , F10.4 , '*D ' )                   
  626 FORMAT( F5.1 , 'KM---D---' , 10X , 'W=' , F5.2  )                    
      WRITE( 15 , 600 )                                                          
  900 CONTINUE                                                                  
      RETURN                                                                    
      END
C
C     Subroutine SETDAT
C                                                                       
      SUBROUTINE SETDAT( IWRT )                                                 
      DIMENSION IA(100) , CORP(100) , CORS(100) , ALAT(100) , ALON(100)         
     1             , AHIGH(100) , STX(100) , STY(100)                           
      DIMENSION IAI(100) , IY(100) , IM(100) , ID(100) , IUD(100) ,             
     1             IP(100) , IHP(100) , IMP(100) , SECP(100) ,                  
     2             IVS(100) , IS(100) , IHS(100) , IMS(100) , SECS(100)         
     3             , MFP(100) , FP(100) , COM( 100 , 5 )                        
     4             , AM(100) , BM(100)                                          
      COMMON/I/ IY0 ,IM0,ID0,IH0, IMI0 , SEC0 , EX , EY , ED , T0,FMW         
      DIMENSION TP(100) , TP0(100) , TS(100) , TS0(100) , IL(100)               
      DIMENSION JP(100) , JS(100)                                               
      COMMON/S/ IA , CORP , CORS , ALAT , ALON , AHIGH , STX , STY , NST        
     1          , AM , BM                                                       
      COMMON/P/ IAI , IY , IM , ID , IUD , IP , IHP , IMP , SECP ,              
     1             IVS , IS , IHS , IMS , SECS , MFP , FP , COM , N             
      COMMON/T/ TP , TP0 , TS , TS0 ,IHX, IL                                    
      COMMON/J/ JP , JS , NN                                                    
      CHARACTER*4 COM                                                           
      CHARACTER*6 IA , IAI                                                      
      CHARACTER*1 IP , IS , JP , JS , IBLNK , IUD , IVS,ISTR, MFP               
      DATA IBLNK/' '/,ISTR/'*'/                                                 
      DO  10  I  =  1 , 100                                                     
      TP( I ) = 0.0                                                             
      TS( I ) = 0.0                                                             
      TP0( I ) = 0.0                                                            
      TS0( I ) = 0.0                                                            
   10 CONTINUE                                                                  
C
      IHX=IH0
C                                                                               
C                                                                               
      DO  20  I  =  1 , N                                                       
      TP0( I ) = 3600. * FLOAT(IHP(I)-IHX) + 60. * FLOAT( IMP( I ) )            
     +             + SECP( I )                                                  
      TS0( I ) = 3600. * FLOAT(IHS(I)-IHX) + 60. * FLOAT( IMS( I ) )            
     +             + SECS( I )                                                  
   20 CONTINUE                                                                  
      DO  30  I  =  1 , N                                                       
      IL( I ) = 0                                                               
      DO  32  J  =  1 , NST                                                     
      IF( IA( J ) .NE. IAI( I ) ) GO  TO  32                                    
      IL( I ) = J                                                               
C                                                                               
      GO  TO  30                                                                
   32 CONTINUE   
C                                                               
      IF( IWRT .EQ. 1 ) WRITE( 15 , 600 ) IAI( I )                               
  600 FORMAT(  A6 , '   CANNOT BE FOUND IN THE STATION DATA SET' )          
      IL( I ) = 0                                                               
      IP( I ) = IBLNK                                                           
      IS( I ) = IBLNK                                                           
      JP(I) = IP(I)                                                             
      JS(I) = IS(I)                                                             
   30 CONTINUE                                                                  
      DO  40  I  =  1 , N                                                    
      TP( I ) = TP0( I ) + CORP( IL( I ) )                                      
      TS( I ) = TS0( I ) + CORS( IL( I ) )                                      
   40 CONTINUE                                                                  
      RETURN                                                                    
      END 
C
C     Subroutine INITP
C                                                                      
      SUBROUTINE INITP( IW )                                                    
      DIMENSION IA(100) , CORP(100) , CORS(100) , ALAT(100) , ALON(100)         
     1             , AHIGH(100) , STX(100) , STY(100)                           
      DIMENSION IAI(100) , IY(100) , IM(100) , ID(100) , IUD(100) ,             
     1             IP(100) , IHP(100) , IMP(100) , SECP(100) ,                  
     2             IVS(100) , IS(100) , IHS(100) , IMS(100) , SECS(100)         
     3             , MFP(100) , FP(100) , COM( 100 , 5 )                        
     4             , AM(100) , BM(100) , FMFP(100)                              
      DIMENSION TP(100) , TP0(100) , TS(100) , TS0(100) , IL(100)               
      COMMON/M/ ALAT0 , ALON0 , NSTOP , FPS                                     
      COMMON/S/ IA , CORP , CORS , ALAT , ALON , AHIGH , STX , STY , NST        
     1          , AM , BM                                                       
      COMMON/P/ IAI , IY , IM , ID , IUD , IP , IHP , IMP , SECP ,              
     1             IVS , IS , IHS , IMS , SECS , MFP , FP , COM , N             
      COMMON/T/ TP , TP0 , TS , TS0 ,IHX,IL                                     
      COMMON/I/ IY0 , IM0 , ID0 , IH0 , IMI0 , SEC0 ,EX ,EY ,ED ,T0,FMW         
      COMMON/MG/ FMFP , FPMAG , LFPMAG                                          
      CHARACTER*1 IP , IS , ISTAR , IBLNK , IUD , IVS , LFPMAG , MFP            
      CHARACTER*6 IA , IAI                                                      
      CHARACTER*4 COM                                                           
      DATA ISTAR/'*'/ , IBLNK/' '/                                              
      M  =  0                                                                   
      TMIN  =  86400.0                                                          
      DO  10  I  =  1 , N                                                       
      IF( IL(I) .EQ. 0 ) GO  TO  10                                             
      IF( IP(I) .EQ. ISTAR .OR. IP(I) .EQ. IBLNK ) GO TO 10                     
      IF( TP( I ) .GT. TMIN ) GO  TO  10                                        
      M  =  I                                                                   
      TMIN  =  TP( I )                                                          
   10 CONTINUE                                                                  
      IF( M .EQ. 0 ) GO  TO  1                                                  
      IH0  =  IHP( M )                                                          
      IMI0 = IMP( M )                                                           
      SEC0  =  SECP( M ) - 10.0                                                 
      GO  TO  2                                                                 
    1 CONTINUE                                                                  
      DO  20  I  =  1 , N                                                       
      IF( IL(I) .EQ. 0 ) GO  TO  20                                             
      IF( IS(I) .EQ. ISTAR .OR. IS(I) .EQ. IBLNK ) GO  TO  20                   
      IF( TS( I ) .GT. TMIN ) GO  TO  20                                        
      M  =  I                                                                   
      TMIN  =  TS( I )                                                          
   20 CONTINUE                                                                  
      IH0  =  IHS( M )                                                          
      IMI0  =  IMS( M )                                                         
      SEC0  =  SECS( M ) - 10.0                                                 
    2 CONTINUE                                                                  
      IY0  =  IY( M )                                                           
      IM0 = IM( M )                                                             
      ID0  =  ID( M )                                                           
      EX  =  STX( IL( M ) ) + 2.0                                               
      EY = STY( IL( M ) ) + 2.0                                                 
      ED  =  10.0                                                               
      T0  =  3600. * FLOAT(IH0-IHX) + 60. * FLOAT( IMI0 ) + SEC0                
      CALL PRTOXY( ELAT0 , ELON0 , ALAT0 , ALON0 , EX , EY , 1 )                
C                                                                               
C     F-P MAGNITUDE                                                             
C                                                                               
      FPMAG = 0.                                                                
      NFM = 0                                                                   
      FMSUM = 0.                                                                
      LFPMAG = ISTAR                                                            
C                                                                               
      DO 30 I = 1 , N                                                           
      FMFP(I) = 0.                                                              
      K = IL(I)                                                                 
      ABAM = ABS( AM(K) )                                                       
      ABBM = ABS( BM(K) )                                                       
      IF( ABAM .LT. 0.00001 .AND. ABBM .LT. 0.00001 ) GO TO 30                  
      IF( MFP(I) .EQ. ISTAR ) GO TO 30                                          
      IF( FP(I)  .LT. 0.1   ) GO TO 30                                          
           LFPMAG  = IBLNK                                                      
           FMFP(I) = AM(K) + BM(K) * ALOG10( FP(I) )                            
           FMSUM = FMSUM + FMFP(I)                                              
           NFM   = NFM + 1                                                      
   30 CONTINUE                                                                  
C                                                                               
      IF( LFPMAG .EQ. IBLNK ) FPMAG = FMSUM / FLOAT( NFM )                      
C                                                                               
      IF( IW .NE. 1 ) RETURN                                                    
      WRITE( 15 , 600 ) IY0 , IM0 , ID0 , IH0 , IMI0 , SEC0 ,                    
     1             ELAT0 , ELON0 , ED , EX , EY                                 
  600 FORMAT(/,10X , 'INITIAL VALUE OF PARAMETERS' , 5X ,                  
     1         i4,  4I3 , F5.1 , 2F12.3 , F6.1 , 2F10.1 )                        
      RETURN                                                                    
      END 
C
C     Subroutine WEIP
C                                                                      
      SUBROUTINE WEIP                                                           
C     CONSTRUCTION OF WEIGHT VECTOR                                             
      DIMENSION IP( 100 ) , IS( 100 )                                           
      DIMENSION WP(100) , WS(100)                                               
      DIMENSION R(100) , AZ(100) , TPC(100) , TSC(100)                          
     1             , PDTDD(100) , SDTDD(100) , PDTDH(100) , SDTDH(100)          
      COMMON/J/ IP , IS , NN                                                    
      COMMON/W/ WP , WS , NWP , NWS                                             
      COMMON/DTR/ R , AZ , TPC , TSC , PDTDD , SDTDD , PDTDH , SDTDH            
      COMMON/M/ ALAT0 , ALON0 , NSTOP , FPS                                     
      CHARACTER*1 IP , IS , ISTAR , IBLNK                                       
      DATA ISTAR/'*'/ , IBLNK/' '/                                              
      N  =  NN                                                                  
      DO  10  I  =  1 , N                                                       
      WP( I ) = 0.0                                                             
      IF( IP( I ) .EQ. ISTAR ) GO  TO  12                                       
      IF( IP( I ) .EQ. IBLNK ) GO  TO  12                                       
      CALL WRNK( IP( I ) , WPR )                                                
      CALL WDST( R( I ) , WPD )                                                 
      WP( I ) = WPR * WPD                                                       
   12 CONTINUE                                                                  
      WS( I ) = 0.0                                                             
      IF( IS( I ) .EQ. ISTAR ) GO  TO  10                                       
      IF( IS( I ) .EQ. IBLNK ) GO  TO  10                                       
      CALL WRNK( IS( I ) , WSR )                                                
      CALL WDST( R( I ) , WSD )                                                 
      WS( I ) = FPS * WSR * WSD                                                 
   10 CONTINUE                                                                  
      RETURN                                                                    
      END 
C
C     Subroutine TRV
C                                                                      
      SUBROUTINE TRV( DELT,ED,Z , T , FDTDD , FDTDH , ANG , IPS , ISET )        
      DIMENSION TT( 20 ) , DTDD( 20 ) , DTDH( 20 ) ,                            
     1             A( 20 , 20 ) , TLK( 20 , 20 ) , CLK( 20 , 20 ) ,             
     2             AI1( 20 ) , AIH( 20 ) , DEL( 20 ) , V( 20 )                  
      DIMENSION HL(20) , VP(20) , VS(20)                                        
      COMMON/V/ HL , VP , VS , NL , NLM                                         
C                                                                               
      DED = 1.0                                                                 
C                                                                               
      IF( ISET .EQ. 0 ) GO  TO  3                                               
      IF( IPS .EQ. 1 ) GO  TO  1                                                
      DO  10  I  =  1 , NL                                                      
      V( I ) = VP( I )                                                          
   10 CONTINUE                                                                  
      GO  TO  2                                                                 
    1 CONTINUE                                                                  
      DO  20  I  =  1 , NL                                                      
      V( I ) = VS( I )                                                          
   20 CONTINUE                                                                  
    2 CONTINUE                                                                  
      LVZ  =  0                                                                 
      DO  30  I  =  1 , NLM                                                     
      I1  =  I  +  1                                                            
      DO  30  J  =  I1 , NL                                                     
      A( I , J ) = V( I ) / V( J )                                              
      IF( V( J ) .LE. V( I ) ) GO  TO  32                                       
      CLK( I , J ) = SQRT( 1.0 - A( I , J ) ** 2 )                              
      TLK( I , J ) = A( I , J ) / CLK( I , J )                                  
      GO  TO  30                                                                
   32 CONTINUE                                                                  
      LVZ  =  1                                                                 
      CLK( I , J ) = 9999.9                                                     
      TLK( I , J ) = 9999.9                                                     
   30 CONTINUE                                                                  
      RETURN                                                                    
    3 CONTINUE                                                                  
C                                                                               
C     DTDH = (T2 - T1 ) / ( ED2 - ED1 )                                         
C                                                                               
      ED1 = ED + DED                                                            
      ED2 = ED - DED                                                            
      IF( ED2 .LT. 0. ) ED2 = ED                                                
C                                                                               
      CALL TDLT( HL , V , ED1, DELT , TT , DTDD , DTDH , AI1 , AIH ,            
     1             NL , Z , A , CLK , TLK , LVZ , DEL , 20 )                    
      CALL SMXMN( TT , DUM , T1, NL , NDUM , NMIN , 20 )                        
C                                                                               
      CALL TDLT( HL , V , ED2, DELT , TT , DTDD , DTDH , AI1 , AIH ,            
     1             NL , Z , A , CLK , TLK , LVZ , DEL , 20 )                    
      CALL SMXMN( TT , DUM , T2, NL , NDUM , NMIN , 20 )                        
C                                                                               
      FDTDH = ( T2 - T1 ) / ( ED2 - ED1 )                                       
C                                                                               
C     TRAVEL TIME AND DTDD                                                      
C                                                                               
      CALL TDLT( HL , V , ED , DELT , TT , DTDD , DTDH , AI1 , AIH ,            
     1             NL , Z , A , CLK , TLK , LVZ , DEL , 20 )                    
      CALL SMXMN( TT , DUM , T , NL , NDUM , NMIN , 20 )                        
C     FDTDH  =  DTDH( NMIN )                                                    
      FDTDD = DTDD( NMIN )                                                      
      ANG  =  AIH( NMIN )                                                       
      RETURN                                                                    
      END    
C
C     Subroutine DTDRV
C                                                                   
      SUBROUTINE DTDRV                                                          
      DIMENSION HL(20) , VP(20) , VS(20)                                        
      DIMENSION IA(100) , CORP(100) , CORS(100) , ALAT(100) , ALON(100)         
     1             , AHIGH(100) , STX(100) , STY(100) ,AM(100),BM(100)          
      DIMENSION R(100) , AZ(100) , TPC(100) , TSC(100)                          
     1             , PDTDD(100) , SDTDD(100) , PDTDH(100) , SDTDH(100)          
      DIMENSION TP(100) , TP0(100) , TS(100) , TS0(100) , IL(100)               
      DIMENSION IP( 100 ) , IS( 100 )                                           
      DIMENSION ANGP( 100 ) , ANGS( 100 )                                       
      COMMON/V/ HL , VP , VS , NL , NLM                                         
      COMMON/S/ IA , CORP , CORS , ALAT , ALON , AHIGH , STX , STY , NST        
     1         ,AM , BM                                                         
      COMMON/I/ IY0,IM0,ID0,IH0, IMI0 , SEC0 , EX , EY , ED , T0,FMW         
      COMMON/DTR/ R , AZ , TPC , TSC , PDTDD , SDTDD , PDTDH , SDTDH            
      COMMON/T/ TP , TP0 , TS , TS0 ,IHX, IL                                    
      COMMON/J/ IP , IS , NN                                                    
      COMMON/AG/ ANGP , ANGS                                                    
      CHARACTER*6 IA                                                            
      CHARACTER*1 IP , IS                                                       
      N  =  NN                                                                  
      DO  10  I  =  1 , N                                                       
      IF( IL(I) .EQ. 0 ) GO  TO  10                                             
      K  =  IL( I )                                                             
      CALL DSTAZ( EX , EY , STX(K) , STY(K) , R(I) , AZ(I) )                    
   10 CONTINUE                                                                  
C     P TRAVEL TIMES AND THEIR DERIVATIVES                                      
      CALL TRV( 0.,0.,0., 0. , 0. , 0. , 0. , 0 , 1 )                           
      DO  20  I  =  1 , N                                                       
      IF( IL(I) .EQ. 0 ) GO  TO  20                                             
      Z = AHIGH( IL(I)  )                                                       
      CALL TRV( R(I),ED,Z , TPC(I) , PDTDD(I) , PDTDH(I) , ANGP(I) ,            
     1             0 , 0 )                                                      
   20 CONTINUE                                                                  
C     S TRAVEL TIMES AND THEIR DERIVATIVES                                      
      CALL TRV( 0.,0.,0., 0. , 0. , 0. , 0. , 1 , 1 )                           
      DO  30  I  =  1 , N                                                       
      IF( IL(I) .EQ. 0 ) GO  TO  30                                             
      Z = AHIGH( IL( I ) )                                                      
      CALL TRV( R(I),ED,Z , TSC(I) , SDTDD(I) , SDTDH(I) , ANGS(I) ,            
     1             1 , 0 )                                                      
   30 CONTINUE                                                                  
      RETURN                                                                    
      END   
C
C     Subroutine DSTAZ
C                                                                    
      SUBROUTINE DSTAZ( EX , EY , SX , SY , R , AZ )                            
C     AZ : MEASURED FROM NORTH TO EAST ( RADIAN )                               
      SEX  =  SX  -  EX                                                         
      SEY  =  SY  -  EY                                                         
      R2  =  SEX ** 2 + SEY ** 2                                                
      R  =  SQRT( R2 )                                                          
      AZ  =  ATAN2( SEX , SEY )                                                 
      RETURN                                                                    
      END 
C
C     Subroutine SECHM
C                                                                      
      SUBROUTINE SECHM( T , IH , IM , SEC )   
                                  
      IH  =  T / 3600.0                                                         
      IM  =  ( T - 3600. * FLOAT( IH ) ) / 60.                                  
      SEC  =  T - 3600. * FLOAT( IH ) - 60. * FLOAT( IM )                       
      RETURN                                                                    
      END     
C
C     Subroutine WRNK
C                                                                  
      SUBROUTINE WRNK( LR , W )                                                 
      COMMON/WR/ WA , WB , WC , WD                                              
      CHARACTER*1 LR , LA , LB , LC , LD                                        
      DATA LA/'A'/ , LB/'B'/ , LC/'C'/ , LD/'D'/                                
      W  =  0.0                                                                 
      IF( LR .EQ. LA ) W = WA                                                   
      IF( LR .EQ. LB ) W = WB                                                   
      IF( LR .EQ. LC ) W = WC                                                   
      IF( LR .EQ. LD ) W = WD                                                   
      RETURN                                                                    
      END  
C
C     Subroutine WDST
C                                                                     
      SUBROUTINE WDST( D , W )                                                  
      COMMON/MW/ W1 , W2 , D1 , D2 , FD1 , FD2                                  
      IF( D .LT. D1 ) W = W1                                                    
      IF( D .GE. D1 .AND. D .LT.D2 ) W = FD1 + FD2 * D                          
      IF( D .GT. D2 ) W = W2                                                    
      RETURN                                                                    
      END   
C
C     Subroutine SETVEC
C                                                                    
      SUBROUTINE SETVEC                                                         
      DIMENSION TP(100) , TP0(100) , TS(100) , TS0(100) , IL(100)               
      DIMENSION Y( 200 )                                                        
      DIMENSION FMSHD( 9 ) , FMSHH( 9 ) , CRFSP( 9 , 9 ) , CRFSS( 9 , 9)          
      DIMENSION IP(100) , IS(100)                                               
      DIMENSION R(100) , AZ(100) , TPC(100) , TSC(100)                          
     1             , PDTDD(100) , SDTDD(100) , PDTDH(100) , SDTDH(100)          
      COMMON/F/ FMSHD , FMSHH , CRFSP , CRFSS                                     
      COMMON/T/ TP , TP0 , TS , TS0 ,IHX, IL                                    
      COMMON/Y/ Y , NY , NP , NS                                                
      COMMON/J/ IP , IS , NN                                                    
      COMMON/DTR/ R , AZ , TPC , TSC , PDTDD , SDTDD , PDTDH , SDTDH            
      COMMON/I/ IY0,IM0,ID0,IH0,IMI0 , SEC0 , EX , EY , ED , T0,FMW         
      CHARACTER*1 IP , IS , ISTAR , IBLK       
C                         
      DATA ISTAR/'*'/ , IBLK/' '/  
C                                             
      N  =  NN  
C
C                                                                
      NP  =  0                                                                  
      NY  =  0                                                                  
      DO  10  I  =  1 , N                                                       
      IF( IL(I) .EQ. 0 ) GO  TO  10                                             
      IF( IP( I ) .EQ. ISTAR .OR. IP( I ) .EQ. IBLK ) GO  TO  10                
      NY  =  NY  +  1                                                           
      NP  =  NP  +  1                                                           
      CALL FLSPT( FMSHD , FMSHH , CRFSP , R(I) , ED , TPFL , TP(I) )              
      Y( NY ) = TPFL - TPC( I ) - T0                                            
   10 CONTINUE                                                                  
      NS  =  0                                                                  
      DO  20  I  =  1 , N                                                       
      IF( IL(I) .EQ. 0 ) GO  TO  20                                             
      IF( IS( I ) .EQ. ISTAR .OR. IS( I ) .EQ. IBLK ) GO TO 20                  
      NY  =  NY  +  1                                                           
      NS  =  NS  +  1                                                           
      CALL FLSPT( FMSHD , FMSHH , CRFSS , R(I) , ED , TSFL , TS(I) )              
      Y( NY ) = TSFL - TSC( I ) - T0                                            
   20 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END 
C
C     Subroutine AMTRX
C                                                                      
      SUBROUTINE AMTRX( IDFX )                                                  
      DIMENSION A( 200 , 4 )                                                    
      DIMENSION R(100) , AZ(100) , TPC(100) , TSC(100)                          
     1             , PDTDD(100) , SDTDD(100) , PDTDH(100) , SDTDH(100)          
      DIMENSION IP(100) , IS(100)                                               
      DIMENSION TP(100) , TP0(100) , TS(100) , TS0(100) , IL(100)               
      COMMON/A/ A , NA                                                          
      COMMON/DTR/ R , AZ , TPC , TSC , PDTDD , SDTDD , PDTDH , SDTDH            
      COMMON/J/ IP , IS , NN                                                    
      COMMON/T/ TP , TP0 , TS , TS0 ,IHX,IL                                     
      CHARACTER*1 IP , IS , ISTAR , IBLK                                        
      DATA ISTAR/'*'/ , IBLK/' '/                                               
      N  =  NN                                                                  
      NA  =  0                                                                  
      DO  10  I  =  1 , N                                                       
      IF( IL(I) .EQ. 0 ) GO  TO  10                                             
      IF( IP(I) .EQ. ISTAR .OR. IP(I) .EQ. IBLK ) GO  TO  10                    
      FC  =  -COS( AZ(I) )                                                      
      FS = -SIN( AZ( I ) )                                                      
      NA  =  NA  +  1                                                           
      A( NA , 1 ) = PDTDD( I ) * FS                                             
      A( NA , 2 ) = PDTDD( I ) * FC                                             
      A( NA , 3 ) = 1.0                                                         
      IF( IDFX .EQ. 1 ) GO  TO  10                                              
      A( NA , 4 ) = PDTDH( I )                                                  
   10 CONTINUE                                                                  
      DO  20  I  =  1 , N                                                       
      IF( IL(I) .EQ. 0 ) GO  TO  20                                             
      IF( IS( I ) .EQ. ISTAR .OR. IS( I ) .EQ. IBLK ) GO TO 20                  
      FC = -COS( AZ( I ) )                                                      
      FS = -SIN( AZ( I ) )                                                      
      NA  =  NA  +  1                                                           
      A( NA , 1 ) = SDTDD( I ) * FS                                             
      A( NA , 2 ) = SDTDD( I ) * FC                                             
      A( NA , 3 ) = 1.0                                                         
      IF( IDFX .EQ. 1 ) GO  TO  20                                              
      A( NA , 4 ) = SDTDH( I )                                                  
   20 CONTINUE                                                                  
      RETURN                                                                    
      END 
C
C     Subroutine WVCTR
C                                                                      
      SUBROUTINE WVCTR                                                          
      DIMENSION W( 200 )                                                        
      DIMENSION WP(100) , WS(100)                                               
      DIMENSION IP(100) , IS(100)                                               
      DIMENSION TP(100) , TP0(100) , TS(100) , TS0(100) , IL(100)               
      COMMON/WV/ W , NW                                                         
      COMMON/W/ WP , WS , NWP , NWS                                             
      COMMON/J/ IP , IS , NN                                                    
      COMMON/T/ TP , TP0 , TS , TS0 ,IHX, IL                                    
      CHARACTER*1 IP , IS , ISTAR , IBLK                                        
      DATA ISTAR/'*'/ , IBLK/' '/                                               
      N  =  NN                                                                  
      NW  =  0                                                                  
      DO  10  I  =  1 , N                                                       
      IF( IL(I) .EQ. 0 ) GO  TO  10                                             
      IF( IP(I) .EQ. ISTAR .OR. IP(I) .EQ. IBLK ) GO  TO  10                    
      NW  =  NW  +  1                                                           
      W( NW ) = WP( I )                                                         
   10 CONTINUE                                                                  
      DO  20  I  =  1 , N                                                       
      IF( IL(I) .EQ. 0 ) GO  TO  20                                             
      IF( IS( I ) .EQ. ISTAR .OR. IS( I ) .EQ. IBLK ) GO  TO  20                
      NW  =  NW  +  1                                                           
      W( NW ) = WS( I )                                                         
   20 CONTINUE                                                                  
      RETURN                                                                    
      END 
C
C     Subroutine PARW
C                                                                      
      SUBROUTINE PARW( IW )                                                     
      DIMENSION WA( 4 )                                                         
      COMMON/PW/ WA , NPW                                                       
      SIGX  = 10.0                                                              
      SIGY  = 10.0                                                              
      SIGZ  =  5.0                                                              
      SIGT  =  5.0                                                              
      WA( 1 ) = SIGX                                                            
      WA( 2 ) = SIGY                                                            
      WA( 3 ) = SIGT                                                            
      WA( 4 ) = SIGZ                                                            
      IF( IW .NE. 1 ) RETURN                                                    
      WRITE( 15 , 600 )                                                          
  600 FORMAT( /, 'WEIGHT FOR PARAMETERS' )                                   
      WRITE( 15 , 601 ) SIGX , SIGY , SIGZ , SIGT                                
  601 FORMAT(      'SIGX=' , F6.1 , 'KM' , 5X ,                                 
     1             'SIGY=' , F6.1 , 'KM' , 5X ,                                 
     2             'SIGZ=' , F6.1 , 'KM' , 5X ,                                 
     3             'SIGT=' , F6.1 , 'SEC'   )                                   
      RETURN                                                                    
      END 
C
C     Subroutine CNVSOL
C                                                                      
      SUBROUTINE CNVSOL(IDFX)                                                  
      DIMENSION X0( 4 ) , DX( 4 )                                               
      DIMENSION DXMAX( 4 ) , DXMAX1(4)                                          
      COMMON/CV/ X0 , DX , ICNV                                                 
C     INPUT     X0 : INITIAL VALUE VECTOR                                       
C     OUTPUT    DX : CORRECTION VECTOR                                          
C               X0 : SOLUTION VECTOR                                            
C           ICNV = 1 : CONVERGENCE                                              
C                = 0 : NOT CONVERGENCE                                          
C                = 2 : CONVERGENCE WITH SLIGHTLY OSCILLATION                    
C                                                                               
      DXMAX( 1 ) = 0.05                                                         
      DXMAX( 2 ) = 0.05                                                         
      DXMAX( 3 ) = 0.01                                                         
      DXMAX( 4 ) = 0.1                                                          
C                                                                               
      DXMAX1( 1 ) = 0.5                                                         
      DXMAX1( 2 ) = 0.5                                                         
      DXMAX1( 3 ) = 0.05                                                        
      DXMAX1( 4 ) = 1.0                                                         
C                                                                               
      ICNV  =  1                                                                
      M = 4                                                                     
      IF( IDFX .EQ. 1 ) M=3                                                     
      DO  10  I  =  1 , M                                                       
      ADX  =  ABS( DX( I ) )                                                    
      IF( ADX .GT. DXMAX( I ) ) ICNV  =  2                                      
      IF( ADX .GT. DXMAX1( I ) ) ICNV  =  0                                     
   10 CONTINUE                                                                  
      DO  20  I  =  1 , M                                                       
      X0( I ) = X0( I ) + DX( I )                                               
   20 CONTINUE                                                                  
      RETURN                                                                    
      END       
C
C     Subroutine LSTSQ
C                                                                
      SUBROUTINE LSTSQ ( IDFX )                                                 
      DIMENSION Y( 200 )                                                        
      DIMENSION A( 200 , 4 )                                                    
      DIMENSION W( 200 )                                                        
      DIMENSION WA( 4 )                                                         
      DIMENSION SOL( 4 ) , SIG( 4 ) , RSL( 4 , 4 ) , RSD( 200 )                 
      DIMENSION S( 4 , 4 ) , SR( 4 , 4 ) , C( 4 , 4 ) , C1( 4 , 4 ) ,           
     1             SS( 4 , 4 ) , T( 4 )                                         
      COMMON/WV/ W , NW                                                         
      COMMON/A/ A , NA                                                          
      COMMON/PW/ WA , NPW                                                       
      COMMON/Y/ Y , NY , NP , NS                                                
      COMMON/LS/ SOL , SIG , RSD , RSL , VAR                                    
C                                                                               
C     TET : DAMPING FACTOR FOR DIAGONAL ELEMENTS                                
C                                                                               
      TET =  1.0                                                                
C 1987/02/16                                                                    
C                                                                               
      N  =  NA                                                                  
      M  =  4                                                                   
C                                                                               
      DO  1  I  =  1 , 4                                                        
      SOL( I ) = 0.0                                                            
      SIG( I ) = 0.0                                                            
      DO  1  J  =  1 , 4                                                        
      S( I , J ) = 0.0                                                          
      SR( I , J ) = 0.0                                                         
      RSL( I , J ) = 0.0                                                        
      C( I , J ) = 0.0                                                          
      C1( I , J ) = 0.0                                                         
    1 CONTINUE                                                                  
C                                                                               
      IF( IDFX .EQ. 1 ) M = 3                                                   
      IF( N .LT. M ) RETURN                                                     
C     PARAMETER - WEIGHTED MATRIX OF A                                          
      DO  10  I  =  1 , N                                                       
      DO  10  J  =  1 , M                                                       
      A( I , J ) = WA( J ) * A( I , J )                                         
   10 CONTINUE                                                                  
C                                                                               
      DO  20  I  =  1 , M                                                       
      DO  20  J  =  1 , M                                                       
      S( I , J ) = 0.0                                                          
      DO  20  K  =  1 , N                                                       
      S( I , J ) = S( I , J ) + W( K ) * A( K , I ) * A( K , J )                
   20 CONTINUE                                                                  
C                                                                               
      DO  30  I  =  1 , M                                                       
      T( I ) = 0.0                                                              
      DO  30  J  =  1 ,N                                                        
      T( I ) = T( I ) + W( J ) * A( J , I ) * Y( J )                            
   30 CONTINUE                                                                  
      DO  40  I  =  1 , M                                                       
      DO  40  J  =  1 , M                                                       
      SR( I , J ) = S( I , J )                                                  
   40 CONTINUE                                                                  
C                                                                               
      DO  50  I  =  1 , M                                                       
      S( I , I ) = S( I , I ) + TET                                             
   50 CONTINUE                                                                  
C                                                                               
C     INVERSE MATRIX OF S( I , I )                                              
      NDIM  =  4                                                                
      CALL INV30R( S , SS , M , NDIM ,ILL )                                     
CCC   CALL INV30S( SR , C , M , NDIM )                                          
      DO  55  I  =  1 , M                                                       
      DO  55  J  =  1 , M                                                       
      S( I , J ) = SS( I , J )                                                  
   55 CONTINUE                                                                  
      DO  60  I  =  1 , M                                                       
      DO  60  J  =  1 , M                                                       
      RSL( I , J ) = 0.0                                                        
      DO  60  K  =  1 , M                                                       
      RSL( I , J ) = S( I , K ) * SR( K , J ) + RSL( I , J )                    
   60 CONTINUE                                                                  
      DO  70  I  =  1 ,  M                                                      
      SOL( I ) = 0.0                                                            
      DO  70  J  =  1 , M                                                       
      SOL( I ) = SOL( I ) + S( I , J ) * T( J )                                 
   70 CONTINUE                                                                  
C                                                                               
C     RESIDUAL VECTOR                                                           
C                                                                               
      DO  80  I  =  1 , N                                                       
      RSD( I ) = 0.0                                                            
      DO  82  J  =  1 , M                                                       
      RSD( I ) = RSD( I ) + A( I , J ) * SOL( J )                               
   82 CONTINUE                                                                  
      RSD( I ) = Y( I ) - RSD( I )                                              
   80 CONTINUE                                                                  
      IF( N .EQ. M ) RETURN                                                     
C                                                                               
C     RELATIVE VARIANCE                                                         
C                                                                               
      VAR  =  0.0                                                               
      DO  90  I  =  1 , N                                                       
      VAR  =  VAR  +  W( I ) * RSD( I ) ** 2                                    
   90 CONTINUE                                                                  
      VAR1 =  VAR / FLOAT( N - M )                                              
C     VARIANCE                                                                  
C                                                                               
      DO  100  I  =  1 , M                                                      
      DO  100  J  =  1 , M                                                      
      C1( I , J ) = 0.                                                          
      DO  100  K  =  1 , M                                                      
      C1( I , J ) = C1( I , J ) + SR( I , K ) * S( K , J )                      
  100 CONTINUE                                                                  
      DO  110  I  =  1 , M                                                      
      DO  110  J  =  1 , M                                                      
      C( I , J ) = 0.                                                           
      DO  110  K  =  1 , M                                                      
      C( I , J ) = C( I , J ) + S( I , K ) * C1( K , J )                        
  110 CONTINUE                                                                  
C                                                                               
      DO  120  I  =  1  , M                                                     
      CC2  =  VAR1 *  C( I , I )                                                
      SIG( I ) = SQRT( CC2 )                                                    
  120 CONTINUE                                                                  
      DO  130  I  =  1 , M                                                      
      SOL( I ) = WA( I ) * SOL( I )                                             
      SIG( I ) = WA( I ) * SIG( I )                                             
  130 CONTINUE                                                                  
      RETURN                                                                    
      END             
C
C     Subroutine INV30S
C                                                          
      SUBROUTINE   INV30S ( A, B, N, NDIM )                                     
C                                                                               
      DIMENSION    A (NDIM, NDIM) ,  B ( NDIM, NDIM)                            
      DN = 0.000000001                                                          
      DO 1 M=1,N                                                                
      DO 1 L=1,N                                                                
    1 B(M,L)=0                                                                  
      DO 2 M=1,N                                                                
    2 B(M,M)=1                                                                  
      DO 12 I=1,N                                                               
      IF  ( ABS ( A(I, I)) - DN )   3, 8, 8                                     
    3 II=I+1                                                                    
      DO 4 J=II,N                                                               
      IF ( ABS (A (I,J) ) - DN ) 4, 6, 6                                        
    4 CONTINUE                                                                  
      WRITE (15,5)                                                               
    5 FORMAT (/,'NO SOLUTION'   )                                               
      RETURN                                                                    
    6 DO 7 K=1,N                                                                
      R=A(K,I)                                                                  
      A(K,I)=A(K,J)                                                             
      A(K,J)=R                                                                  
      RR=B(K,I)                                                                 
      B(K,I)=B(K,J)                                                             
    7 B(K,J)=RR                                                                 
    8 S=A(I,I)                                                                  
      DO 9 J=1,N                                                                
      A(I,J)=A(I,J)/S                                                           
    9 B(I,J)=B(I,J)/S                                                           
      DO 12 K=1,N                                                               
      IF (K-I) 10,12,10                                                         
   10 T=A(K,I)                                                                  
      DO 11 L=1,N                                                               
      A(K,L)=A(K,L)-T*A(I,L)                                                    
   11 B(K,L)=B(K,L)-T*B(I,L)                                                    
   12 CONTINUE                                                                  
      RETURN                                                                    
      END         
C
C     Subroutine Reset
C                                                              
      SUBROUTINE RESET                                                          
      DIMENSION TP(100) , TP0(100) , TS(100) , TS0(100) , IL(100)               
      DIMENSION WP(100) , WS(100)                                               
      DIMENSION R(100) , AZ(100) , TPC(100) , TSC(100)                          
     1             , PDTDD(100) , SDTDD(100) , PDTDH(100) , SDTDH(100)          
      DIMENSION Y( 200 )                                                        
      DIMENSION A( 200 , 4 )                                                    
      DIMENSION W( 200 )                                                        
      DIMENSION SOL( 4 ) , SIG( 4 ) , RSL( 4 , 4 ) , RSD( 200 )                 
      DIMENSION X0( 4 ) , DX( 4 )                                               
      DIMENSION WA( 4 )                                                         
      DIMENSION ANGP( 100 ) , ANGS( 100 )                                       
      COMMON/T/ TP , TP0 , TS , TS0 ,IHX,IL                                     
      COMMON/W/ WP , WS , NWP , NWS                                             
      COMMON/DTR/ R , AZ , TPC , TSC , PDTDD , SDTDD , PDTDH , SDTDH            
      COMMON/WV/ W , NW                                                         
      COMMON/A/ A , NA                                                          
      COMMON/Y/ Y , NY , NP , NS                                                
      COMMON/LS/ SOL , SIG , RSD , RSL , VAR                                    
      COMMON/CV/ X0 , DX , ICNV                                                 
      COMMON/PW/ WA , NPW                                                       
      COMMON/AG/ ANGP , ANGS                                                    
C                                                                               
C     CLEAR PARAMETERS                                                          
C                                                                               
      DO  10  I  =  1 , 100                                                     
C                                                                               
      TP( I ) = 0.0                                                             
      TS( I ) = 0.0                                                             
      TP0( I ) = 0.0                                                            
      TS0( I ) = 0.0                                                            
      IL( I ) = 0                                                               
C                                                                               
      WP( I ) = 0.0                                                             
      WS( I ) = 0.0                                                             
C                                                                               
      R( I ) = 0.0                                                              
      AZ( I ) = 0.0                                                             
      TPC( I ) = 0.0                                                            
      TSC( I ) = 0.0                                                            
      PDTDD( I ) = 0.0                                                          
      SDTDD( I ) = 0.0                                                          
      PDTDH( I ) = 0.0                                                          
      SDTDH( I ) = 0.0                                                          
C                                                                               
C                                                                               
      I2  =  2 * I                                                              
C                                                                               
      Y( I ) = 0.0                                                              
      Y( I2 ) = 0.0                                                             
C                                                                               
      W( I ) = 0.0                                                              
      W( I2 ) = 0.0                                                             
C                                                                               
      ANGP( I ) = 0.0                                                           
      ANGS( I ) = 0.0                                                           
C                                                                               
      RSD( I ) = 0.0                                                            
      RSD( I2 ) = 0.0                                                           
C                                                                               
      DO  12  J  =  1 , 4                                                       
      A( I , J ) = 0.0                                                          
      A( I2 , J ) = 0.0                                                         
   12 CONTINUE                                                                  
   10 CONTINUE                                                                  
C                                                                               
      DO  20  I  =  1 , 4                                                       
      SOL( I ) = 0.0                                                            
      SIG( I ) = 0.0                                                            
      X0( I ) = 0.0                                                             
      DX( I ) = 0.0                                                             
      DO  22  J  =  1 , 4                                                       
      RSL( I , J ) = 0.0                                                        
   22 CONTINUE                                                                  
   20 CONTINUE                                                                  
      RETURN                                                                    
      END 
C
C     Subroutine OUTPUT2018
C                                                                      
      SUBROUTINE OUTPUT2018( IWE , IWS )                                            
      DIMENSION IA(100) , CORP(100) , CORS(100) , ALAT(100) , ALON(100)         
     1             , AHIGH(100) , STX(100) , STY(100)                           
      DIMENSION IAI(100) , IY(100) , IM(100) , ID(100) , IUD(100) ,             
     1             IP(100) , IHP(100) , IMP(100) , SECP(100) ,                  
     2             IVS(100) , IS(100) , IHS(100) , IMS(100) , SECS(100 )        
     3             , MFP(100) , FP(100) , COM( 100 , 5 )                        
     4             ,AM(100) , BM(100) , FMFP(100)                               
      DIMENSION SOL( 4 ) , SIG( 4 ) , RSL( 4 , 4 ) , RSD( 200 )                 
      DIMENSION X( 4 ) , DX( 4 )                                                
      DIMENSION FMSHD( 9 ) , FMSHH( 9 ) , CRFSP( 9 , 9 ) , CRFSS( 9 , 9)          
      DIMENSION ANGP( 100 ) , ANGS( 100 )                                       
      DIMENSION TP(100) , TP0(100) , TS(100) , TS0(100) , IL(100)               
      DIMENSION R(100) , AZ(100) , TPC(100) , TSC(100)                          
     1             , PDTDD(100) , SDTDD(100) , PDTDH(100) , SDTDH(100)          
      DIMENSION WP(100) , WS(100)                                               
      COMMON/S/ IA , CORP , CORS , ALAT , ALON , AHIGH , STX , STY , NST        
     1           , AM, BM                                                       
      COMMON/P/ IAI , IY , IM , ID , IUD , IP , IHP , IMP , SECP ,              
     1             IVS , IS , IHS , IMS , SECS , MFP , FP , COM , N             
      COMMON/LS/ SOL , SIG , RSD , RSL , VAR                                    
      COMMON/CV/ X  , DX , ICNV                                                 
      COMMON/F/ FMSHD , FMSHH , CRFSP , CRFSS                                     
      COMMON/AG/ ANGP , ANGS                                                    
      COMMON/I/ IY0,IM0,ID0,IH0, IMI0 , SEC0 , EX , EY , ED , T0,FMW         
      COMMON/T/ TP , TP0 , TS , TS0 ,IHX, IL                                    
      COMMON/DTR/ R , AZ , TPC , TSC , PDTDD , SDTDD , PDTDH , SDTDH            
      COMMON/W/ WP , WS , NWP , NWS                                             
      COMMON/M/ ALAT0 , ALON0 , NSTOP , FPS                                     
      COMMON/OT/ NEQ                                                            
      COMMON/OD/ NSEQ , NYR , JYMD , IOTH , IOTM , OTS ,                        
     1           DOT , HLAT , DLAT , HLNG , DLNG , HDEP , DDEP ,                
     2           SIGM, MST , NP , NS , XX,DXX , YY,DYY ,                        
     3           FMAMP  , GPMAG,                                                
     4           IDXHP , IDXBL , JDMAMP , JDMFP                                 
      COMMON/MG/ FMFP , FPMAG , LFPMAG                                          
      COMMON/O/ PKFN1, CMNT1
      CHARACTER*1 IP , IS , ISTAR , IBLK , IUD , IVS , MFP                      
     1           , JDMAMP , JDMFP , SICNV, LFPMAG                               
      CHARACTER*6 IA,IAI                                                        
      CHARACTER*4 COM                                                           
      CHARACTER*23 PKFN1                                                           
      CHARACTER*40 CMNT1 
      DATA ISTAR/'*'/ , IBLK/' '/                                               
      DEG  =  57.29578                                                          
      RALT0  =  ALAT0 / DEG                                                     
      ALTKM  =  111.195                                                         
      ALNKM = 111.195 * COS( RALT0 )                                            
C                                                                               
C     STANDARD ERROR                                                            
C                                                                               
      SX  =  SIG( 1 )                                                           
      SY  =  SIG( 2 )                                                           
      SZ  =  SIG( 4 )                                                           
      ST  =  SIG( 3 )                                                           
C                                                                               
      SICNV = IBLK                                                              
      IF( ICNV .EQ. 0 ) SICNV = ISTAR                                           
C                                                                               
C                                                                               
      CALL SECHM( T0 , IH , IMI , SEC )                                         
      IH = IH + IHX                                                             
C                                                                               
      CALL PRTOXY( ELAT , ELON , ALAT0 , ALON0 , X(1) , X(2) , 1 )              
      ERELT  =  SY / ALTKM                                                      
      ERELN = SX / ALNKM                                                        
      NSEQ = NEQ                                                                
      NYR = NEQ                                                                 
      JYMD = 10000 * IY0 + 100 * IM0 + ID0                                      
      IOTH = IH                                                                 
      IOTM = IMI                                                                
      OTS  = SEC                                                                
      DOT = ST                                                                  
      HLAT = ELAT                                                               
      DLAT = ERELT                                                              
      HLNG = ELON                                                               
      DLNG = ERELN                                                              
      HDEP = X(4)                                                               
      DDEP = SZ                                                                 
      SIGM= VAR                                                                 
      MST = N                                                                   
      NP = 0                                                                    
      NS = 0                                                                    
      DO 90 I = 1 , MST                                                         
      IF( IP(I).NE.ISTAR .AND. IP(I).NE. IBLK ) NP = NP + 1                     
      IF( IS(I).NE.ISTAR .AND. IS(I).NE. IBLK ) NS = NS + 1                     
   90 CONTINUE                                                                  
      XX  = X(1)                                                                
      DXX = SX                                                                  
      YY = X(2)                                                                 
      DYY = SY                                                                  
      FMAMP = 3.0                                                               
      JDMAMP = IBLK                                                             
      GPMAG = FPMAG                                                             
      JDMFP = LFPMAG                                                            
      IDXHP = 0                                                                 
      IF( DDEP .EQ. 0.0 ) IDXHP = 1                                             
      IDXBL = 0                                                                 
      IF( ICNV .NE. 0 ) IDXBL = ICNV                                            
C                                                                               
C     C     C     C     C     C     C     C     C     C     C                   
      IF( IWE .NE. 1 ) GO  TO  1                                                
C                                                                               
C                                                                               
      WRITE( 15 , 600 ) NEQ , IY0 , IM0 , ID0 , IH , IMI , SEC , ST ,            
     1             ELAT , ERELT , ELON , ERELN ,                                
     2             X(4) , SZ , X(1) , SX , X(2) , SY , VAR , SICNV              
     3             , FMW                                             
  600 FORMAT( /, 'NEQ=' , I3 , 2X , I4 , 4I3 , F6.2 ,                 
     1             '(' , F5.2 , ')' , 2(F8.4 , '(' , F5.3 , ')' ) ,             
     2             3( F7.1 , '(' , F5.1 , ')' ) , F8.4 ,A1 ,                    
     3             F5.1  )                                                  
C
      WRITE( 17 , 1700 ) NEQ, IY0 , IM0 , ID0 , IH , IMI , SEC , ST ,            
     1             ELAT , ERELT , ELON , ERELN ,                                
     2             X(4) , SZ , X(1) , SX , X(2) , SY , VAR , SICNV              
     3             , FMW  
 1700 FORMAT( I3 , 2X , I4 , 4I3 , F6.2 ,                 
     1             1x, F5.2 , 1x, 2(F8.4 , 1x , F5.3 , 1x ) ,             
     2             3( F7.1 , 1x, F5.1 , 1x ) , F8.4 ,A1 ,                    
     3             F5.1  )                                                  
                                           
C
    1 CONTINUE                                                                  
C                                                                               
C                                                                               
      IF( IWS .NE. 1 ) RETURN                                                   
C     WRITE( 15  ,610 )                                                          
  610 FORMAT(/ )                                                             
C                                                                               
      WRITE( 15 , 614 )                                                          
  614 FORMAT(/, '   RESOLUTION MATRIX' )                                    
      DO  20  I  =  1 , 4                                                       
      WRITE( 15 , 616 ) ( RSL( I , J ) , J = 1 , 4 )                             
  616 FORMAT(  5X , 4F10.4  )                                               
   20 CONTINUE                                                                  
C                                                                               
C     WRITE( 15 , 610 )                                                          
      WRITE( 15 , 612 )                                                          
  612 FORMAT( 26( '-----' )  )                                              
C                                                                               
C     WRITE(15 , 610 )                                                          
C     WRITE(15 , 612 )                                                          
C     STATION DATA                                                              
      WRITE(15 , 620 )                                                          
  620 FORMAT(/, ' *** STATION DATA *** '   )                                
      WRITE(15 , 624 )                                                          
  624 FORMAT( '  STA     DELT  AZMTH RNK --P TIME--   TPFL  TPFSC',            
     1   '  TPCAL  O-C    WP    AIP  RNK --S TIME--   TSFL  TSFSC',              
     2   '  TSCAL  O-C    WS    AIS ' )                                       
C                                                                               
      DO  10  I  =  1 , N                                                       
      IF( IL( I ) .EQ. 0 ) GO TO 10                                             
      TP00  = 0.0                                                               
      TP10  = 0.0                                                               
      TS00  = 0.0                                                               
      TS10  = 0.0                                                               
      RP  =  0.0                                                                
      RS  =  0.0                                                                
      IF( IP(I) .EQ. ISTAR .OR. IP(I) .EQ. IBLK ) GO  TO  30                    
      CALL FLSPT( FMSHD , FMSHH , CRFSP , R(I) , ED , TPFL , TP0(I) )             
      TP00  =  TPFL - T0                                                        
      TP10  =  TP00  +  CORP( IL(I) )                                           
      RP = TP10 - TPC(I)                                                        
   30 CONTINUE                                                                  
      IF( IS(I) .EQ. ISTAR .OR. IS(I) .EQ. IBLK ) GO  TO  32                    
      CALL FLSPT( FMSHD , FMSHH , CRFSS , R(I) , ED , TSFL , TS0(I) )             
      TS00  =  TSFL - T0                                                        
      TS10 = TS00 + CORS( IL(I) )                                               
      RS = TS10 - TSC( I )                                                      
   32 CONTINUE                                                                  
      AZ( I ) = DEG * AZ( I )                                                   
      ANGP( I ) = DEG * ANGP( I )                                               
      ANGS( I ) = DEG * ANGS( I ) 
C
      SECP1=SECP(I)
      SECS1=SECS(I)
      CALL SECHM(SECP1,IHP1,IMP1,SECP2)
      CALL SECHM(SECS1,IHS1,IMS1,SECS2)
      IHP2=IHP(I)+IHP1
      IMP2=IMP(I)+IMP1
      IHS2=IHS(I)+IHS1
      IMS2=IMS(I)+IMS1
C                                              
      WRITE( 15 , 622 ) IAI(I) , R(I) , AZ(I) ,                                  
     1     IP(I) , IHP2 , IMP2 , SECP2 , TP00 , TP10 , TPC(I) ,           
     2     RP , WP(I) , ANGP(I) ,                                               
     4     IS(I) , IHS2 , IMS2 , SECS2 , TS00 , TS10 , TSC(I) ,           
     5     RS , WS(I) , ANGS(I)                                                 
  622 FORMAT(1x, A6 , 2F7.1 , 2( 2X  , A1  , 2I3 , F6.2 ,                     
     1     3F7.2 , F6.2 , F7.2 , F6.1 ) ) 
C
      WRITE( 16 , 1600 ) IAI(I) , ALAT(I), ALON(I), R(I) , AZ(I) ,                                  
     1     IP(I) , IHP2, IMP2 , SECP2 , TP00 , TP10 , TPC(I) ,           
     2     RP , WP(I) , ANGP(I) ,                                               
     4     IS(I) , IHS2 , IMS2 , SECS2 , TS00 , TS10 , TSC(I) ,           
     5     RS , WS(I) , ANGS(I)                                                 
 1600 FORMAT(1x, A6 , 2F9.4,1x, 2F7.1 , 2( 2X  , A1  , 2I3 , F6.2 ,                     
     1     3F7.2 , F6.2 , F7.2 , F6.1 ) )                              
C                             
   10 CONTINUE 
      WRITE( 15 , 612 )                                                                   
      RETURN                                                                    
      END        
C
C     Subroutine PRTOXY
C                                                               
      SUBROUTINE PRTOXY (ALAT,ALONG, ALATO, ALONGO, X,Y,IND)                    
C     PRTOXY TRANSFORMS (X,Y) TO (ALAT,ALONG) IF IND.GE.1                       
C     PRTOXY TRANSFORMS (ALAT,ALONG) TO (X,Y) IF IND.EQ.0                       
C     ALATO,ALONGO  ;  ORIGIN OF COORDINATES                                    
      A=6.377397E3                                                              
      E2=6.674372E-3                                                            
      E12=6.719219E-3                                                           
      RD=5.72958E1                                                              
      DR=1.74533E-2                                                             
      IF (IND .GE. 1)  GO TO 100                                                
      RLAT=ALAT*DR                                                              
      CLAT = COS(RLAT)                                                          
      SLAT = SIN(RLAT)                                                          
      V2 = 1.0 + E12*CLAT**2                                                    
      DAL=ALONG-ALONGO                                                          
      PH1=ALAT+0.5*DR*V2*DAL*DAL*SLAT*CLAT                                      
      RPH1=PH1*DR                                                               
      RPH2=0.5*DR*(PH1+ALATO)                                                   
      R = A*(1.0-E2)/SQRT((1.0-E2*SIN(RPH2)**2)**3)                             
      AN = A/SQRT(1.0-E2*SIN(RPH1)**2)                                          
      Y=(PH1-ALATO)*R*DR                                                        
      X=AN*CLAT*DAL*DR*(1.0+COS(2.0*RLAT)*DAL*DAL*DR*DR/6.0 )                   
      RETURN                                                                    
  100 RLATO=ALATO*DR                                                            
      SLATO = SIN(RLATO)                                                        
      R = A*(1.0-E2)/SQRT((1.0-E2*SLATO**2)**3)                                 
      AN = A/SQRT(1.0-E2*SLATO**2)                                              
      V2 = 1.0 + E12*COS(RLATO)**2                                              
      PH1=ALATO+RD*Y/R                                                          
      RPH1=DR*PH1                                                               
      TPH=TAN(RPH1)                                                             
      CPH=COS(RPH1)                                                             
      F=X/AN                                                                    
      ALAT=PH1-0.5*RD*F*F*V2*TPH                                                
      ALONG=ALONGO+RD*F/CPH-RD*(1.0+2.0*TPH**2)*F**3/(6.0*CPH)                  
      RETURN                                                                    
      END        
C
C     Subroutine TDLT
C                                                               
      SUBROUTINE  TDLT(HL,V,H,DELT,T,DTDD,DTDH     ,AI1,AIH,NLL,                
     1  Z,A,CLK,TLK,LVZ,DEL,N2)                                                 
      DIMENSION A( N2 , N2 ) , CLK( N2 , N2 ) , TLK( N2 , N2 )                  
      DIMENSION HL(N2) , V(N2) , T(N2) , DTDD(N2) , DTDH(N2) , AI1(N2) ,        
     1             DEL(N2) , AIH(N2)                                            
C     PROGRAM FOR TRAVEL TIME CALCULATION FOR MULTI-LAYERED STRUCTURE           
C     PROGRAMMED BY KAZUO SHIBUYA AND MASAKO HORIE , NOVEMBER 1976              
C     AS THE MODIFICATION OF TDLT3 BY HIROO KANAMORI                            
C     EXISTENCE OF LOW VELOCITY LAYERS IS PERMITTED                             
C     HL ... LAYER THICKNESS , V ... VELOCITY STRUCTURE , NLL ... NUMBER        
C     OF LAYERS , H ... FOCAL DEPTH , DELT ... EPICENTRAL DISTANCE              
C     DTDD,DTDH      ... PARTIAL DERIVATIVES OF TRAVEL TIME T                   
C     AI1 .. INCIDENT ANGLE AT STATION , AIH .. EMERGENT ANGLE AT FOCUS         
C     Z ... STATION ELEVATION                                                   
C     LVZ=1 ... LOW VELOCITY LAYERS EXIST                                       
      PI=3.14159                                                                
      H=H+Z                                                                     
      HL(1)=HL(1)+Z                                                             
      HL(NLL)=9000.0                                                            
      DO 1  I=1,NLL                                                             
      T(I)=9999.9                                                               
      AI1(I)=9999.9                                                             
      AIH(I)=9999.9                                                             
      DTDD(I)=9999.9                                                            
      DTDH(I)=9999.9                                                            
    1 CONTINUE                                                                  
      HS=0.0                                                                    
      DO 11  I=1,NLL                                                            
      HS=HS+HL(I)                                                               
C                                                                               
C     REVISE  H.GT.HS TO H.GE.HS (DEC. 24, 1986) RETURN TO ORIGINE(FEB.         
C     1987 )                                                                    
C                                                                               
      IF(H.GT.HS)  GO TO  11                                                    
C                                                                               
C                                                                               
      K=I                                                                       
      KM1=K-1                                                                   
      GO TO  10                                                                 
   11 CONTINUE                                                                  
   10 HMHK=H-(HS-HL(K))                                                         
C *** CALCULATION OF THE TRAVEL TIME AND ITS PARTIAL DERIVATIVES OF THE         
C *** DIRECT WAVE WHEN THE SOURCE IS IN THE K-TH LAYER                          
      IF(K.GT.1)  GO TO  20                                                     
      T(1)=SQRT(H**2+DELT**2)/V(1)                                              
      AI1(1)=ATAN2(DELT,H)                                                      
      AIH(1)=AI1(1)                                                             
      DTDD(1)=DELT/(T(1)*V(1)**2)                                               
      DTDH(1)=H/(T(1)*V(1)**2)                                                  
      GO TO  30                                                                 
   20 X=DELT/H                                                                  
      IJK=1                                                                     
      IF(A(KM1,K).GT.1.00.AND.DELT.NE.0.0)  GO TO  30                           
      IF(LVZ.NE.0)  X=0.10*X                                                    
   80 S1=0.0                                                                    
      S2=0.0                                                                    
      IJK=0                                                                     
      XX=X/SQRT(1.0+X**2)                                                       
      DO 21  IK=1,KM1                                                           
      CR=A(IK,K)*XX                                                             
      IF(CR.LT.1.00)  GO TO  21                                                 
      IJK=1                                                                     
   21 CONTINUE                                                                  
      IF(IJK.EQ.1)  GO TO  30                                                   
      DO 31  IK=1,KM1                                                           
      Q=SQRT(1.0+(1.0-A(IK,K)**2)*X**2)                                         
      S1=S1+HL(IK)*A(IK,K)*X/Q                                                  
   31 S2=S2+HL(IK)*A(IK,K)/Q**3                                                 
      DEL1=S1+HMHK*X                                                            
      G=DEL1-DELT                                                               
      GP=S2+HMHK                                                                
      X=X-G/GP                                                                  
      IF(G.GT.0.01)  GO TO  70                                                  
      RES=ABS(G)-0.01                                                           
      IF(RES) 90,80,80                                                          
   70 DX=0.02*G/GP                                                              
   50 X=X+DX                                                                    
      S1=0.0                                                                    
      DO 41  J=1,KM1                                                            
      Q=SQRT(1.0+(1.0-A(J,K)**2)*X**2)                                          
   41 S1=S1+HL(J)*A(J,K)*X/Q                                                    
      G=S1+HMHK*X-DELT                                                          
      G=G+0.01                                                                  
      IF(G) 50,90,90                                                            
   90 CONTINUE                                                                  
      Q2=SQRT(1.0+X**2)                                                         
      S1=0.0                                                                    
      DO 61  J=1,KM1                                                            
      Q=SQRT(1.0+(1.0-A(J,K)**2)*X**2)                                          
   61 S1=S1+HL(J)*Q2/(Q*V(J))                                                   
      T(K)=S1+HMHK*Q2/V(K)                                                      
      DTDH(K)=1.0/(V(K)*Q2)                                                     
      DTDD(K)=X*DTDH(K)                                                         
      Q=SQRT(1.0+(1.0-A(1,K)**2)*X**2)                                          
      AI1(K)=A(1,K)*X/Q                                                         
      AI1(K)=ATAN(AI1(K))                                                       
      AIH(K)=ATAN(X)                                                            
      SDDEL=9999.9                                                              
   30 CONTINUE                                                                  
      IF(K.GE.NLL.AND.A(KM1,K).GT.1.0) GO TO 2000                               
      IF(K.GE.NLL)  GO TO  1000                                                 
C *** CALCULATION OF THE TRAVEL TIME AND ITS PARTIAL DERIVATIVES OF THE         
C *** HEAD WAVES                                                                
      NLM1=NLL-1                                                                
      DO 91  I=K,NLM1                                                           
      IK=I+1                                                                    
      IF(TLK(I,IK).EQ.9999.9)  TLK(K,IK)=9999.9                                 
   91 CONTINUE                                                                  
      KP1=K+1                                                                   
      DO 101  IL=KP1,NLL                                                        
      ILM1=IL-1                                                                 
      S1=0.0                                                                    
      S2=0.0                                                                    
      DO 111  IK=K,ILM1                                                         
      S1=S1+HL(IK)*TLK(IK,IL)                                                   
  111 S2=S2+HL(IK)/(CLK(IK,IL)*V(IK))                                           
      S3=0.0                                                                    
      S4=0.0                                                                    
      DO 121  IK=1,ILM1                                                         
      S3=S3+HL(IK)*TLK(IK,IL)                                                   
  121 S4=S4+HL(IK)/(CLK(IK,IL)*V(IK))                                           
      DEL1=S1+S3-HMHK*TLK(K,IL)                                                 
      DEL(IL)=DEL1                                                              
      AL=DELT-DEL1                                                              
      IF(AL) 101,100,100                                                        
  100 T(IL)=AL/V(IL)+S2+S4-HMHK/(CLK(K,IL)*V(K))                                
      DTDD(IL)=1.0/V(IL)                                                        
      DTDH(IL)=TLK(K,IL)/V(IL)-1.0/(CLK(K,IL)*V(K))                             
      AI1(IL)=ATAN(TLK(1,IL))                                                   
      AIH(IL)=ASIN(A(K,IL))                                                     
      IF(DTDH(IL).LT.0.0)  AIH(IL)=PI-AIH(IL)                                   
  101 CONTINUE                                                                  
      IF(LVZ.EQ.0.OR.DELT.EQ.0.0)  GO TO  1000                                  
      IF( A(KM1,K).LT.1.00.OR.IJK.EQ.0 )  GO TO  1000                           
      DO 131  I=1,K                                                             
  131 DEL(I)=9999.9                                                             
      CALL SMXMN(DEL,DUM,DEL1,NLL,NDUM,NMIN,N2)                                 
      IF(DELT.GE.DEL1)  GO TO  1000                                             
 2000 CONTINUE                                                                  
C *** TRAVEL TIME CALCULATION OF DIRECT WAVES FOR THE SOURCE IN THE LOW         
C     VELOCITY LAYER                                                            
      DE=PI/180.0                                                               
      X=DELT/H                                                                  
      E=0.5*ATAN(X)                                                             
  110 X=TAN(E)                                                                  
      Q2=SQRT(1.0+X**2)                                                         
      S1=0.0                                                                    
      S2=0.0                                                                    
      DO 141  J=1,KM1                                                           
      Q=SQRT(1.0+(1.0-A(J,K)**2)*X**2)                                          
      S1=S1+HL(J)*A(J,K)*X/Q                                                    
  141 S2=S2+HL(J)*Q2/(Q*V(J))                                                   
      S1=S1+HMHK*X                                                              
      S2=S2+HMHK*Q2/V(K)                                                        
      IF(S1.GT.DELT)  GO TO  60                                                 
      SDDEL=S1                                                                  
      STIME=S2                                                                  
      E=E+DE                                                                    
      GO TO  110                                                                
   60 CONTINUE                                                                  
      DTH=1.0/( V(K)*Q2 )                                                       
      DTD=DTH*X                                                                 
      Q=SQRT(1.0+(1.0-A(1,K)**2)*X**2)                                          
      A1=A(1,K)*X/Q                                                             
      A1=ATAN(A1)                                                               
      AH=ATAN(X)                                                                
      DEL2=S1-SDDEL                                                             
      DEL3=DELT-SDDEL                                                           
      T(K)=STIME+(S2-STIME)*DEL3/DEL2                                           
      AIH(K)=AH*DELT/S1                                                         
      AI1(K)=A1*S1/DELT                                                         
      DTDH(K)=DTH*DELT/S1                                                       
      DTDD(K)=DTD*DELT/S1                                                       
 1000 H=H-Z                                                                     
      HL(1)=HL(1)-Z                                                             
      RETURN                                                                    
      END     
C
C     Subroutine SMXMN
C                                                                  
      SUBROUTINE SMXMN(X,XMAX,XMIN,N,NMAXX,NMINX,N2)                            
      DIMENSION X( N2 )                                                         
      XMAX=X(1)                                                                 
      NMAXX=1                                                                   
      IF( N-1 )  1, 1, 11                                                       
   11 CONTINUE                                                                  
      DO 10  I=2, N                                                             
      IF(X(I)-XMAX)  10,10,20                                                   
   20 XMAX=X(I)                                                                 
      NMAXX=I                                                                   
   10 CONTINUE                                                                  
    1 XMIN=X(1)                                                                 
      NMINX=1                                                                   
      IF( N-1 ) 30, 30, 40                                                      
   40 CONTINUE                                                                  
      DO 60  I= 2, N                                                            
      IF( X(I)- XMIN )  50, 50, 60                                              
   50 XMIN=X(I)                                                                 
      NMINX=I                                                                   
   60 CONTINUE                                                                  
   30 RETURN                                                                    
      END     
C
C     Subroutine FLSPT
C                                                                  
      SUBROUTINE FLSPT (FMSHD,FMSHH,CRFS,D,H,TF,TS        )                       
C   SUPPLY   FMSHD --- MESH POINT OF DELTA                                       
C            FMSHH --- MESH POINT OF HP                                          
C            CRFS --- CORRECTION (FLAT-SPHERE)                                  
C            D,H --- EPICENTRAL DISTANCE AND HYPOCENTRAL DEPTH                  
C            TF --- TRAVEL TIME IN FLAT LAYER                                   
C   DELIVER  TS --- TRAVEL TIME IN LAYERING SPHERE                              
C                                                            
      DIMENSION FMSHD(9),FMSHH(9),CRFS(9,9)                                       
      DO 10 I=1,8                                                               
      IF(D.LT.FMSHD(I)) GO TO 10                                                 
      IF(D.GE.FMSHD(I+1)) GO TO 10                                               
      DO 20 J=1,8                                                               
      IF(H.LT.FMSHH(J)) GO TO 20                                                 
      IF(H.GE.FMSHH(J+1)) GO TO 20                                               
      TF=TS+CRFS(J,I)                                                           
   20 CONTINUE                                                                  
   10 CONTINUE                                                                  
      RETURN                                                                    
      END        
C
C     Subroutine FLSP2
C                                                               
      SUBROUTINE FLSP2 (FMSHD,FMSHH,CRFS,IPS        )                             
C     IPS=0 FOR P AND IPS=1 FOR S                                               
C   READ    FMSHD(I) --- MESH POINT OF DELTA                                     
C           FMSHH(J) --- MESH POINT OF HPC                                       
C           CRFS(J,I) --- CORRECTION (FLAT-SPHERE)                              
C   WRITE   FMSHD,FMSHH,CRFS                                                      
C                                                         
      DIMENSION FMSHD(9),FMSHH(9),CRFS(9,9)                                       
      IF(IPS.EQ.1) GO TO 100                                                    
      READ(12,50)(FMSHD(I),I=1,8)                                                
      READ(12,50)(FMSHH(J),J=1,8)                                                
   50 FORMAT(8F10.0)                                                            
      FMSHD(9)=900.0                                                             
      FMSHH(9)=900.0                                                             
  100 DO 1 I=1,8                                                                
      READ(12,50)(CRFS(J,I),J=1,8)                                              
   51 FORMAT(8F10.0)                                                            
    1 CONTINUE                                                                  
C                                                                               
      IF(IPS.EQ.1) GO TO 101                                                    
      WRITE(15,60)                                                               
   60 FORMAT(///,'FLAT LAYER CORRECTION')                                    
      WRITE(15,61) (FMSHD(I),I=1,9)                                               
   61 FORMAT(3X,'MESH POINT OF DELTA',3X,9F9.1,'  (KM)')                    
      WRITE(15,62) (FMSHH(I),I=1,9)                                               
   62 FORMAT(3X,'MESH POINT OF HPC',5X,9F9.1,'  (KM)')                      
  101 WRITE(15,63) (CRFS(J,1),J=1,8)                                             
   63 FORMAT(3X,'CORRECTION (FLAT-SPHERE)',8F7.2,'  (SEC)')                 
      DO 2 I=2,8                                                                
      WRITE(15,64) (CRFS(J,I),J=1,8)                                             
   64 FORMAT(27X,8F7.2)                                                     
    2 CONTINUE                                                                  
      RETURN                                                                    
      END      
C
C     Subroutine OMIT
C                                                            
      SUBROUTINE OMIT( IOMT )                                                   
      COMMON/OD/ NSEQ , NYR , JYMD , IOTH , IOTM , OTS ,                        
     1           DOT , HLAT , DLAT , HLNG , DLNG , HDEP , DDEP ,                
     2           SIG , NST , NP , NS , X , DX , Y , DY ,                        
     3           FMAMP ,GPMAG ,                                                 
     4           IDXHP , IDXBL , JDMAMP , JDMFP                                 
      CHARACTER*1 JDMAMP , JDMFP                                                
C                                                                               
      DOTM  = 100.                                                              
      DLATM =   5.0                                                             
      DLNGM =   5.0                                                             
      DDEPM = 100.0                                                             
      ABXYM = 999.9                                                             
      SIGM  = 999.9                                                             
      NPM   =   4
      NSM   =   4
      NSTM  =   6
C                                                                               
      IOMT = 1                                                                  
C                                                                               
C     IOMT  +++ 1 : OMIT                                                        
C               0 : WITHIN THE LIMITS                                           
C                                                                               
      IF( NP  .LT. NPM )  RETURN
      IF( NS  .LT. NSM )  RETURN
      IF( NST .LT. NSTM)  RETURN
C 
      IF( DOT .GT. DOTM ) RETURN                                                
      IF( DLAT .GT. DLATM ) RETURN                                              
      IF( DLNG .GT. DLNGM ) RETURN                                              
      IF( DDEP .GT. DDEPM ) RETURN                                              
      ABX = ABS( X )                                                            
      ABY = ABS( Y )                                                            
      IF( ABX .GT. ABXYM ) RETURN                                               
      IF( ABY .GT. ABXYM ) RETURN                                               
      IF( SIG .GT. SIGM )  RETURN                                               
C                                                                               
      IOMT = 0                                                                  
C                                                                               
      RETURN                                                                    
      END 
C
C     Subroutine INV30R
C                                                                      
      SUBROUTINE  INV30R( A, B, N, NDIM, ILL )                                  
      DIMENSION   A(NDIM,NDIM), B(NDIM,NDIM)                                    
C                                                                               
C*******************************************************************            
C*****                                                         *****            
C*****   CALCULATION OF INVERS MATRIX BY GAUSS-JORDAN METHOD   *****            
C*****                                                         *****            
C*******************************************************************            
C                                                                               
C***** INPUT                                                                    
C*****   A    : INPUT MATRIX  A(N,N)                                            
C*****   N    : ORDER OF MATRIX A      ( N .LE. NDIM )                          
C*****   NDIM : DIMENSION OF ARRAY A,B DECLARED IN MAIN ROUTINE                 
C                                                                               
C***** OUTPUT                                                                   
C*****   B    : INVERS MATRIX  B(N,N)                                           
C*****   ILL  : RETURN CODE    =0 ... NORMAL TERMINATION                        
C*****        :                =1 ... INVERSE MATRIX IS NOT OBTAINED            
C                                                                               
      DATA  EPS / 1.E-9 /                                                       
      ILL = 0                                                                   
C*****                                                                          
C***** MAKE UNIT MATRIX                                                         
C*****                                                                          
      DO 2222  J=1,N                                                            
      DO 1111  I=1,N                                                            
        B(I,J) = 0.                                                             
        IF(I.EQ.J)  B(I,J) = 1.                                                 
 1111 CONTINUE                                                                  
 2222 CONTINUE                                                                  
C*****                                                                          
C***** SEARCH MAXIMUM ELEMENT TO GET PIVOT                                      
C*****                                                                          
      DO 7777  M=1,N                                                            
      AMAX = 0.                                                                 
      DO 3333  J=M,N                                                            
        IF( ABS( A(M,J) ) .LT. AMAX )  GO TO 3333                               
        AMAX = ABS( A(M,J) )                                                    
        JMAX = J                                                                
 3333 CONTINUE                                                                  
      IF(AMAX. LT. EPS)  THEN                                                   
        ILL = 1                                                                 
        RETURN                                                                  
      ENDIF                                                                     
C*****                                                                          
C***** INTERCHANGE OF ROW-M AND ROW-JMAX                                        
C*****                                                                          
      DO 4444  I=1,N                                                            
        WORK      = A(I,M)                                                      
        A(I,M)    = A(I,JMAX)                                                   
        A(I,JMAX) = WORK                                                        
        WORK      = B(I,M)                                                      
        B(I,M)    = B(I,JMAX)                                                   
        B(I,JMAX) = WORK                                                        
 4444 CONTINUE                                                                  
C*****                                                                          
C***** MULTIPLICATION OF ROW-M WITH A INVERSE OF PIVOT                          
C*****                                                                          
      AMM = 1./A(M,M)                                                           
      DO 5555  I=1,N                                                            
        A(I,M) = A(I,M)*AMM                                                     
        B(I,M) = B(I,M)*AMM                                                     
 5555 CONTINUE                                                                  
C*****                                                                          
C***** ADDITION OF A MULTIPLE OF ROW-M TO THE OTHER ROW-J                       
C*****                                                                          
      DO 6666  J=1,N                                                            
        IF(J.EQ.M)  GO TO 6666                                                  
        AMJ = A(M,J)                                                            
        DO 6665  I=1,N                                                          
        A(I,J) = A(I,J) - AMJ*A(I,M)                                            
        B(I,J) = B(I,J) - AMJ*B(I,M)                                            
 6665   CONTINUE                                                                
 6666 CONTINUE                                                                  
 7777 CONTINUE                                                                  
C*****                                                                          
      RETURN                                                                    
      END 
C
