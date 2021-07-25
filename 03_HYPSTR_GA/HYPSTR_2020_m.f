C                                                                       
C     SIMULTANEOUSLY DETERMINATION OF HYPOCENTERS AND STATION CORRECTION
C                                                                       
      PARAMETER (MDIM = 50 , NDIM = 60 , KDIM =600 , LDIM = 20)          
      DIMENSION TH( LDIM ) , VP( LDIM ) , VS( LDIM ) , D( LDIM ) ,      
     1          TT( LDIM ) , DTDD( LDIM ) , AI1( LDIM ) ,               
     2          AIH( LDIM ) , A( LDIM , LDIM ) , CLK( LDIM , LDIM),     
     3          TLK( LDIM , LDIM ) , DTDH( LDIM ) , DEL( LDIM )         
      DIMENSION IYMD( MDIM ) , OT( MDIM ) , ELT( MDIM ) ,ELN(MDIM),     
     1          DEP( MDIM ) , EX( MDIM ) , EY( MDIM ) , NDT( MDIM),     
     2          DX( MDIM ) , DY( MDIM ) , DZ( MDIM ) , DOT( MDIM ),     
     3          SDX( MDIM ) , SDY( MDIM ) , SDZ( MDIM ) ,               
     4          SDOT( MDIM ) , STL( MDIM , NDIM ) , TPL(MDIM,NDIM),     
     5          TSL( MDIM , NDIM ) , IRPL( MDIM , NDIM ) ,              
     6          IRSL( MDIM , NDIM ) , IH( MDIM ) , IM( MDIM ) ,         
     7          TP(MDIM,NDIM) , TS(MDIM,NDIM), IRP(MDIM,NDIM) ,         
     8          IRS( MDIM , NDIM ) , TCP( MDIM , NDIM ) ,               
     9          TCS( MDIM , NDIM ) , OMCP( MDIM , NDIM ) ,              
     A          OMCS( MDIM , NDIM ) , WP( MDIM , NDIM ) ,               
     B          WS( MDIM , NDIM ) , STWP( MDIM , NDIM ) ,               
     C          STWS( MDIM , NDIM ) , ASUBP( MDIM , NDIM , 4  ),        
     D          ASUBS( MDIM , NDIM , 4 )                                
     E           , DISKM( MDIM , NDIM ) , AZMRAD( MDIM , NDIM )         
      DIMENSION STA( NDIM ) , SLT( NDIM ) , SLN( NDIM ), SHG(NDIM),     
     1          SX( NDIM ) , SY( NDIM ) , STCP( NDIM ) ,STCS(NDIM),     
     2          DSTCP( NDIM ) , DSTCS( NDIM ) ,                         
     3          SDCP( NDIM ) , SDCS( NDIM )                             
      DIMENSION AA( KDIM , KDIM ) , AAT( KDIM ) , IPVT( KDIM ) ,        
     1          VW( KDIM ) , DET( 2 ), BB( KDIM , KDIM ) ,              
     2          CC( KDIM , KDIM )                                       
     3          , AI(KDIM,KDIM) , AAX(KDIM)                             
      DIMENSION MSHD(9) , MSHH(9) , CRFSP(9,9) , CRFSS(9,9)             
      DIMENSION WHP(4) , WST(2)                                         
      COMMON/H/ H0                                                      
      CHARACTER*3 STA,STL                                               
      CHARACTER*1 IRPL,IRSL,IRP,IRS
      REAL MSHD, MSHH                                     
C                                                                       
      CHARACTER*80 DSNV,DSNST,DSNAT,DSNIN,DSNFL                         
C                                                                       
      DOUBLE PRECISION AA,CC,AI   
C
      OPEN(5, file='input_HYPST_Kuma.dat')
      OPEN(6, file='out_HYPST_2020m1.dat')
      OPEN(7, file='out_HYPST_2020m2.dat', position="append")
      OPEN(8, file='out_HYPST_2020m3.dat', position="append")
C 
      DO 60 I=1,KDIM
      DO 60 J=1,KDIM
      AA(I,J)=0.0
      AI(I,J)=0.0
      BB(I,J)=0.0
      CC(I,J)=0.0
   60 CONTINUE
C
      DO 62 I=1,MDIM
      DO 62 J=1,NDIM
      DO 62 K=1,4
      ASUBP(I,J,K)=0.0
      ASUBS(I,J,K)=0.0
   62 CONTINUE                               
C                                                                       
      NFIX = 1                                                          
      ALT0 = 32.0                                                       
      ALN0 = 131.0                                                      
      NSTOP = 3                                                        
C                                                                       
      READ( 5 , 500 ) ALT0 , ALN0 , H0, NFIX , NSTOP                    
  500 FORMAT( 3F10.3 , 2I10 )                                           
      WRITE(6,632) ALT0,ALN0,H0,NFIX,NSTOP                              
  632 FORMAT(3F10.4, 2I10)                                         
C                                                                       
C     ALT0 , ALN0 ----- ORIGIN OF THE X-Y COORDINATE                    
C     H0          ----- HIGHT CORRESPONDING TO DEP=0KM                  
C     NFIX        ----- STATION NUMBER FIXED AT 0 SEC. ( P PHASE )      
C     NSTOP       ----- ITERATION NUMBER                                
C                                                                       
C     READ FILE NAME                                                    
C     FORMAT --- A80                                                    
C                     DATA IN JCL.CNTL.HYPST                            
C                                                                       
      CALL REDSN(DSNV,DSNST,DSNAT,DSNIN,DSNFL)                          
C                                                                       
C     OPEN FILE                                                         
C                                                                       
      CALL FILOPN(DSNV,DSNST,DSNAT,DSNIN,DSNFL)                         
C                                                                       
      CALL CLPAR( IH , IM ,OT , DOT , EX , DX , EY , DY , DEP ,         
     1            DZ , STCP , DSTCP , STCS , DSTCS , MDIM , NDIM )      
C     INPUT OF CORRECTION DATA OF FLAT-SPHERE TRANSFORMATION            
      CALL FLSP2( MSHD , MSHH , CRFSP , 0 )                             
      CALL FLSP2( MSHD , MSHH , CRFSS , 1 )                             
C                                                                       
C     STATION DATA                                                      
C                                                                       
      CALL STDATA1b( STA , SLT , SLN , SHG , SX , SY , NST , ALT0 ,       
     1             ALN0 , NDIM )                                        
C                                                                       
      CALL OTPT2( STA , SLT , SLN , SHG , SX , SY , NST , ALT0 ,        
     1            ALN0 , NDIM )                                         
C                                                                       
C     VELOCITY STRUCTURE                                                
C                                                                       
      CALL VELSET2( TH , VP , VS , D , NL , LDIM )                       
C                                                                       
      CALL OTPT1( TH , VP , VS , NL , LDIM )                            
C                                                                       
C     ARRIVAL TIME DATA                                                 
C                                                                       
C                                                                       
C     INITIAL HYPOCENTER PARAMETERSAND STATION CORRECTIONS              
C                                                                       
      CALL INITLC( IYMD , IH , IM , OT , ELT , ELN , DEP , EX , EY ,    
     1             ALT0 , ALN0 , NINT , MDIM , STA , STCP , STCS ,      
     2             NST , NDIM )                                         
C                                                                       
      CALL OTPT4( IYMD , IH , IM , OT , ELT , ELN , DEP , EX , EY ,     
     1            ALT0 , ALN0 , NINT , MDIM , STA , STCP , STCS ,       
     2            NST , NDIM )                                          
C                                                                       
C     ARRIVAL TIME DATA                                                 
C                                                                       
      CALL ATDATA2b( STL , TPL , TSL , IRPL , IRSL , NDT , NE , IH , IM,  
     1             MDIM , NDIM  )                                       
C                                                                       
      IF( NE .EQ. NINT ) GO TO 1                                        
      WRITE( 6 , 600 ) NE , NINT                                        
  600 FORMAT(// ,' IRREGULAR PARAMETERS ; NE=', I5, 5X,          
     1         'NINT=' , I5                                  )          
      STOP                                                              
C                                                                       
    1 CONTINUE                                                          
      CALL EVMAT( STA , STL , TPL , TP , TSL , TS , IRPL , IRP ,        
     1            IRSL , IRS , NDT , NE , NST , MDIM , NDIM )           
C                                                                       
      CALL OTPT3( STA , TP , IRP , TS , IRS , NE , NST , MDIM ,         
     1            NDIM  )                                               
C                                                                       
C     WEIGHT SETTING                                                    
C                                                                       
      CALL WEIGHT( IRP , IRS , WP , WS , NE , NST , MDIM , NDIM )       
C                                                                       
C     ITERATION                                                         
C                                                                       
      ITER = 0                                                          
C                                                                       
   10 CONTINUE                                                          
C                                                                       
      ITER = ITER + 1                                                   
      WRITE( 6 , 605 ) ITER                                             
  605 FORMAT(5('    *    ') , 'ITERATION=',I3, 5('    *    '))                                  
C                                                                       
      CALL DISAZ( DISKM , AZMRAD , EX , EY , SX , SY , NE , NST ,       
     1            MDIM , NDIM  )                                        
C                                                                       
C     RESIDUAL VECTOR AND COEFFICIENT MATRIX OF P                       
C                                                                       
      CALL OBSEQ ( DISKM , AZMRAD , IRP , DEP , SHG , NE , NST ,        
     1             MDIM , NDIM , VP , TH , A , CLK , TLK , TT ,         
     2             DTDD , DTDH , AI1 , AIH , DEL , NL , LDIM , TP , OT ,
     3             OMCP , ASUBP , TCP , STCP , MSHD , MSHH , CRFSP )    
C                                                                       
C     FOR S WAVE                                                        
C                                                                       
C                                                                       
      CALL OBSEQ( DISKM , AZMRAD , IRS , DEP , SHG , NE , NST ,         
     1            MDIM , NDIM , VS , TH , A , CLK , TLK , TT ,          
     2            DTDD , DTDH , AI1 , AIH , DEL , NL , LDIM , TS , OT , 
     3            OMCS , ASUBS , TCS , STCS , MSHD , MSHH , CRFSS )     
C                                                                       
C                                                                       
C                                                                       
      IF(ITER .EQ. 1 .OR. ITER .EQ. NSTOP) THEN                         
        CALL OTPT6( IYMD , IH , IM , OT , EX , EY , DEP , STA , DISKM , 
     1              AZMRAD , TP , TCP , OMCP , WP , TS , TCS ,          
     2              OMCS , WS , NE , NST , MDIM , NDIM )                
      ENDIF                                                             
C                                                                       
      CALL SIGM( OMCP , WP , IRP , OMCS , WS , IRS , NE , NST ,         
     1           MDIM , NDIM , NFIX , SIG , SIGP , SIGS ,               
     2           WSIG , WSIGP , WSIGS )                                 
      WRITE( 6 , 640 )                                                  
      WRITE( 6 , 642 ) SIG , SIGP , SIGS                                
      WRITE( 6 , 641 )                                                  
      WRITE( 6 , 642 ) WSIG , WSIGP , WSIGS                             
C                                                                       
C     WEIGHT * EQUATIONS                                                
      CALL CONVW( WP , WS , IRP , IRS , ASUBP , ASUBS , OMCP ,          
     1            OMCS , STWP , STWS , NE , NST , MDIM , NDIM ,         
     2            WHP , WST )                                           
C                                                                       
      CALL AMTRX( AA , KDIM , ASUBP , ASUBS , IRP , IRS ,               
     1            STWP , STWS , NE , NST , MDIM , NDIM , NFIX )         
C                                                                       
      CALL CONVAT( AAT , KMAX , KDIM , ASUBP , ASUBS ,                  
     1             STWP , STWS , OMCP , OMCS , IRP , IRS ,              
     2             NE , NST , MDIM , NDIM , NFIX )                      
C                                                                       
C                                                                       
      DO  32  I  =  1 , KMAX                                            
      DO  32  J  =  1 , KMAX                                            
      BB( I , J ) = AA( I , J )                                         
   32 CONTINUE                                                          
      DO  30  I  =  1 , KMAX                                            
      AA( I , I ) = AA( I , I ) + 1.0                                   
   30 CONTINUE                                                          
C                                                                       
      WRITE( 6 , 630 ) NE , NST , KMAX , NFIX                           
  630 FORMAT(// , 5X ,'NUMBER OF EQ=' , I5 ,'     NUMBER OF ST.=', I5 ,
     1 5x,'NUMBER OF UNKNOWNS=',I5 ,5x,'STATION COR. RESTRAINED =',I5)              
C                                                                       
C   ------------------------------------------------------------------  
C                                                                       
C     REVISE VERSION                                                    
C            CHANGING SUBPROGRAMS FOR ESTIMATION OF INVERSE MATRICES    
C                                                                       
C                     NOV. 02, 1987                                     
C   ------------------------------------------------------------------  
C                                                                       
      CALL INV30R( AA , AI , KMAX , KDIM , ILL )                        
      IF( ILL .NE. 0 ) THEN                                             
         WRITE(6,610) ILL                                               
  610    FORMAT( // , ' INV30R ERROR *** CODE=', I5 )             
      ENDIF                                                             
      DO 40 I = 1 , KMAX                                                
      AAX(I)=0.0                                                        
      DO 40 J = 1 , KMAX                                                
      AAX(I)=AI(I,J)*AAT(J)+AAX(I)                                      
   40 CONTINUE                                                          
      DO 42 I = 1 , KMAX                                                
      AAT(I) = AAX(I)                                                   
   42 CONTINUE                                                          
C                                                                       
C   ------------------------------------------------------------------  
C                                                                       
C                                                                       
      CALL SOL( AAT , DX , DY , DZ , DOT , DSTCP , DSTCS ,              
     1                 KDIM , NE , MDIM , NST , NDIM , NFIX ,           
     2                 WHP , WST )                                      
C                                                                       
      CALL NEWPAR( DX , DY , DZ , DOT , DSTCP , DSTCS , EX ,            
     1             EY , DEP , OT , STCP , STCS , NE , MDIM ,            
     2             NST , NDIM )                                         
C                                                                       
      CALL OTPT7( IYMD , IH , IM , OT , EX , EY , DEP ,DOT,DX,DY,       
     1           DZ , STA , STCP , STCS , DSTCP , DSTCS , NE , NST ,    
     2           MDIM , NDIM , NFIX , ALT0 , ALN0 )                     
C                                                                       
      IF( ITER .LT. NSTOP ) GO  TO  10                                  
C                                                                       
C     STOP ITERATION                                                    
C                                                                       
      ISW = -1                                                          
C                                                                       
C     INVERSE MATRIX                                                    
C                                                                       
C        FIRST STEP --- LU DECOMPOSITION                                
C                                                                       
      DO  34  I  =  1 , KMAX                                            
      DO  34  J  =  1 , KMAX                                            
      CC( I , J ) = BB( I , J )                                         
   34 CONTINUE                                                          
C                                                                       
C                                                                       
C -------------------------------------------------------------------   
C                                                                       
C     REVISED VERSION                                                   
C                                                                       
      CALL INV30R( CC , AI , KMAX , KDIM , ILL )                        
      IF( ILL .NE. 0 ) THEN                                             
         WRITE(6,610) ILL                                               
      ENDIF                                                             
C                                                                       
      DO 44 I=1,KMAX                                                    
      DO 44 J=1,KMAX                                                    
      CC(I,J)=AI(I,J)                                                   
   44 CONTINUE                                                          
C                                                                       
C -------------------------------------------------------------------   
C                                                                       
      CALL DISAZ( DISKM , AZMRAD , EX , EY , SX , SY , NE , NST ,       
     1            MDIM , NDIM  )                                        
      CALL OBSEQ( DISKM , AZMRAD , IRP , DEP , SHG , NE , NST ,         
     1            MDIM , NDIM , VP , TH , A , CLK , TLK , TT ,          
     2            DTDD , DTDH , AI1 , AIH , DEL , NL , LDIM , TP ,      
     3            OT , OMCP , ASUBP , TCP , STCP , MSHD , MSHH , CRFSP )
C                                                                       
      CALL OBSEQ( DISKM , AZMRAD , IRS , DEP , SHG , NE , NST ,         
     1            MDIM , NDIM , VS , TH , A , CLK , TLK , TT ,          
     2            DTDD , DTDH , AI1 , AIH , DEL , NL , LDIM , TS ,      
     3            OT , OMCS , ASUBS , TCS , STCS , MSHD , MSHH , CRFSS )
C                                                                       
      CALL SIGM( OMCP , WP , IRP , OMCS , WS , IRS , NE , NST ,         
     1           MDIM , NDIM , NFIX , SIG , SIGP , SIGS ,               
     2           WSIG , WSIGP , WSIGS  )                                
      WRITE( 6 , 660 )                                                  
  660 FORMAT(  12('----') , '  FINAL SOLUTIONS  ' , 12('----') )   
      WRITE( 6 , 640 )                                                  
  640 FORMAT( '  STANDARD DEV. OF OBSERVATIONAL EQ. SYSTEM(SEC)') 
      WRITE( 6 , 642 ) SIG , SIGP , SIGS                                
      WRITE( 6 , 641 )                                                  
  641 FORMAT('  WEIGHTED MEAN OF RESIDUALS(SEC) ' )              
      WRITE( 6 , 642 ) WSIG , WSIGP , WSIGS                             
  642 FORMAT('SIG(P+S)=',F10.2 , 3X , 'SIG(P)=' , F10.2 , 3X ,    
     1             'SIG(S)=' , F10.5  )                                 
      WRITE( 6 , 650 )                                                  
  650 FORMAT(' TRAVEL TIMES AND (O-C) ' )                        
      CALL OTPT6( IYMD ,IH , IM ,  OT , EX , EY , DEP , STA , DISKM ,   
     1            AZMRAD , TP , TCP , OMCP , WP , TS , TCS ,            
     2            OMCS , WS , NE , NST , MDIM , NDIM  )                 
C                                                                       
      CALL STDV( CC ,        KDIM , SDX , SDY , SDZ , SDOT ,            
     1           NE , MDIM , SDCP , SDCS , NST , NDIM ,                 
     2           NFIX , SIG , WHP , WST )                               
C                                                                       
      WRITE( 6 , 652 )                                                  
  652 FORMAT('  HYPOCENTRAL PARAMETERS AND STATION CORRECTIONS'  
     1              )                                                   
      CALL OTPT7( IYMD ,IH , IM,OT,EX,EY ,DEP,SDOT, SDX , SDY ,         
     1            SDZ , STA ,STCP , STCS , SDCP , SDCS , NE , NST ,     
     2            MDIM , NDIM , NFIX , ALT0 , ALN0 )                    
C                                                                       
C     DIAGONAL ELEMENTS OF COVARIANCE MATRIX                            
C                                                                       
      WRITE( 6 , 674 )                                                  
  674 FORMAT( 1H0,5('+ + + '),'DIAGONAL ELEMENTS OF COV. MATRIX',       
     1     5(' + + + +')  )                                             
      WRITE( 6 , 670 ) ( CC( I , I ) , I = 1 , KMAX )                   
      WRITE( 6 , 662 )                                                  
C                                                                       
C     RESOLUTION MATRIX                                                 
C                                                                       
C     ----- INVERSE MATRIX OF (AA+I) ----                               
C                                                                       
      DO  36  I  =  1 , KMAX                                            
      DO  36  J  =  1 , KMAX                                            
      AA( I , J ) = BB( I , J )                                         
   36 CONTINUE                                                          
      DO  38  I  =  1 , KMAX                                            
      AA( I , I ) = AA( I , I ) + 1.0                                   
   38 CONTINUE                                                          
C                                                                       
C                                                                       
C -------------------------------------------------------------------   
C                                                                       
C     REVISED VERSION                                                   
C                                                                       
      CALL INV30R( AA , AI , KMAX , KDIM , ILL )                        
      IF( ILL .NE. 0 ) THEN                                             
         WRITE(6,610) ILL                                               
      ENDIF                                                             
C                                                                       
      DO 46 I=1,KMAX                                                    
      DO 46 J=1,KMAX                                                    
      AA(I,J)=AI(I,J)                                                   
   46 CONTINUE                                                          
C                                                                       
C -------------------------------------------------------------------   
C     -------------------------------------------------------           
C     ----  R = (AA + I ) -1 * AA ---------------------------           
C     -------------------------------------------------------           
      WRITE( 6 , 662 )                                                  
  662 FORMAT( 1H0 , 12('----------' )  )                                
      WRITE( 6 , 662 )                                                  
      DO  50  I  =  1 , KMAX                                            
      DO  50  J  =  1 , KMAX                                            
      CC( I , J ) = 0.0                                                 
      DO  50  K = 1 , KMAX                                              
      CC( I , J ) = CC( I , J ) + AA( I , K ) * BB( K , J )             
   50 CONTINUE                                                          
      WRITE( 6 , 672 )                                                  
  672 FORMAT(5('+ + + '),'DIAGONAL ELEMENTS OF RESOLUTION MATRIX'  
     1      , 5(' + + + +' ) )                                          
      WRITE( 6 , 670 ) ( CC( I , I ) , I = 1 , KMAX )                   
  670 FORMAT( 8F10.6 )                                             
      WRITE( 6 , 662 )                                                  
C
C     OUTPUT to File No7
C
      WRITE(7,7000) (TH(I),I=1,NL),(VP(I),I=1,NL),(VS(I),I=1,NL), SIG
      WRITE(8,7000) (TH(I),I=1,NL),(VP(I),I=1,NL),(VS(I),I=1,NL), SIG
 7000 FORMAT(25(E12.5,1x))
C
      CLOSE(5)
      CLOSE(6)
      CLOSE(7)
C
      STOP                                                              
      END                                                               
      SUBROUTINE VELSET2( TH , VP , VS , D , NL , NDIM )                 
      DIMENSION TH( NDIM ) , VP( NDIM ) , VS( NDIM ) , D( NDIM )        
C     INPUT OF VELOCITY DATA                                            
C                                                                       
      ZERO  = 0.0000001                                                 
      I  =  0                                                           
    1 CONTINUE                                                          
      I  =  I  +  1                                                     
      READ( 10 , 100, END=20 ) D(I) , VP(I) , VS(I)                     
  100 FORMAT( 8F6.3  )                                                 
      IF( VP( I ) .GT. ZERO ) GO TO 1                                   
   20 NL  =  I  - 1                                                     
      NL1  =  NL  -  1                                                  
      DO  10  I  =  1 , NL1                                             
      I1  =  I  +  1      
      TH( I ) = D( I )                                        
      ! TH( I ) = D( I1 ) - D( I )                                        
   10 CONTINUE                                                          
      TH( NL ) = 99999.9                                                
      RETURN                                                            
      END                                                               
      SUBROUTINE STDATA( STA , SLT , SLN , SHG , SX , SY , NST ,        
     1                   ALT0 , ALN0 , NDIM )                           
C                                                                       
      DIMENSION STA( NDIM ) , SLT( NDIM ) , SLN( NDIM ) ,               
     1          SHG( NDIM ) , SX( NDIM ) , SY( NDIM )                   
      COMMON/H/ H0                                                      
C                                                                       
      CHARACTER*3 STA , BLNK                                            
      DATA BLNK/'   '/                                                  
C                                                                       
C      INPUT OF STATION DATA                                            
C                                                                       
      ZERO = 0.00001                                                    
      I  =  0                                                           
    1 CONTINUE                                                          
      I  =  I  +  1                                                     
      READ( 11 , 100,end=2) STA(I) , SLT(I) , SLN(I) , SHG(I)                
  100 FORMAT(  A3 , 7X , 3F10.4 )                                   
      IF( STA( I ) .NE. BLNK ) GO  TO  1
    2 continue                                
      NST  =  I - 1                                                     
C                                                                       
      DO  10  I  =  1 , NST                                             
      SHG(I)=SHG(I)-H0                                                  
      CALL PRTOXY( SLT(I) , SLN(I) , ALT0 , ALN0 , SX(I) , SY(I) ,      
     1              0   )                                               
   10 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE TRAVEL( DISKM , ED , Z , T , FDTDD , FDTDH ,           
     1                   ANG , V , TH , NL , LVZ , ISET , A ,           
     2                   CLK , TLK , TT , DTDD , DTDH , AI1 ,           
     3                   AIH , DEL , LDIM , MSHD , MSHH , CRFS )        
      DIMENSION V( LDIM ) , TH( LDIM ) , TT( LDIM ) , DTDD( LDIM )      
     1         , DTDH( LDIM ) , AI1( LDIM ) , AIH( LDIM ) ,             
     2          A( LDIM , LDIM ) , CLK( LDIM , LDIM ) ,                 
     3          TLK( LDIM , LDIM ) , DEL( LDIM )                        
      DIMENSION MSHD( 9 ) , MSHH( 9 ) , CRFS( 9 , 9 ) 
      REAL MSHD, MSHH                  
C                                                                       
C     TRAVEL TIMES AND THEIR DERIVATIVES FOR GIVEN STRUCTURE            
C                                                                       
C     NL ; NUMBER OF LAYERS                                             
C     DISKM ; EPICENTER DISTANCE                                        
C     ED ; FOCAL DEPTH                                                  
C     Z  ; STATION HIGHT                                                
C                                                                       
      NL1 = NL - 1                                                      
      IF( ISET .EQ. 0 ) GO  TO  3                                       
      LVZ  =  0                                                         
      DO  10  I  =  1 , NL1                                             
      I1 = I + 1                                                        
      DO  10  J = I1 , NL                                               
      A( I , J ) = V( I ) / V( J )                                      
      IF( V( J ) .LE. V( I )  )  GO  TO  12                             
      CLK( I , J ) = SQRT( 1.0 - A( I , J ) ** 2 )                      
      TLK( I , J ) = A( I , J ) / CLK( I , J )                          
      GO  TO  10                                                        
   12 CONTINUE                                                          
      LVZ  =  1                                                         
      CLK( I , J ) = 9999.9                                             
      TLK( I , J ) = 9999.9                                             
   10 CONTINUE                                                          
      RETURN                                                            
    3 CONTINUE                                                          
C                                                                       
      CALL TDLT( TH , V , ED , DISKM , TT , DTDD , DTDH , AI1 ,         
     1           AIH , NL , Z , A , CLK , TLK,LVZ,DEL,LDIM  )           
C                                                                       
      CALL SMXMN( TT , DUM , TF , NL , NDUM , NMIN , LDIM )             
C                                                                       
      D = DISKM                                                         
      H = ED                                                            
      CALL FLSPT( MSHD , MSHH , CRFS , D , H , TF , T )                 
      FDTDH  =  DTDH( NMIN )                                            
      FDTDD  =  DTDD( NMIN )                                            
      ANG  =  AIH( NMIN )                                               
      RETURN                                                            
      END                                                               
      SUBROUTINE EVMAT( STA , STL , TPL , TP , TSL , TS , IRPL ,        
     1                  IRP , IRSL , IRS , NDT , NE , NST , MDIM ,      
     2                  NDIM )                                          
      DIMENSION STA( NDIM ) , STL( MDIM , NDIM ) ,                      
     1          TPL( MDIM , NDIM ) , TP( MDIM , NDIM ),                 
     2          TSL( MDIM , NDIM ) , TS( MDIM , NDIM ) ,                
     3          IRPL( MDIM , NDIM ) , IRP( MDIM , NDIM ) ,              
     4          IRSL( MDIM , NDIM ) , IRS( MDIM , NDIM )                
     5          , NDT( MDIM )                                           
      CHARACTER*3 STA , STL                                             
      CHARACTER*1 ISTR , IRP , IRPL , IRS , IRSL                        
      DATA ISTR/'*'/                                                    
C                                                                       
C     EVENT MATRICES SET UP                                             
C                                                                       
      DO  10  I  =  1 , NE                                              
      DO  20  J  =  1 , NST                                             
      TP( I , J ) = 0.0                                                 
      TS( I , J ) = 0.0                                                 
      IRP( I , J ) = ISTR                                               
      IRS( I , J ) = ISTR                                               
      DO  22  K  =  1 , NDT( I )                                        
      IF( STL( I , K ) .EQ. STA( J )  )    GO  TO  24                   
   22 CONTINUE                                                          
      GO  TO  20                                                        
   24 CONTINUE                                                          
      TP( I , J ) = TPL( I , K )                                        
      TS( I , J ) = TSL( I , K )                                        
      IRP( I , J ) = IRPL( I , K )                                      
      IRS( I , J ) = IRSL( I , K )                                      
   20 CONTINUE                                                          
   10 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE OBSEQ( DISKM , AZMRAD , IR , DEP , SHG ,               
     1                  NE , NST , MDIM ,                               
     2                  NDIM , V , TH , A , CLK , TLK , TT ,            
     3                  DTDD , DTDH , AI1 , AIH , DEL , NL , LDIM ,     
     4                  TOBS , OT , OMC , ASUB , TC , STC ,             
     5                  MSHD , MSHH , CRFS )                            
      DIMENSION DISKM( MDIM , NDIM ) , AZMRAD( MDIM , NDIM ) ,          
     *          DEP( MDIM ) , SHG( NDIM ) ,                             
     1          IR( MDIM , NDIM ) , V( LDIM ) , TH( LDIM ) ,            
     2      A( LDIM , LDIM ) , CLK( LDIM , LDIM ) , DEL( LDIM ) ,       
     3          TLK( LDIM , LDIM ) , TT( LDIM ) , DTDD( LDIM ) ,        
     4          DTDH( LDIM ) , AI1( LDIM ) , AIH( LDIM ) ,              
     5          TOBS( MDIM , NDIM ) , OT( MDIM ) ,                      
     6          OMC( MDIM , NDIM ) , ASUB( MDIM , NDIM , 4 ) ,          
     7          TC( MDIM , NDIM ) , STC( NDIM ) ,                       
     8          MSHD(9) , MSHH(9) , CRFS(9,9)                           
C                                                                       
      CHARACTER*1 IR , ISTR 
      REAL MSHD, MSHH                                            
      DATA ISTR/'*'/                                                    
C                                                                       
C     OBSERVATIONAL EQUATIONS                                           
C                                                                       
C     AZMRAD ; AZIMUTHS MEASURED FROM NORTH TO EAST IN RAD.             
C                                                                       
      CALL TRAVEL( 0. , 0. , 0. , 0. , 0. , 0. , 0. , V , TH ,          
     1             NL , LVZ , 1 , A , CLK , TLK , TT , DTDD ,           
     2            DTDH , AI1 , AIH , DEL , LDIM , MSHD , MSHH , CRFS )  
      ISET = 0                                                          
      DO  10  I  =  1 , NE                                              
      DO  20  J  =  1 , NST                                             
      AZ  =   AZMRAD( I , J )                                           
      CALL TRAVEL( DISKM(I,J) , DEP(I) , SHG(J) , TC(I,J) ,             
     1             FDTDD , FDTDH , ANG , V , TH , NL , LVZ ,            
     2             ISET , A , CLK , TLK , TT , DTDD , DTDH ,            
     3             AI1 , AIH , DEL , LDIM , MSHD , MSHH , CRFS )        
      IF( IR( I , J ) .EQ. ISTR ) GO  TO  20                            
      OMC( I , J ) = TOBS( I , J ) - TC( I , J ) - OT(I) - STC(J)       
      FS  =  -SIN( AZ )                                                 
      FC  =  -COS( AZ )                                                 
      ASUB( I , J , 1 ) = FS * FDTDD                                    
      ASUB( I , J , 2 ) = FC * FDTDD                                    
      ASUB( I , J , 3 ) = FDTDH                                         
      ASUB( I , J , 4 ) = 1.0                                           
   20 CONTINUE                                                          
   10 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE WEIGHT( IRP , IRS , WP , WS , NE , NST , MDIM ,        
     1                   NDIM )                                         
      DIMENSION IRP( MDIM , NDIM ) , IRS( MDIM , NDIM ) ,               
     1          WP( MDIM , NDIM ) , WS( MDIM , NDIM )                   
      CHARACTER*1 RA , RB , RC , RD , ISTR , IRP , IRS , LRP , LRS      
      DATA RA/'A'/ , RB/'B'/ , RC/'C'/ , RD/'D'/ , ISTR/'*'/            
C     WEIGHT AS TO READING RANKS                                        
C                                                                       
      FSP = 1.0                                                         
C     RATIO OF WP AND WS                                                
      SA  =  0.1                                                        
      SB  =  0.1                                                        
      SC  =  0.1                                                        
      SD  =  1.0                                                        
      WA  =  1.0 / SA                                                   
      WB  =  1.0 / SB                                                   
      WC  =  1.0 / SC                                                   
      WD  =  1.0 / SD                                                   
      DO  10  I  =  1 , NE                                              
      DO  10  J  =  1 , NST                                             
      WP( I , J ) = 0.0                                                 
      WS( I , J ) = 0.0                                                 
      LRP = IRP( I , J )                                                
      LRS = IRS( I , J )                                                
      IF( LRP .EQ. ISTR ) GO  TO  12                                    
      IF( LRP .EQ. RA   ) WP( I , J ) = WA                              
      IF( LRP .EQ. RB   ) WP( I , J ) = WB                              
      IF( LRP .EQ. RC   ) WP( I , J ) = WC                              
      IF( LRP .EQ. RD   ) WP( I , J ) = WD                              
   12 CONTINUE                                                          
      IF( LRS .EQ. ISTR ) GO  TO  10                                    
      IF( LRS .EQ. RA   ) WS( I , J ) = FSP * WA                        
      IF( LRS .EQ. RB   ) WS( I , J ) = FSP * WB                        
      IF( LRS .EQ. RC   ) WS( I , J ) = FSP * WC                        
      IF( LRS .EQ. RD   ) WS( I , J ) = FSP * WD                        
   10 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE CONVAT( AAT , KMAX , KDIM , ASUBP , ASUBS , STWP ,     
     1                   STWS , OMCP , OMCS , IRP , IRS , NE ,          
     2                   NST , MDIM , NDIM , NFIX )                     
      DIMENSION STWP( MDIM , NDIM ) , STWS( MDIM , NDIM ) ,             
     1          ASUBP( MDIM , NDIM , 4 ) , ASUBS( MDIM , NDIM , 4 )     
     2        , OMCP( MDIM , NDIM ) , OMCS( MDIM , NDIM ) ,             
     3          IRP( MDIM , NDIM ) , IRS( MDIM , NDIM ) ,               
     4          AAT( KDIM )                                             
C                                                                       
      CHARACTER*1 IRP , IRS , ISTR                                      
      DATA ISTR/'*'/                                                    
C                                                                       
C     TRANS( AA ) * ( O - C ) VECTOR = AAT                              
C                                                                       
      NE4 = NE * 4                                                      
      DO  10  I  =  1 , NE                                              
      DO  10  KK  = 1 , 4                                               
      K  =  4 * ( I - 1 ) + KK                                          
      AAT( K ) = 0.0                                                    
      DO  12  J  =  1 , NST                                             
      FP  =  1.0                                                        
      FS  =  1.0                                                        
      IF( IRP( I , J ) .EQ. ISTR ) FP = 0.0                             
      IF( IRS( I , J ) .EQ. ISTR ) FS = 0.0                             
      AAT( K ) = AAT( K ) + FP * ASUBP( I , J , KK) * OMCP( I , J )     
     1                    + FS * ASUBS( I , J , KK) * OMCS( I , J )     
   12 CONTINUE                                                          
   10 CONTINUE                                                          
C                                                                       
      K = NE4                                                           
      DO  20  I  =  1 , NST                                             
      IF( I .EQ. NFIX ) GO TO 20                                        
      K = K + 1                                                         
      AAT( K ) = 0.0                                                    
      DO 22 J = 1 , NE                                                  
      AAT( K ) = AAT( K ) + STWP( J , I ) * OMCP( J , I )               
   22 CONTINUE                                                          
   20 CONTINUE                                                          
      DO 30 I = 1 , NST                                                 
      K = K + 1                                                         
      AAT( K ) = 0.0                                                    
      DO 32 J = 1 , NE                                                  
      AAT( K ) = AAT( K ) + STWS( J , I ) * OMCS( J , I )               
   32 CONTINUE                                                          
   30 CONTINUE                                                          
      KMAX = K                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE AMTRX( AA , NA , ASUBP , ASUBS , IRP , IRS ,           
     1                  STWP , STWS , NE , NST , MDIM , NDIM ,          
     2                  NFIX  )                                         
      DIMENSION ASUBP( MDIM , NDIM , 4 ) , ASUBS( MDIM , NDIM , 4 )     
     1        , IRP( MDIM , NDIM ) , IRS( MDIM , NDIM ) ,               
     2          AA( NA , NA ) , STWP( MDIM , NDIM ) ,                   
     3          STWS( MDIM , NDIM )                                     
      CHARACTER*1  IRP  , IRS , ISTR                                    
      DOUBLE PRECISION AA                                               
      DATA ISTR/'*'/                                                    
C                                                                       
C     NFIX ; P STATION CORRECTION OF THIS STATION NO. IS RESTRAINED     
C                                                                       
      IFIX = 0                                                          
      IF( NFIX .NE. 0 ) IFIX = 1                                        
      NSTC = 2 * NST - IFIX                                             
      NE4 = 4 * NE                                                      
      NF = NE4 + NST - IFIX                                             
C                                                                       
C     CLEAR MATRIX AA                                                   
C                                                                       
      DO  10  I  =  1 , NA                                              
      DO  10  J  =  1 , NA                                              
      AA( I , J ) = 0.0                                                 
   10 CONTINUE                                                          
C                                                                       
C     I = 1 , NE4 ; J = 1 , NE4                                         
C                                                                       
      DO  20  I  =  1 , NE                                              
      DO  20  K  =  1 , 4                                               
      IK  =  4 * ( I - 1 ) + K                                          
      DO  20  L  =  1 , 4                                               
      IL  =  4 * ( I - 1 ) + L                                          
      AA( IK , IL ) = 0.0                                               
      DO  20  J  =  1 , NST                                             
      FP  =  1.0                                                        
      FS  =  1.0                                                        
      IF( IRP( I , J ) .EQ. ISTR ) FP = 0.0                             
      IF( IRS( I , J ) .EQ. ISTR ) FS = 0.0                             
      AA( IK , IL ) = AA( IK , IL )                                     
     1              + FP * ASUBP( I , J , K ) * ASUBP( I , J , L )      
     2              + FS * ASUBS( I , J , K ) * ASUBS( I , J , L )      
   20 CONTINUE                                                          
C                                                                       
C     I = 1 , NE4 ; J = NE4 + 1 , NE4 + NSTC                            
C                                                                       
      DO  30  I  =  1 , NE                                              
      DO  30  K  =  1 , 4                                               
      II  =  4 * ( I - 1 ) + K                                          
      JJ  =  NE4                                                        
      DO  32  J  =  1 , NST                                             
      FP  =  1.0                                                        
      IF(  J .EQ. NFIX ) GO  TO  32                                     
      JJ  =  JJ  +  1                                                   
      IF( IRP( I , J ) .EQ. ISTR ) FP = 0.0                             
      AA( II , JJ ) = FP * ASUBP( I , J , K ) * STWP( I , J )           
   32 CONTINUE                                                          
C                                                                       
      DO  34  J  =  1 , NST                                             
      FS  =  1.0                                                        
      JJ  =  NF  +  J                                                   
      IF( IRS( I , J ) .EQ. ISTR ) FS = 0.0                             
      AA( II , JJ ) = FS * ASUBS( I , J , K ) * STWS( I , J )           
   34 CONTINUE                                                          
   30 CONTINUE                                                          
C                                                                       
C     I = NE4 + 1 , NE4 + NSTC ; J = 1 , NE4                            
C                                                                       
      DO  40  II  =  1 , NSTC                                           
      I  =  NE4  +  II                                                  
      DO  40  J  =  1 , NE4                                             
      AA( I , J ) = AA( J , I )                                         
   40 CONTINUE                                                          
C                                                                       
C     I = NE4 + 1 , NE4 + NSTC ; J = NE4 + 1 , NE4 + NSTC               
C                                                                       
      JJ  =  NE4                                                        
      DO  50  J  =  1 , NST                                             
      IF( J .EQ. NFIX ) GO  TO  50                                      
      JJ  =  JJ  +  1                                                   
      AA( JJ , JJ ) = 0.0                                               
      DO  52  I = 1 , NE                                                
      AA( JJ , JJ ) = AA( JJ , JJ ) + STWP( I , J ) * STWP( I , J )     
   52 CONTINUE                                                          
   50 CONTINUE                                                          
      DO  54  J  =  1 , NST                                             
      JJ  =  NF  +  J                                                   
      AA( JJ , JJ ) = 0.0                                               
      DO  56  I  =  1 , NE                                              
      AA( JJ , JJ ) = AA( JJ , JJ ) + STWS( I , J ) * STWS( I , J )     
   56 CONTINUE                                                          
   54 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE CONVW( WP , WS , IRP , IRS , ASUBP , ASUBS ,           
     1                  OMCP , OMCS , STWP , STWS , NE , NST ,          
     2                  MDIM , NDIM  , WHP , WST )                      
      DIMENSION WP( MDIM , NDIM ) , WS( MDIM , NDIM ) ,                 
     1          ASUBP( MDIM , NDIM , 4 ) , ASUBS( MDIM , NDIM , 4 )     
     2        , STWP( MDIM , NDIM ) , STWS( MDIM , NDIM ) ,             
     3          IRP( MDIM , NDIM ) , IRS( MDIM , NDIM ) ,               
     4          OMCP( MDIM , NDIM ) , OMCS( MDIM , NDIM )               
      DIMENSION WHP(4) , WST(2)                                         
      CHARACTER*1 IRP , IRS , ISTR                                      
      DATA ISTR/'*'/                                                    
C     DATA                                                              
      SIGX = 20.0                                                       
      SIGY = 20.0                                                       
      SIGZ =  5.0                                                       
      SIGOT = 2.0                                                       
      SIGP = 0.10                                                       
      SIGS = 0.10                                                       
      WRITE( 6 , 600 )                                                  
  600 FORMAT( 1H0 , 4('+ + + + + '),'DAMPING FACTORS', 4(' + + + + +') )
      WRITE( 6 , 610 ) SIGX , SIGY , SIGZ , SIGOT , SIGP , SIGS         
  610 FORMAT( 1H0 , 'X=', F6.2 , '  Y=', F6.2, '  Z=', F6.2 ,           
     1         '  OT=' , F6.2 , 5X , 'STCP=',F6.2,'  STCS=',F6.2)       
      WRITE( 6,620 )                                                    
  620 FORMAT( 1H0 , 6('+ + + + + + + + ') )                             
C                                                                       
C                                                                       
      WHP(1) = 1.0 / SIGX                                               
      WHP(2) = 1.0 / SIGY                                               
      WHP(3) = 1.0 / SIGZ                                               
      WHP(4) = 1.0 / SIGOT                                              
C                                                                       
      WST(1) = 1.0 / SIGP                                               
      WST(2) = 1.0 / SIGS                                               
C                                                                       
C     CONVOLUTION OF WEIGHT AND COEFFICIENT MATRIX AND RESDUAL VEC.     
C                                                                       
      DO  10  I  =  1 , NE                                              
      DO  10  J  =  1 , NST                                             
      STWP( I , J ) = 0.0                                               
      IF( IRP( I , J ) .EQ. ISTR ) GO  TO  12                           
      DO  14  K  =  1 , 4                                               
      ASUBP( I , J , K ) = WP( I , J ) * ASUBP( I , J , K )             
   14 CONTINUE                                                          
      STWP( I , J ) = WP( I , J )                                       
      OMCP( I , J ) = WP( I , J ) * OMCP( I , J )                       
   12 CONTINUE                                                          
      STWS( I , J ) = 0.0                                               
      IF( IRS( I , J ) .EQ. ISTR ) GO  TO  10                           
      DO  16  K  =  1 , 4                                               
      ASUBS( I , J , K ) = WS( I , J ) * ASUBS( I , J , K )             
   16 CONTINUE                                                          
      STWS( I , J ) = WS( I , J )                                       
      OMCS( I , J ) = WS( I , J ) * OMCS( I , J )                       
   10 CONTINUE                                                          
      DO 20 I = 1 , NE                                                  
      DO 20 J = 1 , NST                                                 
      STWP( I , J ) = STWP( I , J ) / WST( 1 )                          
      STWS( I , J ) = STWS( I , J ) / WST( 2 )                          
      DO 20 K = 1 , 4                                                   
      ASUBP( I , J , K ) = ASUBP( I , J , K ) / WHP( K )                
      ASUBS( I , J , K ) = ASUBS( I , J , K ) / WHP( K )                
   20 CONTINUE                                                          
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE DISAZ( DIST , AZ , EX , EY , SX , SY , NE , NST ,      
     1                  MDIM , NDIM )                                   
      DIMENSION EX( MDIM ) , EY( MDIM ) , SX( NDIM ) , SY( NDIM ) ,     
     1          DIST( MDIM , NDIM ) , AZ( MDIM , NDIM )                 
C                                                                       
C     DISTANCES( KM ) AND AZIMUTHS( RADIAN )                            
C                                                                       
      PIH = 1.570963                                                    
      DO  10  I  =  1 , NE                                              
      DO  10  J  =  1 , NST                                             
      SEX = SX( J ) - EX( I )                                           
      SEY = SY( J ) - EY( I )                                           
      D2 = SEX ** 2 + SEY ** 2                                          
      D = SQRT( D2 )                                                    
      AZR = ATAN2( SEX , SEY )                                          
      DIST( I , J ) = D                                                 
      AZ( I , J ) = AZR                                                 
   10 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE OTPT1( TH , VP , VS , NL , NDIM )                      
      DIMENSION TH( NDIM ) , VP( NDIM ) , VS( NDIM )                    
C                                                                       
C     PRINT OUT OF VELOCITY STRUCTURE                                   
C                                                                       
C     NL ; NUMBER OF LAYERS                                             
C                                                                       
      WRITE( 6 , 600 )                                                  
      DO  10  I  =  1 , NL                                              
      VPVS = VP( I ) / VS( I )                                          
      WRITE( 6 , 610 ) TH( I ) , VP( I ) , VS( I ) , VPVS               
  610 FORMAT( 1H , 10X , F12.1 , 3F12.3  )                              
   10 CONTINUE                                                          
  600 FORMAT( 1H0 , // , 9X , 'THICKNESS(KM)' , 4X ,                    
     1        'VP(KM/S)' , 4X , 'VS(KM/S)', 7X, 'VP/VS' )               
      RETURN                                                            
      END                                                               
      SUBROUTINE OTPT2( STA , SLT , SLN , SHG , SX , SY , NST ,         
     1                  ALT0 , ALN0 , NDIM  )                           
      DIMENSION STA( NDIM ) , SLT( NDIM ) , SLN( NDIM ) ,               
     1          SHG( NDIM ) , SX( NDIM ) , SY( NDIM )                   
      CHARACTER*3 STA                                                   
      WRITE( 6 , 600 ) ALT0 , ALN0                                      
      WRITE( 6 , 602 )                                                  
C                                                                       
      DO 10  I  =  1 , NST                                              
      NUMB  =  I                                                        
      WRITE( 6 , 610 ) NUMB , STA(I) , SLT(I) , SLN(I) , SHG(I) ,       
     1                 SX(I) , SY(I)                                    
  610 FORMAT( 1H , 5X , I5 , 5X , A3 , 2X , 3F10.4 , 2F10.1 )           
   10 CONTINUE                                                          
  602 FORMAT( 1H , 4X , 'ST.NO.' , 4X , 'CODE' , 8X , 'LAT.' ,          
     1        6X , 'LON.' , 5X , 'HIGHT' , 5X , 'X(KM)' ,               
     2        5X , 'Y(KM)'  )                                           
  600 FORMAT( 1H , // , 5X , 'STATION COORDINATES' , 10X ,              
     1           '(LAT0,LON0) = (' , F7.3 , ',' , F8.3 , ')'  )         
      RETURN                                                            
      END                                                               
      SUBROUTINE OTPT4( IYMD ,IH,IM, OT , ELT , ELN , DEP , EX ,        
     1                  EY , ALT0 , ALN0 , NE , MDIM ,                  
     2                  STA , STCP , STCS , NST , NDIM )                
      DIMENSION IYMD( MDIM ) , IH(MDIM), IM(MDIM), OT( MDIM ) ,         
     1         ELT(MDIM), ELN( MDIM ) , DEP( MDIM ) , EX( MDIM ) ,      
     2          EY( MDIM ), STA( NDIM ) , STCP( NDIM ) , STCS( NDIM )   
      CHARACTER*3 STA                                                   
C                                                                       
C     WRITING OF INITIAL HYPOCENTER PARAMETYERS                         
C                                                                       
      WRITE( 6 , 600 )                                                  
  600 FORMAT( 1H0 , // , 5X , 'INITIAL HYPOCENTER PARAMETERS'  )        
      WRITE( 6 , 601 ) ALT0 ,  ALN0                                     
  601 FORMAT( 1H , 10X , 'CENTER OF COORDINATE=(',F7.3,' ,', F7.3,      
     1          ' )'   )                                                
      WRITE( 6 , 602 )                                                  
  602 FORMAT( 1H , 4X , 'EQ.NO.' , 7X , 'YMD' , 8X , 'OT' , 3X ,        
     1        'H' , 3X , 'M' , 3X , 5X , 'S' , 12X , 'LAT.' ,           
     2        6X , 'LON.' , 6X , 'DEP' , 5X , 'X(KM)' , 5X ,            
     3        'Y(KM)'  )                                                
      DO  10  I  =  1 , NE                                              
      NEQ  =  I                                                         
      CALL SECMH( OT(I) , IHX, IMX, S )                                 
      IHEQ=IH(I) + IHX                                                  
      IMEQ=IM(I) + IMX                                                  
      WRITE( 6 , 610 ) NEQ , IYMD(I) , OT(I) ,IHEQ,IMEQ, S , ELT(I),    
     1                 ELN(I) , DEP(I) , EX(I) , EY(I)                  
  610 FORMAT( 1H , 5X , I5 , 2X,I8, F8.2 , 2I4 , F6.2 , 6X ,            
     1        2F10.3 , 3F10.1  )                                        
   10 CONTINUE                                                          
      WRITE( 6 , 620 )                                                  
  620 FORMAT( 1H0 , // , 5X , ' INITIAL VALUES OF STATION CORRECTIONS') 
      WRITE( 6 , 621 )                                                  
  621 FORMAT( 1H , 6X , 'CODE' , 5X , ' ST.CR.(P)' , 5X , ' ST.CR,(S)') 
      DO  20  J  =  1 , NST                                             
      WRITE( 6 , 622 ) STA( J ) , STCP(J) , STCS(J)                     
  622 FORMAT( 1H , 7X , A3 , 2( 5X , F10.2 ) )                          
   20 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE SOL( AAT , DX , DY , DZ , DOT , DSTCP , DSTCS ,        
     1                       KDIM , NE , MDIM , NST , NDIM ,            
     2                IFIX  , WHP , WST )                               
      DIMENSION AAT( KDIM ) , DX( MDIM ) , DY( MDIM ) ,                 
     1          DZ( MDIM ) , DOT( MDIM ) , DSTCP( NDIM ) ,              
     2          DSTCS( NDIM )                                           
      DIMENSION WHP(4) , WST(2)                                         
C                                                                       
C     ITERATIVE CORRECTION VECTORS                                      
C                                                                       
      NE4  =  4 * NE                                                    
      DO  10  I  =  1 , NE                                              
      IK1  =  4 * ( I - 1 ) + 1                                         
      IK2  =  IK1 + 1                                                   
      IK3  =  IK2 + 1                                                   
      IK4  =  IK3 + 1                                                   
      DX( I ) = AAT( IK1 )/ WHP(1)                                      
      DY( I ) = AAT( IK2 )/ WHP(2)                                      
      DZ( I ) = AAT( IK3 )/ WHP(3)                                      
      DOT( I ) = AAT( IK4) / WHP(4)                                     
   10 CONTINUE                                                          
C                                                                       
      IN  =  NE4                                                        
      DO  20  I  =  1 , NST                                             
      DSTCP( I ) = 0.0                                                  
      IF( I .EQ. IFIX ) GO TO 20                                        
      IN  =  IN  +  1                                                   
      DSTCP( I ) = AAT( IN )/ WST(1)                                    
   20 CONTINUE                                                          
C                                                                       
      DO  30  I  =  1 , NST                                             
      IN  =  IN  +  1                                                   
      DSTCS( I ) = AAT( IN )/ WST(2)                                    
   30 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE NEWPAR( DX , DY , DZ , DOT , DSTCP , DSTCS ,           
     1                   EX , EY , DEP , OT , STCP , STCS ,             
     2                   NE , MDIM , NST , NDIM  )                      
      DIMENSION DX( MDIM ) , DY( MDIM ) , DZ( MDIM ) ,                  
     1          DOT( MDIM ) , EX( MDIM ) , EY( MDIM ) ,                 
     2          DEP( MDIM ) , OT( MDIM ) , STCP( NDIM ) ,               
     3          STCS( NDIM ) , DSTCP( NDIM ) , DSTCS( NDIM )            
C     ITERATIVE SOLUTIONS                                               
C                                                                       
      DO  10  I  =  1 , NE                                              
      EX( I ) = EX( I ) + DX( I )                                       
      EY( I ) = EY( I ) + DY( I )                                       
      DEP( I ) = DEP( I ) + DZ( I )                                     
      OT( I ) = OT( I ) + DOT( I )                                      
   10 CONTINUE                                                          
C                                                                       
      DO  20  I  =  1 , NST                                             
      STCP( I ) = STCP( I ) + DSTCP( I )                                
      STCS( I ) = STCS( I ) + DSTCS( I )                                
   20 CONTINUE                                                          
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE STDV( AA    ,     NA , SDX , SDY , SDZ ,SDOT ,         
     1                 NE , MDIM , SDCP , SDCS , NST , NDIM ,           
     2                 IFIX , SIG , WHP , WST )                         
      DIMENSION AA( NA , NA ) , SDX( MDIM ) , SDY( MDIM),SDZ(MDIM),     
     1          SDOT( MDIM ) , SDCP( NDIM ) , SDCS( NDIM )              
      DIMENSION WHP(4) , WST(2)                                         
      DOUBLE PRECISION AA                                               
C                                                                       
C     STANDARD DEVIATIONS OF ITERATIVE SOLUTIONS                        
C                                                                       
C     SIG : SQRT( WEIGHTED SUM OF RES**2 / ( M - N ) )                  
C     M   : NUMBER OF OBSERVATIONAL EQ.                                 
C     N   : NUMBER OF UNKNOWNS                                          
C                                                                       
      NE4  =  4 * NE                                                    
      DO  10  I  =  1 , NE                                              
      IK  =  4 * ( I - 1 )                                              
      IK1 = IK + 1                                                      
      IK2 = IK + 2                                                      
      IK3 = IK + 3                                                      
      IK4 = IK + 4                                                      
       SDX( I ) = SQRT( AA( IK1 , IK1 ) ) * SIG/ WHP(1)                 
       SDY( I ) = SQRT( AA( IK2 , IK2 ) ) * SIG/ WHP(2)                 
      SDZ( I ) = SQRT( AA( IK3 , IK3 ) ) * SIG / WHP(3)                 
      SDOT( I ) = SQRT( AA( IK4 , IK4 ) ) * SIG / WHP(4)                
   10 CONTINUE                                                          
C                                                                       
      IK  = NE4                                                         
      DO  20  I  =  1 , NST                                             
      IF( I .EQ. IFIX ) GO  TO  20                                      
      IK  =  IK  +  1                                                   
      SDCP( I ) = SQRT( AA( IK , IK  ) ) * SIG/ WST(1)                  
   20 CONTINUE                                                          
C                                                                       
      DO  30  I  =  1 , NST                                             
      IK  =  IK  +  1                                                   
      SDCS( I ) = SQRT( AA( IK , IK ) ) * SIG/ WST(2)                   
   30 CONTINUE                                                          
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE OTPT3( STA , TP , IRP , TS , IRS , NE , NST ,          
     1                  MDIM , NDIM  )                                  
      DIMENSION STA( NDIM ) , TP( MDIM , NDIM ) , IRP( MDIM , NDIM ) ,  
     2          TS( MDIM , NDIM ) , IRS( MDIM , NDIM )                  
      CHARACTER*3 STA                                                   
      CHARACTER*1 IRP , IRS                                             
C                                                                       
C     WRITING OF ARRIVAL TIME DATA                                      
C                                                                       
      WRITE( 6 , 600 )                                                  
  600 FORMAT( 1H , // , 10X , 'ARRIVAL TIME DATA'  )                    
  610 FORMAT( 1H0 , 6X , 'CODE' , 8X ,'TP' , 2X , 'RNK' , 7X ,          
     1        'TS' , 2X , 'RNK'  )                                      
      DO  10  I  =  1 , NE                                              
      WRITE( 6 , 640 ) I                                                
  640 FORMAT( 1H , 'EQ. NO.=' , I5  )                                   
      WRITE( 6 , 610 )                                                  
      DO  12  J  =  1 , NST                                             
      WRITE( 6 , 620 ) STA( J ) , TP( I , J ) , IRP( I , J ) ,          
     1                            TS( I , J ) , IRS( I , J )            
  620 FORMAT( 1H , 7X , A3 , 2( F12.2 , 1X , A1 )  )                    
   12 CONTINUE                                                          
      WRITE( 6 , 630 )                                                  
  630 FORMAT( 1H0 )                                                     
   10 CONTINUE                                                          
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE OTPT6(IYMD , IH, IM, OT , EX , EY , DEP , STA ,        
     1     DISKM,AZMRAD,TP , TCP , OMCP , WP , TS , TCS , OMCS , WS ,   
     2                 NE , NST , MDIM , NDIM )                         
      DIMENSION STA( NDIM ) , DISKM(MDIM,NDIM) , AZMRAD(MDIM,NDIM),     
     1          TP( MDIM , NDIM ) , TCP( MDIM , NDIM ) ,                
     2          OMCP( MDIM , NDIM ) , WP( MDIM , NDIM ) ,               
     3          TS( MDIM , NDIM ) , TCS( MDIM , NDIM ) ,                
     4          OMCS( MDIM , NDIM ) , WS( MDIM , NDIM ) ,               
     5          IYMD( MDIM ) , OT( MDIM ) , EX( MDIM ) , EY( MDIM ) ,   
     6          DEP( MDIM ) , IH( MDIM ) , IM( MDIM )                   
      COMMON/H/ H0                                                      
       CHARACTER*3 STA                                                  
C      OUTPUT OF ITERATIVE SOLUTIONS AND CORRESPONDING RESIDUALS        
C                                                                       
      DEG = 180.0 / 3.141592653                                         
C                                                                       
      WRITE( 6 , 600 )                                                  
  600 FORMAT( 1H0 )                                                     
      DO  10  I  =  1 , NE                                              
      DEPH=DEP(I)-H0                                                    
      WRITE( 6 , 610 )IYMD(I),IH(I),IM(I), OT(I) ,EX(I),EY(I),DEPH      
  610 FORMAT( 1H0 , 10X , I8 , 2X,2I3,F6.2,5X,'X=',F8.2,5X,             
     1        'Y=' , F8.2 , 5X , 'DEP=' , F6.2  )                       
      WRITE( 6 , 630 )                                                  
  630 FORMAT( 1H0 , 7X , 'STA' , 3X , 'D(KM)', 3X , 'AZMTH' ,           
     1        7X , 'TP(OBS)' , 3X , 'TP(CAL)' , 3X , '0-C' ,            
     2        5X , 'WP' , 9X , 'TS(OBS)' , 3X , 'TS(CAL)' , 3X ,        
     3        'O-C' , 5X , 'WS' )                                       
      DO  20  J  =  1 , NST                                             
      AZMDG  = DEG * AZMRAD( I , J )                                    
      WP2 = WP( I , J ) ** 2                                            
      WS2 = WS( I , J ) ** 2                                            
      WRITE( 6 , 620 ) STA(J) , DISKM(I , J) , AZMDG , TP( I , J ) ,    
     1                 TCP( I , J ) , OMCP( I , J ), WP2         ,      
     2                 TS( I , J ) , TCS( I , J ) , OMCS( I , J ) ,     
     3                 WS2                                              
  620 FORMAT( 1H , 7X , A3 , 2F8.1 , 4X , 2( 2F10.2 , 2F8.2 , 3X ) )    
C                                                                       
   20 CONTINUE                                                          
   10 CONTINUE                                                          
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE SIGM( OMCP , WP , IRP , OMCS , WS , IRS , NE ,         
     1                 NST , MDIM , NDIM , NFIX , SIG , SIGP ,          
     2                 SIGS , WSIG , WSIGP , WSIGS  )                   
      DIMENSION OMCP( MDIM , NDIM ) , OMCS( MDIM , NDIM ) ,             
     1          WP( MDIM , NDIM ) , WS( MDIM , NDIM ) ,                 
     2          IRP( MDIM , NDIM ) , IRS( MDIM , NDIM )                 
      CHARACTER*1 IRP , IRS , ISTR                                      
      DATA ISTR/'*'/                                                    
      NP  =  0                                                          
      NS  =  0                                                          
      WNP =  0.                                                         
      WNS = 0.                                                          
      SIGP = 0.                                                         
      SIGS = 0.                                                         
      IFIX = 0                                                          
      IF( NFIX .NE. 0 ) IFIX = 1                                        
      K = 4 * NE + 2 * NST - IFIX                                       
      DO  10  I  =  1 , NE                                              
      DO  10  J  =  1 , NST                                             
      IF( IRP( I , J ) .EQ. ISTR ) GO  TO  12                           
      NP  =  NP  +  1                                                   
      WNP  =  WNP  +  WP( I , J )** 2                                   
      SIGP = SIGP +(WP( I , J ) * OMCP( I , J ))**2                     
   12 CONTINUE                                                          
      IF( IRS( I , J ) .EQ. ISTR ) GO  TO  10                           
      NS  =  NS  +  1                                                   
      WNS  =  WNS  +  WS( I , J )** 2                                   
      SIGS = SIGS +(WS( I , J ) * OMCS( I , J ))** 2                    
   10 CONTINUE                                                          
      SIG  =  SIGP  +  SIGS                                             
      N  =  NP  +  NS                                                   
      WN  =  WNP  +  WNS                                                
      SIGP  =  SQRT( SIGP / FLOAT( NP ) )                               
      WSIGP =  SQRT( SIGP / WNP  )                                      
      SIGS  =  SQRT( SIGS / FLOAT( NS ) )                               
      WSIGS =  SQRT( SIGS / WNS  )                                      
      SIG   =  SQRT( SIG / FLOAT( N - K ) )                             
      WSIG  =  SQRT( SIG / WN )                                         
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE OTPT7( IYMD , IH,IM,OT , EX , EY , DEP , DOT ,         
     1        DX,DY,DZ, STA , STCP , STCS , DSTCP , DSTCS , NE , NST ,  
     2                  MDIM , NDIM , NFIX , ALT0 , ALN0 )              
      DIMENSION IYMD( MDIM ) , OT( MDIM ) , EX( MDIM ) , EY( MDIM ) ,   
     1          DEP( MDIM ) , DOT( MDIM ) , DX( MDIM ) , DY( MDIM ) ,   
     2          DZ( MDIM ) , STA( NDIM ) , STCP( NDIM ) , STCS( NDIM ), 
     3          DSTCP( NDIM ) , DSTCS( NDIM ),IH(MDIM),IM(MDIM)         
      COMMON/H/ H0                                                      
      CHARACTER*3 STA                                                   
C                                                                       
C      WRITING OF SOLUTIONS                                             
C                                                                       
      RAD = 0.017453292                                                 
      FLON = COS( ALT0 * RAD ) * 111.195                                
      WRITE( 6 , 610 )                                                  
  610 FORMAT( 1H0 , / , ' HYPOCENTRAL PARAMETERS' )                     
      DO  10  I = 1 , NE                                                
      NUMB = I                                                          
      CALL PRTOXY( ELT , ELN , ALT0 , ALN0 , EX( I ) , EY( I ) , 1 )    
      CALL SECMH( OT( I ) , IHX, IMX, SEC )                             
      DELT  =  DY( I ) / 111.195                                        
      DELN  =  DX( I ) / FLON                                           
      IHEQ=IH(I) + IHX                                                  
      IMEQ=IM(I) + IMX                                                  
      DEPH=DEP(I)-H0                                                    
      WRITE( 6 , 600 ) NUMB , IYMD(I) ,IHEQ,IMEQ, SEC , DOT(I) ,        
     1                 ELT , DELT , ELN , DELN , DEPH, DZ(I),           
     2                 EX(I) , DX(I) , EY(I) , DY(I)                    
  600 FORMAT( 1H , I4 , 2X , I8 , 2I3 , F6.2 , ' (' , F5.2 , ' )' , 2X  
     1        2( F9.4 , ' (' , F7.4 , ' ) ' ), 3( F6.2 , ' (' , F5.2 ,  
     2        ' ) ' )  )                                                
   10 CONTINUE                                                          
      WRITE( 6 , 620 )                                                  
  620 FORMAT( 1H0 , / , ' STATION CORECTIONS'  )                        
      STCP( NFIX ) = 0.0                                                
      DSTCP( NFIX ) = 0.0                                               
      DO  20  I =  1 , NST                                              
      WRITE( 6 , 630 )                                                  
     1                STA( I ) , STCP(I) , DSTCP( I ) ,                 
     2                           STCS(I) , DSTCS( I )                   
  630 FORMAT( 1H ,5X,A3,2X,2(F10.2,' (',F7.2,' )',5X )  )               
   20 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE CLPAR( IH,IM,OT,DOT,EX,DEX,EY,DEY,DEP,DZ ,             
     1                  STCP , DSTCP , STCS , DSTCS , MDIM , NDIM )     
      DIMENSION OT( MDIM ) , DOT( MDIM ) , EX( MDIM ) , DEX( MDIM ) ,   
     1          EY( MDIM ) , DEY( MDIM ) , DEP( MDIM ) , DZ( MDIM ) ,   
     2          STCP( NDIM ) , DSTCP( NDIM ) , STCS( NDIM ) ,           
     3          DSTCS( NDIM ),IH(MDIM),IM(MDIM)                         
C                                                                       
C      CLEAR PARAMETERS                                                 
C                                                                       
      DO 10 I = 1 , MDIM                                                
      IH(I)   = 0                                                       
      IM(I)   = 0                                                       
      OT( I ) = 0.0                                                     
      DOT( I ) = 0.0                                                    
      EX( I ) = 0.0                                                     
      EY( I ) = 0.0                                                     
      DEX( I ) = 0.0                                                    
      DEY( I ) = 0.0                                                    
      DEP( I ) = 0.0                                                    
      DZ( I ) = 0.0                                                     
   10 CONTINUE                                                          
      DO  20  I  =  1 , NDIM                                            
      STCP( I ) = 0.0                                                   
      STCS( I ) = 0.0                                                   
      DSTCP( I ) = 0.0                                                  
      DSTCS( I ) = 0.0                                                  
   20 CONTINUE                                                          
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE FLSPT( MSHD , MSHH , CRFS , D , H , TF , TS )          
C      SUPPLY   MSHD --- MESH POINT OF DELTA                            
C               MSHH --- MESH POINT OF FOCAL DEPTH                      
C               CRFS --- CORRECTION ( FLAT - SPHERE )                   
C               D,H  --- EPICENTRAL DISTANCE AND FOCAL DEPTH            
C               TF   --- TRAVEL TIME IN FLAT LAYER                      
C               TS   --- TRAVEL TIME IN LAYERING SPHERE                 
C                                                                       
      REAL MSHD , MSHH                                                  
      DIMENSION MSHD(9) , MSHH(9) , CRFS(9,9)                           
      DO  10  I  =  1 , 8                                               
      IF( D .LT. MSHD(I) ) GO  TO  10                                   
      IF( D .GE. MSHD( I+1 ) ) GO  TO  10                               
      DO  20  J  =  1 , 8                                               
      IF( H .LT. MSHH(J) ) GO TO 20                                     
      IF( H .GE. MSHH( J+1 ) ) GO  TO  20                               
      TS  =  TF  -  CRFS( J , I )                                       
   20 CONTINUE                                                          
   10 CONTINUE                                                          
      RETURN                                                            
      END                                                               
      SUBROUTINE FILOPN(DSNV,DSNST,DSNAT,DSNIN,DSNFL)                   
C                                                                       
      CHARACTER*80 DSNV,DSNST,DSNAT,DSNIN,DSNFL                         
C                                                                       
C     OPEN FILE                                                         
C                                                                       
C       10 --- DSNV  : VELOCITY DATA                                    
C       11 --- DSNST : STATION DATA                                     
C       12 --- DSNAT : ARRIVAL TIME DATA                                
C       13 --- DSNIN : INITIAL VALUES FOR SOLUTION                      
C       14 --- DSNFL : EARTH FLAT TO SPHERE                             
C                                                                       
      OPEN( 10 , FILE=DSNV  )                             
      OPEN( 11 , FILE=DSNST )                             
      OPEN( 12 , FILE=DSNAT )                             
      OPEN( 13 , FILE=DSNIN )                             
      OPEN( 14 , FILE=DSNFL )                             
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE REDSN(DSNV,DSNST,DSNAT,DSNIN,DSNFL)                    
      CHARACTER*80 DSNV,DSNST,DSNAT,DSNIN,DSNFL                         
C                                                                       
C     READ DATA SET NAME                                                
C                                                                       
      READ( 5 , 500 ) DSNV                                              
      READ( 5 , 500 ) DSNST                                             
      READ( 5 , 500 ) DSNAT                                             
      READ( 5 , 500 ) DSNIN                                             
      READ( 5 , 500 ) DSNFL                                             
  500 FORMAT( A80 )                                                     
      WRITE( 6 , 600 ) DSNV                                             
      WRITE( 6 , 601 ) DSNST                                            
      WRITE( 6 , 602 ) DSNAT                                            
      WRITE( 6 , 603 ) DSNIN                                            
      WRITE( 6 , 604 ) DSNFL                                            
  600 FORMAT( 1H , ' *** DATA SET NAME OF VELOCITY --- ', A80 )         
  601 FORMAT( 1H , ' *** DATA SET NAME OF STATION  --- ', A80 )         
  602 FORMAT( 1H , ' *** DATA SET NAME OF ARRIVALS --- ', A80 )         
  603 FORMAT( 1H , ' *** DATA SET NAME OF INIT. VAL--- ', A80 )         
  604 FORMAT( 1H , ' *** DATA SET NAME OF FLAT     --- ', A80 )         
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE  INV30R( A, B, N, NDIM, ILL )                          
      DIMENSION   A(NDIM,NDIM), B(NDIM,NDIM)                            
      DOUBLE PRECISION A,B,AMM,AMJ,AMAX,WORK                            
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
      SUBROUTINE SECMH( T , IH , IM , SEC )                                     
      IH  =  T / 3600.0                                                         
      IM  =  ( T - 3600. * FLOAT( IH ) ) / 60.                                  
      SEC  =  T - 3600. * FLOAT( IH ) - 60. * FLOAT( IM )                       
      RETURN                                                                    
      END                                                                       
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
      HL(NLL)=90.0                                                            
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
      IF(H.GT.HS)  GO TO  11                                                    
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
      IF(TLK(I,IK).GE.9999.9 .AND. TLK(I,IK) .LT. 10000. )                      
     1          TLK( K , IK ) = 9999.9                                          
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
      SUBROUTINE FLSP2 (MSHD,MSHH,CRFS,IPS        )                             
C     IPS=0 FOR P AND IPS=1 FOR S                                               
C   READ    MSHD(I) --- MESH POINT OF DELTA                                     
C           MSHH(J) --- MESH POINT OF HPC                                       
C           CRFS(J,I) --- CORRECTION (FLAT-SPHERE)                              
C   WRITE   MSHD,MSHH,CRFS                                                      
      REAL MSHD,MSHH                                                            
      DIMENSION MSHD(9),MSHH(9),CRFS(9,9)                                       
      IF(IPS.EQ.1) GO TO 100                                                    
      READ(14,50)(MSHD(I),I=1,8)                                                
      READ(14,50)(MSHH(J),J=1,8)                                                
   50 FORMAT(8F10.0)                                                            
      MSHD(9)=900.0                                                             
      MSHH(9)=900.0                                                             
  100 DO 1 I=1,8                                                                
      READ(14,50)(CRFS(J,I),J=1,8)                                              
    1 CONTINUE                                                                  
C                                                                               
      IF(IPS.EQ.1) GO TO 101                                                    
      WRITE(6,60)                                                               
   60 FORMAT(1H ,///,'FLAT LAYER CORRECTION')                                   
      WRITE(6,61) (MSHD(I),I=1,9)                                               
   61 FORMAT(1H ,3X,'MESH POINT OF DELTA',3X,9F9.1,'  (KM)')                    
      WRITE(6,62) (MSHH(I),I=1,9)                                               
   62 FORMAT(1H ,3X,'MESH POINT OF HPC',5X,9F9.1,'  (KM)')                      
  101 WRITE(6,63) (CRFS(J,1),J=1,8)                                             
   63 FORMAT(1H ,3X,'CORRECTION (FLAT-SPHERE)',8F7.2,'  (SEC)')                 
      DO 2 I=2,8                                                                
      WRITE(6,64) (CRFS(J,I),J=1,8)                                             
   64 FORMAT(1H ,27X,8F7.2)                                                     
    2 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE ATDATA2b( STL , TPL , TSL , IRPL , IRSL , NDT , NE ,             
     1           IH,IM,  MDIM , NDIM )                                          
      DIMENSION STL( MDIM , NDIM ) , TPL( MDIM , NDIM ) ,                       
     1          TSL( MDIM , NDIM ) , IRPL( MDIM , NDIM ) ,                      
     2          IRSL( MDIM , NDIM ) , NDT( MDIM ),IH(MDIM),IM(MDIM)             
C                                                                               
      CHARACTER*3 STL , STA , ISTP , BLNK                                       
      CHARACTER*1 IRP , IRS , IRPL , IRSL , ISTR ,IBLK, IA                      
      DATA ISTP/'PPP'/ , BLNK/'   '/ , ISTR/'*'/ , IBLK/' '/, IA/'A'/           
C                                                                               
      I  =  0                                                                   
    1 CONTINUE                                                                  
      I  =  I  +  1                                                             
      J = 0                                                                     
    2 CONTINUE                                                                  
      J  =  J  +  1                                                             
      READ( 12 , 100, END=4 ) STA , IHP , IMP , SECP , IRP ,                           
     1                       IHS , IMS , SECS , IRS                             
  100 FORMAT( 2X , A3 ,5x, 2( 2I2 , F5.2 , A1,5x ) )                          
      IF( STA .EQ. ISTP ) GO  TO  4                                             
      IF( STA .EQ. BLNK ) GO  TO  3                                             
C                                                                               
      IHE = IH(I)                                                               
      IME = IM(I)                                                               
C                                                                               
      STL( I , J ) = STA                                                        
      TPL( I , J ) = 3600. * FLOAT(IHP-IHE ) + 60. * FLOAT(IMP-IME)             
     1               + SECP                                                     
      TSL( I , J ) = 3600. * FLOAT(IHS-IHE ) + 60. * FLOAT(IMS-IME)             
     1               + SECS
C
C
      IF( IRP .EQ. IBLK ) IRP = IA                                    
      IF( IRS .EQ. IBLK ) IRS = IA                                    
C                                 
C                                                    
      IRPL( I , J ) = IRP                                                       
      IRSL( I , J ) = IRS                                                       
C                                                                               
      IF( IRP .EQ. ISTR ) TPL( I , J ) = 0.0                                    
      IF( IRS .EQ. ISTR ) TSL( I , J ) = 0.0  
C                                                                               
      GO  TO  2                                                                 
    3 CONTINUE                                                                  
      NDT( I ) = J - 1                                                          
      GO  TO  1                                                                 
    4 CONTINUE                                                                  
      NDT(I) = J - 1                                                            
      NE = I                                                                    
      RETURN                                                                    
      END                                                                       
      SUBROUTINE INITLC( IYMD,IH,IM,OT,ELT,ELN , DEP , EX , EY ,                
     1                   ALT0 , ALN0 , NINT , MDIM , STA , STCP ,               
     2                    STCS , NST , NDIM )                                   
      DIMENSION IYMD(MDIM),IH(MDIM),IM(MDIM),OT( MDIM ),ELT( MDIM ),            
     1          ELN( MDIM ) , DEP( MDIM ) , EX( MDIM ) , EY( MDIM ),            
     2          STA( NDIM ) , STCP( NDIM ) , STCS( NDIM )                       
      CHARACTER*3 STA , STX, IBLNK                                              
      DATA IBLNK/'   '/                                                         
C     INITIAL HYPOCENTRAL PARAMETERS                                            
C                                                                               
      I  =  0                                                                   
    1 CONTINUE                                                                  
      I  =  I  +  1                                                             
      READ( 13 , 100, end=2 ) IY,MO,ID , IH(I),IM(I), SEC0 ,
     1      ELT0 , ELN0 ,   DEP0     
  100 FORMAT(
     1     i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,f5.2,1x,f7.4,1x,f8.4,1x,f5.1)                             
      IF( IY .EQ. 0 ) GO  TO  2                                              
      IYMD( I ) = IY*10000+MO*100+ID        
      OT( I ) = SEC0                                                            
      ELT( I ) = ELT0                                                           
      ELN( I ) = ELN0                                                           
      DEP( I ) = DEP0                                                           
      CALL PRTOXY( ELT0 , ELN0 , ALT0 , ALN0 , EX(I) , EY(I) , 0 )              
      GO  TO  1                                                                 
    2 CONTINUE                                                                  
      NINT = I - 1                                                              
C                                                                               
C     STATION CORRECTIONS   : All station corrections were set as 0.0.                                                    
C                                                                               
C   3 CONTINUE                                                                  
C     READ( 13 , 200 ) STX , STCPX , STCSX                                      
C 200 FORMAT( 2X , A3 , 5X , 2F10.2 )                                           
C     IF( STX .EQ. IBLNK ) GO  TO  20                                           
C     DO 10 J = 1 , NST                                                         
C     IF( STX .EQ. STA( J ) ) GO TO 12                                          
C  10 CONTINUE                                                                  
C     WRITE( 6 , 600 ) STX , STCPX , STCSX                                      
C 600 FORMAT( 1H0 , ' INCORRECT STATION CODE ' , A3 , 5X , 2F10.2 )             
C     GO  TO  3                                                                 
C   12 CONTINUE                                                                  
C     STCP( J ) = STCPX                                                         
C     STCS( J ) = STCSX                                                         
C     GO  TO  3                                                                 
C  20 CONTINUE    
C
      STCPX=0.0
      STCSX=0.0
      do 30 i=1,nst
      STCP( i ) = STCPX                                                         
      STCS( i ) = STCSX 
   30 continue                                                        
C
      RETURN                                                                    
      END                                                                       
      SUBROUTINE STDATA1b( STA , SLT , SLN , SHG , SX , SY , NST ,        
     1                   ALT0 , ALN0 , NDIM )                           
C                                                                       
      DIMENSION STA( NDIM ) , SLT( NDIM ) , SLN( NDIM ) ,               
     1          SHG( NDIM ) , SX( NDIM ) , SY( NDIM )                   
      COMMON/H/ H0                                                      
C                                                                       
      CHARACTER*3 STA , BLNK                                            
      DATA BLNK/'   '/                                                  
C
C     SHG(I):Station height in km in input data
C                                                                       
C      INPUT OF STATION DATA                                            
C                                                                       
      ZERO = 0.00001                                                    
      I  =  0                                                           
    1 CONTINUE                                                          
      I  =  I  +  1                                                     
      READ( 11 , 100,end=2) STA(I) , SLT(I) , SLN(I) , SHG(I)           
  100 FORMAT(  A3 , 7X , 3F10.4 )                                   
      IF( STA( I ) .NE. BLNK ) GO  TO  1
    2 continue                                
      NST  =  I - 1                                                     
C                                                                       
      DO  10  I  =  1 , NST                                             
      SHG(I)=SHG(I)-H0                                                  
      CALL PRTOXY( SLT(I) , SLN(I) , ALT0 , ALN0 , SX(I) , SY(I) ,      
     1              0   )                                               
   10 CONTINUE                                                          
      RETURN                                                            
      END
