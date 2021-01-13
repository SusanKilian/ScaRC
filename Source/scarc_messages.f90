!//////////////////////////////////////////////////////////////////////////////////////////////////////
!
! MODULE SCARC_MESSAGES
!
!> \brief Organize directive-based messages services available in ScaRC/UScaRC 
!
!   This includes verbosing, debugging and dumping of different data in the course of the used methods
!
!//////////////////////////////////////////////////////////////////////////////////////////////////////
MODULE SCARC_MESSAGES
  
USE GLOBAL_CONSTANTS
USE PRECISION_PARAMETERS, ONLY: EB, FB
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
USE COMP_FUNCTIONS, ONLY: GET_FILE_NUMBER
USE MESH_VARIABLES, ONLY: MESHES
USE SCARC_CONSTANTS
USE SCARC_TYPES
USE SCARC_VARIABLES

IMPLICIT NONE

CONTAINS

! ------------------------------------------------------------------------------------------------
!> \brief Setup debug file if requested
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MESSAGES
#if defined(WITH_SCARC_VERBOSE) || defined(WITH_SCARC_DEBUG)
INTEGER :: NM, LASTID
#endif

IF (SCARC_ERROR_FILE) HAS_CSV_DUMP = .TRUE.

! If requested, open file for CSV-information about convergence of different solvers
 
IF (HAS_CSV_DUMP) THEN
   IF (MYID == 0) THEN
      WRITE (MSG%FILE_STAT, '(A,A)') TRIM(CHID),'_scarc.csv'
      MSG%LU_STAT = GET_FILE_NUMBER()
      OPEN (MSG%LU_STAT, FILE=MSG%FILE_STAT)
      WRITE(MSG%LU_STAT,*) '  #Pres,   Stack,  #ScaRC,     #CG,     #MG,   Level, #Smooth, SmoType, ', &
                           '#Coarse,     #LU,    Residual,   Cappa'
   ENDIF
ENDIF

! If verbose directive is set, open file for log-information
#ifdef WITH_SCARC_VERBOSE
IF (MYID == 0) THEN
   WRITE (MSG%FILE_MEM, '(A,A)') TRIM(CHID),'_scarc.mem'
   MSG%LU_MEM = GET_FILE_NUMBER()
   OPEN (MSG%LU_MEM, FILE=MSG%FILE_MEM)
   WRITE(MSG%LU_MEM,1001) 'Number','Rank','Name of array','Calling routine', &
                          'State','Type','Dimension','Left1','Right1', &
                          'Left2','Right2','Left3','Right3','Size(array)', &
                          'Sum(LOGICAL)','Sum(INTEGER)','Sum(REAL_EB)','Sum(REAL_FB)'
ENDIF
LASTID = -NSCARC_HUGE_INT
DO NM=LOWER_MESH_INDEX, UPPER_MESH_INDEX
   IF (MYID == LASTID) CYCLE
   WRITE (MSG%FILE_VERBOSE, '(A,A,i3.3)') TRIM(CHID),'.log',MYID+1
   MSG%LU_VERBOSE = GET_FILE_NUMBER()
   OPEN (MSG%LU_VERBOSE, FILE=MSG%FILE_VERBOSE, ACTION = 'readwrite')
   LASTID = MYID
ENDDO
#endif

#ifdef WITH_SCARC_DEBUG
LASTID = -NSCARC_HUGE_INT
DO NM=LOWER_MESH_INDEX, UPPER_MESH_INDEX
   IF (MYID == LASTID) CYCLE
   WRITE (MSG%FILE_DEBUG, '(A,A,i3.3)') TRIM(CHID),'.debug',MYID+1
   MSG%LU_DEBUG = GET_FILE_NUMBER()
   OPEN (MSG%LU_DEBUG, FILE=MSG%FILE_DEBUG, ACTION = 'readwrite')
   LASTID = MYID
ENDDO
#endif

#ifdef WITH_SCARC_VERBOSE
1001 FORMAT(A8,',',A8,',',A30,',',A40,',',A10,',',A10,',',A10,',',A10,',',A10,',',A10,',',A10,',',&
            A10,',',A10,',',A15,',',A15,',',A15,',',A15,',',A15)
#endif
END SUBROUTINE SCARC_SETUP_MESSAGES


! ====================================================================================================
! Start VERBOSE routines
! ====================================================================================================
#ifdef WITH_SCARC_VERBOSE
! ----------------------------------------------------------------------------------------------------
!> \brief Debugging version only: Print out matrix information on specified level for BLENDER
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_VERBOSE_BLENDER_ZONES(NM, NL)
USE SCARC_POINTERS, ONLY: M, L, OL, G, OG, F, XCOR, YCOR, ZCOR, &
                          SCARC_POINT_TO_GRID, SCARC_POINT_TO_OTHER_GRID
INTEGER, INTENT(IN) :: NM, NL
REAL(EB) :: INCX, INCY, INCZ, XCOR0, YCOR0, ZCOR0
INTEGER :: IC, IZL, IZG, ICPT, MAGG, II, JJ, KK, IFACE, IOR0, NOM, ICG, INBR, ICW, ICE, ITYPE = 0
CHARACTER(60) :: CAGG

WRITE(*,*) 'Printing out blender information '
!IF (NL /= NLEVEL_MIN .AND. TYPE_COARSENING /= NSCARC_COARSENING_CUBIC) RETURN

CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

IF (NL == NLEVEL_MIN) THEN
   XCOR => M%X; YCOR => M%Y; ZCOR => M%Z
ELSE
   XCOR => L%XCOR; YCOR => L%YCOR; ZCOR => L%ZCOR
ENDIF

WRITE (CAGG, '(3A,i3.3,A,i3.3,A)') 'python/',TRIM(CHID),'/m',NM,'_l',NL,'.val'
WRITE(*,*) ' ... into ', CAGG
MAGG=GET_FILE_NUMBER()
OPEN(MAGG,FILE=CAGG)

!CALL SCARC_ALLOCATE_INT1(VALUES, 1, L%NX, NSCARC_INIT_ZERO, 'VALUES')

IF (ITYPE == 0) THEN
   WRITE(MAGG,1001) G%NC, G%NC
ELSE
   WRITE(MAGG,1001) G%NC, G%NCE
ENDIF
WRITE(MAGG,1002) L%NX, L%NY, L%NZ
WRITE(MAGG,1003) L%DX, L%DY, L%DZ

IF (NL == NLEVEL_MIN) THEN
   DO KK = 1, L%NZ
      DO JJ = 1, L%NY
         DO II=1, L%NX
            IF (IS_UNSTRUCTURED.AND.L%IS_SOLID(II,JJ,KK)) THEN
               CYCLE
            ELSE
               IC=G%CELL_NUMBER(II,JJ,KK)
               IZL = ABS(G%ZONES_LOCAL(IC))
               IZG = ABS(G%ZONES_GLOBAL(IC))
               ICPT = G%ZONE_CENTERS(IZL)      
    
               XCOR0 = XCOR(II-1)
               ZCOR0 = ZCOR(KK-1)
   
               IF (TWO_D) THEN
                  YCOR0 = 0.0_EB
               ELSE
                  YCOR0 = YCOR(JJ-1)
               ENDIF
   
               IF (IC == ICPT) THEN
                  WRITE(MAGG,1000) IC, -IZG, XCOR0, YCOR0, ZCOR0
               ELSE
                  WRITE(MAGG,1000) IC,  IZG, XCOR0, YCOR0, ZCOR0
               ENDIF
   
            ENDIF
         ENDDO
      ENDDO
   ENDDO
ENDIF

IF (NL > NLEVEL_MIN) THEN
   DO IC = 1, G%NC
      IZL = ABS(G%ZONES_LOCAL(IC))
      IZG = ABS(G%ZONES_GLOBAL(IC))
      WRITE(MAGG,1001) IC, IZG
   ENDDO
ENDIF

! If ITYPE == 1 also plot overlapping information
IF (ITYPE == 1) THEN
   DO IFACE = 1, 6                                  
   
      IOR0 = FACE_ORIENTATION(IFACE)
      F => L%FACE(IOR0)
   
      INCX = 0
      INCY = 0
      INCZ = 0
   
      SELECT CASE(IOR0)
         CASE ( 1)
            INCX = -L%DX
         CASE (-1)
            INCX =  L%DX
         CASE ( 2)
            INCY = -L%DY
         CASE (-2)
            INCY =  L%DY
         CASE ( 3)
            INCZ = -L%DZ
         CASE (-3)
            INCZ =  L%DZ
      END SELECT
   
      DO INBR = 1, F%N_NEIGHBORS
   
         NOM = F%NEIGHBORS(INBR)
         CALL SCARC_POINT_TO_OTHER_GRID(NM, NOM, NL)
   
         DO ICG = OL%GHOST_FIRSTW(IOR0), OL%GHOST_LASTW(IOR0)
            ICW = OG%ICG_TO_ICW(ICG, 1)
            ICE = OG%ICG_TO_ICE(ICG, 1)
            II = G%ICX(ICW) 
            JJ = G%ICY(ICW) 
            KK = G%ICZ(ICW) 
            XCOR0 = XCOR(II-1) + INCX
            ZCOR0 = ZCOR(KK-1) + INCZ
            IF (TWO_D) THEN
               YCOR0 = 0.0_EB
            ELSE
               YCOR0 = YCOR(JJ-1) + INCY
            ENDIF
   WRITE(*,*) 'TODO: FIX BLENDER OUTPUT'
            IZG = ABS(G%ZONES_GLOBAL(ICE))
            WRITE(MAGG,1000) ICE, IZG, XCOR0, YCOR0, ZCOR0
         ENDDO
   
      ENDDO
   ENDDO
ENDIF

!DEALLOCATE(VALUES)
CLOSE(MAGG)

1001 FORMAT(I8,',', I8)
1002 FORMAT(I8,',', I8,',', I8)
1003 FORMAT(E14.6,',',  E14.6,',', E14.6)
1000 FORMAT(I8,',', I8,',', E14.6,',',  E14.6,',', E14.6)
END SUBROUTINE SCARC_VERBOSE_BLENDER_ZONES


! ------------------------------------------------------------------------------------------------
!> \brief Debugging version only: Dump out information for specified quantity
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_VERBOSE_PRESSURE (HP, NM, CNAME)
INTEGER, INTENT(IN) :: NM
REAL(EB), DIMENSION(0:,0:,0:), INTENT(IN) :: HP
CHARACTER(*), INTENT(IN) :: CNAME
CHARACTER(80) :: FN_DUMP, FN_DEBUG1
INTEGER :: LU_DUMP, IX, IY, IZ
INTEGER, SAVE :: LU_DEBUG1
LOGICAL :: BFIRST = .TRUE.

IF (BFIRST) THEN
   WRITE (FN_DEBUG1, '(A,A,A,i3.3)') 'debug/',TRIM(CHID),'_',ICYC
   LU_DEBUG1 = GET_FILE_NUMBER()
   OPEN (LU_DEBUG1, FILE=FN_DEBUG1)
   BFIRST = .FALSE.
ENDIF

WRITE(LU_DEBUG1,*) '==========================================================================='
IF (PREDICTOR) THEN
   WRITE(LU_DEBUG1,*) ' ICYC = ', ICYC, '        PREDICTOR: H'
ELSE
   WRITE(LU_DEBUG1,*) ' ICYC = ', ICYC, '        PREDICTOR: HS'
ENDIF
WRITE(LU_DEBUG1,*) '==========================================================================='
WRITE (FN_DUMP, '(A,A,A,A,A,i3.3)') 'pressure/',TRIM(CHID),'_',TRIM(CNAME),'_',ICYC

LU_DUMP = GET_FILE_NUMBER()
OPEN (LU_DUMP, FILE=FN_DUMP)
DO IZ = 0, MESHES(NM)%KBP1
   IF (TWO_D) THEN
      DO IY = 1, MESHES(NM)%JBAR
         DO IX = 0, MESHES(NM)%IBP1
            WRITE(LU_DUMP,*)  HP(IX, IY, IZ)
         ENDDO
      ENDDO
   ELSE
      DO IY = 0, MESHES(NM)%JBP1
         DO IX = 0, MESHES(NM)%IBP1
            WRITE(LU_DUMP,*)  HP(IX, IY, IZ)
         ENDDO
      ENDDO
   ENDIF
ENDDO
CLOSE(LU_DUMP)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'DUMP_PRESSURE: HP'
#endif
DO IZ = MESHES(NM)%KBP1, 0, -1
   IF (TWO_D) THEN
      DO IY = MESHES(NM)%JBAR, 1, -1
         WRITE(LU_DEBUG1,'(10E14.6)') (HP(IX, IY, IZ), IX = 0, MESHES(NM)%IBP1)
#ifdef WITH_SCARC_DEBUG
         WRITE(MSG%LU_DEBUG,'(10E14.6)') (HP(IX, IY, IZ), IX = 0, MESHES(NM)%IBP1)
#endif
      ENDDO
   ELSE
      DO IY = MESHES(NM)%JBP1, 0, -1
         WRITE(LU_DEBUG1,'(10E14.6)') (HP(IX, IY, IZ), IX = 0, MESHES(NM)%IBP1)
#ifdef WITH_SCARC_DEBUG
         WRITE(MSG%LU_DEBUG,'(10E14.6)') (HP(IX, IY, IZ), IX = 0, MESHES(NM)%IBP1)
#endif
      ENDDO
   ENDIF
ENDDO

END SUBROUTINE SCARC_VERBOSE_PRESSURE


! ------------------------------------------------------------------------------------------------------
!> \brief Verbose version only: Print out Verbose information for compactly stored matrix
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_VERBOSE_CMATRIX(A, CNAME, CTEXT)
CHARACTER(*), INTENT(IN) :: CNAME, CTEXT
TYPE (SCARC_CMATRIX_TYPE), INTENT(INOUT) :: A      
INTEGER :: IC, ICOL
CHARACTER(40) :: CFORM

WRITE(MSG%LU_VERBOSE,*)
WRITE(MSG%LU_VERBOSE,*) '============ START VERBOSE MATRIX ', CNAME, ' AT ', TRIM(CTEXT)
WRITE(MSG%LU_VERBOSE,*) 'INTERNAL NAME OF MATRIX :', A%CNAME
WRITE(MSG%LU_VERBOSE,*) 'REQUESTED SIZES N_ROW, N_VAL:', A%N_ROW, A%N_VAL
WRITE(MSG%LU_VERBOSE,*) 'ALLOCATED SIZES N_ROW, N_VAL:', SIZE(A%ROW), SIZE(A%VAL)

WRITE(MSG%LU_VERBOSE,*)
WRITE(MSG%LU_VERBOSE,*) "------------->", TRIM(CNAME),'%ROW:'
WRITE(MSG%LU_VERBOSE,'(8I12)') (A%ROW(IC), IC=1, A%N_ROW)
WRITE(MSG%LU_VERBOSE,*) "------------->", TRIM(CNAME),'%COL:'
DO IC = 1, A%N_ROW-1
   IF (A%ROW(IC) == 0) CYCLE
   IF (A%ROW(IC+1)-A%ROW(IC) < 10) THEN
      CFORM = "(I8,A,10I12)"
   ELSE IF (A%ROW(IC+1)-A%ROW(IC) < 20) THEN
      CFORM = "(I8,A,20I12)"
   ELSE IF (A%ROW(IC+1)-A%ROW(IC)  < 30) THEN
      CFORM = "(I8,A,30I12)"
   ELSE
      CFORM = "(I8,A,40I12)"
   ENDIF
   WRITE(MSG%LU_VERBOSE,CFORM) IC,':', (A%COL(ICOL), ICOL=A%ROW(IC), A%ROW(IC+1)-1)
ENDDO
IF (ALLOCATED(A%COLG)) THEN
   WRITE(MSG%LU_VERBOSE,*) "------------->", TRIM(CNAME),'%COLG:'
   DO IC = 1, A%N_ROW-1
      IF (A%ROW(IC) == 0) CYCLE
      IF (A%ROW(IC+1)-A%ROW(IC) < 10) THEN
         CFORM = "(I8,A,10I12)"
      ELSE IF (A%ROW(IC+1)-A%ROW(IC) < 20) THEN
         CFORM = "(I8,A,20I12)"
      ELSE IF (A%ROW(IC+1)-A%ROW(IC) < 30) THEN
         CFORM = "(I8,A,30I12)"
      ELSE
         CFORM = "(I8,A,40I12)"
      ENDIF
      WRITE(MSG%LU_VERBOSE,CFORM) IC,':', (A%COLG(ICOL), ICOL=A%ROW(IC), A%ROW(IC+1)-1)
   ENDDO
ENDIF
IF (ALLOCATED(A%VAL)) THEN
WRITE(MSG%LU_VERBOSE,*) "------------->", TRIM(CNAME),'%VAL:'
DO IC = 1, A%N_ROW-1
   IF (A%ROW(IC) == 0) CYCLE
      IF (A%ROW(IC+1)-A%ROW(IC) < 10) THEN
         CFORM = "(I8,A,10E10.2)"
      ELSE IF (A%ROW(IC+1)-A%ROW(IC) < 20) THEN
         CFORM = "(I8,A,20E10.2)"
      ELSE IF (A%ROW(IC+1)-A%ROW(IC) < 30) THEN
         CFORM = "(I8,A,30E10.2)"
      ELSE
         CFORM = "(I8,A,40E10.2)"
      ENDIF
   !WRITE(MSG%LU_VERBOSE,CFORM) IC,':', (A%VAL(ICOL), ICOL=A%ROW(IC), A%ROW(IC+1)-1)
   WRITE(MSG%LU_VERBOSE,*) IC,':', (A%VAL(ICOL), ICOL=A%ROW(IC), A%ROW(IC+1)-1)
ENDDO
WRITE(MSG%LU_VERBOSE,*) '============ END VERBOSE MATRIX ', CNAME, ' AT ', TRIM(CTEXT)
ENDIF

END SUBROUTINE SCARC_VERBOSE_CMATRIX



! ------------------------------------------------------------------------------------------------
!> \brief Debugging version only: Dump out information for specified quantity
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_VERBOSE_VECTOR1 (VC, NM, NL, NG, CNAME)
USE SCARC_POINTERS, ONLY: L, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NM, NL, NG
REAL(EB), DIMENSION(:), INTENT(IN) :: VC
REAL(EB), DIMENSION(0:100) :: VALUES
CHARACTER(*), INTENT(IN) :: CNAME
CHARACTER(80) :: FN_DUMP
INTEGER :: IC, IX, IY, IZ
INTEGER, SAVE :: LU_DUMP

CALL SCARC_POINT_TO_GRID (NM, NL)                    
   
WRITE (FN_DUMP, '(A,A,A,A,A,i3.3)') 'pressure/',TRIM(CHID),'_',TRIM(CNAME),'_',ICYC

LU_DUMP = GET_FILE_NUMBER()
OPEN (LU_DUMP, FILE=FN_DUMP)
!WRITE(LU_DUMP,*) '============================================================='
!WRITE(LU_DUMP,*) ' DEBUG vector ', CNAME
!WRITE(LU_DUMP,*) '============================================================='
DO IZ = L%NZ, 1, -1
   IF (.NOT.TWO_D) WRITE(LU_DUMP,*) '-------- IZ = ', IZ,' ------------------------------------------'
   DO IY = L%NY, 1, -1
      DO IX = 1, L%NX
         IF (NG == NSCARC_GRID_UNSTRUCTURED .AND. L%IS_SOLID(IX,IY,IZ)) THEN
            VALUES(IX)=0.0_EB
         ELSE
            IF (NG == NSCARC_GRID_STRUCTURED) THEN
               IC=L%STRUCTURED%CELL_NUMBER(IX,IY,IZ)
            ELSE
               IC=L%UNSTRUCTURED%CELL_NUMBER(IX,IY,IZ)
            ENDIF
            IF (ABS(VC(IC))<1.0E-14_EB) THEN
               VALUES(IX)=0.0_EB
            ELSE
               VALUES(IX)=VC(IC)
            ENDIF
         ENDIF
      ENDDO
      WRITE(LU_DUMP,'(E14.6)') VALUES(1:L%NX)
   ENDDO
ENDDO
!WRITE(LU_DUMP,*) '============================================================='
CLOSE(LU_DUMP)

END SUBROUTINE SCARC_VERBOSE_VECTOR1

SUBROUTINE SCARC_VERBOSE_VECTOR3 (HP, CNAME)
USE SCARC_POINTERS, ONLY: L
REAL(EB), DIMENSION(0:,0:,0:), INTENT(IN) :: HP
CHARACTER(*), INTENT(IN) :: CNAME
CHARACTER(80) :: FN_DUMP
INTEGER :: IX, IY, IZ
INTEGER, SAVE :: LU_DUMP

WRITE (FN_DUMP, '(A,A,A,A,A,i3.3)') 'pressure/',TRIM(CHID),'_',TRIM(CNAME),'_',ICYC

LU_DUMP = GET_FILE_NUMBER()
OPEN (LU_DUMP, FILE=FN_DUMP)
DO IZ = 1, L%NZ
   DO IY = 1, L%NY
      DO IX = 1, L%NX
         WRITE(LU_DUMP,*) HP(IX, IY, IZ)
      ENDDO
   ENDDO
ENDDO
CLOSE(LU_DUMP)

END SUBROUTINE SCARC_VERBOSE_VECTOR3
#endif


#ifdef WITH_SCARC_DEBUG
! ------------------------------------------------------------------------------------------------
!> \brief Debugging version only: Debug different vectors within a single method
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEBUG_METHOD(CTEXT, NTYPE)
USE SCARC_POINTERS, ONLY: M, L, MGM, A, G, SCARC_POINT_TO_CMATRIX
CHARACTER(*), INTENT(IN) :: CTEXT
INTEGER, INTENT(IN) :: NTYPE
INTEGER :: I, K, NM

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   M   => MESHES(NM)
   L   => SCARC(NM)%LEVEL(NLEVEL_MIN)
   MGM => L%MGM

   WRITE(MSG%LU_DEBUG,*) ' -------------------------------------------------------------------- '
   WRITE(MSG%LU_DEBUG,*) ' -----       ', TRIM(CTEXT)
   WRITE(MSG%LU_DEBUG,*) ' -------------------------------------------------------------------- '
   WRITE(MSG%LU_DEBUG,*) 'PRES_ON_WHOLE_DOMAIN=', PRES_ON_WHOLE_DOMAIN
   WRITE(MSG%LU_DEBUG,*) 'PREDICTOR           =', PREDICTOR
   IF (NTYPE == 7) THEN
      IF (IS_STRUCTURED) THEN
         G => L%STRUCTURED
         IF (IS_POISSON) THEN
            A => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_POISSON)
            CALL SCARC_DEBUG_CMATRIX(A, 'POISSON','STRUCTURED')
         ELSE IF (IS_LAPLACE) THEN
            A => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_LAPLACE)
            CALL SCARC_DEBUG_CMATRIX(A, 'LAPLACE','STRUCTURED')
         ENDIF
      ELSE
         G => L%UNSTRUCTURED
         IF (IS_POISSON) THEN
            A => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_POISSON)
            CALL SCARC_DEBUG_CMATRIX(A, 'POISSON','UNSTRUCTURED')
         ELSE IF (IS_LAPLACE) THEN
            A => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_LAPLACE)
            CALL SCARC_DEBUG_CMATRIX(A, 'LAPLACE','UNSTRUCTURED')
         ENDIF
      ENDIF
   ELSE IF (NTYPE == 6) THEN
      IF (PREDICTOR) THEN
         WRITE(MSG%LU_DEBUG,*) 'H'
         WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MESHES(NM)%H(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      ELSE
         WRITE(MSG%LU_DEBUG,*) 'HS'
         WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MESHES(NM)%HS(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      ENDIF
   ELSE IF (NTYPE == 5) THEN
      WRITE(MSG%LU_DEBUG,*) 'MGM%HS'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%HS(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      WRITE(MSG%LU_DEBUG,*) 'MGM%HU'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%HU(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      WRITE(MSG%LU_DEBUG,*) 'MGM%HD'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%HD(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
   ELSE
   WRITE(MSG%LU_DEBUG,*) 'FVX'
   WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MESHES(MYID+1)%FVX(I,1,K), I=0, M%IBAR), K=M%KBAR,0,-1)
   !WRITE(MSG%LU_DEBUG,*) 'FVZ'
   !WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MESHES(MYID+1)%FVZ(I,1,K), I=0, M%IBAR), K=M%KBAR,0,-1)
   IF (IS_MGM) THEN
      WRITE(MSG%LU_DEBUG,*) 'MGM%H1'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H1(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      WRITE(MSG%LU_DEBUG,*) 'MGM%H2'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H2(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      IF (TYPE_MGM_BC == NSCARC_MGM_BC_EXPOL) THEN
         WRITE(MSG%LU_DEBUG,*) 'MGM%H4'
         WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H4(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      ENDIF
      WRITE(MSG%LU_DEBUG,*) 'MGM%H5'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H5(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      IF (NTYPE >= 2) THEN
         !WRITE(MSG%LU_DEBUG,*) 'MGM%H3(.,0,.)'
         !WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H3(I,0,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
         WRITE(MSG%LU_DEBUG,*) 'MGM%H3(.,1,.)'
         WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H3(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
         !WRITE(MSG%LU_DEBUG,*) 'MGM%H3(.,2,.)'
         !WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H3(I,2,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      ENDIF
      IF (NTYPE >= 1) THEN
         WRITE(MSG%LU_DEBUG,*) 'H'
         WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MESHES(MYID+1)%H(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
         WRITE(MSG%LU_DEBUG,*) 'HS'
         WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MESHES(MYID+1)%HS(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      ENDIF
   ENDIF
   !IF (IS_MGM.AND.NTYPE >=2) THEN
      !WRITE(MSG%LU_DEBUG,*) 'MGM%UP'
      !WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MGM%UP(I,1,K), I=0, M%IBAR), K=M%KBAR,0,-1)
      !WRITE(MSG%LU_DEBUG,*) 'MGM%UL'
      !WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H2(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      !WRITE(MSG%LU_DEBUG,*) 'MGM%UU'
      !WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MGM%UU(I,1,K), I=0, M%IBAR), K=M%KBAR,0,-1)
   !ELSE
      IF (IS_MGM) THEN
         WRITE(MSG%LU_DEBUG,*) 'MGM%UU'
         WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MGM%UU(I,1,K), I=0, M%IBAR), K=M%KBAR,0,-1)
      ENDIF
      WRITE(MSG%LU_DEBUG,*) 'MESHES(MYID+1)%U'
      WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MESHES(MYID+1)%U(I,1,K), I=0, M%IBAR), K=M%KBAR,0,-1)
      WRITE(MSG%LU_DEBUG,*) 'MESHES(MYID+1)%US'
      WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MESHES(MYID+1)%US(I,1,K), I=0, M%IBAR), K=M%KBAR,0,-1)
      !WRITE(MSG%LU_DEBUG,*) 'MESHES(MYID+1)%W'
      !WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MESHES(MYID+1)%W(I,1,K), I=0, M%IBAR), K=M%KBAR,0,-1)
      !WRITE(MSG%LU_DEBUG,*) 'MESHES(MYID+1)%WS'
      !WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MESHES(MYID+1)%WS(I,1,K), I=0, M%IBAR), K=M%KBAR,0,-1)
      !WRITE(MSG%LU_DEBUG,*) 'MGM%WW'
      !WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MGM%WW(I,1,K), I=0, M%IBAR), K=M%KBAR,0,-1)
   !ENDIF
   !WRITE(MSG%LU_DEBUG,*) 'MGM%WP'
   !WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MGM%WP(I,1,K), I=0, M%IBAR), K=M%KBAR,0,-1)
   !WRITE(MSG%LU_DEBUG,*) 'MGM%WL'
   !WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MGM%WL(I,1,K), I=0, M%IBAR), K=M%KBAR,0,-1)
   !WRITE(MSG%LU_DEBUG,*) 'MGM%WW'
   !WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MGM%WW(I,1,K), I=0, M%IBAR), K=M%KBAR,0,-1)
   ENDIF
ENDDO

END SUBROUTINE SCARC_DEBUG_METHOD

! ------------------------------------------------------------------------------------------------
!> \brief Debugging version only: Dump out information for specified quantity
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEBUG_VECTOR3 (VC, NM, NL, CNAME)
USE SCARC_POINTERS, ONLY: L, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NM, NL
REAL(EB), DIMENSION(:,:,:), INTENT(IN) :: VC
CHARACTER(*), INTENT(IN) :: CNAME
INTEGER :: IX, IY, IZ

CALL SCARC_POINT_TO_GRID (NM, NL)                    
WRITE(MSG%LU_DEBUG,*) '============================================================='
WRITE(MSG%LU_DEBUG,*) ' DEBUG vector ', CNAME
WRITE(MSG%LU_DEBUG,*) '============================================================='
DO IZ = L%NZ, 1, -1
   IF (.NOT.TWO_D) WRITE(MSG%LU_DEBUG,*) '-------- IZ = ', IZ,' ------------------------------------------'
   DO IY = L%NY, 1, -1
      WRITE(MSG%LU_DEBUG,'(8E14.6)') (VC(IX, IY, IZ), IX = 1, L%NX)
   ENDDO
ENDDO
WRITE(MSG%LU_DEBUG,*) '============================================================='

END SUBROUTINE SCARC_DEBUG_VECTOR3


! ------------------------------------------------------------------------------------------------
!> \brief Debugging version only: Dump out information for specified quantity
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEBUG_VECTOR3_BIG (HH, NM, CNAME)
INTEGER, INTENT(IN) :: NM
REAL(EB), DIMENSION(0:,0:,0:), INTENT(IN) :: HH
CHARACTER(*), INTENT(IN) :: CNAME
INTEGER :: IX, IY, IZ

WRITE(MSG%LU_DEBUG,*) '============================================================='
WRITE(MSG%LU_DEBUG,*) ' DEBUG VECTOR3 ', CNAME
WRITE(MSG%LU_DEBUG,*) '============================================================='
DO IZ = MESHES(NM)%KBP1, 0, -1
   !IF (.NOT.TWO_D) WRITE(MSG%LU_DEBUG,*) '-------- IZ = ', IZ,' ------------------------------------------'
   !WRITE(MSG%LU_DEBUG,*) '-------- IZ = ', IZ,' ------------------------------------------'
   !DO IY = MESHES(NM)%JBP1, 0, -1
   DO IY = MESHES(NM)%JBAR, 1, -1
      WRITE(MSG%LU_DEBUG,'(10E14.6)') (HH(IX, IY, IZ), IX = 0, MESHES(NM)%IBP1)
   ENDDO
ENDDO
WRITE(MSG%LU_DEBUG,*) '============================================================='

END SUBROUTINE SCARC_DEBUG_VECTOR3_BIG



! ================================================================================================
! Start  WITH_SCARC_DEBUG  - Part
! Collection of routines which print out different quantities or allow to preset them
! ================================================================================================
! ------------------------------------------------------------------------------------------------------
!> \brief Debugging version only: Print out debug information for integer vector
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEBUG_INT1(ARR, I1, I2, CNAME, CTEXT)
INTEGER, DIMENSION(:), INTENT(IN) :: ARR
INTEGER, INTENT(IN) :: I1, I2
CHARACTER(*), INTENT(IN) :: CNAME, CTEXT
INTEGER :: IC
WRITE(MSG%LU_DEBUG,*) '============ DEBUGGING INT1 ARRAY ', CNAME, ' AT ', TRIM(CTEXT)
WRITE(MSG%LU_DEBUG,'(8I6)') (ARR(IC), IC=I1, I2)
END SUBROUTINE SCARC_DEBUG_INT1


! ------------------------------------------------------------------------------------------------------
!> \brief Debugging version only: Print out debug information for double precision vector
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEBUG_REAL1(ARR, I1, I2, CNAME, CTEXT)
REAL(EB), DIMENSION(:), INTENT(IN) :: ARR
CHARACTER(*), INTENT(IN) :: CNAME, CTEXT
INTEGER, INTENT(IN) :: I1, I2
INTEGER :: IC
WRITE(MSG%LU_DEBUG,*) '============ DEBUGGING REAL1 ARRAY ', CNAME, ' AT ', TRIM(CTEXT)
WRITE(MSG%LU_DEBUG,'(8E14.6)') (ARR(IC), IC=I1, I2)
END SUBROUTINE SCARC_DEBUG_REAL1


! ------------------------------------------------------------------------------------------------------
!> \brief Debugging version only: Print out debug information for aggregation zones
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEBUG_ZONES(G, IC, ITYPE, CTEXT)
TYPE (SCARC_GRID_TYPE), POINTER, INTENT(IN) :: G
INTEGER, INTENT(IN) :: IC, ITYPE
CHARACTER(*), INTENT(IN) :: CTEXT
WRITE(MSG%LU_DEBUG,*) '================= DEBUG_ZONES at ', CTEXT
IF (IC /= -1) WRITE(MSG%LU_DEBUG,*) ' IC = ', IC
IF (ITYPE == 1) THEN
   WRITE(MSG%LU_DEBUG,*) '-------------- ZONES_LOCAL: internal:'
   WRITE(MSG%LU_DEBUG,'(8I12)') G%ZONES_LOCAL(1:G%NC)
   WRITE(MSG%LU_DEBUG,*) '-------------- ZONES_LOCAL: overlap:'
   WRITE(MSG%LU_DEBUG,'(8I12)') G%ZONES_LOCAL(G%NC+1: G%NCE2)
ELSE
   WRITE(MSG%LU_DEBUG,*) '-------------- ZONES_GLOBAL: internal:'
   WRITE(MSG%LU_DEBUG,'(8I12)') G%ZONES_GLOBAL(1:G%NC)
   WRITE(MSG%LU_DEBUG,*) '-------------- ZONES_GLOBAL: overlap:'
   WRITE(MSG%LU_DEBUG,'(8I12)') G%ZONES_GLOBAL(G%NC+1: G%NCE2)
ENDIF
WRITE(MSG%LU_DEBUG,*) '-------------- ZONE_CENTERS'
WRITE(MSG%LU_DEBUG,'(8I12)') G%ZONE_CENTERS
END SUBROUTINE SCARC_DEBUG_ZONES


! ------------------------------------------------------------------------------------------------------
!> \brief Debugging version only: Print out debug information for compactly stored matrix
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEBUG_CMATRIX(A, CNAME, CTEXT)
CHARACTER(*), INTENT(IN) :: CNAME, CTEXT
TYPE (SCARC_CMATRIX_TYPE), INTENT(INOUT) :: A      
INTEGER :: IC, ICOL
CHARACTER(40) :: CFORM

WRITE(MSG%LU_DEBUG,*)
WRITE(MSG%LU_DEBUG,*) '============ START DEBUGGING MATRIX ', CNAME, ' AT ', TRIM(CTEXT)
WRITE(MSG%LU_DEBUG,*) 'INTERNAL NAME OF MATRIX :', A%CNAME
WRITE(MSG%LU_DEBUG,*) 'REQUESTED SIZES N_ROW, N_VAL:', A%N_ROW, A%N_VAL
WRITE(MSG%LU_DEBUG,*) 'ALLOCATED SIZES N_ROW, N_VAL:', SIZE(A%ROW), SIZE(A%VAL)

WRITE(MSG%LU_DEBUG,*)
WRITE(MSG%LU_DEBUG,*) "------------->", TRIM(CNAME),'%ROW:'
WRITE(MSG%LU_DEBUG,'(8I12)') (A%ROW(IC), IC=1, A%N_ROW)
WRITE(MSG%LU_DEBUG,*) "------------->", TRIM(CNAME),'%COL:'
DO IC = 1, A%N_ROW-1
   IF (A%ROW(IC) == 0) CYCLE
   IF (A%ROW(IC+1)-A%ROW(IC) < 10) THEN
      CFORM = "(I8,A,10I12)"
   ELSE IF (A%ROW(IC+1)-A%ROW(IC) < 20) THEN
      CFORM = "(I8,A,20I12)"
   ELSE IF (A%ROW(IC+1)-A%ROW(IC)  < 30) THEN
      CFORM = "(I8,A,30I12)"
   ELSE
      CFORM = "(I8,A,40I12)"
   ENDIF
   WRITE(MSG%LU_DEBUG,CFORM) IC,':', (A%COL(ICOL), ICOL=A%ROW(IC), A%ROW(IC+1)-1)
ENDDO
IF (ALLOCATED(A%COLG)) THEN
   WRITE(MSG%LU_DEBUG,*) "------------->", TRIM(CNAME),'%COLG:'
   DO IC = 1, A%N_ROW-1
      IF (A%ROW(IC) == 0) CYCLE
      IF (A%ROW(IC+1)-A%ROW(IC) < 10) THEN
         CFORM = "(I8,A,10I12)"
      ELSE IF (A%ROW(IC+1)-A%ROW(IC) < 20) THEN
         CFORM = "(I8,A,20I12)"
      ELSE IF (A%ROW(IC+1)-A%ROW(IC) < 30) THEN
         CFORM = "(I8,A,30I12)"
      ELSE
         CFORM = "(I8,A,40I12)"
      ENDIF
      WRITE(MSG%LU_DEBUG,CFORM) IC,':', (A%COLG(ICOL), ICOL=A%ROW(IC), A%ROW(IC+1)-1)
   ENDDO
ENDIF
IF (ALLOCATED(A%VAL)) THEN
WRITE(MSG%LU_DEBUG,*) "------------->", TRIM(CNAME),'%VAL:'
DO IC = 1, A%N_ROW-1
   IF (A%ROW(IC) == 0) CYCLE
      IF (A%ROW(IC+1)-A%ROW(IC) < 10) THEN
         CFORM = "(I8,A,10E10.2)"
      ELSE IF (A%ROW(IC+1)-A%ROW(IC) < 20) THEN
         CFORM = "(I8,A,20E10.2)"
      ELSE IF (A%ROW(IC+1)-A%ROW(IC) < 30) THEN
         CFORM = "(I8,A,30E10.2)"
      ELSE
         CFORM = "(I8,A,40E10.2)"
      ENDIF
   WRITE(MSG%LU_DEBUG,*) IC,':', (A%VAL(ICOL), ICOL=A%ROW(IC), A%ROW(IC+1)-1)
   !WRITE(MSG%LU_DEBUG,CFORM) IC,':', (A%VAL(ICOL), ICOL=A%ROW(IC), A%ROW(IC+1)-1)
ENDDO
WRITE(MSG%LU_DEBUG,*) '============ END DEBUGGING MATRIX ', CNAME, ' AT ', TRIM(CTEXT)
ENDIF

END SUBROUTINE SCARC_DEBUG_CMATRIX


! ------------------------------------------------------------------------------------------------------
!> \brief Debugging version only: Print out debug information for compactly stored matrix
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEBUG_RELAX(A, CNAME, CTEXT)
CHARACTER(*), INTENT(IN) :: CNAME, CTEXT
TYPE (SCARC_CMATRIX_TYPE), INTENT(INOUT) :: A      
INTEGER :: IC, ICOL
CHARACTER(40) :: CFORM

WRITE(MSG%LU_DEBUG,*)
WRITE(MSG%LU_DEBUG,*) '============ START DEBUGGING RELAX ', CNAME, ' AT ', TRIM(CTEXT)
WRITE(MSG%LU_DEBUG,*) 'INTERNAL NAME OF MATRIX :', A%CNAME
WRITE(MSG%LU_DEBUG,*) 'REQUESTED SIZES N_ROW, N_VAL:', A%N_ROW, A%N_VAL
WRITE(MSG%LU_DEBUG,*) 'ALLOCATED SIZES N_ROW, N_VAL:', SIZE(A%ROW), SIZE(A%VAL)

WRITE(MSG%LU_DEBUG,*)
WRITE(MSG%LU_DEBUG,*) "------------->", TRIM(CNAME),'%ROW:'
WRITE(MSG%LU_DEBUG,'(8I12)') (A%ROW(IC), IC=1, A%N_ROW)
WRITE(MSG%LU_DEBUG,*) "------------->", TRIM(CNAME),'%COL:'
DO IC = 1, A%N_ROW-1
   IF (A%ROW(IC) == 0) CYCLE
   IF (A%ROW(IC+1)-A%ROW(IC) < 10) THEN
      CFORM = "(I8,A,10I12)"
   ELSE IF (A%ROW(IC+1)-A%ROW(IC) < 20) THEN
      CFORM = "(I8,A,20I12)"
   ELSE IF (A%ROW(IC+1)-A%ROW(IC)  < 30) THEN
      CFORM = "(I8,A,30I12)"
   ELSE
      CFORM = "(I8,A,40I12)"
   ENDIF
   WRITE(MSG%LU_DEBUG,CFORM) IC,':', (A%COL(ICOL), ICOL=A%ROW(IC), A%ROW(IC+1)-1)
ENDDO
IF (ALLOCATED(A%RELAX)) THEN
WRITE(MSG%LU_DEBUG,*) "------------->", TRIM(CNAME),'%VAL:'
DO IC = 1, A%N_ROW-1
   IF (A%ROW(IC) == 0) CYCLE
      IF (A%ROW(IC+1)-A%ROW(IC) < 10) THEN
         CFORM = "(I8,A,10E10.2)"
      ELSE IF (A%ROW(IC+1)-A%ROW(IC) < 20) THEN
         CFORM = "(I8,A,20E10.2)"
      ELSE IF (A%ROW(IC+1)-A%ROW(IC) < 30) THEN
         CFORM = "(I8,A,30E10.2)"
      ELSE
         CFORM = "(I8,A,40E10.2)"
      ENDIF
   WRITE(MSG%LU_DEBUG,*) IC,':', (A%RELAX(ICOL), ICOL=A%ROW(IC), A%ROW(IC+1)-1)
   !WRITE(MSG%LU_DEBUG,CFORM) IC,':', (A%RELAX(ICOL), ICOL=A%ROW(IC), A%ROW(IC+1)-1)
ENDDO
WRITE(MSG%LU_DEBUG,*) '============ END DEBUGGING MATRIX ', CNAME, ' AT ', TRIM(CTEXT)
ENDIF

END SUBROUTINE SCARC_DEBUG_RELAX


! ------------------------------------------------------------------------------------------------
!> \brief Debugging version only: Print out debug information for specified vector
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEBUG_LEVEL (NV, CVEC, NL)
USE SCARC_POINTERS, ONLY: L, G, VC, SCARC_POINT_TO_GRID, SCARC_POINT_TO_VECTOR
INTEGER, INTENT(IN) :: NV, NL
REAL (EB) :: VALUES(0:100)
INTEGER :: NM, II, JJ, KK, IC, NNX, NNY, NNZ
CHARACTER (*), INTENT(IN) :: CVEC

!IF (TYPE_SOLVER /= NSCARC_SOLVER_MAIN) RETURN
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
   VC => SCARC_POINT_TO_VECTOR (NM, NL, NV)

   NNX=MIN(10,L%NX)
   NNY=MIN(10,L%NY)
   NNZ=MIN(10,L%NZ)

   WRITE(MSG%LU_DEBUG,*) 'IS_UNSTRUCTURED =', IS_UNSTRUCTURED
   WRITE(MSG%LU_DEBUG,*) '=========================================================='
   WRITE(MSG%LU_DEBUG,2001) CVEC, NM, NL
   WRITE(MSG%LU_DEBUG,2002) G%NC, NNX, NNY, NNZ, NV, TYPE_GRID
   WRITE(MSG%LU_DEBUG,*) '=========================================================='
   !IF ((IS_AMG.OR.IS_CG_AMG.OR.HAS_COARSENING_AMG) .AND. NL > NLEVEL_MIN) THEN

   !   WRITE(MSG%LU_DEBUG, '(4E14.6)') VC

   !ELSE
   !IF (NL == NLEVEL_MIN) THEN
   DO KK = NNZ, 1, - 1
      DO JJ = NNY, 1, - 1
         DO II=1, NNX
            IF (IS_UNSTRUCTURED.AND.L%IS_SOLID(II,JJ,KK)) THEN
               VALUES(II)=0.0_EB
               !CYCLE
            ELSE
               IC=G%CELL_NUMBER(II,JJ,KK)
               IF (ABS(VC(IC))<1.0E-14_EB) THEN
                  VALUES(II)=0.0_EB
               ELSE
                  VALUES(II)=VC(IC)
               ENDIF
            ENDIF
         ENDDO
         WRITE(MSG%LU_DEBUG, MSG%CFORM3) (VALUES(II), II=1, NNX)
      ENDDO
      IF (.NOT. TWO_D) WRITE(MSG%LU_DEBUG, *) '----------------'
   ENDDO
   !ENDIF
   WRITE(MSG%LU_DEBUG, *) '---------------- Overlap ----------------'
   WRITE(MSG%LU_DEBUG, '(4E14.6)') (VC(IC), IC = G%NC+1, G%NCE)
   !ENDIF
ENDDO

!CALL SCARC_MATLAB_VECTOR(NV, CVEC, NL)

2001 FORMAT('=== ',A,' on mesh ',I8,' on level ',I8)
2002 FORMAT('=== NC = ',I6, ': NX, NY, NZ=',3I6,': NV=',I6,': GRID=', I6)
END SUBROUTINE SCARC_DEBUG_LEVEL

! ------------------------------------------------------------------------------------------------
!> \brief Debugging version only: Print out debug information for specified combination of vector and mesh
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEBUG_LEVEL_MESH (X, CVEC, NTYPE, NM, NL)
USE SCARC_POINTERS, ONLY: SCARC_POINT_TO_GRID
TYPE (SCARC_LEVEL_TYPE), POINTER :: LL=>NULL()
TYPE (SCARC_GRID_TYPE), POINTER :: GG=>NULL()
INTEGER, INTENT(IN) :: NM, NL, NTYPE
REAL(EB), INTENT(IN), DIMENSION(:) :: X
REAL (EB) :: VALUES(0:100)
INTEGER :: II, JJ, KK, IC, NNX, NNY, NNZ
CHARACTER (*), INTENT(IN) :: CVEC

LL => SCARC(NM)%LEVEL(NL)
IF (NTYPE == NSCARC_GRID_STRUCTURED) THEN
   GG => LL%STRUCTURED
ELSE IF (NTYPE == NSCARC_GRID_UNSTRUCTURED) THEN
   GG => LL%UNSTRUCTURED
ENDIF
NNX=MIN(10,LL%NX)
NNY=MIN(10,LL%NY)
NNZ=MIN(10,LL%NZ)

WRITE(MSG%LU_DEBUG,*) '=========================================================='
WRITE(MSG%LU_DEBUG,2001) CVEC, NM, NL
WRITE(MSG%LU_DEBUG,2002) GG%NC, NNX, NNY, NNZ
WRITE(MSG%LU_DEBUG,*) '=========================================================='
DO KK = NNZ, 1, - 1
   DO JJ = NNY, 1, - 1
      VALUES = 0.0_EB
      DO II=1, NNX
         IF (IS_UNSTRUCTURED.AND.LL%IS_SOLID(II,JJ,KK)) THEN
            CYCLE
         ELSE
            IC=GG%CELL_NUMBER(II,JJ,KK)
            IF (ABS(X(IC))>1.0E-14_EB) VALUES(II)=X(IC)
         ENDIF
      ENDDO
      WRITE(MSG%LU_DEBUG, MSG%CFORM3) (VALUES(II), II=1, NNX)
   ENDDO
   IF (.NOT. TWO_D) WRITE(MSG%LU_DEBUG, *) '----------------'
ENDDO
WRITE(MSG%LU_DEBUG, *) '---------------- Overlap ----------------'
WRITE(MSG%LU_DEBUG, '(4E14.6)') (X(IC), IC = GG%NC+1, GG%NCE)

2001 FORMAT('=== ',A,' on mesh ',I8,' on level ',I8)
2002 FORMAT('=== NC = ',I6, ': NX, NY, NZ=',3I6)
END SUBROUTINE SCARC_DEBUG_LEVEL_MESH


! ------------------------------------------------------------------------------------------------
!> \brief Debugging version only: Print out debug information for specified quantity
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEBUG_QUANTITY(NTYPE, NL, CQUANTITY)
USE SCARC_POINTERS, ONLY: M, L, OL, G, OG, SV, SCARC_POINT_TO_GRID, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NTYPE, NL
INTEGER :: NM, NOM, IW, I, IOR0, INBR, III, JJJ, KKK, IWG, NNX, NNY, NNZ
CHARACTER (*), INTENT(IN) :: CQUANTITY

SELECT CASE (NTYPE)

   ! ------------------------------------------------------------------------------------------------
   ! Debug FACEINFO
   ! ------------------------------------------------------------------------------------------------
   CASE (NSCARC_DEBUG_FACE)

      DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
         L => SCARC(NM)%LEVEL(NL)
         WRITE(MSG%LU_DEBUG,1000) CQUANTITY, NM, NL
         DO IOR0 = -3, 3
            IF (IOR0 == 0) CYCLE
            WRITE(MSG%LU_DEBUG,*) '========================================='
            WRITE(MSG%LU_DEBUG,*) '============= DEBUGGING FACE(',IOR0,'): FOR LEVEL ', NL
            WRITE(MSG%LU_DEBUG,*) '========================================='
            WRITE(MSG%LU_DEBUG,*) 'FACE(.)%DH: '
            WRITE(MSG%LU_DEBUG,'(12F8.2)')  L%FACE(IOR0)%DH
            WRITE(MSG%LU_DEBUG,*) 'FACE(.)%NOP:', L%FACE(IOR0)%NOP
            WRITE(MSG%LU_DEBUG,*) 'FACE(.)%NCW:', L%FACE(IOR0)%NCW
            WRITE(MSG%LU_DEBUG,*) 'FACE(.)%NX: ', L%FACE(IOR0)%NX
            WRITE(MSG%LU_DEBUG,*) 'FACE(.)%NY: ', L%FACE(IOR0)%NY
            WRITE(MSG%LU_DEBUG,*) 'FACE(.)%NZ: ', L%FACE(IOR0)%NZ
            WRITE(MSG%LU_DEBUG,*) 'FACE(.)%NW0:', L%FACE(IOR0)%NCW0
            WRITE(MSG%LU_DEBUG,*) 'FACE(.)%N_NEIGHBORS: ', L%FACE(IOR0)%N_NEIGHBORS
            WRITE(MSG%LU_DEBUG,*) 'FACE(.)%INCR_BOUNDARY: ', L%FACE(IOR0)%INCR_BOUNDARY
            WRITE(MSG%LU_DEBUG,*) 'FACE(.)%SCAL_DIRICHLET: ', L%FACE(IOR0)%SCAL_DIRICHLET
            WRITE(MSG%LU_DEBUG,*) 'FACE(.)%SCAL_NEUMANN: ', L%FACE(IOR0)%SCAL_NEUMANN
            WRITE(MSG%LU_DEBUG,*) 'FACE(.)%INCR_FACE: ', L%FACE(IOR0)%INCR_FACE
            WRITE(MSG%LU_DEBUG,*) '----------------------------------------------'
            WRITE(MSG%LU_DEBUG,*)
            IF (L%FACE(IOR0)%N_NEIGHBORS /= 0) THEN
               DO INBR=1,L%FACE(IOR0)%N_NEIGHBORS
                  NOM = L%FACE(IOR0)%NEIGHBORS(INBR)
                  OL => SCARC(NM)%OSCARC(NOM)%LEVEL(NL)
                  SELECT CASE(TYPE_GRID)
                  CASE (NSCARC_GRID_STRUCTURED)
                     OG => OL%STRUCTURED
                  CASE (NSCARC_GRID_UNSTRUCTURED)
                     OG => OL%UNSTRUCTURED
                  END SELECT
                  WRITE(MSG%LU_DEBUG,*) 'N_NEIGHBORS:', L%FACE(IOR0)%N_NEIGHBORS
                  WRITE(MSG%LU_DEBUG,*) 'NOM:', NOM
                  WRITE(MSG%LU_DEBUG,*) 'SIZE(OG%ICG_TO_IWG)=',SIZE(OG%ICG_TO_IWG)
                  WRITE(MSG%LU_DEBUG,*) 'SIZE(OG%ICG_TO_ICE)=',SIZE(OG%ICG_TO_ICE)
                  WRITE(MSG%LU_DEBUG,'(a,i8,a,2i8)') '---OG(',NOM,')%GHOST_LASTW(.):',OG%NCG
                  WRITE(MSG%LU_DEBUG,*)
                  WRITE(MSG%LU_DEBUG,'(a,i8,a)') '------OG(',NOM,')%ICG_TO_IWG:'
                  DO IW = 1, OG%NCG
                     WRITE(MSG%LU_DEBUG,'(16i8)') OG%ICG_TO_IWG(IW)
                  ENDDO
                  WRITE(MSG%LU_DEBUG,*)
                  WRITE(MSG%LU_DEBUG,'(a,i8,a)') '------OG(',NOM,')%ICG_TO_ICE:'
                  DO IW = 1, OG%NCG
                     WRITE(MSG%LU_DEBUG,'(16i8)') OG%ICG_TO_ICE(IW, 1)
                  ENDDO
                  WRITE(MSG%LU_DEBUG,*)
                  WRITE(MSG%LU_DEBUG,'(a,i8,a)') '------OG(',NOM,')%ICG_TO_ICW:'
                  DO IW = 1, OG%NCG
                     WRITE(MSG%LU_DEBUG,'(16i8)') OG%ICG_TO_ICW(IW, 1)
                  ENDDO
               ENDDO
            ENDIF
         ENDDO
      ENDDO

   ! ------------------------------------------------------------------------------------------------
   ! Debug Pressure information 
   ! ------------------------------------------------------------------------------------------------
   CASE (NSCARC_DEBUG_PRESSURE)

      DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

         CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

         NNX=MIN(8,L%NX)
         NNY=MIN(8,L%NY)
         NNZ=MIN(8,L%NZ)

         WRITE(MSG%LU_DEBUG,*) '========= PRESSURE ========= ', NM, NL, CQUANTITY
         IF (PREDICTOR) THEN
            WRITE(MSG%LU_DEBUG,*) 'RHO'
            DO KKK = NNZ+1, 0, -1
               WRITE(MSG%LU_DEBUG,'(I8,10E14.6)') KKK, (M%RHO(III, 1, KKK), III=0,NNX+1)
            ENDDO
            WRITE(MSG%LU_DEBUG,*) 'H'
            DO KKK = NNZ+1, 0, -1
               WRITE(MSG%LU_DEBUG,'(I8,10E14.6)') KKK, (M%H(III, 1, KKK), III=0,NNX+1)
            ENDDO
         ELSE
            WRITE(MSG%LU_DEBUG,*) 'RHOS'
            DO KKK = NNZ+1, 0, -1
               WRITE(MSG%LU_DEBUG,'(I8,10E14.6)') KKK, (M%RHOS(III, 1, KKK), III=0,NNX+1)
            ENDDO
            WRITE(MSG%LU_DEBUG,*) 'HS'
            DO KKK = NNZ+1, 0, -1
               WRITE(MSG%LU_DEBUG,'(I8,10E14.6)') KKK, (M%HS(III, 1, KKK), III=0,NNX+1)
            ENDDO
         ENDIF
         WRITE(MSG%LU_DEBUG,*) 'FVX'
         DO KKK = NNZ+1, 0, -1
            WRITE(MSG%LU_DEBUG,'(I8,10E14.6)') KKK, (M%FVX(III, 1, KKK), III=0,NNX+1)
         ENDDO
         WRITE(MSG%LU_DEBUG,*) 'FVY'
         DO KKK = NNZ+1, 0, -1
            WRITE(MSG%LU_DEBUG,'(I8,10E14.6)') KKK, (M%FVY(III, 1, KKK), III=0,NNX+1)
         ENDDO
         WRITE(MSG%LU_DEBUG,*) 'FVZ'
         DO KKK = NNZ+1, 0, -1
            WRITE(MSG%LU_DEBUG,'(I8,10E14.6)') KKK, (M%FVZ(III, 1, KKK), III=0,NNX+1)
         ENDDO
         WRITE(MSG%LU_DEBUG,*) 'KRES'
         DO KKK = NNZ+1, 0, -1
            WRITE(MSG%LU_DEBUG,'(I8,10E14.6)') KKK, (M%KRES(III, 1, KKK), III=0,NNX+1)
         ENDDO
         WRITE(MSG%LU_DEBUG,*) 'M%PRHS'
         DO KKK = NNZ+1, 1, -1
            WRITE(MSG%LU_DEBUG,'(I8,9E14.6)') KKK, (M%PRHS(III, 1, KKK), III=1,NNX+1)
         ENDDO
         !WRITE(MSG%LU_DEBUG,*) 'P%PRHS'
         !DO KKK = NNZ+1, 1, -1
         !WRITE(MSG%LU_DEBUG,'(I8,9E14.6)') KKK, (P%PRHS(III, 1, KKK), III=1,NNX+1)
         !ENDDO

      ENDDO

   ! ------------------------------------------------------------------------------------------------
   ! Debug stack information
   ! ------------------------------------------------------------------------------------------------
   CASE (NSCARC_DEBUG_STACK)

      WRITE(MSG%LU_DEBUG,*) 'N_STACK_TOTAL=',N_STACK_TOTAL
      DO I = 1, N_STACK_TOTAL
         SV  => STACK(I)%SOLVER
         WRITE(MSG%LU_DEBUG,*) '===================== STACK ', I,' ======================'
         WRITE(MSG%LU_DEBUG,*) '-------------------- SOLVER:'
         WRITE(MSG%LU_DEBUG,*) 'NAME=',SV%CNAME
         WRITE(MSG%LU_DEBUG,*) '-- SETTING:'
         WRITE(MSG%LU_DEBUG,*) 'EPS   = ',SV%EPS
         WRITE(MSG%LU_DEBUG,*) 'RES   = ',SV%RES
         WRITE(MSG%LU_DEBUG,*) 'RESIN = ',SV%RESIN
         WRITE(MSG%LU_DEBUG,*) 'OMEGA = ',SV%OMEGA
         WRITE(MSG%LU_DEBUG,*) 'ITE   = ',SV%ITE
         WRITE(MSG%LU_DEBUG,*) 'NIT   = ',SV%NIT
         WRITE(MSG%LU_DEBUG,*) '-- TYPES:'
         WRITE(MSG%LU_DEBUG,*) 'TYPE_PARENT   = ',SV%TYPE_PARENT
         WRITE(MSG%LU_DEBUG,*) 'TYPE_SOLVER   = ',SV%TYPE_SOLVER
         WRITE(MSG%LU_DEBUG,*) 'TYPE_STAGE    = ',SV%TYPE_STAGE
         WRITE(MSG%LU_DEBUG,*) 'TYPE_SCOPE(0) = ',SV%TYPE_SCOPE(0)
         WRITE(MSG%LU_DEBUG,*) 'TYPE_PRECON   = ',SV%TYPE_RELAX
         WRITE(MSG%LU_DEBUG,*) 'TYPE_ACCURACY = ',SV%TYPE_ACCURACY
         WRITE(MSG%LU_DEBUG,*) 'TYPE_INTERPOL = ',SV%TYPE_INTERPOL
         WRITE(MSG%LU_DEBUG,*) 'TYPE_CYCLING  = ',SV%TYPE_CYCLING
         WRITE(MSG%LU_DEBUG,*) 'TYPE_TWOLEVEL = ',SV%TYPE_TWOLEVEL
         WRITE(MSG%LU_DEBUG,*) '-- POINTERS:'
         WRITE(MSG%LU_DEBUG,*) 'X   = ',SV%X
         WRITE(MSG%LU_DEBUG,*) 'B   = ',SV%B
         WRITE(MSG%LU_DEBUG,*) 'D   = ',SV%D
         WRITE(MSG%LU_DEBUG,*) 'E   = ',SV%E
         WRITE(MSG%LU_DEBUG,*) 'R   = ',SV%R
         WRITE(MSG%LU_DEBUG,*) 'V   = ',SV%V
         WRITE(MSG%LU_DEBUG,*) 'Y   = ',SV%Y
         WRITE(MSG%LU_DEBUG,*) 'Z   = ',SV%Z
      ENDDO

   ! ------------------------------------------------------------------------------------------------
   ! Debug WALLINFO
   ! ------------------------------------------------------------------------------------------------
   CASE (NSCARC_DEBUG_WALL)

      DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
         CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
         WRITE(MSG%LU_DEBUG,1000) CQUANTITY, NM, NL
         WRITE(MSG%LU_DEBUG,*) 'SIZE(G%ICE_TO_IWG)=',SIZE(G%ICE_TO_IWG)
         WRITE(MSG%LU_DEBUG,*) 'NM  =',NM
         WRITE(MSG%LU_DEBUG,*) 'NL  =',NL
         WRITE(MSG%LU_DEBUG,*) 'NC  =',G%NC
         WRITE(MSG%LU_DEBUG,*) 'NCE =',G%NCE
         WRITE(MSG%LU_DEBUG,*) 'N_WALL_CELLS  =',L%N_WALL_CELLS
         WRITE(MSG%LU_DEBUG,*) 'N_WALL_CELLS_EXT  =',L%N_WALL_CELLS_EXT
         WRITE(MSG%LU_DEBUG,*) 'N_WALL_CELLS_INT  =',L%N_WALL_CELLS_INT
         WRITE(MSG%LU_DEBUG,*)
         WRITE(MSG%LU_DEBUG,'(a,I8,a)') '------G%ICE_TO_IWG:'
         WRITE(MSG%LU_DEBUG,'(8I8)') (G%ICE_TO_IWG(IW), IW = G%NC+1, G%NCE)
         WRITE(MSG%LU_DEBUG,*)
         IF (N_DIRIC_GLOBAL(NLEVEL_MIN) == 0) THEN
            WRITE(MSG%LU_DEBUG,'(a,I8,a)') '------G%ICE_TO_ICN:'
            WRITE(MSG%LU_DEBUG,'(8I8)') (G%ICE_TO_ICN(IW), IW = G%NC+1, G%NCE)
         ENDIF
         WRITE(MSG%LU_DEBUG,*)
         IF (NL == 1) THEN
            WRITE(MSG%LU_DEBUG,*) '============= WALL(.)%IXG:', NM
            WRITE(MSG%LU_DEBUG,'(16I6)') (G%WALL(IW)%IXG, IW=1,L%N_WALL_CELLS)
            WRITE(MSG%LU_DEBUG,*) '============= WALL(.)%IYG:', NM
            WRITE(MSG%LU_DEBUG,'(16I6)') (G%WALL(IW)%IYG, IW=1,L%N_WALL_CELLS)
            WRITE(MSG%LU_DEBUG,*) '============= WALL(.)%IZG:', NM
            WRITE(MSG%LU_DEBUG,'(16I6)') (G%WALL(IW)%IZG, IW=1,L%N_WALL_CELLS)
            WRITE(MSG%LU_DEBUG,*)
            WRITE(MSG%LU_DEBUG,*) '============= WALL(.)%IXW:', NM
            WRITE(MSG%LU_DEBUG,'(16I6)') (G%WALL(IW)%IXW, IW=1,L%N_WALL_CELLS)
            WRITE(MSG%LU_DEBUG,*) '============= WALL(.)%IYW:', NM
            WRITE(MSG%LU_DEBUG,'(16I6)') (G%WALL(IW)%IYW, IW=1,L%N_WALL_CELLS)
            WRITE(MSG%LU_DEBUG,*) '============= WALL(.)%IZW:', NM
            WRITE(MSG%LU_DEBUG,'(16I6)') (G%WALL(IW)%IZW, IW=1,L%N_WALL_CELLS)
            WRITE(MSG%LU_DEBUG,*)
            WRITE(MSG%LU_DEBUG,*) '============= WALL(.)%IXN(1):', NM
            WRITE(MSG%LU_DEBUG,'(16I6)') (G%WALL(IW)%IXN(1), IW=1,L%N_WALL_CELLS)
            WRITE(MSG%LU_DEBUG,*) '============= WALL(.)%IYN(1):', NM
            WRITE(MSG%LU_DEBUG,'(16I6)') (G%WALL(IW)%IYN(1), IW=1,L%N_WALL_CELLS)
            WRITE(MSG%LU_DEBUG,*) '============= WALL(.)%IZN(1):', NM
            WRITE(MSG%LU_DEBUG,'(16I6)') (G%WALL(IW)%IZN(1), IW=1,L%N_WALL_CELLS)
            WRITE(MSG%LU_DEBUG,*)
            WRITE(MSG%LU_DEBUG,*) '============= WALL(.)%IXN(2):', NM
            WRITE(MSG%LU_DEBUG,'(16I6)') (G%WALL(IW)%IXN(2), IW=1,L%N_WALL_CELLS)
            WRITE(MSG%LU_DEBUG,*) '============= WALL(.)%IYN(2):', NM
            WRITE(MSG%LU_DEBUG,'(16I6)') (G%WALL(IW)%IYN(2), IW=1,L%N_WALL_CELLS)
            WRITE(MSG%LU_DEBUG,*) '============= WALL(.)%IZN(2):', NM
            WRITE(MSG%LU_DEBUG,'(16I6)') (G%WALL(IW)%IZN(2), IW=1,L%N_WALL_CELLS)
         ENDIF
         WRITE(MSG%LU_DEBUG,*)
         WRITE(MSG%LU_DEBUG,*) '============= WALL(.)%BTYPE:', NM
         WRITE(MSG%LU_DEBUG,'(16I6)') (G%WALL(IW)%BTYPE, IW=1,L%N_WALL_CELLS)
         WRITE(MSG%LU_DEBUG,*)
         WRITE(MSG%LU_DEBUG,*) '============= WALL(.)%IOR:', NM
         WRITE(MSG%LU_DEBUG,'(16I6)') (G%WALL(IW)%IOR, IW=1,L%N_WALL_CELLS)
         WRITE(MSG%LU_DEBUG,*)
         WRITE(MSG%LU_DEBUG,*) '============= WALL(.)%NOM:', NM
         WRITE(MSG%LU_DEBUG,'(16I6)') (G%WALL(IW)%NOM, IW=1,L%N_WALL_CELLS)
         WRITE(MSG%LU_DEBUG,*)
         WRITE(MSG%LU_DEBUG,*)
         WRITE(MSG%LU_DEBUG,*) '============= WALL(.)%ICW:', NM
         DO IW=1,L%N_WALL_CELLS
            WRITE(MSG%LU_DEBUG,'(a,I8, a,I8)') 'IW=',IW,':',G%WALL(IW)%ICW
         ENDDO
         WRITE(MSG%LU_DEBUG,*)
         WRITE(MSG%LU_DEBUG,*) '============= WALL(.)%ICE:', NM
         DO IW=1,L%N_WALL_CELLS
            IF (G%WALL(IW)%NOM /=0) &
               WRITE(MSG%LU_DEBUG,'(a,I8, a,I8)') 'IW=',IW,':',G%WALL(IW)%ICE
         ENDDO
         WRITE(MSG%LU_DEBUG,*)
         WRITE(MSG%LU_DEBUG,*)
         WRITE(MSG%LU_DEBUG,*) '============= WALL(.)%ICG:', NM
         DO IW=1,L%N_WALL_CELLS
            IF (G%WALL(IW)%NOM /=0) &
               WRITE(MSG%LU_DEBUG,'(a,I8, a,I8)') 'IW=',IW,':',G%WALL(IW)%ICG
         ENDDO
         WRITE(MSG%LU_DEBUG,*) '====================================================='
         WRITE(MSG%LU_DEBUG,*) ' Plotting out M%WALL-structure'
         WRITE(MSG%LU_DEBUG,*) '====================================================='
         WRITE(MSG%LU_DEBUG,*) ' M%WALL(.)%WALL_INDEX'
         WRITE(MSG%LU_DEBUG,'(16I6)') (M%WALL(IW)%WALL_INDEX, IW=1,L%N_WALL_CELLS)
         WRITE(MSG%LU_DEBUG,*) ' M%WALL(.)%SURF_INDEX'
         WRITE(MSG%LU_DEBUG,'(16I6)') (M%WALL(IW)%SURF_INDEX, IW=1,L%N_WALL_CELLS)
         WRITE(MSG%LU_DEBUG,*) ' M%WALL(.)%BACK_INDEX'
         WRITE(MSG%LU_DEBUG,'(16I6)') (M%WALL(IW)%BACK_INDEX, IW=1,L%N_WALL_CELLS)
         WRITE(MSG%LU_DEBUG,*) ' M%WALL(.)%BOUNDARY_TYPE'
         WRITE(MSG%LU_DEBUG,'(16I6)') (M%WALL(IW)%BOUNDARY_TYPE, IW=1,L%N_WALL_CELLS)
         WRITE(MSG%LU_DEBUG,*) ' M%WALL(.)%OBST_INDEX'
         WRITE(MSG%LU_DEBUG,'(16I6)') (M%WALL(IW)%OBST_INDEX, IW=1,L%N_WALL_CELLS)
         WRITE(MSG%LU_DEBUG,*) ' M%WALL(.)%PRESSURE_BC_INDEX'
         WRITE(MSG%LU_DEBUG,'(16I6)') (M%WALL(IW)%PRESSURE_BC_INDEX, IW=1,L%N_WALL_CELLS)
         WRITE(MSG%LU_DEBUG,*) ' M%WALL(.)%ONE_D%PRESSURE_ZONE'
         WRITE(MSG%LU_DEBUG,'(16I6)') (M%WALL(IW)%ONE_D%PRESSURE_ZONE, IW=1,L%N_WALL_CELLS)
         WRITE(MSG%LU_DEBUG,*) ' M%WALL(.)%VENT_INDEX'
         WRITE(MSG%LU_DEBUG,'(16I6)') (M%WALL(IW)%VENT_INDEX, IW=1,L%N_WALL_CELLS)
         WRITE(MSG%LU_DEBUG,*) ' M%WALL(.)%NOM'
         WRITE(MSG%LU_DEBUG,'(16I6)') (M%EXTERNAL_WALL(IW)%NOM, IW=1,L%N_WALL_CELLS_EXT)
         WRITE(MSG%LU_DEBUG,*) ' M%WALL(.)%ONE_D%II'
         WRITE(MSG%LU_DEBUG,'(16I6)') (M%WALL(IW)%ONE_D%II, IW=1,L%N_WALL_CELLS)
         WRITE(MSG%LU_DEBUG,*) ' M%WALL(.)%ONE_D%JJ'
         WRITE(MSG%LU_DEBUG,'(16I6)') (M%WALL(IW)%ONE_D%JJ, IW=1,L%N_WALL_CELLS)
         WRITE(MSG%LU_DEBUG,*) ' M%WALL(.)%ONE_D%KK'
         WRITE(MSG%LU_DEBUG,'(16I6)') (M%WALL(IW)%ONE_D%KK, IW=1,L%N_WALL_CELLS)
         WRITE(MSG%LU_DEBUG,*) ' M%WALL(.)%ONE_D%IIG'
         WRITE(MSG%LU_DEBUG,'(16I6)') (M%WALL(IW)%ONE_D%IIG, IW=1,L%N_WALL_CELLS)
         WRITE(MSG%LU_DEBUG,*) ' M%WALL(.)%ONE_D%JJG'
         WRITE(MSG%LU_DEBUG,'(16I6)') (M%WALL(IW)%ONE_D%JJG, IW=1,L%N_WALL_CELLS)
         WRITE(MSG%LU_DEBUG,*) ' M%WALL(.)%ONE_D%KKG'
         WRITE(MSG%LU_DEBUG,'(16I6)') (M%WALL(IW)%ONE_D%KKG, IW=1,L%N_WALL_CELLS)
         WRITE(MSG%LU_DEBUG,*) ' M%WALL(.)%ONE_D%N_LAYER_CELLS'
         WRITE(MSG%LU_DEBUG,'(16I6)') (M%WALL(IW)%ONE_D%IOR, IW=1,L%N_WALL_CELLS)
         WRITE(MSG%LU_DEBUG,*) ' M%WALL(.)%ONE_D%IOR'
         !WRITE(MSG%LU_DEBUG,'(16I6)') (M%WALL(IW)%ONE_D%N_LAYER_CELLS, IW=1,L%N_WALL_CELLS)

      ENDDO
      !ENDIF

   ! ------------------------------------------------------------------------------------------------
   ! Debug complete grid information
   ! ------------------------------------------------------------------------------------------------
   CASE (NSCARC_DEBUG_GRID)
      DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
         CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
         WRITE(MSG%LU_DEBUG,*) 'M%N_OBST=',M%N_OBST
         WRITE(MSG%LU_DEBUG,*) 'M%OBST ... I1, I2, J1, J2, K1, K2'
         DO IWG = 1, M%N_OBST
            WRITE(MSG%LU_DEBUG,'(6I8)') M%OBSTRUCTION(IWG)%I1,M%OBSTRUCTION(IWG)%I2,&
               M%OBSTRUCTION(IWG)%J1,M%OBSTRUCTION(IWG)%J2,&
               M%OBSTRUCTION(IWG)%K1,M%OBSTRUCTION(IWG)%K2
         ENDDO
         WRITE(MSG%LU_DEBUG,*) 'M%N_OBST=',M%N_OBST
         WRITE(MSG%LU_DEBUG,*) 'M%N_WALL_CELLS=',M%N_WALL_CELLS
         WRITE(MSG%LU_DEBUG,*) 'M%CELL_INDEX:'
         DO JJJ = L%NY+1,0,-1
            WRITE(MSG%LU_DEBUG,*) ' ------------- JJJ = ', JJJ
            DO KKK = L%NZ+1,0,-1
               WRITE(MSG%LU_DEBUG,*) (M%CELL_INDEX(III,JJJ,KKK), III=0,L%NX+1)
            ENDDO
         ENDDO
         WRITE(MSG%LU_DEBUG,*) 'M%WALL(.)%BOUNDARY_TYPE:'
         WRITE(MSG%LU_DEBUG,'(16i6)') (M%WALL(IWG)%BOUNDARY_TYPE, IWG=1, L%N_WALL_CELLS)

         WRITE(MSG%LU_DEBUG,*) 'CELL_COUNT:', CELL_COUNT(NM)

         WRITE(MSG%LU_DEBUG,*) 'M%WALL_INDEX:'
         WRITE(MSG%LU_DEBUG,'(i8,a,6i8)') ( IWG, ' : ', &
            M%WALL_INDEX(IWG, 1),  &
            M%WALL_INDEX(IWG,-1),  &
            M%WALL_INDEX(IWG, 2),  &
            M%WALL_INDEX(IWG,-2),  &
            M%WALL_INDEX(IWG, 3),  &
            M%WALL_INDEX(IWG,-3),  &
            IWG=1, CELL_COUNT(NM))

         WRITE(MSG%LU_DEBUG,*) 'M%WALL(.)%ONE_D% IOR,II,JJ,KK, BOUNDARY_TYPE, BTYPE, PRESSURE_BC_INDEX:'
         DO IWG = 1, L%N_WALL_CELLS
            WRITE(MSG%LU_DEBUG,'(9I8)') &
               IWG,M%WALL(IWG)%ONE_D%IOR,M%WALL(IWG)%ONE_D%II,M%WALL(IWG)%ONE_D%JJ,M%WALL(IWG)%ONE_D%KK,&
               M%WALL(IWG)%BOUNDARY_TYPE, G%WALL(IWG)%BTYPE, M%WALL(IWG)%PRESSURE_BC_INDEX
         ENDDO

         WRITE(MSG%LU_DEBUG,*) 'GRIG%CELL_NUMBER(...)'
         DO JJJ=L%NY+1,0,-1
            WRITE(MSG%LU_DEBUG,*) ' ------------- JJJ = ', JJJ
            DO KKK = L%NZ+1,0,-1
               WRITE(MSG%LU_DEBUG,*) (G%CELL_NUMBER(III,JJJ,KKK),III=0,L%NX+1)
            ENDDO
         ENDDO
         WRITE(MSG%LU_DEBUG,*) 'GRIL%IS_SOLID(...)'
         DO JJJ=L%NY+1,0,-1
            WRITE(MSG%LU_DEBUG,*) ' ------------- JJJ = ', JJJ
            DO KKK = L%NZ+1,0,-1
               WRITE(MSG%LU_DEBUG,*) (L%IS_SOLID(III,JJJ,KKK),III=0,L%NX+1)
            ENDDO
         ENDDO
         WRITE(MSG%LU_DEBUG,*) 'M%CELL_INDEX:'
         DO JJJ=L%NY+1,0,-1
            WRITE(MSG%LU_DEBUG,*) ' ------------- JJJ = ', JJJ
            DO KKK = L%NZ+1,0,-1
               WRITE(MSG%LU_DEBUG,*) (M%CELL_INDEX(III,JJJ,KKK),III=0,L%NX+1)
            ENDDO
         ENDDO
      ENDDO

END SELECT

1000 FORMAT('======================================================================================',/, &
   '=== ', A30,' for mesh ',i3,' on level ', i3, /, &
   '======================================================================================')

END SUBROUTINE SCARC_DEBUG_QUANTITY


! ------------------------------------------------------------------------------------------------
!> \brief Debugging version only: Print out vector information on specified level for MATLAB
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_MATLAB_VECTOR (NV, CVEC, NL)
USE SCARC_POINTERS, ONLY: G, VC, SCARC_POINT_TO_GRID, SCARC_POINT_TO_VECTOR
INTEGER, INTENT(IN) :: NV, NL
INTEGER :: NM 
CHARACTER (*), INTENT(IN) :: CVEC
INTEGER :: JC, MVEC
CHARACTER(60) :: CNAME, CFORM

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

   VC => SCARC_POINT_TO_VECTOR (NM, NL, NV)
   WRITE (CNAME, '(A,A1,A,i2.2,A,i2.2,A)') 'matlab/',CVEC,'_mesh',NM,'_level',NL,'_vec.txt'
   WRITE (CFORM, '(I3, A)' ) G%NC-1, "(F7.2,;),F7.2"
   MVEC=GET_FILE_NUMBER()

   OPEN(MVEC,FILE=CNAME)
   WRITE(MVEC, *) CVEC, ' = ['
   WRITE(MVEC,'(8F12.2)') (VC(JC),JC=1,G%NC)
   WRITE(MVEC, *) ' ]'
   CLOSE(MVEC)

ENDDO

END SUBROUTINE SCARC_MATLAB_VECTOR


! ------------------------------------------------------------------------------------------------
!> \brief Debugging version only: Print out matrix information on specified level for MATLAB
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_MATLAB_MATRIX(VAL, ROW, COL, NC1, NC2, NM, NL, CNAME)
REAL(EB), DIMENSION(:), INTENT(IN) :: VAL
INTEGER, DIMENSION(:), INTENT(IN) :: ROW
INTEGER, DIMENSION(:), INTENT(IN) :: COL
INTEGER, INTENT(IN) :: NM, NL, NC1, NC2
CHARACTER(*), INTENT(IN) :: CNAME
INTEGER :: IC, JC, ICOL, MMATRIX
CHARACTER(60) :: CFILE, CFORM
REAL(EB) :: MATRIX_LINE(1000)

WRITE (CFILE, '(A,A,A,i2.2,A,i2.2,A)') 'matlab/',TRIM(CNAME),'_mesh',NM,'_level',NL,'_mat.txt'
!WRITE (CFORM, '(A,I3, 2A)' ) "(", NC2-1, "(F9.3,','),F9.3,';')"
!WRITE (CFORM, '(A,I3, 2A)' ) "(", NC2-1, "(F9.3,' '),F9.3,' ')"
WRITE (CFORM, '(A,I3, 2A)' ) "(", NC2-1, "(F9.3,' '),F9.3,' ')"
MMATRIX=GET_FILE_NUMBER()
OPEN(MMATRIX,FILE=CFILE)
!WRITE(MMATRIX, *) CNAME, ' = ['
DO IC = 1, NC1
   MATRIX_LINE=0.0_EB
   DO JC = 1, NC2
      DO  ICOL= ROW(IC), ROW(IC+1)-1
         !IF (COL(ICOL)==JC) MATRIX_LINE(JC)=VAL(ICOL)/25.0_EB
         IF (COL(ICOL)==JC) MATRIX_LINE(JC)=VAL(ICOL)
      ENDDO
   ENDDO
   WRITE(MMATRIX, CFORM) (MATRIX_LINE(JC),JC=1,NC2)
ENDDO
!WRITE(MMATRIX, *) ' ]'
CLOSE(MMATRIX)

END SUBROUTINE SCARC_MATLAB_MATRIX


! ------------------------------------------------------------------------------------------------
!> \brief Debugging version only: Print out matrix information on specified level for PYTHON
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_PYTHON_MATRIX(NL, CNAME)
USE SCARC_POINTERS, ONLY: G, A, SCARC_POINT_TO_GRID, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NL
CHARACTER(*), INTENT(IN) :: CNAME
INTEGER :: NM, IC, JC, ICOL, MVAL, MCOL, MROW, I, J, NLEN
CHARACTER(60) :: CVAL, CROW, CCOL
INTEGER :: STENCIL(7) = 99999999, COLUMNS(7) = 99999999

if (NL /= NLEVEL_MIN) RETURN

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
   A => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_POISSON)

   WRITE (CVAL, '(5A,i2.2,A,i2.2,A)') 'python/',TRIM(CHID),'_',TRIM(CNAME),'_m',NM,'_l',NL,'.val'
   WRITE (CCOL, '(5A,i2.2,A,i2.2,A)') 'python/',TRIM(CHID),'_',TRIM(CNAME),'_m',NM,'_l',NL,'.col'
   WRITE (CROW, '(5A,i2.2,A,i2.2,A)') 'python/',TRIM(CHID),'_',TRIM(CNAME),'_m',NM,'_l',NL,'.row'

   MVAL=GET_FILE_NUMBER()
   MCOL=GET_FILE_NUMBER()
   MROW=GET_FILE_NUMBER()

   OPEN(MVAL,FILE=CVAL)
   OPEN(MCOL,FILE=CCOL)
   OPEN(MROW,FILE=CROW)
   
   !WRITE(MVAL, *) '['
   !WRITE(MCOL, *) '['
   !WRITE(MROW, *) '['
   
   DO IC = 1, G%NC
      I = 1
      COLUMNS = 0
      STENCIL = NSCARC_HUGE_INT
      DO ICOL= A%ROW(IC), A%ROW(IC+1)-1
         COLUMNS(I) = ICOL
         STENCIL(I) = A%COL(ICOL)
         I = I + 1
      ENDDO
      NLEN = I - 1

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'IC, NLEN, COLUMNS1, STENCIL1:', IC, NLEN, COLUMNS(1:NLEN), STENCIL(1:NLEN)
#endif

      DO I = NLEN, 2, -1
         DO J = 1, I-1
            IF (STENCIL(J) > STENCIL(J+1)) THEN
               JC = STENCIL(J+1)
               STENCIL(J+1) = STENCIL(J)
               STENCIL(J) = JC
               JC = COLUMNS(J+1)
               COLUMNS(J+1) = COLUMNS(J)
               COLUMNS(J) = JC
            ENDIF
         ENDDO
      ENDDO

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) '          COLUMNS2, STENCIL2:', IC, NLEN, COLUMNS(1:NLEN), STENCIL(1:NLEN)
#endif

      DO I = 1, NLEN
         ICOL = COLUMNS(I)
         WRITE(MCOL,1001, ADVANCE="NO") A%COL(ICOL)-1
         WRITE(MVAL,1002, ADVANCE="NO") A%VAL(ICOL)
      ENDDO
      WRITE(MROW,1001, ADVANCE="NO") A%ROW(IC)-1

   ENDDO
   WRITE(MROW,1001, ADVANCE="NO") A%ROW(IC)-1

   !WRITE(MVAL, *) ' ]'
   !WRITE(MCOL, *) ' ]'
   !WRITE(MROW, *) ' ]'

   CLOSE(MVAL)
   CLOSE(MCOL)
   CLOSE(MROW)

ENDDO MESHES_LOOP

1001 FORMAT(I8,',')
1002 FORMAT(E10.2,',')
END SUBROUTINE SCARC_PYTHON_MATRIX


! ------------------------------------------------------------------------------------------------
!> \brief Debugging version only: Print out aggregation zones information on specified level for PYTHON
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_PYTHON_ZONES(NM, NL, CNAME)
USE SCARC_POINTERS, ONLY: G, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NM, NL
CHARACTER(*), INTENT(IN) :: CNAME
INTEGER :: IC, MAGG
CHARACTER(60) :: CAGG

CALL SCARC_POINT_TO_GRID(NM, NL)

WRITE (CAGG, '(5A,i2.2,A,i2.2,A)') 'python/',TRIM(CHID),'_',TRIM(CNAME),'_m',NM,'_l',NL,'.val'
MAGG=GET_FILE_NUMBER()
OPEN(MAGG,FILE=CAGG)
!WRITE(MAGG, *) '['
DO IC = 1, G%N_FINE-1
   WRITE(MAGG,1001, ADVANCE="NO") G%ZONES_GLOBAL(IC)
ENDDO
WRITE(MAGG,1002, ADVANCE="NO") G%ZONES_GLOBAL(G%N_FINE)
!WRITE(MAGG, *) ' ]'
CLOSE(MAGG)

1001 FORMAT(I8,',')
1002 FORMAT(I8)
END SUBROUTINE SCARC_PYTHON_ZONES

! ================================================================================================
! End  WITH_SCARC_DEBUG  - Part
! ================================================================================================
#endif

END MODULE SCARC_MESSAGES


