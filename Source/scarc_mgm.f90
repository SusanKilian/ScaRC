!//////////////////////////////////////////////////////////////////////////////////////////////////////
!
! MODULE SCARC_MGM
! 
!> \brief Setup environment for call of McKeeney-Greengard-Mayo method (still experimental )
!
!//////////////////////////////////////////////////////////////////////////////////////////////////////
MODULE SCARC_MGM

USE GLOBAL_CONSTANTS
USE PRECISION_PARAMETERS
USE MESH_VARIABLES
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
USE COMP_FUNCTIONS, ONLY: CURRENT_TIME, GET_FILE_NUMBER, SHUTDOWN
USE MPI
USE SCARC_CONSTANTS
USE SCARC_TYPES
USE SCARC_VARIABLES
USE SCARC_UTILITIES, ONLY: IS_VALID_DIRECTION, GET_PERM
USE SCARC_MPI
USE SCARC_STORAGE
USE SCARC_CONVERGENCE
USE SCARC_VECTORS
USE SCARC_MATRICES

IMPLICIT NONE

CONTAINS


! ----------------------------------------------------------------------------------------------------
!> \brief Allocate velocity vectors for the setting of internal boundary conditions in McKeeney-Greengard-Mayo method
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MGM(NLMIN, NLMAX)
USE SCARC_POINTERS, ONLY: L, G, MGM, GWC, LM, UM, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NLMIN, NLMAX
INTEGER :: NM, NL, IC, IW, IOR0, I, J, K, TYPE_SCOPE_SAVE
INTEGER, ALLOCATABLE, DIMENSION(:,:) :: CNT
REAL(EB) :: SX, SY, SZ

CROUTINE = 'SCARC_SETUP_MGM'

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   DO NL = NLMIN, NLMAX

      L => SCARC(NM)%LEVEL(NL)
      MGM => L%MGM
      IS_MGM = .TRUE.

      ! Store number of cells and external/internal boundary cells for simpler use in MGM method

      G => L%STRUCTURED
      MGM%NCS = G%NC
      
      G => L%UNSTRUCTURED
      MGM%NCU = G%NC

      MGM%NWE = L%N_WALL_CELLS_EXT
      MGM%NWI = L%N_WALL_CELLS_INT
      !MGM%NW1 = 1
      MGM%NW1 = L%N_WALL_CELLS_EXT + 1
      MGM%NW2 = L%N_WALL_CELLS_EXT + L%N_WALL_CELLS_INT

      ! Allocate workspace for the storage of the different pressure parts within the MGM methods

      CALL SCARC_ALLOCATE_REAL3(MGM%H1, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%H1', CROUTINE)
      CALL SCARC_ALLOCATE_REAL3(MGM%H2, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%H2', CROUTINE)
      CALL SCARC_ALLOCATE_REAL3(MGM%H3, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%H3', CROUTINE)

      IF (TYPE_MGM_BC == NSCARC_MGM_BC_EXPOL) &
         CALL SCARC_ALLOCATE_REAL3(MGM%H4, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%H4', CROUTINE)

      CALL SCARC_ALLOCATE_REAL3(MGM%H5, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%H5', CROUTINE)
      CALL SCARC_ALLOCATE_REAL3(MGM%H6, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%H6', CROUTINE)
      CALL SCARC_ALLOCATE_REAL3(MGM%H7, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%H7', CROUTINE)

      CALL SCARC_ALLOCATE_REAL3(MGM%HS, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%HS', CROUTINE)
      CALL SCARC_ALLOCATE_REAL3(MGM%HU, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%HU', CROUTINE)
      CALL SCARC_ALLOCATE_REAL3(MGM%HD, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%HD', CROUTINE)

      CALL SCARC_ALLOCATE_REAL3(MGM%UU, 0, L%NX, 0, L%NY, 0, L%NZ, NSCARC_INIT_ZERO, 'MGM%UU', CROUTINE)
      CALL SCARC_ALLOCATE_REAL3(MGM%VV, 0, L%NX, 0, L%NY, 0, L%NZ, NSCARC_INIT_ZERO, 'MGM%VV', CROUTINE)
      CALL SCARC_ALLOCATE_REAL3(MGM%WW, 0, L%NX, 0, L%NY, 0, L%NZ, NSCARC_INIT_ZERO, 'MGM%WW', CROUTINE)

      CALL SCARC_ALLOCATE_REAL3(MGM%U1, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%U1', CROUTINE)
      CALL SCARC_ALLOCATE_REAL3(MGM%V1, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%V1', CROUTINE)
      CALL SCARC_ALLOCATE_REAL3(MGM%W1, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%W1', CROUTINE)

      CALL SCARC_ALLOCATE_REAL3(MGM%U2, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%U2', CROUTINE)
      CALL SCARC_ALLOCATE_REAL3(MGM%V2, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%V2', CROUTINE)
      CALL SCARC_ALLOCATE_REAL3(MGM%W2, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%W2', CROUTINE)

      CALL SCARC_ALLOCATE_REAL3(MGM%U3, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%U3', CROUTINE)
      CALL SCARC_ALLOCATE_REAL3(MGM%V3, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%V3', CROUTINE)
      CALL SCARC_ALLOCATE_REAL3(MGM%W3, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%W3', CROUTINE)

      CALL SCARC_ALLOCATE_REAL1(MGM%OH1, 1, MGM%NWE, NSCARC_INIT_ZERO, 'MGM%OH1', CROUTINE)
      CALL SCARC_ALLOCATE_REAL1(MGM%OH2, 1, MGM%NWE, NSCARC_INIT_ZERO, 'MGM%OH2', CROUTINE)
      CALL SCARC_ALLOCATE_REAL1(MGM%OH3, 1, MGM%NWE, NSCARC_INIT_ZERO, 'MGM%OH3', CROUTINE)

      CALL SCARC_ALLOCATE_REAL1(MGM%OU3, 1, MGM%NWE, NSCARC_INIT_ZERO, 'MGM%OU3', CROUTINE)
      CALL SCARC_ALLOCATE_REAL1(MGM%OV3, 1, MGM%NWE, NSCARC_INIT_ZERO, 'MGM%OV3', CROUTINE)
      CALL SCARC_ALLOCATE_REAL1(MGM%OW3, 1, MGM%NWE, NSCARC_INIT_ZERO, 'MGM%OW3', CROUTINE)

      CALL SCARC_ALLOCATE_REAL1(MGM%BC , 1, MGM%NWE, NSCARC_INIT_ZERO, 'MGM%BC ', CROUTINE)
      CALL SCARC_ALLOCATE_REAL1(MGM%WEIGHT, 1, MGM%NWE, NSCARC_INIT_ZERO, 'MGM%WEIGHT', CROUTINE)

      CALL SCARC_ALLOCATE_INT2(CNT, 1, G%NC, -3, 3, NSCARC_INIT_NONE, 'CNT', CROUTINE)

      CNT = 2.0_EB ;  CNT(:,0) = 0.0_EB

      CALL SCARC_ALLOCATE_INT2(MGM%BTYPE, 1, MGM%NWE, -3, 3, NSCARC_INIT_NONE, 'MGM%BTYPE', CROUTINE)

      DO IW = 1, MGM%NWE

         GWC => G%WALL(IW)

         I = GWC%IXW
         J = GWC%IYW
         K = GWC%IZW
      
         IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE
     
         IOR0 = GWC%IOR
         IC = G%CELL_NUMBER(I,J,K)
       
         ! Counter for the main diagonal entries to be considered
         IF (GWC%BTYPE == DIRICHLET) THEN
            CNT(IC, IOR0) = CNT(IC, IOR0) + 1 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A, i4,A,i4,A,7I4)') 'DIRICHLET: CNT(',IC,',',IOR0,')=', CNT(IC,IOR0)
#endif
         ELSE IF (GWC%BTYPE == NEUMANN) THEN
            CNT(IC, IOR0) = CNT(IC, IOR0) - 1 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A, i4,A,i4,A,7I4)') 'NEUMANN  : CNT(',IC,',',IOR0,')=', CNT(IC,IOR0)
#endif
         ENDIF

      ENDDO

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '============================================='
DO IC = 1, G%NC
WRITE(MSG%LU_DEBUG,'(A, i4,A,7I4)') 'CNT(',IC,',-3:3)=', (CNT(IC,I), I=-3,3)
ENDDO
#endif
      DO IW = 1, MGM%NWE

         GWC => G%WALL(IW)
         IF (GWC%BTYPE /= INTERNAL) CYCLE

         I = GWC%IXW
         J = GWC%IYW
         K = GWC%IZW
      
         IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE
         IC = G%CELL_NUMBER(I,J,K)

         DO IOR0 = -3, 3
            IF (CNT(IC,IOR0) > 2) THEN
               MGM%BTYPE(IW,IOR0) = DIRICHLET
            ELSE IF (CNT(IC,IOR0) < 2) THEN
               MGM%BTYPE(IW,IOR0) = NEUMANN
            ELSE
               MGM%BTYPE(IW,IOR0) = INTERNAL
            ENDIF
         ENDDO

         IF (TWO_D) THEN
            SX = REAL(CNT(IC,1) + CNT(IC,-1) - 2, EB)
            SZ = REAL(CNT(IC,3) + CNT(IC,-3) - 2, EB)
            MGM%WEIGHT(IW) = 1.0_EB/(SX*L%DXI2 + SZ*L%DZI2)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,i4,A,3E14.6,A,7I4)') 'MGM%WEIGHT(',IW,')=', MGM%WEIGHT(IW),SX,SZ,' : ', (MGM%BTYPE(IW,I),I=-3,3)
#endif
         ELSE
            SX = REAL(CNT(IC,1) + CNT(IC,-1) - 2, EB)
            SY = REAL(CNT(IC,2) + CNT(IC,-2) - 2, EB)
            SZ = REAL(CNT(IC,3) + CNT(IC,-3) - 2, EB)
            MGM%WEIGHT(IW) = 1.0_EB/(SX*L%DXI2 + SY*L%DYI2 + SZ*L%DZI2)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,i4,A,4E14.6,A,7I4)') 'MGM%WEIGHT(',IW,')=', MGM%WEIGHT(IW),SX,SY,SZ,' : ', (MGM%BTYPE(IW,I),I=-3,3)
#endif
         ENDIF
         
      ENDDO

      CALL SCARC_DEALLOCATE_INT2(CNT, 'CNT', CROUTINE)

       
      ! The following code is purely experimental and addresses the solution of the LU method with fully stored matrices
      ! Note SCARC_MGM_USE_LU is usually set to FALSE
      IF (SCARC_MGM_USE_LU) THEN

         TYPE_SCOPE_SAVE = TYPE_SCOPE(0)
         TYPE_SCOPE(0) = NSCARC_SCOPE_LOCAL

         LM => SCARC_POINT_TO_CMATRIX (G, NSCARC_MATRIX_LM)
         UM => SCARC_POINT_TO_CMATRIX (G, NSCARC_MATRIX_UM)

         LM%N_VAL =  G%NC**2 / 2
         LM%N_ROW =  G%NC + 1

         UM%N_VAL =  G%NC**2 / 2
         UM%N_ROW =  G%NC + 1

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SETTING SIZES FOR L AND U MATRICES'
WRITE(MSG%LU_DEBUG,*) 'LM%N_VAL =', LM%N_VAL
WRITE(MSG%LU_DEBUG,*) 'LM%N_ROW =', LM%N_ROW
WRITE(MSG%LU_DEBUG,*) 'UM%N_VAL =', UM%N_VAL
WRITE(MSG%LU_DEBUG,*) 'UM%N_ROW =', UM%N_ROW
#endif

         CALL SCARC_ALLOCATE_CMATRIX (LM, NLEVEL_MIN, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_LIGHT, 'MGM%L', CROUTINE)
         CALL SCARC_ALLOCATE_CMATRIX (UM, NLEVEL_MIN, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_LIGHT, 'MGM%U', CROUTINE)

         CALL SCARC_ALLOCATE_REAL2(MGM%ASQ, 1, G%NC, 1, G%NC, NSCARC_INIT_ZERO, 'MGM%ASQ', CROUTINE)
         CALL SCARC_ALLOCATE_REAL2(MGM%LSQ, 1, G%NC, 1, G%NC, NSCARC_INIT_ZERO, 'MGM%LSQ', CROUTINE)
         CALL SCARC_ALLOCATE_REAL2(MGM%USQ, 1, G%NC, 1, G%NC, NSCARC_INIT_ZERO, 'MGM%USQ', CROUTINE)
   
         CALL SCARC_ALLOCATE_REAL1(MGM%B, 1, G%NC, NSCARC_INIT_ZERO, 'B', CROUTINE)
         CALL SCARC_ALLOCATE_REAL1(MGM%Y, 1, G%NC, NSCARC_INIT_ZERO, 'Y', CROUTINE)
         CALL SCARC_ALLOCATE_REAL1(MGM%X, 1, G%NC, NSCARC_INIT_ZERO, 'X', CROUTINE)
   
         CALL SCARC_SETUP_MGM_LU(NM, NLEVEL_MIN)

         TYPE_SCOPE(0) = TYPE_SCOPE_SAVE

      ENDIF

   ENDDO
ENDDO

END SUBROUTINE SCARC_SETUP_MGM


! ---------------------------------------------------------------------------------------------
!> \brief Setup LU-decomposition for McKeeney-Greengard-Mayo method
! ---------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MGM_LU(NM, NL)
USE SCARC_POINTERS, ONLY: L, G, MGM, A, LM, UM, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NM, NL
INTEGER :: I, J, IC0, IC, JC, KC, ICOL, IP, NMAX_U, NMAX_L
REAL (EB) :: SCAL, SCAL2, VL = 0.0_EB, VU = 0.0_EB, VAL
INTEGER :: TYPE_SCOPE_SAVE

TYPE_SCOPE_SAVE = TYPE_SCOPE(0)
TYPE_SCOPE(0) = NSCARC_SCOPE_LOCAL

L   => SCARC(NM)%LEVEL(NL)
G   => L%UNSTRUCTURED
MGM => L%MGM
A   => G%LAPLACE


#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_CMATRIX (A, 'LAPLACE', 'SETUP_MGM_LU: INIT ')
WRITE(MSG%LU_DEBUG,*) 'G%NC =', G%NC
#endif
#ifdef WITH_SCARC_DEBUG2
CALL SCARC_MATLAB_MATRIX(A%VAL, A%ROW, A%COL, G%NC, G%NC, NM, NL, 'LAPLACE')
#endif
 
DO IC = 1, G%NC
   !JC = G%PERM_FW(IC)
   JC = IC

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'PERMUTATION  --- IC =', IC, '-----------> JC=',JC
#endif

   IF (JC == 0) CYCLE

   DO IP = A%ROW(IC), A%ROW(IC+1)-1
      ICOL = A%COL(IP)

#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG,*) 'PERMUTATION ICOL ', ICOL, A%VAL(IP), ' TO ', IC, ICOL
#endif

      MGM%ASQ(JC,ICOL) = A%VAL(IP)
   ENDDO
ENDDO

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) '------- MGM%A - Copy (1:24)'
DO IC = 1, G%NC
   WRITE(MSG%LU_DEBUG,'(24F8.2)') (MGM%ASQ(IC, JC), JC=1, 24)
ENDDO
#endif

! Temporarily extract full matrix from compact storage technique - just for proof of concept
! Consider permutation in G%PERM
 
LM  => SCARC_POINT_TO_CMATRIX (G, NSCARC_MATRIX_LM)
UM  => SCARC_POINT_TO_CMATRIX (G, NSCARC_MATRIX_UM)

DO JC = 1, G%NC
   !IC = G%PERM_FW(JC)
   IC = JC
   UM%ROW(IC) = IC
   UM%COL(IC) = IC
   LM%ROW(IC) = IC
   LM%COL(IC) = IC
ENDDO
UM%ROW(G%NC+1) = G%NC+1
LM%ROW(G%NC+1) = G%NC+1

NMAX_U = G%NC
NMAX_L = G%NC

ROW_LOOP: DO IC0 = 1, G%NC  

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '==================================> IC0=', IC0
#endif
   !IC = G%PERM_FW(IC0)
   IC = IC0

   VAL = 1.0_EB
   MGM%LSQ(IC,IC) = VAL

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,1000) IC
WRITE(MSG%LU_DEBUG,1100) IC, IC, MGM%LSQ(IC,IC)
#endif
   CALL SCARC_INSERT_TO_CMATRIX(LM, VAL, IC, IC, G%NC, NMAX_L, 'LM')

   COL_LOOP: DO JC = IC, G%NC

      SCAL  = 0.0_EB
      SCAL2 = 0.0_EB
      DO KC = 1, IC-1
         SCAL = SCAL + MGM%LSQ(IC,KC) * MGM%USQ(KC,JC)
         VL = SCARC_EVALUATE_CMATRIX (LM, IC, KC)
         VU = SCARC_EVALUATE_CMATRIX (UM, KC, JC)
         SCAL2 = SCAL2 + VL * VU
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,1200) IC, KC, KC, JC, JC, SCAL, VL, VU, SCAL2
#endif
      ENDDO
      VAL = MGM%ASQ(IC,JC) - SCAL
      MGM%USQ(IC,JC) = VAL

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,1300) IC, JC, MGM%ASQ(IC,JC), IC, JC, MGM%USQ(IC,JC)
#endif
      !VAL = SCARC_EVALUATE_CMATRIX(A, IC, JC)  - SCAL
      CALL SCARC_INSERT_TO_CMATRIX(UM, VAL, IC, JC, G%NC, NMAX_U, 'UM')

      SCAL = 0.0_EB
      SCAL2 = 0.0_EB
      DO KC = 1, IC-1
         SCAL = SCAL + MGM%LSQ(JC,KC) * MGM%USQ(KC,IC)
         VL = SCARC_EVALUATE_CMATRIX (LM, JC, KC)
         VU = SCARC_EVALUATE_CMATRIX (UM, KC, IC)
         SCAL2 = SCAL2 + VL * VU
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,1200) JC, KC, KC, IC, KC, VL, VU, SCAL, SCAL2
#endif
      ENDDO
      VAL = (MGM%ASQ(JC,IC) - SCAL)/MGM%USQ(IC,IC)
      MGM%LSQ(JC,IC) = VAL


#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,1400) JC, IC, MGM%ASQ(JC,IC), IC, IC, MGM%USQ(IC, IC), JC, IC, MGM%LSQ(IC,JC)
#endif
      !VAL = (SCARC_EVALUATE_CMATRIX(A, JC, IC)  - SCAL)/MGM%USQ(IC,IC)
      CALL SCARC_INSERT_TO_CMATRIX(LM, VAL, JC, IC, G%NC, NMAX_L, 'LM')

   ENDDO COL_LOOP
   !LM%ROW(IC+1) = IL
   !UM%ROW(IC+1) = IL

ENDDO ROW_LOOP

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) '=============================== MGM%A (1:24) '
DO I = 1, G%NC
   WRITE(MSG%LU_DEBUG,'(24F8.2)') (MGM%ASQ(I, J), J=1, 24)
ENDDO
WRITE(MSG%LU_DEBUG,*) 'MGM%L'
DO I = 1, G%NC
   WRITE(MSG%LU_DEBUG,'(24F8.2)') (MGM%LSQ(I, J), J=1, 24)
ENDDO
WRITE(MSG%LU_DEBUG,*) 'MGM%U'
DO I = 1, G%NC
   WRITE(MSG%LU_DEBUG,'(24F8.2)') (MGM%USQ(I, J), J=1, 24)
ENDDO
#endif
#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_CMATRIX (LM, 'MGM%L-FINAL', 'SETUP_MGM_LU ')
CALL SCARC_DEBUG_CMATRIX (UM, 'MGM%U-FINAL', 'SETUP_MGM_LU ')
#endif


DO I = 1, G%NC
   DO J = 1, I-1
      MGM%ASQ(I,J) = MGM%LSQ(I,J)
   ENDDO
   DO J = I, G%NC
      MGM%ASQ(I,J) = MGM%USQ(I,J)
   ENDDO
ENDDO

TYPE_SCOPE(0) = TYPE_SCOPE_SAVE

#ifdef WITH_SCARC_DEBUG
1000 FORMAT('================= IC : ', I2,' ===========================')
1100 FORMAT('LSQ(',I2,',',I2,'):', E14.6)
1200 FORMAT('LSQ(',I2,',',I2,'),  USQ(',I2,',',I2,') --> JC:', I2, ',  SCAL:', E14.6, ', VL, VU:', 2E14.6,', SCAL2:',E14.6)
1300 FORMAT('ASQ(',I2,',',I2,'):',E14.6,',  USQ(',I2,',',I2,'):', E14.6)
1400 FORMAT('ASQ(',I2,',',I2,'):',E14.6,',  USQ(',I2,',',I2,'):', E14.6,',  LSQ(',I2,',',I2,'):', E14.6)
#endif
END SUBROUTINE SCARC_SETUP_MGM_LU


SUBROUTINE SCARC_MGM_DUMP (CTYPE, ITE_MGM)
USE SCARC_POINTERS, ONLY: L, MGM, HP
INTEGER, INTENT(IN) :: ITE_MGM
CHARACTER(*), INTENT(IN) :: CTYPE
CHARACTER(80) :: FN_DUMP
INTEGER :: IX, IY, IZ, NM
INTEGER, SAVE :: LU_DUMP

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
 

   L => SCARC(NM)%LEVEL(NLEVEL_MIN)
   MGM => L%MGM

   SELECT CASE (TRIM(CTYPE))
      CASE ('H1')
         HP => MGM%H1
      CASE ('H2')
         HP => MGM%H2
      CASE ('H3')
         HP => MGM%H3
      CASE ('H4')
         HP => MGM%H4
      CASE ('H5')
         HP => MGM%H5
      CASE ('H6')
         HP => MGM%H6
      CASE ('H7')
         HP => MGM%H7
      CASE ('HS')
         HP => MGM%HS
      CASE ('HU')
         HP => MGM%HU
      CASE ('HD')
         HP => MGM%HD
   END SELECT

   WRITE (FN_DUMP, '(A,A,A,I1,A,A,A,i4.4,A,I4.4)') 'pressure/',TRIM(CHID),'_M',NM,'_',TRIM(CTYPE),'_TPI',&
                                                    TOTAL_PRESSURE_ITERATIONS,'_MGM',ITE_MGM
   
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
ENDDO

END SUBROUTINE SCARC_MGM_DUMP


! ------------------------------------------------------------------------------------------------------
!> \brief Convergence state of MGM method
! ------------------------------------------------------------------------------------------------------
INTEGER FUNCTION SCARC_MGM_CONVERGENCE_STATE(ITE_MGM)
USE SCARC_POINTERS, ONLY: MGM
INTEGER, INTENT(IN) :: ITE_MGM
INTEGER :: NM

! Note: Convergence history of previous Krylov method is available in ITE and CAPPA 

SCARC_MGM_CONVERGENCE_STATE = NSCARC_MGM_FAILURE
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   MGM => SCARC(NM)%LEVEL(NLEVEL_MIN)%MGM
   IF (MGM%VELOCITY_ERROR > VELOCITY_ERROR_GLOBAL) VELOCITY_ERROR_GLOBAL = MGM%VELOCITY_ERROR

   SELECT CASE (ITE_MGM)
      CASE (-1)
#ifdef WITH_SCARC_VERBOSE2
      IF (VELOCITY_ERROR_GLOBAL <= SCARC_MGM_ACCURACY) THEN
         WRITE(MSG%LU_VERBOSE,1300) ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
                                    MGM%ITE_POISSON, MGM%ITE, MGM%ITE_LAPLACE, VELOCITY_ERROR_GLOBAL, ' ... success'
      ELSE
         WRITE(MSG%LU_VERBOSE,1300) ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
                                    MGM%ITE_POISSON, MGM%ITE, MGM%ITE_LAPLACE, VELOCITY_ERROR_GLOBAL, ' ... failed'
      ENDIF
      WRITE(MSG%LU_VERBOSE,2000) 
#endif
      CASE (0)
         MGM%ITE = 0
         MGM%ITE_LAPLACE = 0
         MGM%ITE_POISSON = ITE                 
         MGM%CAPPA_POISSON = CAPPA
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,1100)   ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
                              MGM%ITE_POISSON, MGM%ITE, MGM%ITE_LAPLACE, VELOCITY_ERROR_GLOBAL
#endif
#ifdef WITH_SCARC_VERBOSE
!   WRITE(MSG%LU_VERBOSE,1100) ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
!                              MGM%ITE_POISSON, MGM%ITE, MGM%ITE_LAPLACE, VELOCITY_ERROR_GLOBAL
!   WRITE(MSG%LU_VERBOSE,1101)   TOTAL_PRESSURE_ITERATIONS, MGM%ITE_LAPLACE, VELOCITY_ERROR_GLOBAL
#endif
      CASE DEFAULT
         MGM%ITE = ITE_MGM
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,1200)   ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
                              MGM%ITE_POISSON, MGM%ITE, ITE, VELOCITY_ERROR_GLOBAL
#endif
#ifdef WITH_SCARC_VERBOSE
!   WRITE(MSG%LU_VERBOSE,1200) ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
!                              MGM%ITE_POISSON, MGM%ITE, ITE, VELOCITY_ERROR_GLOBAL
    WRITE(MSG%LU_VERBOSE,1101)   TOTAL_PRESSURE_ITERATIONS, VELOCITY_ERROR_GLOBAL
#endif
         IF (ITE > MGM%ITE_LAPLACE) THEN
            MGM%ITE_LAPLACE = MAX(ITE, MGM%ITE_LAPLACE)
            MGM%CAPPA_LAPLACE = CAPPA
         ENDIF
   END SELECT
   IF (VELOCITY_ERROR_GLOBAL <= SCARC_MGM_ACCURACY) SCARC_MGM_CONVERGENCE_STATE = NSCARC_MGM_SUCCESS

ENDDO

#ifdef WITH_SCARC_VERBOSE
1101 FORMAT(I6, ' , ', E11.3)
#endif
#ifdef WITH_SCARC_DEBUG
1100 FORMAT('TS ',I6, ', #PI: ', I6,', #TPI: ', I6, &
            ' , #POISSON: ', I6,&
            ' , #MGM: ', I6,&
            ' , #LAPLACE    : ', I6,&
            ' , VE: ', E14.6)
1200 FORMAT('TS ',I6, ', #PI: ', I6,', #TPI: ', I6, &
            ' , #POISSON: ', I6,&
            ' , #MGM: ', I6,&
            ' , #LAPLACE    : ', I6,&
            ' , VE: ', E14.6)
1300 FORMAT('TS ',I6, ', #PI: ', I6,', #TPI: ', I6, &
            ' , #POISSON: ', I6,&
            ' , #MGM: ', I6,&
            ' , #LAPLACE_max: ', I6,&
            ' , VE: ', E14.6, a14)
2000 FORMAT('------------------------------------------------------------------------------------')
#endif

END FUNCTION SCARC_MGM_CONVERGENCE_STATE

! ------------------------------------------------------------------------------------------------
!> \brief Set correct boundary values at external and internal boundaries
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_MGM_UPDATE_GHOSTCELLS(NTYPE)
USE SCARC_POINTERS, ONLY: M, L, G, GWC, HP, MGM, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NTYPE
INTEGER :: NM, IW, IOR0, IXG, IYG, IZG, IXW, IYW, IZW 
#ifdef WITH_SCARC_DEBUG
INTEGER :: I, K
#endif

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NLEVEL_MIN)                                   
   MGM => SCARC(NM)%LEVEL(NLEVEL_MIN)%MGM

   SELECT CASE(NTYPE)

      CASE  (NSCARC_MGM_POISSON, NSCARC_MGM_SCARC, NSCARC_MGM_USCARC, NSCARC_MGM_TERMINATE) 

         IF (NTYPE == NSCARC_MGM_POISSON) THEN
            HP => MGM%H1
         ELSE IF (NTYPE == NSCARC_MGM_TERMINATE) THEN
            HP => MGM%H3
         ELSE IF (NTYPE == NSCARC_MGM_SCARC) THEN
            HP => MGM%HS
         ELSE IF (NTYPE == NSCARC_MGM_USCARC) THEN
            HP => MGM%HU
         ELSE
            WRITE(*,*) 'ERROR IN UMPDATE_GHOSTCELLS'
         ENDIF
      
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'UPDATE_MGM_GHOST_CELLS:1: HP: NTYPE:', NTYPE
WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((HP(I,1,K), I=0, L%NX+1), K=L%NZ+1,0,-1)
#endif
       
         WALL_CELLS_LOOP: DO IW = 1, L%N_WALL_CELLS_EXT
      
            GWC => G%WALL(IW)
      
            IF (GWC%BTYPE == INTERNAL) CYCLE

            IXG = GWC%IXG
            IYG = GWC%IYG
            IZG = GWC%IZG
      
            IXW = GWC%IXW
            IYW = GWC%IYW
            IZW = GWC%IZW
      
            IOR0 = GWC%IOR
      
            SELECT CASE (IOR0)
               CASE ( 1)
                  IF (GWC%BTYPE==DIRICHLET) THEN
                     HP(IXG,IYW,IZW) = -HP(IXW,IYW,IZW) + 2.0_EB * M%BXS(IYW,IZW)
                  ELSE IF (GWC%BTYPE==NEUMANN) THEN
                     HP(IXG,IYW,IZW) =  HP(IXW,IYW,IZW) - L%DX *M%BXS(IYW,IZW)
                  ENDIF
               CASE (-1)
                  IF (GWC%BTYPE==DIRICHLET) THEN
                     HP(IXG,IYW,IZW) = -HP(IXW,IYW,IZW) + 2.0_EB * M%BXF(IYW,IZW)
                  ELSE IF (GWC%BTYPE==NEUMANN) THEN
                     HP(IXG,IYW,IZW) =  HP(IXW,IYW,IZW) + L%DX *M%BXF(IYW,IZW)
                  ENDIF
               CASE ( 2)
                  IF (GWC%BTYPE==DIRICHLET) THEN
                     HP(IXW,IYG,IZW) = -HP(IXW,IYW,IZW) + 2.0_EB * M%BYS(IXW,IZW)
                  ELSE IF (GWC%BTYPE==NEUMANN) THEN
                     HP(IXW,IYG,IZW) =  HP(IXW,IYW,IZW) - L%DY *M%BYS(IXW,IZW)
                  ENDIF
               CASE (-2)
                  IF (GWC%BTYPE==DIRICHLET) THEN
                     HP(IXW,IYG,IZW) = -HP(IXW,IYW,IZW) + 2.0_EB * M%BYF(IXW,IZW)
                  ELSE IF (GWC%BTYPE==NEUMANN) THEN
                     HP(IXW,IYG,IZW) =  HP(IXW,IYW,IZW) + L%DY *M%BYF(IXW,IZW)
                  ENDIF
               CASE ( 3)
                  IF (GWC%BTYPE==DIRICHLET) THEN
                     HP(IXW,IYW,IZG) = -HP(IXW,IYW,IZW) + 2.0_EB * M%BZS(IXW,IYW)
                  ELSE IF (GWC%BTYPE==NEUMANN) THEN
                     HP(IXW,IYW,IZG) =  HP(IXW,IYW,IZW) - L%DZ *M%BZS(IXW,IYW)
                  ENDIF
               CASE (-3)
                  IF (GWC%BTYPE==DIRICHLET) THEN
                     HP(IXW,IYW,IZG) = -HP(IXW,IYW,IZW) + 2.0_EB * M%BZF(IXW,IYW)
                  ELSE IF (GWC%BTYPE==NEUMANN) THEN
                     HP(IXW,IYW,IZG) =  HP(IXW,IYW,IZW) + L%DZ *M%BZF(IXW,IYW)
                  ENDIF
            END SELECT
#ifdef WITH_SCARC_DEBUG
            WRITE(MSG%LU_DEBUG,'(A, 5I6, E14.6)') 'UPDATE_GHOST_CELLS: IW, IOR0, IXW, IYW, IZG, HP:',&
                                                   IW, IOR0, IXW, IYW, IZG, HP(IXW, IYW, IZG)
#endif
      
         ENDDO WALL_CELLS_LOOP

      ! 
      ! Update ghostcells for local Laplace problems
      ! Along external boundaries use zero Dirichlet or Neumann BC's
      ! Along mesh interfaces use Dirichlet BC's corresponding to interface settings stored in MGM%BC
      ! 
      CASE (NSCARC_MGM_LAPLACE)

         HP => MGM%H2
   
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'UPDATE_MGM_GHOST_CELLS:1: HP: NTYPE: LAPLACE'
WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((HP(IXG,1,IZG), IXG=0, L%NX+1), IZG=L%NZ+1,0,-1)
WRITE(MSG%LU_DEBUG,*) 'UPDATE_MGM_GHOST_CELLS:1: BC: NTYPE: LAPLACE'
WRITE(MSG%LU_DEBUG,MSG%CFORM1) (MGM%BC(IW), IW=1, L%N_WALL_CELLS_EXT)
#endif
    
         WALL_CELLS_LOOP_LAPLACE: DO IW = 1, L%N_WALL_CELLS_EXT
      
            GWC => G%WALL(IW)
      
            IXG = GWC%IXG
            IYG = GWC%IYG
            IZG = GWC%IZG
      
            IXW = GWC%IXW
            IYW = GWC%IYW
            IZW = GWC%IZW
      
            IOR0 = GWC%IOR
      
            SELECT CASE (ABS(IOR0))

               CASE (1)
                  IF (GWC%BTYPE == INTERNAL) THEN
                     HP(IXG,IYW,IZW) = -HP(IXW,IYW,IZW) + 2.0_EB * MGM%BC(IW)
#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG,1000) IOR0, IW, IXG, IYW, IZW, MGM%BC(IW), HP(IXW, IYW, IZW), HP(IXG, IYW, IZW)
#endif
                  ELSE IF (GWC%BTYPE == DIRICHLET) THEN
                     HP(IXG,IYW,IZW) = -HP(IXW,IYW,IZW) 
                  ELSE IF (GWC%BTYPE == NEUMANN) THEN
                     HP(IXG,IYW,IZW) =  HP(IXW,IYW,IZW) 
                  ENDIF

               CASE (2)
                  IF (GWC%BTYPE == INTERNAL) THEN
                     HP(IXW,IYG,IZW) = -HP(IXW,IYW,IZW) + 2.0_EB * MGM%BC(IW)
#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG,1000) IOR0, IW, IXW, IYG, IZW, MGM%BC(IW), HP(IXW, IYW, IZW), HP(IXW, IYG, IZW)
#endif
                  ELSE IF (GWC%BTYPE==DIRICHLET) THEN
                     HP(IXW,IYG,IZW) = -HP(IXW,IYW,IZW) 
                  ELSE IF (GWC%BTYPE==NEUMANN) THEN
                     HP(IXW,IYG,IZW) =  HP(IXW,IYW,IZW) 
                  ENDIF

               CASE (3)
                  IF (GWC%BTYPE == INTERNAL) THEN
                     HP(IXW,IYW,IZG) = -HP(IXW,IYW,IZW) + 2.0_EB * MGM%BC(IW)
#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG,1000) IOR0, IW, IXW, IYW, IZG, MGM%BC(IW), HP(IXW, IYW, IZW), HP(IXW, IYW, IZG)
#endif
                  ELSE IF (GWC%BTYPE==DIRICHLET) THEN
                     HP(IXW,IYW,IZG) = -HP(IXW,IYW,IZW) 
                  ELSE IF (GWC%BTYPE==NEUMANN) THEN
                     HP(IXW,IYW,IZG) =  HP(IXW,IYW,IZW) 
                  ENDIF
            END SELECT
      
         ENDDO WALL_CELLS_LOOP_LAPLACE

   END SELECT

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'UPDATE_GHOST_CELLS:2: HP'
WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((HP(I,1,K), I=0, L%NX+1), K=L%NZ+1,0,-1)
#endif
#ifdef WITH_SCARC_VERBOSE2
   CALL SCARC_VERBOSE_PRESSURE (HP, NM, 'H')
#endif

ENDDO
#ifdef WITH_SCARC_DEBUG
1000 FORMAT('UMPDATE: LAPLACE: INTERNAL: IOR0=', I4,': IW=', I4, ': I,J,K=', 3I4,' BC, HPOLD, HP:', 3E14.6)
#endif

END SUBROUTINE SCARC_MGM_UPDATE_GHOSTCELLS

! ----------------------------------------------------------------------------------------------------
!> \brief Store preliminary solution vector in McKeeney-Greengard-Mayo method
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_MGM_STORE_SOLUTION(NTYPE)
USE SCARC_POINTERS, ONLY: L, G, ST, MGM, HP, GWC, M
INTEGER, INTENT(IN) :: NTYPE
INTEGER :: NM, IX, IY, IZ, ICS, ICU, ICE, IOR0, IW

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   M   => MESHES(NM)
   L   => SCARC(NM)%LEVEL(NLEVEL_MIN)
   ST  => L%STAGE(NSCARC_STAGE_ONE)
   MGM => L%MGM

   SELECT CASE(NTYPE)

      ! --------------- Store structured ScaRC solution
      CASE (NSCARC_MGM_SCARC)
         G  => L%STRUCTURED
         HP => MGM%HS
         DO IZ = 1, L%NZ
            DO IY = 1, L%NY
               DO IX = 1, L%NX
                  ICS = G%CELL_NUMBER(IX, IY, IZ)              ! structured cell number
                  HP(IX, IY, IZ) = ST%X(ICS) 
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG,'(A, 4I6,1E14.6)') 'MGM-SCARC:A: IX, IY, IZ, ICS, HS:',IX,IY,IZ,ICS,HP(IX,IY,IZ)
#endif
               ENDDO
            ENDDO
         ENDDO

         DO IW = 1, L%N_WALL_CELLS_EXT
            GWC => G%WALL(IW)
            IF (GWC%BTYPE /= INTERNAL) CYCLE
            IOR0 = GWC%IOR
            IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE
            IX = GWC%IXG ;  IY = GWC%IYG ;  IZ = GWC%IZG
            ICE = L%STRUCTURED%CELL_NUMBER(IX, IY, IZ)
            HP(IX, IY, IZ) = ST%X(ICE) 
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG,'(A, 5I6,1E14.6)') 'MGM-SCARC:B: IX, IY, IZ, IW, ICE, H1:',IX,IY,IZ,IW,ICE,HP(IX,IY,IZ)
#endif
         ENDDO

      ! --------------- Store unstructured UScaRC solution
      CASE (NSCARC_MGM_USCARC)
         G  => L%UNSTRUCTURED
         HP => MGM%HU
         HP = 0.0_EB
         DO IZ = 1, L%NZ
            DO IY = 1, L%NY
               DO IX = 1, L%NX
                  IF (L%IS_SOLID(IX, IY, IZ)) CYCLE
                  ICS = G%CELL_NUMBER(IX, IY, IZ)           ! unstructured cell number
                  HP(IX, IY, IZ) = ST%X(ICS) 
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG,'(A, 4I6,1E14.6)') 'MGM-USCARC:A: IX, IY, IZ, ICS, HS:',IX,IY,IZ,ICS,HP(IX,IY,IZ)
#endif
               ENDDO
            ENDDO
         ENDDO

         DO IW = 1, L%N_WALL_CELLS_EXT
            GWC => G%WALL(IW)
            IF (GWC%BTYPE /= INTERNAL) CYCLE
            IOR0 = GWC%IOR
            IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE
            IX = GWC%IXG ;  IY = GWC%IYG ;  IZ = GWC%IZG
            ICE = G%CELL_NUMBER(IX, IY, IZ)
            HP(IX, IY, IZ) = ST%X(ICE) 
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG,'(A, 5I6,1E14.6)') 'MGM-USCARC:B: IX, IY, IZ, IW, ICE, H1:',IX,IY,IZ,IW,ICE,HP(IX,IY,IZ)
#endif
         ENDDO

      ! --------------- Build difference between structured ScaRC and unstructured UScaRC solution
      CASE (NSCARC_MGM_DIFFERENCE)

         MGM%HD = MGM%HU - MGM%HS

         G  => L%STRUCTURED
         IF (TWO_D) THEN
            !DO IW = L%N_WALL_CELLS_EXT+1, L%N_WALL_CELLS_EXT + L%N_WALL_CELLS_INT
            DO IW = 1, L%N_WALL_CELLS_EXT + L%N_WALL_CELLS_INT
               GWC => G%WALL(IW)
               IOR0 = GWC%IOR
               IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE
               IX = GWC%IXG ;  IZ = GWC%IZG
               IF (L%IS_SOLID(IX, 1, IZ)) MGM%HD(IX, 0:2, IZ) = 0.0_EB
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A, 3I6,3E14.6)') 'MGM-DIFFERENCE:2D: IW, IX, IZ, HD:',IW, IX,IZ,MGM%HD(IX,0,IZ), MGM%HD(IX,1,IZ),MGM%HD(IX,2,IZ)
#endif
            ENDDO
         ELSE
            DO IW = L%N_WALL_CELLS_EXT+1, L%N_WALL_CELLS_EXT + L%N_WALL_CELLS_INT
               GWC => G%WALL(IW)
               IOR0 = GWC%IOR
               IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE
               IX = GWC%IXG ;  IY = GWC%IYG ;  IZ = GWC%IZG
               IF (L%IS_SOLID(IX, IY, IZ)) MGM%HD(IX, IY, IZ) = 0.0_EB
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A, 4I6,E14.6)') 'MGM-DIFFERENCE:3D: IW, IX, IY, IZ, HD:',IW, IX,IY, IZ,MGM%HD(IX,IY,IZ)
#endif
            ENDDO
         ENDIF

      ! --------------- Store inhomogeneous structured Poisson solution
      CASE (NSCARC_MGM_POISSON)
         G  => L%STRUCTURED
         HP => MGM%H1
         !HP = 0.0_EB
         DO IZ = 1, L%NZ
            DO IY = 1, L%NY
               DO IX = 1, L%NX
                  ICS = G%CELL_NUMBER(IX, IY, IZ)              ! structured cell number
                  HP(IX, IY, IZ) = ST%X(ICS) 
#ifdef WITH_SCARC_DEBUG
IF (IY == 1) WRITE(MSG%LU_DEBUG,'(A, 4I6,1E14.6)') 'MGM-POISSON:A: IX, IY, IZ, ICS, H1:',IX,IY,IZ,ICS,HP(IX,IY,IZ)
#endif
               ENDDO
            ENDDO
         ENDDO

         DO IW = 1, L%N_WALL_CELLS_EXT
            GWC => G%WALL(IW)
            IF (GWC%BTYPE /= INTERNAL) CYCLE
            IOR0 = GWC%IOR
            IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE
            IX = GWC%IXG ;  IY = GWC%IYG ;  IZ = GWC%IZG
            ICE = L%STRUCTURED%CELL_NUMBER(IX, IY, IZ)
            HP(IX, IY, IZ) = ST%X(ICE) 
#ifdef WITH_SCARC_DEBUG
IF (IY == 1) WRITE(MSG%LU_DEBUG,'(A, 5I6,1E14.6)') 'MGM-POISSON:B: IX, IY, IZ, IW, ICE, H1:',IX,IY,IZ,IW,ICE,HP(IX,IY,IZ)
#endif
         ENDDO

      ! --------------- Store homogeneous unstructured Laplace solution
      CASE (NSCARC_MGM_LAPLACE)

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '=======================> START OF MGM_STORE_SOLUTION: LAPLACE'
   WRITE(MSG%LU_DEBUG,*) 'MGM%H2'
   WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H2(IX,1,IZ), IX=0, L%NX+1), IZ=L%NZ+1,0,-1)
   WRITE(MSG%LU_DEBUG,*) 'MGM%H5'
   WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H5(IX,1,IZ), IX=0, L%NX+1), IZ=L%NZ+1,0,-1)
#endif

         G => L%UNSTRUCTURED
         IF (TYPE_MGM_BC == NSCARC_MGM_BC_EXPOL) THEN
            DO IZ = 0, L%NZ+1
               DO IY = 0, L%NY+1
                  DO IX = 0, L%NX+1
                     IF (L%IS_SOLID(IX, IY, IZ)) CYCLE
                     ICU = G%CELL_NUMBER(IX, IY, IZ)              ! structured cell number
                     MGM%H4(IX, IY, IZ) = MGM%H2(IX, IY, IZ)  
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG,'(A, 4I6,1E14.6)') 'MGM:H4:EXPOL: IX, IY, IZ, ICU, H3:', IX, IY, IZ, ICU, MGM%H4(IX,IY,IZ)
#endif
                  ENDDO
               ENDDO
            ENDDO
         ENDIF

         MGM%H2 = 0.0_EB
         IF (SCARC_MGM_USE_LU) THEN
            DO IZ = 1, L%NZ
               DO IY = 1, L%NY
                  DO IX = 1, L%NX
                     IF (L%IS_SOLID(IX, IY, IZ)) CYCLE
                     ICU = G%PERM_BW(G%CELL_NUMBER(IX, IY, IZ))              ! structured cell number
                     MGM%H2(IX, IY, IZ) = MGM%X(ICU)
                     MGM%H5(IX, IY, IZ) = MGM%H5(IX, IY, IZ) + MGM%X(ICU)
#ifdef WITH_SCARC_DEBUG 
IF (IY == 1) WRITE(MSG%LU_DEBUG,'(A, 4I6,2E14.6)') 'MGM-LAPLACE:A: IX, IY, IZ, ICU, H2:',IX,IY,IZ,ICU,MGM%H2(IX,IY,IZ), &
                                                    MGM%X(G%PERM_BW(ICU))
#endif
                  ENDDO
               ENDDO
            ENDDO
         ELSE
            DO IZ = 1, L%NZ
               DO IY = 1, L%NY
                  DO IX = 1, L%NX
                     IF (L%IS_SOLID(IX, IY, IZ)) CYCLE
                     ICU = G%CELL_NUMBER(IX, IY, IZ)              ! structured cell number
                     MGM%H2(IX, IY, IZ) = ST%X(ICU)
                     MGM%H5(IX, IY, IZ) = MGM%H5(IX, IY, IZ) + ST%X(ICU)
#ifdef WITH_SCARC_DEBUG 
IF (IY == 1) WRITE(MSG%LU_DEBUG,'(A, 4I6,2E14.6)') 'MGM-LAPLACE:A: IX, IY, IZ, ICU, H2:',IX,IY,IZ,ICU,MGM%H2(IX,IY,IZ), &
                                                    ST%X(ICU)
#endif
                  ENDDO
               ENDDO
            ENDDO
         ENDIF

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '=======================> END   OF MGM_STORE_SOLUTION: LAPLACE'
   WRITE(MSG%LU_DEBUG,*) 'MGM%H2'
   WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H2(IX,1,IZ), IX=0, L%NX+1), IZ=L%NZ+1,0,-1)
   WRITE(MSG%LU_DEBUG,*) 'MGM%H5'
   WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H5(IX,1,IZ), IX=0, L%NX+1), IZ=L%NZ+1,0,-1)
#endif

      ! --------------- Merge structured inhomogeneous Poisson and unstructured homogeneous Laplace solutions
      CASE (NSCARC_MGM_MERGE)

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '=======================> START OF MGM_STORE_SOLUTION: MGM_MERGE'
   WRITE(MSG%LU_DEBUG,*) 'MGM%H3'
   WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H3(IX,1,IZ), IX=0, L%NX+1), IZ=L%NZ+1,0,-1)
   WRITE(MSG%LU_DEBUG,*) 'MGM%H2'
   WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H2(IX,1,IZ), IX=0, L%NX+1), IZ=L%NZ+1,0,-1)
#endif

         DO IZ = 0, L%NZ+1
            DO IY = 0, L%NY+1
               DO IX = 0, L%NX+1
                  !MGM%H3(IX, IY, IZ) = MGM%H3(IX, IY, IZ) + MGM%H2(IX, IY, IZ)              ! Variant A
                  MGM%H3(IX, IY, IZ) = MGM%H1(IX, IY, IZ) + MGM%H2(IX, IY, IZ)              ! Variant A
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG,'(A, 3I6,3E14.6)') 'MGM:M: IX, IY, IZ, H1, H2, H3:',  IX, IY, IZ, &
                                                    MGM%H1(IX,IY,IZ),MGM%H2(IX,IY,IZ), MGM%H3(IX, IY, IZ)
#endif
               ENDDO
            ENDDO
         ENDDO


#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '=======================> END   OF MGM_STORE_SOLUTION: MGM_MERGE'
   WRITE(MSG%LU_DEBUG,*) 'MGM%H3'
   WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H3(IX,1,IZ), IX=0, L%NX+1), IZ=L%NZ+1,0,-1)
   WRITE(MSG%LU_DEBUG,*) 'MGM%H2'
   WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H2(IX,1,IZ), IX=0, L%NX+1), IZ=L%NZ+1,0,-1)
#endif

#ifdef WITH_SCARC_VERBOSE2
         CALL SCARC_VERBOSE_VECTOR3 (MGM%H1,'H1')
         CALL SCARC_VERBOSE_VECTOR3 (MGM%H2,'H2')
         CALL SCARC_VERBOSE_VECTOR3 (HP, 'HP')
#endif

      ! --------------- Terminate MGM method and extract predictor/corrector solution for FDS code
      CASE (NSCARC_MGM_TERMINATE)

         IF (PREDICTOR) THEN

            DO IZ = 0, L%NZ+1
               DO IY = 0, L%NY+1
                  DO IX = 0, L%NX+1
                     M%H(IX, IY, IZ) = MGM%H3(IX, IY, IZ) 
#ifdef WITH_SCARC_DEBUG
IF (IY == 1) WRITE(MSG%LU_DEBUG,'(A, 3I6,1E14.6)') 'MGM-PREDICTOR: IX, IY, IZ, H3:', IX, IY, IZ, M%H(IX,IY,IZ)
#endif
                  ENDDO
               ENDDO
            ENDDO
            IF (TWO_D) THEN
               DO IZ = 1, L%NZ
                  DO IX = 1, L%NX
                     IF (L%IS_SOLID(IX,1,IZ)) M%H(IX, 0:2, IZ) = 0.0_EB
#ifdef WITH_SCARC_DEBUG
IF (IY == 1) WRITE(MSG%LU_DEBUG,'(A, 3I6,1E14.6)') 'MGM-PREDICTOR: IX, IY, IZ, H3:', IX, IY, IZ, M%H(IX,IY,IZ)
#endif
                  ENDDO
               ENDDO
            ELSE
               DO IZ = 1, L%NZ
                  DO IY = 1, L%NY
                     DO IX = 1, L%NX
                        IF (L%IS_SOLID(IX,IY,IZ)) M%H(IX, IY, IZ) = 0.0_EB
#ifdef WITH_SCARC_DEBUG
IF (IY == 1) WRITE(MSG%LU_DEBUG,'(A, 3I6,1E14.6)') 'MGM-PREDICTOR: IX, IY, IZ, H3:', IX, IY, IZ, M%H(IX,IY,IZ)
#endif
                     ENDDO
                  ENDDO
               ENDDO
            ENDIF

         ELSE

            DO IZ = 0, L%NZ+1
               DO IY = 0, L%NY+1
                  DO IX = 0, L%NX+1
                     M%HS(IX, IY, IZ) = MGM%H3(IX, IY, IZ) 
#ifdef WITH_SCARC_DEBUG
IF (IY == 1) WRITE(MSG%LU_DEBUG,'(A, 3I6,1E14.6)') 'MGM-CORRECTOR: IX, IY, IZ, H3:', IX, IY, IZ, M%HS(IX,IY,IZ)
#endif
                  ENDDO
               ENDDO
            ENDDO
            IF (TWO_D) THEN
               DO IZ = 1, L%NZ
                  DO IX = 1, L%NX
                     IF (L%IS_SOLID(IX,1,IZ)) M%HS(IX, 0:2, IZ) = 0.0_EB
#ifdef WITH_SCARC_DEBUG
IF (IY == 1) WRITE(MSG%LU_DEBUG,'(A, 3I6,1E14.6)') 'MGM-PREDICTOR: IX, IY, IZ, H3:', IX, IY, IZ, M%HS(IX,IY,IZ)
#endif
                  ENDDO
               ENDDO
            ELSE
               DO IZ = 1, L%NZ
                  DO IY = 1, L%NY
                     DO IX = 1, L%NX
                        IF (L%IS_SOLID(IX,IY,IZ)) M%HS(IX, IY, IZ) = 0.0_EB
#ifdef WITH_SCARC_DEBUG
IF (IY == 1) WRITE(MSG%LU_DEBUG,'(A, 3I6,1E14.6)') 'MGM-PREDICTOR: IX, IY, IZ, H3:', IX, IY, IZ, M%HS(IX,IY,IZ)
#endif
                     ENDDO
                  ENDDO
               ENDDO
            ENDIF
         ENDIF

   END SELECT

ENDDO

#ifdef WITH_SCARC_DEBUG
1000 FORMAT (A, ': IX, IY, IZ =', 3I4,': ICU =', I4, ': HP =', E14.6)
#endif
END SUBROUTINE SCARC_MGM_STORE_SOLUTION


! ----------------------------------------------------------------------------------------------------
!> \brief Copy solution vector in McKeeney-Greengard-Mayo method
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_MGM_COPY(NTYPE)
USE SCARC_POINTERS, ONLY: M, L, MGM
INTEGER, INTENT(IN) :: NTYPE
INTEGER :: NM
INTEGER :: IX, IY, IZ
#ifdef WITH_SCARC_DEBUG
INTEGER :: I, K
#endif

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   M   => MESHES(NM)
   L   => SCARC(NM)%LEVEL(NLEVEL_MIN)
   MGM => L%MGM

   SELECT CASE(NTYPE)

      CASE (NSCARC_MGM_COPY_H1_TO_H3)                     ! Copy H1 to H3
         MGM%H3 = MGM%H1

#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG,*) '---------- COPY H1 TO H3:', TOTAL_PRESSURE_ITERATIONS
      WRITE(MSG%LU_DEBUG,*) 'MGM%H1'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H1(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      WRITE(MSG%LU_DEBUG,*) 'MGM%H3'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H3(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
#endif

      CASE (NSCARC_MGM_COPY_HS_TO_H1)                     ! Copy HS to H1
         MGM%H1 = MGM%HS

#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG,*) '---------- COPY HS TO H1:', TOTAL_PRESSURE_ITERATIONS
      WRITE(MSG%LU_DEBUG,*) 'MGM%H2'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%HS(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      WRITE(MSG%LU_DEBUG,*) 'MGM%H2'
      WRITE(MSG%LU_DEBUG,MSG%CFORM1) ((MGM%H1(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
#endif

      CASE (NSCARC_MGM_COPY_HU_TO_H3)                     ! Copy HU to H3
         MGM%H3 = MGM%HU
         IF (TWO_D) THEN
            DO IZ = 1, L%NZ
               DO IX = 1, L%NX
                  IF (L%IS_SOLID(IX,1,IZ)) MGM%H3(IX, 0:2, IZ) = 0.0_EB
               ENDDO
            ENDDO
         ELSE
            DO IZ = 1, L%NZ
               DO IY = 1, L%NY
                  DO IX = 1, L%NX
                     IF (L%IS_SOLID(IX,IY,IZ)) MGM%H3(IX, IY, IZ) = 0.0_EB
                  ENDDO
               ENDDO
            ENDDO
         ENDIF

#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG,*) '---------- COPY HU TO H3:', TOTAL_PRESSURE_ITERATIONS
      WRITE(MSG%LU_DEBUG,*) 'MGM%HU'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%HU(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      WRITE(MSG%LU_DEBUG,*) 'MGM%H3'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H3(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
#endif

      CASE (NSCARC_MGM_COPY_HD_TO_H2)                     ! Copy HD to H2
         MGM%H2 = MGM%HD
         IF (TWO_D) THEN
            DO IZ = 1, L%NZ
               DO IX = 1, L%NX
                  IF (L%IS_SOLID(IX,1,IZ)) MGM%H2(IX, 0:2, IZ) = 0.0_EB
               ENDDO
            ENDDO
         ELSE
            DO IZ = 1, L%NZ
               DO IY = 1, L%NY
                  DO IX = 1, L%NX
                     IF (L%IS_SOLID(IX,IY,IZ)) MGM%H2(IX, IY, IZ) = 0.0_EB
                  ENDDO
               ENDDO
            ENDDO
         ENDIF

#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG,*) '---------- COPY HD TO H2:', TOTAL_PRESSURE_ITERATIONS
      WRITE(MSG%LU_DEBUG,*) 'MGM%HD'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%HD(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      WRITE(MSG%LU_DEBUG,*) 'MGM%H2'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H2(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
#endif

      CASE (NSCARC_MGM_COPY_HD_TO_H4)                     ! Copy HD to H2
         MGM%H4 = MGM%HD

#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG,*) '---------- COPY HD TO H4:', TOTAL_PRESSURE_ITERATIONS
      WRITE(MSG%LU_DEBUG,*) 'MGM%HD'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%HD(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      WRITE(MSG%LU_DEBUG,*) 'MGM%H4'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H4(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
#endif

      CASE (NSCARC_MGM_COPY_H2_TO_H4)                     ! Copy H2 to H2
         MGM%H4 = MGM%H2

#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG,*) '---------- COPY H2 TO H4:', TOTAL_PRESSURE_ITERATIONS
      WRITE(MSG%LU_DEBUG,*) 'MGM%H2'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H2(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      WRITE(MSG%LU_DEBUG,*) 'MGM%H4'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H4(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
#endif

      CASE (NSCARC_MGM_COPY_OH1_TO_OH2)                     ! Copy OH1 to OH2
         MGM%OH2 = MGM%OH1

#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG,*) '---------- COPY OH1 TO OH2:', TOTAL_PRESSURE_ITERATIONS
      WRITE(MSG%LU_DEBUG,*) 'MGM%OH1'
      WRITE(MSG%LU_DEBUG,MSG%CFORM1) (MGM%OH1(I), I=1, L%N_WALL_CELLS_EXT)
      WRITE(MSG%LU_DEBUG,*) 'MGM%OH2'
      WRITE(MSG%LU_DEBUG,MSG%CFORM1) (MGM%OH2(I), I=1, L%N_WALL_CELLS_EXT)
#endif

      CASE (NSCARC_MGM_DIFF_H2_VS_HD)                     ! Copy HD to HS
         MGM%H7 = MGM%H2 - MGM%HD

#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG,*) '---------- COMPARING H2_VS_HD:', TOTAL_PRESSURE_ITERATIONS
      WRITE(MSG%LU_DEBUG,*) 'MGM%H2'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H2(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      WRITE(MSG%LU_DEBUG,*) 'MGM%HD'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%HD(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      WRITE(MSG%LU_DEBUG,*) 'MGM%H7_H2_VS_HD'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H7(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
#endif

      CASE (NSCARC_MGM_DIFF_H3_VS_HU)                     ! Copy HD to HS
         MGM%H6 = MGM%H3 - MGM%HU

#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG,*) '---------- COMPARING H3_VS_HU:', TOTAL_PRESSURE_ITERATIONS
      WRITE(MSG%LU_DEBUG,*) 'MGM%H3'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H3(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      WRITE(MSG%LU_DEBUG,*) 'MGM%HU'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%HU(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
      WRITE(MSG%LU_DEBUG,*) 'MGM%H6_H3_VS_HU'
      WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H6(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
#endif

   END SELECT

ENDDO

END SUBROUTINE SCARC_MGM_COPY

! ---------------------------------------------------------------------------------------------------------------
!> \brief Set internal boundary conditions for unstructured, homogeneous part of McKeeney-Greengard-Mayo method
! ---------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MGM_WORKSPACE(NL)
USE SCARC_POINTERS, ONLY: M, L, MGM
INTEGER, INTENT(IN) :: NL
INTEGER  :: NM

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   M   => MESHES(NM)
   L   => SCARC(NM)%LEVEL(NL)
   MGM => L%MGM

   IF (PREDICTOR) THEN
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'MGM_SETUP_WORKSPACE: PREDICTOR'
#endif
      MGM%U1 = M%U
      MGM%V1 = M%V
      MGM%W1 = M%W
   ELSE
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'MGM_SETUP_WORKSPACE: CORRECTOR'
#endif
      MGM%U1 = M%US
      MGM%V1 = M%VS
      MGM%W1 = M%WS
   ENDIF

   MGM%H1 = 0.0_EB
   !MGM%H2 = 0.0_EB
   MGM%H3 = 0.0_EB
   !IF (TYPE_MGM_BC == NSCARC_MGM_BC_EXPOL) MGM%H4 = 0.0_EB
   MGM%H5 = 0.0_EB
   MGM%H6 = 0.0_EB
   MGM%H7 = 0.0_EB

   MGM%HS = 0.0_EB
   MGM%HU = 0.0_EB
   MGM%HD = 0.0_EB

ENDDO

END SUBROUTINE SCARC_SETUP_MGM_WORKSPACE


! ---------------------------------------------------------------------------------------------------------------
!> \brief Set interface boundary conditions for unstructured, homogeneous part of McKeeney-Greengard-Mayo method
! ---------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MGM_INTERFACES(NM, NL)
USE SCARC_POINTERS, ONLY: L, G, F, MGM, GWC, OL, OG, OH1, OH2, H2, ST, SCARC_POINT_TO_OTHER_GRID
INTEGER, INTENT(IN) :: NM, NL
INTEGER :: IW, I, J, K, IOR0, IFACE, INBR, NOM, ICG, IC, IWG, ICW, ITYPE
REAL(EB) :: VAL, HB(-3:3) = 0.0_EB

ITYPE = TYPE_MGM_BC
IF (TYPE_MGM_BC == NSCARC_MGM_BC_EXPOL .AND. TOTAL_PRESSURE_ITERATIONS <= 2) ITYPE = NSCARC_MGM_BC_MEAN

MGM => L%MGM

SELECT CASE (ITYPE)

   !
   ! -------------------------- Simple time delayed mean value
   !
   CASE (NSCARC_MGM_BC_MEAN)

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '%%%%%%%%%%%%%%%%% START MEAN SETTING:', TOTAL_PRESSURE_ITERATIONS
   WRITE(MSG%LU_DEBUG,*) 'MGM%H2'
   WRITE(MSG%LU_DEBUG,MSG%CFORM1) ((MGM%H2(I,1,K), I=1, L%NX), K=L%NZ,1,-1)
   WRITE(MSG%LU_DEBUG,*) 'MGM%OH1'
   WRITE(MSG%LU_DEBUG,MSG%CFORM1) (MGM%OH1(I), I=1, L%N_WALL_CELLS_EXT)
#endif
      MGM%BC = 0.0_EB
      DO IW = 1, L%N_WALL_CELLS_EXT
      
         GWC => G%WALL(IW)
         IF (GWC%BTYPE /= INTERNAL) CYCLE
         
         I = GWC%IXW
         J = GWC%IYW
         K = GWC%IZW
         
         IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE
         
         IOR0 = GWC%IOR
         IF (IOR0 == 0) CYCLE
         F => SCARC(NM)%LEVEL(NL)%FACE(IOR0)
      
         MGM%BC(IW) = 0.5_EB * (MGM%H2(I, J, K) + MGM%OH1(IW)) 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,2000) IOR0, IW, I,J,K, MGM%H2(I,J,K), MGM%OH1(IW), MGM%BC(IW) , F%SCAL_DIRICHLET*MGM%BC(IW)
#endif

         IC = G%CELL_NUMBER(I,J,K)
         ST%B(IC) = ST%B(IC) + F%SCAL_DIRICHLET * MGM%BC(IW)

      ENDDO

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '%%%%%%%%%%%%%%%%% END MEAN SETTING:', TOTAL_PRESSURE_ITERATIONS
   WRITE(MSG%LU_DEBUG,*) 'MGM%BC'
   WRITE(MSG%LU_DEBUG,MSG%CFORM1) (MGM%BC(I), I=1, L%N_WALL_CELLS_EXT)
   WRITE(MSG%LU_DEBUG,*) 'ST%B'
   WRITE(MSG%LU_DEBUG,MSG%CFORM1) (ST%B(I), I=1, G%NC)
#endif

   !
   ! -------------------------- Linear Extrapolation in time
   !
   CASE (NSCARC_MGM_BC_EXPOL)
   
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '%%%%%%%%%%%%%%%%% EXPOL SETTING:', TOTAL_PRESSURE_ITERATIONS
   WRITE(MSG%LU_DEBUG,*) 'MGM%H2'
   WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H2(I,1,K), I=0, L%NX+1), K=L%NZ+1,0,-1)
   WRITE(MSG%LU_DEBUG,*) 'MGM%H4'
   WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H4(I,1,K), I=0, L%NX+1), K=L%NZ+1,0,-1)
   WRITE(MSG%LU_DEBUG,*) 'MGM%OH1'
   WRITE(MSG%LU_DEBUG,MSG%CFORM1) (MGM%OH1(I), I=1, L%N_WALL_CELLS_EXT)
   WRITE(MSG%LU_DEBUG,*) 'MGM%OH2'
   WRITE(MSG%LU_DEBUG,MSG%CFORM1) (MGM%OH2(I), I=1, L%N_WALL_CELLS_EXT)
#endif
      MGM%BC = 0.0_EB
      DO IW = 1, L%N_WALL_CELLS_EXT
      
         GWC => G%WALL(IW)
         IF (GWC%BTYPE /= INTERNAL) CYCLE
         
         I = GWC%IXW
         J = GWC%IYW
         K = GWC%IZW
         
         IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE
         
         IOR0 = GWC%IOR
         IF (IOR0 == 0) CYCLE
         F => SCARC(NM)%LEVEL(NL)%FACE(IOR0)
      
         MGM%BC(IW) = MGM%H2(I, J, K) + MGM%OH1(IW) - 0.5_EB*(MGM%H4(I, J, K) + MGM%OH2(IW))  
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,2100) IOR0, IW, I,J,K, MGM%H2(I,J,K), MGM%OH1(IW), MGM%H4(I,J,K), MGM%OH2(IW), &
                         MGM%BC(IW) , F%SCAL_DIRICHLET*MGM%BC(IW)
#endif

         IC = G%CELL_NUMBER(I,J,K)
         ST%B(IC) = ST%B(IC) + F%SCAL_DIRICHLET * MGM%BC(IW)

      ENDDO

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '%%%%%%%%%%%%%%%%% END EXPOL SETTING:', TOTAL_PRESSURE_ITERATIONS
   WRITE(MSG%LU_DEBUG,*) 'MGM%BC'
   WRITE(MSG%LU_DEBUG,MSG%CFORM1) (MGM%BC(I), I=1, L%N_WALL_CELLS_EXT)
#endif

   !
   ! -------------------------- True (approximate) solution
   !
   CASE (NSCARC_MGM_BC_TRUE)

      H2  => MGM%H2
      OH1 => MGM%OH1
      OH2 => MGM%OH2

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '%%%%%%%%%%%%%%%%% START TRUE SETTING:', TOTAL_PRESSURE_ITERATIONS
   WRITE(MSG%LU_DEBUG,*) 'MGM%H2'
   WRITE(MSG%LU_DEBUG,MSG%CFORM1) ((MGM%H2(I,1,K), I=1, L%NX), K=L%NZ,1,-1)
   WRITE(MSG%LU_DEBUG,*) 'MGM%OH1'
   WRITE(MSG%LU_DEBUG,MSG%CFORM1) (MGM%OH1(I), I=1, L%N_WALL_CELLS_EXT)
   WRITE(MSG%LU_DEBUG,*) 'MGM%OH2'
   WRITE(MSG%LU_DEBUG,MSG%CFORM1) (MGM%OH2(I), I=1, L%N_WALL_CELLS_EXT)
#endif
      MGM%BC = 0.0_EB
      MGM_TRUE_FACE_LOOP: DO IFACE = 1, 6
         IOR0 = FACE_ORIENTATION(IFACE)
         F => L%FACE(IOR0)
         MGM_TRUE_NBR_LOOP: DO INBR = 1, F%N_NEIGHBORS
      
            NOM = F%NEIGHBORS(INBR)
            CALL SCARC_POINT_TO_OTHER_GRID(NM, NOM, NL)
      
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '------ IOR0: GHOST_FIRSTW, GHOST_LASTW:', IOR0, OL%GHOST_FIRSTW(IOR0), OL%GHOST_LASTW(IOR0)
#endif
            MGM_TRUE_FACECELL_LOOP: DO ICG = OL%GHOST_FIRSTW(IOR0), OL%GHOST_LASTW(IOR0)

               IWG = OG%ICG_TO_IWG(ICG)
               ICW = OG%ICG_TO_ICW(ICG, 1)

               I = G%ICX(ICW) 
               J = G%ICY(ICW) 
               K = G%ICZ(ICW) 

               IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '          IWG, ICW, I, J, K :', IWG, ICW, I, J, K
#endif
               HB = 0.0_EB
               MGM_TRUE_IOR_SELECT: SELECT CASE (IOR0)

                  ! ---------------------------------------
                  CASE( 1)
                     HB( 1) = 0.5_EB * (OH1(IWG)  + OH2(IWG) )
                     HB(-1) = 0.5_EB * (H2(1,J,K) + H2(2,J,K))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) IOR0, IWG, IWG, -1,  -1, IWG, -1, -1, OH1(IWG),   OH2(IWG),  1, HB( 1)
WRITE(MSG%LU_DEBUG,1000) IOR0, IWG, 11,  J,   K, 2  ,  J,  K, H2(1,J,K), H2(2,J,K), -1, HB(-1)
#endif
                     IF (.NOT.TWO_D) THEN
                        IF (MGM%BTYPE(IWG,2) == INTERNAL) THEN
                           HB( 2) = 0.5_EB * (H2(1,J-1,K) + OH1(IWG+F%INCRS(2)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) IOR0, IWG, I, J-1, K, IWG+F%INCRS(2),-1,-1, H2(1,J-1,K),OH1(IWG+F%INCRS(2)) , 3, HB(2)
#endif
                        ENDIF
                        IF (MGM%BTYPE(IWG,-2) == INTERNAL) THEN
                           HB(-2) = 0.5_EB * (H2(1,J+1,K) + OH1(IWG+F%INCRS(-2)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) IOR0, IWG, I, J-1, K, IWG+F%INCRS(-2),-1,-1, H2(1,J-1,K),OH1(IWG+F%INCRS(-2)) , 3, HB(2)
#endif
                        ENDIF
                     ENDIF
                     IF (MGM%BTYPE(IWG, 3) == INTERNAL) THEN
                        HB( 3) = 0.5_EB * (H2(1,J,K-1) + OH1(IWG+F%INCRS(3)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) IOR0, IWG, I, J, K-1, IWG+F%INCRS(3),-1,-1, H2(1,J,K-1), OH1(IWG+F%INCRS(3)), 3, HB(3)
#endif
                     ENDIF
                     IF (MGM%BTYPE(IWG,-3) == INTERNAL) THEN
                        HB(-3) = 0.5_EB *(H2(1,J,K+1) + OH1(IWG+F%INCRS(-3)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) IOR0, IWG, I, J,  K+1, IWG+F%INCRS(-3), 1, -1, H2(1,J,K+1), OH1(IWG+F%INCRS(-3)), -3, HB(-3)
#endif
                     ENDIF


                  ! ---------------------------------------
                  CASE(-1)
                     HB( 1) = 0.5_EB * (H2(I-1,J,K) + H2(I,J,K))
                     HB(-1) = 0.5_EB * (OH1(IWG)    + OH2(IWG) )
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) IOR0, IWG, I-1, J, K, I, J, K, H2(I-1,J,K), H2(I,J,K), 1, HB(1)
WRITE(MSG%LU_DEBUG,1000) IOR0, IWG, IWG, -1,  -1, IWG, -1, -1, OH1(IWG), OH2(IWG), -1, HB(-1)
#endif
                     IF (.NOT.TWO_D) THEN
                        IF (MGM%BTYPE(IWG,2) == INTERNAL) THEN
                           HB( 2) = 0.5_EB * (H2(I,J-1,K) + OH1(IWG+F%INCRS(2)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) IOR0, IWG, I, J-1, K, IWG+F%INCRS(2), H2(I,J-1,K), OH1(IWG+F%INCRS(2)), 3, HB(2)
#endif
                        ENDIF
                        IF (MGM%BTYPE(IWG,-2) == INTERNAL) THEN
                           HB(-2) = 0.5_EB * (H2(I,J+1,K) + OH1(IWG+F%INCRS(-2)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) IOR0, IWG, I, J-1, K, IWG+F%INCRS(-2), H2(I,J-1,K), OH1(IWG+F%INCRS(-2)), 3, HB(2)
#endif
                        ENDIF
                     ENDIF
                     IF (MGM%BTYPE(IWG, 3) == INTERNAL) THEN   
                        HB( 3) = 0.5_EB * (H2(I,J,K-1) + OH1(IWG+F%INCRS(3)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) IOR0, IWG, I, J, K-1, IWG+F%INCRS(3), -1, -1, H2(I,J,K-1), OH1(IWG+F%INCRS(3)),  3, HB(3)
#endif
                     ENDIF
                     IF (MGM%BTYPE(IWG, -3) == INTERNAL) THEN  
                        HB(-3) = 0.5_EB * (H2(I,J,K+1) + OH1(IWG+F%INCRS(-3)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) IOR0, IWG, I, J, K+1, IWG+F%INCRS(-3), -1, -1, H2(I,J,K+1), OH1(IWG+F%INCRS(-3)), -3, HB(-3)
#endif
                     ENDIF

                  ! ---------------------------------------
                  CASE( 3)
                     WRITE(*,*) 'Not yet done'
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) ' 3: Not yet done'
#endif
                  ! ---------------------------------------
                  CASE(-3)
                     WRITE(*,*) 'Not yet done'
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '-3: Not yet done'
#endif
               END SELECT MGM_TRUE_IOR_SELECT

               IF (TWO_D) THEN
                  VAL = (L%DXI2*(HB(1)+HB(-1)) + L%DZI2*(HB(3)+HB(-3))) * MGM%WEIGHT(IWG)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,I4,5E14.6)') 'IWG,HB(1),HB(-1),HB(3),HB(-3),WEIGHT BC:',&
        IWG,HB(1),HB(-1),HB(3),HB(-3),MGM%WEIGHT(IWG)
#endif
               ELSE
                  VAL = (L%DXI2*(HB(1)+HB(-1)) + L%DYI2*(HB(2)+HB(-2)) + L%DZI2*(HB(3)+HB(-3))) * MGM%WEIGHT(IWG)
               ENDIF

               MGM%BC(IWG) = VAL

               IC = G%CELL_NUMBER(I,J,K)
               ST%B(IC) = ST%B(IC) + F%SCAL_DIRICHLET * VAL
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,3000) IOR0, IWG, I,J,K, MGM%H2(I,J,K), MGM%OH1(IWG), MGM%OH2(IWG), MGM%BC(IWG) , F%SCAL_DIRICHLET*MGM%BC(IWG)
#endif
            ENDDO MGM_TRUE_FACECELL_LOOP

         ENDDO MGM_TRUE_NBR_LOOP
      ENDDO MGM_TRUE_FACE_LOOP

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '%%%%%%%%%%%%%%%%% END TRUE SETTING:', TOTAL_PRESSURE_ITERATIONS
   WRITE(MSG%LU_DEBUG,*) 'MGM%BC'
   WRITE(MSG%LU_DEBUG,MSG%CFORM1) (MGM%BC(I), I=1, L%N_WALL_CELLS_EXT)
#endif


   !
   ! -------------------------------------------- MGM - Taylor
   !
   CASE (NSCARC_MGM_BC_TAYLOR)
   
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '%%%%%%%%%%%%%%%%% TAYLOR SETTING:', TOTAL_PRESSURE_ITERATIONS
#endif
      DO IW = 1, L%N_WALL_CELLS_EXT
      
         GWC => G%WALL(IW)
         IF (GWC%BTYPE /= INTERNAL) CYCLE
         
         I = GWC%IXW
         J = GWC%IYW
         K = GWC%IZW
         
         IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE
         IOR0 = GWC%IOR
         IF (IOR0 == 0) CYCLE
         F => SCARC(NM)%LEVEL(NL)%FACE(IOR0)
      
         SELECT CASE (IOR0)
            CASE(1)
               VAL = -L%DXI2 * (MGM%H2(I, J, K) - MGM%H2(I-1, J, K))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,4i4,3E14.6)') 'MGM-BC: TAYLOR: IOR =  1: IW,I,J,K, HPM, HPP, VAL, VAL2: ', &
                     IW, I,J,K, MGM%H2(I-1,J,K), MGM%H2(I,J,K), VAL
#endif
            CASE(-1)
               VAL =  L%DXI2 * (MGM%H2(I+1, J, K) - MGM%H2(I, J, K))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,4i4,3E14.6)') 'MGM-BC: TAYLOR: IOR = -1: IW,I,J,K, HPM, HPP, VAL, VAL2: ', &
                     IW, I,J,K, MGM%H2(I,J,K), MGM%H2(I+1,J,K), VAL
#endif
            CASE(2)
               VAL = -L%DYI2 * (MGM%H2(I, J, K) - MGM%H2(I, J-1, K))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,4i4,3E14.6)') 'MGM-BC: TAYLOR: IOR =  2: IW,I,J,K, HPM, HPP, VAL, VAL2: ', &
                     IW, I,J,K, MGM%H2(I,J-1,K), MGM%H2(I,J,K), VAL
#endif
            CASE(-2)
               VAL =  L%DYI2 * (MGM%H2(I, J+1, K) - MGM%H2(I, J, K))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,4i4,3E14.6)') 'MGM-BC: TAYLOR: IOR = -2: IW,I,J,K, HPM, HPP, VAL, VAL2: ', &
                     IW, I,J,K, MGM%H2(I,J,K), MGM%H2(I,J+1,K), VAL
#endif
            CASE(3)
               VAL = -L%DZI2 * (MGM%H2(I, J, K) - MGM%H2(I, J, K-1))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,4i4,3E14.6)') 'MGM-BC: TAYLOR: IOR =  3: IW,I,J,K, HPM, HPP, VAL, VAL2: ', &
                     IW, I,J,K, MGM%H2(I,J,K-1), MGM%H2(I,J,K), VAL
#endif
            CASE(-3)
               VAL =  +L%DZI2 * (MGM%H2(I, J, K+1) - MGM%H2(I, J, K))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,4i4,3E14.6)') 'MGM-BC: TAYLOR: IOR = -3: IW,I,J,K, HPM, HPP, VAL, VAL2: ', &
                     IW, I,J,K, MGM%H2(I,J,K), MGM%H2(I,J,K+1), VAL
#endif
         END SELECT

         IC = G%CELL_NUMBER(I,J,K)
         ST%B(IC) =  VAL

      ENDDO

END SELECT

#ifdef WITH_SCARC_DEBUG
1000 FORMAT('MGM-BC: TRUE: IOR =  ', I4, ': IW: ',I3,': SET1: ', 3I3,': SET2: ', 3I3,': V1,V2: ', 2E14.6,&
            ' : HB(',i3,')=', E14.6, ': W: ',E14.6)
2000 FORMAT('MGM-BC: MEAN: IOR = ',I4,' : IW =', I4,' : I,J,K =', 3I4,': H, OH, BC , SCAL*BC = ', 4E14.6)
2100 FORMAT('MGM-BC: EXPOL: IOR = ',I4,' : IW =', I4,' : I,J,K =', 3I4,': H, OH, H-1, OH-1, BC , SCAL*BC = ', 6E14.6)
3000 FORMAT('MGM-BC: TRUE: IOR = ',I4,' : IW =', I4,' : I,J,K =', 3I4,': H, OH1, OH2, BC , SCAL*BC = ', 5E14.6)
#endif
END SUBROUTINE SCARC_SETUP_MGM_INTERFACES


! ---------------------------------------------------------------------------------------------------------------
!> \brief Set BC's along internal obstructions for MGM method
! ---------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MGM_OBSTRUCTIONS
USE SCARC_POINTERS, ONLY: L, G, MGM, ST, UU, VV, WW, GWC
INTEGER :: IW, I, J, K, IOR0, IC
REAL(EB) :: VAL

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '%%%%%%%%%%%%%%%%% MGM-BC: OBSTRUCTION:', TOTAL_PRESSURE_ITERATIONS
   WRITE(MSG%LU_DEBUG,*) 'MGM%U1'
   WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MGM%U1(I,1,K), I=0, L%NX), K=L%NZ,1,-1)
   WRITE(MSG%LU_DEBUG,*) 'MGM%U2'
   WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MGM%U2(I,1,K), I=0, L%NX), K=L%NZ,1,-1)
   WRITE(MSG%LU_DEBUG,*) 'MGM%U3'
   WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MGM%U3(I,1,K), I=0, L%NX), K=L%NZ,1,-1)
#endif

DO IW = L%N_WALL_CELLS_EXT+1, L%N_WALL_CELLS_EXT + L%N_WALL_CELLS_INT
   
   GWC => G%WALL(IW)
   UU  => MGM%U1
   VV  => MGM%V1
   WW  => MGM%W1
   
   I = GWC%IXW
   J = GWC%IYW
   K = GWC%IZW
   
   IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE
   
   IOR0 = GWC%IOR
   IC   = G%CELL_NUMBER(I,J,K)
   
   SELECT CASE (IOR0)
      CASE(1)
         VAL =  L%DXI * DTI * UU(I-1,J,K)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,5i4,2E14.6)') 'MGM-BC: OBSTRUCTION: IOR =  1: IW,I,J,K,IC,UU(I-1,J,K),B(IC):', &
                                      IW,I-1,J,K, IC, UU(I-1,J,K), VAL
#endif
      CASE(-1)
         VAL = -L%DXI * DTI * UU(I,J,K)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,5i4,2E14.6)') 'MGM-BC: OBSTRUCTION: IOR = -1: IW,I,J,K,IC,UU(I,J,K),  B(IC):', &
                                      IW,I,J,K, IC, UU(I,J,K), VAL
#endif
      CASE(2)
         VAL =  L%DYI * DTI * VV(I,J-1,K)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,5i4,2E14.6)') 'MGM-BC: OBSTRUCTION: IOR =  2: IW,I,J,K,IC,VV(I,J-1,K),B(IC):', &
                                      IW,I,J-1,K, IC, VV(I,J-1,K), VAL
#endif
      CASE(-2)
         VAL = -L%DYI * DTI * VV(I,J,K)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,5i4,2E14.6)') 'MGM-BC: OBSTRUCTION: IOR = -2: IW,I,J,K,IC,VV(I,J,K),  B(IC):', &
                                      IW,I,J,K, IC, VV(I,J,K), VAL
#endif
      CASE(3)
         VAL =  L%DZI * DTI * WW(I,J,K-1)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,5i4,2E14.6)') 'MGM-BC: OBSTRUCTION: IOR =  3: IW,I,J,K,IC,WW(I,J,K-1),B(IC):', &
                                      IW,I,J,K-1, IC, WW(I,J,K-1), VAL
#endif
      CASE(-3)
         VAL = -L%DZI * DTI * WW(I,J,K)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,5i4,2E14.6)') 'MGM-BC: OBSTRUCTION: IOR = -3: IW,I,J,K,IC,WW(I,J,K),  B(IC):', &
                                      IW,I,J,K, IC, WW(I,J,K), VAL
#endif
   END SELECT

   !IF (BFIRST_WORKSPACE) ST%B(IC) = ST%B(IC) + VAL                 ! Variant A
   ST%B(IC) = ST%B(IC) + VAL                                    ! Variant B
   
ENDDO

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'ST%B'
   WRITE(MSG%LU_DEBUG,MSG%CFORM1) (ST%B(IC), IC=1, G%NC)
#endif

END SUBROUTINE SCARC_SETUP_MGM_OBSTRUCTIONS


! ---------------------------------------------------------------------------------------------------------------
!> \brief Set internal boundary conditions for unstructured, homogeneous part of McKeeney-Greengard-Mayo method
! ---------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_MGM_UPDATE_VELOCITY(NTYPE)
USE SCARC_POINTERS, ONLY: M, L, G, GWC, MGM, UU, VV, WW, HP
INTEGER, INTENT(IN) :: NTYPE
INTEGER  :: NM, I, J, K, IW, IOR0

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   M   => MESHES(NM)
   L   => SCARC(NM)%LEVEL(NLEVEL_MIN)
   MGM => L%MGM

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '=======================> START OF UMPDATE_VELOCITY:'
   WRITE(MSG%LU_DEBUG,*) 'MGM%U1'
   WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MGM%U1(I,1,K), I=0, M%IBAR), K=M%KBAR,1,-1)
   WRITE(MSG%LU_DEBUG,*) 'MGM%W1'
   WRITE(MSG%LU_DEBUG,MSG%CFORM1) ((MGM%W1(I,1,K), I=1, M%IBAR), K=M%KBAR,0,-1)
   WRITE(MSG%LU_DEBUG,*) 'MGM%U2'
   WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MGM%U2(I,1,K), I=0, M%IBAR), K=M%KBAR,1,-1)
   WRITE(MSG%LU_DEBUG,*) 'MGM%W2'
   WRITE(MSG%LU_DEBUG,MSG%CFORM1) ((MGM%W2(I,1,K), I=1, M%IBAR), K=M%KBAR,0,-1)
   WRITE(MSG%LU_DEBUG,*) 'MGM%U3'
   WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MGM%U3(I,1,K), I=0, M%IBAR), K=M%KBAR,1,-1)
   WRITE(MSG%LU_DEBUG,*) 'MGM%W3'
   WRITE(MSG%LU_DEBUG,MSG%CFORM1) ((MGM%W3(I,1,K), I=1, M%IBAR), K=M%KBAR,0,-1)
#endif
   MGM_PART_SELECT: SELECT CASE (NTYPE)

      !
      ! ------------- Update velocity with new information after Poisson pass 
      !
      CASE (NSCARC_MGM_POISSON)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '================================ UPDATE-VELOCITY-POISSON:P: ', PREDICTOR, M%IBAR, M%JBAR, M%KBAR, DT
#endif
         HP => MGM%H1
         IF (PREDICTOR) THEN
            UU => M%U
            VV => M%V
            WW => M%W
         ELSE
            UU => M%US
            VV => M%VS
            WW => M%WS
         ENDIF

         DO K=1,M%KBAR
            DO J=1,M%JBAR
               DO I=0,M%IBAR
                  MGM%U1(I,J,K) = UU(I,J,K) - DT*( M%FVX(I,J,K) + M%RDXN(I)*(HP(I+1,J,K)-HP(I,J,K) ))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,3i4,6E14.6)') 'VELO X:P: ',I,J,K,UU(I,J,K), M%FVX(I,J,K), HP(I+1,J,K), HP(I,J,K), UU(I,J,K), MGM%U1(I,J,K)
#endif
               ENDDO
            ENDDO
         ENDDO
            
         DO K=1,M%KBAR
            DO J=0,M%JBAR
               DO I=1,M%IBAR
                  MGM%V1(I,J,K) = VV(I,J,K) - DT*( M%FVY(I,J,K) + M%RDYN(J)*(HP(I,J+1,K)-HP(I,J,K) ))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,3i4,6E14.6)') 'VELO Y:P: ',I,J,K,VV(I,J,K), M%FVY(I,J,K), HP(I,J+1,K), HP(I,J,K), VV(I,J,K), MGM%V1(I,J,K)
#endif
               ENDDO
            ENDDO
         ENDDO
            
         DO K=0,M%KBAR
            DO J=1,M%JBAR
               DO I=1,M%IBAR
                  MGM%W1(I,J,K) = WW(I,J,K) - DT*( M%FVZ(I,J,K) + M%RDZN(K)*(HP(I,J,K+1)-HP(I,J,K) ))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,3i4,6E14.6)') 'VELO Z:P: ',I,J,K,WW(I,J,K), M%FVZ(I,J,K), HP(I,J,K+1), HP(I,J,K), WW(I,J,K), MGM%W1(I,J,K)
#endif
               ENDDO
            ENDDO
         ENDDO
            
         MGM%U3 = MGM%U1
         MGM%V3 = MGM%V1
         MGM%W3 = MGM%W1 

      !
      ! ------------- Update velocity with new information after Laplace pass
      !
      CASE (NSCARC_MGM_LAPLACE)

         HP => MGM%H2
            
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '================================ UPDATE-VELOCITY-LAPLACE:P: ', PREDICTOR, M%IBAR, M%JBAR, M%KBAR, DT
   WRITE(MSG%LU_DEBUG,*) 'MGM%H1'
   WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H1(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
   WRITE(MSG%LU_DEBUG,*) 'MGM%H2'
   WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H2(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
   WRITE(MSG%LU_DEBUG,*) 'MGM%H3'
   WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((MGM%H3(I,1,K), I=0, M%IBAR+1), K=M%KBAR+1,0,-1)
WRITE(MSG%LU_DEBUG,*) '------------------------------------------------------------'
#endif
            
         DO K=1,M%KBAR
            DO J=1,M%JBAR
               DO I=0,M%IBAR
                  MGM%U2(I,J,K) = - DT * M%RDXN(I)*(HP(I+1,J,K)-HP(I,J,K))
#ifdef WITH_SCARC_DEBUG
IF (J==1) WRITE(MSG%LU_DEBUG,'(A,3i4,4E14.6)') 'VELO X:L: ',I,J,K,HP(I+1,J,K), HP(I,J,K), HP(I+1,J,K)-HP(I,J,K), MGM%U2(I,J,K)
#endif
               ENDDO
            ENDDO
         ENDDO

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '------------------------------------------------------------'
#endif
            
         DO K=1,M%KBAR
            DO J=0,M%JBAR
               DO I=1,M%IBAR
                  MGM%V2(I,J,K) = - DT * M%RDYN(J)*(HP(I,J+1,K)-HP(I,J,K))
#ifdef WITH_SCARC_DEBUG
IF (J==1) WRITE(MSG%LU_DEBUG,'(A,3i4,4E14.6)') 'VELO Y:L: ',I,J,K,HP(I,J+1,K), HP(I,J,K), HP(I,J+1,K)-HP(I,J,K), MGM%V2(I,J,K)
#endif
               ENDDO
            ENDDO
         ENDDO
            
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '------------------------------------------------------------'
#endif

         DO K=0,M%KBAR
            DO J=1,M%JBAR
               DO I=1,M%IBAR
                  MGM%W2(I,J,K) = - DT * M%RDZN(K)*(HP(I,J,K+1)-HP(I,J,K))
#ifdef WITH_SCARC_DEBUG
IF (J==1) WRITE(MSG%LU_DEBUG,'(A,3i4,4E14.6)') 'VELO Z:L: ',I,J,K,HP(I,J,K+1), HP(I,J,K), HP(I,J,K+1)-HP(I,J,K), MGM%W2(I,J,K)
#endif
               ENDDO
            ENDDO
         ENDDO

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '------------------------------------------------------------'
#endif
            
         ! Recompute velocities on obstruction cells, such that correct normal derivative of Laplace solution is used 
         DO IW = L%N_WALL_CELLS_EXT + 1, L%N_WALL_CELLS_EXT + L%N_WALL_CELLS_INT

            GWC => G%WALL(IW)
            IF (GWC%BOUNDARY_TYPE /= SOLID_BOUNDARY) CYCLE

            IOR0 = GWC%IOR
            I = GWC%IXW
            J = GWC%IYW
            K = GWC%IZW
            
            SELECT CASE(IOR0)
               CASE( 1)
                  MGM%U2(I-1,J,K) = - MGM%U1(I-1,J,K)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,4I4,E14.6)') 'VELO Y:L: NOGRAD:',IOR0, I-1,J,K,MGM%U2(I-1,J,K)
#endif
               CASE(-1)
                  MGM%U2(I,J,K)   = - MGM%U1(I,J,K) 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,4I4,E14.6)') 'VELO Y:L: NOGRAD:',IOR0, I,J,K,MGM%U2(I,J,K)
#endif
               CASE( 2)
                  MGM%V2(I,J-1,K) = - MGM%V1(I,J-1,K) 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,4I4,E14.6)') 'VELO Y:L: NOGRAD:',IOR0, I,J-1,K,MGM%V2(I,J-1,K)
#endif
               CASE(-2)
                  MGM%V2(I,J,K)   = - MGM%V1(I,J,K) 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,4I4,E14.6)') 'VELO Y:L: NOGRAD:',IOR0, I,J,K,MGM%V2(I,J,K)
#endif
               CASE( 3)
                  MGM%W2(I,J,K-1) = - MGM%W1(I,J,K-1) 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,4I4,E14.6)') 'VELO Y:L: NOGRAD:',IOR0, I,J,K-1,MGM%W2(I,J,K-1)
#endif
               CASE(-3)
                  MGM%W2(I,J,K)   = - MGM%W1(I,J,K) 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,4I4,E14.6)') 'VELO Y:L: NOGRAD:',IOR0, I,J,K,MGM%W2(I,J,K)
#endif
            END SELECT

         ENDDO

         MGM%U3 = MGM%U1 + MGM%U2
         MGM%V3 = MGM%V1 + MGM%V2
         MGM%W3 = MGM%W1 + MGM%W2

   END SELECT MGM_PART_SELECT
ENDDO

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '=======================> END OF UMPDATE_VELOCITY:'
WRITE(MSG%LU_DEBUG,*) 'MGM%U1'
WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MGM%U1(I,1,K), I=0, M%IBAR), K=M%KBAR,1,-1)
WRITE(MSG%LU_DEBUG,*) 'MGM%W1'
WRITE(MSG%LU_DEBUG,MSG%CFORM1) ((MGM%W1(I,1,K), I=1, M%IBAR), K=M%KBAR,0,-1)
WRITE(MSG%LU_DEBUG,*) '------------------------------------------------------------'
WRITE(MSG%LU_DEBUG,*) 'MGM%U2'
WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MGM%U2(I,1,K), I=0, M%IBAR), K=M%KBAR,1,-1)
WRITE(MSG%LU_DEBUG,*) 'MGM%W2'
WRITE(MSG%LU_DEBUG,MSG%CFORM1) ((MGM%W2(I,1,K), I=1, M%IBAR), K=M%KBAR,0,-1)
WRITE(MSG%LU_DEBUG,*) '------------------------------------------------------------'
WRITE(MSG%LU_DEBUG,*) 'MGM%U3'
WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MGM%U3(I,1,K), I=0, M%IBAR), K=M%KBAR,1,-1)
WRITE(MSG%LU_DEBUG,*) 'MGM%W3'
WRITE(MSG%LU_DEBUG,MSG%CFORM1) ((MGM%W3(I,1,K), I=1, M%IBAR), K=M%KBAR,0,-1)
WRITE(MSG%LU_DEBUG,*) '------------------------------------------------------------'
#endif
 
END SUBROUTINE SCARC_MGM_UPDATE_VELOCITY


! ---------------------------------------------------------------------------------------------------------------
!> \brief Set internal boundary conditions for unstructured, homogeneous part of McKeeney-Greengard-Mayo method
! ---------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_MGM_COMPUTE_VELOCITY_ERROR(NTYPE)
USE SCARC_POINTERS, ONLY: M, L, G, MGM, GWC, EWC, HP, UU, VV, WW
INTEGER, INTENT(IN) ::  NTYPE
INTEGER :: NM, I, J, K, IW, IOR0, IIO1, IIO2, JJO1, JJO2, KKO1, KKO2, IIO, JJO, KKO, ITYPE
REAL(EB) :: UN_NEW_OTHER, UN_NEW, DUDT, DVDT, DWDT
#ifdef WITH_SCARC_DEBUG
INTEGER :: III, KKK
#endif
TYPE(MESH_TYPE), POINTER :: M2
TYPE(OMESH_TYPE), POINTER :: OM

MESH_REAL = 0.0_EB                            
RANK_REAL = 0.0_EB
UN_NEW_OTHER = 0.0_EB

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   M   => MESHES(NM)
   L   => SCARC(NM)%LEVEL(NLEVEL_MIN)
   MGM => L%MGM
   IF (NTYPE == NSCARC_MGM_POISSON) THEN
      HP  => MGM%H1
   ELSE
      HP  => MGM%H3
   ENDIF

   MGM%VELOCITY_ERROR = 0.0_EB

   UU => MGM%UU
   VV => MGM%VV
   WW => MGM%WW

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '=======================> XXXX:MGM_VELOCITY_ERROR', NTYPE, DT
   WRITE(MSG%LU_DEBUG,*) 'HP'
   WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((HP(III,1,KKK), III=0, M%IBAR+1), KKK=M%KBAR+1,0,-1)
   WRITE(MSG%LU_DEBUG,*) 'UU'
   WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((UU(III,1,KKK), III=0, M%IBAR), KKK=M%KBAR,0,-1)
   WRITE(MSG%LU_DEBUG,*) 'M%U'
   WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((M%U(III,1,KKK), III=0, M%IBAR), KKK=M%KBAR,0,-1)
   WRITE(MSG%LU_DEBUG,*) 'M%W'
   WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((M%W(III,1,KKK), III=0, M%IBAR), KKK=M%KBAR,0,-1)
   WRITE(MSG%LU_DEBUG,*) 'M%FVX'
   WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((M%FVX(III,1,KKK), III=0, M%IBAR), KKK=M%KBAR,0,-1)
   WRITE(MSG%LU_DEBUG,*) 'M%FVZ'
   WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((M%FVZ(III,1,KKK), III=0, M%IBAR), KKK=M%KBAR,0,-1)
   WRITE(MSG%LU_DEBUG,*) 'L%N_WALL_CELLS_EXT = ', L%N_WALL_CELLS_EXT
   WRITE(MSG%LU_DEBUG,*) 'L%N_WALL_CELLS_INT = ', L%N_WALL_CELLS_INT
#endif

   SELECT CASE (NTYPE)

      !
      ! ------------------------- Poisson case ---------------------------------------------------------
      !
      CASE (NSCARC_MGM_POISSON) 

         WALLCELLS_POISSON_LOOP: DO IW = 1, L%N_WALL_CELLS_EXT + L%N_WALL_CELLS_INT

            GWC => G%WALL(IW)

            IF (GWC%BOUNDARY_TYPE /= SOLID_BOUNDARY         .AND. &
                GWC%BOUNDARY_TYPE /= INTERPOLATED_BOUNDARY) CYCLE

            IOR0 = GWC%IOR

            I = GWC%IXG
            J = GWC%IYG
            K = GWC%IZG

            ! Update normal component of velocity at the mesh boundary

            SELECT CASE(IOR0)
               CASE( 1)
                  UN_NEW = M%U(I,J,K)   - DT*(M%FVX(I,J,K)   + M%RDXN(I)  *(HP(I+1,J,K)-HP(I,J,K)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A25,4i4,5E14.6)') 'ERR-P 1: UN_NEW:',IW,I,J,K,UN_NEW,MGM%U1(I,J,K),M%FVX(I,J,K),HP(I+1,J,K),HP(I,J,K)
#endif
               CASE(-1)
                  UN_NEW = M%U(I-1,J,K) - DT*(M%FVX(I-1,J,K) + M%RDXN(I-1)*(HP(I,J,K)-HP(I-1,J,K)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A25,4i4,5E14.6)') 'ERR-P-1: UN_NEW: ',IW,I,J,K,UN_NEW,MGM%U1(I-1,J,K),M%FVX(I-1,J,K),HP(I,J,K),HP(I-1,J,K)
#endif
               CASE( 2)
                  UN_NEW = M%V(I,J,K)   - DT*(M%FVY(I,J,K)   + M%RDYN(J)  *(HP(I,J+1,K)-HP(I,J,K)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A25,4i4,5E14.6)') 'ERR-P 2: UN_NEW: ',IW,I,J,K,UN_NEW,MGM%V1(I,J,K),M%FVY(I,J,K),HP(I,J+1,K),HP(I,J,K)
#endif
               CASE(-2)
                  UN_NEW = M%V(I,J-1,K) - DT*(M%FVY(I,J-1,K) + M%RDYN(J-1)*(HP(I,J,K)-HP(I,J-1,K)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A25,4i4,5E14.6)') 'ERR-P-2: UN_NEW: ',IW,I,J,K,UN_NEW,MGM%V1(I,J-1,K),M%FVY(I,J-1,K),HP(I,J,K),HP(I,J-1,K)
#endif
               CASE( 3)
                  UN_NEW = M%W(I,J,K)   - DT*(M%FVZ(I,J,K)   + M%RDZN(K)  *(HP(I,J,K+1)-HP(I,J,K)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A25,4i4,5E14.6)') 'ERR-P 3: UN_NEW: ',IW,I,J,K,UN_NEW,MGM%W1(I,J,K),M%FVZ(I,J,K),HP(I,J,K+1),HP(I,J,K)
#endif
               CASE(-3)
               UN_NEW = M%W(I,J,K-1) - DT*(M%FVZ(I,J,K-1) + M%RDZN(K-1)*(HP(I,J,K)-HP(I,J,K-1)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A25,4i4,5E14.6)') 'ERR-P-3: UN_NEW: ',IW,I,J,K,UN_NEW,MGM%W1(I,J,K-1),M%FVZ(I,J,K-1),HP(I,J,K),HP(I,J,K-1)
#endif
            END SELECT

            IF (M%WALL(IW)%BOUNDARY_TYPE==INTERPOLATED_BOUNDARY) THEN
         
               UN_NEW_OTHER = 0._EB
         
               EWC => M%EXTERNAL_WALL(IW)

               OM => M%OMESH(EWC%NOM)
               M2 => MESHES(EWC%NOM)

               IIO1 = EWC%IIO_MIN
               JJO1 = EWC%JJO_MIN
               KKO1 = EWC%KKO_MIN
               IIO2 = EWC%IIO_MAX
               JJO2 = EWC%JJO_MAX
               KKO2 = EWC%KKO_MAX
         
               IOR_SELECT_1: SELECT CASE(IOR0)
                  CASE( 1)
                     DO KKO=KKO1,KKO2
                        DO JJO=JJO1,JJO2
                           DO IIO=IIO1,IIO2
                              DUDT = -OM%FVX(IIO,JJO,KKO)   - M2%RDXN(IIO)  *(HP(I+1,J,K)-HP(I,J,K))
                              UN_NEW_OTHER = UN_NEW_OTHER + OM%U(IIO,JJO,KKO)   + DT*DUDT
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A25,4I4,E14.6,3I4)') 'P:PRES: 1: UN_NEW_OTHER ', IW, IIO, JJO, KKO, UN_NEW_OTHER, IIO+1, JJO, KKO
#endif
                           ENDDO
                        ENDDO
                     ENDDO
                  CASE(-1)
                     DO KKO=KKO1,KKO2
                        DO JJO=JJO1,JJO2
                           DO IIO=IIO1,IIO2
                              DUDT = -OM%FVX(IIO-1,JJO,KKO) - M2%RDXN(IIO-1)*(HP(I,J,K)-HP(I-1,J,K))
                              UN_NEW_OTHER = UN_NEW_OTHER + OM%U(IIO-1,JJO,KKO) + DT*DUDT
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A25,4I4,E14.6,3i4)') 'P:PRES:-1: UN_NEW_OTHER ', IW, IIO-1, JJO, KKO, UN_NEW_OTHER, IIO, JJO, KKO
#endif
                           ENDDO
                        ENDDO
                     ENDDO
                  CASE( 2)
                     DO KKO=KKO1,KKO2
                        DO JJO=JJO1,JJO2
                           DO IIO=IIO1,IIO2
                              DVDT = -OM%FVY(IIO,JJO,KKO)   - M2%RDYN(JJO)  *(HP(I,J+1,K)-HP(I,J,K))
                              UN_NEW_OTHER = UN_NEW_OTHER + OM%V(IIO,JJO,KKO)   + DT*DVDT
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A25,4i4,E14.6,3i4)') 'P:PRES: 2: UN_NEW_OTHER ', IW, IIO, JJO, KKO, UN_NEW_OTHER, IIO, JJO+1, KKO
#endif
                           ENDDO
                        ENDDO
                     ENDDO
                  CASE(-2)
                     DO KKO=KKO1,KKO2
                        DO JJO=JJO1,JJO2
                           DO IIO=IIO1,IIO2
                              DVDT = -OM%FVY(IIO,JJO-1,KKO) - M2%RDYN(JJO-1)*(HP(I,J,K)-HP(I,J-1,K))
                              UN_NEW_OTHER = UN_NEW_OTHER + OM%V(IIO,JJO-1,KKO) + DT*DVDT
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A25,4i4,E14.6,3i4)') 'P:PRES:-2: UN_NEW_OTHER ', IW, IIO, JJO-1, KKO, UN_NEW_OTHER, IIO, JJO, KKO
#endif
                           ENDDO
                        ENDDO
                     ENDDO
                  CASE( 3)
                     DO KKO=KKO1,KKO2
                        DO JJO=JJO1,JJO2
                           DO IIO=IIO1,IIO2
                              DWDT = -OM%FVZ(IIO,JJO,KKO)   - M2%RDZN(KKO)  *(HP(I,J,K+1)-HP(I,J,K))
                              UN_NEW_OTHER = UN_NEW_OTHER + OM%W(IIO,JJO,KKO)   + DT*DWDT
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A25,4i4,E14.6,3i4)') 'P:PRES: 3: UN_NEW_OTHER ', IW, IIO, JJO, KKO, UN_NEW_OTHER, IIO, JJO, KKO+1
#endif
                           ENDDO
                        ENDDO
                     ENDDO
                  CASE(-3)
                     DO KKO=KKO1,KKO2
                        DO JJO=JJO1,JJO2
                           DO IIO=IIO1,IIO2
                              DWDT = -OM%FVZ(IIO,JJO,KKO-1) - M2%RDZN(KKO-1)*(HP(IIO,JJO,KKO)-HP(IIO,JJO,KKO-1))
                              UN_NEW_OTHER = UN_NEW_OTHER + OM%W(IIO,JJO,KKO-1) + DT*DWDT
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A25,4i4,E14.6,3i4)') 'P:PRES:-3: UN_NEW_OTHER ', IW, IIO, JJO, KKO-1, UN_NEW_OTHER, IIO, JJO, KKO
#endif
                           ENDDO
                        ENDDO
                     ENDDO
               END SELECT IOR_SELECT_1
            ENDIF

            IF (M%WALL(IW)%BOUNDARY_TYPE == SOLID_BOUNDARY) THEN
               UN_NEW_OTHER = -SIGN(1._EB,REAL(IOR0,EB))*MESHES(NM)%WALL(IW)%ONE_D%U_NORMAL_S
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A25,4I4, E14.6)') 'VELO-ERR:SP: UN_NEW_OTHER:',  IW, I, J, K, UN_NEW_OTHER
#endif
            ENDIF

            ! Compute velocity difference

            MGM%VELOCITY_ERROR = MAX(MGM%VELOCITY_ERROR, ABS(UN_NEW - UN_NEW_OTHER))

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,4I4, 3E14.6)') '-----------------------------------------------------------------------> FINAL : ',&
                                            IW, I, J, K, UN_NEW, UN_NEW_OTHER, MGM%VELOCITY_ERROR
#endif

         ENDDO WALLCELLS_POISSON_LOOP

      !
      ! ------------------------- Laplace case ---------------------------------------------------------
      !
      CASE (NSCARC_MGM_LAPLACE) 

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'GWC%BOUNDARY_TYPE:'
WRITE(MSG%LU_DEBUG,'(5I5)') (G%WALL(IW)%BOUNDARY_TYPE, IW=1, L%N_WALL_CELLS_EXT + L%N_WALL_CELLS_INT)
WRITE(MSG%LU_DEBUG,*) 'MGM%U3'
WRITE(MSG%LU_DEBUG,MSG%CFORM2) ((MGM%U3(I,1,K), I=0, M%IBAR), K=M%KBAR,1,-1)
WRITE(MSG%LU_DEBUG,*) 'MGM%W3'
WRITE(MSG%LU_DEBUG,MSG%CFORM1) ((MGM%W3(I,1,K), I=1, M%IBAR), K=M%KBAR,0,-1)
WRITE(MSG%LU_DEBUG,*) 'MGM%OU3'
WRITE(MSG%LU_DEBUG,'(5E14.6)') (MGM%OU3(IW), IW=1, L%N_WALL_CELLS_EXT)
WRITE(MSG%LU_DEBUG,*) 'MGM%OW3'
WRITE(MSG%LU_DEBUG,'(5E14.6)') (MGM%OW3(IW), IW=1, L%N_WALL_CELLS_EXT)
WRITE(MSG%LU_DEBUG,*) 'MGM%VELOCITY_ERROR:', MGM%VELOCITY_ERROR
#endif
         WALLCELLS_LAPLACE_LOOP: DO IW = 1, L%N_WALL_CELLS_EXT + L%N_WALL_CELLS_INT

            GWC => G%WALL(IW)

            ITYPE = GWC%BOUNDARY_TYPE
            IF (.NOT. ((IW > L%N_WALL_CELLS_EXT .AND. ITYPE == SOLID_BOUNDARY) .OR. ITYPE == INTERPOLATED_BOUNDARY)) CYCLE

            IOR0 = GWC%IOR

            I = GWC%IXG
            J = GWC%IYG
            K = GWC%IZG

            ! Update normal component of velocity at the mesh boundary

            UN_NEW = 0.0_EB
            UN_NEW_OTHER = 0.0_EB
            SELECT CASE(IOR0)
               CASE( 1)
                  !UN_NEW = M%U(I,J,K)   - DT*(M%FVX(I,J,K)   + M%RDXN(I)  *(HP(I+1,J,K)-HP(I,J,K))*DHFCT)
                  UN_NEW = MGM%U3(I,J,K)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) IOR0, IW, ITYPE, I, J, K, UN_NEW
#endif
               CASE(-1)
                  !UN_NEW = M%U(I-1,J,K) - DT*(M%FVX(I-1,J,K) + M%RDXN(I-1)*(HP(I,J,K)-HP(I-1,J,K))*DHFCT)
                  UN_NEW = MGM%U3(I-1,J,K)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) IOR0, IW, ITYPE, I-1, J, K, UN_NEW
#endif
               CASE( 2)
                  !UN_NEW = M%V(I,J,K)   - DT*(M%FVY(I,J,K)   + M%RDYN(J)  *(HP(I,J+1,K)-HP(I,J,K))*DHFCT)
                  UN_NEW = MGM%V3(I,J,K)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) IOR0, IW, ITYPE, I, J, K, UN_NEW
#endif
               CASE(-2)
                  !UN_NEW = M%V(I,J-1,K) - DT*(M%FVY(I,J-1,K) + M%RDYN(J-1)*(HP(I,J,K)-HP(I,J-1,K))*DHFCT)
                  UN_NEW = MGM%V3(I,J-1,K)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) IOR0, IW, ITYPE, I, J-1, K, UN_NEW
#endif
               CASE( 3)
                  !UN_NEW = M%W(I,J,K)   - DT*(M%FVZ(I,J,K)   + M%RDZN(K)  *(HP(I,J,K+1)-HP(I,J,K))*DHFCT)
                  UN_NEW = MGM%W3(I,J,K) 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) IOR0, IW, ITYPE, I, J, K, UN_NEW
#endif
               CASE(-3)
                  !UN_NEW = M%W(I,J,K-1) - DT*(M%FVZ(I,J,K-1) + M%RDZN(K-1)*(HP(I,J,K)-HP(I,J,K-1))*DHFCT)
                  UN_NEW = MGM%W3(I,J,K-1) 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) IOR0, IW, ITYPE, I, J, K-1, UN_NEW
#endif
            END SELECT

            IF (GWC%BOUNDARY_TYPE == INTERPOLATED_BOUNDARY) THEN
               SELECT CASE(ABS(IOR0))
                  CASE( 1)
                     UN_NEW_OTHER = MGM%OU3(IW)
                  CASE( 2)
                     UN_NEW_OTHER = MGM%OV3(IW)
                  CASE( 3)
                     UN_NEW_OTHER = MGM%OW3(IW)
               END SELECT
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,2000) IOR0, IW, ITYPE, I, J, K, UN_NEW_OTHER
#endif
            ENDIF

            IF (GWC%BOUNDARY_TYPE == SOLID_BOUNDARY) THEN
               UN_NEW_OTHER = -SIGN(1._EB,REAL(IOR0,EB))*MESHES(NM)%WALL(IW)%ONE_D%U_NORMAL_S
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,3000) IOR0, IW, ITYPE, I, J, K, UN_NEW_OTHER
#endif
            ENDIF

            ! Compute velocity difference

            MGM%VELOCITY_ERROR = MAX(MGM%VELOCITY_ERROR, ABS(UN_NEW - UN_NEW_OTHER))

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,4000) MGM%VELOCITY_ERROR
#endif

         ENDDO WALLCELLS_LAPLACE_LOOP

   END SELECT

   MESH_REAL(NM) = MGM%VELOCITY_ERROR
   RANK_REAL = MAX(RANK_REAL, MESH_REAL(NM))

ENDDO MESHES_LOOP

IF (N_MPI_PROCESSES>1) & 
   CALL MPI_ALLREDUCE(MPI_IN_PLACE, RANK_REAL, 1, MPI_DOUBLE_PRECISION, MPI_MAX, MPI_COMM_WORLD, IERROR)
VELOCITY_ERROR_GLOBAL = RANK_REAL

#ifdef WITH_SCARC_DEBUG
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
WRITE(MSG%LU_DEBUG,*) '========================================================================================='
WRITE(MSG%LU_DEBUG,*) '============> ALL MESHES: VELOCITY_ERROR_GLOBAL =', MGM%VELOCITY_ERROR
WRITE(MSG%LU_DEBUG,*) '========================================================================================='
ENDDO 
1000 FORMAT('VE:LAPLACE: OWN : IOR0=',I4,': IW=',I4,':ITYPE=', I4,': I,J,K=', 3I4, ': UN_NEW      :', E14.6)
2000 FORMAT('VE:LAPLACE: NBR : IOR0=',I4,': IW=',I4,':ITYPE=', I4,': I,J,K=', 3I4, ': UN_NEW_OTHER:', E14.6)
3000 FORMAT('VE:LAPLACE: SOL : IOR0=',I4,': IW=',I4,':ITYPE=', I4,': I,J,K=', 3I4, ': UN_NEW_OTHER:', E14.6)
4000 FORMAT('VE:LAPLACE:',83X,'---> FINAL: ', E14.6)
#endif
END SUBROUTINE SCARC_MGM_COMPUTE_VELOCITY_ERROR


! ---------------------------------------------------------------------------------------------
!> \brief Perform LU-decompositions for local Laplace matrices 
! ---------------------------------------------------------------------------------------------
SUBROUTINE SCARC_METHOD_MGM_LU(NS, NL)
USE SCARC_POINTERS, ONLY: L, G, MGM, A, LM, UM, ST, SCARC_POINT_TO_GRID, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NS, NL
INTEGER :: J, K, N, NM
REAL(EB) :: VAL
#ifdef WITH_SCARC_DEBUG
INTEGER :: I
#endif

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)   
   A   => SCARC_POINT_TO_CMATRIX (G, NSCARC_MATRIX_LAPLACE)
   LM  => SCARC_POINT_TO_CMATRIX (G, NSCARC_MATRIX_LM)
   UM  => SCARC_POINT_TO_CMATRIX (G, NSCARC_MATRIX_UM)
   MGM => L%MGM
   ST  => L%STAGE(STACK(NS)%SOLVER%TYPE_STAGE)

   N = G%NC

   DO J = 1, N
      MGM%B(J) = ST%B(G%PERM_BW(J))
   ENDDO
#ifdef WITH_SCARC_DEBUG2
   WRITE(MSG%LU_DEBUG,*) '=============================== A'
   DO I = 1, N
      WRITE(MSG%LU_DEBUG,'(24F8.2)') (MGM%ASQ(I, J), J=1, 24)
   ENDDO
   WRITE(MSG%LU_DEBUG,*) '=============================== L'
   DO I = 1, N
      WRITE(MSG%LU_DEBUG,'(24F8.2)') (MGM%LSQ(I, J), J=1, 24)
   ENDDO
   WRITE(MSG%LU_DEBUG,*) '=============================== U'
   DO I = 1, N
      WRITE(MSG%LU_DEBUG,'(24F8.2)') (MGM%USQ(I, J), J=1, 24)
   ENDDO
#endif
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '=============================== G%PERM_FW'
   WRITE(MSG%LU_DEBUG,'(I5)') (G%PERM_FW(I), I=1, G%NC)
   WRITE(MSG%LU_DEBUG,*) '=============================== G%PERM_BW'
   WRITE(MSG%LU_DEBUG,'(I5)') (G%PERM_BW(I), I=1, G%NC)
   WRITE(MSG%LU_DEBUG,*) '=============================== ST%B'
   WRITE(MSG%LU_DEBUG,'(5E14.6)') (ST%B(I), I=1, G%NC)
   WRITE(MSG%LU_DEBUG,*) '=============================== MGM%B'
   WRITE(MSG%LU_DEBUG,'(5E14.6)') (MGM%B(I), I=1, G%NC)
   CALL SCARC_DEBUG_CMATRIX (LM, 'MGM%L', 'METHOD_MGM_LU ')
   CALL SCARC_DEBUG_CMATRIX (UM, 'MGM%U', 'METHOD_MGM_LU ')
#endif


   DO J = G%NONZERO, N
      MGM%Y(J) = MGM%B(J)
      DO K = 1, J-1
         VAL = SCARC_EVALUATE_CMATRIX(LM, J, K)
         MGM%Y(J) = MGM%Y(J) - VAL*MGM%Y(K)
         !MGM%Y(J) = MGM%Y(J) - MGM%ASQ(J,K)*MGM%Y(K)
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,'(A,2I4,4E14.6)') 'A: J, K, Y(J), Y(K), MGM%ASQ(J,K), LM(J,K):', J, K,  &
                        MGM%Y(J), MGM%Y(K), MGM%ASQ(J,K), VAL
#endif
      ENDDO
   ENDDO

   DO J = N, 1, -1
      MGM%X(J) = MGM%Y(J)
      DO K = J+1, N
         VAL = SCARC_EVALUATE_CMATRIX(UM, J, K)
         MGM%X(J) = MGM%X(J) - VAL*MGM%X(K)
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,'(A,2I4,4E14.6)') 'B: J, K, X(J), X(K), MGM%ASQ(J,K), UM(J,K):', J, K,  &
                        MGM%X(J), MGM%X(K), MGM%ASQ(J,K), VAL
#endif
      ENDDO
      VAL = SCARC_EVALUATE_CMATRIX(UM, J, J)
      MGM%X(J) = MGM%X(J)/VAL
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,'(A,I4,3E14.6)') 'C: J, X(J), MGM%ASQ(J,J):', J, MGM%X(J), MGM%ASQ(J,J), VAL
#endif
   ENDDO

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '=============================== Y'
   WRITE(MSG%LU_DEBUG,'(5E14.6)') (MGM%Y(I), I=1, G%NC)
   WRITE(MSG%LU_DEBUG,*) '=============================== X'
   WRITE(MSG%LU_DEBUG,'(5E14.6)') (MGM%X(I), I=1, G%NC)
#endif

ENDDO

END SUBROUTINE SCARC_METHOD_MGM_LU

! ====================================================================================================
! End MGM routines
! ====================================================================================================

END MODULE SCARC_MGM


