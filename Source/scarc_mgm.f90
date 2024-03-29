!=======================================================================================================================
!
! MODULE SCARC_MGM
! 
!> \brief Setup environment for call of McKeeney-Greengard-Mayo method (still experimental )
!
!=======================================================================================================================
MODULE SCARC_MGM

USE GLOBAL_CONSTANTS
USE PRECISION_PARAMETERS
USE MESH_VARIABLES
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
USE COMP_FUNCTIONS, ONLY: CURRENT_TIME, GET_FILE_NUMBER, SHUTDOWN
USE SCARC_CONSTANTS
USE SCARC_VARIABLES
USE SCARC_STORAGE
USE SCARC_CONVERGENCE

IMPLICIT NONE (TYPE,EXTERNAL)

CONTAINS


! ------------------------------------------------------------------------------------------------------------------
!> \brief Allocate vectors and define variables needed for McKeeney-Greengard-Mayo method
! ------------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MGM (NL)
USE SCARC_POINTERS, ONLY: L, G, MGM, LO, UP, SCARC_POINT_TO_MGM, SCARC_POINT_TO_GRID, SCARC_POINT_TO_CMATRIX
USE SCARC_CONVERGENCE, ONLY: VELOCITY_ERROR_MGM, NIT_MGM
INTEGER, INTENT(IN):: NL
INTEGER:: NM

CROUTINE = 'SCARC_SETUP_MGM'
IS_MGM = .TRUE.

CALL SCARC_SET_GRID_TYPE(NSCARC_GRID_UNSTRUCTURED)

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)

   ! Initialize MGM related variables
    
   MGM%NCS = L%STRUCTURED%NC
   MGM%NCU = L%UNSTRUCTURED%NC

   MGM%NWE = L%N_WALL_CELLS_EXT
   MGM%NWI = L%N_WALL_CELLS_INT
   MGM%NW1 = L%N_WALL_CELLS_EXT+1
   MGM%NW2 = L%N_WALL_CELLS_EXT+L%N_WALL_CELLS_INT

   VELOCITY_ERROR_MGM = SCARC_MGM_ACCURACY
   NIT_MGM = SCARC_MGM_ITERATIONS

   ! Allocate workspace for the storage of the different vectors in the MGM methods

   ! SIP   : structured inhomogeneous Poisson solution (pass 1)
   ! UIP   : unstructured inhomogeneous Poisson solution (merge)
   ! UHL   : unstructured homogeneous Laplace solution (pass 2)
   ! UHL2  : unstructured homogeneous Laplace solution of previous time step (extrapolation BCs only)

   CALL SCARC_ALLOCATE_REAL3 (MGM%SIP, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%SIP', CROUTINE)
   CALL SCARC_ALLOCATE_REAL3 (MGM%UIP, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%UIP', CROUTINE)
   CALL SCARC_ALLOCATE_REAL3 (MGM%UHL, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%UHL', CROUTINE)

   ! OUIP  : other unstructured inhomogeneous Poisson solution on boundary
   ! OUHL  : other unstructured homogeneous Laplace solution on boundary
   ! OUHL2 : other unstructured homogeneous Laplace solution of previous time step on boundary

   CALL SCARC_ALLOCATE_REAL1 (MGM%OUIP, 1, MGM%NWE, NSCARC_INIT_ZERO, 'MGM%OUIP', CROUTINE)
   CALL SCARC_ALLOCATE_REAL1 (MGM%OUHL, 1, MGM%NWE, NSCARC_INIT_ZERO, 'MGM%OUHL', CROUTINE)

   IF (TYPE_MGM_BC == NSCARC_MGM_BC_EXPOL) THEN
      CALL SCARC_ALLOCATE_REAL3 (MGM%UHL2, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%UHL2', CROUTINE)
      CALL SCARC_ALLOCATE_REAL1 (MGM%OUHL2, 1, MGM%NWE, NSCARC_INIT_ZERO, 'MGM%OUHL2', CROUTINE)
   ENDIF

   ! BXS, BXF, BYS, BYF, BZS, BZF: boundary value vectors for the different faces

   CALL SCARC_ALLOCATE_REAL2 (MGM%BXS, 1, L%NY, 1, L%NZ, NSCARC_INIT_ZERO, 'MGM%BXS', CROUTINE)
   CALL SCARC_ALLOCATE_REAL2 (MGM%BXF, 1, L%NY, 1, L%NZ, NSCARC_INIT_ZERO, 'MGM%BXF', CROUTINE)
   CALL SCARC_ALLOCATE_REAL2 (MGM%BYS, 1, L%NX, 1, L%NZ, NSCARC_INIT_ZERO, 'MGM%BYS', CROUTINE)
   CALL SCARC_ALLOCATE_REAL2 (MGM%BYF, 1, L%NX, 1, L%NZ, NSCARC_INIT_ZERO, 'MGM%BYF', CROUTINE)
   CALL SCARC_ALLOCATE_REAL2 (MGM%BZS, 1, L%NX, 1, L%NY, NSCARC_INIT_ZERO, 'MGM%BZS', CROUTINE)
   CALL SCARC_ALLOCATE_REAL2 (MGM%BZF, 1, L%NX, 1, L%NY, NSCARC_INIT_ZERO, 'MGM%BZF', CROUTINE)

   ! UVEL,   VVEL,  WVEL : u-, v- and w-velocity components 
   ! OUVEL, OVVEL, OWVEL : u-, v- and w-velocity components of other mesh

   CALL SCARC_ALLOCATE_REAL3 (MGM%UVEL, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%UVEL', CROUTINE)
   CALL SCARC_ALLOCATE_REAL3 (MGM%VVEL, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%VVEL', CROUTINE)
   CALL SCARC_ALLOCATE_REAL3 (MGM%WVEL, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%WVEL', CROUTINE)

   CALL SCARC_ALLOCATE_REAL1 (MGM%OUVEL, 1, MGM%NWE, NSCARC_INIT_ZERO, 'MGM%OUVEL', CROUTINE)
   CALL SCARC_ALLOCATE_REAL1 (MGM%OVVEL, 1, MGM%NWE, NSCARC_INIT_ZERO, 'MGM%OVVEL', CROUTINE)
   CALL SCARC_ALLOCATE_REAL1 (MGM%OWVEL, 1, MGM%NWE, NSCARC_INIT_ZERO, 'MGM%OWVEL', CROUTINE)

   ! SCARC  : ScaRC solution (structured inhomogeneous by construction)
   ! USCARC : UScaRC solution (unstructured inhomogeneous by construction)
   ! DSCARC : difference of UScaRC and ScaRC solution 

   CALL SCARC_ALLOCATE_REAL3 (MGM%SCARC,  0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%SCARC',  CROUTINE)
   CALL SCARC_ALLOCATE_REAL3 (MGM%USCARC, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%USCARC', CROUTINE)
   CALL SCARC_ALLOCATE_REAL3 (MGM%DSCARC, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%DSCARC', CROUTINE)

   ! UIP_VS_USCARC : difference vector of unstructured inhomogeneous Poisson versus UScaRC
   ! UHL_VS_DSCARC : difference vector of unstructured homogeneous Laplace versus difference UScaRC-ScaRC

   CALL SCARC_ALLOCATE_REAL3 (MGM%UIP_VS_USCARC, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%UIP_VS_US', CROUTINE)
   CALL SCARC_ALLOCATE_REAL3 (MGM%UHL_VS_DSCARC, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%UHL_VS_DSCARC', CROUTINE)

   ! U1, V1, W1: u-, v- and w-velocity components in first MGM pass
   ! U2, V2, W2: u-, v- and w-velocity components in second MGM pass

   CALL SCARC_ALLOCATE_REAL3 (MGM%U1, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%U1', CROUTINE)
   CALL SCARC_ALLOCATE_REAL3 (MGM%V1, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%V1', CROUTINE)
   CALL SCARC_ALLOCATE_REAL3 (MGM%W1, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%W1', CROUTINE)
 
   CALL SCARC_ALLOCATE_REAL3 (MGM%U2, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%U2', CROUTINE)
   CALL SCARC_ALLOCATE_REAL3 (MGM%V2, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%V2', CROUTINE)
   CALL SCARC_ALLOCATE_REAL3 (MGM%W2, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'MGM%W2', CROUTINE)

   ! Configure boundary cell counters and weights for 'True Approximate' boundary setting
   ! BTYPE  : Type of boundary condition in single boundary cells (Dirichlet/Neumann/Internal)
   ! WEIGHT : Weight for true approximate setting in single boundary cells

   IF (TYPE_MGM_BC == NSCARC_MGM_BC_TRUE) THEN

      CALL SCARC_ALLOCATE_INT2 (MGM%BTYPE, 1, MGM%NWE, -3, 3, NSCARC_INIT_NONE, 'MGM%BTYPE', CROUTINE)
      CALL SCARC_ALLOCATE_REAL1 (MGM%WEIGHT, 1, MGM%NWE, NSCARC_INIT_ZERO, 'MGM%WEIGHT', CROUTINE)

      CALL SCARC_SETUP_MGM_TRUE_APPROXIMATE 

   ENDIF

   ! Allocate workspace for MGM solution, RHS and auxiliary vectors if not solver by CG

   IF (TYPE_MGM_LAPLACE /= NSCARC_MGM_LAPLACE_CG) THEN

      CALL SCARC_ALLOCATE_REAL1 (MGM%X, 1, G%NC, NSCARC_INIT_ZERO, 'X', CROUTINE)
      CALL SCARC_ALLOCATE_REAL1 (MGM%B, 1, G%NC, NSCARC_INIT_ZERO, 'B', CROUTINE)

#ifdef WITH_MKL
      IF (TYPE_MKL_PRECISION == NSCARC_PRECISION_SINGLE) THEN
         CALL SCARC_ALLOCATE_REAL1_FB (MGM%X_FB, 1, G%NC, NSCARC_INIT_ZERO, 'X', CROUTINE)
         CALL SCARC_ALLOCATE_REAL1_FB (MGM%B_FB, 1, G%NC, NSCARC_INIT_ZERO, 'B', CROUTINE)
      ENDIF
#endif

   ENDIF

   ! The following code is still experimental and addresses the solution of the LU method compactly stored matrices

   IF (TYPE_MGM_LAPLACE == NSCARC_MGM_LAPLACE_LU .OR. TYPE_MGM_LAPLACE == NSCARC_MGM_LAPLACE_LUPERM) THEN

      LO => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_LOWER)
      UP => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_UPPER)

      CALL SCARC_ALLOCATE_REAL1 (MGM%Y, 1, G%NC, NSCARC_INIT_ZERO, 'Y', CROUTINE)

      CALL SCARC_SETUP_MGM_PASS2_SIZES(NM, NLEVEL_MIN)             ! TODO
      CALL SCARC_ALLOCATE_CMATRIX (LO, NLEVEL_MIN, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_LIGHT, 'LO', CROUTINE)
      CALL SCARC_ALLOCATE_CMATRIX (UP, NLEVEL_MIN, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_LIGHT, 'UP', CROUTINE)
   
      CALL SCARC_SETUP_MGM_PASS2(NM, NLEVEL_MIN)

   ENDIF

ENDDO

END SUBROUTINE SCARC_SETUP_MGM


! -------------------------------------------------------------------------------------------------------------
!> \brief Setup structures for the true approximate boundary setting in MGM
! -------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MGM_TRUE_APPROXIMATE 
USE SCARC_POINTERS, ONLY: L, G, GWC, MGM
INTEGER:: IW, I, J, K, IOR0, IC 
INTEGER, ALLOCATABLE, DIMENSION(:,:):: CNT
REAL(EB):: SX, SY, SZ

! temporarily allocate counter

CALL SCARC_ALLOCATE_INT2 (CNT, 1, G%NC, -3, 3, NSCARC_INIT_NONE, 'CNT', CROUTINE)
CNT = 2;  CNT(:,0) = 0

DO IW = 1, MGM%NWE

   GWC => G%WALL(IW)

   I = GWC%IXW
   J = GWC%IYW
   K = GWC%IZW

   IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE

   IOR0 = GWC%IOR
   IC = G%CELL_NUMBER(I, J, K)
 
   ! Counter for the main diagonal entries to be considered

   IF (GWC%BTYPE == DIRICHLET) THEN
      CNT(IC, IOR0) = CNT(IC, IOR0) + 1 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, '(A, i4, A, i4, A, 7I6)') 'DIRICHLET: CNT(',IC, ',',IOR0, ')=', CNT(IC, IOR0)
#endif
   ELSE IF (GWC%BTYPE == NEUMANN) THEN
      CNT(IC, IOR0) = CNT(IC, IOR0) - 1 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, '(A, i4, A, i4, A, 7I6)') 'NEUMANN  : CNT(',IC, ',',IOR0, ')=', CNT(IC, IOR0)
#endif
   ENDIF

ENDDO

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, *) '============================================='
DO IC = 1, G%NC
WRITE(MSG%LU_DEBUG, '(A, i4, A, 7I6)') 'CNT(',IC, ',-3:3)=', (CNT(IC, I), I = -3, 3)
ENDDO
#endif
! Configure boundary type information for 'True Approximate' boundary setting

DO IW = 1, MGM%NWE

   GWC => G%WALL(IW)
   IF (GWC%BTYPE /= INTERNAL) CYCLE

   I = GWC%IXW
   J = GWC%IYW
   K = GWC%IZW

   IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE
   IC = G%CELL_NUMBER(I, J, K)

   DO IOR0 = -3, 3
      IF (CNT(IC, IOR0) > 2) THEN
         MGM%BTYPE(IW, IOR0) = DIRICHLET
      ELSE IF (CNT(IC, IOR0) < 2) THEN
         MGM%BTYPE(IW, IOR0) = NEUMANN
      ELSE
         MGM%BTYPE(IW, IOR0) = INTERNAL
      ENDIF
   ENDDO

   IF (TWO_D) THEN
      SX = REAL(CNT(IC, 1) + CNT(IC, -1) - 2, EB)
      SZ = REAL(CNT(IC, 3) + CNT(IC, -3) - 2, EB)
      MGM%WEIGHT(IW) = 1.0_EB/(SX*L%DXI2+SZ*L%DZI2)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, '(A, i4, A, 3E14.6, A, 7I6)') 'MGM%WEIGHT(',IW, ')=', MGM%WEIGHT(IW), SX, SZ, ' : ', &
                                                  (MGM%BTYPE(IW, I), I = -3, 3)
#endif
   ELSE
      SX = REAL(CNT(IC, 1) + CNT(IC, -1) - 2, EB)
      SY = REAL(CNT(IC, 2) + CNT(IC, -2) - 2, EB)
      SZ = REAL(CNT(IC, 3) + CNT(IC, -3) - 2, EB)
      MGM%WEIGHT(IW) = 1.0_EB/(SX*L%DXI2+SY*L%DYI2+SZ*L%DZI2)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, '(A, i4, A, 4E14.6, A, 7I6)') 'MGM%WEIGHT(',IW, ')=', MGM%WEIGHT(IW), SX, SY, SZ, ' : ', &
                                                  (MGM%BTYPE(IW, I), I = -3, 3)
#endif
   ENDIF
   
ENDDO

CALL SCARC_DEALLOCATE_INT2 (CNT, 'CNT', CROUTINE)

END SUBROUTINE SCARC_SETUP_MGM_TRUE_APPROXIMATE


! -------------------------------------------------------------------------------------------------------------
!> \brief Setup sizes for LU-decomposition of McKeeney-Greengard-Mayo method
! -------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MGM_PASS2_SIZES(NM, NL)
USE SCARC_POINTERS, ONLY: G, LO, UP, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN):: NM, NL
INTEGER:: IC, JC, NMAX_U, NMAX_L

CROUTINE = 'SCARC_SETUP_MGM_PASS2_SIZES'

CALL SCARC_SET_GRID_TYPE (NSCARC_GRID_UNSTRUCTURED)
CALL SCARC_POINT_TO_GRID (NM, NL)

! Preset pointers for LO and UP with one-value rows (corresponding to initialization with diagonal element)
 
NMAX_U = G%NC
NMAX_L = G%NC

UP%N_ROW = G%NC+1
LO%N_ROW = G%NC+1
UP%N_VAL = G%NC
LO%N_VAL = G%NC

ROW_LOOP: DO IC = 1, G%NC  
   COL_LOOP: DO JC = IC, G%NC
      UP%N_VAL = UP%N_VAL + 1
      LO%N_VAL = LO%N_VAL + 1
   ENDDO COL_LOOP
ENDDO ROW_LOOP

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, *) 'LO%N_VAL =', LO%N_VAL
WRITE(MSG%LU_DEBUG, *) 'LO%N_ROW =', LO%N_ROW
WRITE(MSG%LU_DEBUG, *) 'UP%N_VAL =', UP%N_VAL
WRITE(MSG%LU_DEBUG, *) 'UP%N_ROW =', UP%N_ROW
#endif
END SUBROUTINE SCARC_SETUP_MGM_PASS2_SIZES


! -------------------------------------------------------------------------------------------------------------
!> \brief Setup LU-decomposition for McKeeney-Greengard-Mayo method
! -------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MGM_PASS2(NM, NL)
USE SCARC_POINTERS, ONLY: G, A, LO, UP, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN):: NM, NL
INTEGER:: IC0, IC, JC, KC, NMAX_U, NMAX_L
REAL (EB):: SCAL, VL = 0.0_EB, VU = 0.0_EB, VAL
INTEGER:: TYPE_SCOPE_SAVE

CROUTINE = 'SCARC_SETUP_MGM_PASS2'

TYPE_SCOPE_SAVE = TYPE_SCOPE(0)
TYPE_SCOPE(0) = NSCARC_SCOPE_LOCAL

CALL SCARC_SET_GRID_TYPE (NSCARC_GRID_UNSTRUCTURED)
CALL SCARC_POINT_TO_GRID (NM, NL)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, *) 'SETTING SIZES FOR L AND U MATRICES'
WRITE(MSG%LU_DEBUG, *) 'LO%N_VAL =', LO%N_VAL
WRITE(MSG%LU_DEBUG, *) 'LO%N_ROW =', LO%N_ROW
WRITE(MSG%LU_DEBUG, *) 'UP%N_VAL =', UP%N_VAL
WRITE(MSG%LU_DEBUG, *) 'UP%N_ROW =', UP%N_ROW
#endif
A  => G%LAPLACE
#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_CMATRIX (A, 'LAPLACE', 'SETUP_MGM_PASS2: INIT ')
WRITE(MSG%LU_DEBUG, *) 'G%NC =', G%NC
#endif
#ifdef WITH_SCARC_DEBUG2
CALL SCARC_MATLAB_MATRIX(A%VAL, A%ROW, A%COL, G%NC, G%NC, NM, NL, 'LAPLACE')
#endif

! Preset pointers for LM and UM with one-value rows (corresponding to initialization with diagonal element)
!
DO IC = 1, G%NC
   UP%ROW(IC) = IC ;  UP%COL(IC) = IC
   LO%ROW(IC) = IC ;  LO%COL(IC) = IC
ENDDO
UP%ROW(G%NC+1) = G%NC+1
LO%ROW(G%NC+1) = G%NC+1

NMAX_U = G%NC
NMAX_L = G%NC

ROW_LOOP: DO IC0 = 1, G%NC  

   IC = IC0

   ! Set main diagonal element of L to 1.0
   VAL = 1.0_EB
   CALL SCARC_INSERT_TO_CMATRIX (LO, VAL, IC, IC, G%NC, NMAX_L, 'LO')

   COL_LOOP: DO JC = IC, G%NC

      SCAL = 0.0_EB
      DO KC = 1, IC-1
         VL = SCARC_EVALUATE_CMATRIX (LO, IC, KC)
         VU = SCARC_EVALUATE_CMATRIX (UP, KC, JC)
         SCAL = SCAL+VL*VU
      ENDDO

      VAL = SCARC_EVALUATE_CMATRIX(A, IC, JC)  - SCAL
      IF (ABS(VAL) > TWO_EPSILON_EB) CALL SCARC_INSERT_TO_CMATRIX (UP, VAL, IC, JC, G%NC, NMAX_U, 'UM')

      SCAL = 0.0_EB
      DO KC = 1, IC-1
         VL = SCARC_EVALUATE_CMATRIX (LO, JC, KC)
         VU = SCARC_EVALUATE_CMATRIX (UP, KC, IC)
         SCAL = SCAL+VL*VU
      ENDDO
      VAL = (SCARC_EVALUATE_CMATRIX(A, JC, IC) - SCAL)/SCARC_EVALUATE_CMATRIX(UP, IC, IC)
      IF (ABS(VAL) > TWO_EPSILON_EB) CALL SCARC_INSERT_TO_CMATRIX (LO, VAL, JC, IC, G%NC, NMAX_L, 'LM')

   ENDDO COL_LOOP

ENDDO ROW_LOOP

CALL SCARC_REDUCE_CMATRIX (LO, 'LO', CROUTINE)
CALL SCARC_REDUCE_CMATRIX (UP, 'UP', CROUTINE)
#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_CMATRIX (LO, 'MGM%L-FINAL', 'SETUP_MGM_PASS2 ')
CALL SCARC_DEBUG_CMATRIX (UP, 'MGM%U-FINAL', 'SETUP_MGM_PASS2 ')
#endif

TYPE_SCOPE(0) = TYPE_SCOPE_SAVE

#ifdef WITH_SCARC_DEBUG
1000 FORMAT('================= IC : ', I3, ' ===========================')
1100 FORMAT('LLL(',I3, ',',I3, '):', E14.6)
1200 FORMAT('LLL(',I3, ',',I3, '),  UUU(',I3, ',',I3, ') --> JC:', I3, ', VL, VU:', 2E14.6, ', SCAL2:',E14.6)
1300 FORMAT('AAA(',I3, ',',I3, '):',E14.6, ',  UUU(',I3, ',',I3, '):', E14.6)
1400 FORMAT('AAA(',I3, ',',I3, '):',E14.6, ',  UUU(',I3, ',',I3, '):', E14.6, ',  LLL(',I3, ',',I3, '):', E14.6)
#endif
END SUBROUTINE SCARC_SETUP_MGM_PASS2


! --------------------------------------------------------------------------------------------------------------------
!> \brief Convergence state of MGM method
! --------------------------------------------------------------------------------------------------------------------
INTEGER FUNCTION SCARC_MGM_CONVERGENCE_STATE(ITE_MGM, NTYPE)
USE SCARC_POINTERS, ONLY: MGM, SCARC_POINT_TO_MGM
INTEGER, INTENT(IN):: ITE_MGM, NTYPE
INTEGER:: NM

SCARC_MGM_CONVERGENCE_STATE = NSCARC_MGM_FAILURE
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MGM(NM, NLEVEL_MIN)

   SCARC_MGM_ACCURACY   = VELOCITY_ERROR_GLOBAL  ! Store achieved MGM accuracy for statistics in chid.out
   SCARC_MGM_ITERATIONS = ITE_MGM                ! Store required MGM iterations for statistics in chid.out

   SELECT CASE (NTYPE)

      ! Initialization - after first structured inhomogeneous Poisson solution

      CASE (0)

         MGM%ITE = 0
         MGM%ITE_LAPLACE = 0
         MGM%ITE_POISSON = ITE                   ! ITE, CAPPA contain statistics of preceding structured CG-solution
         MGM%CAPPA_POISSON = CAPPA
#ifdef WITH_SCARC_DEBUG
         WRITE(MSG%LU_DEBUG, 1100) ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
                                   MGM%ITE_POISSON, MGM%CAPPA_POISSON, &
                                   MGM%ITE, VELOCITY_ERROR_GLOBAL
#endif
#ifdef WITH_SCARC_VERBOSE
         WRITE(MSG%LU_VERBOSE, 1100) ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
                                     MGM%ITE_POISSON, MGM%CAPPA_POISSON, &
                                     MGM%ITE, VELOCITY_ERROR_GLOBAL
#endif

      ! MGM iteration - after each unstructured homogeneous Laplace solution

      CASE (1)

         MGM%ITE = ITE_MGM
#ifdef WITH_SCARC_DEBUG
         IF (TYPE_MGM_LAPLACE == NSCARC_MGM_LAPLACE_CG) THEN
            WRITE(MSG%LU_DEBUG, 1200) ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
                                      MGM%ITE_POISSON, MGM%CAPPA_POISSON, &
                                      ITE, CAPPA, &
                                      MGM%ITE, VELOCITY_ERROR_GLOBAL
         ELSE
            WRITE(MSG%LU_DEBUG, 1201) ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
                                      MGM%ITE_POISSON, MGM%CAPPA_POISSON, &
                                      MGM%ITE, VELOCITY_ERROR_GLOBAL
         ENDIF
#endif
#ifdef WITH_SCARC_VERBOSE
         IF (TYPE_MGM_LAPLACE == NSCARC_MGM_LAPLACE_CG) THEN
            WRITE(MSG%LU_VERBOSE, 1200) ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
                                        MGM%ITE_POISSON, MGM%CAPPA_POISSON, &
                                        ITE, CAPPA, &
                                        MGM%ITE, VELOCITY_ERROR_GLOBAL
         ELSE
            WRITE(MSG%LU_VERBOSE, 1201) ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
                                        MGM%ITE_POISSON, MGM%CAPPA_POISSON, &
                                        MGM%ITE, VELOCITY_ERROR_GLOBAL
         ENDIF
#endif
         IF (TYPE_MGM_LAPLACE == NSCARC_MGM_LAPLACE_CG .AND. ITE > MGM%ITE_LAPLACE) THEN
            MGM%ITE_LAPLACE = MAX(ITE, MGM%ITE_LAPLACE)            ! Store worst Laplace-CG statistics
            MGM%CAPPA_LAPLACE = CAPPA
         ENDIF                         

      ! Termination - after whole MGM solution

      CASE (-1)

         CAPPA = MGM%CAPPA_POISSON   ! Reset to Krylov statistics of Poisson solution for statistics in chid.out
         ITE   = MGM%ITE_POISSON


#ifdef WITH_SCARC_DEBUG
      IF (VELOCITY_ERROR_GLOBAL <= VELOCITY_ERROR_MGM) THEN
         IF (TYPE_MGM_LAPLACE == NSCARC_MGM_LAPLACE_CG) THEN
            WRITE(MSG%LU_DEBUG, 1300) ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
                                      MGM%ITE_POISSON, MGM%CAPPA_POISSON, &
                                      MGM%ITE_LAPLACE, MGM%CAPPA_LAPLACE, &
                                      MGM%ITE, VELOCITY_ERROR_GLOBAL, ' ... success'
         ELSE
            WRITE(MSG%LU_DEBUG, 1301) ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
                                      MGM%ITE_POISSON, MGM%CAPPA_POISSON, &
                                      MGM%ITE, VELOCITY_ERROR_GLOBAL, ' ... success'
         ENDIF
      ELSE
         IF (TYPE_MGM_LAPLACE == NSCARC_MGM_LAPLACE_CG) THEN
            WRITE(MSG%LU_DEBUG, 1300) ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
                                      MGM%ITE_POISSON, MGM%CAPPA_POISSON, &
                                      MGM%ITE_LAPLACE, MGM%CAPPA_LAPLACE, &
                                      MGM%ITE, VELOCITY_ERROR_GLOBAL, ' ... failure'
         ELSE
            WRITE(MSG%LU_DEBUG, 1301) ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
                                      MGM%ITE_POISSON, MGM%CAPPA_POISSON, &
                                      MGM%ITE, VELOCITY_ERROR_GLOBAL, ' ... failure'
         ENDIF
      ENDIF
#endif
#ifdef WITH_SCARC_VERBOSE
      IF (VELOCITY_ERROR_GLOBAL <= VELOCITY_ERROR_MGM) THEN
         IF (TYPE_MGM_LAPLACE == NSCARC_MGM_LAPLACE_CG) THEN
            WRITE(MSG%LU_VERBOSE, 1300) ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
                                        MGM%ITE_POISSON, MGM%CAPPA_POISSON, &
                                        MGM%ITE_LAPLACE, MGM%CAPPA_LAPLACE, &
                                        MGM%ITE, VELOCITY_ERROR_GLOBAL, ' ... success'
         ELSE
            WRITE(MSG%LU_VERBOSE, 1301) ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
                                        MGM%ITE_POISSON, MGM%CAPPA_POISSON, &
                                        MGM%ITE, VELOCITY_ERROR_GLOBAL, ' ... success'
         ENDIF
      ELSE
         IF (TYPE_MGM_LAPLACE == NSCARC_MGM_LAPLACE_CG) THEN
            WRITE(MSG%LU_VERBOSE, 1300) ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
                                        MGM%ITE_POISSON, MGM%CAPPA_POISSON, &
                                        MGM%ITE_LAPLACE, MGM%CAPPA_LAPLACE, &
                                        MGM%ITE, VELOCITY_ERROR_GLOBAL, ' ... failure'
         ELSE
            WRITE(MSG%LU_VERBOSE, 1301) ICYC, PRESSURE_ITERATIONS, TOTAL_PRESSURE_ITERATIONS, &
                                        MGM%ITE_POISSON, MGM%CAPPA_POISSON, &
                                        MGM%ITE, VELOCITY_ERROR_GLOBAL, ' ... failure'
         ENDIF
      ENDIF
#endif
   END SELECT
   IF (VELOCITY_ERROR_GLOBAL <= VELOCITY_ERROR_MGM) SCARC_MGM_CONVERGENCE_STATE = NSCARC_MGM_SUCCESS

ENDDO

#ifdef WITH_SCARC_VERBOSE
1100 FORMAT('ICYC ',I6, ', #PI: ', I6,', #TPI: ', I6, &
            ' , #POIS: ',    I5, ' , RATE: ',    F6.2, &
            ' , #MGM: ',     I5, ' , VEL_ERR: ', E10.2, a14)
1200 FORMAT('ICYC ',I6, ', #PI: ', I6,', #TPI: ', I6, &
            ' , #POIS: ',    I5, ' , RATE: ',    F6.2, &
            ' , #LAPL:    ',    I5, ' , RATE:    ',    F6.2, &
            ' , #MGM: ',     I5, ' , VEL_ERR: ', E10.2, a14)
1201 FORMAT('ICYC ',I6, ', #PI: ', I6,', #TPI: ', I6, &
            ' , #POIS: ',    I5, ' , RATE: ',    F6.2, &
            ' , #MGM: ',     I5, ' , VEL_ERR: ', E10.2, a14)
1300 FORMAT('ICYC ',I6, ', #PI: ', I6,', #TPI: ', I6, &
            ' , #POIS: ',    I5, ' , RATE: ',    F6.2, &
            ' , #LAPLmax: ', I5, ' , RATEmax: ', F6.2, &
            ' , #MGM: ',     I5, ' , VEL_ERR: ', E10.2, a14)
1301 FORMAT('ICYC ',I6, ', #PI: ', I6,', #TPI: ', I6, &
            ' , #POIS: ',    I5, ' , RATE: ',    F6.2, &
            ' , #MGM: ',     I5, ' , VEL_ERR: ', E10.2, a14)
#endif
END FUNCTION SCARC_MGM_CONVERGENCE_STATE


! --------------------------------------------------------------------------------------------------------------
!> \brief Set correct boundary values at external and internal boundaries
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_MGM_UPDATE_GHOSTCELLS(NTYPE)
USE SCARC_POINTERS, ONLY: M, L, G, GWC, HP, MGM, BXS, BXF, BYS, BYF, BZS, BZF, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN):: NTYPE
INTEGER:: NM, IW, IOR0, IXG, IYG, IZG, IXW, IYW, IZW 
#ifdef WITH_SCARC_DEBUG2
INTEGER:: I, K
#endif

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NLEVEL_MIN)                                   

   SELECT CASE(NTYPE)

      ! Update ghostcells for local Laplace problems
      ! Along external boundaries use zero Dirichlet or Neumann BC's
      ! Along mesh interfaces use Dirichlet BC's corresponding to MGM interface settings 
        
      CASE (NSCARC_MGM_LAPLACE)

         HP => MGM%UHL
         BXS => MGM%BXS ; BXF => MGM%BXF
         BYS => MGM%BYS ; BYF => MGM%BYF
         BZS => MGM%BZS ; BZF => MGM%BZF
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, *) 'UPDATE_MGM_GHOST_CELLS:1: HP: NTYPE: LAPLACE'
WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((HP(IXG, 1, IZG), IXG = 0, L%NX+1), IZG = L%NZ+1, 0, -1)
WRITE(MSG%LU_DEBUG,*) 'BXS:'
WRITE(MSG%LU_DEBUG,'(2E14.6)') ((BXS(IYW,IZW),IYW=1,L%NY),IZW=1,L%NZ)
WRITE(MSG%LU_DEBUG,*) 'BXF:'
WRITE(MSG%LU_DEBUG,'(2E14.6)') ((BXF(IYW,IZW),IYW=1,L%NY),IZW=1,L%NZ)
WRITE(MSG%LU_DEBUG,*) 'BYS:'
WRITE(MSG%LU_DEBUG,'(9E14.6)') ((BYS(IXW,IZW),IXW=1,L%NX),IZW=1,L%NZ)
WRITE(MSG%LU_DEBUG,*) 'BYF:'
WRITE(MSG%LU_DEBUG,'(9E14.6)') ((BYF(IXW,IZW),IXW=1,L%NX),IZW=1,L%NZ)
WRITE(MSG%LU_DEBUG,*) 'BZS:'
WRITE(MSG%LU_DEBUG,'(9E14.6)') ((BZS(IXW,IYW),IXW=1,L%NX),IYW=1,L%NY)
WRITE(MSG%LU_DEBUG,*) 'BZF:'
WRITE(MSG%LU_DEBUG,'(9E14.6)') ((BZF(IXW,IYW),IXW=1,L%NX),IYW=1,L%NY)
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
      
            SELECT CASE (IOR0)
               CASE ( 1)
                  IF (GWC%BTYPE == INTERNAL) THEN
                     HP(IXG, IYW, IZW) = -HP(IXW, IYW, IZW) + 2.0_EB*BXS(IYW, IZW)
#ifdef WITH_SCARC_DEBUG2
      WRITE(MSG%LU_DEBUG, 1000) IOR0, IW, IXG, IYW, IZW, BXS(IYW, IZW), HP(IXW, IYW, IZW), HP(IXG, IYW, IZW)
#endif
                  ELSE IF (GWC%BTYPE == DIRICHLET) THEN
                     HP(IXG, IYW, IZW) = -HP(IXW, IYW, IZW) 
                  ELSE IF (GWC%BTYPE == NEUMANN) THEN
                     HP(IXG, IYW, IZW) =  HP(IXW, IYW, IZW) 
                  ENDIF
               CASE (-1)
                  IF (GWC%BTYPE == INTERNAL) THEN
                     HP(IXG, IYW, IZW) = -HP(IXW, IYW, IZW) + 2.0_EB*BXF(IYW, IZW)
#ifdef WITH_SCARC_DEBUG2
      WRITE(MSG%LU_DEBUG, 1000) IOR0, IW, IXG, IYW, IZW, BXF(IYW, IZW), HP(IXW, IYW, IZW), HP(IXG, IYW, IZW)
#endif
                  ELSE IF (GWC%BTYPE == DIRICHLET) THEN
                     HP(IXG, IYW, IZW) = -HP(IXW, IYW, IZW) 
                  ELSE IF (GWC%BTYPE == NEUMANN) THEN
                     HP(IXG, IYW, IZW) =  HP(IXW, IYW, IZW) 
                  ENDIF
               CASE ( 2)
                  IF (GWC%BTYPE == INTERNAL) THEN
                     HP(IXW, IYG, IZW) = -HP(IXW, IYW, IZW) + 2.0_EB*BYS(IXW, IZW)
#ifdef WITH_SCARC_DEBUG2
      WRITE(MSG%LU_DEBUG, 1000) IOR0, IW, IXW, IYG, IZW, BYS(IXW, IZW), HP(IXW, IYW, IZW), HP(IXW, IYG, IZW)
#endif
                  ELSE IF (GWC%BTYPE == DIRICHLET) THEN
                     HP(IXW, IYG, IZW) = -HP(IXW, IYW, IZW) 
                  ELSE IF (GWC%BTYPE == NEUMANN) THEN
                     HP(IXW, IYG, IZW) =  HP(IXW, IYW, IZW) 
                  ENDIF
               CASE (-2)
                  IF (GWC%BTYPE == INTERNAL) THEN
                     HP(IXW, IYG, IZW) = -HP(IXW, IYW, IZW) + 2.0_EB*BYF(IXW, IZW)
#ifdef WITH_SCARC_DEBUG2
      WRITE(MSG%LU_DEBUG, 1000) IOR0, IW, IXW, IYG, IZW, BYF(IXW, IZW), HP(IXW, IYW, IZW), HP(IXW, IYG, IZW)
#endif
                  ELSE IF (GWC%BTYPE == DIRICHLET) THEN
                     HP(IXW, IYG, IZW) = -HP(IXW, IYW, IZW) 
                  ELSE IF (GWC%BTYPE == NEUMANN) THEN
                     HP(IXW, IYG, IZW) =  HP(IXW, IYW, IZW) 
                  ENDIF
               CASE ( 3)
                  IF (GWC%BTYPE == INTERNAL) THEN
                     HP(IXW, IYW, IZG) = -HP(IXW, IYW, IZW) + 2.0_EB*BZS(IXW, IYW)
#ifdef WITH_SCARC_DEBUG2
      WRITE(MSG%LU_DEBUG, 1000) IOR0, IW, IXW, IYW, IZG, BZS(IXW, IYW), HP(IXW, IYW, IZW), HP(IXW, IYW, IZG)
#endif
                  ELSE IF (GWC%BTYPE == DIRICHLET) THEN
                     HP(IXW, IYW, IZG) = -HP(IXW, IYW, IZW) 
                  ELSE IF (GWC%BTYPE == NEUMANN) THEN
                     HP(IXW, IYW, IZG) =  HP(IXW, IYW, IZW) 
                  ENDIF
               CASE (-3)
                  IF (GWC%BTYPE == INTERNAL) THEN
                     HP(IXW, IYW, IZG) = -HP(IXW, IYW, IZW) + 2.0_EB*BZF(IXW, IYW)
#ifdef WITH_SCARC_DEBUG2
      WRITE(MSG%LU_DEBUG, 1000) IOR0, IW, IXW, IYW, IZG, BZF(IXW, IYW), HP(IXW, IYW, IZW), HP(IXW, IYW, IZG)
#endif
                  ELSE IF (GWC%BTYPE == DIRICHLET) THEN
                     HP(IXW, IYW, IZG) = -HP(IXW, IYW, IZW) 
                  ELSE IF (GWC%BTYPE == NEUMANN) THEN
                     HP(IXW, IYW, IZG) =  HP(IXW, IYW, IZW) 
                  ENDIF
            END SELECT
      
         ENDDO WALL_CELLS_LOOP_LAPLACE

      ! All other cases: 
      ! Updating the ghost cells for the inhomogeneous structured Poisson as well as 
      ! the (optional) ScaRC and UScaRC solutions in MGM and when terminating the current MGM run

      CASE  (NSCARC_MGM_POISSON, NSCARC_MGM_SCARC, NSCARC_MGM_USCARC, NSCARC_MGM_TERMINATE) 

         SELECT CASE (NTYPE)
            CASE (NSCARC_MGM_POISSON)
               HP => MGM%SIP
            CASE (NSCARC_MGM_SCARC) 
               HP => MGM%SCARC
            CASE (NSCARC_MGM_USCARC)
               HP => MGM%USCARC
            CASE (NSCARC_MGM_TERMINATE)
               HP => MGM%UIP
         END SELECT

         BXS => M%BXS ; BXF => M%BXF
         BYS => M%BYS ; BYF => M%BYF
         BZS => M%BZS ; BZF => M%BZF
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, *) 'UPDATE_MGM_GHOST_CELLS:1: HP: NTYPE:', NTYPE
WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((HP(I, 1, K), I = 0, L%NX+1), K = L%NZ+1, 0, -1)
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
                  IF (GWC%BTYPE == DIRICHLET) THEN
                     HP(IXG, IYW, IZW) = -HP(IXW, IYW, IZW) + 2.0_EB*BXS(IYW, IZW)
                  ELSE IF (GWC%BTYPE == NEUMANN) THEN
                     HP(IXG, IYW, IZW) =  HP(IXW, IYW, IZW) - L%DX*BXS(IYW, IZW)
                  ENDIF
               CASE (-1)
                  IF (GWC%BTYPE == DIRICHLET) THEN
                     HP(IXG, IYW, IZW) = -HP(IXW, IYW, IZW) + 2.0_EB*BXF(IYW, IZW)
                  ELSE IF (GWC%BTYPE == NEUMANN) THEN
                     HP(IXG, IYW, IZW) =  HP(IXW, IYW, IZW) + L%DX*BXF(IYW, IZW)
                  ENDIF
               CASE ( 2)
                  IF (GWC%BTYPE == DIRICHLET) THEN
                     HP(IXW, IYG, IZW) = -HP(IXW, IYW, IZW) + 2.0_EB*BYS(IXW, IZW)
                  ELSE IF (GWC%BTYPE == NEUMANN) THEN
                     HP(IXW, IYG, IZW) =  HP(IXW, IYW, IZW) - L%DY*BYS(IXW, IZW)
                  ENDIF
               CASE (-2)
                  IF (GWC%BTYPE == DIRICHLET) THEN
                     HP(IXW, IYG, IZW) = -HP(IXW, IYW, IZW) + 2.0_EB*BYF(IXW, IZW)
                  ELSE IF (GWC%BTYPE == NEUMANN) THEN
                     HP(IXW, IYG, IZW) =  HP(IXW, IYW, IZW) + L%DY*BYF(IXW, IZW)
                  ENDIF
               CASE ( 3)
                  IF (GWC%BTYPE == DIRICHLET) THEN
                     HP(IXW, IYW, IZG) = -HP(IXW, IYW, IZW) + 2.0_EB*BZS(IXW, IYW)
                  ELSE IF (GWC%BTYPE == NEUMANN) THEN
                     HP(IXW, IYW, IZG) =  HP(IXW, IYW, IZW) - L%DZ*BZS(IXW, IYW)
                  ENDIF
               CASE (-3)
                  IF (GWC%BTYPE == DIRICHLET) THEN
                     HP(IXW, IYW, IZG) = -HP(IXW, IYW, IZW) + 2.0_EB*BZF(IXW, IYW)
                  ELSE IF (GWC%BTYPE == NEUMANN) THEN
                     HP(IXW, IYW, IZG) =  HP(IXW, IYW, IZW) + L%DZ*BZF(IXW, IYW)
                  ENDIF
            END SELECT
#ifdef WITH_SCARC_DEBUG2
            WRITE(MSG%LU_DEBUG, '(A, 5I6, E14.6)') 'UPDATE_GHOST_CELLS: IW, IOR0, IXW, IYW, IZG, HP:',&
                                                   IW, IOR0, IXW, IYW, IZG, HP(IXW, IYW, IZG)
#endif
      
         ENDDO WALL_CELLS_LOOP

   END SELECT
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, *) 'UPDATE_GHOST_CELLS:2: HP'
WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((HP(I, 1, K), I = 0, L%NX+1), K = L%NZ+1, 0, -1)
CALL SCARC_DEBUG_PRESSURE (HP, NM, 'H')
#endif

ENDDO
#ifdef WITH_SCARC_DEBUG2
1000 FORMAT('MGM_UPDATE: LAPLACE: INTERNAL: IOR0=', I6, ': IW=', I6, ': I, J, K=', 3I6, ' BC, HPOLD, HP:', 3E14.6)
#endif

END SUBROUTINE SCARC_MGM_UPDATE_GHOSTCELLS


! ------------------------------------------------------------------------------------------------------------------
!> \brief Copy specified vectors in McKeeney-Greengard-Mayo method 
! ------------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_MGM_COPY(NTYPE)
USE SCARC_POINTERS, ONLY: L, MGM, SCARC_POINT_TO_MGM
#ifdef WITH_SCARC_DEBUG
USE SCARC_POINTERS, ONLY: M
#endif
INTEGER, INTENT(IN):: NTYPE
INTEGER:: NM
INTEGER:: IX, IY, IZ
#ifdef WITH_SCARC_DEBUG
INTEGER:: I, K
#endif

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MGM(NM, NLEVEL_MIN)

   SELECT CASE(NTYPE)

      ! Copy structured ihomogeneous Poisson MGM solution to unstructured ihomogeneous Poisson MGM solution (initialization) 

      CASE (NSCARC_MGM_SIP_TO_UIP) 

         MGM%UIP = MGM%SIP
#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG, *) '---------- COPY SP TO UP:', TOTAL_PRESSURE_ITERATIONS
      WRITE(MSG%LU_DEBUG, *) 'MGM%SIP'
      WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%SIP(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
      WRITE(MSG%LU_DEBUG, *) 'MGM%UIP'
      WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%UIP(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
#endif

      ! Copy structured inhomogeneous ScaRC solution to structured inhomogeneous Poisson MGM solution

      CASE (NSCARC_MGM_SCARC_TO_SIP)                    

         MGM%SIP = MGM%SCARC
#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG, *) '---------- COPY SS TO SP:', TOTAL_PRESSURE_ITERATIONS
      WRITE(MSG%LU_DEBUG, *) 'MGM%SCARC'
      WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%SCARC(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
      WRITE(MSG%LU_DEBUG, *) 'MGM%SIP'
      WRITE(MSG%LU_DEBUG, MSG%CFORM1) ((MGM%SIP(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
#endif

      ! Copy unstructured inhomogeneous ScaRC solution to unstructured inhomogeneous Poisson MGM solution

      CASE (NSCARC_MGM_USCARC_TO_UIP)                     

         MGM%UIP = MGM%USCARC
         IF (TWO_D) THEN
            DO IZ = 1, L%NZ
               DO IX = 1, L%NX
                  IF (L%IS_SOLID(IX, 1, IZ)) MGM%UIP(IX, 0:2, IZ) = 0.0_EB
               ENDDO
            ENDDO
         ELSE
            DO IZ = 1, L%NZ
               DO IY = 1, L%NY
                  DO IX = 1, L%NX
                     IF (L%IS_SOLID(IX, IY, IZ)) MGM%UIP(IX, IY, IZ) = 0.0_EB
                  ENDDO
               ENDDO
            ENDDO
         ENDIF

#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG, *) '---------- COPY US TO UP:', TOTAL_PRESSURE_ITERATIONS
      WRITE(MSG%LU_DEBUG, *) 'MGM%USCARC'
      WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%USCARC(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
      WRITE(MSG%LU_DEBUG, *) 'MGM%UIP'
      WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%UIP(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
#endif

      ! Copy difference between ScaRC and UScaRC to unstructured homogeneous Laplace MGM solution

      CASE (NSCARC_MGM_DSCARC_TO_UHL)    

         MGM%UHL = MGM%DSCARC

         IF (TWO_D) THEN
            DO IZ = 1, L%NZ
               DO IX = 1, L%NX
                  IF (L%IS_SOLID(IX, 1, IZ)) MGM%UHL(IX, 0:2, IZ) = 0.0_EB
               ENDDO
            ENDDO
         ELSE
            DO IZ = 1, L%NZ
               DO IY = 1, L%NY
                  DO IX = 1, L%NX
                     IF (L%IS_SOLID(IX, IY, IZ)) MGM%UHL(IX, IY, IZ) = 0.0_EB
                  ENDDO
               ENDDO
            ENDDO
         ENDIF
#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG, *) '---------- COPY DSU TO UL:', TOTAL_PRESSURE_ITERATIONS
      WRITE(MSG%LU_DEBUG, *) 'MGM%DSCARC'
      WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%DSCARC(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
      WRITE(MSG%LU_DEBUG, *) 'MGM%UHL'
      WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%UHL(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
#endif

      ! Copy difference between ScaRC and UScaRC to previous unstructured homogeneous Laplace solution UHL2

      CASE (NSCARC_MGM_DSCARC_TO_UHL2)  

         MGM%UHL2 = MGM%DSCARC
#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG, *) '---------- COPY DSU TO ULP:', TOTAL_PRESSURE_ITERATIONS
      WRITE(MSG%LU_DEBUG, *) 'MGM%DSCARC'
      WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%DSCARC(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
      WRITE(MSG%LU_DEBUG, *) 'MGM%UHL2'
      WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%UHL2(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
#endif

      ! Copy unstructured homogeneous Laplace MGM solution UHL to previous UHL2

      CASE (NSCARC_MGM_UHL_TO_UHL2)  

         MGM%UHL2 = MGM%UHL
#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG, *) '---------- COPY UL TO ULP:', TOTAL_PRESSURE_ITERATIONS
      WRITE(MSG%LU_DEBUG, *) 'MGM%UHL'
      WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%UHL(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
      WRITE(MSG%LU_DEBUG, *) 'MGM%UHL2'
      WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%UHL2(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
#endif

      ! Copy other unstructured homogeneous Laplace MGM solution to previous 

      CASE (NSCARC_MGM_OUHL_TO_OUHL2) 

         MGM%OUHL2 = MGM%OUHL
#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG, *) '---------- COPY OUL TO OULP:', TOTAL_PRESSURE_ITERATIONS
      WRITE(MSG%LU_DEBUG, *) 'MGM%OUHL'
      WRITE(MSG%LU_DEBUG, MSG%CFORM1) (MGM%OUHL(I), I = 1, L%N_WALL_CELLS_EXT)
      WRITE(MSG%LU_DEBUG, *) 'MGM%OUHL2'
      WRITE(MSG%LU_DEBUG, MSG%CFORM1) (MGM%OUHL2(I), I = 1, L%N_WALL_CELLS_EXT)
#endif


   END SELECT

ENDDO

END SUBROUTINE SCARC_MGM_COPY


! ------------------------------------------------------------------------------------------------------------------
!> \brief Build difference of specified vectors in McKeeney-Greengard-Mayo method
! ------------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_MGM_DIFF(NTYPE)
USE SCARC_POINTERS, ONLY: L, G, ST, MGM, GWC, SCARC_POINT_TO_MGM
#ifdef WITH_SCARC_DEBUG
USE SCARC_POINTERS, ONLY: M
#endif
INTEGER, INTENT(IN):: NTYPE
INTEGER:: NM, IX, IY, IZ, IOR0, IW
#ifdef WITH_SCARC_DEBUG
INTEGER :: I, K
#endif

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MGM(NM, NLEVEL_MIN)
   ST  => L%STAGE(NSCARC_STAGE_ONE)

   SELECT CASE(NTYPE)

      ! Build difference between ScaRC and UScaRC solution

      CASE (NSCARC_MGM_USCARC_VS_SCARC)

         G  => L%STRUCTURED
         MGM%DSCARC = MGM%USCARC-MGM%SCARC

         IF (TWO_D) THEN
            !DO IW = L%N_WALL_CELLS_EXT+1, L%N_WALL_CELLS_EXT+L%N_WALL_CELLS_INT
            DO IW = 1, L%N_WALL_CELLS_EXT+L%N_WALL_CELLS_INT
               GWC => G%WALL(IW)
               IOR0 = GWC%IOR
               IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE
               IX = GWC%IXG;  IZ = GWC%IZG
               IF (L%IS_SOLID(IX, 1, IZ)) MGM%DSCARC(IX, 0:2, IZ) = 0.0_EB
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, '(A, 3I6, 3E14.6)') 'MGM-DIFFERENCE:2D: IW, IX, IZ, DSU:',&
                                       IW, IX, IZ, MGM%DSCARC(IX, 0, IZ), MGM%DSCARC(IX, 1, IZ), MGM%DSCARC(IX, 2, IZ)
#endif
            ENDDO
         ELSE
            DO IW = L%N_WALL_CELLS_EXT+1, L%N_WALL_CELLS_EXT+L%N_WALL_CELLS_INT
               GWC => G%WALL(IW)
               IOR0 = GWC%IOR
               IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE
               IX = GWC%IXG;  IY = GWC%IYG;  IZ = GWC%IZG
               IF (L%IS_SOLID(IX, IY, IZ)) MGM%DSCARC(IX, IY, IZ) = 0.0_EB
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, '(A, 4I6, E14.6)') 'MGM-DIFFERENCE:3D: IW, IX, IY, IZ, DSU:',IW, IX, IY, IZ, MGM%DSCARC(IX, IY, IZ)
#endif
            ENDDO
         ENDIF

      ! Build difference of unstructured Laplace MGM solution and difference of ScaRC and UScaRC

      CASE (NSCARC_MGM_UHL_VS_DSCARC)            

         MGM%UHL_VS_DSCARC = MGM%UHL-MGM%DSCARC
#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG, *) '---------- COMPARING UL_VS_DSU:', TOTAL_PRESSURE_ITERATIONS
      WRITE(MSG%LU_DEBUG, *) 'MGM%UHL'
      WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%UHL(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
      WRITE(MSG%LU_DEBUG, *) 'MGM%DSCARC'
      WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%DSCARC(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
      WRITE(MSG%LU_DEBUG, *) 'MGM%UHL_VS_DSCARC_UL_VS_DSU'
      WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%UHL_VS_DSCARC(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
#endif

      ! Build difference of unstructured Poisson MGM solution and UScaRC solution

      CASE (NSCARC_MGM_UIP_VS_USCARC)                     

         MGM%UIP_VS_USCARC = MGM%UIP-MGM%USCARC

#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG, *) '---------- COMPARING UP_VS_US:', TOTAL_PRESSURE_ITERATIONS
      WRITE(MSG%LU_DEBUG, *) 'MGM%UIP'
      WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%UIP(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
      WRITE(MSG%LU_DEBUG, *) 'MGM%USCARC'
      WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%USCARC(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
      WRITE(MSG%LU_DEBUG, *) 'MGM%UIP_VS_USCARC'
      WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%UIP_VS_USCARC(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
#endif

   END SELECT

ENDDO

END SUBROUTINE SCARC_MGM_DIFF


! ------------------------------------------------------------------------------------------------------------------
!> \brief Store specified type of vector in McKeeney-Greengard-Mayo method
! ------------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_MGM_STORE(NTYPE)
USE SCARC_POINTERS, ONLY: L, G, ST, MGM, GWC, M, SCARC_POINT_TO_MGM
INTEGER, INTENT(IN):: NTYPE
INTEGER:: NM, IX, IY, IZ, ICS, ICU, ICE, IOR0, IW

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MGM(NM, NLEVEL_MIN)
   ST  => L%STAGE(NSCARC_STAGE_ONE)

   SELECT CASE(NTYPE)

      ! ---------- Store ScaRC solution MGM%SCARC

      CASE (NSCARC_MGM_SCARC)

         G  => L%STRUCTURED

         DO IZ = 1, L%NZ
            DO IY = 1, L%NY
               DO IX = 1, L%NX
                  ICS = G%CELL_NUMBER(IX, IY, IZ)  
                  MGM%SCARC(IX, IY, IZ) = ST%X(ICS) 
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG, '(A, 4I6, 1E14.6)') 'MGM-SCARC:A: IX, IY, IZ, ICS, SS:',&
                                                                   IX, IY, IZ, ICS, MGM%SCARC(IX, IY, IZ)
#endif
               ENDDO
            ENDDO
         ENDDO

         DO IW = 1, L%N_WALL_CELLS_EXT
            GWC => G%WALL(IW)
            IF (GWC%BTYPE /= INTERNAL) CYCLE
            IOR0 = GWC%IOR
            IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE
            IX = GWC%IXG;  IY = GWC%IYG;  IZ = GWC%IZG
            ICE = L%STRUCTURED%CELL_NUMBER(IX, IY, IZ)
            MGM%SCARC(IX, IY, IZ) = ST%X(ICE) 
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG, '(A, 5I6, 1E14.6)') 'MGM-SCARC:B: IX, IY, IZ, IW, ICE, SS:',&
                                                                   IX, IY, IZ, IW, ICE, MGM%SCARC(IX, IY, IZ)
#endif
         ENDDO

      ! ---------- Store UScaRC solution MGM%USCARC

      CASE (NSCARC_MGM_USCARC)

         G  => L%UNSTRUCTURED

         MGM%USCARC = 0.0_EB
         DO IZ = 1, L%NZ
            DO IY = 1, L%NY
               DO IX = 1, L%NX
                  IF (L%IS_SOLID(IX, IY, IZ)) CYCLE
                  ICS = G%CELL_NUMBER(IX, IY, IZ)           ! unstructured cell number
                  MGM%USCARC(IX, IY, IZ) = ST%X(ICS) 
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG, '(A, 4I6, 1E14.6)') 'MGM-USCARC:A: IX, IY, IZ, ICS, SS:',&
                                                                    IX, IY, IZ, ICS, MGM%USCARC(IX, IY, IZ)
#endif
               ENDDO
            ENDDO
         ENDDO

         DO IW = 1, L%N_WALL_CELLS_EXT
            GWC => G%WALL(IW)
            IF (GWC%BTYPE /= INTERNAL) CYCLE
            IOR0 = GWC%IOR
            IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE
            IX = GWC%IXG;  IY = GWC%IYG;  IZ = GWC%IZG
            ICE = G%CELL_NUMBER(IX, IY, IZ)
            MGM%USCARC(IX, IY, IZ) = ST%X(ICE) 
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG, '(A, 5I6, 1E14.6)') 'MGM-USCARC:B: IX, IY, IZ, IW, ICE, SP:',&
                                                                    IX, IY, IZ, IW, ICE, MGM%USCARC(IX, IY, IZ)
#endif
         ENDDO


      ! ---------- Store structured inhomogeneous Poisson solution MGM%SIP

      CASE (NSCARC_MGM_POISSON)

         G  => L%STRUCTURED

         !MGM%SIP = 0.0_EB
         DO IZ = 1, L%NZ
            DO IY = 1, L%NY
               DO IX = 1, L%NX
                  ICS = G%CELL_NUMBER(IX, IY, IZ)              ! structured cell number
                  MGM%SIP(IX, IY, IZ) = ST%X(ICS) 
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG, '(A, 4I6, 1E14.6)') 'MGM-POISSON:A: IX, IY, IZ, ICS, SP:',&
                                                                     IX, IY, IZ, ICS, MGM%SIP(IX, IY, IZ)
#endif
               ENDDO
            ENDDO
         ENDDO

         DO IW = 1, L%N_WALL_CELLS_EXT
            GWC => G%WALL(IW)
            IF (GWC%BTYPE /= INTERNAL) CYCLE
            IOR0 = GWC%IOR
            IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE
            IX = GWC%IXG;  IY = GWC%IYG;  IZ = GWC%IZG
            ICE = L%STRUCTURED%CELL_NUMBER(IX, IY, IZ)
            MGM%SIP(IX, IY, IZ) = ST%X(ICE) 
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG, '(A, 5I6, 1E14.6)') 'MGM-POISSON:B: IX, IY, IZ, IW, ICE, SP:',&
                                                                     IX, IY, IZ, IW, ICE, MGM%SIP(IX, IY, IZ)
#endif
         ENDDO

      ! ---------- Store homogeneous unstructured Laplace solution MGM%UHL

      CASE (NSCARC_MGM_LAPLACE)

         G => L%UNSTRUCTURED

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, *) '=======================> BEGIN OF MGM_STORE_SOLUTION: LAPLACE'
IF (TYPE_MGM_LAPLACE == NSCARC_MGM_LAPLACE_CG) THEN
   WRITE(MSG%LU_DEBUG, *) 'ST%X'
   WRITE(MSG%LU_DEBUG, MSG%CFORM1) (ST%X(ICU), ICU = 1, G%NC)
ELSE
   WRITE(MSG%LU_DEBUG, *) 'MGM%X'
   WRITE(MSG%LU_DEBUG, MSG%CFORM1) (MGM%X(ICU), ICU = 1, G%NC)
ENDIF
#endif
         IF (TYPE_MGM_BC == NSCARC_MGM_BC_EXPOL) THEN
            DO IZ = 0, L%NZ+1
               DO IY = 0, L%NY+1
                  DO IX = 0, L%NX+1
                     IF (L%IS_SOLID(IX, IY, IZ)) CYCLE
                     ICU = G%CELL_NUMBER(IX, IY, IZ)                  ! unstructured cell number
                     MGM%UHL2(IX, IY, IZ) = MGM%UHL(IX, IY, IZ)       ! also store second level
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG, '(A, 4I6, 1E14.6)') 'MGM:ULP:EXPOL: IX, IY, IZ, ICU, UP:', &
                                                                     IX, IY, IZ, ICU, MGM%UHL2(IX, IY, IZ)
#endif
                  ENDDO
               ENDDO
            ENDDO
         ENDIF

         MGM%UHL = 0.0_EB
         SELECT CASE (TYPE_MGM_LAPLACE)
            CASE (NSCARC_MGM_LAPLACE_CG)
               DO IZ = 1, L%NZ
                  DO IY = 1, L%NY
                     DO IX = 1, L%NX
                        IF (L%IS_SOLID(IX, IY, IZ)) CYCLE
                        ICU = G%CELL_NUMBER(IX, IY, IZ)               ! unstructured cell number
                        MGM%UHL(IX, IY, IZ) = ST%X(ICU)               ! solution contained in ST%X
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG, '(A, 4I6, 2E14.6)') 'MGM-LAPLACE:CG: IX, IY, IZ, ICU, UL:',&
                                                                      IX, IY, IZ, ICU, MGM%UHL(IX, IY, IZ), ST%X(ICU)
#endif
                     ENDDO
                  ENDDO
               ENDDO
            CASE (NSCARC_MGM_LAPLACE_LUPERM)
               DO IZ = 1, L%NZ
                  DO IY = 1, L%NY
                     DO IX = 1, L%NX
                        IF (L%IS_SOLID(IX, IY, IZ)) CYCLE
                        ICU = G%PERM_FW(G%CELL_NUMBER(IX, IY, IZ))    ! unstructured permuted cell number
                        MGM%UHL(IX, IY, IZ) = MGM%X(ICU)              ! solution contained in MGM%X
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG, '(A, 4I6, 2E14.6)') 'MGM-LAPLACE:LUPERM: IX, IY, IZ, ICU, UL:',&
                                                      IX, IY, IZ, ICU, MGM%UHL(IX, IY, IZ), MGM%X(ICU)
#endif
                     ENDDO
                  ENDDO
               ENDDO
            CASE DEFAULT
               DO IZ = 1, L%NZ
                  DO IY = 1, L%NY
                     DO IX = 1, L%NX
                        IF (L%IS_SOLID(IX, IY, IZ)) CYCLE
                        ICU = G%CELL_NUMBER(IX, IY, IZ)                ! unstructured cell number
                        MGM%UHL(IX, IY, IZ) = MGM%X(ICU)               ! solution contained in MGM%X
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG, '(A, 4I6, 2E14.6)') 'MGM-LAPLACE:LU: IX, IY, IZ, ICU, UL:',&
                                                      IX, IY, IZ, ICU, MGM%UHL(IX, IY, IZ), MGM%X(ICU)
#endif
                     ENDDO
                  ENDDO
               ENDDO
          END SELECT

#ifdef WITH_SCARC_DEBUG2
   WRITE(MSG%LU_DEBUG, *) '=======================> END   OF MGM_STORE_SOLUTION: LAPLACE'
   WRITE(MSG%LU_DEBUG, *) 'MGM%UHL'
   WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%UHL(IX, 1, IZ), IX = 0, L%NX+1), IZ = L%NZ+1, 0, -1)
#endif

      ! ---------- Merge structured inhomogeneous Poisson and unstructured homogeneous Laplace solutions

      CASE (NSCARC_MGM_MERGE)

#ifdef WITH_SCARC_DEBUG2
   WRITE(MSG%LU_DEBUG, *) '=======================> START OF MGM_STORE_SOLUTION: MGM_MERGE'
   WRITE(MSG%LU_DEBUG, *) 'MGM%SIP'
   WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%SIP(IX, 1, IZ), IX = 0, L%NX+1), IZ = L%NZ+1, 0, -1)
   WRITE(MSG%LU_DEBUG, *) 'MGM%UHL'
   WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%UHL(IX, 1, IZ), IX = 0, L%NX+1), IZ = L%NZ+1, 0, -1)
#endif

         DO IZ = 0, L%NZ+1
            DO IY = 0, L%NY+1
               DO IX = 0, L%NX+1
                  MGM%UIP(IX, IY, IZ) = MGM%SIP(IX, IY, IZ) + MGM%UHL(IX, IY, IZ)              ! Variant A
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG, '(A, 3I6, 3E14.6)') 'MGM:M: IX, IY, IZ, SP, UL, UP:',  IX, IY, IZ, &
                                                      MGM%SIP(IX, IY, IZ), MGM%UHL(IX, IY, IZ), MGM%UIP(IX, IY, IZ)
#endif
               ENDDO
            ENDDO
         ENDDO


#ifdef WITH_SCARC_DEBUG2
   WRITE(MSG%LU_DEBUG, *) '=======================> END   OF MGM_STORE_SOLUTION: MGM_MERGE'
   WRITE(MSG%LU_DEBUG, *) 'MGM%UIP'
   WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%UIP(IX, 1, IZ), IX = 0, L%NX+1), IZ = L%NZ+1, 0, -1)
#endif

#ifdef WITH_SCARC_VERBOSE2
         CALL SCARC_VERBOSE_VECTOR3 (MGM%SIP, 'SP')
         CALL SCARC_VERBOSE_VECTOR3 (MGM%UHL, 'UL')
         CALL SCARC_VERBOSE_VECTOR3 (HP, 'HP')
#endif

      ! ---------- Terminate MGM method and extract predictor/corrector solution for FDS code

      CASE (NSCARC_MGM_TERMINATE)

         IF (PREDICTOR) THEN

            DO IZ = 0, L%NZ+1
               DO IY = 0, L%NY+1
                  DO IX = 0, L%NX+1
                     M%H(IX, IY, IZ) = MGM%UIP(IX, IY, IZ) 
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG, '(A, 3I6, 1E14.6)') 'MGM-PREDICTOR: IX, IY, IZ, UP:', IX, IY, IZ, M%H(IX, IY, IZ)
#endif
                  ENDDO
               ENDDO
            ENDDO
            IF (TWO_D) THEN
               DO IZ = 1, L%NZ
                  DO IX = 1, L%NX
                     IF (L%IS_SOLID(IX, 1, IZ)) M%H(IX, 0:2, IZ) = 0.0_EB
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG, '(A, 3I6, 1E14.6)') 'MGM-PREDICTOR: IX, IY, IZ, UP:', IX, IY, IZ, M%H(IX, IY, IZ)
#endif
                  ENDDO
               ENDDO
            ELSE
               DO IZ = 1, L%NZ
                  DO IY = 1, L%NY
                     DO IX = 1, L%NX
                        IF (L%IS_SOLID(IX, IY, IZ)) M%H(IX, IY, IZ) = 0.0_EB
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG, '(A, 3I6, 1E14.6)') 'MGM-PREDICTOR: IX, IY, IZ, UP:', IX, IY, IZ, M%H(IX, IY, IZ)
#endif
                     ENDDO
                  ENDDO
               ENDDO
            ENDIF

         ELSE

            DO IZ = 0, L%NZ+1
               DO IY = 0, L%NY+1
                  DO IX = 0, L%NX+1
                     M%HS(IX, IY, IZ) = MGM%UIP(IX, IY, IZ) 
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG, '(A, 3I6, 1E14.6)') 'MGM-CORRECTOR: IX, IY, IZ, UP:', IX, IY, IZ, M%HS(IX, IY, IZ)
#endif
                  ENDDO
               ENDDO
            ENDDO
            IF (TWO_D) THEN
               DO IZ = 1, L%NZ
                  DO IX = 1, L%NX
                     IF (L%IS_SOLID(IX, 1, IZ)) M%HS(IX, 0:2, IZ) = 0.0_EB
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG, '(A, 3I6, 1E14.6)') 'MGM-PREDICTOR: IX, IY, IZ, UP:', IX, IY, IZ, M%HS(IX, IY, IZ)
#endif
                  ENDDO
               ENDDO
            ELSE
               DO IZ = 1, L%NZ
                  DO IY = 1, L%NY
                     DO IX = 1, L%NX
                        IF (L%IS_SOLID(IX, IY, IZ)) M%HS(IX, IY, IZ) = 0.0_EB
#ifdef WITH_SCARC_DEBUG2
IF (IY == 1) WRITE(MSG%LU_DEBUG, '(A, 3I6, 1E14.6)') 'MGM-PREDICTOR: IX, IY, IZ, UP:', IX, IY, IZ, M%HS(IX, IY, IZ)
#endif
                     ENDDO
                  ENDDO
               ENDDO
            ENDIF
         ENDIF

   END SELECT

ENDDO

!#ifdef WITH_SCARC_DEBUG
!1000 FORMAT (A, ': IX, IY, IZ =', 3I6, ': ICU =', I6, ': HP =', E14.6)
!#endif
END SUBROUTINE SCARC_MGM_STORE


! --------------------------------------------------------------------------------------------------------------
!> \brief Setup workspace for McKeeney-Greengard-Mayo method
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MGM_WORKSPACE(NL)
USE SCARC_POINTERS, ONLY: M, MGM, SCARC_POINT_TO_MGM
INTEGER, INTENT(IN):: NL
INTEGER  :: NM

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MGM(NM, NL)

   IF (PREDICTOR) THEN
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, *) 'MGM_SETUP_WORKSPACE: PREDICTOR'
#endif
      MGM%U1 = M%U
      MGM%V1 = M%V
      MGM%W1 = M%W
   ELSE
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, *) 'MGM_SETUP_WORKSPACE: CORRECTOR'
#endif
      MGM%U1 = M%US
      MGM%V1 = M%VS
      MGM%W1 = M%WS
   ENDIF

   MGM%SIP = 0.0_EB
   !MGM%UHL = 0.0_EB
   MGM%UIP = 0.0_EB
   !IF (TYPE_MGM_BC == NSCARC_MGM_BC_EXPOL) MGM%UHL2 = 0.0_EB
   MGM%UIP_VS_USCARC = 0.0_EB
   MGM%UHL_VS_DSCARC = 0.0_EB

   MGM%SCARC = 0.0_EB
   MGM%USCARC = 0.0_EB
   MGM%DSCARC = 0.0_EB

ENDDO

END SUBROUTINE SCARC_SETUP_MGM_WORKSPACE


! --------------------------------------------------------------------------------------------------------------
!> \brief Set interface boundary conditions for unstructured, homogeneous part of McKeeney-Greengard-Mayo method
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_MGM_SET_INTERFACES(VB, NM)
USE SCARC_POINTERS, ONLY: L, F, G, OL, OG, MGM, UHL, UHL2, OUHL, OUHL2, BTYPE, &
                          SCARC_POINT_TO_MGM, SCARC_POINT_TO_OTHER_GRID
INTEGER, INTENT(IN):: NM
REAL(EB), DIMENSION(:),   INTENT(IN), POINTER :: VB
INTEGER:: I, J, K, IOR0, IFACE, INBR, NOM, ICG, ICW, IWG, ITYPE
REAL(EB):: VAL, HB(-3:3) = 0.0_EB

ITYPE = TYPE_MGM_BC
IF (TYPE_MGM_BC == NSCARC_MGM_BC_EXPOL .AND. TOTAL_PRESSURE_ITERATIONS <= 2) ITYPE = NSCARC_MGM_BC_MEAN

!MGM_MESH_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

!   CALL SCARC_POINT_TO_MGM(NM, NL)
!   G  => L%UNSTRUCTURED
   BTYPE => MGM%BTYPE

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG, *) '%%%%%%%%%%%%%%%%% START SET INTERFACES:', TOTAL_PRESSURE_ITERATIONS, TYPE_MGM_BC
   WRITE(MSG%LU_DEBUG, *) 'MGM%UHL'
   WRITE(MSG%LU_DEBUG, MSG%CFORM1) ((MGM%UHL(I, 1, K), I = 1, L%NX), K = L%NZ, 1, -1)
   WRITE(MSG%LU_DEBUG, *) 'MGM%OUHL'
   WRITE(MSG%LU_DEBUG, MSG%CFORM1) (MGM%OUHL(I), I = 1, L%N_WALL_CELLS_EXT)
#endif

   MGM_FACE_LOOP: DO IFACE = 1, 6

      IOR0 = FACE_ORIENTATION(IFACE)
      F => L%FACE(IOR0)

      MGM_NBR_LOOP: DO INBR = 1, F%N_NEIGHBORS
         
         NOM = F%NEIGHBORS(INBR)
         CALL SCARC_POINT_TO_OTHER_GRID (NM, NOM, NLEVEL_MIN)
         
#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG, *) '------ IOR0: GHOST_FIRSTW, GHOST_LASTW:', IOR0, OL%GHOST_FIRSTW(IOR0), OL%GHOST_LASTW(IOR0)
#endif
         MGM_CELL_LOOP: DO ICG = OL%GHOST_FIRSTW(IOR0), OL%GHOST_LASTW(IOR0)

            IWG = OG%ICG_TO_IWG(ICG)
            ICW = OG%ICG_TO_ICW(ICG, 1)

            IF (ICW == -1) CYCLE

            I = G%ICX(ICW) 
            J = G%ICY(ICW) 
            K = G%ICZ(ICW) 

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, *) '          IWG, ICW, I, J, K :', IWG, ICW, I, J, K, TYPE_GRID
#endif

            SELECT CASE (ITYPE)

               ! Boundary setting along interfaces by simple mean values

               CASE (NSCARC_MGM_BC_MEAN)

                  SELECT CASE (IOR0)
                     CASE ( 1)
                        VAL = 0.5_EB * (UHL(I, J, K) + OUHL(IWG)) 
                     CASE (-1)
                        VAL = 0.5_EB * (UHL(I, J, K) + OUHL(IWG)) 
                     CASE ( 2)
                        VAL = 0.5_EB * (UHL(I, J, K) + OUHL(IWG)) 
                     CASE (-2)
                        VAL = 0.5_EB * (UHL(I, J, K) + OUHL(IWG)) 
                     CASE ( 3)
                        VAL = 0.5_EB * (UHL(I, J, K) + OUHL(IWG)) 
                     CASE (-3)
                        VAL = 0.5_EB * (UHL(I, J, K) + OUHL(IWG)) 
                  END SELECT

               ! Boundary setting along interfaces by extrapolation

               CASE (NSCARC_MGM_BC_EXPOL)

                  SELECT CASE (IOR0)
                     CASE ( 1)
                        VAL = UHL(I, J, K) + OUHL(IWG) - 0.5_EB*(UHL2(I, J, K) + OUHL2(IWG))  
                     CASE (-1)
                        VAL = UHL(I, J, K) + OUHL(IWG) - 0.5_EB*(UHL2(I, J, K) + OUHL2(IWG))  
                     CASE ( 2)
                        VAL = UHL(I, J, K) + OUHL(IWG) - 0.5_EB*(UHL2(I, J, K) + OUHL2(IWG))  
                     CASE (-2)
                        VAL = UHL(I, J, K) + OUHL(IWG) - 0.5_EB*(UHL2(I, J, K) + OUHL2(IWG))  
                     CASE ( 3)
                        VAL = UHL(I, J, K) + OUHL(IWG) - 0.5_EB*(UHL2(I, J, K) + OUHL2(IWG))  
                     CASE (-3)
                        VAL = UHL(I, J, K) + OUHL(IWG) - 0.5_EB*(UHL2(I, J, K) + OUHL2(IWG))  
                  END SELECT

               ! Boundary setting along interfaces by simple mean values

               CASE (NSCARC_MGM_BC_TRUE)

                  HB = 0.0_EB
                  MGM_TRUE_IOR_SELECT: SELECT CASE (IOR0)

                     ! ---------------------------------------
                     CASE( 1)
                        HB( 1) = 0.5_EB * (OUHL(IWG)    + OUHL2(IWG) )
                        HB(-1) = 0.5_EB * (UHL(1, J, K) + UHL(2, J, K))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, 1000) IOR0, IWG, IWG, -1,  -1, IWG, -1, -1, OUHL(IWG),   OUHL2(IWG),  1, HB( 1)
WRITE(MSG%LU_DEBUG, 1000) IOR0, IWG, 1,  J,   K, 2  ,  J,  K, UHL(1, J, K), UHL(2, J, K), -1, HB(-1)
#endif
                        IF (.NOT.TWO_D) THEN
                           IF (BTYPE(IWG, 2) == INTERNAL) THEN
                              HB( 2) = 0.5_EB * (UHL(1, J-1, K) + OUHL(IWG+F%INCR_STENCIL(2)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, 1000) IOR0, IWG, I, J-1, K, IWG+F%INCR_STENCIL(2), -1, -1, UHL(1, J-1, K), &
                          OUHL(IWG+F%INCR_STENCIL(2)), 3, HB(2)
#endif
                           ENDIF
                           IF (BTYPE(IWG, -2) == INTERNAL) THEN
                              HB(-2) = 0.5_EB * (UHL(1, J+1, K) + OUHL(IWG+F%INCR_STENCIL(-2)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, 1000) IOR0, IWG, I, J-1, K, IWG+F%INCR_STENCIL(-2), -1, -1, UHL(1, J-1, K), &
                          OUHL(IWG+F%INCR_STENCIL(-2)), 3, HB(2)
#endif
                           ENDIF
                        ENDIF
                        IF (BTYPE(IWG, 3) == INTERNAL) THEN
                           HB( 3) = 0.5_EB * (UHL(1, J, K-1) + OUHL(IWG+F%INCR_STENCIL(3)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, 1000) IOR0, IWG, I, J, K-1, IWG+F%INCR_STENCIL(3), -1, -1, UHL(1, J, K-1), &
                          OUHL(IWG+F%INCR_STENCIL(3)), 3, HB(3)
#endif
                        ENDIF
                        IF (BTYPE(IWG, -3) == INTERNAL) THEN
                           HB(-3) = 0.5_EB *(UHL(1, J, K+1) + OUHL(IWG+F%INCR_STENCIL(-3)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, 1000) IOR0, IWG, I, J,  K+1, IWG+F%INCR_STENCIL(-3), 1, -1, UHL(1, J, K+1), &
                          OUHL(IWG+F%INCR_STENCIL(-3)), -3, HB(-3)
#endif
                        ENDIF


                     ! ---------------------------------------
                     CASE(-1)
                        HB( 1) = 0.5_EB * (UHL(I-1, J, K) + UHL(I, J, K))
                        HB(-1) = 0.5_EB * (OUHL(IWG)      + OUHL2(IWG) )
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, 1000) IOR0, IWG, I-1, J, K, I, J, K, UHL(I-1, J, K), UHL(I, J, K), 1, HB(1)
WRITE(MSG%LU_DEBUG, 1000) IOR0, IWG, IWG, -1,  -1, IWG, -1, -1, OUHL(IWG), OUHL2(IWG), -1, HB(-1)
#endif
                        IF (.NOT.TWO_D) THEN
                           IF (BTYPE(IWG, 2) == INTERNAL) THEN
                              HB( 2) = 0.5_EB * (UHL(I, J-1, K) + OUHL(IWG+F%INCR_STENCIL(2)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, 1000) IOR0, IWG, I, J-1, K, IWG+F%INCR_STENCIL(2), UHL(I, J-1, K), &
                          OUHL(IWG+F%INCR_STENCIL(2)), 3, HB(2)
#endif
                           ENDIF
                           IF (BTYPE(IWG, -2) == INTERNAL) THEN
                              HB(-2) = 0.5_EB * (UHL(I, J+1, K) + OUHL(IWG+F%INCR_STENCIL(-2)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, 1000) IOR0, IWG, I, J-1, K, IWG+F%INCR_STENCIL(-2), UHL(I, J-1, K), &
                          OUHL(IWG+F%INCR_STENCIL(-2)), 3, HB(2)
#endif
                           ENDIF
                        ENDIF
                        IF (BTYPE(IWG, 3) == INTERNAL) THEN   
                           HB( 3) = 0.5_EB * (UHL(I, J, K-1) + OUHL(IWG+F%INCR_STENCIL(3)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, 1000) IOR0, IWG, I, J, K-1, IWG+F%INCR_STENCIL(3), -1, -1, UHL(I, J, K-1), &
                          OUHL(IWG+F%INCR_STENCIL(3)),  3, HB(3)
#endif
                        ENDIF
                        IF (BTYPE(IWG, -3) == INTERNAL) THEN  
                           HB(-3) = 0.5_EB * (UHL(I, J, K+1) + OUHL(IWG+F%INCR_STENCIL(-3)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, 1000) IOR0, IWG, I, J, K+1, IWG+F%INCR_STENCIL(-3), -1, -1, UHL(I, J, K+1), &
                          OUHL(IWG+F%INCR_STENCIL(-3)), -3, HB(-3)
#endif
                        ENDIF

                     ! ---------------------------------------
                     CASE( 3)
                        WRITE(*,*) 'Not yet done'
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, *) ' 3: Not yet done'
#endif
                     ! ---------------------------------------
                     CASE(-3)
                        WRITE(*,*) 'Not yet done'
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, *) '-3: Not yet done'
#endif
                  END SELECT MGM_TRUE_IOR_SELECT

                  IF (TWO_D) THEN
                     VAL = (L%DXI2*(HB(1)+HB(-1)) + L%DZI2*(HB(3)+HB(-3))) * MGM%WEIGHT(IWG)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, '(A, I6, 5E14.6)') 'IWG, HB(1), HB(-1), HB(3), HB(-3), WEIGHT BC:',&
                                        IWG, HB(1), HB(-1), HB(3), HB(-3), MGM%WEIGHT(IWG)
#endif
                  ELSE
                     VAL = (L%DXI2*(HB(1)+HB(-1)) + L%DYI2*(HB(2)+HB(-2)) + L%DZI2*(HB(3)+HB(-3))) * MGM%WEIGHT(IWG)
                  ENDIF


            END SELECT

            SELECT CASE (IOR0)
               CASE ( 1)
                  MGM%BXS(J,K) = VAL
               CASE (-1)
                  MGM%BXF(J,K) = VAL
               CASE ( 2)
                  MGM%BYS(I,K) = VAL
               CASE (-2)
                  MGM%BYF(I,K) = VAL
               CASE ( 3)
                  MGM%BZS(I,J) = VAL
               CASE (-3)
                  MGM%BZF(I,J) = VAL
            END SELECT

            VB(ICW) = VB(ICW) + F%SCAL_DIRICHLET * VAL    

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'MGM_SET_INTERFACES: B(',ICW,')=', VB(ICW)
#endif
         ENDDO MGM_CELL_LOOP
      ENDDO MGM_NBR_LOOP
   ENDDO MGM_FACE_LOOP
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'MGM%BXS:'
WRITE(MSG%LU_DEBUG,'(2E14.6)') ((MGM%BXS(J,K),J=1,L%NY),K=1,L%NZ)
WRITE(MSG%LU_DEBUG,*) 'MGM%BXF:'
WRITE(MSG%LU_DEBUG,'(2E14.6)') ((MGM%BXF(J,K),J=1,L%NY),K=1,L%NZ)
WRITE(MSG%LU_DEBUG,*) 'MGM%BYS:'
WRITE(MSG%LU_DEBUG,'(9E14.6)') ((MGM%BYS(I,K),I=1,L%NX),K=1,L%NZ)
WRITE(MSG%LU_DEBUG,*) 'MGM%BYF:'
WRITE(MSG%LU_DEBUG,'(9E14.6)') ((MGM%BYF(I,K),I=1,L%NX),K=1,L%NZ)
WRITE(MSG%LU_DEBUG,*) 'MGM%BZS:'
WRITE(MSG%LU_DEBUG,'(9E14.6)') ((MGM%BZS(I,J),I=1,L%NX),J=1,L%NY)
WRITE(MSG%LU_DEBUG,*) 'MGM%BZF:'
WRITE(MSG%LU_DEBUG,'(9E14.6)') ((MGM%BZF(I,J),I=1,L%NX),J=1,L%NY)
#endif
!ENDDO MGM_MESH_LOOP

#ifdef WITH_SCARC_DEBUG
1000 FORMAT('MGM-BC: MEAN : IOR = ',I3, ': IWG =',I3, ': I, J, K =', 3I6, ': H, OH, BC, SCAL*BC = ', 4E14.6)
2000 FORMAT('MGM-BC: EXPOL: IOR = ',I3, ': IWG =',I3, ': I, J, K =', 3I6, ': H, OH, H-1, OH-1, BC, SCAL*BC = ', 6E14.6)
3000 FORMAT('MGM-BC: TRUE : IOR = ',I3, ': IWG: ',I3, ': SET1: ', 3I3, ': SET2: ', 3I3, ': V1, V2: ', 2E14.6, &
            ' : HB(',i3, ')=', E14.6, ': W: ',E14.6)
3100 FORMAT('MGM-BC: TRUE: IOR = ',I6, ' : IW =', I6, ' : I, J, K =', 3I6, ': H, OUHL, OUHL2, BC, SCAL*BC = ', 5E14.6)
#endif
END SUBROUTINE SCARC_MGM_SET_INTERFACES


! --------------------------------------------------------------------------------------------------------------
!> \brief Set BC's along internal obstructions for MGM method
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_MGM_SET_OBSTRUCTIONS(VB)
USE SCARC_POINTERS, ONLY: L, G, MGM, UU, VV, WW, GWC, SCARC_POINT_TO_MGM
REAL(EB), DIMENSION(:), POINTER, INTENT(IN) :: VB
INTEGER:: IW, I, J, K, IOR0, IC
REAL(EB):: VAL

!MGM_MESH_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

!   CALL SCARC_POINT_TO_MGM(NM, NL)
!   G => L%UNSTRUCTURED

#ifdef WITH_SCARC_DEBUG2
   WRITE(MSG%LU_DEBUG, *) '%%%%%%%%%%%%%%%%% MGM-BC: OBSTRUCTION:', TOTAL_PRESSURE_ITERATIONS
   WRITE(MSG%LU_DEBUG, *) 'MGM%U1'
   WRITE(MSG%LU_DEBUG, MSG%CFORM2) ((MGM%U1(I, 1, K), I = 0, L%NX), K = L%NZ, 1, -1)
   WRITE(MSG%LU_DEBUG, *) 'MGM%U2'
   WRITE(MSG%LU_DEBUG, MSG%CFORM2) ((MGM%U2(I, 1, K), I = 0, L%NX), K = L%NZ, 1, -1)
   WRITE(MSG%LU_DEBUG, *) 'MGM%UVEL'
   WRITE(MSG%LU_DEBUG, MSG%CFORM2) ((MGM%UVEL(I, 1, K), I = 0, L%NX), K = L%NZ, 1, -1)
#endif

   MGM_OBST_LOOP: DO IW = L%N_WALL_CELLS_EXT+1, L%N_WALL_CELLS_EXT+L%N_WALL_CELLS_INT
      
      GWC => G%WALL(IW)

      UU  => MGM%U1
      VV  => MGM%V1
      WW  => MGM%W1
      
      I = GWC%IXW
      J = GWC%IYW
      K = GWC%IZW
      
      IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE
      
      IOR0 = GWC%IOR
      IC   = G%CELL_NUMBER(I, J, K)
      
      SELECT CASE (IOR0)
         CASE(1)
            VAL =  L%DXI*DTI*UU(I-1, J, K)
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A, 5i4, 2E14.6)') 'MGM-BC: OBSTRUCTION: IOR =  1: IW, I, J, K, IC, UU(I-1, J, K), B(IC):', &
                                         IW, I-1, J, K, IC, UU(I-1, J, K), VAL
#endif
         CASE(-1)
            VAL = -L%DXI*DTI*UU(I, J, K)
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A, 5i4, 2E14.6)') 'MGM-BC: OBSTRUCTION: IOR = -1: IW, I, J, K, IC, UU(I  , J, K),  B(IC):', &
                                         IW, I, J, K, IC, UU(I, J, K), VAL
#endif
         CASE(2)
            VAL =  L%DYI*DTI*VV(I, J-1, K)

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A, 5i4, 2E14.6)') 'MGM-BC: OBSTRUCTION: IOR =  2: IW, I, J, K, IC, VV(I, J-1, K), B(IC):', &
                                         IW, I, J-1, K, IC, VV(I, J-1, K), VAL
#endif
         CASE(-2)
            VAL = -L%DYI*DTI*VV(I, J, K)
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A, 5i4, 2E14.6)') 'MGM-BC: OBSTRUCTION: IOR = -2: IW, I, J, K, IC, VV(I, J, K),  B(IC):', &
                                         IW, I, J, K, IC, VV(I, J, K), VAL
#endif
         CASE(3)
            VAL =  L%DZI*DTI*WW(I, J, K-1)
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A, 5i4, 2E14.6)') 'MGM-BC: OBSTRUCTION: IOR =  3: IW, I, J, K, IC, WW(I, J, K-1), B(IC):', &
                                         IW, I, J, K-1, IC, WW(I, J, K-1), VAL
#endif
         CASE(-3)
            VAL = -L%DZI*DTI*WW(I, J, K)
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A, 5i4, 2E14.6)') 'MGM-BC: OBSTRUCTION: IOR = -3: IW, I, J, K, IC, WW(I, J, K),  B(IC):', &
                                         IW, I, J, K, IC, WW(I, J, K), VAL
#endif
      END SELECT

      !IF (BFIRST_WORKSPACE) VB(IC) = VB(IC) + VAL             ! Variant A
      VB(IC) = VB(IC) + VAL                                    ! Variant B
      
   ENDDO MGM_OBST_LOOP

#ifdef WITH_SCARC_DEBUG2
      WRITE(MSG%LU_DEBUG, *) 'VB'
      WRITE(MSG%LU_DEBUG, MSG%CFORM1) (VB(IC), IC = 1, G%NC)
#endif

!ENDDO MGM_MESH_LOOP

END SUBROUTINE SCARC_MGM_SET_OBSTRUCTIONS


! --------------------------------------------------------------------------------------------------------------
!> \brief Update velocities after either the first or second pass of the MGM method
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_MGM_UPDATE_VELOCITY(NTYPE)
USE SCARC_POINTERS, ONLY: M, L, GWC, MGM, UU, VV, WW, HP, SCARC_POINT_TO_MGM
INTEGER, INTENT(IN):: NTYPE
INTEGER  :: NM, I, J, K, IW, IOR0

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MGM(NM, NLEVEL_MIN)

#ifdef WITH_SCARC_DEBUG2
   WRITE(MSG%LU_DEBUG, *) '=======================> START OF MGM_UPDATE_VELOCITY:'
   WRITE(MSG%LU_DEBUG, *) 'MGM%U1'
   WRITE(MSG%LU_DEBUG, MSG%CFORM2) ((MGM%U1(I, 1, K), I = 0, M%IBAR), K = M%KBAR, 1, -1)
   WRITE(MSG%LU_DEBUG, *) 'MGM%W1'
   WRITE(MSG%LU_DEBUG, MSG%CFORM1) ((MGM%W1(I, 1, K), I = 1, M%IBAR), K = M%KBAR, 0, -1)
   WRITE(MSG%LU_DEBUG, *) 'MGM%U2'
   WRITE(MSG%LU_DEBUG, MSG%CFORM2) ((MGM%U2(I, 1, K), I = 0, M%IBAR), K = M%KBAR, 1, -1)
   WRITE(MSG%LU_DEBUG, *) 'MGM%W2'
   WRITE(MSG%LU_DEBUG, MSG%CFORM1) ((MGM%W2(I, 1, K), I = 1, M%IBAR), K = M%KBAR, 0, -1)
   WRITE(MSG%LU_DEBUG, *) 'MGM%UVEL'
   WRITE(MSG%LU_DEBUG, MSG%CFORM2) ((MGM%UVEL(I, 1, K), I = 0, M%IBAR), K = M%KBAR, 1, -1)
   WRITE(MSG%LU_DEBUG, *) 'MGM%WVEL'
   WRITE(MSG%LU_DEBUG, MSG%CFORM1) ((MGM%WVEL(I, 1, K), I = 1, M%IBAR), K = M%KBAR, 0, -1)
#endif
   MGM_PART_SELECT: SELECT CASE (NTYPE)
      
      ! Update velocity with new information of previous structured inhomogeneous Poisson solution (SIP)
      
      CASE (NSCARC_MGM_POISSON)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, *) '================================ UPDATE-VELOCITY-POISSON:P: ', PREDICTOR, M%IBAR, M%JBAR, M%KBAR, DT
#endif
         HP => MGM%SIP
         IF (PREDICTOR) THEN
            UU => M%U
            VV => M%V
            WW => M%W
         ELSE
            UU => M%US
            VV => M%VS
            WW => M%WS
         ENDIF

         DO K = 1, M%KBAR
            DO J = 1, M%JBAR
               DO I = 0, M%IBAR
                  MGM%U1(I, J, K) = UU(I, J, K) - DT*( M%FVX(I, J, K) + M%RDXN(I)*(HP(I+1, J, K)-HP(I, J, K)) )
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A, 3i4, 6E14.6)') 'VELO X:P: ',I, J, K, UU(I, J, K), M%FVX(I, J, K), HP(I+1, J, K), HP(I, J, K), &
                                         UU(I, J, K), MGM%U1(I, J, K)
#endif
               ENDDO
            ENDDO
         ENDDO
            
         DO K = 1, M%KBAR
            DO J = 0, M%JBAR
               DO I = 1, M%IBAR
                  MGM%V1(I, J, K) = VV(I, J, K) - DT*( M%FVY(I, J, K) + M%RDYN(J)*(HP(I, J+1, K)-HP(I, J, K)) )
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A, 3i4, 6E14.6)') 'VELO Y:P: ',I, J, K, VV(I, J, K), M%FVY(I, J, K), HP(I, J+1, K), HP(I, J, K), &
                                         VV(I, J, K), MGM%V1(I, J, K)
#endif
               ENDDO
            ENDDO
         ENDDO
            
         DO K = 0, M%KBAR
            DO J = 1, M%JBAR
               DO I = 1, M%IBAR
                  MGM%W1(I, J, K) = WW(I, J, K) - DT*( M%FVZ(I, J, K) + M%RDZN(K)*(HP(I, J, K+1)-HP(I, J, K)) )
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A, 3i4, 6E14.6)') 'VELO Z:P: ',I, J, K, WW(I, J, K), M%FVZ(I, J, K), HP(I, J, K+1), HP(I, J, K), &
                                         WW(I, J, K), MGM%W1(I, J, K)
#endif
               ENDDO
            ENDDO
         ENDDO
            
         MGM%UVEL = MGM%U1
         MGM%VVEL = MGM%V1
         MGM%WVEL = MGM%W1 

      ! Update velocity with new information of previous unstructured homogeneous Laplace solution (UHL)
      
      CASE (NSCARC_MGM_LAPLACE)

         HP => MGM%UHL
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, *) '================================ UPDATE-VELOCITY-LAPLACE:P: ', PREDICTOR, M%IBAR, M%JBAR, M%KBAR, DT
   WRITE(MSG%LU_DEBUG, *) 'MGM%SIP'
   WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%SIP(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
   WRITE(MSG%LU_DEBUG, *) 'MGM%UHL'
   WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%UHL(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
   WRITE(MSG%LU_DEBUG, *) 'MGM%UIP'
   WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((MGM%UIP(I, 1, K), I = 0, M%IBAR+1), K = M%KBAR+1, 0, -1)
WRITE(MSG%LU_DEBUG, *) '------------------------------------------------------------'
#endif
            
         DO K = 1, M%KBAR
            DO J = 1, M%JBAR
               DO I = 0, M%IBAR
                  MGM%U2(I, J, K) = - DT*M%RDXN(I)*(HP(I+1, J, K)-HP(I, J, K))
#ifdef WITH_SCARC_DEBUG2
IF (J == 1) WRITE(MSG%LU_DEBUG, '(A, 3i4, 4E14.6)') 'VELO X:L: ',I, J, K, HP(I+1, J, K), HP(I, J, K), &
                                                     HP(I+1, J, K)-HP(I, J, K), MGM%U2(I, J, K)
#endif
               ENDDO
            ENDDO
         ENDDO

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, *) '------------------------------------------------------------'
#endif
            
         DO K = 1, M%KBAR
            DO J = 0, M%JBAR
               DO I = 1, M%IBAR
                  MGM%V2(I, J, K) = - DT*M%RDYN(J)*(HP(I, J+1, K)-HP(I, J, K))
#ifdef WITH_SCARC_DEBUG2
IF (J == 1) WRITE(MSG%LU_DEBUG, '(A, 3i4, 4E14.6)') 'VELO Y:L: ',I, J, K, HP(I, J+1, K), &
                                                     HP(I, J, K), HP(I, J+1, K)-HP(I, J, K), MGM%V2(I, J, K)
#endif
               ENDDO
            ENDDO
         ENDDO
            
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, *) '------------------------------------------------------------'
#endif

         DO K = 0, M%KBAR
            DO J = 1, M%JBAR
               DO I = 1, M%IBAR
                  MGM%W2(I, J, K) = - DT*M%RDZN(K)*(HP(I, J, K+1)-HP(I, J, K))
#ifdef WITH_SCARC_DEBUG2
IF (J == 1) WRITE(MSG%LU_DEBUG, '(A, 3i4, 4E14.6)') 'VELO Z:L: ',I, J, K, HP(I, J, K+1), &
                                                     HP(I, J, K), HP(I, J, K+1)-HP(I, J, K), MGM%W2(I, J, K)
#endif
               ENDDO
            ENDDO
         ENDDO

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, *) '------------------------------------------------------------'
#endif
            
         ! Recompute velocities on obstruction cells, such that correct normal derivative of Laplace solution is used 
         DO IW = L%N_WALL_CELLS_EXT+1, L%N_WALL_CELLS_EXT+L%N_WALL_CELLS_INT

            GWC => L%UNSTRUCTURED%WALL(IW)
            IF (GWC%BOUNDARY_TYPE /= SOLID_BOUNDARY) CYCLE

            IOR0 = GWC%IOR
            I = GWC%IXW
            J = GWC%IYW
            K = GWC%IZW
            
            SELECT CASE(IOR0)
               CASE( 1)
                  MGM%U2(I-1, J, K) = - MGM%U1(I-1, J, K)
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A, 6I8, E14.6)') 'VELO Y:L: NOGRAD:',IOR0, I-1, J, K, MGM%U2(I-1, J, K)
#endif
               CASE(-1)
                  MGM%U2(I, J, K)   = - MGM%U1(I, J, K) 
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A, 6I8, E14.6)') 'VELO Y:L: NOGRAD:',IOR0, I, J, K, MGM%U2(I, J, K)
#endif
               CASE( 2)
                  MGM%V2(I, J-1, K) = - MGM%V1(I, J-1, K) 
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A, 6I8, E14.6)') 'VELO Y:L: NOGRAD:',IOR0, I, J-1, K, MGM%V2(I, J-1, K)
#endif
               CASE(-2)
                  MGM%V2(I, J, K)   = - MGM%V1(I, J, K) 
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A, 6I8, E14.6)') 'VELO Y:L: NOGRAD:',IOR0, I, J, K, MGM%V2(I, J, K)
#endif
               CASE( 3)
                  MGM%W2(I, J, K-1) = - MGM%W1(I, J, K-1) 
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A, 6I8, E14.6)') 'VELO Y:L: NOGRAD:',IOR0, I, J, K-1, MGM%W2(I, J, K-1)
#endif
               CASE(-3)
                  MGM%W2(I, J, K)   = - MGM%W1(I, J, K) 
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A, 6I8, E14.6)') 'VELO Y:L: NOGRAD:',IOR0, I, J, K, MGM%W2(I, J, K)
#endif
            END SELECT

         ENDDO

         MGM%UVEL = MGM%U1+MGM%U2
         MGM%VVEL = MGM%V1+MGM%V2
         MGM%WVEL = MGM%W1+MGM%W2

   END SELECT MGM_PART_SELECT
ENDDO
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, *) '=======================> END OF MGM_UPDATE_VELOCITY:'
WRITE(MSG%LU_DEBUG, *) 'MGM%U1'
WRITE(MSG%LU_DEBUG, MSG%CFORM2) ((MGM%U1(I, 1, K), I = 0, M%IBAR), K = M%KBAR, 1, -1)
WRITE(MSG%LU_DEBUG, *) 'MGM%W1'
WRITE(MSG%LU_DEBUG, MSG%CFORM1) ((MGM%W1(I, 1, K), I = 1, M%IBAR), K = M%KBAR, 0, -1)
WRITE(MSG%LU_DEBUG, *) '------------------------------------------------------------'
WRITE(MSG%LU_DEBUG, *) 'MGM%U2'
WRITE(MSG%LU_DEBUG, MSG%CFORM2) ((MGM%U2(I, 1, K), I = 0, M%IBAR), K = M%KBAR, 1, -1)
WRITE(MSG%LU_DEBUG, *) 'MGM%W2'
WRITE(MSG%LU_DEBUG, MSG%CFORM1) ((MGM%W2(I, 1, K), I = 1, M%IBAR), K = M%KBAR, 0, -1)
WRITE(MSG%LU_DEBUG, *) '------------------------------------------------------------'
WRITE(MSG%LU_DEBUG, *) 'MGM%UVEL'
WRITE(MSG%LU_DEBUG, MSG%CFORM2) ((MGM%UVEL(I, 1, K), I = 0, M%IBAR), K = M%KBAR, 1, -1)
WRITE(MSG%LU_DEBUG, *) 'MGM%WVEL'
WRITE(MSG%LU_DEBUG, MSG%CFORM1) ((MGM%WVEL(I, 1, K), I = 1, M%IBAR), K = M%KBAR, 0, -1)
WRITE(MSG%LU_DEBUG, *) '------------------------------------------------------------'
#endif
 
END SUBROUTINE SCARC_MGM_UPDATE_VELOCITY


! --------------------------------------------------------------------------------------------------------------
!> \brief Set internal boundary conditions for unstructured, homogeneous part of McKeeney-Greengard-Mayo method
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_MGM_COMPUTE_VELOCITY_ERROR(NTYPE)
USE SCARC_POINTERS, ONLY: M, L, MGM, GWC, EWC, HP, SCARC_POINT_TO_MGM
INTEGER, INTENT(IN) ::  NTYPE
INTEGER:: NM, I, J, K, IW, IOR0, IIO1, IIO2, JJO1, JJO2, KKO1, KKO2, IIO, JJO, KKO, ITYPE
REAL(EB):: UN_NEW_OTHER, UN_NEW, DUDT, DVDT, DWDT
#ifdef WITH_SCARC_DEBUG
INTEGER:: III, KKK, IBAR0, JBAR0, KBAR0
#endif
TYPE(MESH_TYPE), POINTER:: M2
TYPE(OMESH_TYPE), POINTER:: OM

MESH_REAL = 0.0_EB                            
RANK_REAL = 0.0_EB
UN_NEW_OTHER = 0.0_EB

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MGM(NM, NLEVEL_MIN)

   IF (NTYPE == NSCARC_MGM_POISSON) THEN
      HP  => MGM%SIP
   ELSE
      HP  => MGM%UIP
   ENDIF

   MGM%VELOCITY_ERROR = 0.0_EB
#ifdef WITH_SCARC_DEBUG
   IBAR0 = MAX(10, M%IBAR)
   JBAR0 = MAX(10, M%JBAR)
   KBAR0 = MAX(10, M%KBAR)
   WRITE(MSG%LU_DEBUG, *) '=======================> XXXX:MGM_VELOCITY_ERROR', NTYPE, DT
   WRITE(MSG%LU_DEBUG, *) 'HP'
   WRITE(MSG%LU_DEBUG, MSG%CFORM3) ((HP(III, 1, KKK), III = 0, IBAR0+1), KKK = KBAR0+1, 0, -1)
   WRITE(MSG%LU_DEBUG, *) 'UU'
   !WRITE(MSG%LU_DEBUG, MSG%CFORM2) ((UU(III, 1, KKK), III = 0, IBAR0), KKK = KBAR0, 0, -1)
   !WRITE(MSG%LU_DEBUG, *) 'M%U'
   WRITE(MSG%LU_DEBUG, MSG%CFORM2) ((M%U(III, 1, KKK), III = 0, IBAR0), KKK = KBAR0, 0, -1)
   WRITE(MSG%LU_DEBUG, *) 'M%W'
   WRITE(MSG%LU_DEBUG, MSG%CFORM2) ((M%W(III, 1, KKK), III = 0, IBAR0), KKK = KBAR0, 0, -1)
   WRITE(MSG%LU_DEBUG, *) 'M%FVX'
   WRITE(MSG%LU_DEBUG, MSG%CFORM2) ((M%FVX(III, 1, KKK), III = 0, IBAR0), KKK = KBAR0, 0, -1)
   WRITE(MSG%LU_DEBUG, *) 'M%FVZ'
   WRITE(MSG%LU_DEBUG, MSG%CFORM2) ((M%FVZ(III, 1, KKK), III = 0, IBAR0), KKK = KBAR0, 0, -1)
   WRITE(MSG%LU_DEBUG, *) 'L%N_WALL_CELLS_EXT = ', L%N_WALL_CELLS_EXT
   WRITE(MSG%LU_DEBUG, *) 'L%N_WALL_CELLS_INT = ', L%N_WALL_CELLS_INT
#endif

   SELECT CASE (NTYPE)

      ! ------------------------- Poisson case
       
      CASE (NSCARC_MGM_POISSON) 

         WALLCELLS_POISSON_LOOP: DO IW = 1, L%N_WALL_CELLS_EXT+L%N_WALL_CELLS_INT

            GWC => L%STRUCTURED%WALL(IW)                        ! point to structured grid

            IF (GWC%BOUNDARY_TYPE /= SOLID_BOUNDARY         .AND. &
                GWC%BOUNDARY_TYPE /= INTERPOLATED_BOUNDARY) CYCLE

            IOR0 = GWC%IOR

            I = GWC%IXG
            J = GWC%IYG
            K = GWC%IZG

            ! Update normal component of velocity at the mesh boundary

            SELECT CASE(IOR0)
               CASE( 1)
                  UN_NEW = M%U(I, J, K)   - DT*(M%FVX(I, J, K)   + M%RDXN(I)  *(HP(I+1, J, K)-HP(I, J, K)))
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A25, 6I8, 5E14.6)') 'ERR-P 1: UN_NEW:',IW, I, J, K, UN_NEW, MGM%U1(I, J, K), M%FVX(I, J, K), &
                                           HP(I+1, J, K), HP(I, J, K)
#endif
               CASE(-1)
                  UN_NEW = M%U(I-1, J, K) - DT*(M%FVX(I-1, J, K) + M%RDXN(I-1)*(HP(I, J, K)-HP(I-1, J, K)))
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A25, 6I8, 5E14.6)') 'ERR-P-1: UN_NEW: ',IW, I, J, K, UN_NEW, MGM%U1(I-1, J, K), M%FVX(I-1, J, K), &
                                           HP(I, J, K), HP(I-1, J, K)
#endif
               CASE( 2)
                  UN_NEW = M%V(I, J, K)   - DT*(M%FVY(I, J, K)   + M%RDYN(J)  *(HP(I, J+1, K)-HP(I, J, K)))
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A25, 6I8, 5E14.6)') 'ERR-P 2: UN_NEW: ',IW, I, J, K, UN_NEW, MGM%V1(I, J, K), M%FVY(I, J, K), &
                                           HP(I, J+1, K), HP(I, J, K)
#endif
               CASE(-2)
                  UN_NEW = M%V(I, J-1, K) - DT*(M%FVY(I, J-1, K) + M%RDYN(J-1)*(HP(I, J, K)-HP(I, J-1, K)))
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A25, 6I8, 5E14.6)') 'ERR-P-2: UN_NEW: ',IW, I, J, K, UN_NEW, MGM%V1(I, J-1, K), M%FVY(I, J-1, K), &
                                           HP(I, J, K), HP(I, J-1, K)
#endif
               CASE( 3)
                  UN_NEW = M%W(I, J, K)   - DT*(M%FVZ(I, J, K)   + M%RDZN(K)  *(HP(I, J, K+1)-HP(I, J, K)))
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A25, 6I8, 5E14.6)') 'ERR-P 3: UN_NEW: ',IW, I, J, K, UN_NEW, MGM%W1(I, J, K), M%FVZ(I, J, K), &
                                           HP(I, J, K+1), HP(I, J, K)
#endif
               CASE(-3)
                  UN_NEW = M%W(I, J, K-1) - DT*(M%FVZ(I, J, K-1) + M%RDZN(K-1)*(HP(I, J, K)-HP(I, J, K-1)))
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A25, 6I8, 5E14.6)') 'ERR-P-3: UN_NEW: ',IW, I, J, K, UN_NEW, MGM%W1(I, J, K-1), M%FVZ(I, J, K-1), &
                                           HP(I, J, K), HP(I, J, K-1)
#endif
            END SELECT

            IF (M%WALL(IW)%BOUNDARY_TYPE == INTERPOLATED_BOUNDARY) THEN
         
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
                     DO KKO = KKO1, KKO2
                        DO JJO = JJO1, JJO2
                           DO IIO = IIO1, IIO2
                              DUDT = -OM%FVX(IIO, JJO, KKO)   - M2%RDXN(IIO)  *(HP(I+1, J, K)-HP(I, J, K))
                              UN_NEW_OTHER = UN_NEW_OTHER+OM%U(IIO, JJO, KKO)   + DT*DUDT
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A25, 6I8, E14.6, 3I6)') 'P:PRES: 1: UN_NEW_OTHER ', IW, IIO, JJO, KKO, UN_NEW_OTHER, IIO+1, JJO, KKO
#endif
                           ENDDO
                        ENDDO
                     ENDDO
                  CASE(-1)
                     DO KKO = KKO1, KKO2
                        DO JJO = JJO1, JJO2
                           DO IIO = IIO1, IIO2
                              DUDT = -OM%FVX(IIO-1, JJO, KKO) - M2%RDXN(IIO-1)*(HP(I, J, K)-HP(I-1, J, K))
                              UN_NEW_OTHER = UN_NEW_OTHER+OM%U(IIO-1, JJO, KKO) + DT*DUDT
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A25, 6I8, E14.6, 3i4)') 'P:PRES:-1: UN_NEW_OTHER ', IW, IIO-1, JJO, KKO, UN_NEW_OTHER, IIO, JJO, KKO
#endif
                           ENDDO
                        ENDDO
                     ENDDO
                  CASE( 2)
                     DO KKO = KKO1, KKO2
                        DO JJO = JJO1, JJO2
                           DO IIO = IIO1, IIO2
                              DVDT = -OM%FVY(IIO, JJO, KKO)   - M2%RDYN(JJO)  *(HP(I, J+1, K)-HP(I, J, K))
                              UN_NEW_OTHER = UN_NEW_OTHER+OM%V(IIO, JJO, KKO)   + DT*DVDT
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A25, 6I8, E14.6, 3i4)') 'P:PRES: 2: UN_NEW_OTHER ', IW, IIO, JJO, KKO, UN_NEW_OTHER, IIO, JJO+1, KKO
#endif
                           ENDDO
                        ENDDO
                     ENDDO
                  CASE(-2)
                     DO KKO = KKO1, KKO2
                        DO JJO = JJO1, JJO2
                           DO IIO = IIO1, IIO2
                              DVDT = -OM%FVY(IIO, JJO-1, KKO) - M2%RDYN(JJO-1)*(HP(I, J, K)-HP(I, J-1, K))
                              UN_NEW_OTHER = UN_NEW_OTHER+OM%V(IIO, JJO-1, KKO) + DT*DVDT
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A25, 6I8, E14.6, 3i4)') 'P:PRES:-2: UN_NEW_OTHER ', IW, IIO, JJO-1, KKO, UN_NEW_OTHER, IIO, JJO, KKO
#endif
                           ENDDO
                        ENDDO
                     ENDDO
                  CASE( 3)
                     DO KKO = KKO1, KKO2
                        DO JJO = JJO1, JJO2
                           DO IIO = IIO1, IIO2
                              DWDT = -OM%FVZ(IIO, JJO, KKO)   - M2%RDZN(KKO)  *(HP(I, J, K+1)-HP(I, J, K))
                              UN_NEW_OTHER = UN_NEW_OTHER+OM%W(IIO, JJO, KKO)   + DT*DWDT
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A25, 6I8, E14.6, 3i4)') 'P:PRES: 3: UN_NEW_OTHER ', IW, IIO, JJO, KKO, UN_NEW_OTHER, IIO, JJO, KKO+1
#endif
                           ENDDO
                        ENDDO
                     ENDDO
                  CASE(-3)
                     DO KKO = KKO1, KKO2
                        DO JJO = JJO1, JJO2
                           DO IIO = IIO1, IIO2
                              DWDT = -OM%FVZ(IIO, JJO, KKO-1) - M2%RDZN(KKO-1)*(HP(IIO, JJO, KKO)-HP(IIO, JJO, KKO-1))
                              UN_NEW_OTHER = UN_NEW_OTHER+OM%W(IIO, JJO, KKO-1) + DT*DWDT
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A25, 6I8, E14.6, 3i4)') 'P:PRES:-3: UN_NEW_OTHER ', IW, IIO, JJO, KKO-1, UN_NEW_OTHER, IIO, JJO, KKO
#endif
                           ENDDO
                        ENDDO
                     ENDDO
               END SELECT IOR_SELECT_1
            ENDIF

            IF (M%WALL(IW)%BOUNDARY_TYPE == SOLID_BOUNDARY) THEN
               UN_NEW_OTHER = -SIGN(1._EB, REAL(IOR0, EB))*MESHES(NM)%WALL(IW)%ONE_D%U_NORMAL_S
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A25, 6I8, E14.6)') 'VELO-ERR:SP: UN_NEW_OTHER:',  IW, I, J, K, UN_NEW_OTHER
#endif
            ENDIF

            ! Compute velocity difference

            MGM%VELOCITY_ERROR = MAX(MGM%VELOCITY_ERROR, ABS(UN_NEW-UN_NEW_OTHER))

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG, '(A, 6I8, 3E14.6)') '-----------------------------------------------------------------------> FINAL : ',&
                                            IW, I, J, K, UN_NEW, UN_NEW_OTHER, MGM%VELOCITY_ERROR
#endif

         ENDDO WALLCELLS_POISSON_LOOP

      ! ------------------------- Laplace case
       
      CASE (NSCARC_MGM_LAPLACE) 

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, *) 'GWC%BOUNDARY_TYPE:'
WRITE(MSG%LU_DEBUG, '(5I5)') (L%UNSTRUCTURED%WALL(IW)%BOUNDARY_TYPE, IW = 1, L%N_WALL_CELLS_EXT+L%N_WALL_CELLS_INT)
WRITE(MSG%LU_DEBUG, *) 'MGM%UVEL'
WRITE(MSG%LU_DEBUG, MSG%CFORM2) ((MGM%UVEL(I, 1, K), I = 0, M%IBAR), K = M%KBAR, 1, -1)
WRITE(MSG%LU_DEBUG, *) 'MGM%WVEL'
WRITE(MSG%LU_DEBUG, MSG%CFORM1) ((MGM%WVEL(I, 1, K), I = 1, M%IBAR), K = M%KBAR, 0, -1)
WRITE(MSG%LU_DEBUG, *) 'MGM%OUVEL'
WRITE(MSG%LU_DEBUG, '(5E14.6)') (MGM%OUVEL(IW), IW = 1, L%N_WALL_CELLS_EXT)
WRITE(MSG%LU_DEBUG, *) 'MGM%OWVEL'
WRITE(MSG%LU_DEBUG, '(5E14.6)') (MGM%OWVEL(IW), IW = 1, L%N_WALL_CELLS_EXT)
WRITE(MSG%LU_DEBUG, *) 'MGM%VELOCITY_ERROR:', MGM%VELOCITY_ERROR
#endif
         WALLCELLS_LAPLACE_LOOP: DO IW = 1, L%N_WALL_CELLS_EXT+L%N_WALL_CELLS_INT

            GWC => L%UNSTRUCTURED%WALL(IW)                     ! point to unstructured grid

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
                  !UN_NEW = M%U(I, J, K)   - DT*(M%FVX(I, J, K)   + M%RDXN(I)  *(HP(I+1, J, K)-HP(I, J, K))*DHFCT)
                  UN_NEW = MGM%UVEL(I, J, K)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, 1000) IOR0, IW, ITYPE, I, J, K, UN_NEW
#endif
               CASE(-1)
                  !UN_NEW = M%U(I-1, J, K) - DT*(M%FVX(I-1, J, K) + M%RDXN(I-1)*(HP(I, J, K)-HP(I-1, J, K))*DHFCT)
                  UN_NEW = MGM%UVEL(I-1, J, K)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, 1000) IOR0, IW, ITYPE, I-1, J, K, UN_NEW
#endif
               CASE( 2)
                  !UN_NEW = M%V(I, J, K)   - DT*(M%FVY(I, J, K)   + M%RDYN(J)  *(HP(I, J+1, K)-HP(I, J, K))*DHFCT)
                  UN_NEW = MGM%VVEL(I, J, K)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, 1000) IOR0, IW, ITYPE, I, J, K, UN_NEW
#endif
               CASE(-2)
                  !UN_NEW = M%V(I, J-1, K) - DT*(M%FVY(I, J-1, K) + M%RDYN(J-1)*(HP(I, J, K)-HP(I, J-1, K))*DHFCT)
                  UN_NEW = MGM%VVEL(I, J-1, K)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, 1000) IOR0, IW, ITYPE, I, J-1, K, UN_NEW
#endif
               CASE( 3)
                  !UN_NEW = M%W(I, J, K)   - DT*(M%FVZ(I, J, K)   + M%RDZN(K)  *(HP(I, J, K+1)-HP(I, J, K))*DHFCT)
                  UN_NEW = MGM%WVEL(I, J, K) 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, 1000) IOR0, IW, ITYPE, I, J, K, UN_NEW
#endif
               CASE(-3)
                  !UN_NEW = M%W(I, J, K-1) - DT*(M%FVZ(I, J, K-1) + M%RDZN(K-1)*(HP(I, J, K)-HP(I, J, K-1))*DHFCT)
                  UN_NEW = MGM%WVEL(I, J, K-1) 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, 1000) IOR0, IW, ITYPE, I, J, K-1, UN_NEW
#endif
            END SELECT

            IF (GWC%BOUNDARY_TYPE == INTERPOLATED_BOUNDARY) THEN
               SELECT CASE(ABS(IOR0))
                  CASE( 1)
                     UN_NEW_OTHER = MGM%OUVEL(IW)
                  CASE( 2)
                     UN_NEW_OTHER = MGM%OVVEL(IW)
                  CASE( 3)
                     UN_NEW_OTHER = MGM%OWVEL(IW)
               END SELECT
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, 2000) IOR0, IW, ITYPE, I, J, K, UN_NEW_OTHER
#endif
            ENDIF

            IF (GWC%BOUNDARY_TYPE == SOLID_BOUNDARY) THEN
               UN_NEW_OTHER = -SIGN(1._EB, REAL(IOR0, EB))*MESHES(NM)%WALL(IW)%ONE_D%U_NORMAL_S
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, 3000) IOR0, IW, ITYPE, I, J, K, UN_NEW_OTHER
#endif
            ENDIF

            ! Compute velocity difference

            MGM%VELOCITY_ERROR = MAX(MGM%VELOCITY_ERROR, ABS(UN_NEW-UN_NEW_OTHER))

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG, 4000) MGM%VELOCITY_ERROR
#endif

         ENDDO WALLCELLS_LAPLACE_LOOP

   END SELECT

   MESH_REAL(NM) = MGM%VELOCITY_ERROR
   RANK_REAL = MAX(RANK_REAL, MESH_REAL(NM))

ENDDO MESHES_LOOP

IF (N_MPI_PROCESSES > 1) & 
   CALL MPI_ALLREDUCE(MPI_IN_PLACE, RANK_REAL, 1, MPI_DOUBLE_PRECISION, MPI_MAX, MPI_COMM_WORLD, IERROR)
VELOCITY_ERROR_GLOBAL = RANK_REAL

#ifdef WITH_SCARC_DEBUG
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
WRITE(MSG%LU_DEBUG, *) '========================================================================================='
WRITE(MSG%LU_DEBUG, *) '============> ALL MESHES: VELOCITY_ERROR_GLOBAL =', MGM%VELOCITY_ERROR
WRITE(MSG%LU_DEBUG, *) '========================================================================================='
ENDDO 
1000 FORMAT('VE:LAPLACE: OWN : IOR0=',I6, ': IW=',I6, ':ITYPE=', I6, ': I, J, K=', 3I6, ': UN_NEW      :', E14.6)
2000 FORMAT('VE:LAPLACE: NBR : IOR0=',I6, ': IW=',I6, ':ITYPE=', I6, ': I, J, K=', 3I6, ': UN_NEW_OTHER:', E14.6)
3000 FORMAT('VE:LAPLACE: SOL : IOR0=',I6, ': IW=',I6, ':ITYPE=', I6, ': I, J, K=', 3I6, ': UN_NEW_OTHER:', E14.6)
4000 FORMAT('VE:LAPLACE:',83X, '---> FINAL: ', E14.6)
#endif
END SUBROUTINE SCARC_MGM_COMPUTE_VELOCITY_ERROR

END MODULE SCARC_MGM


