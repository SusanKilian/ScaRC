!//////////////////////////////////////////////////////////////////////////////////////////////////////
! 
! MODULE SCARC_METHODS
!
!> \brief Collection of available ScaRC/UScaRC solvers : 
!
!  - Krylov method (without and with coarse grid correction)
!  - McKeeney-Greengard-Mayo method (still experimental)
!  - Geometric Multigrid method
!  - Algebraic Multigrid method
!  - FFT method (Crayfish Pak)
!  - IntelMKL methods (Pardiso/Cluster_Sparse_Solver)
!
!//////////////////////////////////////////////////////////////////////////////////////////////////////
MODULE SCARC_METHODS
  
USE GLOBAL_CONSTANTS
USE PRECISION_PARAMETERS, ONLY: EB, FB
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
USE COMP_FUNCTIONS, ONLY: CURRENT_TIME
USE MPI
USE SCARC_CONSTANTS
USE SCARC_TYPES
USE SCARC_VARIABLES
USE SCARC_MESSAGES
USE SCARC_TIMINGS, ONLY: CPU
USE SCARC_ERRORS
USE SCARC_UTILITIES
USE SCARC_STACK
USE SCARC_VECTORS
USE SCARC_MATRICES
USE SCARC_CONVERGENCE
USE SCARC_GMG
#ifdef WITH_SCARC_AMG
USE SCARC_AMG
#endif
USE SCARC_MGM
USE SCARC_FFT
#ifdef WITH_SCARC_MKL
USE SCARC_MKL
#endif

IMPLICIT NONE

CONTAINS

! -----------------------------------------------------------------------------------------------------------------
!> \brief  Setup environment for Krylov methods
! -----------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_KRYLOV_ENVIRONMENT(NSTACK)
INTEGER, INTENT(INOUT) :: NSTACK

NSTACK = NSCARC_STACK_ROOT
STACK(NSTACK)%SOLVER => MAIN_CG
CALL SCARC_SETUP_KRYLOV(NSCARC_SOLVER_MAIN, NSCARC_SCOPE_GLOBAL, NSCARC_STAGE_ONE, NSTACK, NLEVEL_MIN, NLEVEL_MIN)

 
! Setup preconditioner for Krylov solver
 
NSTACK = NSTACK + 1
SELECT_KRYLOV_PRECON: SELECT CASE (TYPE_PRECON)

   ! Jacobi-preconditioning (acting locally by default)

   CASE (NSCARC_RELAX_JAC)
      STACK(NSTACK)%SOLVER => PRECON_JAC
      CALL SCARC_SETUP_PRECON(NSTACK, NSCARC_SCOPE_LOCAL)

   ! SSOR-preconditioning (acting locally by default)

   CASE (NSCARC_RELAX_SSOR)
      STACK(NSTACK)%SOLVER => PRECON_SSOR
      CALL SCARC_SETUP_PRECON(NSTACK, NSCARC_SCOPE_LOCAL)

   ! JACOBI-preconditioning in matrix form (acting locally by default)

   CASE (NSCARC_RELAX_MJAC)
      STACK(NSTACK)%SOLVER => PRECON_MJAC
      CALL SCARC_SETUP_PRECON(NSTACK, NSCARC_SCOPE_LOCAL)
      CALL SCARC_SETUP_MJAC(NLEVEL_MIN, NLEVEL_MAX)

   ! GS-preconditioning in matrix form (acting locally by default)

   CASE (NSCARC_RELAX_MGS)
      STACK(NSTACK)%SOLVER => PRECON_MGS
      CALL SCARC_SETUP_PRECON(NSTACK, NSCARC_SCOPE_LOCAL)
      CALL SCARC_SETUP_MGS(NLEVEL_MIN, NLEVEL_MAX)

   ! SGS-preconditioning in matrix form (acting locally by default)

   CASE (NSCARC_RELAX_MSGS)
      STACK(NSTACK)%SOLVER => PRECON_MSGS
      CALL SCARC_SETUP_PRECON(NSTACK, NSCARC_SCOPE_LOCAL)
      CALL SCARC_SETUP_MSGS(NLEVEL_MIN, NLEVEL_MAX)

   ! SOR-preconditioning in matrix form (acting locally by default)

   CASE (NSCARC_RELAX_MSOR)
      STACK(NSTACK)%SOLVER => PRECON_MSOR
      CALL SCARC_SETUP_PRECON(NSTACK, NSCARC_SCOPE_LOCAL)
      CALL SCARC_SETUP_MSOR(NLEVEL_MIN, NLEVEL_MAX, NSTACK)

   ! SSOR-preconditioning in matrix form (acting locally by default)

   CASE (NSCARC_RELAX_MSSOR)
      STACK(NSTACK)%SOLVER => PRECON_MSSOR
      CALL SCARC_SETUP_PRECON(NSTACK, NSCARC_SCOPE_LOCAL)
      CALL SCARC_SETUP_MSSOR(NLEVEL_MIN, NLEVEL_MAX, NSTACK)

   ! LU-preconditioning in matrix form (acting locally by default)

   CASE (NSCARC_RELAX_LU)
      STACK(NSTACK)%SOLVER => PRECON_LU
      CALL SCARC_SETUP_PRECON(NSTACK, NSCARC_SCOPE_LOCAL)
      CALL SCARC_SETUP_LU(NLEVEL_MIN, NLEVEL_MAX)

   ! ILU(0)-preconditioning in matrix form (acting locally by default)

   CASE (NSCARC_RELAX_ILU)
      STACK(NSTACK)%SOLVER => PRECON_ILU
      CALL SCARC_SETUP_PRECON(NSTACK, NSCARC_SCOPE_LOCAL)
      CALL SCARC_SETUP_ILU(NLEVEL_MIN, NLEVEL_MAX)

   ! FFT-preconditioning (acting locally by default)

   CASE (NSCARC_RELAX_FFT)
      STACK(NSTACK)%SOLVER => PRECON_FFT
      CALL SCARC_SETUP_PRECON(NSTACK, NSCARC_SCOPE_LOCAL)
      CALL SCARC_SETUP_FFT(NLEVEL_MIN, NLEVEL_MIN)

   ! FFT-preconditioning (acting locally by default)

   CASE (NSCARC_RELAX_FFTO)
      STACK(NSTACK)%SOLVER => PRECON_FFT
      CALL SCARC_SETUP_PRECON(NSTACK, NSCARC_SCOPE_LOCAL)
      CALL SCARC_SETUP_FFTO(NLEVEL_MIN, NLEVEL_MIN)

#ifdef WITH_SCARC_MKL
   ! LU-preconditioning based on MKL (either locally or globally acting depending on user specification)

   CASE (NSCARC_RELAX_MKL)
      STACK(NSTACK)%SOLVER => PRECON_MKL

      SELECT CASE(TYPE_SCOPE(1))

         ! Globally acting - call global CLUSTER_SPARSE_SOLVER from MKL

         CASE (NSCARC_SCOPE_GLOBAL)
            CALL SCARC_SETUP_PRECON(NSTACK, NSCARC_SCOPE_GLOBAL)
            CALL SCARC_SETUP_CLUSTER(NLEVEL_MIN, NLEVEL_MIN)

         ! locally acting - call global PARDISO solver from MKL

         CASE (NSCARC_SCOPE_LOCAL)
            CALL SCARC_SETUP_PRECON(NSTACK, NSCARC_SCOPE_LOCAL)
            CALL SCARC_SETUP_PARDISO(NLEVEL_MIN, NLEVEL_MIN)

      END SELECT
#endif

 
   ! Preconditioning by Geometric multigrid,
   ! either locally or Globally acting, depending on user specification stored in TYPE_SCOPE(1)
 
   CASE (NSCARC_RELAX_MULTIGRID)

      STACK(NSTACK)%SOLVER => PRECON_MG
      CALL SCARC_SETUP_PRECON(NSTACK, TYPE_SCOPE(1))
      CALL SCARC_SETUP_MULTIGRID(NSCARC_SOLVER_PRECON, TYPE_SCOPE(1), NSCARC_STAGE_TWO, NSTACK, &
                                 NLEVEL_MIN, NLEVEL_MAX)

      NSTACK = NSTACK + 1
      SELECT CASE (TYPE_SMOOTH)

         ! Jacobi-smoothing (acting locally by default)

         CASE (NSCARC_RELAX_JAC)
            STACK(NSTACK)%SOLVER => SMOOTH_JAC
            CALL SCARC_SETUP_SMOOTH(NSTACK, NSCARC_SCOPE_LOCAL)

         ! SSOR-smoothing (acting locally by default)

         CASE (NSCARC_RELAX_SSOR)
            STACK(NSTACK)%SOLVER => SMOOTH_SSOR
            CALL SCARC_SETUP_SMOOTH(NSTACK, NSCARC_SCOPE_LOCAL)

         ! Jacobi-preconditioning in matrix form (acting locally by default)

         CASE (NSCARC_RELAX_MJAC)
            STACK(NSTACK)%SOLVER => SMOOTH_MJAC
            CALL SCARC_SETUP_SMOOTH(NSTACK, NSCARC_SCOPE_LOCAL)
            CALL SCARC_SETUP_MJAC(NLEVEL_MIN, NLEVEL_MAX-1)

         ! GS-preconditioning in matrix form (acting locally by default)

         CASE (NSCARC_RELAX_MGS)
            STACK(NSTACK)%SOLVER => SMOOTH_MGS
            CALL SCARC_SETUP_SMOOTH(NSTACK, NSCARC_SCOPE_LOCAL)
            CALL SCARC_SETUP_MGS(NLEVEL_MIN, NLEVEL_MAX-1)

         ! SGS-preconditioning in matrix form (acting locally by default)

         CASE (NSCARC_RELAX_MSGS)
            STACK(NSTACK)%SOLVER => SMOOTH_MSGS
            CALL SCARC_SETUP_SMOOTH(NSTACK, NSCARC_SCOPE_LOCAL)
            CALL SCARC_SETUP_MSGS(NLEVEL_MIN, NLEVEL_MAX-1)

         ! SOR-preconditioning in matrix form (acting locally by default)

         CASE (NSCARC_RELAX_MSOR)
            STACK(NSTACK)%SOLVER => SMOOTH_MSOR
            CALL SCARC_SETUP_SMOOTH(NSTACK, NSCARC_SCOPE_LOCAL)
            CALL SCARC_SETUP_MSOR(NLEVEL_MIN, NLEVEL_MAX-1, NSTACK)

         ! SSOR-preconditioning in matrix form (acting locally by default)

         CASE (NSCARC_RELAX_MSSOR)
            STACK(NSTACK)%SOLVER => SMOOTH_MSSOR
            CALL SCARC_SETUP_SMOOTH(NSTACK, NSCARC_SCOPE_LOCAL)
            CALL SCARC_SETUP_MSSOR(NLEVEL_MIN, NLEVEL_MAX-1, NSTACK)

         ! FFT-smoothing (acting locally by default)

         CASE (NSCARC_RELAX_FFT)
            STACK(NSTACK)%SOLVER => SMOOTH_FFT
            CALL SCARC_SETUP_SMOOTH(NSTACK, NSCARC_SCOPE_LOCAL)
            CALL SCARC_SETUP_FFT(NLEVEL_MIN, NLEVEL_MAX-1)

         ! FFTO-smoothing (acting locally by default)

         CASE (NSCARC_RELAX_FFTO)
            STACK(NSTACK)%SOLVER => SMOOTH_FFTO
            CALL SCARC_SETUP_SMOOTH(NSTACK, NSCARC_SCOPE_LOCAL)
            CALL SCARC_SETUP_FFTO(NLEVEL_MIN, NLEVEL_MAX-1)
#ifdef WITH_SCARC_MKL
         ! LU-smoothing (acting locally by default)

         CASE (NSCARC_RELAX_MKL)
            STACK(NSTACK)%SOLVER => SMOOTH_MKL
            CALL SCARC_SETUP_SMOOTH(NSTACK, NSCARC_SCOPE_LOCAL)
            CALL SCARC_SETUP_PARDISO(NLEVEL_MIN, NLEVEL_MIN)
#endif
      END SELECT

      ! Coarse grid solver (same scope of action as calling GMG)

      NSTACK = NSTACK + 1
      CALL SCARC_SETUP_COARSE_SOLVER(NSCARC_STAGE_TWO, TYPE_SCOPE(1), NSTACK, NLEVEL_MAX, NLEVEL_MAX)

END SELECT SELECT_KRYLOV_PRECON

 
! If two-level Krylov, allocate intermediate structures for interpolation and workspace for global coarse solver
 
IF (HAS_TWO_LEVELS) THEN

   IF (.NOT.IS_CG_AMG) CALL SCARC_SETUP_INTERPOLATION(NSCARC_STAGE_ONE, NLEVEL_MIN+1, NLEVEL_MAX)

   NSTACK = NSTACK + 1
   CALL SCARC_SETUP_COARSE_SOLVER(NSCARC_STAGE_ONE, NSCARC_SCOPE_GLOBAL, NSTACK, NLEVEL_MAX, NLEVEL_MAX)

ENDIF

END SUBROUTINE SCARC_SETUP_KRYLOV_ENVIRONMENT


! ------------------------------------------------------------------------------------------------
!> \brief Perform global conjugate gradient method based on global Possion-matrix
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_METHOD_KRYLOV(NSTACK, NPARENT, NRHS, NLEVEL)
USE SCARC_CONVERGENCE
INTEGER, INTENT(IN) :: NSTACK, NPARENT, NRHS, NLEVEL
INTEGER :: NSTATE, NS, NP, NL, NG
REAL (EB) :: ALPHA0, BETA0, SIGMA0, SIGMA1=0.0_EB
REAL (EB) :: TNOW, TNOWI

TNOW = CURRENT_TIME()
ITE_CG = 0

! Get current and parent stack position, and current level
TYPE_MATVEC = NSCARC_MATVEC_GLOBAL
NS  = NSTACK
NP  = NPARENT
NL  = NLEVEL
NG = TYPE_GRID


#ifdef WITH_SCARC_POSTPROCESSING
IF (ICYC == 1) THEN
   CALL SCARC_DUMP_SYSTEM(NS, NSCARC_DUMP_MESH)
   CALL SCARC_DUMP_SYSTEM(NS, NSCARC_DUMP_A)
ENDIF
#endif

! ---------- Initialization:
!   - Get parameters for current scope (note: NL denotes the finest level)
!   - Get right hand side vector and clear solution vectors

CALL SCARC_SETUP_SOLVER(NS, NP)
TYPE_GRID = NG

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '====================== BEGIN KRYLOV METHOD 'SIGMA0
WRITE(MSG%LU_DEBUG,*) 'NSTACK  =', NSTACK
WRITE(MSG%LU_DEBUG,*) 'NPARENT =', NPARENT
WRITE(MSG%LU_DEBUG,*) 'NRHS    =', NRHS
WRITE(MSG%LU_DEBUG,*) 'NLEVEL  =', NLEVEL
WRITE(MSG%LU_DEBUG,*) 'TYPE_GRID  =', TYPE_GRID
WRITE(MSG%LU_DEBUG,*) 'TYPE_MATVEC  =', TYPE_MATVEC
WRITE(MSG%LU_DEBUG,*) 'TYPE_PRECON  =', TYPE_PRECON
WRITE(MSG%LU_DEBUG,*) 'ITYPE  =', STACK(NS)%SOLVER%TYPE_RELAX
WRITE(MSG%LU_DEBUG,*) 'PRES_ON_WHOLE_DOMAIN =', PRES_ON_WHOLE_DOMAIN
WRITE(MSG%LU_DEBUG,*) 'IS_STRUCTURED = ', IS_STRUCTURED
WRITE(MSG%LU_DEBUG,*) 'IS_UNSTRUCTURED = ', IS_UNSTRUCTURED
WRITE(MSG%LU_DEBUG,*) 'IS_LAPLACE = ', IS_LAPLACE
WRITE(MSG%LU_DEBUG,*) 'IS_POISSON = ', IS_POISSON
#endif

CALL SCARC_SETUP_WORKSPACE(NS, NL, NRHS)

!CALL SCARC_PRESET_VECTOR(B, NL)

! In case of pure Neumann boundary conditions setup condensed system

IF (N_DIRIC_GLOBAL(NLEVEL_MIN) == 0) THEN
   CALL SCARC_VECTOR_INIT (X, 0.0_EB, NL)                    
   CALL SCARC_FILTER_MEANVALUE(B, NL)                       
   CALL SCARC_SETUP_SYSTEM_CONDENSED (B, NL, 1)            
ENDIF

#ifdef WITH_SCARC_POSTPROCESSING
CALL SCARC_DUMP_SYSTEM(NS, NSCARC_DUMP_B)
#endif

#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (X, 'CG-METHOD: X INIT0 ', NL)
CALL SCARC_DEBUG_LEVEL (B, 'CG-METHOD: B INIT0 ', NL)
CALL SCARC_DEBUG_METHOD('BEGIN OF KRYLOV METHOD ',7)                     
#endif

! Compute initial residual 

IF (IS_MGM .AND. NSTACK == 3) THEN
   TYPE_MATVEC = NSCARC_MATVEC_LOCAL
ELSE
   TYPE_MATVEC = NSCARC_MATVEC_GLOBAL
ENDIF
CALL SCARC_MATVEC_PRODUCT (X, R, NL)                         !  r^0 := A*x^0
CALL SCARC_VECTOR_SUM     (B, R, -1.0_EB, 1.0_EB, NL)        !  r^0 := r^0 - b     corresponds to  A*x^0 - b

RES    = SCARC_L2NORM (R, NL)                                !  res   := ||r^0||
RESIN  = RES                                                 !  resin := res
NSTATE = SCARC_CONVERGENCE_STATE (0, NS, NL)                 !  res < tolerance ?

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SUSI: KRYLOV: NSTACK:', NSTACK, ': TYPE_MATVEC=', TYPE_MATVEC
CALL SCARC_DEBUG_LEVEL (X, 'CG-METHOD: X INIT1 ', NL)
CALL SCARC_DEBUG_LEVEL (B, 'CG-METHOD: B INIT1 ', NL)
#endif

! Perform initial preconditioning

IF (NSTATE /= NSCARC_STATE_CONV_INITIAL) THEN                !  if no convergence yet, call intial preconditioner
   CALL SCARC_PRECONDITIONER(NS, NS, NL)                     !  v^0 := Precon(r^0)
   SIGMA1 = SCARC_SCALAR_PRODUCT(R, V, NL)                   !  SIGMA1 := (r^0,v^0)
#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (R, 'CG-METHOD: R INIT1 ', NL)
CALL SCARC_DEBUG_LEVEL (V, 'CG-METHOD: V INIT1 ', NL)
WRITE(MSG%LU_DEBUG,*) 'SIGMA1=', SIGMA1
#endif
   CALL SCARC_VECTOR_COPY (V, D, -1.0_EB, NL)                !  d^0 := -v^0
ENDIF

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'RESIN, RES, ITE, SIGMA1:', RESIN, RES, ITE, SIGMA1
CALL SCARC_DEBUG_LEVEL (D, 'CG-METHOD: D INIT1 ', NL)
#endif


! ---------- Perform conjugate gradient looping

CG_LOOP: DO ITE = 1, NIT

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '========================> CG : ITE =', ITE
#endif
   TNOWI = CURRENT_TIME()
   CALL SCARC_INCREASE_ITERATION_COUNTS(ITE)

   !TYPE_MATVEC = NSCARC_MATVEC_LOCAL
   CALL SCARC_MATVEC_PRODUCT (D, Y, NL)                      !  y^k := A*d^k

   ALPHA0 = SCARC_SCALAR_PRODUCT (D, Y, NL)                   !  ALPHA0 := (d^k,y^k)     corresponds to   (d^k,A*d^k)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'ALPHA0, SIGMA1=', ALPHA0, SIGMA1
CALL SCARC_DEBUG_LEVEL (Y, 'CG-METHOD: Y AFTER MAT-VEC ', NL)
#endif

   ALPHA0 = SIGMA1/ALPHA0                                      !  ALPHA0 := (r^k,v^k)/(d^k,A*d^k)

   CALL SCARC_VECTOR_SUM (D, X, ALPHA0, 1.0_EB, NL)           !  x^{k+1} := x^k + ALPHA0 * d^k
   CALL SCARC_VECTOR_SUM (Y, R, ALPHA0, 1.0_EB, NL)           !  r^{k+1} := r^k + ALPHA0 * y^k   ~  r^k + ALPHA0 * A * d^k

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'ITE, ITE_CG=', ITE, ITE_CG
CALL SCARC_DEBUG_LEVEL (X, 'CG-METHOD: X ITE ', NL)
CALL SCARC_DEBUG_LEVEL (Y, 'CG-METHOD: Y ITE ', NL)
CALL SCARC_DEBUG_LEVEL (R, 'CG-METHOD: R ITE ', NL)
WRITE(MSG%LU_DEBUG,*) '======================> CG : ITE2 =', ITE
#endif

   RES = SCARC_L2NORM (R, NL)                                !  res := ||r^{k+1}||
   NSTATE = SCARC_CONVERGENCE_STATE (0, NS, NL)              !  res < tolerance ??
   IF (NSTATE /= NSCARC_STATE_PROCEED) EXIT CG_LOOP

   CALL SCARC_PRECONDITIONER(NS, NS, NL)                     !  v^{k+1} := Precon(r^{k+1})

   SIGMA0 = SCARC_SCALAR_PRODUCT (R, V, NL)                  !  SIGMA0 := (r^{k+1},v^{k+1})
   BETA0  = SIGMA0/SIGMA1                                     !  BETA0  := (r^{k+1},v^{k+1})/(r^k,v^k)
   SIGMA1 = SIGMA0                                            !  save last SIGMA0

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '======================> CG : ITE3 =', ITE
CALL SCARC_DEBUG_LEVEL (V, 'CG-METHOD: V ITE ', NL)
CALL SCARC_DEBUG_LEVEL (D, 'CG-METHOD: D ITE ', NL)
#endif

   CALL SCARC_VECTOR_SUM (V, D, -1.0_EB, BETA0, NL)           !  d^{k+1} := -v^{k+1} + BETA0 * d^{k+1}

   CPU(MYID)%ITERATION=MAX(CPU(MYID)%ITERATION,CURRENT_TIME()-TNOWI)

ENDDO CG_LOOP

! ---------- Determine convergence rate and print corresponding information
! In case of CG as main solver:
!   - Transfer ScaRC solution vector X to FDS pressure vector
!   - Set ghost cell values along external boundaries
!   - Exchange values along internal boundaries

CALL SCARC_CONVERGENCE_RATE(NSTATE, NS, NL)

IF (N_DIRIC_GLOBAL(NLEVEL_MIN) == 0) THEN
   CALL SCARC_RESTORE_LAST_CELL(X, NL)
   CALL SCARC_FILTER_MEANVALUE(X, NL)
ENDIF

#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (X, 'CG-METHOD: X FINAL', NL)
WRITE(MSG%LU_DEBUG,*) '=======================>> CG : END =', ITE
#endif

IF (TYPE_SOLVER == NSCARC_SOLVER_MAIN .AND. .NOT.IS_MGM) THEN
   CALL SCARC_UPDATE_MAINCELLS(NLEVEL_MIN)
   CALL SCARC_UPDATE_GHOSTCELLS(NLEVEL_MIN)
#ifdef WITH_SCARC_POSTPROCESSING
   CALL SCARC_PRESSURE_DIFFERENCE(NLEVEL_MIN)
   CALL SCARC_DUMP_SYSTEM(NS, NSCARC_DUMP_X)
#endif
ENDIF

#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_METHOD('END OF KRYLOV METHOD ',6)                     
#endif

CALL SCARC_RELEASE_SOLVER(NS, NP)

END SUBROUTINE SCARC_METHOD_KRYLOV


! ------------------------------------------------------------------------------------------------
!> \brief Setup environment needed for the use of the McKenney-Greengard-Mayo method
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MGM_ENVIRONMENT(NSTACK)
INTEGER, INTENT(INOUT) :: NSTACK

! Allocate velocity vectors along internal obstructions for the setting of internal BC's

CALL SCARC_SETUP_MGM(NLEVEL_MIN, NLEVEL_MIN)

! ------- First part of method: Setup CG solver for inhomogeneous problem on structured discretization
!         Use FFT-preconditioning by default

WRITE(*,*) 'CAUTION: TODO: PRECON SCARC MGM!'
TYPE_PRECON = NSCARC_RELAX_FFT
TYPE_PRECON = NSCARC_RELAX_SSOR
CALL SCARC_SET_GRID_TYPE(NSCARC_GRID_STRUCTURED)

NSTACK = NSCARC_STACK_ROOT
STACK(NSTACK)%SOLVER => MAIN_CG_STRUCTURED
CALL SCARC_SETUP_KRYLOV(NSCARC_SOLVER_MAIN, NSCARC_SCOPE_GLOBAL, NSCARC_STAGE_ONE, NSTACK, NLEVEL_MIN, NLEVEL_MIN)

NSTACK = NSTACK + 1
STACK(NSTACK)%SOLVER => PRECON_FFT
CALL SCARC_SETUP_PRECON(NSTACK, NSCARC_SCOPE_LOCAL)
CALL SCARC_SETUP_FFT(NLEVEL_MIN, NLEVEL_MIN)


! ------- Second part of method: Setup CG solver for homogeneous problem on unstructured discretization
!         Only working for compact matrix storage technique (because of the unstructured grid)
!         Use LU-preconditioning by default

WRITE(*,*) 'CAUTION: TODO: PRECON USCARC MGM!'
TYPE_PRECON = NSCARC_RELAX_SSOR
TYPE_MATRIX = NSCARC_MATRIX_COMPACT
CALL SCARC_SET_GRID_TYPE(NSCARC_GRID_UNSTRUCTURED)

NSTACK = NSTACK + 1
STACK(NSTACK)%SOLVER => MAIN_CG_UNSTRUCTURED
CALL SCARC_SETUP_KRYLOV(NSCARC_SOLVER_MAIN, NSCARC_SCOPE_GLOBAL, NSCARC_STAGE_ONE, NSTACK, NLEVEL_MIN, NLEVEL_MIN)

NSTACK = NSTACK + 1
IF (TYPE_PRECON == NSCARC_RELAX_MKL .AND. TYPE_MATRIX == NSCARC_MATRIX_COMPACT) THEN
#ifdef WITH_SCARC_MKL
   STACK(NSTACK)%SOLVER => PRECON_MKL
   CALL SCARC_SETUP_PRECON(NSTACK, NSCARC_SCOPE_LOCAL)
   CALL SCARC_SETUP_PARDISO(NLEVEL_MIN, NLEVEL_MIN)          ! use global PARDISO from MKL
#else
   WRITE(*,*) 'MGM-method: MKL-preconditioning required, MKL library not available, using LU preconditioning'
   STACK(NSTACK)%SOLVER => PRECON_SSOR
   CALL SCARC_SETUP_PRECON(NSTACK, NSCARC_SCOPE_LOCAL)
   !CALL SCARC_SETUP_LU(NLEVEL_MIN, NLEVEL_MIN)
#endif

     ELSE
   STACK(NSTACK)%SOLVER => PRECON_SSOR
   CALL SCARC_SETUP_PRECON(NSTACK, NSCARC_SCOPE_LOCAL)
   !CALL SCARC_SETUP_LU(NLEVEL_MIN, NLEVEL_MAX)
 ENDIF

END SUBROUTINE SCARC_SETUP_MGM_ENVIRONMENT


! ------------------------------------------------------------------------------------------------
!> \brief Perform global conjugate gradient method based on global Possion-matrix
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_METHOD_MGM(NSTACK)
INTEGER, INTENT(IN) :: NSTACK
INTEGER :: ITE_MGM, STATE_MGM
LOGICAL :: COMPARE_SCARC_VS_USCARC = .TRUE., USE_OVERLAPS = .TRUE.

CALL SCARC_SETUP_MGM_WORKSPACE(NLEVEL_MIN)

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'MGM-METHOD: START, TPI=', TOTAL_PRESSURE_ITERATIONS
#endif
!
! Pass 1: Solve structured inhomogeneous Poisson solution
!
CALL SCARC_SET_SYSTEM_TYPE (NSCARC_GRID_STRUCTURED, NSCARC_MATRIX_POISSON)
CALL SCARC_METHOD_KRYLOV (NSTACK, NSCARC_STACK_ZERO, NSCARC_RHS_INHOMOGENEOUS, NLEVEL_MIN)

CALL SCARC_MGM_STORE_SOLUTION (NSCARC_MGM_POISSON)             ! store solution in MGM%H1
CALL SCARC_MGM_UPDATE_GHOSTCELLS (NSCARC_MGM_POISSON)

CALL SCARC_MGM_COPY (NSCARC_MGM_COPY_H1_TO_H3)             ! first use MGM%H1 as solution MGM%H3
CALL SCARC_MGM_UPDATE_VELOCITY (NSCARC_MGM_POISSON)
CALL SCARC_MGM_COMPUTE_VELOCITY_ERROR (NSCARC_MGM_POISSON)

#ifdef WITH_SCARC_DEBUG
CALL SCARC_MGM_DUMP('H1',0)
CALL SCARC_MGM_DUMP('H3',0)
#endif

STATE_MGM = SCARC_MGM_CONVERGENCE_STATE(0)
   
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'MGM-METHOD: AFTER POISSON ITE, CAPPA, TPI=', ITE, CAPPA, STATE_MGM, &
                          TOTAL_PRESSURE_ITERATIONS, VELOCITY_ERROR_GLOBAL
   CALL SCARC_DEBUG_METHOD ('PART1 of MGM: AFTER POISSON SOLUTION',2)                     
#endif
   
! If requested accuracy already reached, reset method type (which has been changed during Krylov method) to MGM and leave
IF (STATE_MGM == NSCARC_MGM_SUCCESS) THEN
   
   TYPE_METHOD = NSCARC_METHOD_MGM                      
   
!
! Pass 2: Solve local homogeneous Laplace problems:
! Perform iteration based on the solution of local homogeneous Laplace problems
! As BC's to neighbors simple mean values of the previous Laplaces solutions along interfaces are used
!
ELSE
   
   ! If comparison with correct UScaRC method is selected, also compute UScaRC solution
   ! Store ScaRC solution in MGM%HS, UScaRC soltution in MGM%HU and difference of both in MGM%HD
   ! All contain correct external BC's and ghost cells
   IF (COMPARE_SCARC_VS_USCARC) THEN
   
      CALL SCARC_MGM_STORE_SOLUTION (NSCARC_MGM_SCARC)       ! store structured solution in HS
      CALL SCARC_MGM_UPDATE_GHOSTCELLS (NSCARC_MGM_SCARC)
   
      CALL SCARC_SET_SYSTEM_TYPE (NSCARC_GRID_UNSTRUCTURED, NSCARC_MATRIX_POISSON)
      CALL SCARC_METHOD_KRYLOV (NSTACK, NSCARC_STACK_ZERO, NSCARC_RHS_INHOMOGENEOUS, NLEVEL_MIN)

      CALL SCARC_MGM_STORE_SOLUTION (NSCARC_MGM_USCARC)      ! store unstructured solution in HU
      CALL SCARC_MGM_UPDATE_GHOSTCELLS (NSCARC_MGM_USCARC)

      CALL SCARC_MGM_STORE_SOLUTION (NSCARC_MGM_DIFFERENCE)  ! build difference HD = HU - HS
   
#ifdef WITH_SCARC_DEBUG
      CALL SCARC_MGM_DUMP('HS',0)
      CALL SCARC_MGM_DUMP('HU',0)
      CALL SCARC_MGM_DUMP('HD',0)
#endif

#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG,*) 'MGM-METHOD: AFTER COMPARISON, TPI=', TOTAL_PRESSURE_ITERATIONS
      CALL SCARC_DEBUG_METHOD('PART0 in MGM: DIFFERENCE SCARC VS USCARC',5)                 
#endif

   ENDIF

   ! If very first pressure solution ever, then use UScaRC solution as final solution and
   ! store difference of ScaRC and UScaRC for the definition of the interface BC's in next pressure solution
   IF (NMESHES > 1 .AND. ( (TOTAL_PRESSURE_ITERATIONS <= 1) .OR. &
                           (TOTAL_PRESSURE_ITERATIONS <= 2  .AND.TYPE_MGM_BC == NSCARC_MGM_BC_EXPOL) ) ) THEN

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'MGM-METHOD: VERY FIRST ITERATION, TPI=', TOTAL_PRESSURE_ITERATIONS, TYPE_MGM_BC
#endif

       CALL SCARC_MGM_COPY (NSCARC_MGM_COPY_HD_TO_H2)   
       CALL SCARC_MGM_COPY (NSCARC_MGM_COPY_HU_TO_H3)   

       IF (TYPE_MGM_BC == NSCARC_MGM_BC_TRUE) THEN
           CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MGM_TRUE, NSCARC_NONE, NLEVEL_MIN)
       ELSE 
           CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MGM_MEAN, NSCARC_NONE, NLEVEL_MIN)
       ENDIF
       IF (TYPE_MGM_BC == NSCARC_MGM_BC_EXPOL .AND. TOTAL_PRESSURE_ITERATIONS == 1) THEN
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'MGM-METHOD: SAVING ALSO H4'
#endif
          CALL SCARC_MGM_COPY (NSCARC_MGM_COPY_H2_TO_H4)
          CALL SCARC_MGM_COPY (NSCARC_MGM_COPY_OH1_TO_OH2)
       ENDIF

#ifdef WITH_SCARC_DEBUG
      CALL SCARC_MGM_DUMP('H2',0)
      CALL SCARC_MGM_DUMP('H3',0)
#endif

    ! Otherwise define BC's along obstructions based on MGM-logic and compute correction by Laplace solution
    ! Define BC's along mesh interfaces by 'simple mean' or 'true approximate' based on previous Laplace solutions
   ELSE

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'MGM-METHOD: REST OF ITERATIONS, TPI=', TOTAL_PRESSURE_ITERATIONS
#endif

      MGM_CORRECTION_LOOP: DO ITE_MGM = 1, SCARC_MGM_ITERATIONS
      

#ifdef WITH_SCARC_DEBUG
         WRITE(MSG%LU_DEBUG,*) '=============> SUSI: STARTING MGM-iteration ', ITE_MGM, TOTAL_PRESSURE_ITERATIONS
#endif
         CALL SCARC_SET_SYSTEM_TYPE (NSCARC_GRID_UNSTRUCTURED, NSCARC_MATRIX_LAPLACE)
         CALL SCARC_METHOD_KRYLOV (NSTACK+2, NSCARC_STACK_ZERO, NSCARC_RHS_HOMOGENEOUS, NLEVEL_MIN)

         IF (SCARC_MGM_USE_LU) CALL SCARC_METHOD_MGM_LU(NSTACK+2, NLEVEL_MIN)
      
         IF (TYPE_MGM_BC == NSCARC_MGM_BC_EXPOL) THEN
            CALL SCARC_MGM_COPY (NSCARC_MGM_COPY_H2_TO_H4)
            CALL SCARC_MGM_COPY (NSCARC_MGM_COPY_OH1_TO_OH2)
         ENDIF

         CALL SCARC_MGM_STORE_SOLUTION (NSCARC_MGM_LAPLACE)

         IF (USE_OVERLAPS) CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MGM_MEAN, NSCARC_NONE, NLEVEL_MIN)
         CALL SCARC_MGM_UPDATE_GHOSTCELLS (NSCARC_MGM_LAPLACE)


         CALL SCARC_MGM_STORE_SOLUTION (NSCARC_MGM_MERGE)
   
#ifdef WITH_SCARC_DEBUG
         CALL SCARC_MGM_DUMP('H2',ITE_MGM)
         CALL SCARC_MGM_DUMP('H3',ITE_MGM)
#endif

#ifdef WITH_SCARC_DEBUG
         WRITE(MSG%LU_DEBUG,*) 'MGM-METHOD AFTER LAPLACE, TPI=', TOTAL_PRESSURE_ITERATIONS
         CALL SCARC_DEBUG_METHOD('PART3 of MGM: AFTER LAPLACE SOLUTION',2)                 
#endif
   
         CALL SCARC_MGM_UPDATE_VELOCITY (NSCARC_MGM_LAPLACE)
         CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MGM_VELO, NSCARC_NONE, NLEVEL_MIN)
         CALL SCARC_MGM_COMPUTE_VELOCITY_ERROR (NSCARC_MGM_LAPLACE)
   
         STATE_MGM = SCARC_MGM_CONVERGENCE_STATE(ITE_MGM)
   
#ifdef WITH_SCARC_DEBUG
         WRITE(MSG%LU_DEBUG,*) 'MGM-METHOD AFTER VELOCITY-ERROR, TPI=', TOTAL_PRESSURE_ITERATIONS, ITE_MGM, VELOCITY_ERROR_GLOBAL
         CALL SCARC_DEBUG_METHOD('PART4 of MGM: AFTER MERGE ',2)                            
#endif
         CALL SCARC_MGM_COPY (NSCARC_MGM_DIFF_H2_VS_HD)
         CALL SCARC_MGM_COPY (NSCARC_MGM_DIFF_H3_VS_HU)
         IF (STATE_MGM == NSCARC_MGM_SUCCESS) EXIT MGM_CORRECTION_LOOP
   
         IF (TYPE_MGM_BC == NSCARC_MGM_BC_TRUE) THEN
            CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MGM_TRUE, NSCARC_NONE, NLEVEL_MIN)
         ELSE 
            CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MGM_MEAN, NSCARC_NONE, NLEVEL_MIN)
         ENDIF
   
      ENDDO MGM_CORRECTION_LOOP
      
      STATE_MGM = SCARC_MGM_CONVERGENCE_STATE(-1)
   
   ENDIF
ENDIF

TYPE_METHOD = NSCARC_METHOD_MGM
CALL SCARC_MGM_STORE_SOLUTION (NSCARC_MGM_TERMINATE)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'MGM-METHOD FINISHED: TYPE_METHOD, TPI = ', TYPE_METHOD, TOTAL_PRESSURE_ITERATIONS, VELOCITY_ERROR_GLOBAL
CALL SCARC_DEBUG_METHOD('PART6 of MGM: LEAVING SCARC ',1)                         
#endif

END SUBROUTINE SCARC_METHOD_MGM


! ------------------------------------------------------------------------------------------------
!> \brief Setup environment for multigrid method
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MULTIGRID_ENVIRONMENT(NSTACK)
INTEGER, INTENT(INOUT) :: NSTACK

NSTACK = NSCARC_STACK_ROOT
STACK(NSTACK)%SOLVER => MAIN_GMG
CALL SCARC_SETUP_MULTIGRID(NSCARC_SOLVER_MAIN, NSCARC_SCOPE_GLOBAL, NSCARC_STAGE_ONE, NSTACK, NLEVEL_MIN, NLEVEL_MAX)

NSTACK = NSTACK + 1
SELECT CASE(TYPE_SMOOTH)

   ! Jacobi-smoothing (acting locally by default)

   CASE (NSCARC_RELAX_JAC)
      STACK(NSTACK)%SOLVER => SMOOTH_JAC
      CALL SCARC_SETUP_SMOOTH(NSTACK, NSCARC_SCOPE_LOCAL)

   ! SSOR-smoothing (acting locally by default)

   CASE (NSCARC_RELAX_SSOR)
      STACK(NSTACK)%SOLVER => SMOOTH_SSOR
      CALL SCARC_SETUP_SMOOTH(NSTACK, NSCARC_SCOPE_LOCAL)

   ! Jacobi-preconditioning in matrix form (acting locally by default)

   CASE (NSCARC_RELAX_MJAC)
      STACK(NSTACK)%SOLVER => SMOOTH_MJAC
      CALL SCARC_SETUP_SMOOTH(NSTACK, NSCARC_SCOPE_LOCAL)
      CALL SCARC_SETUP_MJAC(NLEVEL_MIN, NLEVEL_MAX)

    ! GS-preconditioning in matrix form (acting locally by default)

   CASE (NSCARC_RELAX_MGS)
      STACK(NSTACK)%SOLVER => SMOOTH_MGS
      CALL SCARC_SETUP_SMOOTH(NSTACK, NSCARC_SCOPE_LOCAL)
      CALL SCARC_SETUP_MGS(NLEVEL_MIN, NLEVEL_MAX)

   ! SGS-preconditioning in matrix form (acting locally by default)

   CASE (NSCARC_RELAX_MSGS)
      STACK(NSTACK)%SOLVER => SMOOTH_MSGS
      CALL SCARC_SETUP_SMOOTH(NSTACK, NSCARC_SCOPE_LOCAL)
      CALL SCARC_SETUP_MSGS(NLEVEL_MIN, NLEVEL_MAX)

   ! SOR-preconditioning in matrix form (acting locally by default)

   CASE (NSCARC_RELAX_MSOR)
      STACK(NSTACK)%SOLVER => SMOOTH_MSOR
      CALL SCARC_SETUP_SMOOTH(NSTACK, NSCARC_SCOPE_LOCAL)
      CALL SCARC_SETUP_MSOR(NLEVEL_MIN, NLEVEL_MAX, NSTACK)

   ! SSOR-preconditioning in matrix form (acting locally by default)

   CASE (NSCARC_RELAX_MSSOR)
      STACK(NSTACK)%SOLVER => SMOOTH_MSSOR
      CALL SCARC_SETUP_SMOOTH(NSTACK, NSCARC_SCOPE_LOCAL)
      CALL SCARC_SETUP_MSSOR(NLEVEL_MIN, NLEVEL_MAX, NSTACK)

   ! FFT-smoothing (acting locally by default)

   CASE (NSCARC_RELAX_FFT)
      STACK(NSTACK)%SOLVER => SMOOTH_FFT
      CALL SCARC_SETUP_SMOOTH(NSTACK, NSCARC_SCOPE_LOCAL)
      CALL SCARC_SETUP_FFT(NLEVEL_MIN, NLEVEL_MAX-1)

   ! FFTO-smoothing (acting locally by default)

   CASE (NSCARC_RELAX_FFTO)
      STACK(NSTACK)%SOLVER => SMOOTH_FFTO
      CALL SCARC_SETUP_SMOOTH(NSTACK, NSCARC_SCOPE_LOCAL)
      CALL SCARC_SETUP_FFTO(NLEVEL_MIN, NLEVEL_MAX-1)

#ifdef WITH_SCARC_MKL
   ! Smoothing by LU-decomposition

   CASE (NSCARC_RELAX_MKL)
      CALL SCARC_SETUP_SMOOTH(NSTACK, TYPE_SCOPE(2))

      SELECT CASE(TYPE_SCOPE(2))

         ! Globally acting - call global CLUSTER_SPARSE_SOLVER on MKL

         CASE (NSCARC_SCOPE_GLOBAL)
            STACK(NSTACK)%SOLVER => SMOOTH_MKL
            CALL SCARC_SETUP_CLUSTER(NLEVEL_MIN, NLEVEL_MIN)

         ! Locally acting - call local PARDISO solvers based on MKL

         CASE (NSCARC_SCOPE_LOCAL)
            STACK(NSTACK)%SOLVER => SMOOTH_MKL
            CALL SCARC_SETUP_PARDISO(NLEVEL_MIN, NLEVEL_MIN)

      END SELECT
#endif

END SELECT

! Globally acting coarse grid solver

NSTACK = NSTACK + 1
CALL SCARC_SETUP_COARSE_SOLVER(NSCARC_STAGE_ONE, NSCARC_SCOPE_GLOBAL, NSTACK, NLEVEL_MAX, NLEVEL_MAX)

END SUBROUTINE SCARC_SETUP_MULTIGRID_ENVIRONMENT


! ----------------------------------------------------------------------------------------------------
!> \brief Store Jacobi preconditioner in matrix form
! Based on the following splitting of A = D - E - F
! where :   D is the diagonal part
! the MJAC-preconditioner in matrix form is defined
!           M_MJAC = D 
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MJAC(NLMIN, NLMAX)
USE SCARC_POINTERS, ONLY: G, A, AB, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NLMIN, NLMAX
INTEGER :: NM, NL, IC

CROUTINE = 'SCARC_SETUP_MJAC'

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   LEVEL_LOOP: DO NL = NLMIN, NLMAX

      CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

      SELECT CASE(TYPE_MATRIX)
         CASE (NSCARC_MATRIX_COMPACT)
            A => G%POISSON
            CALL SCARC_ALLOCATE_REAL1(A%RELAX, 1, G%NC, NSCARC_INIT_ZERO, 'G%POISSON%RELAX', CROUTINE)
            DO IC = 1, G%NC
               A%RELAX(IC) = 1.0_EB/A%VAL(A%ROW(IC))
            ENDDO 
         CASE (NSCARC_MATRIX_BANDWISE)
            AB => G%POISSONB
            CALL SCARC_ALLOCATE_REAL1(AB%RELAXD, 1, AB%N_DIAG,  NSCARC_INIT_ZERO, 'G%POISSON%RELAXD', CROUTINE)
            DO IC = 1, G%NC
               AB%RELAXD(IC) = 1.0_EB/AB%VAL(IC, AB%POS(0))
            ENDDO 
       END SELECT

   ENDDO LEVEL_LOOP
ENDDO MESHES_LOOP

END SUBROUTINE SCARC_SETUP_MJAC


! ----------------------------------------------------------------------------------------------------
!> \brief Store GS preconditioner in matrix form
! Based on the following splitting of A = D - E - F
! where :   D is the diagonal part
!          -E is the strictly lower part
!          -F is the strictly upper part
! the SGS-preconditioner in matrix form is defined
!           M_MGS = (D - E) = (I - E D^{-1}) D
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MGS(NLMIN, NLMAX)
USE SCARC_POINTERS, ONLY: G, A, AB, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NLMIN, NLMAX
INTEGER :: NM, NL, IC, JC, IPTR

CROUTINE = 'SCARC_SETUP_MGS'
MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   LEVEL_LOOP: DO NL = NLMIN, NLMAX

      CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

      SELECT CASE (SCARC_GET_MATRIX_TYPE(NL))

 
         ! ---------- Matrix in compact storage technique
 
         CASE (NSCARC_MATRIX_COMPACT)

            A => G%POISSON
            CALL SCARC_ALLOCATE_REAL1(A%RELAX, 1, A%N_VAL, NSCARC_INIT_ZERO, 'G%POISSON%RELAX', CROUTINE)

            DO IC = 1, G%NC
               DO IPTR = A%ROW(IC), A%ROW(IC+1)-1
                  JC = A%COL(IPTR)
                  IF (JC <  IC) A%RELAX(IPTR) = A%VAL(IPTR) / A%VAL(A%ROW(JC))
                  IF (JC == IC) A%RELAX(IPTR) = A%VAL(IPTR)
               ENDDO
            ENDDO 

 
         ! ---------- Matrix in bandwise storage technique
 
         CASE (NSCARC_MATRIX_BANDWISE)

            AB => G%POISSONB
            CALL SCARC_ALLOCATE_REAL2(AB%RELAX, 1, AB%N_DIAG, 1, AB%N_STENCIL, NSCARC_INIT_ZERO, 'G%POISSONB%RELAX', CROUTINE)
            WRITE(*,*) 'SCARC_SETUP_MGS: BANDWISE: NOT FINISHED YET'

      END SELECT

   ENDDO LEVEL_LOOP
ENDDO MESHES_LOOP

END SUBROUTINE SCARC_SETUP_MGS


! ----------------------------------------------------------------------------------------------------
!> \brief Store symmetric Gauss-Seidel preconditioner in matrix form
! Based on the following splitting of A = D - E - F
! where :   D is the diagonal part
!          -E is the strictly lower part
!          -F is the strictly upper part
! the SGS-preconditioner in matrix form is defined
!           M_MSGS = (D - E) D^{-1} (D - F)  =  (I - E D^{-1}) (D - F)
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MSGS(NLMIN, NLMAX)
USE SCARC_POINTERS, ONLY: G, A, AB, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NLMIN, NLMAX
INTEGER :: NM, NL, IC, JC, IPTR, I, IS, IL, IOR0

CROUTINE = 'SCARC_SETUP_MSGS'
MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   LEVEL_LOOP: DO NL = NLMIN, NLMAX

      CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

      SELECT CASE(TYPE_MATRIX)
 
         ! ---------- Matrix in compact storage technique
 
         CASE (NSCARC_MATRIX_COMPACT)

            A => G%POISSON
            CALL SCARC_ALLOCATE_REAL1(A%RELAX, 1, A%N_VAL, NSCARC_INIT_ZERO, 'G%POISSON%RELAX', CROUTINE)
            A%RELAX = A%VAL

            DO IC = 1, G%NC
               !  l(i,j) = a(i,j)/a(j,j)
               DO IPTR = A%ROW(IC), A%ROW(IC+1)-1
                  JC = A%COL(IPTR)
                  IF (JC < IC)  A%RELAX(IPTR) = A%VAL(IPTR) / A%VAL(A%ROW(JC))
               ENDDO
            ENDDO 

 
         ! ---------- Matrix in bandwise storage technique
 
         CASE (NSCARC_MATRIX_BANDWISE)

            AB => G%POISSONB
            CALL SCARC_ALLOCATE_REAL2(AB%RELAX, 1, AB%N_DIAG, 1, AB%N_STENCIL, NSCARC_INIT_ZERO, 'G%AB%RELAX', CROUTINE)
            AB%RELAX = AB%VAL
            DO IOR0 = 3, 1, -1
               IS = AB%TARGET(IOR0)
               IL = AB%LENGTH(IOR0)
               DO I = 1, AB%LENGTH(IOR0)
                  AB%RELAX(IS+I-1, AB%POS(IOR0)) = AB%VAL(IS+I-1, AB%POS(IOR0))/AB%VAL(I,AB%POS(0))
               ENDDO
            ENDDO

      END SELECT

   ENDDO LEVEL_LOOP
ENDDO MESHES_LOOP
      
END SUBROUTINE SCARC_SETUP_MSGS


! ----------------------------------------------------------------------------------------------------
!> \brief Store SOR preconditioner in matrix form
! Based on the following splitting of A = D - E - F
! where :   D is the diagonal part
!          -E is the strictly lower part
!          -F is the strictly upper part
! the SOR-preconditioner in matrix form is defined
!           M_MSOR = (D−ωE) = (I−ωE D^{-1}) D
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MSOR(NLMIN, NLMAX, NSTACK)
USE SCARC_POINTERS, ONLY: G, A, AB, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NLMIN, NLMAX, NSTACK
REAL (EB) :: OMEGA
INTEGER :: NM, NL, IC, JC, IPTR

CROUTINE = 'SCARC_SETUP_MSOR'
OMEGA = STACK(NSTACK)%SOLVER%OMEGA

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   LEVEL_LOOP: DO NL = NLMIN, NLMAX

      CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

      SELECT CASE(TYPE_MATRIX)

         ! ---------- Matrix in compact storage technique
 
         CASE (NSCARC_MATRIX_COMPACT)

            A => G%POISSON
            CALL SCARC_ALLOCATE_REAL1(A%RELAX, 1, A%N_VAL, NSCARC_INIT_ZERO, 'G%POISSON%RELAX', CROUTINE)
      
            DO IC = 1, G%NC
      
               !DO IPTR = A%ROW(IC), A%ROW(IC+1)-1
               !   JC = A%COL(IPTR)
               !   IF (JC <  IC) A%RELAX(IPTR) = OMEGA * A%VAL(IPTR) / A%VAL(A%ROW(JC))
               !   IF (JC == IC) A%RELAX(IPTR) = A%VAL(IPTR)
               !ENDDO
               DO IPTR = A%ROW(IC), A%ROW(IC+1)-1
                  JC = A%COL(IPTR)
                  IF (JC < IC) THEN
                     A%RELAX(IPTR) = OMEGA * A%VAL(IPTR) / A%VAL(A%ROW(JC))
                  ELSE IF (JC == IC) THEN
                     A%RELAX(IPTR) = A%VAL(IPTR)
                  ELSE IF (JC > IC) THEN
                     A%RELAX(IPTR) = OMEGA * A%VAL(IPTR) 
                  ENDIF
               ENDDO
      
            ENDDO 

 
         ! ---------- Matrix in bandwise storage technique
 
         CASE (NSCARC_MATRIX_BANDWISE)

            AB => G%POISSONB
            CALL SCARC_ALLOCATE_REAL2(AB%RELAX, 1, AB%N_DIAG, 1, AB%N_STENCIL, NSCARC_INIT_ZERO, 'G%POISSONB%RELAX', CROUTINE)
            WRITE(*,*) 'SCARC_SETUP_MSOR: BANDWISE: NOT FINISHED YET'
      
      END SELECT

   ENDDO LEVEL_LOOP
ENDDO MESHES_LOOP

END SUBROUTINE SCARC_SETUP_MSOR


! ----------------------------------------------------------------------------------------------------
!> \brief Store SSOR preconditioner in matrix form
! Based on the following splitting of A = D - E - F
! where :   D is the diagonal part
!          -E is the strictly lower part
!          -F is the strictly upper part
! the SSOR-preconditioner in matrix form is defined
!           B_SSOR = 1/(omega * (2-omega)) * (D - omega * E) D^{-1} (D - omega * F)
!                  = (I - omega E D^{-1}) * [1/(omega * (2-omega)) * D -  1/(2-omega) * F]
! Defining the triangular matrices
!               L  = I - omega E D^{-1}
!               U  = 1/(omega * (2-omega)) * D -  1/(2-omega) * F
! the SSOR-preconditioning can be thought as the solution of two triangular systems
! Both matrices can be stored as a single matrix that occupies the same amount of storage as A
! where the same row and column pointers can be used as for A (identical pattern)
! Note that the diagonal elements of L are 1 (and are omitted, only the diagonal of U is stored there)
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MSSOR(NLMIN, NLMAX, NSTACK)
USE SCARC_POINTERS, ONLY: G, A, AB, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NLMIN, NLMAX, NSTACK
INTEGER :: NM, NL, IC, JC, IPTR, INCR, IS, IL, IOR0, I
REAL(EB) :: OMEGA, SCAL1, SCAL2

CROUTINE = 'SCARC_SETUP_MSSOR'

OMEGA = STACK(NSTACK)%SOLVER%OMEGA
SCAL1  = 1.0_EB / (OMEGA * (2.0_EB - OMEGA))
SCAL2  = 1.0_EB / (2.0_EB - OMEGA)

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   LEVEL_LOOP: DO NL = NLMIN, NLMAX

      CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

      SELECT CASE(TYPE_MATRIX)

 
         ! ---------- Matrix in compact storage technique
 
         CASE (NSCARC_MATRIX_COMPACT)

            A => G%POISSON
            CALL SCARC_ALLOCATE_REAL1(A%RELAX, 1, A%N_VAL, NSCARC_INIT_ZERO, 'G%POISSON%RELAX', CROUTINE)
      
            DO IC = 1, G%NC
      
               DO IPTR = A%ROW(IC), A%ROW(IC+1)-1
                  JC = A%COL(IPTR)
                  IF (JC < IC) THEN
                     A%RELAX(IPTR) = OMEGA * A%VAL(IPTR) / A%VAL(A%ROW(JC))
                  ELSE IF (JC == IC) THEN
                     A%RELAX(IPTR) = SCAL1 * A%VAL(IPTR)
                  ELSE IF (JC > IC) THEN
                     A%RELAX(IPTR) = SCAL2 * A%VAL(IPTR)
                  ENDIF
               ENDDO

            ENDDO 

 
         ! ---------- Matrix in bandwise storage technique
 
         CASE (NSCARC_MATRIX_BANDWISE)

            AB => G%POISSONB
            CALL SCARC_ALLOCATE_REAL2(AB%RELAX, 1, AB%N_DIAG, 1, AB%N_STENCIL, NSCARC_INIT_ZERO, 'G%POISSONB%RELAX', CROUTINE)

            IF (TWO_D) THEN
               INCR = -2
            ELSE
               INCR = -1
            ENDIF
            
            DO IOR0 = 3, 1, INCR
               IS = AB%TARGET(IOR0)
               IL = AB%LENGTH(IOR0)
               DO I = 1, AB%LENGTH(IOR0)
                  AB%RELAX(IS+I-1, AB%POS(IOR0)) = OMEGA * AB%VAL(IS+I-1, AB%POS(IOR0))/AB%VAL(I,AB%POS(0))
               ENDDO
            ENDDO
            DO I = 1, AB%LENGTH(0)
               AB%RELAX(I, AB%POS(0)) = SCAL1 * AB%VAL(I, AB%POS(0))
            ENDDO
            DO IOR0 = -1, -3, INCR
               IS = AB%TARGET(IOR0)
               IL = AB%LENGTH(IOR0)
               DO I = IS, IS + AB%LENGTH(IOR0) -1
                  AB%RELAX(I, AB%POS(IOR0)) = SCAL2 * AB%VAL(I, AB%POS(IOR0))
               ENDDO
            ENDDO
!               L  = I - omega E D^{-1}
!               U  = 1/(omega * (2-omega)) * D -  1/(2-omega) * F
      END SELECT

   ENDDO LEVEL_LOOP
ENDDO MESHES_LOOP

!STACK(NSTACK)%SOLVER%OMEGA = 1.0_EB

END SUBROUTINE SCARC_SETUP_MSSOR


! ----------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize ILU(0) decomposition of Poisson matrix
! L- and U-parts are stored in the same array, diagonal elements of L are supposed to be 1
! Based on Saad-algorithm 10.4 from 'Iterative Methods for Sparse Linear Systems':
!   for i = 2 , ... , n do
!      for k = 1 , ... , i-1 and for (i,k) in NZ(A) do
!         compute a_ik = a_ik / a_kk
!         for j = k+1 , ... , n and for (i,j) in NZ(A) do
!            compute a_ij = a_ij - a_ik a_kj
!         enddo
!      enddo
!   enddo
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_LU(NLMIN, NLMAX)
USE SCARC_POINTERS, ONLY: G, A, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NLMIN, NLMAX
INTEGER :: NM, NL, IC, JC, KC, IPTR, JPTR, KPTR, KPTR0

CROUTINE = 'SCARC_SETUP_LU'

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '===============>> SETTING UP LU: ', NLMIN, NLMAX, LOWER_MESH_INDEX, UPPER_MESH_INDEX
#endif
MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   LEVEL_LOOP: DO NL = NLMIN, NLMAX

      CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '===============>> SETTING UP LU: ', NM, NL
#endif

      A => G%POISSON
      CALL SCARC_ALLOCATE_REAL1(A%RELAX, 1, A%N_VAL, NSCARC_INIT_ZERO, 'G%POISSON%RELAX', CROUTINE)
      A%RELAX = A%VAL
    
      CELL_LOOP: DO IC = 2, G%NC
   
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '===============>> SETTING UP LU: IC = ', IC
#endif
         COLUMN_LOOP: DO IPTR = A%ROW(IC), A%ROW(IC+1)-1
  
            KC = A%COL(IPTR)                         ! get number of neighboring cell
            IF (KC >= IC) CYCLE                      ! only consider neighbors with lower cell numbers than IC
            IF (A%RELAX(IPTR) == 0) CYCLE
 
            KPTR = A%ROW(KC)                         ! get diagonal entry of neighbor
            A%RELAX(IPTR) = A%RELAX(IPTR)/A%RELAX(KPTR)

            DO JPTR = A%ROW(IC), A%ROW(IC+1)-1

               JC = A%COL(JPTR)
               IF (JC<=KC) CYCLE                     ! only consider neighbors with higher cell numbers than IC
               IF (A%RELAX(JPTR) == 0) CYCLE

               KPTR = -1
               DO KPTR0 = A%ROW(KC), A%ROW(KC+1)-1
                  IF (A%COL(KPTR0) == JC) THEN
                    KPTR = KPTR0
                  ENDIF
               ENDDO
               IF (KPTR>0) A%RELAX(JPTR) = A%RELAX(JPTR) - A%RELAX(IPTR) * A%RELAX(KPTR)

            ENDDO
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'IPTR, KC, KPTR, JPTR : A%RELAX:', IPTR, KC, KPTR, JPTR, A%RELAX(IPTR)
#endif

         ENDDO COLUMN_LOOP
      ENDDO CELL_LOOP
#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_RELAX(A, 'RELAX', 'SETUP_LU')
#endif
   ENDDO LEVEL_LOOP
ENDDO MESHES_LOOP

END SUBROUTINE SCARC_SETUP_LU

! ----------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize LU decomposition of Poisson matrix
! L- and U-parts are stored in the same array, diagonal elements of L are supposed to be 1
!   for i = 2 , ... , n do
!      for k = 1 , ... , i-1 and for (i,k) do
!         compute a_ik = a_ik / a_kk
!         for j = k+1 , ... , n and for (i,j) do
!            compute a_ij = a_ij - a_ik a_kj
!         enddo
!      enddo
!   enddo
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_ILU(NLMIN, NLMAX)
USE SCARC_POINTERS, ONLY: G, A, AB, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NLMIN, NLMAX
INTEGER :: NM, NL, IC, JC, KC, IPTR, JPTR, KPTR, KPTR0, IOR0, JOR0, KOR0
LOGICAL :: BFOUND

CROUTINE = 'SCARC_SETUP_ILU'

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   LEVEL_LOOP: DO NL = NLMIN, NLMAX

      CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

      SELECT CASE(TYPE_MATRIX)

 
         ! ---------- Matrix in compact storage technique
 
         CASE (NSCARC_MATRIX_COMPACT)

            A => G%POISSON
            CALL SCARC_ALLOCATE_REAL1(A%RELAX, 1, A%N_VAL, NSCARC_INIT_ZERO, 'G%POISSON%RELAX', CROUTINE)
            A%RELAX = A%VAL
      
            CELL_LOOP: DO IC = 2, G%NC
      
               COLUMN_LOOP: DO IPTR = A%ROW(IC), A%ROW(IC+1)-1
      
                  KC = A%COL(IPTR)                        ! get number of neighboring cell
                  IF (KC >= IC) CYCLE                      ! only consider neighbors with lower cell numbers than IC
                  IF (A%RELAX(IPTR) == 0) CYCLE
      
                  KPTR = A%ROW(KC)                        ! get diagonal entry of neighbor
                  A%RELAX(IPTR) = A%RELAX(IPTR)/A%RELAX(KPTR)
      
                  DO JPTR = A%ROW(IC), A%ROW(IC+1)-1
      
                     JC = A%COL(JPTR)
                     IF (JC<=KC) CYCLE                     ! only consider neighbors with higher cell numbers than IC
                     IF (A%RELAX(JPTR) == 0) CYCLE
      
                     KPTR = -1
                     DO KPTR0 = A%ROW(KC), A%ROW(KC+1)-1
                        IF (A%COL(KPTR0) == JC) THEN
                          KPTR = KPTR0
                        ENDIF
                     ENDDO
                     IF (KPTR>0) A%RELAX(JPTR) = A%RELAX(JPTR) - A%RELAX(IPTR) * A%RELAX(KPTR)
      
                  ENDDO
      
               ENDDO COLUMN_LOOP
            ENDDO CELL_LOOP
      
 
         ! ---------- Matrix in bandwise storage technique
 
         CASE (NSCARC_MATRIX_BANDWISE)

            AB => G%POISSONB
            CALL SCARC_ALLOCATE_REAL2(AB%RELAX, 1, AB%N_DIAG, 1, AB%N_STENCIL, NSCARC_INIT_ZERO, 'G%POISSONB%RELAX', CROUTINE)
            AB%RELAX = AB%VAL
      
            CELL_BANDWISE_LOOP: DO IC = 2, G%NC
      
               COLUMN_BANDWISE_LOOP: DO IOR0 = 3, -3, -1 
      
                  IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE

                  KC = IC + AB%OFFSET(IOR0)                           ! get number of neighboring cell
                  IF (KC<=0  .OR. KC >= IC) CYCLE                     ! only consider neighbors with lower cell numbers than IC
                  IF (AB%RELAX(IC, AB%POS(IOR0)) == 0) CYCLE
      
                  AB%RELAX(IC,AB%POS(IOR0)) = AB%RELAX(IC, AB%POS(IOR0))/AB%RELAX(KC,AB%POS(0))
      
                  DO JOR0 = 3, -3, -1

                     IF (TWO_D .AND. ABS(JOR0) == 2) CYCLE
      
                     JC = IC + AB%OFFSET(JOR0)                  ! get number of neighboring cell

                     IF (JC<=KC .OR. JC >G%NC) CYCLE            ! only consider neighbors with higher cell numbers than IC
                     IF (AB%RELAX(IC, AB%POS(JOR0)) == 0) CYCLE
      
                     BFOUND = .FALSE.
                     DO KOR0 = 3, -3, -1
                        IF (TWO_D .AND. ABS(KOR0) == 2) CYCLE
                        IF (KC + AB%OFFSET(KOR0) == JC) THEN
                           BFOUND = .TRUE.
                           EXIT
                        ENDIF
                     ENDDO
                     IF (BFOUND) AB%RELAX(JC, AB%POS(JOR0)) = AB%RELAX(JC, AB%POS(JOR0)) - &
                                 AB%RELAX(IC, AB%POS(IOR0)) * AB%RELAX(KC, AB%POS(KOR0))
      
                  ENDDO
      
               ENDDO COLUMN_BANDWISE_LOOP
            ENDDO CELL_BANDWISE_LOOP
      
      END SELECT

   ENDDO LEVEL_LOOP
ENDDO MESHES_LOOP

END SUBROUTINE SCARC_SETUP_ILU


! ----------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize vectors for MKL-methods
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_COARSE_SOLVER(NSTAGE, NSCOPE, NSTACK, NLMIN, NLMAX)
INTEGER, INTENT(IN)    :: NSCOPE, NSTAGE, NLMIN, NLMAX
INTEGER, INTENT(INOUT) :: NSTACK

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SETUP COARSE_SOLVER: START'
#endif
SELECT_COARSE: SELECT CASE (TYPE_COARSE)

   ! -------------- CG-method is used as iterative coarse grid solver
   CASE (NSCARC_COARSE_ITERATIVE)

      ! initialize current stack position as CG-method
      STACK(NSTACK)%SOLVER => COARSE_KRYLOV
      CALL SCARC_SETUP_KRYLOV(NSCARC_SOLVER_COARSE, NSCOPE, NSTAGE, NSTACK, NLMIN, NLMAX)

      ! and next stack position as its SSOR-preconditioner
      NSTACK = NSTACK + 1
      TYPE_PRECON = NSCARC_RELAX_SSOR
      STACK(NSTACK)%SOLVER => PRECON_SSOR
      CALL SCARC_SETUP_PRECON(NSTACK, NSCOPE)

   ! -------------- LU-decomposition (from MKL) is used as direct coarse grid solver
#ifdef WITH_SCARC_MKL 
   CASE (NSCARC_COARSE_DIRECT)

      ! Global scope in the multi-mesh case:
      ! initialize current stack position as global CLUSTER_SPARSE_SOLVER
      !IF (NSCOPE == NSCARC_SCOPE_GLOBAL .AND. NMESHES > 1) THEN
      IF (NMESHES > 1) THEN
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SETUP COARSE_SOLVER: CLUSTER'
#endif
         STACK(NSTACK)%SOLVER => COARSE_CLUSTER
         CALL SCARC_SETUP_MKL(NSCARC_SOLVER_COARSE, NSCOPE, NSTAGE, NSTACK, NLMIN, NLMAX)
         CALL SCARC_SETUP_CLUSTER(NLMIN, NLMAX)

      ! Local scope:
      ! initialize current stack position as PARDISO solver
      ELSE
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SETUP COARSE_SOLVER: PARDISO'
#endif
         STACK(NSTACK)%SOLVER => COARSE_PARDISO
         CALL SCARC_SETUP_MKL(NSCARC_SOLVER_COARSE, NSCOPE, NSTAGE, NSTACK, NLMIN, NLMAX)
         CALL SCARC_SETUP_PARDISO(NLMIN, NLMAX)
      ENDIF
#endif

   ! -------------- Otherwise: print error message
   CASE DEFAULT
      CALL SCARC_SHUTDOWN(NSCARC_ERROR_PARSE_INPUT, SCARC_NONE, TYPE_COARSE)

END SELECT SELECT_COARSE
END SUBROUTINE SCARC_SETUP_COARSE_SOLVER


! ----------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize vectors for additive or multiplicative coarse grid
! (corresponding to Schwarz domain decomposition method)
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_INTERPOLATION(NSTAGE, NLMIN, NLMAX)
USE SCARC_POINTERS, ONLY: G, ST, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NSTAGE, NLMIN, NLMAX
INTEGER :: NM, NL

CROUTINE = 'SCARC_SETUP_INTERPOLATION'

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   LEVEL_LOOP: DO NL = NLMIN, NLMAX

      CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
      ST => SCARC(NM)%LEVEL(NL)%STAGE(NSTAGE)

      CALL SCARC_ALLOCATE_REAL1(ST%X, 1, G%NCE, NSCARC_INIT_ZERO, 'ST%X', CROUTINE)
      CALL SCARC_ALLOCATE_REAL1(ST%B, 1, G%NCE, NSCARC_INIT_ZERO, 'ST%B', CROUTINE)
      CALL SCARC_ALLOCATE_REAL1(ST%V, 1, G%NCE, NSCARC_INIT_ZERO, 'ST%Q', CROUTINE)
      CALL SCARC_ALLOCATE_REAL1(ST%R, 1, G%NCE, NSCARC_INIT_ZERO, 'ST%W', CROUTINE)
      CALL SCARC_ALLOCATE_REAL1(ST%Y, 1, G%NCE, NSCARC_INIT_ZERO, 'ST%Y', CROUTINE)
      CALL SCARC_ALLOCATE_REAL1(ST%Z, 1, G%NCE, NSCARC_INIT_ZERO, 'ST%Z', CROUTINE)

   ENDDO LEVEL_LOOP
ENDDO MESHES_LOOP

END SUBROUTINE SCARC_SETUP_INTERPOLATION

! ------------------------------------------------------------------------------------------------
!> \brief Perform geometric multigrid method based on global possion-matrix
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_METHOD_MULTIGRID(NSTACK, NPARENT, NRHS, NLEVEL)
INTEGER, INTENT(IN) :: NSTACK, NPARENT, NRHS, NLEVEL
INTEGER :: NS, NP, NL
INTEGER :: NSTATE, ICYCLE
REAL (EB) :: TNOW, TNOW_COARSE

TNOW = CURRENT_TIME()
ITE_MG = 0

! Store current and parent stack position and current level

TYPE_MATVEC = NSCARC_MATVEC_GLOBAL
NS = NSTACK
NP = NPARENT
NL = NLEVEL

 
! ---------- Initialization:
!   - Save SETTING (in case that subsequent solvers with different SETTING are called)
!   - Define parameters for current scope (note: NL denotes the finest level)
!   - Initialize solution, right hand side vector
  
CALL SCARC_SETUP_SOLVER(NS, NP)
CALL SCARC_SETUP_WORKSPACE(NS, NL, NRHS)

  
! ---------- Compute initial defect:  
!            RESIN := || B - A*X ||
!   - Initialize cycle counts for MG-iteration
!   - Perform initial matrix-vector product on finest level
!   - calculate norm of initial residual on finest level
  
#ifdef WITH_SCARC_DEBUG
!CALL SCARC_PRESET_VECTOR(B, NL)
CALL SCARC_DEBUG_LEVEL (X, 'MG INIT: X', NL)
CALL SCARC_DEBUG_LEVEL (B, 'MG INIT: B', NL)
#endif

CALL SCARC_MATVEC_PRODUCT (X, V, NL)                                  !  V := A*X
CALL SCARC_VECTOR_SUM (B, V, 1.0_EB, -1.0_EB, NL)                     !  V := B - V

#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (V, 'MG INIT: V', NL)
#endif

RES    = SCARC_L2NORM (V, NL)                                         !  RESIN := ||V||
RESIN  = RES
NSTATE = SCARC_CONVERGENCE_STATE (0, NS, NL)                          !  RES < TOL already ??

IF (TYPE_SOLVER == NSCARC_SOLVER_PRECON .AND. RESIN <= 1E-6_EB) THEN
   CALL SCARC_VECTOR_SUM (V, X, 1.0_EB, 1.0_EB, NL)                    !  x := omega * v + x
   CALL SCARC_UPDATE_PRECONDITIONER(NLEVEL_MIN)
   CALL SCARC_RELEASE_SOLVER(NS, NP)
   RETURN
ENDIF

ICYCLE = SCARC_CYCLING_CONTROL(NSCARC_CYCLING_SETUP, NL)

  
! ---------- Perform multigrid-looping (start each iteration on finest level)
  
MULTIGRID_LOOP: DO ITE = 1, NIT

   CALL SCARC_INCREASE_ITERATION_COUNTS(ITE)

   NL = NLEVEL_MIN
   ICYCLE = SCARC_CYCLING_CONTROL(NSCARC_CYCLING_RESET, NL)

   CYCLE_LOOP: DO WHILE (ICYCLE /= NSCARC_CYCLING_EXIT)

      ! Presmoothing  (smoothing/restriction till coarsest level is reached)
      ! initial and final residual are passed via vector V by default
 
      PRESMOOTHING_LOOP: DO WHILE (NL < NLEVEL_MAX)
         !IF (ITE /= 1) CALL SCARC_SMOOTHER (NSCARC_CYCLING_PRESMOOTH, NS+1, NS, NL)         ! D_fine   := Smooth(defect)
         CALL SCARC_SMOOTHER (NSCARC_CYCLING_PRESMOOTH, NS+1, NS, NL)         ! D_fine   := Smooth(defect)
         CALL SCARC_RESTRICTION (V, B, NL, NL+1)                              ! B_coarse := Rest(D_fine)
#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (V, 'MG PRE: V', NL)
CALL SCARC_DEBUG_LEVEL (B, 'MG PRE: B', NL+1)
#endif
         CALL SCARC_VECTOR_CLEAR (X, NL+1)                                    ! use zero initial guess on coarse level
         NL = NL + 1                                                          ! set coarser level
      ENDDO PRESMOOTHING_LOOP

 
      ! Coarse grid solver
 
      TNOW_COARSE = CURRENT_TIME()
      CALL SCARC_METHOD_COARSE(NS+2, NS, NLEVEL_MAX)                          ! X_coarse := exact_sol(.)
      CPU(MYID)%COARSE =CPU(MYID)%COARSE+CURRENT_TIME()-TNOW_COARSE

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '==================> AFTER SCARC_METHOD_COARSE'
CALL SCARC_DEBUG_LEVEL (X, 'MG COA: X', NLEVEL_MAX)
CALL SCARC_DEBUG_LEVEL (B, 'MG COA: B', NLEVEL_MAX)
#endif
 
      ! Postsmoothing (smoothing/restriction till finest level is reached again)
 
      POSTSMOOTHING_LOOP: DO WHILE (NL > NLEVEL_MIN)
         NL=NL-1
         CALL SCARC_PROLONGATION (X, V, NL+1, NL)                             ! V_fine := Prol(X_coarse)
         CALL SCARC_VECTOR_SUM (V, X, 1.0_EB, 1.0_EB, NL)                     ! X_fine := V_fine + X_fine

#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (V, 'MG after PROL: V', NL)
CALL SCARC_DEBUG_LEVEL (X, 'MG new X', NL)
CALL SCARC_DEBUG_LEVEL (B, 'MG before POST: B', NL)
#endif
         CALL SCARC_SMOOTHER (NSCARC_CYCLING_POSTSMOOTH, NS+1, NS, NL)        ! V_fine := Smooth(defect)
#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (V, 'MG POST: V', NL)
CALL SCARC_DEBUG_LEVEL (X, 'MG POST: X', NL)
#endif
         ICYCLE = SCARC_CYCLING_CONTROL(NSCARC_CYCLING_PROCEED, NL)           ! perform requested cycle
         IF (ICYCLE /= NSCARC_CYCLING_POSTSMOOTH) CYCLE CYCLE_LOOP
      ENDDO POSTSMOOTHING_LOOP

   ENDDO CYCLE_LOOP

   IF (NL /= NLEVEL_MIN) CALL SCARC_SHUTDOWN(NSCARC_ERROR_MULTIGRID_LEVEL, SCARC_NONE, NL)

 
   ! Compute norm of new residual on finest level and  leave loop correspondingly
 
   CALL SCARC_MATVEC_PRODUCT (X, V, NL)                                       ! V := A*X
   CALL SCARC_VECTOR_SUM (B, V, 1.0_EB, -1.0_EB, NL)                          ! V := F - V

#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (X, 'MG ITE X', NL)
CALL SCARC_DEBUG_LEVEL (V, 'MG RES V', NL)
#endif
   RES = SCARC_L2NORM (V, NL)                                                 ! RES := ||V||
   NSTATE = SCARC_CONVERGENCE_STATE(0, NS, NL)                                ! convergence ?
   IF (NSTATE /= NSCARC_STATE_PROCEED) EXIT MULTIGRID_LOOP

ENDDO MULTIGRID_LOOP

  
! ---------- Determine convergence rate and print corresponding information:
! In case of MG as main solver:
!   - Transfer ScaRC solution vector X to FDS pressure vector
!   - Set ghost cell values along external boundaries
!   - Exchange values along internal boundaries (consistency!)
  
CALL SCARC_CONVERGENCE_RATE(NSTATE, NS, NL)

#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (X, 'MG method: FINAL X', NL)
#endif

SELECT CASE (TYPE_SOLVER)
   CASE (NSCARC_SOLVER_MAIN)
      CALL SCARC_UPDATE_MAINCELLS(NLEVEL_MIN)
      CALL SCARC_UPDATE_GHOSTCELLS(NLEVEL_MIN)
#ifdef WITH_SCARC_POSTPROCESSING
      CALL SCARC_PRESSURE_DIFFERENCE(NLEVEL_MIN)
#endif
   CASE (NSCARC_SOLVER_PRECON)
      CALL SCARC_UPDATE_PRECONDITIONER(NLEVEL_MIN)
END SELECT

CALL SCARC_RELEASE_SOLVER(NS, NP)

END SUBROUTINE SCARC_METHOD_MULTIGRID


! ------------------------------------------------------------------------------------------------
!> \brief Perform requested MKL solver (global/local)
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_METHOD_MKL(NSTACK, NPARENT, NLEVEL)
INTEGER, INTENT(IN) :: NSTACK, NPARENT, NLEVEL

#ifdef WITH_SCARC_MKL
SELECT_MKL: SELECT CASE (TYPE_MKL(0))
   CASE (NSCARC_MKL_GLOBAL)
      CALL SCARC_METHOD_CLUSTER(NSTACK, NPARENT, NLEVEL)
   CASE (NSCARC_MKL_LOCAL)
      CALL SCARC_METHOD_PARDISO(NSTACK, NPARENT, NLEVEL)
END SELECT SELECT_MKL
#else
WRITE(*,*) 'MKL not defined on stack position ', NSTACK, ' for parent ', NPARENT, ' on level ', NLEVEL
#endif

END SUBROUTINE SCARC_METHOD_MKL


! ------------------------------------------------------------------------------------------------
!> \brief Perform requested coarse grid solver (iterative/direct)
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_METHOD_COARSE(NSTACK, NPARENT, NLEVEL)
INTEGER, INTENT(IN) :: NSTACK, NPARENT, NLEVEL

SELECT CASE (TYPE_COARSE)

   CASE (NSCARC_COARSE_ITERATIVE)
      CALL SCARC_METHOD_KRYLOV (NSTACK, NPARENT, NSCARC_RHS_DEFECT, NLEVEL)

   CASE (NSCARC_COARSE_DIRECT)
#ifdef WITH_SCARC_MKL
      !IF (STACK(NPARENT)%SOLVER%TYPE_SCOPE(0) == NSCARC_SCOPE_GLOBAL .AND. NMESHES > 1) THEN
      IF (NMESHES > 1) THEN
         CALL SCARC_METHOD_CLUSTER (NSTACK, NPARENT, NLEVEL)
      ELSE
         CALL SCARC_METHOD_PARDISO (NSTACK, NPARENT, NLEVEL)
      ENDIF
#else
      CALL SCARC_SHUTDOWN(NSCARC_ERROR_DIRECT_NOMKL, SCARC_NONE, NLEVEL)
#endif

END SELECT

END SUBROUTINE SCARC_METHOD_COARSE

! ------------------------------------------------------------------------------------------------------
!> \brief Perform preceding FFT method to improve start solution for ScaRC
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_METHOD_FFT
USE MESH_POINTERS
USE POIS, ONLY: H2CZSS, H3CZSS
REAL(EB), POINTER, DIMENSION(:,:,:) :: HP
INTEGER :: NM, I, J, K
LOGICAL :: WITH_BDRY = .FALSE.

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   !CALL POINT_TO_MESH(NM)
   
   IF (PREDICTOR) THEN
      HP => H
   ELSE
      HP => HS
   ENDIF
   
   ! Call the Poisson solver
 
   IF (.NOT.TWO_D) CALL H3CZSS(BXS,BXF,BYS,BYF,BZS,BZF,ITRN,JTRN,PRHS,POIS_PTB,SAVE1,WORK,HX)
   IF (TWO_D .AND. .NOT.CYLINDRICAL) CALL H2CZSS(BXS,BXF,BZS,BZF,ITRN,PRHS,POIS_PTB,SAVE1,WORK,HX)
   
   DO K=1,KBAR
     DO J=1,JBAR
        DO I=1,IBAR
            HP(I,J,K) = PRHS(I,J,K)
        ENDDO
      ENDDO
   ENDDO
   
   ! Apply boundary conditions to H
 
   IF (WITH_BDRY) THEN
      DO K=1,KBAR
         DO J=1,JBAR
            IF (LBC==3 .OR. LBC==4)             HP(0,J,K)    = HP(1,J,K)    - DXI*BXS(J,K)
            IF (LBC==3 .OR. LBC==2 .OR. LBC==6) HP(IBP1,J,K) = HP(IBAR,J,K) + DXI*BXF(J,K)
            IF (LBC==1 .OR. LBC==2)             HP(0,J,K)    =-HP(1,J,K)    + 2._EB*BXS(J,K)
            IF (LBC==1 .OR. LBC==4 .OR. LBC==5) HP(IBP1,J,K) =-HP(IBAR,J,K) + 2._EB*BXF(J,K)
            IF (LBC==5 .OR. LBC==6)             HP(0,J,K)    = HP(1,J,K)
            IF (LBC==0) THEN
               HP(0,J,K) = HP(IBAR,J,K)
               HP(IBP1,J,K) = HP(1,J,K)
            ENDIF
         ENDDO
      ENDDO
      
      DO K=1,KBAR
         DO I=1,IBAR
            IF (MBC==3 .OR. MBC==4) HP(I,0,K)    = HP(I,1,K)    - DETA*BYS(I,K)
            IF (MBC==3 .OR. MBC==2) HP(I,JBP1,K) = HP(I,JBAR,K) + DETA*BYF(I,K)
            IF (MBC==1 .OR. MBC==2) HP(I,0,K)    =-HP(I,1,K)    + 2._EB*BYS(I,K)
            IF (MBC==1 .OR. MBC==4) HP(I,JBP1,K) =-HP(I,JBAR,K) + 2._EB*BYF(I,K)
            IF (MBC==0) THEN
               HP(I,0,K) = HP(I,JBAR,K)
               HP(I,JBP1,K) = HP(I,1,K)
            ENDIF
         ENDDO
      ENDDO
      
      DO J=1,JBAR
         DO I=1,IBAR
            IF (NBC==3 .OR. NBC==4)  HP(I,J,0)    = HP(I,J,1)    - DZETA*BZS(I,J)
            IF (NBC==3 .OR. NBC==2)  HP(I,J,KBP1) = HP(I,J,KBAR) + DZETA*BZF(I,J)
            IF (NBC==1 .OR. NBC==2)  HP(I,J,0)    =-HP(I,J,1)    + 2._EB*BZS(I,J)
            IF (NBC==1 .OR. NBC==4)  HP(I,J,KBP1) =-HP(I,J,KBAR) + 2._EB*BZF(I,J)
            IF (NBC==0) THEN
               HP(I,J,0) = HP(I,J,KBAR)
               HP(I,J,KBP1) = HP(I,J,1)
            ENDIF
         ENDDO
      ENDDO
   ENDIF

ENDDO MESHES_LOOP

END SUBROUTINE SCARC_METHOD_FFT




#ifdef WITH_SCARC_MKL
! ------------------------------------------------------------------------------------------------
!> \brief Perform global Pardiso-method based on MKL
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_METHOD_CLUSTER(NSTACK, NPARENT, NLEVEL)
USE SCARC_POINTERS, ONLY: L, G, MKL, V1, V2, AS, V1_FB, V2_FB, &
                          SCARC_POINT_TO_GRID, SCARC_POINT_TO_VECTOR, SCARC_POINT_TO_VECTOR_FB, &
                          SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NSTACK, NPARENT, NLEVEL
INTEGER ::  NM, NS, NP, NL
REAL (EB) :: TNOW

NS = NSTACK
NP = NPARENT
NL = NLEVEL

TNOW = CURRENT_TIME()

CALL SCARC_SETUP_SOLVER(NS, NP)
CALL SCARC_SETUP_WORKSPACE(NS, NL, NSCARC_RHS_INHOMOGENEOUS)

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
   AS => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_POISSON_SYM)

   V1 => SCARC_POINT_TO_VECTOR (NM, NL, B)
   V2 => SCARC_POINT_TO_VECTOR (NM, NL, X)

   MKL => L%MKL
   MKL%PHASE  = 33                                ! only solving

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'G%NC_GLOBAL=', G%NC_GLOBAL
WRITE(MSG%LU_DEBUG,*) 'CLUSTER, PRE, V1:'
WRITE(MSG%LU_DEBUG,'(6E14.6)') V1
WRITE(MSG%LU_DEBUG,*) 'CLUSTER, PRE, V2:'
WRITE(MSG%LU_DEBUG,'(6E14.6)') V2
CALL SCARC_DEBUG_CMATRIX(AS, 'AS','CLUSTER')
#endif

   IF (TYPE_MKL_PRECISION == NSCARC_PRECISION_SINGLE) THEN

      V1_FB => SCARC_POINT_TO_VECTOR_FB (NM, NL, B)
      V2_FB => SCARC_POINT_TO_VECTOR_FB (NM, NL, X)

      V1_FB = REAL(V1, FB)
      V2_FB = 0.0_FB
      CALL CLUSTER_SPARSE_SOLVER_S(MKL%CT, MKL%MAXFCT, MKL%MNUM, MKL%MTYPE, MKL%PHASE, G%NC_GLOBAL, &
                                   AS%VAL_FB, AS%ROW, AS%COL, MKL%PERM, MKL%NRHS, MKL%IPARM, &
                                   MKL%MSGLVL, V1_FB, V2_FB, MPI_COMM_WORLD, MKL%ERROR)
      V2 = REAL(V2_FB, EB)

   ELSE

      CALL CLUSTER_SPARSE_SOLVER_D(MKL%CT, MKL%MAXFCT, MKL%MNUM, MKL%MTYPE, MKL%PHASE, G%NC_GLOBAL, &
                                   AS%VAL, AS%ROW, AS%COL, MKL%PERM, MKL%NRHS, MKL%IPARM, &
                                   MKL%MSGLVL, V1, V2, MPI_COMM_WORLD, MKL%ERROR)
   ENDIF

   IF (MKL%ERROR /= 0) CALL SCARC_SHUTDOWN(NSCARC_ERROR_MKL_INTERNAL, SCARC_NONE, MKL%ERROR)

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'CLUSTER, POST, V1:'
WRITE(MSG%LU_DEBUG,'(2E14.6)') V1
WRITE(MSG%LU_DEBUG,*) 'CLUSTER, POST, V2:'
WRITE(MSG%LU_DEBUG,'(2E14.6)') V2
#endif
ENDDO MESHES_LOOP

CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_VECTOR_PLAIN, X, NL)

IF (TYPE_SOLVER == NSCARC_SOLVER_MAIN) THEN
   CALL SCARC_UPDATE_MAINCELLS (NLEVEL_MIN)
   CALL SCARC_UPDATE_GHOSTCELLS(NLEVEL_MIN)
ENDIF

CALL SCARC_RELEASE_SOLVER(NS, NP)

END SUBROUTINE SCARC_METHOD_CLUSTER
#endif


#ifdef WITH_SCARC_MKL
! ------------------------------------------------------------------------------------------------
!> \brief Perform global Pardiso-method based on MKL
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_METHOD_PARDISO(NSTACK, NPARENT, NLEVEL)
USE SCARC_POINTERS, ONLY: L, G, MKL, AS, V1, V2, V1_FB, V2_FB, &
                          SCARC_POINT_TO_GRID, SCARC_POINT_TO_VECTOR, SCARC_POINT_TO_VECTOR_FB, &
                          SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NSTACK, NPARENT, NLEVEL
INTEGER ::  NM, NS, NP, NL
REAL (EB) :: TNOW

TNOW = CURRENT_TIME()

NS = NSTACK
NP = NPARENT
NL = NLEVEL

CALL SCARC_SETUP_SOLVER(NS, NP)
CALL SCARC_SETUP_WORKSPACE(NS, NL, NSCARC_RHS_INHOMOGENEOUS)

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
   AS => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_POISSON_SYM)

   V1 => SCARC_POINT_TO_VECTOR (NM, NL, B)
   V2 => SCARC_POINT_TO_VECTOR (NM, NL, X)

   MKL => L%MKL
   MKL%PHASE  = 33         ! only solving

   IF (TYPE_MKL_PRECISION == NSCARC_PRECISION_SINGLE) THEN

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'PARDISO SINGLE, PRE, V1:', G%NC, SIZE(V1)
WRITE(MSG%LU_DEBUG,'(6E14.6)') V1
WRITE(MSG%LU_DEBUG,*) 'PARDISO SINGLE, PRE, V2:', G%NC, SIZE(V2)
WRITE(MSG%LU_DEBUG,'(6E14.6)') V2
!CALL SCARC_DEBUG_CMATRIX(AS, 'AS','PARDISO')
#endif

      V1_FB => SCARC_POINT_TO_VECTOR_FB (NM, NL, B)
      V2_FB => SCARC_POINT_TO_VECTOR_FB (NM, NL, X)

      V1_FB = REAL(V1, FB)
      V2_FB = 0.0_FB
      CALL PARDISO_S(MKL%PT, MKL%MAXFCT, MKL%MNUM, MKL%MTYPE, MKL%PHASE, G%NC, &
                     AS%VAL_FB, AS%ROW, AS%COL, MKL%PERM, MKL%NRHS, MKL%IPARM, &
                     MKL%MSGLVL, V1_FB, V2_FB, MKL%ERROR)

      V2 = REAL(V2_FB, EB)

   ELSE

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'PARDISO DOUBLE, PRE, V1:', G%NC, SIZE(V1)
WRITE(MSG%LU_DEBUG,'(6E14.6)') V1
WRITE(MSG%LU_DEBUG,*) 'PARDISO DOUBLE, PRE, V2:', G%NC, SIZE(V2)
WRITE(MSG%LU_DEBUG,'(6E14.6)') V2
!CALL SCARC_DEBUG_CMATRIX(AS, 'AS','PARDISO')
#endif

      V2 = 0.0_EB
      CALL PARDISO_D(MKL%PT, MKL%MAXFCT, MKL%MNUM, MKL%MTYPE, MKL%PHASE, G%NC, &
                     AS%VAL, AS%ROW, AS%COL, MKL%PERM, MKL%NRHS, MKL%IPARM, &
                     MKL%MSGLVL, V1, V2, MKL%ERROR)
   ENDIF

   IF (MKL%ERROR /= 0) CALL SCARC_SHUTDOWN(NSCARC_ERROR_MKL_INTERNAL, SCARC_NONE, MKL%ERROR)

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'PARDISO, POST, V1:'
WRITE(MSG%LU_DEBUG,'(2E14.6)') V1
WRITE(MSG%LU_DEBUG,*) 'PARDISO, POST, V2:'
WRITE(MSG%LU_DEBUG,'(2E14.6)') V2
#endif
ENDDO MESHES_LOOP

IF (TYPE_SOLVER == NSCARC_SOLVER_MAIN) THEN
   CALL SCARC_UPDATE_MAINCELLS (NLEVEL_MIN)
   CALL SCARC_UPDATE_GHOSTCELLS(NLEVEL_MIN)
ENDIF

CALL SCARC_RELEASE_SOLVER(NSTACK, NPARENT)

END SUBROUTINE SCARC_METHOD_PARDISO
#endif




! ----------------------------------------------------------------------------------------------------
!> \brief Set initial solution corresponding to boundary data in BXS, BXF, ...
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_WORKSPACE(NS, NL, NRHS)
USE SCARC_POINTERS, ONLY: M, L, F, G, SV, ST, STP, GWC, PRHS, HP, SCARC_POINT_TO_GRID
#ifdef WITH_SCARC_POSTPROCESSING
USE SCARC_POINTERS, ONLY: PR
#endif
INTEGER, INTENT(IN) :: NS, NL, NRHS
REAL(EB) :: VAL
INTEGER  :: NM, IW, IW1, IW2, IOR0, I, J, K, IC
LOGICAL  :: BFIRST_WORKSPACE = .FALSE.

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'STARTING SETUP_WORKSPACE ', NS, NL, NRHS
#endif

SV  => STACK(NS)%SOLVER

SELECT_SOLVER_TYPE: SELECT CASE (SV%TYPE_SOLVER)

         ! ---------- If used as main solver use values from pressure-routine as initialization
 
         CASE (NSCARC_SOLVER_MAIN)
         
            MAIN_MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
         
               CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
               ST => SCARC(NM)%LEVEL(NL)%STAGE(SV%TYPE_STAGE)
         
               PRHS => M%PRHS
               IF (PREDICTOR) THEN
                  HP => M%H
               ELSE
                  HP => M%HS
               ENDIF


#ifdef WITH_SCARC_POSTPROCESSING
               PR => L%PRESSURE
               IF (PREDICTOR) THEN
                  PR%H_OLD = PR%H_NEW
               ELSE
                  PR%HS_OLD = PR%HS_NEW
               ENDIF
               PR%B_OLD = ST%B
#endif
         
               ! Get right hand side (PRHS from pres.f90) and initial vector (H or HS from last time step)

               SELECT_RHS_TYPE: SELECT CASE (NRHS)
         
                  ! Solve original problem with inhomegeneous boundary conditions
                  CASE (NSCARC_RHS_INHOMOGENEOUS)
            
                     IF (IS_MGM) BFIRST_WORKSPACE = .TRUE.

                     !$OMP PARALLEL DO PRIVATE(IC) SCHEDULE(STATIC)
                     DO IC = 1, G%NC
                        ST%X(IC) = HP(G%ICX(IC), G%ICY(IC), G%ICZ(IC))        ! use last iterate as initial solution
                        ST%B(IC) = PRHS(G%ICX(IC), G%ICY(IC), G%ICZ(IC))      ! get new RHS from surrounding code
                     ENDDO                         
                     !$OMP END PARALLEL DO
                     ST%X = 0.0_EB                      ! CAUTION - ONLY TEMPORARILY - TODO
            
                     !!$OMP PARALLEL 
                     MAIN_INHOMOGENEOUS_LOOP: DO IOR0 = -3, 3, 1 
            
                        IF (IOR0 == 0) CYCLE
                        F => SCARC(NM)%LEVEL(NL)%FACE(IOR0)
                        
                        IW1 = F%NCW0
                        IW2 = F%NCW0 + F%NCW - 1
            
                        !!$OMP DO PRIVATE(IW, GWC, I, J, K, IC, VAL) SCHEDULE(STATIC)
                        FACE_INHOMOGENEOUS_LOOP: DO IW = IW1, IW2
            
                           GWC => G%WALL(IW)
               
                           I = GWC%IXW
                           J = GWC%IYW
                           K = GWC%IZW
               
                           IF (TWO_D .AND. J /= 1) CALL SCARC_SHUTDOWN(NSCARC_ERROR_GRID_INDEX, SCARC_NONE, J)
               
                           IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE
               
                           IC = G%CELL_NUMBER(I,J,K)
               
                           ! ---------- Dirichlet BC's:
                           ! these are based on the SETTING in BTYPE
                           ! in the structured case this corresponds to the face-wise SETTING according to the FFT
                           ! (this allows to use local FFT's as preconditioners)
                           ! in the unstructured case only open boundary cells lead to Dirichlet BC's

                           IF_DIRICHLET: IF (GWC%BTYPE == DIRICHLET) THEN
               
                              SELECT CASE (IOR0)
                              CASE (1)
                                 VAL =  M%BXS(J,K)
                              CASE (-1)
                                 VAL =  M%BXF(J,K)
                              CASE (2)
                                 VAL =  M%BYS(I,K)
                              CASE (-2)
                                 VAL =  M%BYF(I,K)
                              CASE (3)
                                 VAL =  M%BZS(I,J)
                              CASE (-3)
                                 VAL =  M%BZF(I,J)
                              END SELECT
               
                              ST%B(IC) = ST%B(IC) + F%SCAL_DIRICHLET * VAL
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,'(A, 5I6,2E14.6)') 'SETUP_WORKSPACE: DIRICHLET: IW, I, J, K, IC, VAL, B(IC):', &
                                             IW, I, J, K, IC, VAL, ST%B(IC)
#endif
               
                           ENDIF IF_DIRICHLET
               
                           ! ---------- Neumann BC's:
                           ! Note for the unstructured case only:
                           ! Here, the matrix also contains Neumann BC's for those cells which have a
                           ! PRESSURE_BC_INDEX == DIRICHLET but are NOT open; these cells must be excluded below,
                           ! because BXS, BXF, ... contain the Dirichlet information from pres.f90 there;
                           ! excluding them corresponds to a homogeneous Neumann condition for these cells

                           IF_NEUMANN: IF (GWC%BTYPE == NEUMANN) THEN
               
                              IF (IS_UNSTRUCTURED .AND. M%WALL(IW)%PRESSURE_BC_INDEX /= NEUMANN) CYCLE
               
                              SELECT CASE (IOR0)
                              CASE (1)
                                 VAL =  M%BXS(J,K)
                              CASE (-1)
                                 VAL =  M%BXF(J,K)
                              CASE (2)
                                 VAL =  M%BYS(I,K)
                              CASE (-2)
                                 VAL =  M%BYF(I,K)
                              CASE (3)
                                 VAL =  M%BZS(I,J)
                              CASE (-3)
                                 VAL =  M%BZF(I,J)
                              END SELECT
               
                              ST%B(IC) = ST%B(IC) + F%SCAL_NEUMANN * VAL

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,'(A, 5I6,2E14.6)') 'SETUP_WORKSPACE: NEUMANN  : IW, I, J, K, IC, VAL, B(IC):', &
                                             IW, I, J, K, IC, VAL, ST%B(IC)
#endif
               
                           ENDIF IF_NEUMANN
            
                        ENDDO FACE_INHOMOGENEOUS_LOOP
                        !!$OMP END DO

                     ENDDO MAIN_INHOMOGENEOUS_LOOP
                     !!$OMP END PARALLEL 
         

                  ! Solve problem with homegeneous boundary conditions (MGM only)

                  CASE (NSCARC_RHS_HOMOGENEOUS)
         
                     ST%B = 0.0_EB                                    ! set RHS to zero
                     ST%X = 0.0_EB                                    ! use zero as initial vector
                     ST%V = 0.0_EB                                 
                     ST%D = 0.0_EB                                
                     ST%R = 0.0_EB                               
                     ST%Y = 0.0_EB                              
                     ST%Z = 0.0_EB                             
  
                     IF (NMESHES > 1) CALL SCARC_SETUP_MGM_INTERFACES(NM, NL)         ! setup BC's along mesh interfaces
                     CALL SCARC_SETUP_MGM_OBSTRUCTIONS                                ! setup BC's along internal obstructions

#ifdef WITH_SCARC_DEBUG
                     CALL SCARC_DEBUG_LEVEL_MESH(ST%B, 'RHS second pass MGM', NSCARC_GRID_UNSTRUCTURED, NM, NL)
#endif
                  BFIRST_WORKSPACE = .FALSE.
         
               END SELECT SELECT_RHS_TYPE
 

#ifdef WITH_SCARC_POSTPROCESSING
               PR%B_NEW = ST%B
#endif
         
            ENDDO MAIN_MESHES_LOOP
            
            ! In case of a Krylov method clear overlapping parts of auxiliary vectors

            IF (IS_CG.OR.IS_MGM.OR.HAS_TWO_LEVELS) THEN
               DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
                  L  => SCARC(NM)%LEVEL(NL)
                  ST => SCARC(NM)%LEVEL(NL)%STAGE(SV%TYPE_STAGE)
                  ST%D(1:G%NCE) = 0.0_EB
                  ST%R(1:G%NCE) = 0.0_EB
                  ST%V(1:G%NCE) = 0.0_EB
                  ST%Y(1:G%NCE) = 0.0_EB
                  ST%Z(1:G%NCE) = 0.0_EB
               ENDDO
            ENDIF
         
            ! In case of a multigrid method as main solver clear
            ! overlapping parts of auxiliary vectors and coarse grid solver vectors

            IF (IS_GMG) THEN
               DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
                  L  => SCARC(NM)%LEVEL(NL)
                  ST => SCARC(NM)%LEVEL(NL)%STAGE(SV%TYPE_STAGE)
                  ST%V(1:G%NCE) = 0.0_EB
                  ST%Z(1:G%NCE) = 0.0_EB
               ENDDO
            ENDIF
         
            ! In case of pure Neumann or periodic BCs, broadcast RHS(end) from last mesh
            ! to all and store it on all meshes

            IF (N_DIRIC_GLOBAL(NLEVEL_MIN) == 0) THEN
               IF (UPPER_MESH_INDEX == NMESHES) THEN
                  L  => SCARC(NMESHES)%LEVEL(NL)
                  ST => SCARC(NMESHES)%LEVEL(NL)%STAGE(SV%TYPE_STAGE)
                  MESH_REAL = ST%B(G%NC)
               ELSE
                  MESH_REAL = 0.0_EB
               ENDIF
               IF (N_MPI_PROCESSES > 1) &
                  CALL MPI_ALLGATHER(MPI_IN_PLACE, 1, MPI_DOUBLE_PRECISION, MESH_REAL, 1, MPI_DOUBLE_PRECISION,&
                                     MPI_COMM_WORLD, IERROR)
               DO NM = 1, NMESHES
                  SCARC(NM)%RHS_END = MESH_REAL(NMESHES)
               ENDDO
            ENDIF
         
 
         ! ---------- If MG is used as Krylov preconditioner, vector R of main Krylov is the new RHS for MG
 
         CASE (NSCARC_SOLVER_PRECON)
         
            IF (IS_CG_MG) THEN
               DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
                  L   => SCARC(NM)%LEVEL(NL)
                  ST  => SCARC(NM)%LEVEL(NL)%STAGE(SV%TYPE_STAGE)              ! current stage
                  STP => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_ONE)           ! parent stage
                  ST%X(1:G%NCE) = 0.0_EB
                  ST%B(1:G%NCE) = STP%R(1:G%NCE)                                                
                  ST%V(1:G%NCE) = 0.0_EB
                  ST%Z(1:G%NCE) = 0.0_EB
               ENDDO
            ENDIF
         
 
         ! ---------- If used as coarse grid solver start with zero initialization
 
         CASE (NSCARC_SOLVER_COARSE)
         
            DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
               ST => SCARC(NM)%LEVEL(NL)%STAGE(SV%TYPE_STAGE)
               ST%X = 0.0_EB
               ST%D = 0.0_EB
               ST%R = 0.0_EB
               ST%V = 0.0_EB
               ST%Y = 0.0_EB
               ST%Z = 0.0_EB
            ENDDO
         
END SELECT SELECT_SOLVER_TYPE

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'LEAVING SETUP_WORKSPACE ', NS, NL, NRHS
#endif
END SUBROUTINE SCARC_SETUP_WORKSPACE




! --------------------------------------------------------------------------------------------------------
!> \brief Copy final solution from GMG (as preconditioner) to corresponding vector of CG (as main solver)
! --------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_UPDATE_PRECONDITIONER(NL)
INTEGER, INTENT(IN) :: NL
INTEGER :: NM
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_ONE)%V = SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_TWO)%X
ENDDO
END SUBROUTINE SCARC_UPDATE_PRECONDITIONER


! --------------------------------------------------------------------------------------------------------
!> \brief Finalize data for pressure vector (predictor/corrector) when local ScaRC solver has finished
! --------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_UPDATE_MAINCELLS(NL)
USE SCARC_POINTERS, ONLY: M, G, L, ST, HP, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NL
INTEGER :: NM, IC 
#ifdef WITH_SCARC_DEBUG
INTEGER :: I, K
#endif

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
   ST  => L%STAGE(NSCARC_STAGE_ONE)

   IF (PREDICTOR) THEN
      HP => M%H
   ELSE
      HP => M%HS
   ENDIF

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'UPDATE_MAIN_CELLS:1: HP'
WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((HP(I,1,K), I=0, L%NX+1), K=L%NZ+1,0,-1)
#endif

   HP = 0.0_EB
   !!$OMP PARALLEL DO PRIVATE(IC) SCHEDULE(STATIC)
   DO IC = 1, G%NC
      HP (G%ICX(IC), G%ICY(IC), G%ICZ(IC)) = ST%X(IC)
#ifdef WITH_SCARC_DEBUG2
      WRITE(MSG%LU_DEBUG,'(A, 4I6, E14.6)') 'UPDATE_MAIN_CELLS: IC, IX, IY, IZ, HP(IC):', &
                                             IC, G%ICX(IC), G%ICY(IC), G%ICZ(IC), ST%X(IC)
#endif
   ENDDO
   !!$OMP END PARALLEL DO 

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'UPDATE_MAIN_CELLS:2: HP'
WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((HP(I,1,K), I=0,L%NX+1), K=L%NZ+1,0,-1)
#endif

#ifdef WITH_SCARC_DEBUG
   CALL SCARC_DEBUG_VECTOR3_BIG (HP, NM, 'HP: UPDATE_MAIN_CELLS')
#endif
#ifdef WITH_SCARC_VERBOSE2
   CALL SCARC_VERBOSE_PRESSURE (HP, NM, 'main')
#endif
ENDDO

END SUBROUTINE SCARC_UPDATE_MAINCELLS


! ------------------------------------------------------------------------------------------------
!> \brief Set correct boundary values at external and internal boundaries
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_UPDATE_GHOSTCELLS(NL)
USE SCARC_POINTERS, ONLY: M, L, G, GWC, HP, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NL
INTEGER :: NM, IW, IOR0, IXG, IYG, IZG, IXW, IYW, IZW 
#ifdef WITH_SCARC_DEBUG
INTEGER :: I, K
#endif

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

   IF (PREDICTOR) THEN
      HP => M%H
   ELSE
      HP => M%HS
   ENDIF

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'UPDATE_GHOST_CELLS:1: HP'
WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((HP(I,1,K), I=0, L%NX+1), K=L%NZ+1,0,-1)
#endif
   ! Compute ghost cell values
 
   !!$OMP PARALLEL DO SHARED(HP, M, L, G) PRIVATE(IW, IXG, IYG, IZG, IXW, IYW, IZW, IOR0, GWC) SCHEDULE(STATIC)
   WALL_CELLS_LOOP: DO IW = 1, L%N_WALL_CELLS_EXT

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
#ifdef WITH_SCARC_DEBUG2
      WRITE(MSG%LU_DEBUG,'(A, 5I6, E14.6)') 'UPDATE_GHOST_CELLS: IW, IOR0, IXW, IYW, IZG, HP:',&
                                             IW, IOR0, IXW, IYW, IZG, HP(IXW, IYW, IZG)
#endif

   ENDDO WALL_CELLS_LOOP
   !!$OMP END PARALLEL DO

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'UPDATE_GHOST_CELLS:2: HP'
WRITE(MSG%LU_DEBUG,MSG%CFORM3) ((HP(I,1,K), I=0, L%NX+1), K=L%NZ+1,0,-1)
#endif
#ifdef WITH_SCARC_DEBUG
   CALL SCARC_DEBUG_VECTOR3_BIG (HP, NM, 'HP: UPDATE_GHOST_CELLS')
#endif
#ifdef WITH_SCARC_VERBOSE2
   CALL SCARC_VERBOSE_PRESSURE (HP, NM, 'h')
#endif

ENDDO

! Perform data exchange to achieve consistency of ghost values along internal boundaries
! Note: this is most probably no longer necessary because MESH_EXCHANGE(5) is used after the call of ScaRC

CALL SCARC_EXCHANGE(NSCARC_EXCHANGE_PRESSURE, NSCARC_NONE, NL)

#ifdef WITH_SCARC_DEBUG
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
   IF (PREDICTOR) THEN
         HP => M%H
   ELSE
      HP => M%HS
   ENDIF
   CALL SCARC_DEBUG_VECTOR3_BIG (HP, NM, 'HP: UPDATE_GHOST_CELLS - AFTER EXCHANGE')
ENDDO
#endif
   
END SUBROUTINE SCARC_UPDATE_GHOSTCELLS
   
! -----------------------------------------------------------------------------------------------
!> \brief Preconditioning method which is based on the following input and output convention:
!  - the residual which has to be preconditioned is passed in via vector R
!  - the result of preconditioning is passed out via vector V
!  - for several variants Y and Z are used as auxiliary vectors
!  - in the comments: call is based on current grid level l (mostly the finest one)
!  -                  l=1 denotes the finest  grid level NLEVEL_MIN
!  -                  l=L denotes the coarset grid level NLEVEL_MAX
! -----------------------------------------------------------------------------------------------
SUBROUTINE SCARC_PRECONDITIONER(NS, NP, NL)
INTEGER, INTENT(IN) :: NS, NP, NL     
INTEGER :: IL

SELECT_PRECON_TYPE: SELECT CASE (TYPE_TWOLEVEL)

   ! ---------- Classical one-level preconditioning
 
   CASE (NSCARC_TWOLEVEL_NONE)

      CALL SCARC_VECTOR_COPY (R, V, 1.0_EB, NL)                   !  v := r
      CALL SCARC_RELAXATION (R, V, NS+1, NP, NL)                  !  v := Relax(r)
 
   ! ---------- Additive two-level preconditioning
 
   CASE (NSCARC_TWOLEVEL_ADD)

      CALL SCARC_VECTOR_COPY (R, B, 1.0_EB, NL)                   !  Use r^l as right hand side for preconditioner
      DO IL = NL, NLEVEL_MAX-1                                    !  successively restrict to coarser levels up to coarsest
         CALL SCARC_RESTRICTION (B, B, IL, IL+1)                  !  b^{l+1} := Restriction(r^l)
      ENDDO
      CALL SCARC_METHOD_COARSE(NS+2, NS, NLEVEL_MAX)              !  solve A^L * x^L := b^L on coarsest level
      CALL SCARC_VECTOR_COPY (X, Z, 1.0_EB, NLEVEL_MAX)           !  z^L := x^L

      DO IL = NLEVEL_MAX-1, NL, -1                                !  successively interpolate to finer levels up to finest
         CALL SCARC_PROLONGATION(Z, Z, IL+1, IL)                  !  z^l := Prolongation(z^{l+1})
      ENDDO
      CALL SCARC_VECTOR_COPY (R, V, 1.0_EB, NL)                   !  v^l := r^l
      CALL SCARC_RELAXATION (R, V, NS+1, NP, NL)                  !  v^l := Relax(r^l)
      CALL SCARC_VECTOR_SUM (Z, V, 1.0_EB, 1.0_EB, NL)            !  v^l := z^l + v^l

   ! ---------- Multiplicative two-level preconditioning (coarse first, fine second)
 
   CASE (NSCARC_TWOLEVEL_MUL)

      CALL SCARC_VECTOR_COPY (R, B, 1.0_EB, NL)                   !  Use r^l as right hand side for preconditioner

      DO IL = NL, NLEVEL_MAX-1
         CALL SCARC_RESTRICTION (B, B, IL, IL+1)                  !  b^{l+1} := Restriction(r^l)
      ENDDO

      CALL SCARC_METHOD_COARSE(NS+2, NS, NLEVEL_MAX)              !  solve A^L * x^L := b^L on coarsest level
      CALL SCARC_VECTOR_COPY (X, Y, 1.0_EB, NLEVEL_MAX)           !  y^L := x^L

      DO IL = NLEVEL_MAX-1, NL, -1
         CALL SCARC_PROLONGATION (Y, Y, NL+1, NL)                 !  y^l := Prolongation(y^{l+1})
      ENDDO
      CALL SCARC_MATVEC_PRODUCT (Y, Z, NL)                        !  z^l := A^l * y^l

      CALL SCARC_VECTOR_SUM (R, Z, 1.0_EB, -1.0_EB, NL)           !  z^l := r^l - z^l
      CALL SCARC_VECTOR_COPY (Z, V, 1.0_EB, NL)                   !  v^l := z^l
      CALL SCARC_RELAXATION (Z, V, NS+1, NP, NL)                  !  v^l := Relax(z^l)
      CALL SCARC_VECTOR_SUM (Y, V, 1.0_EB, 1.0_EB, NL)            !  v^l := y^l - z^l

   ! ---------- Multiplicative two-level preconditioning (fine first, coarse second):
   ! coarse level is one level away from finest one (one coarsening step)
 
   CASE (NSCARC_TWOLEVEL_MUL2)

      CALL SCARC_VECTOR_COPY (R, V, 1.0_EB, NL)                   !  v^l := r^l
      CALL SCARC_RELAXATION (R, V, NS+1, NP, NL)                  !  v^l := Relax(r^l)
      CALL SCARC_MATVEC_PRODUCT (V, Z, NL)                        !  z^l := A^{l} * v^l

      CALL SCARC_VECTOR_SUM (R, Z, 1.0_EB, -1.0_EB, NL)           !  z^l := r^l - z^l

      CALL SCARC_RESTRICTION (Z, B, NL, NL+1)                     !  b^{l+1} := rest(R^{l})
      CALL SCARC_METHOD_COARSE(NS+2, NS, NLEVEL_MAX)              !  x^{l+1} := A^{l+1}^{-1}(b^{l+1})
      CALL SCARC_PROLONGATION (X, Z, NL+1, NL)                    !  v^l := Prolongation(x^{l+1})
      CALL SCARC_VECTOR_SUM (Z, V, 1.0_EB, 1.0_EB, NL)            !  z^l := r^l - z^l
 
   ! ---------- Only coarse grid preconditioner
 
   CASE (NSCARC_TWOLEVEL_COARSE)

      CALL SCARC_VECTOR_COPY (R, B, 1.0_EB, NL)                   !  Use r^l as right hand side for preconditioner
      DO IL = NL, NLEVEL_MAX-1                                    !  successively restrict to coarser levels up to coarsest
         CALL SCARC_RESTRICTION (B, B, IL, IL+1)                  !  b^{l+1} := Restriction(b^l)
      ENDDO

      CALL SCARC_METHOD_COARSE(NS+2, NS, NLEVEL_MAX)              !  solve A^L * x^L := b^L on coarsest level
      CALL SCARC_VECTOR_COPY (X, Y, 1.0_EB, NLEVEL_MAX)           !  y^L := x^L

      DO IL = NLEVEL_MAX-1, NL, -1                                !  successively interpolate to finer levels up to finest
         CALL SCARC_PROLONGATION (Y, Y, NL+1, NL)                 !  y^l := Prolongation(y^{l+1})
      ENDDO
      CALL SCARC_VECTOR_COPY (Y, V, 1.0_EB, NL)                   !  v^l := y^l

END SELECT SELECT_PRECON_TYPE

END SUBROUTINE SCARC_PRECONDITIONER

! ------------------------------------------------------------------------------------------------
!> \brief Perform smoothing based on specified relaxation method
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SMOOTHER(NTYPE, NSTACK, NPARENT, NLEVEL)
INTEGER, INTENT(IN) :: NTYPE, NSTACK, NPARENT, NLEVEL
INTEGER :: NSTATE=0, NS, NP, NL
REAL(EB) :: TNOW
LOGICAL :: BMATVEC, BL2NORM, BVERBOSE
 
! ---------- Initialization
 
TNOW = CURRENT_TIME()
TYPE_MATVEC = NSCARC_MATVEC_GLOBAL
NS = NSTACK
NP = NPARENT
NL = NLEVEL

CALL SCARC_SETUP_SOLVER(NS, NP)
 
! Calculate initial defect on l2-norm on level NL (only if BMATVEC and Bl2NORM are set to .TRUE.)
! Because initial vector in MG is set to zero, this defect corresponds to F
 
ITE = 0
BVERBOSE = .FALSE.
IF (BVERBOSE) THEN
   BL2NORM  = .TRUE.
   BMATVEC  = .TRUE.
ELSE
   BL2NORM  = .FALSE.
   BMATVEC  = .FALSE.
ENDIF

IF (BMATVEC) THEN
   CALL SCARC_MATVEC_PRODUCT (X, V, NL)                                 !  v := A*x
   CALL SCARC_VECTOR_SUM (B, V, 1.0_EB, -1.0_EB, NL)                    !  v := b - v   
ENDIF

IF (BL2NORM) THEN
   RESIN = SCARC_L2NORM (V, NL)                                         !  resin := ||v||
ELSE
   RESIN = SCARC_RESIDUAL
ENDIF
IF (BVERBOSE) NSTATE = SCARC_CONVERGENCE_STATE(NTYPE, NS, NL)

#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (X, 'SMOOTH init X', NL)
CALL SCARC_DEBUG_LEVEL (B, 'SMOOTH init B', NL)
#endif
 
! ---------- Smoothing loop - only temporarily
 
!IF (NTYPE == NSCARC_CYCLING_PRESMOOTH) THEN
!   NIT = SCARC_MULTIGRID_PRESMOOTH
!ELSE
!   NIT = SCARC_MULTIGRID_POSTSMOOTH
!ENDIF

SMOOTH_LOOP: DO ITE=1, NIT

   CALL SCARC_INCREASE_ITERATION_COUNTS(ITE)

#ifdef WITH_SCARC_MKL
   IF (TYPE_SMOOTH == NSCARC_RELAX_MKL) THEN
      CALL SCARC_VECTOR_COPY(V, Z, 1.0_EB, NL)                          !  use additional auxiliary vector Z
      CALL SCARC_RELAXATION (Z, V, NS, NP, NL)                          !  v := Relax(z)
   ELSE
      CALL SCARC_RELAXATION (V, V, NS, NP, NL)                          !  v := Relax(v)
   ENDIF
#else
   CALL SCARC_RELAXATION (V, V, NS, NP, NL)                             !  v := Relax(v)
#endif

#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (V, 'SMOOTH ite V', NL)
#endif

   CALL SCARC_VECTOR_SUM (V, X, OMEGA, 1.0_EB, NL)                      !  x := omega * v + x
   CALL SCARC_MATVEC_PRODUCT (X, V, NL)                                 !  v := A*x
   CALL SCARC_VECTOR_SUM (B, V, 1.0_EB, -1.0_EB, NL)                    !  v := b - v

#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (X, 'SMOOTH ite X', NL)
CALL SCARC_DEBUG_LEVEL (V, 'SMOOTH ite V2', NL)
#endif

   IF (BL2NORM) THEN
      RES = SCARC_L2NORM (V, NL)                                        !  res := ||v||
      IF (BVERBOSE) THEN
         NSTATE = SCARC_CONVERGENCE_STATE(NTYPE, NS, NL)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SMOOTH - RESIDUAL, NSTATE =', RES, NSTATE
CALL SCARC_DEBUG_LEVEL (V, 'SMOOTH ite V2', NL)
#endif
         IF (NSTATE /= NSCARC_STATE_PROCEED) EXIT SMOOTH_LOOP
      ENDIF
   ENDIF

ENDDO SMOOTH_LOOP

CALL SCARC_RELEASE_SOLVER(NS, NP)

CPU(MYID)%SMOOTHER = CPU(MYID)%SMOOTHER + CURRENT_TIME() - TNOW
END SUBROUTINE SCARC_SMOOTHER


! ------------------------------------------------------------------------------------------------
!> \brief Perform preconditioning based on requested local solvers
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_RELAXATION (NV1, NV2, NS, NP, NL)
USE SCARC_POINTERS, ONLY: L, G, A, AB, FFT, V1, V2, &
                          SCARC_POINT_TO_GRID, SCARC_POINT_TO_VECTOR, SCARC_POINT_TO_VECTOR_FB, &
                          SCARC_POINT_TO_CMATRIX, SCARC_POINT_TO_BMATRIX
#ifdef WITH_SCARC_MKL
USE SCARC_POINTERS, ONLY: AS, MKL, V1_FB, V2_FB
#endif
USE POIS, ONLY: H2CZSS, H3CZSS
REAL(EB) :: AUX, OMEGA_SSOR = 1.5_EB 
REAL (EB) :: TNOW
INTEGER, INTENT(IN) :: NV1, NV2, NS, NP, NL
INTEGER :: NM, NP0, IC, JC, ICOL, ITYPE, IDIAG, IPTR, INCR, IOR0, IC0, IY, IZ

TNOW = CURRENT_TIME()
ITYPE = STACK(NS-1)%SOLVER%TYPE_RELAX

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'CALLING RELAXATION, NV1, NV2, NS, NP, NL:', NV1, NV2, NS, NP, NL, ITYPE
#endif

IF ((IS_AMG .OR. IS_CG_AMG) .AND. NL > NLEVEL_MIN .AND. ITYPE == NSCARC_RELAX_FFT) ITYPE = NSCARC_RELAX_SSOR

SELECT CASE (ITYPE)

   ! --------- Preconditioning by blockwise Jacobi
 
   CASE (NSCARC_RELAX_JAC)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) ' ===================== RELAX: JACOBI'
#endif
      JACOBI_MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

         CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
         V2 => SCARC_POINT_TO_VECTOR(NM, NL, NV2)

         SELECT CASE (SCARC_GET_MATRIX_TYPE(NL))
            
            ! ---------- Matrix in compact storage technique
 
            CASE (NSCARC_MATRIX_COMPACT)

               IF (IS_LAPLACE) THEN
                  A => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_LAPLACE)
               ELSE
                  A => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_POISSON)
               ENDIF
               !$OMP PARALLEL DO PRIVATE(IC) SCHEDULE(STATIC)
               DO IC = 1, G%NC
                  V2(IC) = V2(IC) / A%VAL(A%ROW(IC))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'IC, A%ROW, A%VAL, V2:', IC, A%ROW(IC), A%VAL(A%ROW(IC)), V2(IC)
#endif
               ENDDO
               !$OMP END PARALLEL DO

            ! ---------- Matrix in bandwise storage technique
 
            CASE (NSCARC_MATRIX_BANDWISE)

               AB => SCARC_POINT_TO_BMATRIX(G, NSCARC_MATRIX_POISSON)
               !$OMP PARALLEL DO PRIVATE(IC) SCHEDULE(STATIC)
               DO IC = 1, G%NC
                  V2(IC) = V2(IC) / AB%VAL(IC, AB%POS(0))
               ENDDO
               !$OMP END PARALLEL DO

         END SELECT 

      ENDDO JACOBI_MESHES_LOOP

 
   ! --------- Preconditioning by blockwise SSOR
 
   CASE (NSCARC_RELAX_SSOR)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) ' ===================== RELAX: SSOR'
#endif
      SSOR_MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

         CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
         V2 => SCARC_POINT_TO_VECTOR(NM, NL, NV2)

         SELECT CASE (SCARC_GET_MATRIX_TYPE(NL))

            ! ---------- Matrix in compact storage technique
 
            CASE (NSCARC_MATRIX_COMPACT)

               IF (IS_LAPLACE) THEN
                  A => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_LAPLACE)
               ELSE
                  A => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_POISSON)
               ENDIF

               IF (NL == NLEVEL_MIN) THEN
               SSOR_FORWARD_COMPACT_LOOP: DO IC = 1, G%NC                                   ! forward SSOR step
                  AUX = 0.0_EB
                  DO ICOL = A%ROW(IC)+1, A%ROW(IC+1)-1
                     IF (A%COL(ICOL) >= IC) CYCLE                                          ! only process lower diags
                     IF (A%COL(ICOL) <= G%NC) THEN
                        AUX = AUX + A%VAL(ICOL) * V2(A%COL(ICOL))  ! ignore overlaps
                     ENDIF
                  ENDDO
                  V2(IC) = (V2(IC) - AUX * OMEGA_SSOR) / A%VAL(A%ROW(IC))
               ENDDO SSOR_FORWARD_COMPACT_LOOP

               SSOR_BACKWARD_COMPACT_LOOP: DO IC = G%NC-1, 1, -1                           ! backward SSOR step
                  AUX = 0.0_EB
                  DO ICOL = A%ROW(IC)+1, A%ROW(IC+1)-1
                     IF (A%COL(ICOL) <= IC) CYCLE                                         ! only process upper diags
                     IF (A%COL(ICOL) <= G%NC) THEN                                        ! ignore overlaps
                        AUX = AUX + A%VAL(ICOL) * V2(A%COL(ICOL))
                     ENDIF
                  ENDDO
                  V2(IC) = V2(IC) - AUX * OMEGA_SSOR / A%VAL(A%ROW(IC))
               ENDDO SSOR_BACKWARD_COMPACT_LOOP

               ELSE

               SSOR_FORWARD_COMPACT_LOOP_COARSE: DO IC = 1, G%NC                                   ! forward SSOR step
                  AUX = 0.0_EB
                  DO ICOL = A%ROW(IC)+1, A%ROW(IC+1)-1
                     IF (A%COL(ICOL) >= IC .OR. A%COL(ICOL) == 0) CYCLE                            ! only process lower diags
                     IF (A%COL(ICOL) <= G%NC) THEN
                        AUX = AUX + A%VAL(ICOL) * V2(A%COL(ICOL))  ! ignore overlaps
                     ENDIF
                  ENDDO
                  V2(IC) = (V2(IC) - AUX * OMEGA_SSOR) / A%VAL(A%ROW(IC))
               ENDDO SSOR_FORWARD_COMPACT_LOOP_COARSE

               SSOR_BACKWARD_COMPACT_LOOP_COARSE: DO IC = G%NC-1, 1, -1                           ! backward SSOR step
                  AUX = 0.0_EB
                  DO ICOL = A%ROW(IC)+1, A%ROW(IC+1)-1
                     IF (A%COL(ICOL) <= IC .OR. A%COL(ICOL) == 0) CYCLE                   ! only process upper diags
                     IF (A%COL(ICOL) <= G%NC) THEN                                        ! ignore overlaps
                        AUX = AUX + A%VAL(ICOL) * V2(A%COL(ICOL))
                     ENDIF
                  ENDDO
                  V2(IC) = V2(IC) - AUX * OMEGA_SSOR / A%VAL(A%ROW(IC))
               ENDDO SSOR_BACKWARD_COMPACT_LOOP_COARSE

               ENDIF

 
         ! ---------- Matrix in bandwise storage technique
 
         CASE (NSCARC_MATRIX_BANDWISE)

            AB => SCARC_POINT_TO_BMATRIX(G, NSCARC_MATRIX_POISSON)

 
            ! 2D version
 
            IF (TWO_D) THEN

               SSOR_FORWARD_BANDWISE_2D_LOOP: DO IC = 1, G%NC                 ! forward SSOR step
                  AUX = 0.0_EB
                  DO IOR0 = 1, 3, 2                                          ! only process lower x- and z-diag
                     JC = IC + AB%OFFSET(IOR0)
                     IF (JC >= 1 .AND. JC <= G%NC) AUX = AUX + AB%VAL(IC, AB%POS(IOR0)) * V2(JC)
                  ENDDO
                  V2(IC) = (V2(IC) - AUX * OMEGA_SSOR) / AB%VAL(IC, AB%POS(0))
               ENDDO SSOR_FORWARD_BANDWISE_2D_LOOP

               SSOR_BACKWARD_BANDWISE_2D_LOOP: DO IC = G%NC-1, 1, -1          ! backward SSOR step
                  AUX = 0.0_EB
                  DO IOR0 = -1, -3, -2                                       ! only process upper x- and z-diag
                     JC = IC + AB%OFFSET(IOR0)
                     IF (JC <= G%NC) THEN
                        AUX = AUX + AB%VAL(IC, AB%POS(IOR0)) * V2(JC)
                     ENDIF
                  ENDDO
                  V2(IC) = V2(IC) - AUX * OMEGA_SSOR / AB%VAL(IC, AB%POS(0))
               ENDDO SSOR_BACKWARD_BANDWISE_2D_LOOP

 
            ! 3D version
 
            ELSE

               SSOR_FORWARD_BANDWISE_3D_LOOP: DO IC = 1, G%NC                  ! forward SSOR step
                  AUX = 0.0_EB
                  DO IOR0 = 1, 3                                             ! only process lower diags
                     IF (AB%POS(IOR0) == 0) CYCLE                            ! no contribution for y-direction in 2D
                     JC = IC + AB%OFFSET(IOR0)
                     IF (JC >= 1 .AND. JC <= G%NC) AUX = AUX + AB%VAL(IC, AB%POS(IOR0)) * V2(JC)
                  ENDDO
                  V2(IC) = (V2(IC) - AUX * OMEGA_SSOR) / AB%VAL(IC, AB%POS(0))
               ENDDO SSOR_FORWARD_BANDWISE_3D_LOOP

               SSOR_BACKWARD_BANDWISE_3D_LOOP: DO IC = G%NC-1, 1, -1           ! backward SSOR step
                  AUX = 0.0_EB
                  DO IOR0 = -1, -3, -1                                       ! only process upper diags
                     IF (AB%POS(IOR0) == 0) CYCLE                            ! no contribution for y-direction in 2D
                     JC = IC + AB%OFFSET(IOR0)
                     IF (JC >= IC .AND. JC <= G%NC) AUX = AUX + AB%VAL(IC, AB%POS(IOR0)) * V2(JC)
                  ENDDO
                  V2(IC) = V2(IC) - AUX * OMEGA_SSOR / AB%VAL(IC, AB%POS(0))
               ENDDO SSOR_BACKWARD_BANDWISE_3D_LOOP

            ENDIF

         END SELECT 

      ENDDO SSOR_MESHES_LOOP

 
   ! --------- Preconditioning by Jacobi in matrix form
 
   CASE (NSCARC_RELAX_MJAC)

      MJAC_MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

         CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

         V1 => SCARC_POINT_TO_VECTOR(NM, NL, NV1)
         V2 => SCARC_POINT_TO_VECTOR(NM, NL, NV2)

         SELECT CASE(TYPE_MATRIX)

            ! ------------ Matrix in compact storage technique

            CASE (NSCARC_MATRIX_COMPACT)
               A => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_POISSON)
               CALL SCARC_SCALING_VARIABLE(G%NC, A%RELAX, V1, V2)

            ! ------------ Matrix in bandwise storage technique

            CASE (NSCARC_MATRIX_BANDWISE)
               AB => SCARC_POINT_TO_BMATRIX(G, NSCARC_MATRIX_POISSON)
               CALL SCARC_SCALING_VARIABLE(G%NC, AB%RELAXD, V1, V2)

         END SELECT

      ENDDO MJAC_MESHES_LOOP

 
   ! --------- Preconditioning by different matrix-form preconditioners
   ! in all cases the preconditioner is given as separate matrix which is based
   ! on the same storage technique as the matrix AC itself;
   ! two tridiagonal systems have to be solved
   ! V1 contains the RHS to be solved for, V2 will contain the solution
 
   CASE (NSCARC_RELAX_MGS, NSCARC_RELAX_MSGS, NSCARC_RELAX_MSOR, NSCARC_RELAX_MSSOR, NSCARC_RELAX_ILU)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) ' ===================== RELAX: OTHER'
#endif
      LU_MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

         CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

         V1 => SCARC_POINT_TO_VECTOR(NM, NL, NV1)
         V2 => SCARC_POINT_TO_VECTOR(NM, NL, NV2)
      
         SELECT CASE(TYPE_MATRIX)

            ! ------------ Matrix in compact storage technique

            CASE (NSCARC_MATRIX_COMPACT)

               A => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_POISSON)
      
               ! Forward solve:   Solve V2 = L^-1 V1
               ! Compute sol(i) = rhs(i) - sum L(i,j) x sol(j)

               DO IC = 1, G%NC
                  V2(IC) = V1(IC)
                  DO IPTR = A%ROW(IC), A%ROW(IC+1)-1
                     JC = A%COL(IPTR)
                     IF (JC >= IC) CYCLE
                     V2(IC) = V2(IC) - A%RELAX(IPTR) * V2(JC)
                  ENDDO
               ENDDO
      
               ! If preconditioner is not symmetric, upper matrix U is zero and nothing has to be solved

               IF (ITYPE == NSCARC_RELAX_MGS .OR. ITYPE == NSCARC_RELAX_MSOR) CYCLE
      
               ! Backward solve : Compute sol: inv(U) sol

               DO IC = G%NC, 1, -1
      
                  DO IPTR = A%ROW(IC), A%ROW(IC+1)-1
                     JC = A%COL(IPTR)
                     IF (JC <= IC) CYCLE
                     V2(IC) = V2(IC) - A%RELAX(IPTR) * V2(JC)
                  ENDDO
      
                  ! Compute sol(i) = sol(i)/U(i,i)

                  IDIAG = A%ROW(IC)
                  V2(IC) = V2(IC)/A%RELAX(IDIAG)
      
               ENDDO
      

            ! ---------- Matrix in bandwise storage technique
 
            CASE (NSCARC_MATRIX_BANDWISE)

               AB => SCARC_POINT_TO_BMATRIX(G, NSCARC_MATRIX_POISSON)
      
               IF (TWO_D) THEN
                  INCR = -2
               ELSE 
                  INCR = -1
               ENDIF
               
               ! Forward solve:   V2 = L^-1 V1
               ! Compute sol(i) = rhs(i) - sum L(i,j) x sol(j)

               !!$OMP PARALLEL DO PRIVATE(IC, JC, IOR0) SCHEDULE(STATIC)
               DO IC = 1, G%NC
                  V2(IC) = V1(IC)
                  DO IOR0 = 3, 1, INCR
                     JC = IC + AB%OFFSET(IOR0)
                     IF (JC <= 0) CYCLE
                     V2(IC) = V2(IC) - AB%RELAX(IC, AB%POS(IOR0)) * V2(JC)
                  ENDDO
               ENDDO
               !!$OMP END PARALLEL DO 
      
               ! If preconditioner is not symmetric, upper matrix U is zero and nothing has to be solved

               IF (ITYPE == NSCARC_RELAX_MGS .OR. ITYPE == NSCARC_RELAX_MSOR) CYCLE
      
               ! Backward solve
               ! Compute sol: inv(U) sol

               !!$OMP PARALLEL DO PRIVATE(IC, JC, IOR0) SCHEDULE(STATIC)
               DO IC = G%NC, 1, -1
      
                  DO IOR0 = -1, -3, INCR
                     JC = IC + AB%OFFSET(IOR0)
                     IF (JC > G%NC) CYCLE
                     V2(IC) = V2(IC) - AB%RELAX(IC, AB%POS(IOR0)) * V2(JC)
                  ENDDO
      
                  ! Compute sol(i) = sol(i)/U(i,i)
                  V2(IC) = V2(IC)/AB%RELAX(IC, AB%POS(0))
               ENDDO
               !!$OMP END PARALLEL DO 
      
         END SELECT
      
      ENDDO LU_MESHES_LOOP
      

 
   ! --------- Preconditioning by blockwise Geometric Multigrid
 
   !CASE (NSCARC_RELAX_MULTIGRID)

       NP0 = NP               ! Only dummy command until multigrid is used again
   !   CALL SCARC_METHOD_MULTIGRID (NS, NP, NSCARC_RHS_DEFECT, NLEVEL_MIN)

 
   ! --------- Preconditioning by blockwise FFT based on Crayfishpak
 
   CASE (NSCARC_RELAX_FFT)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) ' ===================== RELAX: FFT'
#endif
      DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

         CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
         FFT => L%FFT

         V1  => SCARC_POINT_TO_VECTOR(NM, NL, NV1)
         V2  => SCARC_POINT_TO_VECTOR(NM, NL, NV2)

#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (NV1, 'RELAX-FFT: NV1 INIT ', NL)
CALL SCARC_DEBUG_LEVEL (NV2, 'RELAX-FFT: NV2 INIT ', NL)
#endif
         !$OMP PARALLEL DO PRIVATE(IC) SCHEDULE(STATIC)
         DO IC = 1, G%NC
            FFT%PRHS(G%ICX(IC), G%ICY(IC), G%ICZ(IC)) = V1(IC)
         ENDDO
         !$OMP END PARALLEL DO

         IF (TWO_D) THEN
            CALL H2CZSS (FFT%BXS,  FFT%BXF, FFT%BZS, FFT%BZF, FFT%ITRN, &
                         FFT%PRHS, FFT%POIS_PTB, FFT%SAVE1, FFT%WORK, FFT%HX)
         ELSE
            CALL H3CZSS (FFT%BXS,  FFT%BXF, FFT%BYS, FFT%BYF, FFT%BZS, FFT%BZF, FFT%ITRN, FFT%JTRN, &
                         FFT%PRHS, FFT%POIS_PTB, FFT%SAVE1, FFT%WORK, FFT%HX)
         ENDIF

         !$OMP PARALLEL DO PRIVATE(IC) SCHEDULE(STATIC)
         DO IC = 1, G%NC
            V2(IC) = FFT%PRHS(G%ICX(IC), G%ICY(IC), G%ICZ(IC)) 
         ENDDO
         !$OMP END PARALLEL DO 

#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (NV2, 'RELAX-FFT: NV2 EXIT ', NL)
#endif
      ENDDO

 
   ! --------- Preconditioning by blockwise overlapping FFT based on Crayfishpak 
   !           still test-version for tunnel-shaped geometries of type Mx1
 
   CASE (NSCARC_RELAX_FFTO)

      ! Exchange overlapping parts

      CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_VECTOR_PLAIN, NV1, NL)

      DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

         CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
         FFT => L%FFT

         V1  => SCARC_POINT_TO_VECTOR(NM, NL, NV1)
         V2  => SCARC_POINT_TO_VECTOR(NM, NL, NV2)

         IF (NM == 1) THEN

            !$OMP PARALLEL DO PRIVATE(IC) SCHEDULE(STATIC)
            DO IC = 1, G%NC
               FFT%PRHS(G%ICX(IC), G%ICY(IC), G%ICZ(IC)) = V1(IC)
            ENDDO
            !$OMP END PARALLEL DO

            IC0 = G%NC+1
            DO IZ = 1, L%NZ
               DO IY = 1, L%NY
                  FFT%PRHS(FFT%IBAR, IY, IZ) = V1(IC0)
               IC0 = IC0 + 1
               ENDDO
            ENDDO

         ELSE IF (NM == NMESHES) THEN

            !$OMP PARALLEL DO PRIVATE(IC) SCHEDULE(STATIC)
            DO IC = 1, G%NC
               FFT%PRHS(G%ICX(IC)+1, G%ICY(IC), G%ICZ(IC)) = V1(IC)
            ENDDO
            !$OMP END PARALLEL DO

            IC0 = G%NC+1
            DO IZ = 1, L%NZ
               DO IY = 1, L%NY
                  FFT%PRHS(1, IY, IZ) = V1(IC0)
                  IC0 = IC0 + 1
               ENDDO
            ENDDO

         ELSE 

            !$OMP PARALLEL DO PRIVATE(IC) SCHEDULE(STATIC)
            DO IC = 1, G%NC
               FFT%PRHS(G%ICX(IC)+1, G%ICY(IC), G%ICZ(IC)) = V1(IC)
            ENDDO
            !$OMP END PARALLEL DO

            IC0 = G%NC+1
            DO IZ = 1, L%NZ
               DO IY = 1, L%NY
                  FFT%PRHS(1, IY, IZ) = V1(IC0)
                  IC0 = IC0 + 1
               ENDDO
            ENDDO
            DO IZ = 1, L%NZ
               DO IY = 1, L%NY
                  FFT%PRHS(FFT%IBAR, IY, IZ) = V1(IC0)
                  IC0 = IC0 + 1
               ENDDO
            ENDDO

         ENDIF

 
         ! Call corresponding FFT solver
 
         IF (TWO_D) THEN
            CALL H2CZSS (FFT%BXS,  FFT%BXF, FFT%BZS, FFT%BZF, FFT%ITRN, &
                         FFT%PRHS, FFT%POIS_PTB, FFT%SAVE1, FFT%WORK, FFT%HX)
         ELSE
            CALL H3CZSS (FFT%BXS,  FFT%BXF, FFT%BYS, FFT%BYF, FFT%BZS, FFT%BZF, FFT%ITRN, FFT%JTRN, &
                         FFT%PRHS, FFT%POIS_PTB, FFT%SAVE1, FFT%WORK, FFT%HX)
         ENDIF

 
         ! Extract computed data from FFT%PRHS
 
         IF (NM == 1) THEN
            !$OMP PARALLEL DO PRIVATE(IC) SCHEDULE(STATIC)
            DO IC = 1, G%NC
               V2(IC) = FFT%PRHS(G%ICX(IC), G%ICY(IC), G%ICZ(IC)) 
            ENDDO
            !$OMP END PARALLEL DO 

            IC0 = G%NC+1
            DO IZ = 1, L%NZ
               DO IY = 1, L%NY
                  V2(IC0) = FFT%PRHS(FFT%IBAR, IY, IZ) 
                  IC0 = IC0 + 1
               ENDDO
            ENDDO

         ELSE IF (NM == NMESHES) THEN

            !$OMP PARALLEL DO PRIVATE(IC) SCHEDULE(STATIC)
            DO IC = 1, G%NC
               V2(IC) = FFT%PRHS(G%ICX(IC)+1, G%ICY(IC), G%ICZ(IC)) 
            ENDDO
            !$OMP END PARALLEL DO 

            IC0 = G%NC+1
            DO IZ = 1, L%NZ
               DO IY = 1, L%NY
                  V2(IC0) = FFT%PRHS(1, IY, IZ) 
                  IC0 = IC0 + 1
               ENDDO
            ENDDO

         ELSE

            !$OMP PARALLEL DO PRIVATE(IC) SCHEDULE(STATIC)
            DO IC = 1, G%NC
               V2(IC) = FFT%PRHS(G%ICX(IC)+1, G%ICY(IC), G%ICZ(IC)) 
            ENDDO
            !$OMP END PARALLEL DO 

            IC0 = G%NC+1
            DO IZ = 1, L%NZ
               DO IY = 1, L%NY
                  V2(IC0) = FFT%PRHS(1, IY, IZ) 
                  IC0 = IC0 + 1
               ENDDO
            ENDDO
            DO IZ = 1, L%NZ
               DO IY = 1, L%NY
                  V2(IC0) = FFT%PRHS(FFT%IBAR, IY, IZ) 
                  IC0 = IC0 + 1
               ENDDO
            ENDDO

         ENDIF

      ENDDO

      ! Exchange overlapping parts

      CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_VECTOR_MEAN, NV2, NL)


#ifdef WITH_SCARC_MKL
 
   ! --------- Preconditioning by LU-decomposition
 
   CASE (NSCARC_RELAX_MKL)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) ' ===================== RELAX: MKL'
#endif
      ! Preconditioning by Cluster Sparse Solver from MKL
 
      MKL_SCOPE_IF: IF (STACK(NS)%SOLVER%TYPE_SCOPE(0) == NSCARC_SCOPE_GLOBAL) THEN

         MKL_SCOPE_GLOBAL_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

            CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
            MKL => L%MKL
            AS => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_POISSON_SYM)

            MKL%PHASE  = 33                            ! only solving

            V1 => SCARC_POINT_TO_VECTOR (NM, NL, NV1)
            V2 => SCARC_POINT_TO_VECTOR (NM, NL, NV2)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'G%NC:', G%NC
WRITE(MSG%LU_DEBUG,*) 'G%NC_GLOBAL:', G%NC_GLOBAL
WRITE(MSG%LU_DEBUG,*) 'CLUSTER, PRE, V1:'
WRITE(MSG%LU_DEBUG,'(6E14.6)') V1
WRITE(MSG%LU_DEBUG,*) 'CLUSTER, PRE, V2:'
WRITE(MSG%LU_DEBUG,'(6E14.6)') V2
CALL SCARC_DEBUG_CMATRIX(AS, 'AS','CLUSTER')
#endif

            IF (TYPE_MKL_PRECISION == NSCARC_PRECISION_SINGLE) THEN

               V1_FB => SCARC_POINT_TO_VECTOR_FB (NM, NL, NV1)
               V2_FB => SCARC_POINT_TO_VECTOR_FB (NM, NL, NV2)

               V1_FB(1:G%NC) = REAL(V1(1:G%NC), FB)
               V2_FB(1:G%NC) = 0.0_FB

               CALL CLUSTER_SPARSE_SOLVER_S(MKL%CT, MKL%MAXFCT, MKL%MNUM, MKL%MTYPE, MKL%PHASE, G%NC_GLOBAL, &
                                            AS%VAL_FB, AS%ROW, AS%COL, MKL%PERM, MKL%NRHS, MKL%IPARM, &
                                            MKL%MSGLVL, V1_FB, V2_FB, MPI_COMM_WORLD, MKL%ERROR)

               V2(1:G%NC) = REAL(V2_FB(1:G%NC), EB)

            ELSE
               CALL CLUSTER_SPARSE_SOLVER_D(MKL%CT, MKL%MAXFCT, MKL%MNUM, MKL%MTYPE, MKL%PHASE, G%NC_GLOBAL, &
                                            AS%VAL, AS%ROW, AS%COL, MKL%PERM, MKL%NRHS, MKL%IPARM, &
                                            MKL%MSGLVL, V1, V2, MPI_COMM_WORLD, MKL%ERROR)
            ENDIF
            IF (MKL%ERROR /= 0) CALL SCARC_SHUTDOWN(NSCARC_ERROR_MKL_INTERNAL, SCARC_NONE, MKL%ERROR)

         ENDDO MKL_SCOPE_GLOBAL_LOOP

 
      ! Preconditioning by Pardiso Solver from MKL
 
      ELSE MKL_SCOPE_IF

         MKL_SCOPE_LOCAL_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

            CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
            MKL => L%MKL
            AS => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_POISSON_SYM)

            MKL%PHASE  = 33                            ! only solving

            V1 => SCARC_POINT_TO_VECTOR (NM, NL, NV1)
            V2 => SCARC_POINT_TO_VECTOR (NM, NL, NV2)

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'PARDISO, G%NC=',G%NC
WRITE(MSG%LU_DEBUG,*) 'PARDISO, G%NC=',G%NC
WRITE(MSG%LU_DEBUG,*) 'PARDISO, PRE, V1:'
WRITE(MSG%LU_DEBUG,'(6E14.6)') V1
WRITE(MSG%LU_DEBUG,*) 'PARDISO, PRE, V2:'
WRITE(MSG%LU_DEBUG,'(6E14.6)') V2
CALL SCARC_DEBUG_CMATRIX(AS, 'AS','CLUSTER')
#endif
            IF (TYPE_MKL_PRECISION == NSCARC_PRECISION_SINGLE) THEN

               V1_FB => SCARC_POINT_TO_VECTOR_FB (NM, NL, NV1)
               V2_FB => SCARC_POINT_TO_VECTOR_FB (NM, NL, NV2)

               V1_FB(1:G%NC) = REAL(V1(1:G%NC), FB)
               V2_FB(1:G%NC) = 0.0_FB
   
               CALL PARDISO_S(MKL%PT, MKL%MAXFCT, MKL%MNUM, MKL%MTYPE, MKL%PHASE, G%NC, &
                              AS%VAL_FB, AS%ROW, AS%COL, &
                              MKL%PERM, MKL%NRHS, MKL%IPARM, MKL%MSGLVL, V1_FB, V2_FB, MKL%ERROR)

               V2(1:G%NC) = REAL(V2_FB(1:G%NC), EB)
            ELSE

               V1 => SCARC_POINT_TO_VECTOR (NM, NL, NV1)
               V2 => SCARC_POINT_TO_VECTOR (NM, NL, NV2)

               CALL PARDISO_D(MKL%PT, MKL%MAXFCT, MKL%MNUM, MKL%MTYPE, MKL%PHASE, G%NC, &
                              AS%VAL, AS%ROW, AS%COL, &
                              MKL%PERM, MKL%NRHS, MKL%IPARM, MKL%MSGLVL, V1, V2, MKL%ERROR)

            ENDIF
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'MKL%ERROR:', MKL%ERROR
#endif
            IF (MKL%ERROR /= 0) CALL SCARC_SHUTDOWN(NSCARC_ERROR_MKL_INTERNAL, SCARC_NONE, MKL%ERROR)

         ENDDO MKL_SCOPE_LOCAL_LOOP

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'PARDISO, POST, V1:'
WRITE(MSG%LU_DEBUG,'(6E14.6)') V1
WRITE(MSG%LU_DEBUG,*) 'PARDISO, POST, V2:'
WRITE(MSG%LU_DEBUG,'(6E14.6)') V2
#endif
      ENDIF MKL_SCOPE_IF

#endif

END SELECT

CPU(MYID)%RELAXATION =CPU(MYID)%RELAXATION+CURRENT_TIME()-TNOW
END SUBROUTINE SCARC_RELAXATION

END MODULE SCARC_METHODS


