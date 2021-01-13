!//////////////////////////////////////////////////////////////////////////////////////////////////////
!
! MODULE SCARC_STACK_ADMINISTRAION
!
!> \brief Introduce stack hierarchy for the different consecutive solution methods and
!   organize their alternate calls
!
!//////////////////////////////////////////////////////////////////////////////////////////////////////
MODULE SCARC_STACK
  
USE GLOBAL_CONSTANTS
USE PRECISION_PARAMETERS, ONLY: EB, FB
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
USE MPI
USE SCARC_CONSTANTS
USE SCARC_VARIABLES
USE SCARC_MESSAGES
USE SCARC_STORAGE
USE SCARC_CONVERGENCE

IMPLICIT NONE

CONTAINS

! ----------------------------------------------------------------------------------------------------
!> \brief Setup environent on specified stack level
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_STACK(NSTACK)
USE SCARC_POINTERS, ONLY: SV
INTEGER, INTENT(IN):: NSTACK

SV => STACK(NSTACK)%SOLVER

SV%TYPE_ACCURACY      = TYPE_ACCURACY 
SV%TYPE_COARSE        = TYPE_COARSE
SV%TYPE_COARSENING    = TYPE_COARSENING
SV%TYPE_CYCLING       = TYPE_CYCLING
SV%TYPE_EXCHANGE      = TYPE_EXCHANGE
SV%TYPE_GRID          = TYPE_GRID
SV%TYPE_INTERPOL      = TYPE_INTERPOL
SV%TYPE_LEVEL         = TYPE_LEVEL 
SV%TYPE_MATRIX        = TYPE_MATRIX
SV%TYPE_METHOD        = TYPE_METHOD
SV%TYPE_MKL           = TYPE_MKL
SV%TYPE_MKL_PRECISION = TYPE_MKL_PRECISION
SV%TYPE_MULTIGRID     = TYPE_MULTIGRID
SV%TYPE_PARENT        = TYPE_PARENT
SV%TYPE_PRECON        = TYPE_PRECON
SV%TYPE_RELAX         = TYPE_RELAX
SV%TYPE_SCOPE         = TYPE_SCOPE
SV%TYPE_SMOOTH        = TYPE_SMOOTH
SV%TYPE_SOLVER        = TYPE_SOLVER
SV%TYPE_STAGE         = TYPE_STAGE
SV%TYPE_STENCIL       = TYPE_STENCIL
SV%TYPE_TWOLEVEL      = TYPE_TWOLEVEL
SV%TYPE_VECTOR        = TYPE_VECTOR

END SUBROUTINE SCARC_SETUP_STACK


! ----------------------------------------------------------------------------------------------------
!> \brief Restore environent on specified stack level
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_RESTORE_STACK(NSTACK)
USE SCARC_POINTERS, ONLY: SV
INTEGER, INTENT(IN) :: NSTACK

SV => STACK(NSTACK)%SOLVER

TYPE_ACCURACY      = SV%TYPE_ACCURACY 
TYPE_COARSE        = SV%TYPE_COARSE
TYPE_COARSENING    = SV%TYPE_COARSENING
TYPE_CYCLING       = SV%TYPE_CYCLING
TYPE_EXCHANGE      = SV%TYPE_EXCHANGE
TYPE_GRID          = SV%TYPE_GRID
TYPE_INTERPOL      = SV%TYPE_INTERPOL
TYPE_LEVEL         = SV%TYPE_LEVEL 
TYPE_MATRIX        = SV%TYPE_MATRIX
TYPE_METHOD        = SV%TYPE_METHOD
TYPE_MKL           = SV%TYPE_MKL
TYPE_MKL_PRECISION = SV%TYPE_MKL_PRECISION
TYPE_MULTIGRID     = SV%TYPE_MULTIGRID
TYPE_PARENT        = SV%TYPE_PARENT
TYPE_PRECON        = SV%TYPE_PRECON
TYPE_RELAX         = SV%TYPE_RELAX
TYPE_SCOPE         = SV%TYPE_SCOPE
TYPE_SMOOTH        = SV%TYPE_SMOOTH
TYPE_SOLVER        = SV%TYPE_SOLVER
TYPE_STAGE         = SV%TYPE_STAGE
TYPE_STENCIL       = SV%TYPE_STENCIL
TYPE_TWOLEVEL      = SV%TYPE_TWOLEVEL
TYPE_VECTOR        = SV%TYPE_VECTOR

END SUBROUTINE SCARC_RESTORE_STACK


! ----------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize vectors for Krylov method
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_VECTORS()
USE SCARC_POINTERS, ONLY: G, SV, ST, SCARC_POINT_TO_GRID
INTEGER :: NM, NSTACK, NL

CROUTINE = 'SCARC_SETUP_VECTORS'

! If multiple grid types are used (currently only in MGM method) take care that the vectors are
! allocated in the longest necessary length which corresponds to the structured discretization.
! The related workspaces are also used for possible shorter instances in the unstructured discretization
IF (HAS_MULTIPLE_GRIDS) CALL SCARC_SET_GRID_TYPE(NSCARC_GRID_STRUCTURED)   

DO NSTACK = 1, N_STACK_TOTAL

   SV  => STACK(NSTACK)%SOLVER

   DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
      DO NL = SV%TYPE_LEVEL(1), SV%TYPE_LEVEL(2)

         CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
         ST => SCARC(NM)%LEVEL(NL)%STAGE(SV%TYPE_STAGE)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SETUP_VECTORS: NC, NCE =', G%NC, G%NCE, SV%X, SV%B, SV%D 
#endif
         IF (SV%X /= NSCARC_UNDEF_INT) CALL SCARC_ALLOCATE_REAL1(ST%X, 1, G%NCE, NSCARC_INIT_ZERO, 'ST%X', CROUTINE)
         IF (SV%B /= NSCARC_UNDEF_INT) CALL SCARC_ALLOCATE_REAL1(ST%B, 1, G%NCE, NSCARC_INIT_ZERO, 'ST%B', CROUTINE)
         IF (SV%D /= NSCARC_UNDEF_INT) CALL SCARC_ALLOCATE_REAL1(ST%D, 1, G%NCE, NSCARC_INIT_ZERO, 'ST%D', CROUTINE)
         IF (SV%R /= NSCARC_UNDEF_INT) CALL SCARC_ALLOCATE_REAL1(ST%R, 1, G%NCE, NSCARC_INIT_ZERO, 'ST%R', CROUTINE)
         IF (SV%V /= NSCARC_UNDEF_INT) CALL SCARC_ALLOCATE_REAL1(ST%V, 1, G%NCE, NSCARC_INIT_ZERO, 'ST%V', CROUTINE)
         IF (SV%Y /= NSCARC_UNDEF_INT) CALL SCARC_ALLOCATE_REAL1(ST%Y, 1, G%NCE, NSCARC_INIT_ZERO, 'ST%Y', CROUTINE)
         IF (SV%Z /= NSCARC_UNDEF_INT) CALL SCARC_ALLOCATE_REAL1(ST%Z, 1, G%NCE, NSCARC_INIT_ZERO, 'ST%Z', CROUTINE)

#ifdef WITH_SCARC_DEBUG
         IF (SV%E /= NSCARC_UNDEF_INT) CALL SCARC_ALLOCATE_REAL1(ST%E, 1, G%NCE, NSCARC_INIT_ZERO, 'ST%E', CROUTINE)
#endif

         IF (TYPE_MKL_PRECISION == NSCARC_PRECISION_SINGLE) THEN 
            IF (SV%X_FB /= NSCARC_UNDEF_INT) CALL SCARC_ALLOCATE_REAL1_FB(ST%X_FB, 1, G%NCE, NSCARC_INIT_ZERO, 'ST%X_FB', CROUTINE)
            IF (SV%B_FB /= NSCARC_UNDEF_INT) CALL SCARC_ALLOCATE_REAL1_FB(ST%B_FB, 1, G%NCE, NSCARC_INIT_ZERO, 'ST%B_FB', CROUTINE)
            IF (SV%R_FB /= NSCARC_UNDEF_INT) CALL SCARC_ALLOCATE_REAL1_FB(ST%R_FB, 1, G%NCE, NSCARC_INIT_ZERO, 'ST%R_FB', CROUTINE)
            IF (SV%V_FB /= NSCARC_UNDEF_INT) CALL SCARC_ALLOCATE_REAL1_FB(ST%V_FB, 1, G%NCE, NSCARC_INIT_ZERO, 'ST%V_FB', CROUTINE)
         ENDIF

      ENDDO
   ENDDO
ENDDO

END SUBROUTINE SCARC_SETUP_VECTORS

! ------------------------------------------------------------------------------------------------
!> \brief  Setup environement for current solver 
! i.e. set pointers to used vectors related to current position in stack
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_SOLVER(NS, NP)
USE SCARC_POINTERS, ONLY: SV, SVP
INTEGER, INTENT(IN) :: NS, NP                          ! references to current stack and parent
 
! If not first solver in stack, store last iteration parameters of parent solver NP
 
IF (NP > 0) THEN
   SVP => STACK(NP)%SOLVER
   SVP%ITE   = ITE
   SVP%RES   = RES
   SVP%RESIN = RESIN
   SVP%CAPPA = CAPPA
ENDIF
 
! Set new environment for solver on stack position NS
 
SV => STACK(NS)%SOLVER

CNAME = SV%CNAME
ITE   = 0
NIT   = SV%NIT
EPS   = SV%EPS
OMEGA = SV%OMEGA
RESIN = SV%RESIN
CAPPA = -1.0

TYPE_PARENT = NP

TYPE_ACCURACY       = SV%TYPE_ACCURACY
TYPE_COARSE         = SV%TYPE_COARSE
TYPE_COARSENING     = SV%TYPE_COARSENING
TYPE_CYCLING        = SV%TYPE_CYCLING
TYPE_GRID           = SV%TYPE_GRID
TYPE_EXCHANGE       = SV%TYPE_EXCHANGE
TYPE_INTERPOL       = SV%TYPE_INTERPOL
TYPE_LEVEL          = SV%TYPE_LEVEL
TYPE_MATRIX         = SV%TYPE_MATRIX
TYPE_METHOD         = SV%TYPE_METHOD
TYPE_MKL            = SV%TYPE_MKL
TYPE_MKL_PRECISION  = SV%TYPE_MKL_PRECISION
TYPE_MULTIGRID      = SV%TYPE_MULTIGRID
TYPE_PARENT         = SV%TYPE_PARENT
TYPE_PRECON         = SV%TYPE_PRECON
TYPE_RELAX          = SV%TYPE_RELAX
TYPE_SCOPE          = SV%TYPE_SCOPE
TYPE_SMOOTH         = SV%TYPE_SMOOTH
TYPE_SOLVER         = SV%TYPE_SOLVER
TYPE_STAGE          = SV%TYPE_STAGE
TYPE_STENCIL        = SV%TYPE_STENCIL
TYPE_TWOLEVEL       = SV%TYPE_TWOLEVEL
TYPE_VECTOR         = SV%TYPE_VECTOR

X = SV%X
B = SV%B
D = SV%D
R = SV%R
V = SV%V
Y = SV%Y
Z = SV%Z

#ifdef WITH_SCARC_DEBUG
E = SV%E
#endif

IF (TYPE_SOLVER == NSCARC_SOLVER_MAIN) ITE_TOTAL = 0

END SUBROUTINE SCARC_SETUP_SOLVER


! ------------------------------------------------------------------------------------------------
!> \brief Reset environment of calling routine when leaving solver
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_RELEASE_SOLVER(NS, NP)
USE SCARC_POINTERS, ONLY: SV, SVP
INTEGER, INTENT(IN)  :: NS, NP                            ! references to current stack and parent

SV  => STACK(NS)%SOLVER

! Store convergence information of preceding solver for FDS dump routine
 
IF (TYPE_SOLVER == NSCARC_SOLVER_MAIN) THEN
   SCARC_CAPPA      = CAPPA
   SCARC_RESIDUAL   = RES
   SCARC_ITERATIONS = ITE
   !CALL SCARC_CLOSE_CSV_FILE()
ENDIF

SV%RESIN = RESIN
SV%RES   = RES
SV%ITE   = ITE
SV%CAPPA = CAPPA

! If not first solver in stack, reset environment of parent (calling) routine
 
IF (NP > 0) THEN

   SVP => STACK(NP)%SOLVER

   ITE   = SVP%ITE
   NIT   = SVP%NIT
   EPS   = SVP%EPS
   RESIN = SVP%RESIN
   RES   = SVP%RES
   OMEGA = SVP%OMEGA
   CAPPA = SVP%CAPPA

   TYPE_ACCURACY       = SVP%TYPE_ACCURACY
   TYPE_COARSE         = SVP%TYPE_COARSE
   TYPE_COARSENING     = SVP%TYPE_COARSENING
   TYPE_CYCLING        = SVP%TYPE_CYCLING
   TYPE_GRID           = SVP%TYPE_GRID
   TYPE_EXCHANGE       = SVP%TYPE_EXCHANGE
   TYPE_INTERPOL       = SVP%TYPE_INTERPOL
   TYPE_LEVEL          = SVP%TYPE_LEVEL
   TYPE_MATRIX         = SVP%TYPE_MATRIX
   TYPE_METHOD         = SVP%TYPE_METHOD
   TYPE_MKL            = SVP%TYPE_MKL
   TYPE_MKL_PRECISION  = SVP%TYPE_MKL_PRECISION
   TYPE_MULTIGRID      = SVP%TYPE_MULTIGRID
   TYPE_PARENT         = SVP%TYPE_PARENT
   TYPE_PRECON         = SVP%TYPE_PRECON
   TYPE_RELAX          = SVP%TYPE_RELAX
   TYPE_SCOPE          = SVP%TYPE_SCOPE
   TYPE_SMOOTH         = SVP%TYPE_SMOOTH
   TYPE_SOLVER         = SVP%TYPE_SOLVER
   TYPE_STAGE          = SVP%TYPE_STAGE
   TYPE_STENCIL        = SVP%TYPE_STENCIL
   TYPE_TWOLEVEL       = SVP%TYPE_TWOLEVEL
   TYPE_VECTOR         = SVP%TYPE_VECTOR

   X = SVP%X
   B = SVP%B
   D = SVP%D
   R = SVP%R
   V = SVP%V
   Y = SVP%Y
   Z = SVP%Z

#ifdef WITH_SCARC_DEBUG
   E = SVP%E
#endif

ENDIF

END SUBROUTINE SCARC_RELEASE_SOLVER

! ----------------------------------------------------------------------------------------------------
!> \brief Setup references to solution vectors related to used scope (main solver or preconditioner)
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_REFERENCES(BX, BB, BD, BR, BV, BY, BZ, NSTACK)
USE SCARC_POINTERS, ONLY: SV
LOGICAL, INTENT(IN) :: BX, BB, BD, BR, BV, BY, BZ
INTEGER, INTENT(IN) :: NSTACK

SV  => STACK(NSTACK)%SOLVER

SELECT CASE (SV%TYPE_STAGE)

   ! Solver from working stage ONE, e.g. main Krylov or MG solver
 
   CASE (NSCARC_STAGE_ONE)
      IF (BX) SV%X = NSCARC_VECTOR_ONE_X
      IF (BB) SV%B = NSCARC_VECTOR_ONE_B
      IF (BD) SV%D = NSCARC_VECTOR_ONE_D
      IF (BR) SV%R = NSCARC_VECTOR_ONE_R
      IF (BV) SV%V = NSCARC_VECTOR_ONE_V
      IF (BY) SV%Y = NSCARC_VECTOR_ONE_Y
      IF (BZ) SV%Z = NSCARC_VECTOR_ONE_Z

#ifdef WITH_SCARC_MKL
     IF (TYPE_MKL_PRECISION == NSCARC_PRECISION_SINGLE) THEN
        IF (BX) SV%X_FB = NSCARC_VECTOR_ONE_X
        IF (BB) SV%B_FB = NSCARC_VECTOR_ONE_B
        IF (BR) SV%R_FB = NSCARC_VECTOR_ONE_R
        IF (BV) SV%V_FB = NSCARC_VECTOR_ONE_V
     ENDIF
#endif

#ifdef WITH_SCARC_DEBUG
      SV%E = NSCARC_VECTOR_ONE_E
#endif
 
   ! Solver from working stage TWO, e.g. MG solver as preconditioner
 
   CASE (NSCARC_STAGE_TWO)
      IF (BX) SV%X = NSCARC_VECTOR_TWO_X
      IF (BB) SV%B = NSCARC_VECTOR_TWO_B
      IF (BD) SV%D = NSCARC_VECTOR_TWO_D
      IF (BR) SV%R = NSCARC_VECTOR_TWO_R
      IF (BV) SV%V = NSCARC_VECTOR_TWO_V
      IF (BY) SV%Y = NSCARC_VECTOR_TWO_Y
      IF (BZ) SV%Z = NSCARC_VECTOR_TWO_Z
#ifdef WITH_SCARC_DEBUG
      SV%E = NSCARC_VECTOR_TWO_E
#endif

#ifdef WITH_SCARC_MKL
     IF (TYPE_MKL_PRECISION == NSCARC_PRECISION_SINGLE) THEN
        IF (BX) SV%X_FB = NSCARC_VECTOR_TWO_X
        IF (BB) SV%B_FB = NSCARC_VECTOR_TWO_B
        IF (BR) SV%R_FB = NSCARC_VECTOR_TWO_R
        IF (BV) SV%V_FB = NSCARC_VECTOR_TWO_V
     ENDIF
#endif

END SELECT

END SUBROUTINE SCARC_SETUP_REFERENCES

! ----------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize vectors for requested preconditioner
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_PRECON(NSTACK, NSCOPE)
USE SCARC_POINTERS, ONLY: SV, SVP
INTEGER, INTENT(IN) :: NSTACK, NSCOPE
 
! Point to stack entry of called preconditioner and types for preconditioner
 
SV => STACK(NSTACK)%SOLVER

SV%TYPE_SCOPE(0) = NSCOPE

SV%EPS   =  SCARC_PRECON_ACCURACY
SV%NIT   =  SCARC_PRECON_ITERATIONS
SV%OMEGA =  SCARC_PRECON_OMEGA

! Preset name of preconditioning method
 
SELECT CASE(TYPE_PRECON)
   CASE (NSCARC_RELAX_JAC)
      SV%CNAME = 'SCARC_PRECON_JAC'
   CASE (NSCARC_RELAX_SSOR)
      SV%CNAME = 'SCARC_PRECON_SSOR'
   CASE (NSCARC_RELAX_MJAC)
      SV%CNAME = 'SCARC_PRECON_MJAC'
   CASE (NSCARC_RELAX_MGS)
      SV%CNAME = 'SCARC_PRECON_MGS'
   CASE (NSCARC_RELAX_MSGS)
      SV%CNAME = 'SCARC_PRECON_MSGS'
   CASE (NSCARC_RELAX_MSOR)
      SV%CNAME = 'SCARC_PRECON_MSOR'
   CASE (NSCARC_RELAX_MSSOR)
      SV%CNAME = 'SCARC_PRECON_MSSOR'
   CASE (NSCARC_RELAX_LU)
      SV%CNAME = 'SCARC_PRECON_LU'
   CASE (NSCARC_RELAX_ILU)
      SV%CNAME = 'SCARC_PRECON_ILU'
      SV%OMEGA = 1.0_EB
   CASE (NSCARC_RELAX_FFT)
      SV%CNAME = 'SCARC_PRECON_FFT'
      SV%OMEGA = 1.0_EB
   CASE (NSCARC_RELAX_FFTO)
      SV%CNAME = 'SCARC_PRECON_FFTO'
      SV%OMEGA = 1.0_EB
   CASE (NSCARC_RELAX_MULTIGRID)
      SV%CNAME = 'SCARC_PRECON_MG'
#ifdef WITH_SCARC_MKL
   CASE (NSCARC_RELAX_MKL)
      SV%OMEGA = 1.0_EB
      IF (NSCOPE == NSCARC_SCOPE_LOCAL) THEN
         SV%CNAME = 'SCARC_PRECON_PARDISO'
      ELSE
         SV%CNAME = 'SCARC_PRECON_CLUSTER'
         SV%NIT   = 1
      ENDIF
#endif
   CASE DEFAULT
      CALL SCARC_SHUTDOWN(NSCARC_ERROR_PARSE_INPUT, SCARC_NONE, TYPE_PRECON)
END SELECT

! Preset types for preconditioner (use same as for calling solver)
 
SVP => STACK(NSTACK-1)%SOLVER

SV%TYPE_ACCURACY      = SVP%TYPE_ACCURACY
SV%TYPE_GRID          = SVP%TYPE_GRID
SV%TYPE_LEVEL(0:2)    = SVP%TYPE_LEVEL(0:2)
SV%TYPE_INTERPOL      = SVP%TYPE_INTERPOL
SV%TYPE_MATRIX        = SVP%TYPE_MATRIX
SV%TYPE_MKL_PRECISION = SVP%TYPE_MKL_PRECISION
SV%TYPE_RELAX         = SVP%TYPE_RELAX
SV%TYPE_SOLVER        = SVP%TYPE_SOLVER
SV%TYPE_STAGE         = SVP%TYPE_STAGE
 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SETUP_PRECON: TYPE_RELAX =', SV%TYPE_RELAX
#endif

! Preset pointers for preconditioner (use same as for alling solver)
 
SV%X = SVP%X
SV%B = SVP%B
SV%D = SVP%D
SV%R = SVP%R
SV%V = SVP%V
SV%Y = SVP%Y
SV%Z = SVP%Z

IF (TYPE_MKL_PRECISION == NSCARC_PRECISION_SINGLE) THEN
   SV%X_FB = SVP%X_FB
   SV%B_FB = SVP%B_FB
   SV%R_FB = SVP%R_FB
   SV%V_FB = SVP%V_FB
ENDIF

#ifdef WITH_SCARC_DEBUG
SV%E = SVP%E
#endif

END SUBROUTINE SCARC_SETUP_PRECON

! ----------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize vectors for Krylov method
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_SMOOTH(NSTACK, NSCOPE)
USE SCARC_POINTERS, ONLY: SV, SVP
INTEGER, INTENT(IN) :: NSTACK, NSCOPE
 
! Basic setup of stack information and types/names for smoother
 
CALL SCARC_SETUP_STACK(NSTACK)

SV => STACK(NSTACK)%SOLVER
SV%TYPE_SCOPE(0) = NSCOPE
SV%OMEGA =  SCARC_SMOOTH_OMEGA

SELECT CASE(TYPE_SMOOTH)
   CASE (NSCARC_RELAX_JAC)
      SV%CNAME = 'SCARC_SMOOTH_JAC'
   CASE (NSCARC_RELAX_SSOR)
      SV%CNAME = 'SCARC_SMOOTH_SSOR'
   CASE (NSCARC_RELAX_MJAC)
      SV%CNAME = 'SCARC_SMOOTH_MJAC'
   CASE (NSCARC_RELAX_MGS)
      SV%CNAME = 'SCARC_SMOOTH_MGS'
   CASE (NSCARC_RELAX_MSGS)
      SV%CNAME = 'SCARC_SMOOTH_MSGS'
   CASE (NSCARC_RELAX_MSOR)
      SV%CNAME = 'SCARC_SMOOTH_MSOR'
   CASE (NSCARC_RELAX_MSSOR)
      SV%CNAME = 'SCARC_SMOOTH_MSSOR'
   CASE (NSCARC_RELAX_ILU)
      SV%CNAME = 'SCARC_SMOOTH_ILU'
   CASE (NSCARC_RELAX_FFT)
      SV%CNAME = 'SCARC_SMOOTH_FFT'
      SV%OMEGA = 1.0_EB
   CASE (NSCARC_RELAX_FFTO)
      SV%CNAME = 'SCARC_SMOOTH_FFTO'
      SV%OMEGA = 1.0_EB
#ifdef WITH_SCARC_MKL
   CASE (NSCARC_RELAX_MKL)
      SV%OMEGA = 1.0_EB
      IF (NSCOPE == NSCARC_SCOPE_GLOBAL) THEN
         SV%CNAME = 'SCARC_SMOOTH_CLUSTER'
      ELSE
         SV%CNAME = 'SCARC_SMOOTH_PARDISO'
      ENDIF
#endif
   CASE DEFAULT
      CALL SCARC_SHUTDOWN(NSCARC_ERROR_PARSE_INPUT, SCARC_NONE, TYPE_SMOOTH)
END SELECT

! Preset iteration parameters for Multigrid method
 
SV%EPS = SCARC_SMOOTH_ACCURACY
SV%NIT = SCARC_SMOOTH_ITERATIONS

! Preset types for preconditioner (use same descriptors as calling solver)
 
SVP => STACK(NSTACK-1)%SOLVER

SV%TYPE_SOLVER        = NSCARC_SOLVER_SMOOTH
SV%TYPE_STAGE         = SVP%TYPE_STAGE
SV%TYPE_LEVEL(0:2)    = SVP%TYPE_LEVEL(0:2)
SV%TYPE_RELAX         = SVP%TYPE_RELAX
SV%TYPE_GRID          = SVP%TYPE_GRID
SV%TYPE_INTERPOL      = SVP%TYPE_INTERPOL
SV%TYPE_ACCURACY      = SVP%TYPE_ACCURACY
SV%TYPE_MKL_PRECISION = SVP%TYPE_MKL_PRECISION
SV%TYPE_MATRIX        = SVP%TYPE_MATRIX

! Preset references for preconditioner (use same pointers as calling solver)
SV%X = SVP%X
SV%B = SVP%B
SV%D = SVP%D
SV%R = SVP%R
SV%V = SVP%V
SV%Y = SVP%Y
SV%Z = SVP%Z

IF (TYPE_MKL_PRECISION == NSCARC_PRECISION_SINGLE) THEN
   SV%X_FB = SVP%X_FB
   SV%B_FB = SVP%B_FB
   SV%R_FB = SVP%R_FB
   SV%V_FB = SVP%V_FB
ENDIF

#ifdef WITH_SCARC_DEBUG
SV%E = SVP%E
SV%R = SVP%R
#endif

END SUBROUTINE SCARC_SETUP_SMOOTH

! ----------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize vectors for Krylov method
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_KRYLOV(NSOLVER, NSCOPE, NSTAGE, NSTACK, NLMIN, NLMAX)
USE SCARC_POINTERS, ONLY: SV
INTEGER, INTENT(IN) :: NSOLVER, NSCOPE, NSTAGE, NSTACK, NLMIN, NLMAX

! Basic setup of stack information and types for Krylov method
 
CALL SCARC_SETUP_STACK(NSTACK)

SV  => STACK(NSTACK)%SOLVER
SV%TYPE_METHOD   = NSCARC_METHOD_KRYLOV
SV%TYPE_SOLVER   = NSOLVER
SV%TYPE_SCOPE(0) = NSCOPE
SV%TYPE_STAGE    = NSTAGE
SV%TYPE_LEVEL(1) = NLMIN
SV%TYPE_LEVEL(2) = NLMAX
SV%TYPE_MATRIX   = TYPE_MATRIX

! Preset iteration parameters for Krylov method

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SETUP_KRYLOV=', SCARC_KRYLOV_ITERATIONS, NSOLVER, NSCOPE, NSTAGE, NSTACK
#endif
SELECT CASE(NSOLVER)

   ! -------------- Krylov method is used as main solver
   CASE (NSCARC_SOLVER_MAIN)
   
      SV%CNAME = 'SCARC_MAIN_KRYLOV'
   
      SV%EPS = SCARC_KRYLOV_ACCURACY
      SV%NIT = SCARC_KRYLOV_ITERATIONS
   
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SV%NIT=', SV%NIT,SCARC_KRYLOV_ITERATIONS
#endif
      SV%TYPE_RELAX    = TYPE_PRECON
      SV%TYPE_TWOLEVEL = TYPE_TWOLEVEL
   
   ! -------------- Krylov method is used as coarse grid solver solver
   CASE (NSCARC_SOLVER_COARSE)
   
      SV%CNAME = 'SCARC_COARSE_KRYLOV'
   
      SV%EPS = SCARC_COARSE_ACCURACY
      SV%NIT = SCARC_COARSE_ITERATIONS
   
      SV%TYPE_RELAX    = NSCARC_RELAX_SSOR             ! only use SSOR-preconditioning for coarse solver
      SV%TYPE_TWOLEVEL = NSCARC_TWOLEVEL_NONE          ! only use one level for coarse solver
   
   ! -------------- Otherwise: print error message
   CASE DEFAULT
   
      CALL SCARC_SHUTDOWN(NSCARC_ERROR_PARSE_INPUT, SCARC_NONE, NSOLVER)
   
END SELECT

 
! Point to solution vectors (in corresponding scope)
 
CALL SCARC_SETUP_REFERENCES(.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE., NSTACK)

END SUBROUTINE SCARC_SETUP_KRYLOV

! ----------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize vectors for Geometric Multigrid method
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MULTIGRID(NSOLVER, NSCOPE, NSTAGE, NSTACK, NLMIN, NLMAX)
USE SCARC_POINTERS, ONLY: SV
INTEGER, INTENT(IN) :: NSOLVER, NSCOPE, NSTAGE, NSTACK, NLMIN, NLMAX

 
! Basic setup of stack information and types for multigrid method
 
CALL SCARC_SETUP_STACK(NSTACK)

SV  => STACK(NSTACK)%SOLVER
SV%TYPE_METHOD        = NSCARC_METHOD_MULTIGRID
SV%TYPE_SOLVER        = NSOLVER
SV%TYPE_SCOPE(0)      = NSCOPE
SV%TYPE_STAGE         = NSTAGE
SV%TYPE_LEVEL(0)      = NLMAX-NLMIN+1
SV%TYPE_LEVEL(1)      = NLMIN
SV%TYPE_LEVEL(2)      = NLMAX
SV%TYPE_RELAX         = TYPE_SMOOTH
SV%TYPE_GRID          = TYPE_GRID
SV%TYPE_INTERPOL      = TYPE_INTERPOL
SV%TYPE_ACCURACY      = TYPE_ACCURACY
SV%TYPE_CYCLING       = TYPE_CYCLING
SV%TYPE_MATRIX        = TYPE_MATRIX
SV%TYPE_MKL_PRECISION = TYPE_MKL_PRECISION

SELECT CASE(NSOLVER)

   ! -------------- Multigrid method is used as main solver
   CASE (NSCARC_SOLVER_MAIN)
      SV%CNAME = 'SCARC_MAIN_MG'

   ! -------------- Multigrid method is only used as preconditioner for global Krylov method
   CASE (NSCARC_SOLVER_PRECON)
      SV%CNAME = 'SCARC_PRECON_MG'

   ! -------------- Otherwise: print error message
   CASE DEFAULT
      CALL SCARC_SHUTDOWN(NSCARC_ERROR_PARSE_INPUT, SCARC_NONE, NSOLVER)

END SELECT

 
! Preset iteration parameters for Multigrid method
 
SV%EPS = SCARC_MULTIGRID_ACCURACY
SV%NIT = SCARC_MULTIGRID_ITERATIONS

 
! Point to solution vectors (in corresponding scope)
 
CALL SCARC_SETUP_REFERENCES(.TRUE.,.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE., NSTACK)

END SUBROUTINE SCARC_SETUP_MULTIGRID

END MODULE SCARC_STACK


