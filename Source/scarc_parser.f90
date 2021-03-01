!=======================================================================================================================
! 
! MODULE SCARC_PARSER
! 
!> \brief Parse ScaRC related input parameters and initialize basic ScaRC structures
!
!=======================================================================================================================
MODULE SCARC_PARSER
  
USE GLOBAL_CONSTANTS
USE PRECISION_PARAMETERS, ONLY: EB, FB
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
USE SCARC_CONSTANTS
USE SCARC_VARIABLES
USE SCARC_MESSAGES, ONLY: MSG
USE SCARC_TROUBLESHOOTING
USE SCARC_STACK, ONLY: STACK

IMPLICIT NONE

CONTAINS

! ------------------------------------------------------------------------------------------------------------------
!> \brief Determine input parameters needed for setting up the discretization structure
! ------------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_PARSE_INPUT

ITERATE_PRESSURE = .TRUE.  ! Although there is no need to do pressure iterations to drive down 
                           ! velocity error leave it .TRUE. to write out velocity error diagnostics
 
! ------------- Set type of discretization
 
SELECT CASE (TRIM(PRES_METHOD))
   CASE ('SCARC')
      TYPE_GRID = NSCARC_GRID_STRUCTURED
      IS_STRUCTURED   = .TRUE.
      IS_UNSTRUCTURED = .FALSE.
   CASE ('USCARC')
      TYPE_GRID = NSCARC_GRID_UNSTRUCTURED
      IS_STRUCTURED   = .FALSE.
      IS_UNSTRUCTURED = .TRUE.
   CASE DEFAULT
      CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_GRID, NSCARC_NONE)
END SELECT
 
! ------------ Set type of matrix storage (COMPACT/BANDWISE)
 
SELECT CASE (TRIM(SCARC_MATRIX))
   CASE ('COMPACT')
      TYPE_MATRIX = NSCARC_MATRIX_COMPACT
   CASE ('BANDWISE')
      TYPE_MATRIX = NSCARC_MATRIX_BANDWISE
   CASE DEFAULT
      CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_MATRIX, NSCARC_NONE)
END SELECT

! ------------ Set type of matrix stencil (CONSTANT/VARIABLE)
 
SELECT CASE (TRIM(SCARC_STENCIL))
   CASE ('CONSTANT')
      TYPE_STENCIL = NSCARC_STENCIL_CONSTANT
   CASE ('VARIABLE')
      TYPE_STENCIL = NSCARC_STENCIL_VARIABLE
   CASE DEFAULT
      CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_STENCIL, NSCARC_NONE)
END SELECT
 
! ------------ Set type of global solver
 
SELECT CASE (TRIM(SCARC_METHOD))

   ! ------------------------- Global Krylov solver ----------------------------------
   CASE ('KRYLOV')

      TYPE_METHOD = NSCARC_METHOD_KRYLOV

      ! Set type of two-level method
      SELECT CASE (TRIM(SCARC_TWOLEVEL))
         CASE ('NONE')
            TYPE_TWOLEVEL = NSCARC_TWOLEVEL_NONE
         CASE ('ADDITIVE')
            TYPE_TWOLEVEL = NSCARC_TWOLEVEL_ADD
         CASE ('MULTIPLICATIVE')
            TYPE_TWOLEVEL = NSCARC_TWOLEVEL_MUL
         CASE ('MULTIPLICATIVE2')
            TYPE_TWOLEVEL = NSCARC_TWOLEVEL_MUL2
         CASE ('COARSE')
            TYPE_TWOLEVEL = NSCARC_TWOLEVEL_COARSE
         CASE ('MACRO')
            TYPE_TWOLEVEL = NSCARC_TWOLEVEL_MACRO
         CASE DEFAULT
            CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_TWOLEVEL, NSCARC_NONE)
      END SELECT

      ! Set type of interpolation for two-level Krylov method
      SELECT CASE (TRIM(SCARC_KRYLOV_INTERPOL))
         CASE ('NONE')
            TYPE_INTERPOL = NSCARC_UNDEF_INT
         CASE ('CONSTANT')
            TYPE_INTERPOL = NSCARC_INTERPOL_CONSTANT
         CASE ('BILINEAR')
            TYPE_INTERPOL = NSCARC_INTERPOL_BILINEAR
         CASE ('BILINEAR2')
            TYPE_INTERPOL = NSCARC_INTERPOL_BILINEAR2
         CASE ('AMG')
            TYPE_INTERPOL = NSCARC_INTERPOL_AMG
         CASE DEFAULT
            CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_KRYLOV_INTERPOL, NSCARC_NONE)
      END SELECT

      ! Set type of preconditioner (JACOBI/SSOR/MGS/MSGS/MSOR/MSSOR/ILU/LU/FFT/GMG/PARDISO/CLUSTER/OPTIMIZED)
      SELECT CASE (TRIM(SCARC_PRECON))
         CASE ('JACOBI')                                    ! Jacobi preconditioner
            TYPE_PRECON = NSCARC_RELAX_JAC
         CASE ('SSOR')                                      ! Symmetric SOR preconditioner
            TYPE_PRECON = NSCARC_RELAX_SSOR
         CASE ('MJAC')                                      ! Jacobi preconditioner in matrix form
            TYPE_PRECON = NSCARC_RELAX_MJAC
         CASE ('MGS')                                       ! Gauss-Seidel preconditioner in matrix form
            TYPE_PRECON = NSCARC_RELAX_MGS
         CASE ('MSGS')                                      ! Symmetric Gauss-Seidel preconditioner in matrix form
            TYPE_PRECON = NSCARC_RELAX_MSGS
         CASE ('MSOR')                                      ! SOR preconditioner in matrix form
            TYPE_PRECON = NSCARC_RELAX_MSOR
         CASE ('MSSOR')                                     ! Symmetric SOR preconditioner in matrix form
            TYPE_PRECON = NSCARC_RELAX_MSSOR
         CASE ('ILU')                                       ! ILU preconditioner
            TYPE_PRECON = NSCARC_RELAX_ILU
         CASE ('LU')                                        ! LU preconditioner
            TYPE_PRECON = NSCARC_RELAX_LU
         CASE ('MULTIGRID')                                 ! Multigrid preconditioner
            TYPE_PRECON = NSCARC_RELAX_GMG
            SELECT CASE (TRIM(SCARC_SMOOTH))
               CASE ('JACOBI')
                  TYPE_SMOOTH = NSCARC_RELAX_JAC
               CASE ('SSOR')
                  TYPE_SMOOTH = NSCARC_RELAX_SSOR
               CASE ('ILU')
                  TYPE_SMOOTH = NSCARC_RELAX_ILU
               CASE ('FFT')
                  IF (IS_UNSTRUCTURED) CALL SCARC_ERROR(NSCARC_ERROR_FFT_GRID, SCARC_NONE, NSCARC_NONE)
                  TYPE_SMOOTH = NSCARC_RELAX_FFT
               CASE ('PARDISO')
#ifdef WITH_MKL
                  TYPE_SMOOTH = NSCARC_RELAX_MKL
#else
                  TYPE_SMOOTH = NSCARC_RELAX_SSOR
                  CALL SCARC_WARNING(NSCARC_WARNING_NO_MKL_PRECON, SCARC_NONE, NSCARC_NONE)
#endif

               CASE ('CLUSTER')
#ifdef WITH_MKL
                  TYPE_SMOOTH = NSCARC_RELAX_MKL
#else
                  TYPE_SMOOTH = NSCARC_RELAX_SSOR
                  CALL SCARC_ERROR(NSCARC_WARNING_NO_MKL_PRECON, SCARC_NONE, NSCARC_NONE)
#endif
               CASE DEFAULT
                  CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_SMOOTH, NSCARC_NONE)
            END SELECT
         CASE ('FFT')                                                ! FFT preconditioner
            IF (IS_UNSTRUCTURED) CALL SCARC_ERROR(NSCARC_ERROR_FFT_GRID, SCARC_NONE, NSCARC_NONE)
            TYPE_PRECON = NSCARC_RELAX_FFT
         CASE ('FFTO')                                               ! FFT with overlap preconditioner
            IF (IS_UNSTRUCTURED) CALL SCARC_ERROR(NSCARC_ERROR_FFT_GRID, SCARC_NONE, NSCARC_NONE)
            IF (NMESHES == 1) THEN
               TYPE_PRECON = NSCARC_RELAX_FFT
            ELSE
               TYPE_PRECON = NSCARC_RELAX_FFTO
            ENDIF
         CASE ('OPTIMIZED')                                          ! LU preconditioner based on either FFT or PARDISO
#ifdef WITH_MKL
            TYPE_PRECON   = NSCARC_RELAX_OPTIMIZED
            TYPE_MKL(0)   = NSCARC_MKL_LOCAL
            TYPE_SCOPE(1) = NSCARC_SCOPE_LOCAL
#else
            CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_PRECON, NSCARC_NONE)
#endif
         CASE ('PARDISO')                                            ! LU preconditioner based on MKL-PARDISO
#ifdef WITH_MKL
            TYPE_PRECON   = NSCARC_RELAX_MKL
            TYPE_MKL(0)   = NSCARC_MKL_LOCAL
            TYPE_SCOPE(1) = NSCARC_SCOPE_LOCAL
#else
            TYPE_PRECON   = NSCARC_RELAX_SSOR
            CALL SCARC_WARNING(NSCARC_WARNING_NO_MKL_PRECON, SCARC_NONE, NSCARC_NONE)
#endif
         CASE ('CLUSTER')                            !  LU-preconditioner based on MKL Cluster_Sparse_Solver
#ifdef WITH_MKL
            TYPE_PRECON   = NSCARC_RELAX_MKL
            TYPE_MKL(0)   = NSCARC_MKL_GLOBAL
            TYPE_SCOPE(1) = NSCARC_SCOPE_GLOBAL
#else
            TYPE_PRECON   = NSCARC_RELAX_SSOR
            CALL SCARC_WARNING(NSCARC_WARNING_NO_MKL_PRECON, SCARC_NONE, NSCARC_NONE)
#endif
         CASE DEFAULT
            TYPE_PRECON   = NSCARC_RELAX_SSOR
            TYPE_MKL(0)   = NSCARC_MKL_NONE
            TYPE_SCOPE(1) = NSCARC_SCOPE_LOCAL
            CALL SCARC_WARNING(NSCARC_WARNING_ONLY_SSOR_PRECON, SCARC_NONE, NSCARC_NONE)
      END SELECT

      ! set type scope for preconditioner (GLOBAL/LOCAL)
      SELECT CASE (TRIM(SCARC_PRECON_SCOPE))
         CASE ('GLOBAL')
            TYPE_SCOPE(1) = NSCARC_SCOPE_GLOBAL
         CASE ('LOCAL')
            TYPE_SCOPE(1) = NSCARC_SCOPE_LOCAL
         CASE DEFAULT
            TYPE_SCOPE(1) = NSCARC_SCOPE_LOCAL
            CALL SCARC_WARNING(NSCARC_WARNING_NO_GLOBAL_SCOPE, SCARC_PRECON_SCOPE, NSCARC_NONE)
      END SELECT

   ! ------------------------- Global geometric multigrid solver -------------------------------
   CASE ('MULTIGRID')

      TYPE_METHOD = NSCARC_METHOD_MULTIGRID

      ! Set type of multigrid method (GEOMETRIC/ALGEBRAIC)
      SELECT CASE (TRIM(SCARC_MULTIGRID))
         CASE ('GEOMETRIC')
            TYPE_MULTIGRID = NSCARC_MULTIGRID_GEOMETRIC
            TYPE_COARSENING = NSCARC_COARSENING_GMG         ! GMG-default, may be overwritten by SCARC_COARSENING 
         CASE ('ALGEBRAIC')
            TYPE_MULTIGRID = NSCARC_MULTIGRID_ALGEBRAIC
            TYPE_COARSENING = NSCARC_COARSENING_CUBIC       ! AMG-default, may be overwritten by SCARC_COARSENING 
         CASE DEFAULT
            CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_MULTIGRID, NSCARC_NONE)
      END SELECT

      ! Set type of smoother (JACOBI/SGS/SSOR/MSSOR/ILU/PARDISO/CLUSTER)
      SELECT CASE (TRIM(SCARC_SMOOTH))                      ! use same parameters as for preconditioner
         CASE ('JACOBI')                                    ! Jacobi preconditioner
            TYPE_SMOOTH = NSCARC_RELAX_JAC
         CASE ('SSOR')                                      ! SSOR preconditioner
            TYPE_SMOOTH = NSCARC_RELAX_SSOR
         CASE ('MJAC')                                      ! Jacobi preconditioner in matrix form
            TYPE_SMOOTH = NSCARC_RELAX_MJAC
         CASE ('MGS')                                       ! Gauss-Seidel preconditioner in matrix form
            TYPE_SMOOTH = NSCARC_RELAX_MGS
         CASE ('MSGS')                                      ! Symmetric Gauss-Seidel preconditioner in matrix form
            TYPE_SMOOTH = NSCARC_RELAX_MSGS
         CASE ('MSOR')                                      ! SOR preconditioner in matrix form
            TYPE_SMOOTH = NSCARC_RELAX_MSOR
         CASE ('MSSOR')                                     ! Symmetric SOR preconditioner in matrix form
            TYPE_SMOOTH = NSCARC_RELAX_MSSOR
         CASE ('ILU')
            TYPE_SMOOTH = NSCARC_RELAX_ILU
         CASE ('FFT')
            IF (IS_UNSTRUCTURED) CALL SCARC_ERROR(NSCARC_ERROR_FFT_GRID, SCARC_NONE, NSCARC_NONE)
            TYPE_SMOOTH = NSCARC_RELAX_FFT
         CASE ('FFTO')
            IF (IS_UNSTRUCTURED) CALL SCARC_ERROR(NSCARC_ERROR_FFT_GRID, SCARC_NONE, NSCARC_NONE)
            IF (NMESHES == 1) THEN
               TYPE_SMOOTH = NSCARC_RELAX_FFT
            ELSE
               TYPE_SMOOTH = NSCARC_RELAX_FFTO
            ENDIF
         CASE ('OPTIMIZED')
#ifdef WITH_MKL
            TYPE_SMOOTH = NSCARC_RELAX_OPTIMIZED
#else
            CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_SMOOTH, NSCARC_NONE)
#endif
         CASE ('PARDISO')
#ifdef WITH_MKL
            TYPE_SMOOTH = NSCARC_RELAX_MKL
#else
            TYPE_SMOOTH = NSCARC_RELAX_SSOR
            CALL SCARC_WARNING(NSCARC_WARNING_NO_MKL_SMOOTH, SCARC_NONE, NSCARC_NONE)
#endif
         CASE ('CLUSTER')
#ifdef WITH_MKL
            TYPE_SMOOTH = NSCARC_RELAX_MKL
#else
            TYPE_SMOOTH = NSCARC_RELAX_SSOR
            CALL SCARC_WARNING(NSCARC_WARNING_NO_MKL_SMOOTH, SCARC_NONE, NSCARC_NONE)
#endif
         CASE DEFAULT
            TYPE_SMOOTH = NSCARC_RELAX_SSOR
            CALL SCARC_WARNING(NSCARC_WARNING_ONLY_SSOR_SMOOTH, SCARC_NONE, NSCARC_NONE)
      END SELECT

      ! set type scope for smoother (GLOBAL/LOCAL)
      SELECT CASE (TRIM(SCARC_SMOOTH_SCOPE))
         CASE ('GLOBAL')
            TYPE_SCOPE(2) = NSCARC_SCOPE_GLOBAL
         CASE ('LOCAL')
            TYPE_SCOPE(2) = NSCARC_SCOPE_LOCAL
         CASE DEFAULT
            CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_SMOOTH_SCOPE, NSCARC_NONE)
      END SELECT

   ! ------------------------- Global LU-decomposition solver -------------------------------
   CASE ('MKL')

      TYPE_METHOD  = NSCARC_METHOD_LU

      ! Set type of MKL method (global/local)
      SELECT CASE (TRIM(SCARC_MKL_SCOPE))                  
         CASE ('GLOBAL')
#ifdef WITH_MKL
            TYPE_MKL(0)   = NSCARC_MKL_GLOBAL
#else
            CALL SCARC_ERROR(NSCARC_ERROR_MKL_CLUSTER, SCARC_NONE, NSCARC_NONE)
#endif
         CASE ('LOCAL')
#ifdef WITH_MKL
            TYPE_MKL(0)   = NSCARC_MKL_LOCAL
#else
            CALL SCARC_ERROR(NSCARC_ERROR_MKL_PARDISO, SCARC_NONE, NSCARC_NONE)
#endif
         CASE DEFAULT
            CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_MKL_SCOPE, NSCARC_NONE)
      END SELECT

   ! ------------------------- McKenny-Greengard-Mayo solver -------------------------
   CASE ('MGM')

      !  Both structured and unstructured discretization are required
      !  The second pass of this method is purely locally based (the Laplace solutions) 

      HAS_MULTIPLE_GRIDS = .TRUE.

      TYPE_METHOD = NSCARC_METHOD_MGM
      TYPE_SCOPE(0:1) = NSCARC_SCOPE_LOCAL
#ifdef WITH_MKL
      TYPE_MKL(0:1) = NSCARC_MKL_LOCAL
#endif

      ! set type of MGM interface BCs of Laplace problems
      SELECT CASE (TRIM(SCARC_MGM_BC))
         CASE ('MEAN')
            TYPE_MGM_BC = NSCARC_MGM_BC_MEAN
         CASE ('TRUE')
            TYPE_MGM_BC = NSCARC_MGM_BC_TRUE
         CASE ('EXTRAPOLATION')
            TYPE_MGM_BC = NSCARC_MGM_BC_EXPOL
         CASE ('TAYLOR')
            TYPE_MGM_BC = NSCARC_MGM_BC_TAYLOR
         CASE DEFAULT
            CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_MGM_BC, NSCARC_NONE)
      END SELECT

      ! set type of MGM solver for local Laplace problems 
      SELECT CASE (TRIM(SCARC_MGM_LAPLACE_SOLVER))
         CASE ('AMG')
            TYPE_MGM_LAPLACE = NSCARC_MGM_LAPLACE_AMG
         CASE ('CG','KRYLOV')
            TYPE_MGM_LAPLACE = NSCARC_MGM_LAPLACE_CG
         CASE ('FFT')
            TYPE_MGM_LAPLACE = NSCARC_MGM_LAPLACE_FFT
         CASE ('LU')
            TYPE_MGM_LAPLACE = NSCARC_MGM_LAPLACE_LU
         CASE ('LUPERM')
            TYPE_MGM_LAPLACE = NSCARC_MGM_LAPLACE_LUPERM
         CASE ('OPTIMIZED')
            TYPE_MGM_LAPLACE = NSCARC_MGM_LAPLACE_OPTIMIZED
         CASE ('PARDISO')
            TYPE_MGM_LAPLACE = NSCARC_MGM_LAPLACE_PARDISO
         CASE DEFAULT
            CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_MGM_LAPLACE_SOLVER, NSCARC_NONE)
      END SELECT

      ! set type of MGM interpolation for interface BCs of Laplace problems
      SELECT CASE (TRIM(SCARC_MGM_INTERPOLATION))
         CASE ('LINEAR')
            TYPE_MGM_INTERPOL = NSCARC_MGM_INTERPOL_LINEAR
         CASE ('SQUARE')
            TYPE_MGM_INTERPOL = NSCARC_MGM_INTERPOL_SQUARE
         CASE DEFAULT
            CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_MGM_INTERPOLATION, NSCARC_NONE)
      END SELECT

   CASE DEFAULT
      CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_METHOD, NSCARC_NONE)

END SELECT

! If a multigrid solver is used (either as main solver or as preconditioner)
! set types for multigrid, coarse grid solver and cycling pattern
 
IF (TYPE_METHOD == NSCARC_METHOD_MULTIGRID .OR. TYPE_PRECON == NSCARC_RELAX_GMG) THEN

   ! Set type of multigrid (GEOMETRIC/ALGEBRAIC with corresponding coarsening strategy)
   SELECT CASE (TRIM(SCARC_MULTIGRID))
      CASE ('GEOMETRIC')
         TYPE_MULTIGRID = NSCARC_MULTIGRID_GEOMETRIC
      CASE ('ALGEBRAIC')
         TYPE_MULTIGRID = NSCARC_MULTIGRID_ALGEBRAIC
      CASE DEFAULT
         CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_MULTIGRID, NSCARC_NONE)
   END SELECT

   ! Set type of cycling pattern (F/V/W)
   SELECT CASE (TRIM(SCARC_MULTIGRID_CYCLE))
      CASE ('F')
         TYPE_CYCLING = NSCARC_CYCLING_F
      CASE ('V')
         TYPE_CYCLING = NSCARC_CYCLING_V
      CASE ('W')
         TYPE_CYCLING = NSCARC_CYCLING_W
      CASE ('FMG')
         TYPE_CYCLING = NSCARC_CYCLING_FMG
      CASE DEFAULT
         CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_MULTIGRID_CYCLE, NSCARC_NONE)
   END SELECT

   ! Set type of interpolation 
   SELECT CASE (TRIM(SCARC_MULTIGRID_INTERPOL))
      CASE ('STANDARD')
         TYPE_INTERPOL = NSCARC_INTERPOL_STANDARD
      CASE ('CONSTANT')
         TYPE_INTERPOL = NSCARC_INTERPOL_CONSTANT
      CASE ('BILINEAR')
         TYPE_INTERPOL = NSCARC_INTERPOL_BILINEAR
      CASE ('BILINEAR2')
         TYPE_INTERPOL = NSCARC_INTERPOL_BILINEAR2
      CASE ('CLASSICAL')
         TYPE_INTERPOL = NSCARC_INTERPOL_CLASSICAL
      CASE ('DIRECT')
         TYPE_INTERPOL = NSCARC_INTERPOL_DIRECT
      CASE ('AMG')
         TYPE_INTERPOL = NSCARC_INTERPOL_AMG
      CASE DEFAULT
         CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_MULTIGRID_INTERPOL, NSCARC_NONE)
   END SELECT

ENDIF

! ------------ Set type of coarsening strategy in case of multi-level methods

SELECT CASE (TRIM(SCARC_COARSENING))
   CASE ('AGGREGATED')
      TYPE_COARSENING = NSCARC_COARSENING_AGGREGATED
   CASE ('AGGREGATEDS')
      TYPE_COARSENING = NSCARC_COARSENING_AGGREGATED_S
   CASE ('CUBIC')
      TYPE_COARSENING = NSCARC_COARSENING_CUBIC
   CASE ('GMG')
      TYPE_COARSENING = NSCARC_COARSENING_GMG
   CASE DEFAULT
      !CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_COARSENING, NSCARC_NONE)
      TYPE_COARSENING = NSCARC_COARSENING_AGGREGATED
END SELECT

! Set type of coarse grid solver
SELECT CASE (TRIM(SCARC_COARSE))
   CASE ('ITERATIVE')
      TYPE_COARSE = NSCARC_COARSE_ITERATIVE
   CASE ('DIRECT')
#ifdef WITH_MKL
      TYPE_COARSE   = NSCARC_COARSE_DIRECT
      TYPE_MKL(0)   = NSCARC_MKL_COARSE
#else
      CALL SCARC_ERROR(NSCARC_ERROR_MKL_CLUSTER, SCARC_NONE, NSCARC_NONE)
#endif
   CASE DEFAULT
      CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_COARSE, NSCARC_NONE)
END SELECT

! Set type of accuracy (ABSOLUTE/RELATIVE)
 
SELECT CASE (TRIM(SCARC_ACCURACY))
   CASE ('ABSOLUTE')
      TYPE_ACCURACY = NSCARC_ACCURACY_ABSOLUTE
   CASE ('RELATIVE')
      TYPE_ACCURACY = NSCARC_ACCURACY_RELATIVE
   CASE DEFAULT
      CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_ACCURACY, NSCARC_NONE)
END SELECT

! Set type of precision for MKL solver (SINGLE/DOUBLE)
 
SELECT CASE (TRIM(SCARC_MKL_PRECISION))
   CASE ('SINGLE')
      TYPE_MKL_PRECISION = NSCARC_PRECISION_SINGLE
   CASE ('DOUBLE')
      TYPE_MKL_PRECISION = NSCARC_PRECISION_DOUBLE
   CASE DEFAULT
      CALL SCARC_ERROR(NSCARC_ERROR_PARSE_INPUT, SCARC_MKL_PRECISION, NSCARC_NONE)
END SELECT

! -------- Define some logical variables - just for notational convenience
 
IS_STRUCTURED   = (TYPE_GRID == NSCARC_GRID_STRUCTURED)
IS_UNSTRUCTURED = (TYPE_GRID == NSCARC_GRID_UNSTRUCTURED)

IS_CG     = (TYPE_METHOD == NSCARC_METHOD_KRYLOV)
IS_CG_MG  = IS_CG .AND. (TYPE_PRECON == NSCARC_RELAX_GMG) 
IS_CG_GMG = IS_CG .AND. (TYPE_PRECON == NSCARC_RELAX_GMG) .AND. (TYPE_MULTIGRID == NSCARC_MULTIGRID_GEOMETRIC)
IS_CG_AMG = IS_CG .AND. (TYPE_PRECON == NSCARC_RELAX_GMG) .AND. (TYPE_MULTIGRID == NSCARC_MULTIGRID_ALGEBRAIC)

IS_MG  = (TYPE_METHOD == NSCARC_METHOD_MULTIGRID)
IS_GMG = IS_MG .AND. (TYPE_MULTIGRID == NSCARC_MULTIGRID_GEOMETRIC)
IS_AMG = IS_MG .AND. (TYPE_MULTIGRID == NSCARC_MULTIGRID_ALGEBRAIC)

IS_FFT = (TYPE_PRECON == NSCARC_RELAX_FFT)  .OR. (TYPE_SMOOTH == NSCARC_RELAX_FFT)
IS_FFTO= (TYPE_PRECON == NSCARC_RELAX_FFTO) .OR. (TYPE_SMOOTH == NSCARC_RELAX_FFTO)
IS_MKL = (TYPE_PRECON >= NSCARC_RELAX_MKL)  .OR. (TYPE_SMOOTH >= NSCARC_RELAX_MKL) 

HAS_TWO_LEVELS      = IS_CG .AND. (TYPE_PRECON /= NSCARC_RELAX_GMG) .AND. (TYPE_TWOLEVEL > NSCARC_TWOLEVEL_NONE)
HAS_MULTIPLE_LEVELS = IS_MG .OR. IS_CG_MG .OR. HAS_TWO_LEVELS 

IS_CG_ADD    = HAS_TWO_LEVELS .AND. (TYPE_TWOLEVEL == NSCARC_TWOLEVEL_ADD)
IS_CG_MUL    = HAS_TWO_LEVELS .AND. (TYPE_TWOLEVEL == NSCARC_TWOLEVEL_MUL)
IS_CG_MACRO  = HAS_TWO_LEVELS .AND. (TYPE_TWOLEVEL == NSCARC_TWOLEVEL_MACRO)
IS_CG_COARSE = HAS_TWO_LEVELS .AND. (TYPE_TWOLEVEL == NSCARC_TWOLEVEL_COARSE)

HAS_GMG_LEVELS = IS_GMG .OR. IS_CG_GMG .OR. HAS_TWO_LEVELS
HAS_AMG_LEVELS = IS_AMG .OR. IS_CG_AMG 

IS_MGM = TYPE_METHOD == NSCARC_METHOD_MGM

END SUBROUTINE SCARC_PARSE_INPUT

END MODULE SCARC_PARSER

