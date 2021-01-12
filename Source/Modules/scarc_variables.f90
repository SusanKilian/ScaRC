!//////////////////////////////////////////////////////////////////////////////////////////////////////
!
! MODULE SCARC_VARIABLES
!
!> \brief Define the variables used in the different routines of ScaRC/UScaRC
!
!//////////////////////////////////////////////////////////////////////////////////////////////////////
MODULE SCARC_VARIABLES

USE PRECISION_PARAMETERS
USE SCARC_CONSTANTS
USE SCARC_TYPES

IMPLICIT NONE

! ---------- Basic definitions
 
CHARACTER(40) :: SCARC_GRID              = 'STRUCTURED'         !< Type of discretization (STRUCTURED/UNSTRUCTURED)
CHARACTER(40) :: SCARC_METHOD            = 'NONE'               !< Type of global ScaRC solver (Krylov/MULTIGRID)
CHARACTER(40) :: SCARC_MATRIX            = 'NONE'               !< Type of matrix storage (COMPACT/BANDWISE)
CHARACTER(40) :: SCARC_STENCIL           = 'VARIABLE'           !< Type of matrix stencil (CONSTANT/VARIABLE)
CHARACTER(40) :: SCARC_TWOLEVEL          = 'NONE'               !< Type of two-level method (NONE/ADDITIVE/MULTIPLICATIVE)

! ---------- General iteration parameters
 
CHARACTER(40) :: SCARC_ACCURACY          = 'ABSOLUTE'           !< Type of accuracy type (ABSOLUTE/RELATIVE)
REAL (EB)     :: SCARC_CAPPA             =  0.0_EB              !< Convergence rate of selected ScarC solver
INTEGER       :: SCARC_ITERATIONS        =  0                   !< Number of iterations of selected ScaRC solver
REAL (EB)     :: SCARC_RESIDUAL          =  0.0_EB              !< Residual of globally selected ScaRC solver

! ---------- Parameters for coarse grid method
 
CHARACTER(40) :: SCARC_COARSE            = 'DIRECT'             !< Type of coarse grid solver (ITERATIVE/DIRECT)
REAL (EB)     :: SCARC_COARSE_ACCURACY   = 1.E-14_EB            !< Requested accuracy for iterative solver
INTEGER       :: SCARC_COARSE_ITERATIONS = 100                  !< Max number of iterations for iterative solver
INTEGER       :: SCARC_COARSE_LEVEL      =  1                   !< Coarse grid level for twolevel-Krylov method
REAL (EB)     :: SCARC_COARSE_OMEGA      = 0.80E+0_EB           !< Relaxation parameter

CHARACTER(40) :: SCARC_COARSENING = 'GMG'                       !< Coarsening strategy (CUBIC/AGGREGATED/GMG)

! ---------- Parameters for Krylov type methods
 
REAL (EB)     :: SCARC_KRYLOV_ACCURACY   = 1.E-8_EB             !< Requested accuracy for convergence
CHARACTER(40) :: SCARC_KRYLOV_INTERPOL   = 'CONSTANT'           !< Twolevel-interpolation (CONSTANT/BILINEAR)
INTEGER       :: SCARC_KRYLOV_ITERATIONS = 1000                 !< Max number of iterations

! ---------- Parameters for multigrid-type methods
 
CHARACTER(40) :: SCARC_MULTIGRID            = 'GEOMETRIC'       !< Type of MG method (GEOMETRIC/ALGEBRAIC)
REAL (EB)     :: SCARC_MULTIGRID_ACCURACY   = 1.E-8_EB          !< Requested accuracy for convergence
CHARACTER(3)  :: SCARC_MULTIGRID_CYCLE      = 'V'               !< Cycling type  (F/V/W/FULL)
CHARACTER(40) :: SCARC_MULTIGRID_INTERPOL   = 'CONSTANT'        !< Interpolation strategy (CONSTANT/BILINEAR)
INTEGER       :: SCARC_MULTIGRID_ITERATIONS = 100               !< Max number of iterations
INTEGER       :: SCARC_MULTIGRID_LEVEL      = -1                !< User defined number of MG-levels (optionally, maximum else)
INTEGER       :: SCARC_MULTIGRID_PRESMOOTH  = 4                 !< Number of presmoothing iterations
INTEGER       :: SCARC_MULTIGRID_POSTSMOOTH = 4                 !< Number of postsmoothing iterations
LOGICAL       :: SCARC_MULTIGRID_RELAXING   = .TRUE.            !< Relaxing of nullspace (AMG only)
REAL (EB)     :: SCARC_MULTIGRID_THETA      = 0.10E+0_EB        !< Threshold for strength of connection matrix (AMG only)

! ---------- Parameters for MGM method

REAL(EB)      :: SCARC_MGM_ACCURACY        = 1.E-2_EB           !< Requested accuracy for interface velocity error 
CHARACTER(40) :: SCARC_MGM_BC              = 'MEAN'             !< Type of interface boundary condition for local Laplace problems
CHARACTER(40) :: SCARC_MGM_INTERPOL        = 'LINEAR'           !< Type of interpolation for Lapalce BC settings
INTEGER       :: SCARC_MGM_ITERATIONS      = 50                 !< Maximum allowed number of Laplace iterations 
LOGICAL       :: SCARC_MGM_CHECK_LAPLACE   = .FALSE.            !< Requested check of Laplace solutions against ScaRC-UScaRC diff
LOGICAL       :: SCARC_MGM_INIT_EXACT      = .TRUE.             !< Use exact Laplace solution for initialization of interface BCs
LOGICAL       :: SCARC_MGM_USE_LU          = .TRUE.             !< Use permuted LU for Laplace solutions (otherwise UScaRC)

! ---------- Parameters for smoothing method (used in multigrids-methods)
 
CHARACTER(40) :: SCARC_SMOOTH            = 'SSOR'               !< Smoother for MG (JACOBI/SSOR)
REAL (EB)     :: SCARC_SMOOTH_ACCURACY   = 1.E-8_EB             !< Requested accuracy for convergence
INTEGER       :: SCARC_SMOOTH_ITERATIONS = 4                    !< Max number of iterations
REAL (EB)     :: SCARC_SMOOTH_OMEGA      = 0.80E+0_EB           !< Relaxation parameter
CHARACTER(40) :: SCARC_SMOOTH_SCOPE      = 'GLOBAL'             !< Scope of action (LOCAL/GLOBAL)

! ---------- Parameters for preconditioning method (used in Krylov methods)
 
CHARACTER(40) :: SCARC_PRECON            = 'NONE'               !< Preconditioner for CG (JACOBI/SSOR/FFT/PARDISO/MG)
REAL (EB)     :: SCARC_PRECON_ACCURACY   = 1.E-10_EB            !< Requested accuracy for convergence
INTEGER       :: SCARC_PRECON_ITERATIONS = 100                  !< Max number of iterations
REAL (EB)     :: SCARC_PRECON_OMEGA      = 1.50E+0_EB           !< Relaxation parameter
CHARACTER(40) :: SCARC_PRECON_SCOPE      = 'LOCAL'              !< Scope of action (LOCAL/GLOBAL)

! ---------- Parameter for MKL solver
 
CHARACTER(40) :: SCARC_MKL_SCOPE      = 'GLOBAL'                !< Scope of MKL solver (LOCAL/GLOBAL)
CHARACTER(40) :: SCARC_MKL_MTYPE      = 'SYMMETRIC'             !< Type of MKL matrix (SYMMETRIC/UNSYMMETRIC)
CHARACTER(6)  :: SCARC_MKL_PRECISION  = 'DOUBLE'                !< Single/double precision for MKL solver

! ---------- Dump out of error information and error handling
 
LOGICAL :: SCARC_ERROR_FILE = .FALSE.                           !< Print ScaRC statistics into chid_scarc.csv (TRUE/FALSE)
INTEGER :: IERROR = 0                                           !< General error flag - used at different positions

#ifdef WITH_SCARC_POSTPROCESSING
LOGICAL :: SCARC_DUMP = .TRUE.                                  !< Dump out several arrays for POSTPROCESSING use of ScaRC
#endif

! ---------- Logical indicators for different methods and mechanisms
  
LOGICAL :: IS_STRUCTURED        = .FALSE.                       !< Flag for structured discretization
LOGICAL :: IS_UNSTRUCTURED      = .FALSE.                       !< Flag for unstructured discretization
LOGICAL :: IS_PURE_NEUMANN      = .FALSE.                       !< Flag for pure Neumann system
LOGICAL :: IS_MG                = .FALSE.                       !< Flag for Multigrid-method
LOGICAL :: IS_AMG               = .FALSE.                       !< Flag for Algebraic Multigrid-method
LOGICAL :: IS_GMG               = .FALSE.                       !< Flag for Geometric Multigrid-method
LOGICAL :: IS_CG                = .FALSE.                       !< Flag for Krylov method
LOGICAL :: IS_CG_ADD            = .FALSE.                       !< Flag for additive twolevel-Krylov method
LOGICAL :: IS_CG_AMG            = .FALSE.                       !< Flag for Krylov method with AMG-preconditioning
LOGICAL :: IS_CG_GMG            = .FALSE.                       !< Flag for Krylov method with GMG-preconditioning
LOGICAL :: IS_CG_COARSE         = .FALSE.                       !< Flag for only coarse grid solver
LOGICAL :: IS_CG_MACRO          = .FALSE.                       !< Flag for macro coarse grid solver
LOGICAL :: IS_CG_MG             = .FALSE.                       !< Flag for Krylov method with MG-preconditioning
LOGICAL :: IS_CG_MUL            = .FALSE.                       !< Flag for multiplicative Twolevel-Krylov method
LOGICAL :: IS_CG_MUL2           = .FALSE.                       !< Flag for multiplicative-type2 Twolevel-Krylov method
LOGICAL :: IS_FFT               = .FALSE.                       !< Flag for FFT-method
LOGICAL :: IS_FFTO              = .FALSE.                       !< Flag for FFTO-method
LOGICAL :: IS_LAPLACE           = .FALSE.                       !< Flag for use of Laplace matrix (MGM only)
LOGICAL :: IS_POISSON           = .TRUE.                        !< Flag for use of Poisson matrix (MGM only)
LOGICAL :: IS_MKL               = .FALSE.                       !< Flag for MKL-method
LOGICAL :: IS_MKL_LEVEL(10)     = .FALSE.                       !< Flag for level-dependent MKL method
LOGICAL :: IS_MGM               = .FALSE.                       !< Flag for McKeeney-Greengard-Mayo method

LOGICAL :: HAS_CSV_DUMP         = .FALSE.                       !< Flag for CSV-file to be dumped out
LOGICAL :: HAS_MULTIPLE_GRIDS   = .FALSE.                       !< Flag for multiple discretization types
LOGICAL :: HAS_TWO_LEVELS       = .FALSE.                       !< Flag for two grid levels
LOGICAL :: HAS_MULTIPLE_LEVELS  = .FALSE.                       !< Flag for multiple grid levels
LOGICAL :: HAS_AMG_LEVELS       = .FALSE.                       !< Flag for AMG-based grid levels
LOGICAL :: HAS_GMG_LEVELS       = .FALSE.                       !< Flag for GMG-based grid levels

 
! ---------- Globally used types for description of different solvers
  
INTEGER :: TYPE_ACCURACY           = NSCARC_ACCURACY_ABSOLUTE    !< Type of requested accuracy
INTEGER :: TYPE_COARSE             = NSCARC_COARSE_DIRECT        !< Type of coarse grid solver 
INTEGER :: TYPE_COARSENING         = NSCARC_COARSENING_CUBIC     !< Type of grid coarsening 
INTEGER :: TYPE_CYCLING            = NSCARC_CYCLING_V            !< Type of cycling for multigrid method
INTEGER :: TYPE_GRID               = NSCARC_GRID_STRUCTURED      !< Type of discretization 
INTEGER :: TYPE_EXCHANGE           = NSCARC_UNDEF_INT            !< Type of data exchange
INTEGER :: TYPE_EXCHANGE_MATRIX    = NSCARC_MATRIX_POISSON       !< Type of matrix for exchange
INTEGER :: TYPE_INTERPOL           = NSCARC_INTERPOL_CONSTANT    !< Type of interpolation method
INTEGER :: TYPE_LEVEL(0:2)         = NSCARC_UNDEF_INT            !< Type of levels
INTEGER :: TYPE_MATRIX             = NSCARC_MATRIX_COMPACT       !< Type of storage for matrix
INTEGER :: TYPE_MATVEC             = NSCARC_MATVEC_GLOBAL        !< Type of matrix-vector multiplication
INTEGER :: TYPE_METHOD             = NSCARC_METHOD_KRYLOV        !< Type of ScaRC method
INTEGER :: TYPE_MGM_BC             = NSCARC_MGM_BC_MEAN          !< Type of internal MGM boundary conditions
INTEGER :: TYPE_MGM_INTERPOL       = NSCARC_MGM_INTERPOL_LINEAR  !< Type of internal MGM boundary conditions
INTEGER :: TYPE_MKL(0:10)          = NSCARC_MKL_NONE             !< Type of use of MKL solvers
INTEGER :: TYPE_MKL_PRECISION      = NSCARC_PRECISION_DOUBLE     !< Type of double precision MKL solver
INTEGER :: TYPE_MULTIGRID          = NSCARC_MULTIGRID_GEOMETRIC  !< Type of multigrid method 
INTEGER :: TYPE_PARENT             = NSCARC_UNDEF_INT            !< Type of parent (calling) solver
INTEGER :: TYPE_PRECON             = NSCARC_UNDEF_INT            !< Type of preconditioner for iterative solver
INTEGER :: TYPE_RELAX              = NSCARC_UNDEF_INT            !< Type of preconditioner for iterative solver
INTEGER :: TYPE_SCOPE(0:2)         = NSCARC_SCOPE_GLOBAL         !< Type of method scopes
INTEGER :: TYPE_SMOOTH             = NSCARC_UNDEF_INT            !< Type of smoother for multigrid method
INTEGER :: TYPE_SOLVER             = NSCARC_SOLVER_MAIN          !< Type of surrounding solver stage
INTEGER :: TYPE_STAGE              = NSCARC_STAGE_ONE            !< Type of surrounding solver stage
INTEGER :: TYPE_STENCIL            = NSCARC_STENCIL_VARIABLE     !< Type of storage for matrix
INTEGER :: TYPE_TWOLEVEL           = NSCARC_TWOLEVEL_NONE        !< Type of twolevel method
INTEGER :: TYPE_VECTOR             = NSCARC_UNDEF_INT            !< Type of vector to point to

! ---------- Globally used parameters
 
INTEGER :: NLEVEL_MIN, NLEVEL_MAX                           !< Minimum and maximum number of multigrid levels
INTEGER :: NC_GLOBAL(20) = 0                                !< Number of global cells
INTEGER :: N_DIRIC_GLOBAL(20) = 0                           !< Global number of Dirichlet BCs
INTEGER :: N_STACK_TOTAL = 0                                !< Maximum number of used solvers in stack

INTEGER :: N_REQ, N_EXCHANGES, TAG                          !< Information for data exchange
INTEGER :: SNODE, RNODE                                     !< Process Indicator for data exchange

INTEGER,  ALLOCATABLE, DIMENSION (:)  :: REQ                !< Request array for data exchange
INTEGER,  ALLOCATABLE, DIMENSION (:)  :: COUNTS             !< Counter array for data exchange
INTEGER,  ALLOCATABLE, DIMENSION (:)  :: DISPLS             !< Displacement array for data exchange
INTEGER,  ALLOCATABLE, DIMENSION (:)  :: MESH_INT           !< Local integer data array for data exchange
REAL(EB), ALLOCATABLE, DIMENSION (:)  :: MESH_REAL          !< Local real data array for data exchange

INTEGER  :: GLOBAL_INT,  RANK_INT
REAL(EB) :: GLOBAL_REAL, RANK_REAL

INTEGER :: FACE_ORIENTATION(6) = (/1,-1,2,-2,3,-3/)         !< Coordinate direction related order of mesh faces

CHARACTER(60) :: CNAME, CROUTINE

! 
! ---------- Public variables
! 
PUBLIC :: SCARC_ACCURACY                   !< Requested accuracy for ScaRC solver
PUBLIC :: SCARC_CAPPA                      !< Resulting convergence rate of ScaRC solver
PUBLIC :: SCARC_COARSENING                 !< Selection parameter for AMG coarsening strategy (aggregation/cubic)
PUBLIC :: SCARC_ERROR_FILE                 !< Flag to print additional convergence information about current ScaRC call
PUBLIC :: SCARC_GRID                       !< Selection parameter for requested grid variant (structured/unstructured)
PUBLIC :: SCARC_ITERATIONS                 !< Final number of needed iterations for ScaRC solver
PUBLIC :: SCARC_MATRIX                     !< Selection parameter for requested matrix storage technique (compact/bandwise)
PUBLIC :: SCARC_METHOD                     !< Selection parameter for requested ScaRC variant (Krylov/Multigrid/LU)
PUBLIC :: SCARC_MKL_PRECISION              !< Selection parameter for requested MKL precision (double/single)
PUBLIC :: SCARC_RESIDUAL                   !< Final residual after call of ScaRC solver
PUBLIC :: SCARC_TWOLEVEL                   !< Selection parameter for possible twolevel variant (additive/multiplicative)

PUBLIC :: SCARC_COARSE                     !< Selection parameter for type of coarse grid solver (iterative/direct)
PUBLIC :: SCARC_COARSE_ACCURACY            !< Requested accuracy for coarse grid solver
PUBLIC :: SCARC_COARSE_ITERATIONS          !< Maximum number of allowed coarse grid iterations (direct variant only)
PUBLIC :: SCARC_COARSE_OMEGA               !< Relaxation parameter for coarse grid solver (direct variant only)
PUBLIC :: SCARC_COARSE_LEVEL               !< Grid refinement level for coarse grid solver

PUBLIC :: SCARC_KRYLOV_ACCURACY            !< Requested accuracy for Krylov solver
PUBLIC :: SCARC_KRYLOV_ITERATIONS          !< Maximum number of allowed Krylov iterations
PUBLIC :: SCARC_KRYLOV_INTERPOL            !< Selection parameter for interpolation type in case of a twolevel Krylov variant

PUBLIC :: SCARC_MULTIGRID                  !< Selection parameter for multigrid method (geometric/algebraic)
PUBLIC :: SCARC_MULTIGRID_ACCURACY         !< Requested accuracy for multigrid method
PUBLIC :: SCARC_MULTIGRID_ITERATIONS       !< Maximum number of allowed iterations for multigrid method
PUBLIC :: SCARC_MULTIGRID_INTERPOL         !< Selection parameter for interpolation type between multigrid levels
PUBLIC :: SCARC_MULTIGRID_CYCLE            !< Selection parameter of multigrid cycling type (V/W/F)
PUBLIC :: SCARC_MULTIGRID_LEVEL            !< Coarse grid level for multigrid method
PUBLIC :: SCARC_MULTIGRID_PRESMOOTH        !< Number of presmoothing iterations in multigrid method
PUBLIC :: SCARC_MULTIGRID_POSTSMOOTH       !< Number of postesmoothing iterations in multigrid method
PUBLIC :: SCARC_MULTIGRID_RELAXING         !< Relaxing of nullspace
PUBLIC :: SCARC_MULTIGRID_THETA            !< Optional relaxation parameter for multigrid

PUBLIC :: SCARC_MGM_BC                     !< Interface boundary conditions for Laplace problems of MGM method
PUBLIC :: SCARC_MGM_ACCURACY               !< Requested accuracy for velocity error of MGM method
PUBLIC :: SCARC_MGM_INTERPOL               !< Interpolation type for BC definition
PUBLIC :: SCARC_MGM_ITERATIONS             !< Maximum number of allowed Laplace iterations for MGM method
PUBLIC :: SCARC_MGM_CHECK_LAPLACE          !< Requested check of Laplace solutions against ScaRC-UScaRC difference 
PUBLIC :: SCARC_MGM_INIT_EXACT             !< Use exact Laplace solution for initialization of interface BC's
PUBLIC :: SCARC_MGM_USE_LU                 !< Use permuted LU for solution of Laplace problems (instead of UScaRC)

PUBLIC :: SCARC_PRECON                     !< Selection parameter for preconditioner
PUBLIC :: SCARC_PRECON_ACCURACY            !< Requested accuracy for preconditioner 
PUBLIC :: SCARC_PRECON_ITERATIONS          !< Maximum number of allowed iterations for preconditioner
PUBLIC :: SCARC_PRECON_OMEGA               !< Relaxation parameter for preconditioner
PUBLIC :: SCARC_PRECON_SCOPE               !< Scope of activity for preconditioner (global/local)

PUBLIC :: SCARC_SMOOTH                     !< Selection parameter for smoother
PUBLIC :: SCARC_SMOOTH_ACCURACY            !< Requested accuracy for smoother
PUBLIC :: SCARC_SMOOTH_ITERATIONS          !< Maximum number of allowed iterations for smoother
PUBLIC :: SCARC_SMOOTH_OMEGA               !< Relaxation parameter for smoother
PUBLIC :: SCARC_SMOOTH_SCOPE               !< Scope of activity for smoother (global/local)

! 
! ---------- Type declarations
! 
TYPE (SCARC_TYPE)       , SAVE, DIMENSION(:), ALLOCATABLE, TARGET :: SCARC       !< Main ScaRC data structure
TYPE (SCARC_STACK_TYPE) , SAVE, DIMENSION(:), ALLOCATABLE, TARGET :: STACK       !< Stack of consecutive solvers

TYPE (SCARC_STORAGE_TYPE), SAVE, TARGET :: STORAGE                 !< Storage administration for ScaRC arrays

TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: MAIN_CG                  !< Solver structure for Krylov main solver 
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: MAIN_CG_STRUCTURED       !< Solver structure for structured Krylov main solver 
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: MAIN_CG_UNSTRUCTURED     !< Solver structure for unstructured Krylov main solver 
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: MAIN_GMG                 !< Solver structure for Multigrid main solver 

TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: COARSE_KRYLOV            !< Solver structure for Krylov coarse grid solver 

TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: PRECON_JAC          !< Solver structure for Jacobi preconditioner 
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: PRECON_SSOR         !< Solver structure for SSOR preconditioner 
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: PRECON_LU           !< Solver structure for ILU preconditioner 
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: PRECON_ILU          !< Solver structure for ILU preconditioner 
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: PRECON_FFT          !< Solver structure for FFT preconditioner 
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: PRECON_FFTO         !< Solver structure for FFTO preconditioner (including overlap)
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: PRECON_MG           !< Solver structure for Multigrid preconditioner 
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: PRECON_MJAC         !< Solver structure for Jacobi preconditioner (matrix version)
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: PRECON_MGS          !< Solver structure for Gauss-Seidel preconditioner (matrix version)
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: PRECON_MSGS         !< Solver structure for Sym. Gauss-Seidel preconditioner (matrix vs.)
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: PRECON_MSOR         !< Solver structure for SOR preconditioner (matrix version)
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: PRECON_MSSOR        !< Solver structure for SSOR preconditioner (matrix version)

TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: SMOOTH_JAC          !< Solver structure for Jacobi smoother 
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: SMOOTH_SSOR         !< Solver structure for SSOR smoother 
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: SMOOTH_ILU          !< Solver structure for ILU smoother 
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: SMOOTH_FFT          !< Solver structure for FFT smoother 
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: SMOOTH_FFTO         !< Solver structure for FFTO smoother (including overlap)
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: SMOOTH_MJAC         !< Solver structure for Jacobi smoother (matrix version)
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: SMOOTH_MGS          !< Solver structure for Gauss-Seidel smoother (matrix version)
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: SMOOTH_MSGS         !< Solver structure for Sym. Gauss-Seidel smoother (matrix vs.)
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: SMOOTH_MSOR         !< Solver structure for SOR smoother (matrix version)
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: SMOOTH_MSSOR        !< Solver structure for SSOR smoother (matrix version)
TYPE (SCARC_MESSAGE_TYPE), SAVE, TARGET :: MSG
TYPE (SCARC_CPU_TYPE), SAVE, DIMENSION(:), ALLOCATABLE, TARGET :: CPU

!#ifdef WITH_SCARC_MKL
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: MAIN_LU             !< Solver structure for LU-decomposition main solver 
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: COARSE_CLUSTER      !< Solver structure for CLUSTER_SPARSE_SOLVER coarse grid solver 
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: COARSE_PARDISO      !< Solver structure for PARDISO coarse grid solver
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: PRECON_MKL          !< Solver structure for MKL preconditioner 
TYPE (SCARC_SOLVER_TYPE), SAVE, TARGET :: SMOOTH_MKL          !< Solver structure for MKL smoother 
!#endif

TYPE (SCARC_SUBDIVISION_TYPE), SAVE, TARGET :: SUBDIVISION    !< Structure to keep information about subdivision

END MODULE SCARC_VARIABLES


