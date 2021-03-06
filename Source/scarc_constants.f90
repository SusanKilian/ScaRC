!=======================================================================================================================
!
! MODULE SCARC_CONSTANTS
!
!> \brief Define all constants needed in for the different solution strategies in ScaRC/UScaRC
!
!=======================================================================================================================
MODULE SCARC_CONSTANTS

USE PRECISION_PARAMETERS

IMPLICIT NONE

INTEGER, PARAMETER :: NSCARC_ACCURACY_ABSOLUTE       =  1        !< Type of requested accuracy of method: absolute
INTEGER, PARAMETER :: NSCARC_ACCURACY_RELATIVE       =  2        !< Type of requested accuracy of method: relative
                   
INTEGER, PARAMETER :: NSCARC_BUFFER_BASIC            =  1        !< Length of exchange buffer: basic initialization
INTEGER, PARAMETER :: NSCARC_BUFFER_FULL             =  2        !< Length of exchange buffer: full length
INTEGER, PARAMETER :: NSCARC_BUFFER_LAYER1           =  3        !< Length of exchange buffer: one ghost cell layer
INTEGER, PARAMETER :: NSCARC_BUFFER_LAYER2           =  4        !< Length of exchange buffer: two ghost cell layers
INTEGER, PARAMETER :: NSCARC_BUFFER_LAYER4           =  5        !< Length of exchange buffer: four ghost cell layers
INTEGER, PARAMETER :: NSCARC_BUFFER_STENCIL          =  6        !< Length of exchange buffer: stencil size

INTEGER, PARAMETER :: NSCARC_COARSE_ITERATIVE        =  1        !< Type of coarse grid solver: iterative solver
INTEGER, PARAMETER :: NSCARC_COARSE_DIRECT           =  2        !< Type of coarse grid solver: direct solver
INTEGER, PARAMETER :: NSCARC_COARSE_LEVEL            =  3        !< Type of coarse grid solver: only on specified level
INTEGER, PARAMETER :: NSCARC_COARSE_MACRO            =  4        !< Type of coarse grid solver: on a separate macro

INTEGER, PARAMETER :: NSCARC_COARSENING_AGGREGATED   =  1        !< Type of grid coarsening: aggregation-based 
INTEGER, PARAMETER :: NSCARC_COARSENING_AGGREGATED_S =  2        !< Type of grid coarsening: aggregation-based, staggered 
INTEGER, PARAMETER :: NSCARC_COARSENING_CUBIC        =  3        !< Type of grid coarsening: cubic zones 
INTEGER, PARAMETER :: NSCARC_COARSENING_GMG          =  4        !< Type of grid coarsening: GMG-like 

INTEGER, PARAMETER :: NSCARC_CYCLING_F               =  0        !< Type of MG-cycling: F-cycle 
INTEGER, PARAMETER :: NSCARC_CYCLING_V               =  1        !< Type of MG-cycling: V-cycle 
INTEGER, PARAMETER :: NSCARC_CYCLING_W               =  2        !< Type of MG-cycling: W-cycle 
INTEGER, PARAMETER :: NSCARC_CYCLING_FMG             =  3        !< Type of MG-cycling: Full multigrid grid cycle 
INTEGER, PARAMETER :: NSCARC_CYCLING_SETUP           =  4        !< State of MG-cycling: initialize cycle counts
INTEGER, PARAMETER :: NSCARC_CYCLING_RESET           =  5        !< State of MG-cycling: reset cycle counts
INTEGER, PARAMETER :: NSCARC_CYCLING_PROCEED         =  6        !< State of MG-cycling: proceed cycle counts
INTEGER, PARAMETER :: NSCARC_CYCLING_NEXT            =  7        !< State of MG-cycling: perform next cycling loop
INTEGER, PARAMETER :: NSCARC_CYCLING_EXIT            =  8        !< State of MG-cycling: exit cycling loop
INTEGER, PARAMETER :: NSCARC_CYCLING_PRESMOOTH       = -1        !< State of MG-cycling: presmoothing cycle
INTEGER, PARAMETER :: NSCARC_CYCLING_POSTSMOOTH      =  1        !< State of MG-cycling: postsmoothing cycle

INTEGER, PARAMETER :: NSCARC_DATA_BMATRIX            =  1        !< Type of allocated structure: bandwise stored matrix
INTEGER, PARAMETER :: NSCARC_DATA_CMATRIX            =  2        !< Type of allocated structure: compactly stored matrix
INTEGER, PARAMETER :: NSCARC_DATA_INTEGER            =  3        !< Type of allocated structure: integer array
INTEGER, PARAMETER :: NSCARC_DATA_LOGICAL            =  4        !< Type of allocated structure: integer array
INTEGER, PARAMETER :: NSCARC_DATA_REAL_EB            =  5        !< Type of allocated structure: double precision array
INTEGER, PARAMETER :: NSCARC_DATA_REAL_FB            =  6        !< Type of allocated structure: single precision array

#ifdef WITH_SCARC_DEBUG
INTEGER, PARAMETER :: NSCARC_DEBUG_FACE              =  1        !< Type of debugging message: show face information
INTEGER, PARAMETER :: NSCARC_DEBUG_GRID              =  2        !< Type of debugging message: show grid information
INTEGER, PARAMETER :: NSCARC_DEBUG_CMATRIX           =  3        !< Type of debugging message: show specified matrix
INTEGER, PARAMETER :: NSCARC_DEBUG_PRESSURE          =  4        !< Type of debugging message: show pressure quantities
INTEGER, PARAMETER :: NSCARC_DEBUG_STACK             =  5        !< Type of debugging message: show matrix
INTEGER, PARAMETER :: NSCARC_DEBUG_WALL              =  6        !< Type of debugging message: show wall information
#endif
INTEGER, PARAMETER :: NSCARC_ERROR_BOUNDARY_SUM      =  3        !< Type of error message: wrong sum of boundary elements
INTEGER, PARAMETER :: NSCARC_ERROR_BOUNDARY_TYPE     =  4        !< Type of error message: wrong boundary type
INTEGER, PARAMETER :: NSCARC_ERROR_DIRECT_NOMKL      =  5        !< Type of error message: MKL for direct solver missing
INTEGER, PARAMETER :: NSCARC_ERROR_EXCHANGE_RECV     =  6        !< Type of error message: wrong receive exchange structure
INTEGER, PARAMETER :: NSCARC_ERROR_EXCHANGE_DIAG     =  7        !< Type of error message: wrong receive exchange structure
INTEGER, PARAMETER :: NSCARC_ERROR_EXCHANGE_SEND     =  8        !< Type of error message: wrong send exchange structure
INTEGER, PARAMETER :: NSCARC_ERROR_FFT_GRID          =  9        !< Type of error message: no unstructured FFT possible
INTEGER, PARAMETER :: NSCARC_ERROR_GRID_INDEX        = 10        !< Type of error message: error with grid index
INTEGER, PARAMETER :: NSCARC_ERROR_GRID_NUMBER       = 11        !< Type of error message: uneven cell number
INTEGER, PARAMETER :: NSCARC_ERROR_GRID_NUMBERX      = 12        !< Type of error message: uneven cell number in x 
INTEGER, PARAMETER :: NSCARC_ERROR_GRID_NUMBERY      = 13        !< Type of error message: uneven cell number in y 
INTEGER, PARAMETER :: NSCARC_ERROR_GRID_NUMBERZ      = 14        !< Type of error message: uneven cell number in z
INTEGER, PARAMETER :: NSCARC_ERROR_GRID_RESOLUTION   = 15        !< Type of error message: error with grid resolution
INTEGER, PARAMETER :: NSCARC_ERROR_NEIGHBOR_NUMBER   = 16        !< Type of error message: wrong neighbor number
INTEGER, PARAMETER :: NSCARC_ERROR_NEIGHBOR_TYPE     = 17        !< Type of error message: wrong neighbor type
INTEGER, PARAMETER :: NSCARC_ERROR_MATRIX_ALLOCATION = 18        !< Type of error message: error in matrix allocation
INTEGER, PARAMETER :: NSCARC_ERROR_MATRIX_COPY       = 19        !< Type of error message: subdiagonal missing
INTEGER, PARAMETER :: NSCARC_ERROR_MATRIX_SETUP      = 20        !< Type of error message: error in matrix setup
INTEGER, PARAMETER :: NSCARC_ERROR_MATRIX_SIZE       = 21        !< Type of error message: error in matrix size
INTEGER, PARAMETER :: NSCARC_ERROR_MATRIX_SUBDIAG    = 22        !< Type of error message: subdiagonal missing
INTEGER, PARAMETER :: NSCARC_ERROR_MATRIX_SYMMETRY   = 23        !< Type of error message: matrix not symmetric
INTEGER, PARAMETER :: NSCARC_ERROR_MGM_PARDISO       = 24        !< Type of error message: no PARDISO solver for MGM-Laplace 
INTEGER, PARAMETER :: NSCARC_ERROR_MGM_PERMUTATION   = 25        !< Type of error message: error in MGM LU permutation
INTEGER, PARAMETER :: NSCARC_ERROR_MKL_CLUSTER       = 26        !< Type of error message: CLUSTER_SPARSE_SOLVER missing
INTEGER, PARAMETER :: NSCARC_ERROR_MKL_INTERNAL      = 27        !< Type of error message: internal error in MKL routine
INTEGER, PARAMETER :: NSCARC_ERROR_MKL_PARDISO       = 28        !< Type of error message: PARDISO solver missing
INTEGER, PARAMETER :: NSCARC_ERROR_MKL_STACK         = 29        !< Type of error message: MKL method not available on this stack
INTEGER, PARAMETER :: NSCARC_ERROR_MKL_STORAGE       = 30        !< Type of error message: wrong storage scheme in MKL
INTEGER, PARAMETER :: NSCARC_ERROR_MULTIGRID_LEVEL   = 31        !< Type of error message: wrong multigrid level
INTEGER, PARAMETER :: NSCARC_ERROR_PARSE_INPUT       = 32        !< Type of error message: wrong input parameter
INTEGER, PARAMETER :: NSCARC_ERROR_STACK_MESSAGE     = 33        !< Type of error message: error with stack message
INTEGER, PARAMETER :: NSCARC_ERROR_STACK_SOLVER      = 34        !< Type of error message: error in solver stack
INTEGER, PARAMETER :: NSCARC_ERROR_STENCIL           = 35        !< Type of error message: error in matrix stencil
INTEGER, PARAMETER :: NSCARC_ERROR_VECTOR_LENGTH     = 36        !< Type of error message: error in vector length

INTEGER, PARAMETER :: NSCARC_EXCHANGE_AUXILIARY      =  1        !< Type of data exchange: various auxiliary data 
INTEGER, PARAMETER :: NSCARC_EXCHANGE_BASIC_SIZES    =  2        !< Type of data exchange: basic sizes during setup
INTEGER, PARAMETER :: NSCARC_EXCHANGE_CELL_NEIGHBORS =  3        !< Type of data exchange: neighboring cells
INTEGER, PARAMETER :: NSCARC_EXCHANGE_CELL_NUMBERS   =  4        !< Type of data exchange: neighboring cell numbers
INTEGER, PARAMETER :: NSCARC_EXCHANGE_CELL_SIZES     =  5        !< Type of data exchange: neighboring cell sizes
INTEGER, PARAMETER :: NSCARC_EXCHANGE_LAYER2_NUMS    =  6        !< Type of data exchange: numbers of second layer cells
INTEGER, PARAMETER :: NSCARC_EXCHANGE_LAYER2_VALS    =  7        !< Type of data exchange: values of second layer cells
INTEGER, PARAMETER :: NSCARC_EXCHANGE_MATRIX_COLS    =  8        !< Type of data exchange: (local) columns of Poisson matrix
INTEGER, PARAMETER :: NSCARC_EXCHANGE_MATRIX_COLSG   =  9        !< Type of data exchange: global columns of Poisson matrix
INTEGER, PARAMETER :: NSCARC_EXCHANGE_MATRIX_DIAGS   = 10        !< Type of data exchange: diagonal of Poisson matrix
INTEGER, PARAMETER :: NSCARC_EXCHANGE_MATRIX_SIZES   = 11        !< Type of data exchange: size of Poisson matrix 
INTEGER, PARAMETER :: NSCARC_EXCHANGE_MATRIX_VALS    = 12        !< Type of data exchange: values of Poisson matrix
INTEGER, PARAMETER :: NSCARC_EXCHANGE_MGM_SINGLE     = 13        !< Type of data exchange: MGM - Mean value interface
INTEGER, PARAMETER :: NSCARC_EXCHANGE_MGM_DOUBLE     = 14        !< Type of data exchange: MGM - True approximate interface
INTEGER, PARAMETER :: NSCARC_EXCHANGE_MGM_VELO       = 15        !< Type of data exchange: MGM - Velocity interface
INTEGER, PARAMETER :: NSCARC_EXCHANGE_MGM_TRUE       = 16        !< Type of data exchange: MGM - Velocity interface
INTEGER, PARAMETER :: NSCARC_EXCHANGE_NULLSPACE      = 17        !< Type of data exchange: nullspace entries (AMG only)
INTEGER, PARAMETER :: NSCARC_EXCHANGE_PRESSURE       = 18        !< Type of data exchange: pressure values
INTEGER, PARAMETER :: NSCARC_EXCHANGE_SOLIDS         = 19        !< Type of data exchange: pressure values
INTEGER, PARAMETER :: NSCARC_EXCHANGE_VECTOR_MEAN    = 20        !< Type of data exchange: mean values of a vector
INTEGER, PARAMETER :: NSCARC_EXCHANGE_VECTOR_PLAIN   = 21        !< Type of data exchange: plain values of a vector
INTEGER, PARAMETER :: NSCARC_EXCHANGE_ZONE_NEIGHBORS = 22        !< Type of data exchange: aggregation zones (AMG only)
INTEGER, PARAMETER :: NSCARC_EXCHANGE_ZONE_TYPES     = 23        !< Type of data exchange: aggregation zones types (AMG only)

INTEGER, PARAMETER :: NSCARC_GRID_STRUCTURED         =  1        !< Type of discretization: structured 
INTEGER, PARAMETER :: NSCARC_GRID_UNSTRUCTURED       =  2        !< Type of discretization: unstructured 

INTEGER, PARAMETER :: NSCARC_INIT_UNDEF              =-999       !< Type of data allocation: initialize as undefined
INTEGER, PARAMETER :: NSCARC_INIT_NONE               =  -2       !< Type of data allocation: do not initialize
INTEGER, PARAMETER :: NSCARC_INIT_MINUS              =  -1       !< Type of data allocation: initialize with minus one
INTEGER, PARAMETER :: NSCARC_INIT_ZERO               =   0       !< Type of data allocation: initialize with zero
INTEGER, PARAMETER :: NSCARC_INIT_ONE                =   1       !< Type of data allocation: initialize with one
INTEGER, PARAMETER :: NSCARC_INIT_TRUE               =   2       !< Type of data allocation: initialize with .TRUE.
INTEGER, PARAMETER :: NSCARC_INIT_FALSE              =   3       !< Type of data allocation: initialize with .FALSE.
INTEGER, PARAMETER :: NSCARC_INIT_HUGE               =   4       !< Type of data allocation: initialize with .FALSE.

INTEGER, PARAMETER :: NSCARC_INTERPOL_BILINEAR       =  1        !< Type of grid interpolation: bilinear 
INTEGER, PARAMETER :: NSCARC_INTERPOL_CONSTANT       =  2        !< Type of grid interpolation: constant 
INTEGER, PARAMETER :: NSCARC_INTERPOL_CLASSICAL      =  3        !< Type of grid interpolation: classical
INTEGER, PARAMETER :: NSCARC_INTERPOL_DIRECT         =  4        !< Type of grid interpolation: direct 
INTEGER, PARAMETER :: NSCARC_INTERPOL_STANDARD       =  5        !< Type of grid interpolation: standard 
INTEGER, PARAMETER :: NSCARC_INTERPOL_AMG            =  6        !< Type of grid interpolation: AMG-defined
INTEGER, PARAMETER :: NSCARC_INTERPOL_BILINEAR2      =  7        !< Type of grid interpolation: bilinear 

INTEGER, PARAMETER :: NSCARC_KRYLOV_MAIN             =  1        !< Type of Krylov solver: use it as main solver
INTEGER, PARAMETER :: NSCARC_KRYLOV_COARSE           =  2        !< Type of Krylov solver: use it as coarse grid solver

INTEGER, PARAMETER :: NSCARC_LEVEL_MIN               =  0        !< Range of multigrid levels: minimum level
INTEGER, PARAMETER :: NSCARC_LEVEL_MAX               = 10        !< Range of multigrid levels: maximum level
INTEGER, PARAMETER :: NSCARC_LEVEL_SINGLE            =  1        !< Type of multigrid levels: only one level needed
INTEGER, PARAMETER :: NSCARC_LEVEL_MULTI             =  2        !< Type of multigrid levels: multiple levels needed

INTEGER, PARAMETER :: NSCARC_MATRIX_BANDWISE         =  1        !< Type of matrix storage technique: bandwise
INTEGER, PARAMETER :: NSCARC_MATRIX_COMPACT          =  2        !< Type of matrix storage technique: compact
INTEGER, PARAMETER :: NSCARC_MATRIX_CONDENSED        =  3        !< Flag for matrix treatment: condensing applied
INTEGER, PARAMETER :: NSCARC_MATRIX_CONNECTION       =  4        !< Flag for matrix selection: strength of connection matrix
INTEGER, PARAMETER :: NSCARC_MATRIX_FULL             =  5        !< Flag for matrix allocation: maximum possible size
INTEGER, PARAMETER :: NSCARC_MATRIX_LIGHT            =  7        !< Flag for matrix allocation: reduced size (no COLG info)
INTEGER, PARAMETER :: NSCARC_MATRIX_MINIMAL          =  8        !< Flag for matrix allocation: reduced size (only ROW and COL)
INTEGER, PARAMETER :: NSCARC_MATRIX_POISSON          =  9        !< Flag for matrix selection: Poisson 
INTEGER, PARAMETER :: NSCARC_MATRIX_POISSON_PROL     = 10        !< Flag for matrix selection: Poisson times Prolongation 
INTEGER, PARAMETER :: NSCARC_MATRIX_POISSON_SYM      = 11        !< Flag for matrix selection: symmetric Poisson 
INTEGER, PARAMETER :: NSCARC_MATRIX_LAPLACE          = 12        !< Flag for matrix selection: Poisson 
INTEGER, PARAMETER :: NSCARC_MATRIX_LAPLACE_SYM      = 13        !< Flag for matrix selection: Poisson 
INTEGER, PARAMETER :: NSCARC_MATRIX_PROLONGATION     = 14        !< Flag for matrix selection: Prolongation (AMG only)
INTEGER, PARAMETER :: NSCARC_MATRIX_RESTRICTION      = 15        !< Flag for matrix selection: Restriction (AMG only)
INTEGER, PARAMETER :: NSCARC_MATRIX_ZONES            = 16        !< Flag for matrix selection: Aggregation zones (AMG only)
INTEGER, PARAMETER :: NSCARC_MATRIX_LOWER            = 17        !< Flag for matrix selection: Lower matrix for MGM-LU decomposition
INTEGER, PARAMETER :: NSCARC_MATRIX_UPPER            = 18        !< Flag for matrix selection: Upper matrix for MGM-LU decomposition

INTEGER, PARAMETER :: NSCARC_MATVEC_GLOBAL           =  1        !< Scope of matrix-vector product: globally 
INTEGER, PARAMETER :: NSCARC_MATVEC_LOCAL            =  2        !< Scope of matrix-vector product: locally

INTEGER, PARAMETER :: NSCARC_MAX_FACE_NEIGHBORS      = 10        !< Maximum settings: Number of administrable face neighbors
INTEGER, PARAMETER :: NSCARC_MAX_STENCIL             =  7        !< Maximum settings: Number of legs in Poisson stencil
INTEGER, PARAMETER :: NSCARC_MAX_BUFFER0             = 10        !< Maximum settings: Buffer size for initial data exchanges

INTEGER, PARAMETER :: NSCARC_STORAGE_CREATE          =  1        !< Type of memory operation: create array
INTEGER, PARAMETER :: NSCARC_STORAGE_REMOVE          =  2        !< Type of memory operation: remove array
INTEGER, PARAMETER :: NSCARC_STORAGE_RESIZE          =  3        !< Type of memory operation: resize array
INTEGER, PARAMETER :: NSCARC_STORAGE_MAX             = 10000     !< Current maximum of allocatable arrays (may be increased)
                   
INTEGER, PARAMETER :: NSCARC_METHOD_KRYLOV           =  1        !< Global ScaRC method: Krylov solver
INTEGER, PARAMETER :: NSCARC_METHOD_MULTIGRID        =  2        !< Global ScaRC method: Multigrid solver
INTEGER, PARAMETER :: NSCARC_METHOD_LU               =  3        !< Global ScaRC method: LU-decomposition based on MKL
INTEGER, PARAMETER :: NSCARC_METHOD_MGM              =  4        !< Global ScaRC method: McKeeney-Greengard-Mayo solver

INTEGER, PARAMETER :: NSCARC_MKL_NONE                =  0        !< Type of MKL method: no use of MKL 
INTEGER, PARAMETER :: NSCARC_MKL_LOCAL               =  1        !< Type of MKL method: local LU-decompositions 
INTEGER, PARAMETER :: NSCARC_MKL_GLOBAL              =  2        !< Type of MKL method: global LU-decomposition
INTEGER, PARAMETER :: NSCARC_MKL_COARSE              =  3        !< Type of MKL method: only coarse grid level

INTEGER, PARAMETER :: NSCARC_MGM_POISSON             =  1        !< Type of MGM pass: First (inhomogeneous Poisson)
INTEGER, PARAMETER :: NSCARC_MGM_LAPLACE             =  2        !< Type of MGM pass: Second (homogeneous Laplace)
INTEGER, PARAMETER :: NSCARC_MGM_SCARC               =  3        !< Type of MGM pass: Process structured ScaRC solution
INTEGER, PARAMETER :: NSCARC_MGM_USCARC              =  4        !< Type of MGM pass: Process unstructured UScaRC solution
INTEGER, PARAMETER :: NSCARC_MGM_MERGE               =  5        !< Type of MGM pass: Merge first and second pass
INTEGER, PARAMETER :: NSCARC_MGM_TERMINATE           =  6        !< Type of MGM pass: Terminate current MGM  
INTEGER, PARAMETER :: NSCARC_MGM_FAILURE             = 11        !< Type of MGM convergence: Failure
INTEGER, PARAMETER :: NSCARC_MGM_SUCCESS             = 12        !< Type of MGM convergence: Success
INTEGER, PARAMETER :: NSCARC_MGM_BC_EXPOL            = 21        !< Type of internal MGM boundary: Linear extrapolatioln
INTEGER, PARAMETER :: NSCARC_MGM_BC_MEAN             = 22        !< Type of internal MGM boundary: Simple mean value 
INTEGER, PARAMETER :: NSCARC_MGM_BC_TAYLOR           = 23        !< Type of internal MGM boundary: Taylor expansion
INTEGER, PARAMETER :: NSCARC_MGM_BC_TRUE             = 24        !< Type of internal MGM boundary: True approximate
INTEGER, PARAMETER :: NSCARC_MGM_DSCARC_TO_UHL       = 31        !< Type of MGM copy: diff ScaRC-UScaRC to unstruct hom Laplace
INTEGER, PARAMETER :: NSCARC_MGM_DSCARC_TO_UHL2      = 32        !< Type of MGM copy: diff ScaRC-UScaRC to prev unstruct hom Laplace
INTEGER, PARAMETER :: NSCARC_MGM_SCARC_TO_SIP        = 33        !< Type of MGM copy: ScaRC to struct inhom Poisson
INTEGER, PARAMETER :: NSCARC_MGM_USCARC_TO_UIP       = 34        !< Type of MGM copy: UScarc to unstructured inhom Poisson
INTEGER, PARAMETER :: NSCARC_MGM_SIP_TO_UIP          = 35        !< Type of MGM copy: struct Poisson to unstruct Poisson (setup)
INTEGER, PARAMETER :: NSCARC_MGM_UHL_TO_UHL2         = 36        !< Type of MGM copy: unstruct hom Laplace to prev 
INTEGER, PARAMETER :: NSCARC_MGM_OUHL_TO_OUHL2       = 37        !< Type of MGM copy: other unstruct hom Laplace to prev
INTEGER, PARAMETER :: NSCARC_MGM_USCARC_VS_SCARC     = 41        !< Type of MGM difference: UScaRC vs ScaRC
INTEGER, PARAMETER :: NSCARC_MGM_UHL_VS_DSCARC       = 42        !< Type of MGM difference: UScaRC-ScaRC vs unstruct hom Laplace
INTEGER, PARAMETER :: NSCARC_MGM_UIP_VS_USCARC       = 43        !< Type of MGM difference: UScaRC vs unstruct inhom Poisson
INTEGER, PARAMETER :: NSCARC_MGM_LAPLACE_AMG         = 51        !< Type of MGM Laplace solver: AMG method
INTEGER, PARAMETER :: NSCARC_MGM_LAPLACE_CG          = 52        !< Type of MGM Laplace solver: CG method
INTEGER, PARAMETER :: NSCARC_MGM_LAPLACE_PARDISO     = 53        !< Type of MGM Laplace solver: IntelMKL-Pardiso solver
INTEGER, PARAMETER :: NSCARC_MGM_LAPLACE_FFT         = 54        !< Type of MGM Laplace solver: Crayfishpak FFT solver
INTEGER, PARAMETER :: NSCARC_MGM_LAPLACE_LU          = 55        !< Type of MGM Laplace solver: Own LU method
INTEGER, PARAMETER :: NSCARC_MGM_LAPLACE_LUPERM      = 56        !< Type of MGM Laplace solver: Own permuted LU method
INTEGER, PARAMETER :: NSCARC_MGM_LAPLACE_OPTIMIZED   = 57        !< Type of MGM Laplace solver: Best optimized method (FFT/PARDISO)
INTEGER, PARAMETER :: NSCARC_MGM_INTERPOL_LINEAR     = 61        !< Type of MGM interpolation: Use linear interpolation for BC's
INTEGER, PARAMETER :: NSCARC_MGM_INTERPOL_SQUARE     = 62        !< Type of MGM interpolation: Use quadratic interpolation for BC's

INTEGER, PARAMETER :: NSCARC_MULTIGRID_GEOMETRIC     =  1        !< Type of multigrid method: geometric multigrid
INTEGER, PARAMETER :: NSCARC_MULTIGRID_ALGEBRAIC     =  2        !< Type of multigrid method: algebraic multigrid
INTEGER, PARAMETER :: NSCARC_MULTIGRID_MAIN          =  1        !< Type of multigrid method: used as main solver
INTEGER, PARAMETER :: NSCARC_MULTIGRID_PRECON        =  2        !< Type of multigrid method: used as preconditioner

INTEGER, PARAMETER :: NSCARC_ORDER_ACTIVE            =  1        !< Order of aggregation: mesh is active
INTEGER, PARAMETER :: NSCARC_ORDER_LOCKED            = -1        !< Order of aggregation: mesh is locked
INTEGER, PARAMETER :: NSCARC_ORDER_UNASSIGNED        =  0        !< Order of aggregation: mesh is unassigned 

INTEGER, PARAMETER :: NSCARC_PRECISION_SINGLE        =  1        !< Type of data precision: single
INTEGER, PARAMETER :: NSCARC_PRECISION_DOUBLE        =  2        !< Type of data precision: double 

INTEGER, PARAMETER :: NSCARC_RELAX_FFT               =  1        !< Type of preconditioner: FFT-methods
INTEGER, PARAMETER :: NSCARC_RELAX_FFTO              =  2        !< Type of preconditioner: FFTO-methods (including overlap)
INTEGER, PARAMETER :: NSCARC_RELAX_GMG               =  3        !< Type of preconditioner: multigrid methods
INTEGER, PARAMETER :: NSCARC_RELAX_ILU               =  4        !< Type of preconditioner: ILU-decompositions (own)
INTEGER, PARAMETER :: NSCARC_RELAX_JAC               =  5        !< Type of preconditioner: JACOBI-methods
INTEGER, PARAMETER :: NSCARC_RELAX_LU                =  6        !< Type of preconditioner: LU-decompositions (own)
INTEGER, PARAMETER :: NSCARC_RELAX_MGS               =  7        !< Type of preconditioner: MGS-methods (matrix form)
INTEGER, PARAMETER :: NSCARC_RELAX_MJAC              =  8        !< Type of preconditioner: MJAC-methods (matrix form)
INTEGER, PARAMETER :: NSCARC_RELAX_MKL               =  9        !< Type of preconditioner: LU-decompositions (MKL)
INTEGER, PARAMETER :: NSCARC_RELAX_MSGS              = 10        !< Type of preconditioner: MSGS-methods (matrix form)
INTEGER, PARAMETER :: NSCARC_RELAX_MSOR              = 11        !< Type of preconditioner: MSOR-methods (matrix form)
INTEGER, PARAMETER :: NSCARC_RELAX_MSSOR             = 12        !< Type of preconditioner: MSSOR-methods (matrix form)
INTEGER, PARAMETER :: NSCARC_RELAX_OPTIMIZED         = 13        !< Type of preconditioner: Optimized mixture of MKL and FFT
INTEGER, PARAMETER :: NSCARC_RELAX_SSOR              = 14        !< Type of preconditioner: SSOR-methods

INTEGER, PARAMETER :: NSCARC_SCOPE_GLOBAL            =  0        !< Scope of current solver: global
INTEGER, PARAMETER :: NSCARC_SCOPE_LOCAL             =  1        !< Scope of current solver: local

INTEGER, PARAMETER :: NSCARC_SOLVER_MAIN             =  1        !< Type of solver: main solver
INTEGER, PARAMETER :: NSCARC_SOLVER_PRECON           =  2        !< Type of solver: preconditioner
INTEGER, PARAMETER :: NSCARC_SOLVER_SMOOTH           =  3        !< Type of solver: smoother
INTEGER, PARAMETER :: NSCARC_SOLVER_COARSE           =  4        !< Type of solver: coarse grid solver
INTEGER, PARAMETER :: NSCARC_SOLVER_MGM              =  5        !< Type of solver: second pass in MGM solver

INTEGER, PARAMETER :: NSCARC_STACK_ZERO              =   0       !< Order in solver stack: zero position
INTEGER, PARAMETER :: NSCARC_STACK_ROOT              =   1       !< Order in solver stack: root position
INTEGER, PARAMETER :: NSCARC_STACK_MAX               =  10       !< Order in solver stack: maximum position
INTEGER, PARAMETER :: NSCARC_STACK_NOPARENT          = -99       !< Order in solver stack: no parent available

INTEGER, PARAMETER :: NSCARC_STAGE_ONE               =  1        !< Stage of administration for current method: primary stage 
INTEGER, PARAMETER :: NSCARC_STAGE_TWO               =  2        !< Stage of administration for current method: secondary stage 

INTEGER, PARAMETER :: NSCARC_STATE_PROCEED           =  0        !< State of multigrid: proceed loop
INTEGER, PARAMETER :: NSCARC_STATE_CONV_INITIAL      =  1        !< State of multigrid: check initial residual
INTEGER, PARAMETER :: NSCARC_STATE_CONV              =  2        !< State of multigrid: check residual
INTEGER, PARAMETER :: NSCARC_STATE_DIVG              =  3        !< State of multigrid: check divergence

INTEGER, PARAMETER :: NSCARC_STENCIL_CONSTANT        =  1        !< Type of matrix stencil: constant matrix entries
INTEGER, PARAMETER :: NSCARC_STENCIL_VARIABLE        =  2        !< Type of matrix stencil: variable matrix entries

INTEGER, PARAMETER :: NSCARC_TWOLEVEL_NONE           =  0        !< Type of two-level method: only one level
INTEGER, PARAMETER :: NSCARC_TWOLEVEL_ADD            =  1        !< Type of two-level method: additive 2-level 
INTEGER, PARAMETER :: NSCARC_TWOLEVEL_MUL            =  2        !< Type of two-level method: multiplicative 2-level 
INTEGER, PARAMETER :: NSCARC_TWOLEVEL_MUL2           =  3        !< Type of two-level method: multiplicative 2-level, type2
INTEGER, PARAMETER :: NSCARC_TWOLEVEL_COARSE         =  4        !< Type of two-level method: only coarse grid
INTEGER, PARAMETER :: NSCARC_TWOLEVEL_MACRO          =  5        !< Type of two-level method: use macro solver 
INTEGER, PARAMETER :: NSCARC_TWOLEVEL_XMEAN          =  6        !< Type of two-level method: mean values in x-direction

INTEGER, PARAMETER :: NSCARC_VECTOR_ONE_X            =  1        !< Flag for 1D-vector on stage 1: X
INTEGER, PARAMETER :: NSCARC_VECTOR_ONE_B            =  2        !< Flag for 1D-vector on stage 1: B
INTEGER, PARAMETER :: NSCARC_VECTOR_ONE_D            =  3        !< Flag for 1D-vector on stage 1: D
INTEGER, PARAMETER :: NSCARC_VECTOR_ONE_R            =  4        !< Flag for 1D-vector on stage 1: R
INTEGER, PARAMETER :: NSCARC_VECTOR_ONE_V            =  5        !< Flag for 1D-vector on stage 1: V
INTEGER, PARAMETER :: NSCARC_VECTOR_ONE_Y            =  6        !< Flag for 1D-vector on stage 1: Y
INTEGER, PARAMETER :: NSCARC_VECTOR_ONE_Z            =  7        !< Flag for 1D-vector on stage 1: Z
INTEGER, PARAMETER :: NSCARC_VECTOR_ONE_E            =  8        !< Flag for 1D-vector on stage 1: E
INTEGER, PARAMETER :: NSCARC_VECTOR_TWO_X            =  9        !< Flag for 1D-vector on stage 2: X
INTEGER, PARAMETER :: NSCARC_VECTOR_TWO_B            = 10        !< Flag for 1D-vector on stage 2: B
INTEGER, PARAMETER :: NSCARC_VECTOR_TWO_D            = 11        !< Flag for 1D-vector on stage 2: D                
INTEGER, PARAMETER :: NSCARC_VECTOR_TWO_R            = 12        !< Flag for 1D-vector on stage 2: R
INTEGER, PARAMETER :: NSCARC_VECTOR_TWO_V            = 13        !< Flag for 1D-vector on stage 2: V
INTEGER, PARAMETER :: NSCARC_VECTOR_TWO_Y            = 14        !< Flag for 1D-vector on stage 2: Y                
INTEGER, PARAMETER :: NSCARC_VECTOR_TWO_Z            = 15        !< Flag for 1D-vector on stage 2: Z
INTEGER, PARAMETER :: NSCARC_VECTOR_TWO_E            = 16        !< Flag for 1D-vector on stage 2: E
INTEGER, PARAMETER :: NSCARC_VECTOR_H                = 17        !< Flag for 3D-vector H 
INTEGER, PARAMETER :: NSCARC_VECTOR_HS               = 18        !< Flag for 3D-vector HS

INTEGER, PARAMETER :: NSCARC_WARNING_NO_MKL_PRECON   =  1        !< Type of warning message: No MKL preconditioner available
INTEGER, PARAMETER :: NSCARC_WARNING_NO_MKL_SMOOTH   =  2        !< Type of warning message: No MKL smoother available
INTEGER, PARAMETER :: NSCARC_WARNING_NO_MKL_LU       =  3        !< Type of warning message: Using LU instead of MKL
INTEGER, PARAMETER :: NSCARC_WARNING_NO_GLOBAL_SCOPE =  4        !< Type of warning message: No global scope solver available
INTEGER, PARAMETER :: NSCARC_WARNING_ONLY_SSOR_PRECON=  5        !< Type of warning message: Only SSOR preconditioner available
INTEGER, PARAMETER :: NSCARC_WARNING_ONLY_SSOR_SMOOTH=  6        !< Type of warning message: Only SSOR smoother available

INTEGER, PARAMETER :: NSCARC_UNDEF_INT               = -1        !< Flag for undefined integer value
INTEGER, PARAMETER :: NSCARC_ZERO_INT                =  0        !< Flag for zero integer value
INTEGER, PARAMETER :: NSCARC_ONE_INT                 =  1        !< Flag for one integer value

REAL(EB), PARAMETER:: NSCARC_UNDEF_REAL_EB           = -1.0_EB   !< Flag for undefined double precision value
REAL(EB), PARAMETER:: NSCARC_ZERO_REAL_EB            =  0.0_EB   !< Flag for zero double precision value
REAL(EB), PARAMETER:: NSCARC_ONE_REAL_EB             =  1.0_EB   !< Flag for one double precision value

REAL(FB), PARAMETER:: NSCARC_UNDEF_REAL_FB           = -1.0_FB   !< Flag for undefined single precision value
REAL(FB), PARAMETER:: NSCARC_ZERO_REAL_FB            =  0.0_FB   !< Flag for zero single precision value
REAL(FB), PARAMETER:: NSCARC_ONE_REAL_FB             =  1.0_FB   !< Flag for one single precision value

CHARACTER(40), PARAMETER :: SCARC_NONE = 'NONE'                  !< Flag for a dummy character value 
INTEGER, PARAMETER  :: NSCARC_NONE = -123456789                  !< Flag for a dummy integer value 

REAL(FB), PARAMETER :: NSCARC_HUGE_REAL_FB = -999999999.0_FB     !< Flag for an undefined double precision value
REAL(EB), PARAMETER :: NSCARC_HUGE_REAL_EB = -999999999.0_EB     !< Flag for an undefined double precision value
INTEGER, PARAMETER  :: NSCARC_HUGE_INT     = -999999999          !< Flag for an undefined integer value

REAL(EB), PARAMETER :: NSCARC_THRESHOLD_CONVERGENCE = 1.0E-12_EB !< Threshold for convergence
REAL(EB), PARAMETER :: NSCARC_THRESHOLD_DIVGERGENCE = 1.0E+15_EB !< Threshold for divergence

REAL(EB), PARAMETER :: SCALR  = 0.015625_EB                      !< Scaling parameter for geometric multigrid method
REAL(EB), PARAMETER :: SCALP  = 0.0625_EB                        !< Scaling parameter for geometric multigrid method
REAL(EB), PARAMETER :: W1     =  1.0_EB                          !< Weighting parameter for geometric multigrid method
REAL(EB), PARAMETER :: W3     =  3.0_EB                          !< Weighting parameter for geometric multigrid method
REAL(EB), PARAMETER :: W4     =  4.0_EB                          !< Weighting parameter for geometric multigrid method
REAL(EB), PARAMETER :: W9     =  9.0_EB                          !< Weighting parameter for geometric multigrid method
REAL(EB), PARAMETER :: W12    = 12.0_EB                          !< Weighting parameter for geometric multigrid method
REAL(EB), PARAMETER :: W16    = 16.0_EB                          !< Weighting parameter for geometric multigrid method
#ifdef WITH_SCARC_POSTPROCESSING
INTEGER, PARAMETER :: NSCARC_DUMP_A                  =  1        !< Flag for the dumping of matrix A
INTEGER, PARAMETER :: NSCARC_DUMP_B                  =  2        !< Flag for the dumping of right hand side B
INTEGER, PARAMETER :: NSCARC_DUMP_X                  =  3        !< Flag for the dumping of solution X
INTEGER, PARAMETER :: NSCARC_DUMP_MESH               =  4        !< Flag for the dumping of mesh information
#endif

END MODULE SCARC_CONSTANTS

