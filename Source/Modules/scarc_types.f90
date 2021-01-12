!//////////////////////////////////////////////////////////////////////////////////////////////////////
!
! MODULE SCARC_TYPES
!
!> \brief Collection of self-defined data types needed for the different ScaRC/UScaRC solvers
!
!//////////////////////////////////////////////////////////////////////////////////////////////////////
MODULE SCARC_TYPES

USE PRECISION_PARAMETERS
USE SCARC_CONSTANTS

!#ifdef WITH_SCARC_MKL
USE MKL_PARDISO
USE MKL_CLUSTER_SPARSE_SOLVER
!#endif

IMPLICIT NONE

!> \brief Detailed information about arrays created within the ScaRC memory manager

TYPE SCARC_ALLOCATION_TYPE

   INTEGER :: NTYPE  = NSCARC_INIT_NONE               !< Data type of array
   INTEGER :: NDIM   = NSCARC_INIT_NONE               !< Dimension of array
   INTEGER :: NINIT  = NSCARC_INIT_NONE               !< Initialization type of array
   INTEGER :: NRANK  = NSCARC_INIT_NONE               !< Rank of array (order of allocation)
   INTEGER :: NSTATE = NSCARC_INIT_NONE               !< State of array (allocated/removed)

   INTEGER :: LBND(3) = NSCARC_INIT_NONE              !< Left bounds of array for x-,y- and z-direction
   INTEGER :: RBND(3) = NSCARC_INIT_NONE              !< Right bounds of array for x-,y- and z-direction

   CHARACTER(60) :: CNAME                             !< Name of array
   CHARACTER(60) :: CSCOPE                            !< Name of allocating routine 

END TYPE SCARC_ALLOCATION_TYPE
 
!> \brief ScaRC memory manager type
 
TYPE SCARC_STORAGE_TYPE

   TYPE (SCARC_ALLOCATION_TYPE), ALLOCATABLE, DIMENSION(:) :: ALLOCATION_LIST  !< Administrative list of allocated structures

   INTEGER :: IP                                           !< Pointer to current array entry
   INTEGER :: IRANK                                        !< Rank of memory accesses

   INTEGER :: NSUM_INT     = NSCARC_INIT_ZERO              !< Total sum of all allocated integer data
   INTEGER :: NSUM_REAL_EB = NSCARC_INIT_ZERO              !< Total sum of all allocated double precision data
   INTEGER :: NSUM_REAL_FB = NSCARC_INIT_ZERO              !< Total sum of all allocated single precision data

   INTEGER :: N_ARRAYS  = NSCARC_INIT_ZERO                 !< Number of allocated arrays
   INTEGER :: N_BMATRIX = NSCARC_INIT_ZERO                 !< Number of allocated bandwise stored matrices
   INTEGER :: N_CMATRIX = NSCARC_INIT_ZERO                 !< Number of allocated compactly stored matrices
   INTEGER :: N_INT     = NSCARC_INIT_ZERO                 !< Number of allocated integer data
   INTEGER :: N_LOG     = NSCARC_INIT_ZERO                 !< Number of allocated logical data
   INTEGER :: N_REAL_EB = NSCARC_INIT_ZERO                 !< Number of allocated double precision data
   INTEGER :: N_REAL_FB = NSCARC_INIT_ZERO                 !< Number of allocated single precision data

   INTEGER :: NWORK_INT     = NSCARC_INIT_ZERO             !< Workspace occupied by integer arrays
   INTEGER :: NWORK_LOG     = NSCARC_INIT_ZERO             !< Workspace occupied by logical arrays
   INTEGER :: NWORK_REAL_EB = NSCARC_INIT_ZERO             !< Workspace occupied by double precision arrays
   INTEGER :: NWORK_REAL_FB = NSCARC_INIT_ZERO             !< Workspace occupied by single precision arrays

END TYPE SCARC_STORAGE_TYPE

!> \brief Messaging and debugging mechanisms
 
TYPE SCARC_MESSAGE_TYPE

   CHARACTER(60) :: FILE_CPU                               !< Output file name for CPU measurements
   CHARACTER(60) :: FILE_MEM                               !< Output file name for memory management information
   CHARACTER(60) :: FILE_STAT                              !< Output file name for convergence statistcis
   INTEGER :: LU_CPU                                       !< Logical unit for CPU measurements
   INTEGER :: LU_MEM                                       !< Logical unit for memory management information
   INTEGER :: LU_STAT                                      !< Logical unit for convergence statistics
 
!#ifdef WITH_SCARC_DEBUG
   CHARACTER(60) :: FILE_DEBUG                             !< Output file name for debugging information
   CHARACTER(60) :: FILE_DUMP                              !< Output file name for dumping information
   INTEGER :: LU_DEBUG                                     !< Logical unit for debugging information
   INTEGER :: LU_DUMP                                      !< Logical unit for dumping information
   CHARACTER(20) :: CFORM1, CFORM2, CFORM3, CFORM4
!#endif

!#ifdef WITH_SCARC_VERBOSE
   CHARACTER(60)  :: FILE_VERBOSE                          !< Output file name for verbose messages
   INTEGER :: LU_VERBOSE                                   !< Logical unit for verbose messages
!#endif

#ifdef WITH_SCARC_POSTPROCESSING
   INTEGER :: LU_SCARC                                                !< Logical unit for dump of complete ScaRC environment
   INTEGER :: LU_POST, LU_POST1, LU_POST2, LU_POST3                   !< Logical unit for dump of selected data
   CHARACTER(120) :: FILE_SCARC                                       !< Output file name for dumpcomplete ScaRC environment
   CHARACTER(60)  :: FILE_POST, FILE_POST1, FILE_POST2, FILE_POST3    !< Output file names for dumpof selected data
#endif

END TYPE SCARC_MESSAGE_TYPE

!> \brief Information about the neighborship structur within the mesh decomposition 
 
TYPE SCARC_SUBDIVISION_TYPE

   INTEGER, ALLOCATABLE, DIMENSION (:)   :: N_NEIGHBORS      !< Number of meshes for in complete subdivision
   INTEGER, ALLOCATABLE, DIMENSION (:,:) :: NEIGHBORS        !< Global neighborship structure between meshes
   INTEGER, ALLOCATABLE, DIMENSION (:,:) :: ORDER            !< Order of meshes during aggregation process
   INTEGER :: N_CYCLES                                       !< Number of needed cycles during aggregation process
   INTEGER :: N_NEIGHBORS_TOTAL                              !< Sum of all neighbors

END TYPE SCARC_SUBDIVISION_TYPE

 
!> \brief Face information related to wall cells and neighbors
 
TYPE SCARC_FACE_TYPE

   REAL(EB), POINTER, DIMENSION(:) :: DH              !< Step size vector between adjacent faces

   REAL(EB) :: INCR_BOUNDARY                          !< Increment for boundary conditions
   REAL(EB) :: INCR_FACE                              !< Increment for matrix subdiagonals on face
   REAL(EB) :: INCR_INSIDE                            !< Increment for matrix subdiagonals between opposite faces

   REAL(EB) :: SCAL_DIRICHLET                         !< Scaling factor for Dirichlet BC's
   REAL(EB) :: SCAL_NEUMANN                           !< Scaling factor for Neumann BC's
   REAL(EB) :: SCAL_MGM                               !< Scaling factor for MGM method

   INTEGER, ALLOCATABLE, DIMENSION(:) :: NEIGHBORS    !< Adjacent neighbors at that face
   INTEGER  :: N_NEIGHBORS = 0                        !< Number of adjacent neighbors 

   INTEGER  :: NOP = 0                                !< Number of cells between opposite faces
   INTEGER  :: NX = 0, NY = 0, NZ = 0                 !< Cells in different directions on that face
   INTEGER  :: NCW0 = 0, NCW = 0                      !< Number of first wall cell and total number of wall cells
   INTEGER  :: INCRX = 0, INCRY = 0, INCRZ = 0        !< Increments to next internal cell in that face direction

   INTEGER  :: INCRS (-3:3)                           !< Increments within stencil for HB-vector on that face

END TYPE SCARC_FACE_TYPE


 
!> \brief Wall information related to neighbors and BC's
 
TYPE SCARC_WALL_TYPE

   ! different properties of wall cell
   INTEGER :: BTYPE = 0                               !< Type of wall cell (Dirichlet/Neumann/Internal)
   INTEGER :: BOUNDARY_TYPE = 0                       !< Type of boundary for wall cell (Solid/Interpolated/Open))
   INTEGER :: IOR = 0                                 !< Orientation of wall cell
   INTEGER :: NOM = 0                                 !< Adjacent neighbor at wall cell

   INTEGER :: ICW = NSCARC_UNDEF_INT                  !< Internal wall cell for IW
   INTEGER :: IXG, IYG, IZG                           !< Coordinate indices of ghost cells
   INTEGER :: IXW, IYW, IZW                           !< Coordinate indices of (internal) wall cells
   INTEGER :: IXN(2), IYN(2), IZN(2)                  !< Coordinate indices of neighboring cells
   INTEGER :: ICE, ICG                                !< Flag for externa and ghost cell

END TYPE SCARC_WALL_TYPE


 
!> \brief Obstruction information
 
TYPE SCARC_OBST_TYPE
   INTEGER :: I1, I2, J1, J2, K1, K2                     !< Cell indices of obstructions
END TYPE SCARC_OBST_TYPE


!> \brief Compact matrix entries which will be exchanged during generation of condensed system
 
TYPE SCARC_MATRIX_COMPACT_CONDENSED_TYPE

   REAL(EB) :: VAL1(NSCARC_MAX_STENCIL) = 0.0_EB         !< Original values (double precision)
   REAL(EB) :: VAL2(NSCARC_MAX_STENCIL) = 0.0_EB         !< Condensed values (double precision)

   INTEGER :: COL(NSCARC_MAX_STENCIL) = 0                !< Column pointers
   INTEGER :: PTR(NSCARC_MAX_STENCIL) = 0                !< Storage pointer
   INTEGER :: N_COL

END TYPE SCARC_MATRIX_COMPACT_CONDENSED_TYPE


!> \brief Bandwise matrix entries which will exchanged during generation of condensed system
 
TYPE SCARC_MATRIX_BANDWISE_CONDENSED_TYPE

   REAL(EB) :: VAL1(NSCARC_MAX_STENCIL) = 0.0_EB         !< Original values (double precision)
   REAL(EB) :: VAL2(NSCARC_MAX_STENCIL) = 0.0_EB         !< Condensed values (double precision)
   INTEGER  :: IOR0 = 0                                  !< Position pointer
   INTEGER  :: ICO = 0                                   !< Cell pointer

END TYPE SCARC_MATRIX_BANDWISE_CONDENSED_TYPE


!> \brief Compact sparse row (COMPACT) storage technique for matrices
! Is based on three arrays:
!    - non-zero matrix values
!    - corresponding columns pointers
!    - row pointers
 
TYPE SCARC_CMATRIX_TYPE

   REAL(EB), ALLOCATABLE, DIMENSION (:) :: VAL                !< Values of matrix (real precision)
   REAL(EB), ALLOCATABLE, DIMENSION (:) :: ILU                !< ILU-decomposition
   REAL(EB), ALLOCATABLE, DIMENSION (:) :: RELAX              !< Workspace for relaxation
   REAL(EB), DIMENSION (-3:3)           :: STENCIL            !< Store basic stencil information in single precision

   REAL(FB), ALLOCATABLE, DIMENSION (:) :: VAL_FB             !< Values of matrix (single precision)
   REAL(FB), ALLOCATABLE, DIMENSION (:) :: RELAX_FB           !< Workspace for relaxation
   REAL(FB), DIMENSION (-3:3)           :: STENCIL_FB         !< Store basic stencil information in single precision

   TYPE (SCARC_MATRIX_COMPACT_CONDENSED_TYPE) :: CONDENSED(NSCARC_MAX_STENCIL)

   INTEGER,  ALLOCATABLE, DIMENSION (:) :: ROW                !< Row index vector
   INTEGER,  ALLOCATABLE, DIMENSION (:) :: COL                !< Local column index vectors
   INTEGER,  ALLOCATABLE, DIMENSION (:) :: COLG               !< Global column index vectors

   INTEGER :: POS(-3:3) = 0                                   !< Position of IOR's in STENCIL
   INTEGER :: N_CONDENSED = 0                                 !< Number of condensed entries in matrix
   INTEGER :: N_VAL = 0                                       !< Number of matrix values
   INTEGER :: N_ROW = 0                                       !< Number of matrix rows
   INTEGER :: N_STENCIL = 0                                   !< Number of points in matrix stencil
   INTEGER :: N_STENCIL_MAX = 0                               !< Max stencil size (AMG only)
   INTEGER :: NTYPE = 0                                       !< Matrix type
   INTEGER :: NPREC = 0                                       !< Precision type

   CHARACTER(40) :: CNAME                                     !< Name of matrix

END TYPE SCARC_CMATRIX_TYPE

  
!> \brief Bandwise storage technique for matrices
! The entries are stored one diagonal after the other
! Missing entries of subdiagonals are filled with zero
! Is based on two arrays: non-zero matrix entries diagonal-wise and  the offsets from the main diagonal
 
TYPE SCARC_BMATRIX_TYPE

   REAL(EB), ALLOCATABLE, DIMENSION (:)   :: AUX           !< Auxiliary vector (double precision)
   REAL(EB), ALLOCATABLE, DIMENSION (:,:) :: VAL           !< Values of matrix (double precision)
   REAL(EB), ALLOCATABLE, DIMENSION (:,:) :: RELAX         !< Workspace for relaxation
   REAL(EB), ALLOCATABLE, DIMENSION (:)   :: RELAXD        !< Workspace for relaxation - only for diagonal scaling
   REAL(EB), DIMENSION (-3:3)             :: STENCIL       !< Store basic stencil information (double precision)

   REAL(FB), ALLOCATABLE, DIMENSION (:)   :: AUX_FB        !< Auxiliary vector (single precision)
   REAL(FB), ALLOCATABLE, DIMENSION (:,:) :: VAL_FB        !< Values of matrix (single precision)
   REAL(FB), ALLOCATABLE, DIMENSION (:,:) :: RELAX_FB      !< Workspace for relaxation
   REAL(FB), DIMENSION (-3:3)             :: STENCIL_FB    !< Store basic stencil information (single precision)

   TYPE (SCARC_MATRIX_BANDWISE_CONDENSED_TYPE) :: CONDENSED(NSCARC_MAX_STENCIL)

   CHARACTER(40) :: CNAME                                  !< Name of matrix

   INTEGER,  DIMENSION (-3:3) :: OFFSET                    !< Offset pointers
   INTEGER,  DIMENSION (-3:3) :: LENGTH                    !< Relevant diagonal length 
   INTEGER,  DIMENSION (-3:3) :: SOURCE                    !< Source address in corresponding diagonal
   INTEGER,  DIMENSION (-3:3) :: TARGET                    !< Target address in corresponding diagonal

   INTEGER :: POS(-3:3) = 0                                !< position of IOR's in STENCIL and in matrix storage array
   INTEGER :: N_STENCIL = 0                                !< Number of points in matrix stencil
   INTEGER :: N_CONDENSED = 0                              !< Number of condensed entries in matrix
   INTEGER :: N_VAL = 0                                    !< Number of matrix values in general and symmetric cass
   INTEGER :: N_DIAG = 0                                   !< Length of main diagonal

END TYPE SCARC_BMATRIX_TYPE

#ifdef WITH_SCARC_POSTPROCESSING
!> \brief Pressure information (only available if POSTPROCESSING directive is set)
  
TYPE SCARC_PRESSURE_TYPE

   REAL(EB), ALLOCATABLE, DIMENSION (:)       :: B_OLD     !< Old right hand side 
   REAL(EB), ALLOCATABLE, DIMENSION (:)       :: B_NEW     !< New right hand side 
   REAL(EB), ALLOCATABLE, DIMENSION (:, :, :) :: H_OLD     !< Old predictor pressure vectors 
   REAL(EB), ALLOCATABLE, DIMENSION (:, :, :) :: HS_OLD    !< Old corrector pressure vectors 
   REAL(EB), ALLOCATABLE, DIMENSION (:, :, :) :: H_NEW     !< New predictor pressure vectors 
   REAL(EB), ALLOCATABLE, DIMENSION (:, :, :) :: HS_NEW    !< New corrector pressure vectors 
   REAL(EB), ALLOCATABLE, DIMENSION (:, :, :) :: H_DIFF    !< Difference vector of subsequent predictor vectors
   REAL(EB), ALLOCATABLE, DIMENSION (:, :, :) :: HS_DIFF   !< Difference vector of subsequent corrector vectors
   REAL(EB) :: DIFF_H  = 0.0_EB                            !< Norm of predictor difference vectors
   REAL(EB) :: DIFF_HS = 0.0_EB                            !< Norm of corrector difference vectors

END TYPE SCARC_PRESSURE_TYPE
#endif

!> \brief Information for ScaRC-internal instances of Crayfishpak-based FFT methods, used as preconditioners and smoothers
  
TYPE SCARC_FFT_TYPE

   REAL(EB), ALLOCATABLE, DIMENSION (:)       :: SAVE1     !< Saving area 
   REAL(EB), ALLOCATABLE, DIMENSION (:)       :: WORK      !< Workspace 
   REAL(EB), ALLOCATABLE, DIMENSION (:)       :: HX        !< Grid stretching vector 
   REAL(EB), ALLOCATABLE, DIMENSION (:, :)    :: BXS       !< Boundary conditions along XS 
   REAL(EB), ALLOCATABLE, DIMENSION (:, :)    :: BXF       !< Boundary conditions along XF 
   REAL(EB), ALLOCATABLE, DIMENSION (:, :)    :: BYS       !< Boundary conditions along YS 
   REAL(EB), ALLOCATABLE, DIMENSION (:, :)    :: BYF       !< Boundary conditions along YF 
   REAL(EB), ALLOCATABLE, DIMENSION (:, :)    :: BZS       !< Boundary conditions along ZS 
   REAL(EB), ALLOCATABLE, DIMENSION (:, :)    :: BZF       !< Boundary conditions along ZF 
   REAL(EB), ALLOCATABLE, DIMENSION (:, :, :) :: PRHS      !< Current right hand side

   REAL(EB) :: XS                                          !< Minimum x-coordinate of mesh (may include overlap)
   REAL(EB) :: XF                                          !< Maximum x-coordinate of mesh (may include overlap)
   REAL(EB) :: YS                                          !< Minimum y-coordinate of mesh (may include overlap)
   REAL(EB) :: YF                                          !< Maximum y-coordinate of mesh (may include overlap)
   REAL(EB) :: ZS                                          !< Minimum z-coordinate of mesh (may include overlap)
   REAL(EB) :: ZF                                          !< Maximum z-coordinate of mesh (may include overlap)

   REAL(EB) :: POIS_PTB = 0.0_EB                           !< Perturbation parameter
   REAL(EB) :: XLM = 0.0_EB                                !< No Helmholtz equation used

   INTEGER :: LSAVE                                        !< Length of saving area 
   INTEGER :: LWORK                                        !< Length of workspace areas

   INTEGER :: LBC                                          !< Boundary type in x-direction
   INTEGER :: MBC                                          !< Boundary type in y-direction
   INTEGER :: NBC                                          !< Boundary type in z-direction

   INTEGER :: ITRN                                         !< Number of nodes in x-direction
   INTEGER :: JTRN                                         !< Number of nodes in y-direction
   INTEGER :: KTRN                                         !< Number of nodes in z-direction

   INTEGER :: IBAR                                         !< Number of cells in x-direction
   INTEGER :: JBAR                                         !< Number of cells in y-direction
   INTEGER :: KBAR                                         !< Number of cells in z-direction

END TYPE SCARC_FFT_TYPE

!#ifdef WITH_SCARC_MKL
!> \brief MKL information needed for IntelMKL PARDISO and CLUSTER_SPARSE_SOLVER solvers
  
TYPE SCARC_MKL_TYPE

   CHARACTER(40) :: CNAME                                                     !< Name of matrix

   INTEGER, ALLOCATABLE :: IPARM(:)                                           !< Parameter vector
   INTEGER :: MAXFCT, MNUM, MTYPE, PHASE, NRHS, ERROR, MSGLVL                 !< Various MKL specific settings 
   INTEGER :: PERM(1)                                                         !< Permutation parameter 

   TYPE (MKL_PARDISO_HANDLE),               ALLOCATABLE :: PT_H(:), PT(:)     !< Handles for PARDISO 
   TYPE (MKL_CLUSTER_SPARSE_SOLVER_HANDLE), ALLOCATABLE :: CT_H(:), CT(:)     !< Handles for CLUSTER_SPARSE_SOLVER 

END TYPE SCARC_MKL_TYPE
!#endif

!> \brief Different scopes for solution, rhs and auxiliary vectors of different solvers
  
TYPE SCARC_STAGE_TYPE

   REAL (EB), ALLOCATABLE, DIMENSION (:) :: X               !< Solution vector in double precision
   REAL (EB), ALLOCATABLE, DIMENSION (:) :: B               !< Right hand side vector in double precision
   REAL (EB), ALLOCATABLE, DIMENSION (:) :: R               !< Residual vector in double precision
   REAL (EB), ALLOCATABLE, DIMENSION (:) :: D               !< Auxiliary vector in double precision
   REAL (EB), ALLOCATABLE, DIMENSION (:) :: V               !< Auxiliary vector in double precision
   REAL (EB), ALLOCATABLE, DIMENSION (:) :: Y               !< Auxiliary vector in double precision
   REAL (EB), ALLOCATABLE, DIMENSION (:) :: Z               !< Auxiliary vector in double precision

   REAL (FB), ALLOCATABLE, DIMENSION (:) :: X_FB            !< Solution vector vector in single precision
   REAL (FB), ALLOCATABLE, DIMENSION (:) :: B_FB            !< Right hand side vector in single precision
   REAL (FB), ALLOCATABLE, DIMENSION (:) :: R_FB            !< Residual vector in single precision
   REAL (FB), ALLOCATABLE, DIMENSION (:) :: V_FB            !< Auxiliary vector in single precision

!#ifdef WITH_SCARC_DEBUG
   REAL (EB), ALLOCATABLE, DIMENSION (:) :: E               !< Error vector double precision
!#endif

END TYPE SCARC_STAGE_TYPE

!> \brief Multigrid type - to be extended for algebraic multigrid
  
TYPE SCARC_MULTIGRID_TYPE

   REAL(EB) :: APPROX_SPECTRAL_RADIUS = 2.0_EB             !< Relaxation parameter (AMG only)
   REAL(EB) :: AMG_TOL = 0.25_EB                           !< Tolerance for coarsening
   REAL(EB) :: OMEGA = 1.0_EB                              !< Relaxation parameter
   REAL(EB) :: THETA = 0.10_EB                             !< Threshold for aggregation process

   INTEGER :: CYCLING(2) = 0                               !< Counter for multigrid cycling
   INTEGER :: N_PRESMOOTH, N_POSTSMOOTH                    !< Number of pre- and post-processing steps

END TYPE SCARC_MULTIGRID_TYPE

!> \brief Information related to discretization type (structured/unstructured)
  
TYPE SCARC_GRID_TYPE

   ! Basic boundary and interface information
   TYPE (SCARC_WALL_TYPE), ALLOCATABLE, DIMENSION(:) :: WALL   !< wall information

   ! Matrices in different storage types
   TYPE (SCARC_BMATRIX_TYPE) :: POISSONB                       !< Poisson matrix in bandwise storage technique
   TYPE (SCARC_CMATRIX_TYPE) :: POISSON                        !< Poisson matrix in compact storage technique (default)
   TYPE (SCARC_CMATRIX_TYPE) :: LAPLACE                        !< Laplace matrix in compact storage technique
   TYPE (SCARC_CMATRIX_TYPE) :: GALERKIN                       !< Galerkin matrix (AMG only)
!#ifdef WITH_SCARC_MKL
   TYPE (SCARC_CMATRIX_TYPE) :: POISSON_SYM                   !< Symmetric part of compact Poisson matrix (only for MKL)
   TYPE (SCARC_CMATRIX_TYPE) :: GALERKIN_SYM                   !< Galerkin matrix symmetric version (AMG only)
!#endif

   TYPE (SCARC_CMATRIX_TYPE) :: PROLONGATION                   !< Prolongation matrix
   TYPE (SCARC_CMATRIX_TYPE) :: RESTRICTION                    !< Restriction matrix
   TYPE (SCARC_CMATRIX_TYPE) :: CONNECTION                     !< Strength of connection matrix
   TYPE (SCARC_CMATRIX_TYPE) :: ZONES                          !< Aggregation Zones matrix
   TYPE (SCARC_CMATRIX_TYPE) :: POISSON_PROL                   !< Poisson times Prolongation matrix (AMG only)

   TYPE (SCARC_CMATRIX_TYPE) :: LM            
   TYPE (SCARC_CMATRIX_TYPE) :: UM           

   REAL(EB), ALLOCATABLE, DIMENSION (:) :: MEASURES            !< Measure for grid coarsening (AMG only)
   REAL(EB), ALLOCATABLE, DIMENSION (:) :: NULLSPACE           !< Nullspace vector (AMG only)
   REAL(EB), ALLOCATABLE, DIMENSION (:) :: AUX1, AUX2          !< Auxiliary vectors (AMG only)
   REAL(EB), ALLOCATABLE, DIMENSION (:) :: DIAG                !< Matrix diagonal, possible inverted (AMG only)
   REAL(EB), ALLOCATABLE, DIMENSION (:) :: QQ, RR              !< workspace for QR-decompostion (AMG only)

   INTEGER,  ALLOCATABLE, DIMENSION (:) :: CELLS_LOCAL         !< Local coarse cells which influence Galerkin matrix (AMG only)
   INTEGER,  ALLOCATABLE, DIMENSION (:) :: CELLS_GLOBAL        !< Global coarse cells which influence Galerkin matrix (AMG only)
   INTEGER,  ALLOCATABLE, DIMENSION (:) :: LOCAL_TO_GLOBAL     !< Mapping from local to global numbering 
   INTEGER,  ALLOCATABLE, DIMENSION (:) :: ORDER               !< Search order for aggregation

   INTEGER,  ALLOCATABLE, DIMENSION (:) :: ZONES_GLOBAL        !< Global zone numbers (AMG only)
   INTEGER,  ALLOCATABLE, DIMENSION (:) :: ZONES_LOCAL         !< Local  zone numbers (AMG only)
   INTEGER,  ALLOCATABLE, DIMENSION (:) :: ZONE_CENTERS        !< Zone centers for grid coarsening (AMG only)

   REAL(EB), ALLOCATABLE, DIMENSION (:) :: ELAYER2_VALS        !< Values of cells in external second layer(AMG only)
   REAL(EB), ALLOCATABLE, DIMENSION (:) :: ILAYER2_VALS        !< Values of cells in internal second layer(AMG only)
   INTEGER,  ALLOCATABLE, DIMENSION (:) :: ELAYER2_NUMS        !< Number of cells in external second layer (AMG only)
   INTEGER,  ALLOCATABLE, DIMENSION (:) :: ILAYER2_NUMS        !< Number of cells in internal second layer (AMG only)

   ! Pointer arrays for data exchange with neighbors
   INTEGER, ALLOCATABLE, DIMENSION (:)   :: ICE_TO_IWG         !< Mapping from ICE to IWG
   INTEGER, ALLOCATABLE, DIMENSION (:)   :: ICE_TO_ICN         !< Mapping from ICE to ICN

   INTEGER, ALLOCATABLE, DIMENSION (:,:) :: ICG_TO_ICW         !< Mapping from ICG to ICW
   INTEGER, ALLOCATABLE, DIMENSION (:,:) :: ICG_TO_ICE         !< Mapping from ICG to ICE 
   INTEGER, ALLOCATABLE, DIMENSION (:)   :: ICG_TO_IWG         !< Mapping from ICG to IWG
   INTEGER, ALLOCATABLE, DIMENSION (:)   :: ICG_TO_ECELL       !< Mapping from ICG to global neighboring cell (AMG only)
   INTEGER, ALLOCATABLE, DIMENSION (:)   :: ICG_TO_ICELL       !< Mapping from ICG to local neighboring cell (AMG only)
   INTEGER, ALLOCATABLE, DIMENSION (:)   :: ICG_TO_GCELL       !< Mapping from ICG to global neighboring cell (AMG only)
   INTEGER, ALLOCATABLE, DIMENSION (:)   :: ICG_TO_OCELL       !< Mapping from ICG to local neighboring cell (AMG only)
   INTEGER, ALLOCATABLE, DIMENSION (:)   :: ICG_TO_IZONE       !< Mapping from ICG to internal own zone (AMG only)
   INTEGER, ALLOCATABLE, DIMENSION (:)   :: ICG_TO_EZONE       !< Mapping from ICG to external own zone (AMG only)
   INTEGER, ALLOCATABLE, DIMENSION (:)   :: ICG_TO_GZONE       !< Mapping from ICG to global neighboring zone (AMG only)
   INTEGER, ALLOCATABLE, DIMENSION (:)   :: ICG_TO_OZONE       !< Mapping from ICG to local neighboring zone (AMG only)
   INTEGER, ALLOCATABLE, DIMENSION (:)   :: ICG_TO_ELAYER2     !< Mapping from ICG to external second layer (AMG only)
   INTEGER, ALLOCATABLE, DIMENSION (:)   :: ICG_TO_ILAYER2     !< Mapping from ICG to internal second layer (AMG only)

   ! Forward and backward permutation vectors for grid renumbering strategies (MGM only)
   INTEGER, ALLOCATABLE, DIMENSION (:)   :: PERM_FW            !< Permutation vector for LU - forward direction
   INTEGER, ALLOCATABLE, DIMENSION (:)   :: PERM_BW            !< Permutation vector for LU - backward direction
   INTEGER :: NONZERO = 1                                      !< Index of first nonzero entry in RHS vector for permuted LU

   ! Assignment of cell coordinates
   INTEGER, ALLOCATABLE, DIMENSION (:)   :: ICX                !< I-coordinate of cell IC
   INTEGER, ALLOCATABLE, DIMENSION (:)   :: ICY                !< J-coordinate of cell IC
   INTEGER, ALLOCATABLE, DIMENSION (:)   :: ICZ                !< J-coordinate of cell IC

   ! Number and state of single cells
   INTEGER, ALLOCATABLE, DIMENSION (:,:,:) :: CELL_NUMBER      !< Numbering of single cells

   ! Cell numbers of all meshes and offsets between meshes
   INTEGER, ALLOCATABLE, DIMENSION (:) :: NC_LOCAL             !< Number of cells in local meshes
   INTEGER, ALLOCATABLE, DIMENSION (:) :: NC_OFFSET            !< Offset in cell numbering between meshes
   INTEGER :: NC_GLOBAL   = NSCARC_ZERO_INT                    !< Global number of cells in all meshes
   INTEGER :: NC_GALERKIN = NSCARC_ZERO_INT                    !< Number of cells with influence on Galerkin matrix

   ! Local numbers of internal, extended and ghost cells
   INTEGER :: NC   = NSCARC_ZERO_INT                           !< Number of cells needed for matrix
   INTEGER :: NW   = NSCARC_ZERO_INT                           !< Number of wall cells
   INTEGER :: NCG  = NSCARC_ZERO_INT                           !< Number of ghost cells 
   INTEGER :: NCGI = NSCARC_ZERO_INT                           !< Number of ghost zones in internal direction
   INTEGER :: NCGE = NSCARC_ZERO_INT                           !< Number of ghost zones in external direction
   INTEGER :: NZG  = NSCARC_ZERO_INT                           !< Number of ghost zones 
   INTEGER :: NCE  = NSCARC_ZERO_INT                           !< Number of extended cells
   INTEGER :: NCE2 = NSCARC_ZERO_INT                           !< Number of extended cells, second layer
   INTEGER :: ICE2 = NSCARC_ZERO_INT                           !< Counter for extended cells, second layer

   ! Number of Dirichlet and Neumann boundary cells
   INTEGER :: N_DIRIC   = NSCARC_ZERO_INT                      !< Number of Dirichlet BCs
   INTEGER :: N_NEUMANN = NSCARC_ZERO_INT                      !< Number of Neumann BCs
   INTEGER :: N_FINE    = NSCARC_ZERO_INT                       !< Number of fine cells (AMG only)
   INTEGER :: N_COARSE  = NSCARC_ZERO_INT                       !< Number of coarse cells (AMG only)
   INTEGER :: N_ZONES   = NSCARC_ZERO_INT                       !< Number of zones (AMG only)

   INTEGER :: N_STENCIL_MAX  = 25                              !< Max stencil size (AMG only)

   ! Pointer variables and arrays for data exchange with neighbors
   INTEGER :: ICG  = NSCARC_ZERO_INT                           !< Ghost cell pointer for first layer
   INTEGER :: ICG2 = NSCARC_ZERO_INT                           !< Ghost cell pointer for second layer
   INTEGER :: ICE  = NSCARC_ZERO_INT                           !< Ghost cell pointer for extended cells

   INTEGER :: NLEN_BUFFER_LAYER1  = NSCARC_ZERO_INT            !< Length for single layer length exchange on that level
   INTEGER :: NLEN_BUFFER_LAYER2  = NSCARC_ZERO_INT            !< Length for double layer length exchange on that level
   INTEGER :: NLEN_BUFFER_LAYER4  = NSCARC_ZERO_INT            !< Length for fourfold length exchange on that level
   INTEGER :: NLEN_BUFFER_STENCIL = NSCARC_ZERO_INT            !< Length for stencil layer length exchange on that level
   INTEGER :: NLEN_BUFFER_FULL    = NSCARC_ZERO_INT            !< Length for full length exchange on that level

END TYPE SCARC_GRID_TYPE


!> \brief McKenney-Greengard-Mayo method - still experimental
  
TYPE SCARC_MGM_TYPE

   REAL(EB), ALLOCATABLE, DIMENSION (:,:,:) :: H1, H2, H3, H4, H5, H6, H7    !< Pressure vectors of different parts
   REAL(EB), ALLOCATABLE, DIMENSION (:,:,:) :: HS, HU, HD          !< Structured and unstructured solution and their difference
   REAL(EB), ALLOCATABLE, DIMENSION (:)     :: OU3, OV3, OW3       !< Velocity components along external boundaries
   REAL(EB), ALLOCATABLE, DIMENSION (:)     :: OH1, OH2, OH3       !< Other mesh H3 vector
   REAL(EB), ALLOCATABLE, DIMENSION (:)     :: BC                  !< Boundary conditions along internal mesh interfaces
   REAL(EB), ALLOCATABLE, DIMENSION (:)     :: X, Y, B             !< RHS and solution vectors of LU (experimental)
   REAL(EB), ALLOCATABLE, DIMENSION (:,:)   :: ASQ                 !< Matrix for LU-decomposition (experimental)
   REAL(EB), ALLOCATABLE, DIMENSION (:,:)   :: LSQ                 !< Lower part of LU-decomposition (experimental)
   REAL(EB), ALLOCATABLE, DIMENSION (:,:)   :: USQ                 !< Upper part of LU-decomposition (experimental)
   REAL(EB), ALLOCATABLE, DIMENSION (:)     :: WEIGHT              !< Scaling weights for true boundary setting

   REAL(EB), ALLOCATABLE, DIMENSION (:,:,:) :: UU, VV, WW          !< Velocity vectors predictor
   REAL(EB), ALLOCATABLE, DIMENSION (:,:,:) :: U1, V1, W1          !< Velocity vectors predictor
   REAL(EB), ALLOCATABLE, DIMENSION (:,:,:) :: U2, V2, W2          !< Velocity vectors predictor
   REAL(EB), ALLOCATABLE, DIMENSION (:,:,:) :: U3, V3, W3          !< Velocity vectors predictor

   REAL(EB)::  CAPPA_POISSON = 0.0_EB                              !< Convergence rate of Poisson solution
   REAL(EB)::  CAPPA_LAPLACE = 0.0_EB                              !< Max convergence rate of Laplace solutions

   REAL(EB)::  VELOCITY_ERROR = 0.0_EB                             !< Velocity error in single Laplace iteration
   REAL(EB)::  VELOCITY_ERROR_MAX = 0.0_EB                         !< Maximum achieved velocity error over all Laplace iterations
   REAL(EB)::  VELOCITY_TOLERANCE = 1.E-4_EB                       !< Requested velocity tolerance for MGM Laplace problems

   INTEGER, ALLOCATABLE, DIMENSION (:,:) :: BTYPE                  !< Boundary type of an interface cell

   INTEGER :: NCS, NCU                                             !< Number of cells for structured/unstructured grid
   INTEGER :: NW1, NW2, NWI, NWE                                   !< Range of IW's with non-zero B-values

   INTEGER :: ITE = 0                            
   INTEGER :: ITE_POISSON = 0
   INTEGER :: ITE_LAPLACE = 0

END TYPE SCARC_MGM_TYPE

!> \brief Collection of grid level related information on single mesh
  
TYPE SCARC_LEVEL_TYPE

   ! Administrative structures for different components based on given grid level
   TYPE (SCARC_FACE_TYPE), ALLOCATABLE, DIMENSION(:) :: FACE   !< Face information
   TYPE (SCARC_OBST_TYPE), ALLOCATABLE, DIMENSION(:) :: OBST   !< Obstruction information
   TYPE (SCARC_STAGE_TYPE), DIMENSION(2) :: STAGE              !< Hierarchy of solvers and related working vectors
   TYPE (SCARC_GRID_TYPE)      :: STRUCTURED, UNSTRUCTURED     !< Structured and unstructured grid information
   TYPE (SCARC_FFT_TYPE)       :: FFT                          !< FFT preconditioner based on CRAYFISHPAK
   TYPE (SCARC_MGM_TYPE)       :: MGM                          !< McKenney-Greengard-Mayo method 
   TYPE (SCARC_MULTIGRID_TYPE) :: MG                           !< Multigrid method information
!#ifdef WITH_SCARC_MKL
   TYPE (SCARC_MKL_TYPE)       :: MKL                          !< MKL preconditioner based on Intel MKL
#ifdef WITH_SCARC_POSTPROCESSING
   TYPE (SCARC_PRESSURE_TYPE)  :: PRESSURE                     !< Postprocessing of pressure information
#endif
!#endif

   ! Coordinate information
   REAL(EB), ALLOCATABLE, DIMENSION (:) :: XCOR, YCOR, ZCOR    !< Coordinate vectors in x-, y- and z-direction
   REAL(EB), ALLOCATABLE, DIMENSION (:) :: XMID, YMID, ZMID    !< Midpoint vectors in x-, y- and z-direction
   REAL(EB), ALLOCATABLE, DIMENSION (:) :: DXL, DYL, DZL       !< Step size vectors in x-, y- and z-direction
   REAL(EB) :: DX , DY , DZ                                    !< Step sizes in x-, y- and z-direction
   REAL(EB) :: DX2, DY2, DZ2                                   !< Half step sizes in x-, y- and z-direction
   REAL(EB) :: DXH, DYH, DZH                                   !< Half step sizes in x-, y- and z-direction
   REAL(EB) :: DXI, DYI, DZI                                   !< Inversed of step sizes in x-, y- and z-direction
   REAL(EB) :: DXI2, DYI2, DZI2                                !< Squared and inversed step sizes in x-, y- and z-direction

   ! Cell and wall index information:
   !    - on the finest level, the original arrays from FDS are used
   !    - separate arrays will only be allocated for coarser levels
   !    - to address them on all levels, corresponding pointers are used
   INTEGER, POINTER, DIMENSION (:,:,:)     :: CELL_INDEX_PTR   !< Pointer to cell index array
   INTEGER, POINTER, DIMENSION (:,:)       :: WALL_INDEX_PTR   !< Pointer to wall index array
   INTEGER, ALLOCATABLE, DIMENSION (:,:,:) :: CELL_INDEX       !< Cell index list (only allocated for coarser levels)
   INTEGER, ALLOCATABLE, DIMENSION (:,:)   :: WALL_INDEX       !< Wall index list (only allocated for coarser levels)

   LOGICAL, ALLOCATABLE, DIMENSION (:,:,:) :: IS_SOLID         !< State of single cells (.TRUE. if solid/.FALSE. otherwise)

   ! Orientation and different cell related lengths
   INTEGER :: GHOST_FIRSTW(-3:3) = 0                           !< First internal ghost cell numbers for all faces
   INTEGER :: GHOST_LASTW(-3:3)  = 0                           !< Last internal ghost cell numbers for all faces
   INTEGER :: GHOST_FIRSTE(-3:3) = 0                           !< First external ghost cell numbers for all faces
   INTEGER :: GHOST_LASTE(-3:3)  = 0                           !< Last external ghost cell numbers for all faces
   INTEGER :: NX = 0                                           !< Number of grid cells in x-direction
   INTEGER :: NY = 0                                           !< Number of grid cells in y-direction
   INTEGER :: NZ = 0                                           !< Number of grid cells in z-direction

   ! Number of discretizations and obstructions
   INTEGER :: N_DISCRET = 0                                    !< Number of discretization types used
   INTEGER :: N_OBST = 0                                       !< Number of obstructions
   INTEGER :: N_CELL_INDEX = 0                                 !< Number of entries in CELL_INDEX array
   INTEGER :: N_CELLS = 0                                      !< Number of cells in structured discretization
   INTEGER :: N_GHOST_ZONES = 0                                !< Number of adjacent ghost zones
   INTEGER :: N_LAYER2 = 0                                     !< Number of cells in second layer to neighbor (AMG only)
   INTEGER :: N_LAYER2_TOTAL = 0                               !< Total number of cells in second layer (AMG only) 

   ! Different wall related lengths
   INTEGER :: N_WALL_CELLS = 0                                 !< Number of wall cells
   INTEGER :: N_WALL_CELLS_EXT = 0                             !< Number of external wall cells
   INTEGER :: N_WALL_CELLS_INT = 0                             !< Number of internal wall cells
   INTEGER :: N_WALL_CELLS_LOCAL = 0                           !< Number of local wall cells

   INTEGER :: L2PTR = 0                                        !< Pointer to current second layer cell

END TYPE SCARC_LEVEL_TYPE

!> \brief Sample sequence of used solvers in stack
  
TYPE SCARC_SOLVER_TYPE

   CHARACTER(30) :: CNAME = 'NONE'                             !< Name of current solver

   ! Types of different solver components
   INTEGER :: TYPE_ACCURACY      = NSCARC_ACCURACY_ABSOLUTE    !< Type of requested accuracy
   INTEGER :: TYPE_COARSE        = NSCARC_COARSE_DIRECT        !< Type of coarse grid solver for multilevel methods
   INTEGER :: TYPE_COARSENING    = NSCARC_COARSENING_CUBIC     !< Type of grid coarsening 
   INTEGER :: TYPE_CYCLING       = NSCARC_CYCLING_V            !< Type of cycling for multigrid method
   INTEGER :: TYPE_GRID          = NSCARC_GRID_STRUCTURED      !< Type of discretization
   INTEGER :: TYPE_EXCHANGE      = NSCARC_UNDEF_INT            !< Type of data exchange
   INTEGER :: TYPE_INTERPOL      = NSCARC_INTERPOL_CONSTANT    !< Type of interpolation method
   INTEGER :: TYPE_LEVEL(0:2)    = NSCARC_UNDEF_INT            !< Type of levels
   INTEGER :: TYPE_MATRIX        = NSCARC_MATRIX_COMPACT       !< Type of storage for matrix
   INTEGER :: TYPE_METHOD        = NSCARC_METHOD_KRYLOV        !< Type of ScaRC method
   INTEGER :: TYPE_MKL(0:10)     = NSCARC_UNDEF_INT            !< Type of MKL for single levels
   INTEGER :: TYPE_MKL_PRECISION = NSCARC_PRECISION_DOUBLE     !< Type of precision for MKL solver
   INTEGER :: TYPE_MULTIGRID     = NSCARC_MULTIGRID_GEOMETRIC  !< Type of multigrid method
   INTEGER :: TYPE_PARENT        = NSCARC_UNDEF_INT            !< Type of parent (calling) solver
   INTEGER :: TYPE_PRECON        = NSCARC_UNDEF_INT            !< Type of preconditioner for iterative solver
   INTEGER :: TYPE_RELAX         = NSCARC_UNDEF_INT            !< Type of preconditioner for iterative solver
   INTEGER :: TYPE_SCOPE(0:2)    = NSCARC_SCOPE_GLOBAL         !< Type of solver scopes
   INTEGER :: TYPE_SMOOTH        = NSCARC_UNDEF_INT            !< Type of smoother for multigrid method
   INTEGER :: TYPE_SOLVER        = NSCARC_SOLVER_MAIN          !< Type of surrounding solver stage
   INTEGER :: TYPE_STAGE         = NSCARC_STAGE_ONE            !< Type of surrounding solver stage
   INTEGER :: TYPE_STENCIL       = NSCARC_STENCIL_CONSTANT     !< Type of storage for matrix
   INTEGER :: TYPE_TWOLEVEL      = NSCARC_TWOLEVEL_NONE        !< Type of two-level method
   INTEGER :: TYPE_VECTOR        = NSCARC_UNDEF_INT            !< Type of vector to point to


   ! References to different vectors which are needed for the current solver
   INTEGER :: X = NSCARC_UNDEF_INT                             !< Reference to local X-vector, double precision
   INTEGER :: B = NSCARC_UNDEF_INT                             !< Reference to local B-vector, double precision
   INTEGER :: D = NSCARC_UNDEF_INT                             !< Reference to local D-vector, double precision
   INTEGER :: R = NSCARC_UNDEF_INT                             !< Reference to local R-vector, double precision
   INTEGER :: V = NSCARC_UNDEF_INT                             !< Reference to local V-vector, double precision
   INTEGER :: Y = NSCARC_UNDEF_INT                             !< Reference to local Y-vector, double precision
   INTEGER :: Z = NSCARC_UNDEF_INT                             !< Reference to local Z-vector, double precision

   INTEGER :: X_FB = NSCARC_UNDEF_INT                          !< Reference to local X-vector, single precision
   INTEGER :: B_FB = NSCARC_UNDEF_INT                          !< Reference to local B-vector, single precision
   INTEGER :: R_FB = NSCARC_UNDEF_INT                          !< Reference to local R-vector, single precision
   INTEGER :: V_FB = NSCARC_UNDEF_INT                          !< Reference to local V-vector, single precision

#ifdef WITH_SCARC_DEBUG
   INTEGER :: E = NSCARC_UNDEF_INT                             !< Reference to local E-vector, double precision
#endif

   ! Converegence requirements for current solver
   INTEGER  :: NIT   = NSCARC_UNDEF_INT                        !< Maximum iteration number
   INTEGER  :: ITE   = NSCARC_UNDEF_INT                        !< Current iteration number
   REAL(EB) :: EPS   = NSCARC_UNDEF_REAL_EB                    !< Required accuracy
   REAL(EB) :: RES   = NSCARC_UNDEF_REAL_EB                    !< Current residual
   REAL(EB) :: RESIN = NSCARC_UNDEF_REAL_EB                    !< Initial residual
   REAL(EB) :: ERR   = NSCARC_UNDEF_REAL_EB                    !< Initial residual
   REAL(EB) :: OMEGA = NSCARC_UNDEF_REAL_EB                    !< Relaxation parameter
   REAL(EB) :: CAPPA = NSCARC_UNDEF_REAL_EB                    !< Convergence rate

END TYPE SCARC_SOLVER_TYPE

!> \brief Stack type
  
TYPE SCARC_STACK_TYPE
   TYPE (SCARC_SOLVER_TYPE), POINTER :: SOLVER                     !< Type of current solver
   INTEGER :: NSTAGE                                               !< Stage of current solver
END TYPE SCARC_STACK_TYPE

!> \brief Administration other mesh data needed for the coupling of adjacent neighbors
  
TYPE SCARC_OSCARC_TYPE

   TYPE (SCARC_LEVEL_TYPE), ALLOCATABLE, DIMENSION(:) :: LEVEL     !< Level related information

   REAL(EB) :: SEND_BUFFER_REAL0(1:NSCARC_MAX_BUFFER0)             !< Constant length send buffer for setup
   REAL(EB) :: RECV_BUFFER_REAL0(1:NSCARC_MAX_BUFFER0)             !< Constant length receive buffer for setup
   REAL(EB), ALLOCATABLE, DIMENSION (:) :: SEND_BUFFER_REAL        !< Real send buffer 
   REAL(EB), ALLOCATABLE, DIMENSION (:) :: RECV_BUFFER_REAL        !< Real receive buffer 

   INTEGER :: SEND_BUFFER_INT0(NSCARC_MAX_BUFFER0)                 !< Constant length send buffer for setup
   INTEGER :: RECV_BUFFER_INT0(NSCARC_MAX_BUFFER0)                 !< Constant length receive buffer for setup
   INTEGER,  ALLOCATABLE, DIMENSION (:) :: SEND_BUFFER_INT         !< Integer send buffer 
   INTEGER,  ALLOCATABLE, DIMENSION (:) :: RECV_BUFFER_INT         !< Integer receive buffer 

   INTEGER :: NLEN_MAX_BUFFER_LAYER1  = NSCARC_HUGE_INT            !< Length for single layer length exchange
   INTEGER :: NLEN_MAX_BUFFER_LAYER2  = NSCARC_HUGE_INT            !< Length for double layer length exchange
   INTEGER :: NLEN_MAX_BUFFER_LAYER4  = NSCARC_HUGE_INT            !< Length for double layer length exchange
   INTEGER :: NLEN_MAX_BUFFER_STENCIL = NSCARC_HUGE_INT            !< Length for stencil layer length exchange
   INTEGER :: NLEN_MAX_BUFFER_FULL    = NSCARC_HUGE_INT            !< Length for max length exchange

END TYPE SCARC_OSCARC_TYPE

!> \brief Measurement of CPU times
  
TYPE SCARC_CPU_TYPE
   REAL(EB) :: BUFFER_PACKING   = 0.0_EB                           !< Time for data exchange
   REAL(EB) :: BUFFER_UNPACKING = 0.0_EB                           !< Time for data exchange
   REAL(EB) :: AMG              = 0.0_EB                           !< Time for algebraic multigrid solver
   REAL(EB) :: COARSE           = 0.0_EB                           !< Time for coarse grid solver
   REAL(EB) :: EXCHANGE         = 0.0_EB                           !< Time for data exchange
   REAL(EB) :: ITERATION        = 0.0_EB                           !< Time for Krylov solver
   REAL(EB) :: L2NORM           = 0.0_EB                           !< Time for l2-norm
   REAL(EB) :: MATVEC_PRODUCT   = 0.0_EB                           !< Time for matrix vector multiplication
   REAL(EB) :: OVERALL          = 0.0_EB                           !< Complete time for ScaRC
   REAL(EB) :: RELAXATION       = 0.0_EB                           !< Time for relaxation
   REAL(EB) :: SCALAR_PRODUCT   = 0.0_EB                           !< Time for scalar product
   REAL(EB) :: SETUP            = 0.0_EB                           !< Time for setup of requested ScaRC solver
   REAL(EB) :: SMOOTHER         = 0.0_EB                           !< Time for smoothing
   REAL(EB) :: SOLVER           = 0.0_EB                           !< Time for solver 
   INTEGER  :: N_TIMER          = 13                               !< Total number of timers
END TYPE SCARC_CPU_TYPE

!> \brief Basic administration type for ScaRC-method
 
TYPE SCARC_TYPE

   TYPE (SCARC_OSCARC_TYPE), ALLOCATABLE, DIMENSION(:) :: OSCARC   !< ScaRC type on other mesh
   TYPE (SCARC_LEVEL_TYPE) , ALLOCATABLE, DIMENSION(:) :: LEVEL    !< Level related information

   REAL(EB) :: XS, XF, YS, YF, ZS, ZF                              !< x-, y- and z-bounds of grid
   REAL(EB) :: RHS_END = 0.0_EB                                    !< Very last RHS entry, needed for matrix condensing

   INTEGER, ALLOCATABLE, DIMENSION(:) :: NEIGHBORS                 !< List of adjacent neighbors of whole mesh
   INTEGER :: N_NEIGHBORS = 0                                      !< Number of adjacent neighbors of whole mesh
   INTEGER :: NC = 0                                               !< Total number of cells on that mesh
   INTEGER :: IBAR, JBAR, KBAR                                     !< Number of cells (corresponding to main prg)

END TYPE SCARC_TYPE

END MODULE SCARC_TYPES


