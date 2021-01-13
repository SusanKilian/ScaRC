!//////////////////////////////////////////////////////////////////////////////////////////////////////
!
! Module SCARC_MKL:
!
!> \brief Setup environment necessary for the call of the IntelMKL local and global LU-solvers
!   PARDISO and CLUSTER_SPARSE_SOLVER
!
!//////////////////////////////////////////////////////////////////////////////////////////////////////
MODULE SCARC_MKL
  
USE GLOBAL_CONSTANTS
USE PRECISION_PARAMETERS, ONLY: EB, FB
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
USE COMP_FUNCTIONS, ONLY: CURRENT_TIME
USE MPI
USE MKL_PARDISO
USE MKL_CLUSTER_SPARSE_SOLVER
USE SCARC_CONSTANTS
USE SCARC_VARIABLES
USE SCARC_MESSAGES
USE SCARC_ERRORS, ONLY: SCARC_SHUTDOWN
USE SCARC_STACK, ONLY: STACK, SCARC_SETUP_STACK, SCARC_SETUP_REFERENCES
USE SCARC_STORAGE, ONLY: SCARC_ALLOCATE_INT1, SCARC_ALLOCATE_INT2, SCARC_ALLOCATE_INT3

IMPLICIT NONE

CONTAINS


! ----------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize vectors for LU-solvers (based on MKL)
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MKL_ENVIRONMENT(NSTACK)
INTEGER, INTENT(INOUT) :: NSTACK

NSTACK = NSCARC_STACK_ROOT
STACK(NSTACK)%SOLVER => MAIN_LU

CALL SCARC_SETUP_MKL(NSCARC_SOLVER_MAIN, NSCARC_SCOPE_GLOBAL, NSCARC_STAGE_ONE, NSTACK, NLEVEL_MIN, NLEVEL_MIN)

! In the multi-mesh case use CLUSTER_SPARSE_SOLVER, else PARDISO solver (only on finest grid level)

IF (NMESHES > 1) THEN 
   CALL SCARC_SETUP_CLUSTER(NLEVEL_MIN, NLEVEL_MIN)
ELSE 
   CALL SCARC_SETUP_PARDISO(NLEVEL_MIN, NLEVEL_MIN)
ENDIF

END SUBROUTINE SCARC_SETUP_MKL_ENVIRONMENT

! ----------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize vectors for LU-solvers (based on MKL)
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MKL(NSOLVER, NSCOPE, NSTAGE, NSTACK, NLMIN, NLMAX)
USE SCARC_POINTERS, ONLY: SV
INTEGER, INTENT(IN) :: NSOLVER, NSCOPE, NSTAGE, NSTACK, NLMIN, NLMAX

 
! Basic setup of stack information and types for MKL
 
CALL SCARC_SETUP_STACK(NSTACK)

SV  => STACK(NSTACK)%SOLVER
SELECT CASE (NSOLVER)
   CASE (NSCARC_SOLVER_MAIN)
      SV%CNAME = 'SCARC_MAIN_LU'
   CASE (NSCARC_SOLVER_COARSE)
      SV%CNAME = 'SCARC_COARSE_LU'
   CASE DEFAULT
      CALL SCARC_SHUTDOWN(NSCARC_ERROR_PARSE_INPUT, SCARC_NONE, NSOLVER)
END SELECT

! Reset types for LU-decomposition method
 
SV%TYPE_METHOD        = NSCARC_METHOD_LU
SV%TYPE_SOLVER        = NSOLVER
SV%TYPE_SCOPE(0)      = NSCOPE
SV%TYPE_STAGE         = NSTAGE
SV%TYPE_GRID          = TYPE_GRID
SV%TYPE_LEVEL(0)      = NLMAX-NLMIN+1
SV%TYPE_LEVEL(1)      = NLMIN
SV%TYPE_LEVEL(2)      = NLMAX
SV%TYPE_MATRIX        = NSCARC_MATRIX_COMPACT    
SV%TYPE_MKL_PRECISION = TYPE_MKL_PRECISION

 
! Point to solution vectors (in corresponding scope)
 
CALL SCARC_SETUP_REFERENCES(.TRUE.,.TRUE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.TRUE., NSTACK)

END SUBROUTINE SCARC_SETUP_MKL

! ------------------------------------------------------------------------------------------------
!> \brief Initialize CLUSTER_SPARSE_SOLVER from MKL-library
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_CLUSTER(NLMIN, NLMAX)
USE SCARC_POINTERS, ONLY: L, G, AS, MKL, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NLMIN, NLMAX
INTEGER :: NM, NL, I 
REAL (EB) :: TNOW
REAL (EB) :: DUMMY(1)=0.0_EB
REAL (FB) :: DUMMY_FB(1)=0.0_FB

TNOW = CURRENT_TIME()
CROUTINE = 'SCARC_SETUP_CLUSTER'

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   LEVEL_LOOP: DO NL = NLMIN, NLMAX

      CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

      MKL => L%MKL
      AS  => G%POISSON_SYM

      ! Allocate workspace for parameters and pointers needed in MKL-routine
 
      CALL SCARC_ALLOCATE_INT1(MKL%IPARM, 1, 64, NSCARC_INIT_ZERO, 'MKL%IPARM', CROUTINE)

      IF (.NOT.ALLOCATED(MKL%CT)) THEN
         ALLOCATE(MKL%CT(64), STAT=IERROR)
         CALL CHKMEMERR ('SCARC', 'CT', IERROR)
         DO I=1,64
            MKL%CT(I)%DUMMY = 0
         ENDDO
      ENDIF

      ! Define corresponding parameters
      ! Note: IPARM-vectory is allocate from 1:64, not from 0:63
 
      MKL%NRHS   =  1         ! one right hand side
      MKL%MAXFCT =  1         ! one matrix
      MKL%MNUM   =  1         ! number of matrix to be factorized
      MKL%ERROR  =  0         ! initialize error flag
      MKL%MSGLVL =  0         ! do not print statistical information
      IF (SCARC_MKL_MTYPE == 'SYMMETRIC') THEN
         MKL%MTYPE  = -2      ! Matrix type real and symmetric indefinite
      ELSE
         MKL%MTYPE  = 11      ! Matrix type real and non-symmetric
      ENDIF
      MKL%IPARM(1)  =  1      ! no solver default
      IF (N_MPI_PROCESSES > 4) THEN 
         MKL%IPARM(2) =10     ! 10 = MPI Parallel fill-in reordering from METIS. If 3 = OpenMP parallel reordering in Master
      ELSE                    ! Note IPARM(2)=10 has a bug which has been fixed from Intel MKL 2018 update 2 onwards.
         MKL%IPARM(2) = 3
      ENDIF
      MKL%IPARM(4)  = 0       ! no iterative-direct algorithm
      MKL%IPARM(5)  = 0       ! no user fill-in reducing permutation
      MKL%IPARM(6)  = 0       ! =0 solution on the first n components of x
      MKL%IPARM(8)  = 2       ! numbers of iterative refinement steps
      MKL%IPARM(10) = 13      ! perturb the pivot elements with 1E-13
      MKL%IPARM(11) = 1       ! use nonsymmetric permutation and scaling MPS  !!!!! was 1
      MKL%IPARM(13) = 1       ! maximum weighted matching algorithm is switched-off
                              !(default for symmetric). Try iparm(13) = 1 in case of inappropriate accuracy
      MKL%IPARM(14) = 0       ! Output: number of perturbed pivots
      MKL%IPARM(18) = 0       ! Output: number of nonzeros in the factor LU
      MKL%IPARM(19) = 0       ! Output: Mflops for LU factorization
      MKL%IPARM(20) = 0       ! Output: Numbers of CG Iterations
      MKL%IPARM(21) = 1       ! 1x1 diagonal pivoting for symmetric indefinite matrices.
      MKL%IPARM(24) = 0
      MKL%IPARM(27) = 1       ! Check matrix
      MKL%IPARM(40) = 2       ! Matrix, solution and rhs provided in distributed assembled matrix input format.

      MKL%IPARM(41) = G%NC_OFFSET(NM) + 1                      ! first global cell number for mesh NM
      MKL%IPARM(42) = G%NC_OFFSET(NM) + G%NC_LOCAL(NM)         ! last global cell number for mesh NM
      !MKL%IPARM(39) = 2                                       ! provide matrix in distributed format
      !MKL%IPARM(40) = G%NC_OFFSET(NM)+1                       ! first global cell number for mesh NM
      !MKL%IPARM(41) = G%NC_OFFSET(NM)+G%NC_LOCAL(NM)         ! last global cell number for mesh NM
 
      ! First perform only reordering and symbolic factorization
      ! Then perform only factorization
 
      IF (TYPE_MKL_PRECISION == NSCARC_PRECISION_SINGLE) THEN

         MKL%IPARM(28) = 1         ! single precision
         MKL%PHASE = 11
         CALL CLUSTER_SPARSE_SOLVER_S(MKL%CT, MKL%MAXFCT, MKL%MNUM, MKL%MTYPE, MKL%PHASE, G%NC_GLOBAL, &
                                      AS%VAL_FB, AS%ROW, AS%COL, MKL%PERM, MKL%NRHS, MKL%IPARM, &
                                      MKL%MSGLVL, DUMMY_FB, DUMMY_FB, MPI_COMM_WORLD, MKL%ERROR)
         MKL%PHASE = 22
         CALL CLUSTER_SPARSE_SOLVER_S(MKL%CT, MKL%MAXFCT, MKL%MNUM, MKL%MTYPE, MKL%PHASE, G%NC_GLOBAL, &
                                      AS%VAL_FB, AS%ROW, AS%COL, MKL%PERM, MKL%NRHS, MKL%IPARM, &
                                      MKL%MSGLVL, DUMMY_FB, DUMMY_FB, MPI_COMM_WORLD, MKL%ERROR)

         IF (MKL%ERROR /= 0) THEN
            WRITE(*,*) 'ERROR in MKL SETUP, MKL%ERROR=', MKL%ERROR
            CALL MPI_FINALIZE(IERROR)
            STOP
         ENDIF

      ELSE

         MKL%IPARM(28) = 0         ! double precision
         MKL%PHASE = 11
         CALL CLUSTER_SPARSE_SOLVER_D(MKL%CT, MKL%MAXFCT, MKL%MNUM, MKL%MTYPE, MKL%PHASE, G%NC_GLOBAL, &
                                      AS%VAL, AS%ROW, AS%COL, MKL%PERM, MKL%NRHS, MKL%IPARM, &
                                      MKL%MSGLVL, DUMMY, DUMMY, MPI_COMM_WORLD, MKL%ERROR)
         MKL%PHASE = 22
         CALL CLUSTER_SPARSE_SOLVER_D(MKL%CT, MKL%MAXFCT, MKL%MNUM, MKL%MTYPE, MKL%PHASE, G%NC_GLOBAL, &
                                      AS%VAL, AS%ROW, AS%COL, MKL%PERM, MKL%NRHS, MKL%IPARM, &
                                      MKL%MSGLVL, DUMMY, DUMMY, MPI_COMM_WORLD, MKL%ERROR)
         IF (MKL%ERROR /= 0) THEN
            WRITE(*,*) 'ERROR in MKL SETUP, MKL%ERROR=', MKL%ERROR
            CALL MPI_FINALIZE(IERROR)
            STOP
         ENDIF

      ENDIF

   ENDDO LEVEL_LOOP
ENDDO MESHES_LOOP

END SUBROUTINE SCARC_SETUP_CLUSTER


! ------------------------------------------------------------------------------------------------
!> \brief Initialize PARDISO solver from MKL-library
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_PARDISO(NLMIN, NLMAX)
USE SCARC_POINTERS, ONLY: L, G, AS, MKL, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NLMIN, NLMAX
INTEGER :: NM, NL, I, IDUMMY(1)=0
REAL (EB) :: TNOW
REAL (EB) :: DUMMY(1)=0.0_EB
REAL (FB) :: DUMMY_FB(1)=0.0_FB

TNOW = CURRENT_TIME()
CROUTINE = 'SCARC_SETUP_PARDISO'

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   LEVEL_LOOP: DO NL = NLMIN, NLMAX

      CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
      MKL => L%MKL
      AS  => G%POISSON_SYM

      ! Allocate workspace for parameters nnd pointers eeded in MKL-routine
 
      CALL SCARC_ALLOCATE_INT1(MKL%IPARM, 1, 64, NSCARC_INIT_ZERO, 'MKL%IPARM', CROUTINE)

      IF (.NOT.ALLOCATED(MKL%PT)) THEN
         ALLOCATE(MKL%PT(64), STAT=IERROR)
         CALL CHKMEMERR ('SCARC', 'PT', IERROR)
         DO I=1,64
            MKL%PT(I)%DUMMY = 0
         ENDDO
      ENDIF

      ! Define corresponding parameters
      ! Note: IPARM-vectory is allocate from 1:64, not from 0:63
 
      MKL%NRHS   = 1
      MKL%MAXFCT = 1
      MKL%MNUM   = 1

      MKL%IPARM(1)  =  1      ! no solver default
      MKL%IPARM(4)  =  0      ! factorization computed as required by phase
      MKL%IPARM(5)  =  0      ! user permutation ignored
      MKL%IPARM(6)  =  0      ! write solution on x
      MKL%IPARM(8)  =  2      ! numbers of iterative refinement steps
      MKL%IPARM(10) = 13      ! perturb the pivot elements with 1E-13
      MKL%IPARM(11) =  0      ! disable scaling (default for SPD)
      MKL%IPARM(13) =  0      ! disable matching
      MKL%IPARM(18) = -1      ! Output: number of nonzeros in the factor LU
      MKL%IPARM(19) = -1      ! Output: number of floating points operations
      MKL%IPARM(20) =  1      ! Output: Numbers of CG Iterations
      MKL%IPARM(27) =  1      ! use matrix checker
      MKL%IPARM(37) =  0      ! matrix storage in COMPACT-format
      MKL%IPARM(40) = 2       ! Matrix, solution and rhs provided in distributed assembled matrix input format.

      MKL%ERROR  =  0         ! initialize error flag
      MKL%MSGLVL =  0         ! do not print statistical information
      MKL%MTYPE  = -2         ! Matrix type real non-symmetric

      ! First perform only reordering and symbolic factorization
      ! Then perform only factorization
 

      IF (TYPE_MKL_PRECISION == NSCARC_PRECISION_SINGLE) THEN
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SETUP_PARDISO SINGLE: G%NC=',G%NC
CALL SCARC_DEBUG_CMATRIX(AS, 'AS','PARDISO SETUP')
#endif
         MKL%IPARM(28) = 1         ! single precision
         MKL%PHASE = 11
         CALL PARDISO_S(MKL%PT, MKL%MAXFCT, MKL%MNUM, MKL%MTYPE, MKL%PHASE, G%NC, &
                        AS%VAL_FB, AS%ROW, AS%COL, IDUMMY, &
                        MKL%NRHS, MKL%IPARM, MKL%MSGLVL, DUMMY_FB, DUMMY_FB, MKL%ERROR)
         MKL%PHASE = 22
         CALL PARDISO_S(MKL%PT, MKL%MAXFCT, MKL%MNUM, MKL%MTYPE, MKL%PHASE, G%NC, &
                        AS%VAL_FB, AS%ROW, AS%COL, IDUMMY, &
                        MKL%NRHS, MKL%IPARM, MKL%MSGLVL, DUMMY_FB, DUMMY_FB, MKL%ERROR)
      ELSE
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SETUP_PARDISO DOUBLE: G%NC=',G%NC
CALL SCARC_DEBUG_CMATRIX(AS, 'AS','PARDISO SETUP')
#endif
         MKL%IPARM(28) = 0         ! double precision
         MKL%PHASE = 11
         CALL PARDISO_D(MKL%PT, MKL%MAXFCT, MKL%MNUM, MKL%MTYPE, MKL%PHASE, G%NC, &
                        AS%VAL, AS%ROW, AS%COL, IDUMMY, &
                        MKL%NRHS, MKL%IPARM, MKL%MSGLVL, DUMMY, DUMMY, MKL%ERROR)
         MKL%PHASE = 22
         CALL PARDISO_D(MKL%PT, MKL%MAXFCT, MKL%MNUM, MKL%MTYPE, MKL%PHASE, G%NC, &
                        AS%VAL, AS%ROW, AS%COL, IDUMMY, &
                        MKL%NRHS, MKL%IPARM, MKL%MSGLVL, DUMMY, DUMMY, MKL%ERROR)
      ENDIF

   ENDDO LEVEL_LOOP
ENDDO MESHES_LOOP

END SUBROUTINE SCARC_SETUP_PARDISO

END MODULE SCARC_MKL


