!=======================================================================================================================
!
! MODULE SCARC_TROUBLESHOOTING
!
!> \brief Organize program shutdown in case of an error based on corresponding error messages
!
!=======================================================================================================================
MODULE SCARC_TROUBLESHOOTING

USE GLOBAL_CONSTANTS
USE PRECISION_PARAMETERS, ONLY: EB, FB
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
USE SCARC_CONSTANTS
USE SCARC_VARIABLES, ONLY: MSG

IMPLICIT NONE

CONTAINS

! --------------------------------------------------------------------------------------------------------------
!> \brief Shutdown ScaRC with error message
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_ERROR(NERROR, CPARAM, NPARAM)
CHARACTER(*), INTENT(IN) :: CPARAM
INTEGER, INTENT(IN) :: NERROR, NPARAM
CHARACTER(80) :: CERROR

! Assign error message according to specified error

SELECT CASE (NERROR)
   CASE (NSCARC_ERROR_BOUNDARY_SUM)
      CERROR = 'Wrong boundary sum for IOR'
   CASE (NSCARC_ERROR_BOUNDARY_TYPE)
      CERROR = 'Wrong boundary type'
   CASE (NSCARC_ERROR_DIRECT_NOMKL)
      CERROR = 'Direct coarse grid solver is only working in combination with MKL'
   CASE (NSCARC_ERROR_EXCHANGE_RECV)
      CERROR = 'Wrong receive exchange structure'
   CASE (NSCARC_ERROR_EXCHANGE_DIAG)
      CERROR = 'Unknown type of diagonal data exchange'
   CASE (NSCARC_ERROR_EXCHANGE_SEND)
      CERROR = 'Wrong send exchange structure'
   CASE (NSCARC_ERROR_GRID_INDEX)
      CERROR = 'Wrong index for J'
   CASE (NSCARC_ERROR_GRID_NUMBER)
      CERROR = 'Number not divisable by 2'
   CASE (NSCARC_ERROR_GRID_NUMBERX)
      CERROR = 'Number of cells not divisable by 2 in x-direction, NC'
   CASE (NSCARC_ERROR_GRID_NUMBERY)
      CERROR = 'Number of cells not divisable by 2 in y-direction, NC'
   CASE (NSCARC_ERROR_GRID_NUMBERZ)
      CERROR = 'Number of cells not divisable by 2 in z-direction, NC'
   CASE (NSCARC_ERROR_GRID_RESOLUTION)
      CERROR = 'Wrong grid resolution at IOR'
   CASE (NSCARC_ERROR_NEIGHBOR_NUMBER)
      CERROR = 'More than 20 neighbors along one face not allowed'
   CASE (NSCARC_ERROR_NEIGHBOR_TYPE)
      CERROR = 'Wrong neighbor'
   CASE (NSCARC_ERROR_MATRIX_ALLOCATION)
      CERROR = 'Wrong specifier during allocation or deallocation of  matrix'
   CASE (NSCARC_ERROR_MATRIX_SUBDIAG)
      CERROR = 'Subdiagonal missing for system matrix'
   CASE (NSCARC_ERROR_MATRIX_SYMMETRY)
      CERROR = 'Matrix not symmetric for mesh'
   CASE (NSCARC_ERROR_MATRIX_SETUP)
      CERROR = 'Matrix setup failed for level type'
   CASE (NSCARC_ERROR_MATRIX_SIZE)
      CERROR = 'Matrix reducing failed because new length is too big for matrix'
   CASE (NSCARC_ERROR_MATRIX_COPY)
      CERROR = 'Matrix copy failed due to too already existing array'
   CASE (NSCARC_ERROR_MGM_PERMUTATION)
      CERROR = 'Error in MGM permutation for LU'
   CASE (NSCARC_ERROR_MGM_PARDISO)
      CERROR = 'Error in MGM - PARDISO solver for local Laplace systems not available'
   CASE (NSCARC_ERROR_MKL_CLUSTER)
      CERROR = 'MKL Library compile flag not defined, Cluster_Sparse_Solver not available'
   CASE (NSCARC_ERROR_MKL_INTERNAL)
      CERROR = 'The following MKL error was detected'
   CASE (NSCARC_ERROR_MKL_PARDISO)
      CERROR = 'MKL Library compile flag not defined, PARDISO solver not available'
   CASE (NSCARC_ERROR_MKL_STORAGE)
      CERROR = 'Wrong matrix storage scheme for MKL solvers, only COMPACT storage available'
   CASE (NSCARC_ERROR_MULTIGRID_LEVEL)
      CERROR = 'Wrong level for multigrid method'
   CASE (NSCARC_ERROR_PARSE_INPUT)
      CERROR = 'Wrong input parameter'
   CASE (NSCARC_ERROR_STENCIL)
      CERROR = 'Wrong type for matrix stencil - only constant or variable allowed'
   CASE (NSCARC_ERROR_STACK_SOLVER)
      CERROR = 'Wrong number of solvers in stack'
   CASE (NSCARC_ERROR_STACK_MESSAGE)
      CERROR = 'Too many messages in calling stack'
   CASE (NSCARC_ERROR_VECTOR_LENGTH)
      CERROR = 'Inconsistent length for vector allocation'
END SELECT

! Specify more detailed information if available

IF (CPARAM /= SCARC_NONE) THEN
   IF (MY_RANK == 0) WRITE(LU_ERR,1000)  CERROR, CPARAM, TRIM(CHID)
ELSE IF (NPARAM /= NSCARC_NONE) THEN
   IF (MY_RANK == 0) WRITE(LU_ERR,2000)  CERROR, NPARAM, TRIM(CHID)
ELSE
   IF (MY_RANK == 0) WRITE(LU_ERR,3000)  CERROR, TRIM(CHID)
ENDIF

! Also print verbose message if enabled

#ifdef WITH_SCARC_VERBOSE
WRITE(*,*) 'ERROR: WITH_SCARC_VERBOSE,', NERROR, CPARAM, NPARAM
IF (CPARAM /= SCARC_NONE) THEN
   WRITE(MSG%LU_VERBOSE,1000)  CERROR, CPARAM, TRIM(CHID)
ELSE IF (NPARAM /= NSCARC_NONE) THEN
   WRITE(MSG%LU_VERBOSE,2000)  CERROR, NPARAM, TRIM(CHID)
ELSE
   WRITE(MSG%LU_VERBOSE,3000)  CERROR, TRIM(CHID)
ENDIF
CLOSE(MSG%LU_VERBOSE)
#else
WRITE(*,*) 'ERROR: NOT WITH_SCARC_VERBOSE'
#endif

#ifdef WITH_SCARC_DEBUG
WRITE(*,*) 'ERROR: WITH_SCARC_DEBUG'
CLOSE(MSG%LU_DEBUG)
#else
WRITE(*,*) 'ERROR: NOT WITH_SCARC_DEBUG'
#endif

STOP_STATUS = SETUP_STOP
RETURN

1000 FORMAT('Error in ScaRC-solver: ', A,' : ',   A, ' (CHID: ',A,')' )
2000 FORMAT('Error in ScaRC-solver: ', A,' : ', I12, ' (CHID: ',A,')' )
3000 FORMAT('Error in ScaRC-solver: ', A, ' (CHID: ',A,')' )
END SUBROUTINE SCARC_ERROR

! --------------------------------------------------------------------------------------------------------------
!> \brief Print ScaRC warning message
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_WARNING(NWARNING, CPARAM, NPARAM)
CHARACTER(*), INTENT(IN) :: CPARAM
INTEGER, INTENT(IN) :: NWARNING, NPARAM
CHARACTER(80) :: CWARNING

! Assign warning message according to specified error

SELECT CASE (NWARNING)
   CASE (NSCARC_WARNING_NO_MKL_PRECON)
      CWARNING = 'Intel MKL library missing - only SSOR preconditioner is used'
   CASE (NSCARC_WARNING_NO_MKL_SMOOTH)
      CWARNING = 'Intel MKL library missing - only SSOR smoother is used'
   CASE (NSCARC_WARNING_NO_MKL_LU)
      CWARNING = 'Intel MKL library missing - using LU instead'
   CASE (NSCARC_WARNING_ONLY_SSOR_PRECON)
      CWARNING = 'Only SSOR preconditioner available'
   CASE (NSCARC_WARNING_ONLY_SSOR_SMOOTH)
      CWARNING = 'Only SSOR smoother available'
   CASE (NSCARC_WARNING_NO_GLOBAL_SCOPE)
      CWARNING = 'No global scope preconditioner available'
END SELECT

! Specify more detailed information if available

IF (CPARAM /= SCARC_NONE) THEN
   IF (MY_RANK == 0) WRITE(LU_ERR,1000)  CWARNING, CPARAM, TRIM(CHID)
ELSE IF (NPARAM /= NSCARC_NONE) THEN
   IF (MY_RANK == 0) WRITE(LU_ERR,2000)  CWARNING, NPARAM, TRIM(CHID)
ELSE
   IF (MY_RANK == 0) WRITE(LU_ERR,3000)  CWARNING, TRIM(CHID)
ENDIF

! Also print verbose message if enabled

#ifdef WITH_SCARC_VERBOSE
IF (CPARAM /= SCARC_NONE) THEN
   WRITE(MSG%LU_VERBOSE,1000)  CWARNING, CPARAM, TRIM(CHID)
ELSE IF (NPARAM /= NSCARC_NONE) THEN
   WRITE(MSG%LU_VERBOSE,2000)  CWARNING, NPARAM, TRIM(CHID)
ELSE
   WRITE(MSG%LU_VERBOSE,3000)  CWARNING, TRIM(CHID)
ENDIF
#endif

RETURN

1000 FORMAT('Warning in ScaRC-solver: ', A,' : ',   A, ' (CHID: ',A,')' )
2000 FORMAT('Warning in ScaRC-solver: ', A,' : ', I12, ' (CHID: ',A,')' )
3000 FORMAT('Warning in ScaRC-solver: ', A, ' (CHID: ',A,')' )
END SUBROUTINE SCARC_WARNING

END MODULE SCARC_TROUBLESHOOTING

