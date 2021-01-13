!//////////////////////////////////////////////////////////////////////////////////////////////////////
!
! MODULE SCARC_STORAGE
!
!> \brief Organize the allocation, deallocation and resizing of different data structures
!
!   This includes 1-, 2- or 3-dimensional vectors of different types 
!   and compactly or bandwise stored matrices
!
!//////////////////////////////////////////////////////////////////////////////////////////////////////
MODULE SCARC_STORAGE

USE GLOBAL_CONSTANTS
USE PRECISION_PARAMETERS, ONLY: EB, FB
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
USE SCARC_CONSTANTS
USE SCARC_TYPES
USE SCARC_VARIABLES
USE SCARC_MESSAGES
USE SCARC_ERRORS
USE SCARC_UTILITIES

IMPLICIT NONE

CONTAINS

! ------------------------------------------------------------------------------------------------
!> \brief Setup memory management
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_STORAGE
ALLOCATE (STORAGE%ALLOCATION_LIST(NSCARC_STORAGE_MAX), STAT = IERROR)
CALL CHKMEMERR ('SCARC_SETUP_MEMORY_MANAGMENT', 'ALLOCATION_LIST', IERROR)
END SUBROUTINE SCARC_SETUP_STORAGE


! ------------------------------------------------------------------------------------------------
!> \brief Update list of arrays within ScaRC memory management
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_UPDATE_STORAGE(NDATA, NSTATE, NDIM, NINIT, NL1, NR1, NL2, NR2, NL3, NR3, CNAME, CSCOPE)
USE SCARC_POINTERS, ONLY : AL
INTEGER, INTENT(IN) :: NDATA, NSTATE, NDIM, NINIT, NL1, NR1, NL2, NR2, NL3, NR3
INTEGER :: NWORK, NLEN(3), I, IP
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
CHARACTER(20) :: CTYPE, CSTATE, CINIT, CDIM

STORAGE%IP = STORAGE%IP + 1

! Extract basic name of allocated structure and name of calling routine

! Get size of requested structure

IF (NSTATE /= NSCARC_STORAGE_REMOVE) THEN

   AL => STORAGE%ALLOCATION_LIST(STORAGE%IP)

   AL%CNAME  = CNAME
   AL%CSCOPE = CSCOPE

   AL%LBND = NSCARC_INIT_ZERO
   AL%RBND = NSCARC_INIT_ZERO

   IF (NL1 > 0) AL%LBND(1) = NL1
   IF (NR1 > 0) AL%RBND(1) = NR1
   IF (NL2 > 0) AL%LBND(2) = NL2
   IF (NR2 > 0) AL%RBND(2) = NR2
   IF (NL3 > 0) AL%LBND(3) = NL3
   IF (NR3 > 0) AL%RBND(3) = NR3

ELSE

   DO IP = 1, STORAGE%N_ARRAYS
      AL => STORAGE%ALLOCATION_LIST(IP)
      IF (TRIM(CNAME) == AL%CNAME) EXIT
   ENDDO

ENDIF

NWORK = 1
DO I = 1, 3
   NLEN(I) = AL%RBND(I) - AL%LBND(I) + 1
   IF (NLEN(I) /= 0) NWORK = NWORK * NLEN(I)
ENDDO

! Get some full text information for requested structure to dump out in memory file

CTYPE = SCARC_GET_DATA_TYPE(NDATA)
CDIM  = SCARC_GET_DIMENSION(NDIM)
CINIT = SCARC_GET_INIT_TYPE(NDATA, NINIT, NSTATE)

SELECT CASE (NSTATE)
   CASE (NSCARC_STORAGE_CREATE)
      CSTATE = 'CREATE'
      CALL SCARC_UPDATE_STORAGE_COUNTERS(NDATA, NWORK,  1)
   CASE (NSCARC_STORAGE_RESIZE)
      CSTATE = 'RESIZE'
      CALL SCARC_UPDATE_STORAGE_COUNTERS(NDATA, NWORK,  0)
   CASE (NSCARC_STORAGE_REMOVE)
      CSTATE = 'REMOVE'
      CALL SCARC_UPDATE_STORAGE_COUNTERS(NDATA, NWORK, -1)
      NWORK = -NWORK
   CASE DEFAULT
      CSTATE = ' '
END SELECT

#ifdef WITH_SCARC_VERBOSE
IF (MYID == 0) THEN
   WRITE(MSG%LU_MEM,1000) STORAGE%N_ARRAYS, STORAGE%IP, TRIM(AL%CNAME), TRIM(AL%CSCOPE), TRIM(CSTATE), TRIM(CTYPE), TRIM(CDIM), &
                          AL%LBND(1), AL%RBND(1), AL%LBND(2), AL%RBND(2), AL%LBND(3), AL%RBND(3), &
                          NWORK, STORAGE%NWORK_LOG, STORAGE%NWORK_INT, STORAGE%NWORK_REAL_EB, STORAGE%NWORK_REAL_FB 
ENDIF
1000 FORMAT(I8,',',I8,',',A30,',',A40,',',A10,',',A10,',',A10,',',I10,',',&
            I10,',',I10,',',I10,',',I10,',',I10,',',I15,',',I15,',',I15,',',I15,',',I15)
#endif
END SUBROUTINE SCARC_UPDATE_STORAGE


! ------------------------------------------------------------------------------------------------
!> \brief Update memory statistics w.r.t to occupied workspace and number of allocated arrays
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_UPDATE_STORAGE_COUNTERS(NDATA, NWORK, NSCAL)
INTEGER, INTENT(IN) :: NDATA, NWORK, NSCAL

STORAGE%N_ARRAYS = STORAGE%N_ARRAYS + NSCAL
IF (STORAGE%N_ARRAYS > NSCARC_STORAGE_MAX) WRITE(*,*) 'ERROR in APPEND_TO_ALLOCATION_LIST: list of arrays exceeded!'

SELECT CASE (NDATA) 
   CASE (NSCARC_DATA_INTEGER)
      STORAGE%N_INT = STORAGE%N_INT + NSCAL 
      STORAGE%NWORK_INT = STORAGE%NWORK_INT + NSCAL * NWORK
   CASE (NSCARC_DATA_REAL_EB)
      STORAGE%N_INT = STORAGE%N_REAL_EB + NSCAL 
      STORAGE%NWORK_REAL_EB = STORAGE%NWORK_REAL_EB + NSCAL * NWORK
   CASE (NSCARC_DATA_REAL_FB)
      STORAGE%N_INT = STORAGE%N_REAL_FB + NSCAL 
      STORAGE%NWORK_REAL_FB = STORAGE%NWORK_REAL_FB + NSCAL * NWORK
   CASE (NSCARC_DATA_LOGICAL)
      STORAGE%N_INT = STORAGE%N_LOG + NSCAL 
      STORAGE%NWORK_LOG = STORAGE%NWORK_LOG + NSCAL * NWORK
   CASE (NSCARC_DATA_CMATRIX)
      STORAGE%N_CMATRIX = STORAGE%N_CMATRIX + NSCAL 
   CASE (NSCARC_DATA_BMATRIX)
      STORAGE%N_BMATRIX = STORAGE%N_BMATRIX + NSCAL 
END SELECT

END SUBROUTINE SCARC_UPDATE_STORAGE_COUNTERS



! ------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize integer array of dimension 1
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_ALLOCATE_INT1(WORKSPACE, NL1, NR1, NINIT, CNAME, CSCOPE)
INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: WORKSPACE
INTEGER, INTENT(IN) :: NL1, NR1, NINIT
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE

IF (.NOT.ALLOCATED(WORKSPACE)) THEN
   ALLOCATE (WORKSPACE(NL1:NR1), STAT=IERROR)
   CALL CHKMEMERR ('SCARC_ALLOCATE_INT1', CNAME, IERROR)
   SELECT CASE (NINIT)
      CASE (NSCARC_INIT_UNDEF)
         WORKSPACE = NSCARC_UNDEF_INT
      CASE (NSCARC_INIT_ZERO)
         WORKSPACE = NSCARC_ZERO_INT
      CASE (NSCARC_INIT_ONE)
         WORKSPACE = NSCARC_ONE_INT
      CASE (NSCARC_INIT_HUGE)
         WORKSPACE = NSCARC_HUGE_INT
   END SELECT
ELSE
   IF (SIZE(WORKSPACE,1) /= NR1-NL1+1) CALL SCARC_SHUTDOWN(NSCARC_ERROR_VECTOR_LENGTH, CNAME, NSCARC_NONE)
ENDIF

CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_INTEGER, NSCARC_STORAGE_CREATE, 1, NINIT, NL1, NR1, -1, -1, -1, -1, CNAME, CSCOPE)

END SUBROUTINE SCARC_ALLOCATE_INT1


! ------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize integer array of dimension 2
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_ALLOCATE_INT2(WORKSPACE, NL1, NR1, NL2, NR2, NINIT, CNAME, CSCOPE)
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: WORKSPACE
INTEGER, INTENT(IN) :: NL1, NR1, NL2, NR2, NINIT
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE

IF (.NOT.ALLOCATED(WORKSPACE)) THEN
   ALLOCATE (WORKSPACE(NL1:NR1, NL2:NR2), STAT=IERROR)
   CALL CHKMEMERR ('SCARC_ALLOCATE_INT2', CNAME, IERROR)
   SELECT CASE (NINIT)
      CASE (NSCARC_INIT_UNDEF)
         WORKSPACE = NSCARC_UNDEF_INT
      CASE (NSCARC_INIT_ZERO)
         WORKSPACE = NSCARC_ZERO_INT
      CASE (NSCARC_INIT_ONE)
         WORKSPACE = NSCARC_ONE_INT
      CASE (NSCARC_INIT_HUGE)
         WORKSPACE = NSCARC_HUGE_INT
   END SELECT
ELSE
   IF (SIZE(WORKSPACE,1) /= NR1-NL1+1 .OR. &
      SIZE(WORKSPACE,2) /= NR2-NL2+1) THEN
      CALL SCARC_SHUTDOWN(NSCARC_ERROR_VECTOR_LENGTH, CNAME, NSCARC_NONE)
   ENDIF
ENDIF

CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_INTEGER, NSCARC_STORAGE_CREATE, 2, NINIT, NL1, NR1, NL2, NR2, -1, -1, CNAME, CSCOPE)

END SUBROUTINE SCARC_ALLOCATE_INT2


! ------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize integer array of dimension 3
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_ALLOCATE_INT3(WORKSPACE, NL1, NR1, NL2, NR2, NL3, NR3, NINIT, CNAME, CSCOPE)
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
INTEGER, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: WORKSPACE
INTEGER, INTENT(IN) :: NL1, NR1, NL2, NR2, NL3, NR3, NINIT
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE

IF (.NOT.ALLOCATED(WORKSPACE)) THEN
   ALLOCATE (WORKSPACE(NL1:NR1, NL2:NR2, NL3:NR3), STAT=IERROR)
   CALL CHKMEMERR ('SCARC_ALLOCATE_INT3', CNAME, IERROR)
   SELECT CASE (NINIT)
      CASE (NSCARC_INIT_UNDEF)
         WORKSPACE = NSCARC_UNDEF_INT
      CASE (NSCARC_INIT_ZERO)
         WORKSPACE = NSCARC_ZERO_INT
      CASE (NSCARC_INIT_ONE)
         WORKSPACE = NSCARC_ONE_INT
      CASE (NSCARC_INIT_HUGE)
         WORKSPACE = NSCARC_HUGE_INT
   END SELECT
ELSE
   IF (SIZE(WORKSPACE,1) /= NR1-NL1+1 .OR. &
      SIZE(WORKSPACE,2) /= NR2-NL2+1 .OR. &
      SIZE(WORKSPACE,3) /= NR3-NL3+1) THEN
      CALL SCARC_SHUTDOWN(NSCARC_ERROR_VECTOR_LENGTH, CNAME, NSCARC_NONE)
   ENDIF
ENDIF

CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_INTEGER, NSCARC_STORAGE_CREATE, 3, NINIT, NL1, NR1, NL2, NR2, NL3, NR3, CNAME, CSCOPE)

END SUBROUTINE SCARC_ALLOCATE_INT3


! ------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize Logical array of dimension 1
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_ALLOCATE_LOG1(WORKSPACE, NL1, NR1, NINIT, CNAME, CSCOPE)
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
LOGICAL, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: WORKSPACE
INTEGER, INTENT(IN) :: NL1, NR1, NINIT
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE

IF (.NOT.ALLOCATED(WORKSPACE)) THEN
   ALLOCATE (WORKSPACE(NL1:NR1), STAT=IERROR)
   CALL CHKMEMERR ('SCARC_ALLOCATE_INT1', CNAME, IERROR)
   SELECT CASE (NINIT)
      CASE (NSCARC_INIT_TRUE)
         WORKSPACE = .TRUE.
      CASE (NSCARC_INIT_FALSE)
         WORKSPACE = .FALSE.
   END SELECT
ELSE
   IF (SIZE(WORKSPACE,1) /= NR1-NL1+1) THEN
      CALL SCARC_SHUTDOWN(NSCARC_ERROR_VECTOR_LENGTH, CNAME, NSCARC_NONE)
   ENDIF
ENDIF

CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_LOGICAL, NSCARC_STORAGE_CREATE, 1, NINIT, NL1, NR1, -1, -1, -1, -1, CNAME, CSCOPE)

END SUBROUTINE SCARC_ALLOCATE_LOG1


! ------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize Logical array of dimension 2
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_ALLOCATE_LOG2(WORKSPACE, NL1, NR1, NL2, NR2, NINIT, CNAME, CSCOPE)
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
LOGICAL, DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: WORKSPACE
INTEGER, INTENT(IN) :: NL1, NR1, NL2, NR2, NINIT
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE

IF (.NOT.ALLOCATED(WORKSPACE)) THEN
   ALLOCATE (WORKSPACE(NL1:NR1, NL2:NR2), STAT=IERROR)
   CALL CHKMEMERR ('SCARC_ALLOCATE_INT3', CNAME, IERROR)
   SELECT CASE (NINIT)
      CASE (NSCARC_INIT_TRUE)
         WORKSPACE = .TRUE.
      CASE (NSCARC_INIT_FALSE)
         WORKSPACE = .FALSE.
   END SELECT
ELSE
   IF (SIZE(WORKSPACE,1) /= NR1-NL1+1 .OR. &
      SIZE(WORKSPACE,2) /= NR2-NL2+1) THEN
      CALL SCARC_SHUTDOWN(NSCARC_ERROR_VECTOR_LENGTH, CNAME, NSCARC_NONE)
   ENDIF
ENDIF

CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_LOGICAL, NSCARC_STORAGE_CREATE, 2, NINIT, NL1, NR1, NL2, NR2, -1, -1, CNAME, CSCOPE)

END SUBROUTINE SCARC_ALLOCATE_LOG2


! ------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize Logical array of dimension 3
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_ALLOCATE_LOG3(WORKSPACE, NL1, NR1, NL2, NR2, NL3, NR3, NINIT, CNAME, CSCOPE)
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
LOGICAL, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: WORKSPACE
INTEGER, INTENT(IN) :: NL1, NR1, NL2, NR2, NL3, NR3, NINIT
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE

IF (.NOT.ALLOCATED(WORKSPACE)) THEN
   ALLOCATE (WORKSPACE(NL1:NR1, NL2:NR2, NL3:NR3), STAT=IERROR)
   CALL CHKMEMERR ('SCARC_ALLOCATE_INT3', CNAME, IERROR)
   SELECT CASE (NINIT)
      CASE (NSCARC_INIT_TRUE)
         WORKSPACE = .TRUE.
      CASE (NSCARC_INIT_FALSE)
         WORKSPACE = .FALSE.
   END SELECT
ELSE
   IF (SIZE(WORKSPACE,1) /= NR1-NL1+1 .OR. &
      SIZE(WORKSPACE,2) /= NR2-NL2+1 .OR. &
      SIZE(WORKSPACE,3) /= NR3-NL3+1) THEN
      CALL SCARC_SHUTDOWN(NSCARC_ERROR_VECTOR_LENGTH, CNAME, NSCARC_NONE)
   ENDIF
ENDIF

CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_LOGICAL, NSCARC_STORAGE_CREATE, 3, NINIT, NL1, NR1, NL2, NR2, NL3, NR3, CNAME, CSCOPE)

END SUBROUTINE SCARC_ALLOCATE_LOG3


! ------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize real array of dimension 1
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_ALLOCATE_REAL1(WORKSPACE, NL1, NR1, NINIT, CNAME, CSCOPE)
USE PRECISION_PARAMETERS, ONLY: EB
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
REAL(EB), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: WORKSPACE
INTEGER, INTENT(IN) :: NL1, NR1, NINIT
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE

IF (.NOT.ALLOCATED(WORKSPACE)) THEN

   ALLOCATE (WORKSPACE(NL1:NR1), STAT=IERROR)
   CALL CHKMEMERR ('SCARC_ALLOCATE_REAL1', CNAME, IERROR)

   SELECT CASE (NINIT)
      CASE (NSCARC_INIT_UNDEF)
         WORKSPACE = NSCARC_UNDEF_REAL_EB
      CASE (NSCARC_INIT_ZERO)
         WORKSPACE = NSCARC_ZERO_REAL_EB
      CASE (NSCARC_INIT_ONE)
         WORKSPACE = NSCARC_ONE_REAL_EB
      CASE (NSCARC_INIT_HUGE)
         WORKSPACE = NSCARC_HUGE_REAL_EB
   END SELECT

ELSE
   IF (SIZE(WORKSPACE,1) /= NR1-NL1+1) &
      CALL SCARC_SHUTDOWN(NSCARC_ERROR_VECTOR_LENGTH, CNAME, NSCARC_NONE)
ENDIF

CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_REAL_EB, NSCARC_STORAGE_CREATE, 1, NINIT, NL1, NR1, -1, -1, -1, -1, CNAME, CSCOPE)

END SUBROUTINE SCARC_ALLOCATE_REAL1


! ------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize real array of dimension 1
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_ALLOCATE_REAL1_FB(WORKSPACE, NL1, NR1, NINIT, CNAME, CSCOPE)
USE PRECISION_PARAMETERS, ONLY: FB
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
REAL(FB), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: WORKSPACE
INTEGER, INTENT(IN) :: NL1, NR1, NINIT
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE

IF (.NOT.ALLOCATED(WORKSPACE)) THEN
   ALLOCATE (WORKSPACE(NL1:NR1), STAT=IERROR)
   CALL CHKMEMERR ('SCARC_ALLOCATE_REAL1_FB', CNAME, IERROR)
   SELECT CASE (NINIT)
      CASE (NSCARC_INIT_UNDEF)
         WORKSPACE = NSCARC_UNDEF_REAL_FB
      CASE (NSCARC_INIT_ZERO)
         WORKSPACE = NSCARC_ZERO_REAL_FB
      CASE (NSCARC_INIT_ONE)
         WORKSPACE = NSCARC_ONE_REAL_FB
      CASE (NSCARC_INIT_HUGE)
         WORKSPACE = NSCARC_HUGE_REAL_FB
   END SELECT
ELSE
   IF (SIZE(WORKSPACE,1) /= NR1-NL1+1) &
      CALL SCARC_SHUTDOWN(NSCARC_ERROR_VECTOR_LENGTH, CNAME, NSCARC_NONE)
ENDIF

CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_REAL_FB, NSCARC_STORAGE_CREATE, 1, NINIT, NL1, NR1, -1, -1, -1, -1, CNAME, CSCOPE)

END SUBROUTINE SCARC_ALLOCATE_REAL1_FB


! ------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize real array of dimension 2
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_ALLOCATE_REAL2(WORKSPACE, NL1, NR1, NL2, NR2, NINIT, CNAME, CSCOPE)
USE PRECISION_PARAMETERS, ONLY: EB
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
REAL(EB), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: WORKSPACE
INTEGER, INTENT(IN) :: NL1, NR1, NL2, NR2, NINIT
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE

IF (.NOT.ALLOCATED(WORKSPACE)) THEN
   ALLOCATE (WORKSPACE(NL1:NR1, NL2:NR2), STAT=IERROR)
   CALL CHKMEMERR ('SCARC_ALLOCATE_REAL2', CNAME, IERROR)
   SELECT CASE (NINIT)
      CASE (NSCARC_INIT_UNDEF)
         WORKSPACE = NSCARC_UNDEF_REAL_EB
      CASE (NSCARC_INIT_ZERO)
         WORKSPACE = NSCARC_ZERO_REAL_EB
      CASE (NSCARC_INIT_ONE)
         WORKSPACE = NSCARC_ONE_REAL_EB
      CASE (NSCARC_INIT_HUGE)
         WORKSPACE = NSCARC_HUGE_REAL_EB
   END SELECT
ELSE
   IF (SIZE(WORKSPACE,1) /= NR1-NL1+1 .OR. &
      SIZE(WORKSPACE,2) /= NR2-NL2+1) THEN
      CALL SCARC_SHUTDOWN(NSCARC_ERROR_VECTOR_LENGTH, CNAME, NSCARC_NONE)
   ENDIF
ENDIF

CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_REAL_EB, NSCARC_STORAGE_CREATE, 2, NINIT, NL1, NR1, NL2, NR2, -1, -1, CNAME, CSCOPE)

END SUBROUTINE SCARC_ALLOCATE_REAL2


! ------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize real array of dimension 3
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_ALLOCATE_REAL3(WORKSPACE, NL1, NR1, NL2, NR2, NL3, NR3, NINIT, CNAME, CSCOPE)
USE PRECISION_PARAMETERS, ONLY: EB
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
REAL(EB), DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: WORKSPACE
INTEGER, INTENT(IN) :: NL1, NR1, NL2, NR2, NL3, NR3, NINIT
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE

IF (.NOT.ALLOCATED(WORKSPACE)) THEN
   ALLOCATE (WORKSPACE(NL1:NR1, NL2:NR2, NL3:NR3), STAT=IERROR)
   CALL CHKMEMERR ('SCARC_ALLOCATE_REAL3', CNAME, IERROR)
   SELECT CASE (NINIT)
      CASE (NSCARC_INIT_UNDEF)
         WORKSPACE = NSCARC_UNDEF_REAL_EB
      CASE (NSCARC_INIT_ZERO)
         WORKSPACE = NSCARC_ZERO_REAL_EB
      CASE (NSCARC_INIT_ONE)
         WORKSPACE = NSCARC_ONE_REAL_EB
      CASE (NSCARC_INIT_HUGE)
         WORKSPACE = NSCARC_HUGE_REAL_EB
   END SELECT
ELSE
   IF (SIZE(WORKSPACE,1) /= NR1-NL1+1 .OR. &
      SIZE(WORKSPACE,2) /= NR2-NL2+1 .OR. &
      SIZE(WORKSPACE,3) /= NR3-NL3+1) THEN
      CALL SCARC_SHUTDOWN(NSCARC_ERROR_VECTOR_LENGTH, CNAME, NSCARC_NONE)
   ENDIF
ENDIF

CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_REAL_EB, NSCARC_STORAGE_CREATE, 3, NINIT, NL1, NR1, NL2, NR2, NL3, NR3, CNAME, CSCOPE)

END SUBROUTINE SCARC_ALLOCATE_REAL3


! ------------------------------------------------------------------------------------------------
!> \brief Deallocate one-dimensional integer vector 
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEALLOCATE_INT1(WORKSPACE, CNAME, CSCOPE)
INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: WORKSPACE
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
DEALLOCATE(WORKSPACE) 
CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_INTEGER, NSCARC_STORAGE_REMOVE, 1, -1, -1, -1, -1, -1, -1, -1, CNAME, CSCOPE)
END SUBROUTINE SCARC_DEALLOCATE_INT1


! ------------------------------------------------------------------------------------------------
!> \brief Deallocate two-dimensional integer vector 
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEALLOCATE_INT2(WORKSPACE, CNAME, CSCOPE)
INTEGER, ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT) :: WORKSPACE
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
DEALLOCATE(WORKSPACE) 
CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_INTEGER, NSCARC_STORAGE_REMOVE, 2, -1, -1, -1, -1, -1, -1, -1, CNAME, CSCOPE)
END SUBROUTINE SCARC_DEALLOCATE_INT2


! ------------------------------------------------------------------------------------------------
!> \brief Deallocate three-dimensional integer vector 
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEALLOCATE_INT3(WORKSPACE, CNAME, CSCOPE)
INTEGER, ALLOCATABLE, DIMENSION(:,:,:), INTENT(INOUT) :: WORKSPACE
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
DEALLOCATE(WORKSPACE) 
CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_INTEGER, NSCARC_STORAGE_REMOVE, 3, -1, -1, -1, -1, -1, -1, -1, CNAME, CSCOPE)
END SUBROUTINE SCARC_DEALLOCATE_INT3


! ------------------------------------------------------------------------------------------------
!> \brief Deallocate one-dimensional logical vector 
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEALLOCATE_LOG1(WORKSPACE, CNAME, CSCOPE)
LOGICAL, ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: WORKSPACE
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
DEALLOCATE(WORKSPACE) 
CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_LOGICAL, NSCARC_STORAGE_REMOVE, 1, -1, -1, -1, -1, -1, -1, -1, CNAME, CSCOPE)
END SUBROUTINE SCARC_DEALLOCATE_LOG1


! ------------------------------------------------------------------------------------------------
!> \brief Deallocate two-dimensional logical vector 
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEALLOCATE_LOG2(WORKSPACE, CNAME, CSCOPE)
LOGICAL, ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT) :: WORKSPACE
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
DEALLOCATE(WORKSPACE) 
CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_LOGICAL, NSCARC_STORAGE_REMOVE, 2, -1, -1, -1, -1, -1, -1, -1, CNAME, CSCOPE)
END SUBROUTINE SCARC_DEALLOCATE_LOG2


! ------------------------------------------------------------------------------------------------
!> \brief Deallocate three-dimensional logical vector 
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEALLOCATE_LOG3(WORKSPACE, CNAME, CSCOPE)
LOGICAL, ALLOCATABLE, DIMENSION(:,:,:), INTENT(INOUT) :: WORKSPACE
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
DEALLOCATE(WORKSPACE) 
CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_LOGICAL, NSCARC_STORAGE_REMOVE, 3, -1, -1, -1, -1, -1, -1, -1, CNAME, CSCOPE)
END SUBROUTINE SCARC_DEALLOCATE_LOG3


! ------------------------------------------------------------------------------------------------
!> \brief Deallocate one-dimensional double precision vector 
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEALLOCATE_REAL1(WORKSPACE, CNAME, CSCOPE)
REAL(EB), ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: WORKSPACE
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
DEALLOCATE(WORKSPACE) 
CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_REAL_EB, NSCARC_STORAGE_REMOVE, 1, -1, -1, -1, -1, -1, -1, -1, CNAME, CSCOPE)
END SUBROUTINE SCARC_DEALLOCATE_REAL1


! ------------------------------------------------------------------------------------------------
!> \brief Deallocate two-dimensional double precision vector 
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEALLOCATE_REAL2(WORKSPACE, CNAME, CSCOPE)
REAL(EB), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT) :: WORKSPACE
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
DEALLOCATE(WORKSPACE) 
CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_REAL_EB, NSCARC_STORAGE_REMOVE, 2, -1, -1, -1, -1, -1, -1, -1, CNAME, CSCOPE)
END SUBROUTINE SCARC_DEALLOCATE_REAL2


! ------------------------------------------------------------------------------------------------
!> \brief Deallocate three-dimensional double precision vector 
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEALLOCATE_REAL3(WORKSPACE, CNAME, CSCOPE)
REAL(EB), ALLOCATABLE, DIMENSION(:,:,:), INTENT(INOUT) :: WORKSPACE
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
DEALLOCATE(WORKSPACE) 
CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_REAL_EB, NSCARC_STORAGE_REMOVE, 3, -1, -1, -1, -1, -1, -1, -1, CNAME, CSCOPE)
END SUBROUTINE SCARC_DEALLOCATE_REAL3


! ------------------------------------------------------------------------------------------------
!> \brief Deallocate one-dimensional single precision vector 
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEALLOCATE_REAL1_FB(WORKSPACE, CNAME, CSCOPE)
REAL(FB), ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: WORKSPACE
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
DEALLOCATE(WORKSPACE) 
CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_REAL_EB, NSCARC_STORAGE_REMOVE, 1, -1, -1, -1, -1, -1, -1, -1, CNAME, CSCOPE)
END SUBROUTINE SCARC_DEALLOCATE_REAL1_FB


! ------------------------------------------------------------------------------------------------
!> \brief Deallocate one-dimensional single precision vector 
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEALLOCATE_REAL2_FB(WORKSPACE, CNAME, CSCOPE)
REAL(FB), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT) :: WORKSPACE
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
DEALLOCATE(WORKSPACE) 
CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_REAL_EB, NSCARC_STORAGE_REMOVE, 2, -1, -1, -1, -1, -1, -1, -1, CNAME, CSCOPE)
END SUBROUTINE SCARC_DEALLOCATE_REAL2_FB


! ------------------------------------------------------------------------------------------------
!> \brief Deallocate one-dimensional single precision vector 
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEALLOCATE_REAL3_FB(WORKSPACE, CNAME, CSCOPE)
REAL(FB), ALLOCATABLE, DIMENSION(:,:,:), INTENT(INOUT) :: WORKSPACE
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
DEALLOCATE(WORKSPACE) 
CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_REAL_EB, NSCARC_STORAGE_REMOVE, 3, -1, -1, -1, -1, -1, -1, -1, CNAME, CSCOPE)
END SUBROUTINE SCARC_DEALLOCATE_REAL3_FB


! ------------------------------------------------------------------------------------------------
!> \brief Resize one-dimensional integer vector to requested bounds
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_RESIZE_INT1(WORKSPACE, NL1, NR1, CNAME, CSCOPE)
INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: WORKSPACE
INTEGER, INTENT(IN) :: NL1, NR1
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
INTEGER, ALLOCATABLE, DIMENSION(:) :: AUX
INTEGER :: NLC1, NRC1, NSC, NS

NLC1 = LBOUND(WORKSPACE, DIM=1)
NRC1 = UBOUND(WORKSPACE, DIM=1)

IF (NL1 == NLC1 .AND. NR1 == NRC1) THEN
   RETURN
ELSE

   NSC = NRC1 - NLC1 + 1
   NS  = NR1  - NL1  + 1

   ALLOCATE(AUX(1:NSC), STAT = IERROR)                        ! don't track it in memory management, only auxiliary
   AUX(1:NSC) = WORKSPACE(NLC1:NRC1)
   CALL SCARC_DEALLOCATE_INT1 (WORKSPACE, CNAME, CSCOPE)

   CALL SCARC_ALLOCATE_INT1(WORKSPACE, NL1, NR1, NSCARC_INIT_NONE, CNAME, CSCOPE)

   IF (NS < NSC) THEN
      WORKSPACE(NL1:NL1 + NS) = AUX(1:NS)
   ELSE
      WORKSPACE(NL1:NL1 + NSC) = AUX(1:NSC)
   ENDIF
   DEALLOCATE(AUX)

ENDIF

END SUBROUTINE SCARC_RESIZE_INT1


! ------------------------------------------------------------------------------------------------
!> \brief Resize two-dimensional integer vector to requested bounds
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_RESIZE_INT2(WORKSPACE, NL1, NR1, NL2, NR2, CNAME, CSCOPE)
INTEGER, ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT) :: WORKSPACE
INTEGER, INTENT(IN) :: NL1, NR1, NL2, NR2
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
INTEGER, ALLOCATABLE, DIMENSION(:,:) :: AUX
INTEGER :: NLC1, NRC1, NSC1, NS1
INTEGER :: NLC2, NRC2, NSC2, NS2

NLC1 = LBOUND(WORKSPACE, DIM=1)                ! current left  bound for dimension 1
NRC1 = UBOUND(WORKSPACE, DIM=1)                ! current right bound for dimension 1

NLC2 = LBOUND(WORKSPACE, DIM=2)                ! current left  bound for dimension 2
NRC2 = UBOUND(WORKSPACE, DIM=2)                ! current right bound for dimension 2

IF (NL1 == NLC1 .AND. NR1 == NRC1 .AND. &
    NL2 == NLC2 .AND. NR2 == NRC2) THEN
   RETURN
ELSE

   NSC1 = NRC1 - NLC1 + 1
   NSC2 = NRC2 - NLC2 + 1

   NS1 = NR1 - NL1 + 1
   NS2 = NR2 - NL2 + 1

   ALLOCATE(AUX(1: NSC1, 1: NSC2), STAT = IERROR)                     ! don't track it in memory management, only auxiliary
   AUX(1:NSC1, 1:NSC2) = WORKSPACE(NLC1:NRC1, NLC2:NRC2)
   CALL SCARC_DEALLOCATE_INT2(WORKSPACE, CNAME, CSCOPE)

   CALL SCARC_ALLOCATE_INT2(WORKSPACE, NL1, NR1, NL2, NR2, NSCARC_INIT_NONE, CNAME, CSCOPE)

   IF (NS1 < NSC1 .AND. NS2 < NSC2) THEN
      WORKSPACE(NL1:NL1+NS1, NL2:NL2+NS2) = AUX(1:NS1, 1:NS2)
   ELSE IF (NS1 < NSC1 .AND. NS2 > NSC2) THEN
      WORKSPACE(NL1:NL1+NS1, NL2:NL2+NSC2) = AUX(1:NS1, 1:NSC2)
   ELSE IF (NS1 > NSC1 .AND. NS2 < NSC2) THEN
      WORKSPACE(NL1:NL1+NSC1, NL2:NL2+NS2) = AUX(1:NSC1, 1:NS2)
   ELSE
      WORKSPACE(NL1:NL1+NSC1, NL2:NL2+NSC2) = AUX(1:NSC1, 1:NSC2)
   ENDIF

   DEALLOCATE(AUX)

ENDIF

END SUBROUTINE SCARC_RESIZE_INT2


! ------------------------------------------------------------------------------------------------
!> \brief Reduce size of integer vector
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_REDUCE_INT1(WORKSPACE, NSIZE, CNAME, CSCOPE)
INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: WORKSPACE
INTEGER, INTENT(IN) :: NSIZE
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
INTEGER, ALLOCATABLE, DIMENSION(:) :: AUX

IF (NSIZE == SIZE(WORKSPACE)) THEN
   RETURN                                  ! vector already has requested size
ELSE IF (NSIZE < SIZE(WORKSPACE)) THEN

   ALLOCATE(AUX(1: NSIZE), STAT = IERROR)
   AUX(1:NSIZE) = WORKSPACE(1:NSIZE)
   CALL SCARC_DEALLOCATE_INT1 (WORKSPACE, CNAME, CSCOPE)

   CALL SCARC_ALLOCATE_INT1(WORKSPACE, 1, NSIZE, NSCARC_INIT_NONE, CNAME, CSCOPE)
   WORKSPACE(1:NSIZE) = AUX(1:NSIZE)
   DEALLOCATE(AUX)
ENDIF

END SUBROUTINE SCARC_REDUCE_INT1


! ------------------------------------------------------------------------------------------------
!> \brief Reduce size of integer array with dimension 2
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_REDUCE_INT2(WORKSPACE, NSIZE1, NSIZE2, CNAME, CSCOPE)
INTEGER, ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT) :: WORKSPACE
INTEGER, INTENT(IN) :: NSIZE1, NSIZE2
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
INTEGER :: NWORK1, NWORK2, I1, I2
INTEGER, ALLOCATABLE, DIMENSION(:,:) :: AUX

NWORK1 = SIZE(WORKSPACE, DIM = 1)
NWORK2 = SIZE(WORKSPACE, DIM = 2)
IF (NSIZE1 == NWORK1 .AND. NSIZE2 == NWORK2 ) THEN
   RETURN                                  ! vector already has requested size
ELSE IF (NSIZE1 <= NWORK1 .AND. NSIZE2 <= NWORK2) THEN

   ALLOCATE(AUX(1: NWORK1, 1: NWORK2), STAT = IERROR) 
   DO I2 = 1, NWORK2
      DO I1 = 1, NWORK1
         AUX(I1, I2) = WORKSPACE(I1, I2)
      ENDDO
   ENDDO
   CALL SCARC_DEALLOCATE_INT2 (WORKSPACE, CNAME, CSCOPE)

   CALL SCARC_ALLOCATE_INT2(WORKSPACE, 1, NSIZE1, 1, NSIZE2, NSCARC_INIT_NONE, TRIM(CNAME), CSCOPE)
   DO I2 = 1, NSIZE2
      DO I1 = 1, NSIZE1
         WORKSPACE(I1, I2) = AUX(I1, I2)
      ENDDO
   ENDDO
   DEALLOCATE(AUX)
ELSE
   WRITE(*,'(A,2I6,A,2I6)') 'Error in SCARC_REDUCE_INT2, orig ', NWORK1, NWORK2,': new ',NSIZE1, NSIZE2
ENDIF

END SUBROUTINE SCARC_REDUCE_INT2


! ------------------------------------------------------------------------------------------------
!> \brief Expand size of integer vector
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_EXPAND_INT1(WORKSPACE, WORKSPACE_ADD, NSIZE, NSIZE_ADD, CNAME, CSCOPE)
INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: WORKSPACE, WORKSPACE_ADD
INTEGER, INTENT(IN) :: NSIZE, NSIZE_ADD
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
INTEGER, ALLOCATABLE, DIMENSION(:) :: AUX

ALLOCATE(AUX(1: NSIZE + NSIZE_ADD), STAT = IERROR)
AUX(1:NSIZE) = WORKSPACE(1:NSIZE)
AUX(NSIZE+1:NSIZE+NSIZE_ADD) = WORKSPACE_ADD(NSIZE+1:NSIZE+NSIZE_ADD)
CALL SCARC_DEALLOCATE_INT1 (WORKSPACE, CNAME, CSCOPE)

CALL SCARC_ALLOCATE_INT1(WORKSPACE, 1, NSIZE + NSIZE_ADD, NSCARC_INIT_NONE, TRIM(CNAME), CSCOPE)
WORKSPACE(1:NSIZE+NSIZE_ADD) = AUX(1:NSIZE+NSIZE_ADD)
DEALLOCATE(AUX)

END SUBROUTINE SCARC_EXPAND_INT1


! ------------------------------------------------------------------------------------------------
!> \brief Reduce size of integer vector
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_REDUCE_REAL1(WORKSPACE, NSIZE, CNAME, CSCOPE)
REAL(EB), ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: WORKSPACE
INTEGER, INTENT(IN) :: NSIZE
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
REAL(EB), ALLOCATABLE, DIMENSION(:) :: AUX

IF (NSIZE == SIZE(WORKSPACE)) THEN
   RETURN                                  ! vector already has requested size
ELSE IF (NSIZE < SIZE(WORKSPACE)) THEN

   ALLOCATE(AUX(1: NSIZE), STAT = IERROR)
   AUX(1:NSIZE) = WORKSPACE(1:NSIZE)
   CALL SCARC_DEALLOCATE_REAL1 (WORKSPACE, CNAME, CSCOPE)

   CALL SCARC_ALLOCATE_REAL1(WORKSPACE, 1, NSIZE, NSCARC_INIT_NONE, TRIM(CNAME), CSCOPE)
   WORKSPACE(1:NSIZE) = AUX(1:NSIZE)
   DEALLOCATE(AUX)
ENDIF

END SUBROUTINE SCARC_REDUCE_REAL1


! ------------------------------------------------------------------------------------------------
!> \brief Allocate matrix in compact storage format
! Allocate matrix with corresponding pointer and length structures
!    NTYPE == NSCARC_MATRIX_FULL    :  ALLOCATE VAL, COL and COLG
!    NTYPE == NSCARC_MATRIX_LIGHT   :  ALLOCATE VAL, COL 
!    NTYPE == NSCARC_MATRIX_MINIMAL :  ALLOCATE COL 
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_ALLOCATE_CMATRIX(A, NL, NPREC, NTYPE, CNAME, CSCOPE)
TYPE (SCARC_CMATRIX_TYPE), INTENT(INOUT) :: A
INTEGER, INTENT(IN) :: NPREC, NTYPE, NL
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
INTEGER :: NDUMMY

A%CNAME = TRIM(CNAME)
A%NTYPE = NTYPE
A%NPREC = NPREC
NDUMMY = NL

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '1:ALLOCATING A ', A%N_ROW, A%N_VAL, TYPE_SCOPE(0), TYPE_MATVEC
#endif
CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_CMATRIX, NSCARC_STORAGE_CREATE, -1, -1, -1, -1, -1, -1, -1, -1, CNAME, CSCOPE)

CALL SCARC_ALLOCATE_INT1(A%ROW, 1, A%N_ROW, NSCARC_INIT_ZERO, 'A%ROW', CSCOPE)
CALL SCARC_ALLOCATE_INT1(A%COL, 1, A%N_VAL, NSCARC_INIT_ZERO, 'A%COL', CSCOPE)

IF (TYPE_SCOPE(0) == NSCARC_SCOPE_GLOBAL .AND. (NTYPE == NSCARC_MATRIX_LIGHT .OR. NTYPE == NSCARC_MATRIX_FULL)) THEN
   CALL SCARC_ALLOCATE_INT1(A%COLG, 1, A%N_VAL, NSCARC_INIT_ZERO, 'A%COLG', CSCOPE)
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '4:ALLOCATING A%COLG FOR ', CNAME, CSCOPE, NTYPE, TYPE_SCOPE(0)
#endif
ENDIF

IF (NTYPE /= NSCARC_MATRIX_MINIMAL) THEN
   IF (NPREC == NSCARC_PRECISION_SINGLE) THEN
      CALL SCARC_ALLOCATE_REAL1_FB(A%VAL_FB, 1, A%N_VAL, NSCARC_INIT_ZERO, 'A%VAL_FB', CSCOPE)
   ELSE
      CALL SCARC_ALLOCATE_REAL1(A%VAL, 1, A%N_VAL, NSCARC_INIT_ZERO, 'A%VAL', CSCOPE)
   ENDIF
ENDIF

END SUBROUTINE SCARC_ALLOCATE_CMATRIX


! ------------------------------------------------------------------------------------------------
!> \brief Dellocate matrix in compact storage format
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEALLOCATE_CMATRIX(A, CNAME, CSCOPE)
TYPE (SCARC_CMATRIX_TYPE), INTENT(INOUT) :: A
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE

A%N_STENCIL   = 0
A%N_CONDENSED = 0
A%N_ROW       = 0
A%N_VAL       = 0
A%NTYPE       = 0
A%NPREC       = 0
A%STENCIL     = 0
A%POS         = 0

CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_CMATRIX, NSCARC_STORAGE_REMOVE, -1, -1, -1, -1, -1, -1, -1, -1, CNAME, CSCOPE)

IF (ALLOCATED(A%VAL))   CALL SCARC_DEALLOCATE_REAL1 (A%VAL, 'A%VAL', CSCOPE)
IF (ALLOCATED(A%ROW))   CALL SCARC_DEALLOCATE_INT1 (A%ROW, 'A%ROW', CSCOPE)
IF (ALLOCATED(A%COL))   CALL SCARC_DEALLOCATE_INT1 (A%COL, 'A%COL', CSCOPE)
IF (ALLOCATED(A%COLG))  CALL SCARC_DEALLOCATE_INT1 (A%COLG, 'A%COLG', CSCOPE)
IF (ALLOCATED(A%RELAX)) CALL SCARC_DEALLOCATE_REAL1 (A%RELAX, 'A%RELAX', CSCOPE)

END SUBROUTINE SCARC_DEALLOCATE_CMATRIX

! ------------------------------------------------------------------------------------------------
!> \brief Insert value at specified position in matrix of compact storage format
! ------------------------------------------------------------------------------------------------
REAL(EB) FUNCTION SCARC_EVALUATE_CMATRIX(A, IC, JC) 
TYPE (SCARC_CMATRIX_TYPE), INTENT(IN) :: A
INTEGER, INTENT(IN) :: IC, JC
INTEGER :: IP

SCARC_EVALUATE_CMATRIX =  0.0_EB
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'EVAL_CMATRIX: SEARCHING FOR IC, JC :', IC, JC
DO IP = A%ROW(IC), A%ROW(IC+1)-1
WRITE(MSG%LU_DEBUG,*) 'A%COL(',IP,')=', A%COL(IP)
enddo
#endif
DO IP = A%ROW(IC), A%ROW(IC+1)-1
   IF (A%COL(IP) == JC) THEN
      SCARC_EVALUATE_CMATRIX = A%VAL(IP)
      EXIT
   ENDIF
ENDDO

END FUNCTION SCARC_EVALUATE_CMATRIX

! ------------------------------------------------------------------------------------------------
!> \brief Insert value at specified position in matrix of compact storage format
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_INSERT_TO_CMATRIX(A, VAL, IC, JC, NC, NP, CNAME) 
TYPE (SCARC_CMATRIX_TYPE), INTENT(INOUT) :: A
INTEGER, INTENT(IN) :: IC, JC, NC
INTEGER, INTENT(INOUT) :: NP
CHARACTER(*), INTENT(IN) :: CNAME
REAL(EB), INTENT(IN) :: VAL
INTEGER :: IP
#ifndef WITH_SCARC_DEBUG
CHARACTER(1) :: CSAVE
#endif
REAL(EB) :: TOL = 1.0E-14_EB

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) CNAME, IC, JC, VAL, NC, NP
#else
CSAVE = CNAME(1:1)    ! dummy command to justify the argument CNAME in the non-debug case
#endif

IF (NP == A%N_VAL) WRITE(*,*) MYID+1,': SCARC_INSERT_TO_CMATRIX: Error, maximum length already reached'
IF (ABS(VAL) < TOL) RETURN

ALREADY_EXISTING_LOOP: DO IP = A%ROW(IC), A%ROW(IC+1)-1
   IF (JC /= IC .AND. JC == A%COL(IP)) THEN
      WRITE(*,*) MYID+1,': SCARC_INSERT_TO_CMATRIX: Index ', JC,' already exists'
      EXIT
   ENDIF
ENDDO ALREADY_EXISTING_LOOP

IF (JC == IC) THEN
   IP = A%ROW(IC)
   A%VAL(IP) = VAL                               ! COL and ROW already correct
   A%COL(IP) = JC
ELSE
   IP = A%ROW(IC+1)
   A%VAL(IP+1:NP+1) = A%VAL(IP:NP)               ! COL and ROW must be shifted
   A%COL(IP+1:NP+1) = A%COL(IP:NP)
   A%ROW(IC+1:NC+1) = A%ROW(IC+1:NC+1) + 1
   A%VAL(IP) = VAL                               ! COL and ROW already correct
   A%COL(IP) = JC
   NP = NP + 1
ENDIF

#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_CMATRIX (A, CNAME, 'SCARC_INSERT_TO_CMATRIX')
1000 FORMAT('INSERT_TO_CMATRIX, ', A4,'(',I3,',',I3,')=',E14.6,',       NC:', I3,', NP:', I3)
#endif
END SUBROUTINE SCARC_INSERT_TO_CMATRIX

! ------------------------------------------------------------------------------------------------
!> \brief Reduce size of matrix in compact storage format
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_COPY_CMATRIX(A1, A2, CNAME2, CSCOPE)
TYPE (SCARC_CMATRIX_TYPE), INTENT(INOUT) :: A1, A2
CHARACTER(*), INTENT(IN) :: CNAME2, CSCOPE

A2%CNAME = TRIM(CNAME2)
A2%N_STENCIL   = A1%N_STENCIL
A2%N_CONDENSED = A1%N_CONDENSED
A2%N_ROW       = A1%N_ROW
A2%N_VAL       = A1%N_VAL
A2%NTYPE       = A1%NTYPE
A2%NPREC       = A1%NPREC
A2%STENCIL     = A1%STENCIL
A2%POS         = A1%POS

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '1:ALLOCATING A2 ', A2%N_ROW, A2%N_VAL, TYPE_SCOPE(0), TYPE_MATVEC
#endif
CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_CMATRIX, NSCARC_STORAGE_CREATE, -1, -1, -1, -1, -1, -1, -1, -1, CNAME, CSCOPE)

CALL SCARC_ALLOCATE_INT1(A2%ROW, 1, A2%N_ROW, NSCARC_INIT_NONE, 'A2%ROW', CSCOPE)
A2%ROW = A1%ROW

CALL SCARC_ALLOCATE_INT1(A2%COL, 1, A2%N_VAL, NSCARC_INIT_NONE, 'A2%COL', CSCOPE)
A2%COL = A1%COL

CALL SCARC_ALLOCATE_REAL1(A2%VAL, 1, A2%N_VAL, NSCARC_INIT_ZERO, 'A2%VAL', CSCOPE)
A2%VAL = A1%VAL

END SUBROUTINE SCARC_COPY_CMATRIX


! ------------------------------------------------------------------------------------------------
!> \brief Reduce size of matrix in compact storage format
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_REDUCE_CMATRIX(A, CNAME, CSCOPE)
TYPE (SCARC_CMATRIX_TYPE), INTENT(INOUT) :: A
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
REAL(EB), ALLOCATABLE, DIMENSION(:) :: VAL
REAL(FB), ALLOCATABLE, DIMENSION(:) :: VAL_FB
INTEGER , ALLOCATABLE, DIMENSION(:) :: COL, COLG
INTEGER :: NVAL_CURRENT, NVAL_ALLOCATED

NVAL_CURRENT = A%ROW(A%N_ROW)
NVAL_ALLOCATED = SIZE(A%COL)

CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_CMATRIX, NSCARC_STORAGE_RESIZE, -1, -1, -1, -1, -1, -1, -1, -1, CNAME, CSCOPE)

! If the matrix already has the desired size or specified values are to small, return or shutdown
IF (NVAL_ALLOCATED == NVAL_CURRENT) THEN
#ifdef WITH_SCARC_VERBOSE
   WRITE(MSG%LU_VERBOSE,*) ' ...  nothing to do '
#endif
   RETURN
ELSE IF (NVAL_ALLOCATED < NVAL_CURRENT) THEN
#ifdef WITH_SCARC_VERBOSE
   WRITE(MSG%LU_VERBOSE,*) 'Reducing CMATRIX ',CNAME,' from ',NVAL_ALLOCATED,' to ', NVAL_CURRENT,' failed '
#endif
   CALL SCARC_SHUTDOWN(NSCARC_ERROR_MATRIX_SIZE, CNAME, NSCARC_NONE)
ENDIF

! If the allocated size of the matrix values workspace is too large, reduce it to the real size
IF (NVAL_CURRENT < SIZE(A%COL)) THEN

   ALLOCATE(COL(1: NVAL_CURRENT), STAT = IERROR)
   COL(1:NVAL_CURRENT) = A%COL(1:NVAL_CURRENT)
   CALL SCARC_DEALLOCATE_INT1 (A%COL, 'A%COL', CSCOPE)
   CALL SCARC_ALLOCATE_INT1(A%COL, 1, NVAL_CURRENT, NSCARC_INIT_NONE, 'A%COL', CSCOPE)
   A%COL(1:NVAL_CURRENT) = COL(1:NVAL_CURRENT)
   DEALLOCATE(COL)

   IF (ALLOCATED(A%COLG)) THEN
      ALLOCATE(COLG(1: NVAL_CURRENT), STAT = IERROR)
      COLG(1:NVAL_CURRENT) = A%COLG(1:NVAL_CURRENT)
      CALL SCARC_DEALLOCATE_INT1 (A%COLG, 'A%COLG',CSCOPE)
      CALL SCARC_ALLOCATE_INT1(A%COLG, 1, NVAL_CURRENT, NSCARC_INIT_NONE, 'A%COLG', CSCOPE)
      A%COLG(1:NVAL_CURRENT) = COLG(1:NVAL_CURRENT)
      DEALLOCATE(COLG)
   ENDIF

   IF (A%NTYPE /= NSCARC_MATRIX_MINIMAL) THEN
      SELECT CASE (A%NPREC)
         CASE (NSCARC_PRECISION_SINGLE)
            ALLOCATE(VAL_FB(1: NVAL_CURRENT), STAT = IERROR)
            VAL_FB(1:NVAL_CURRENT) = A%VAL_FB(1:NVAL_CURRENT)
            CALL SCARC_DEALLOCATE_REAL1_FB(A%VAL_FB, 'A%VAL_FB', CSCOPE)
            CALL SCARC_ALLOCATE_REAL1_FB(A%VAL_FB, 1, NVAL_CURRENT, NSCARC_INIT_NONE, 'A%VAL_FB', CSCOPE)
            A%VAL_FB(1:NVAL_CURRENT) = VAL_FB(1:NVAL_CURRENT)
            DEALLOCATE(VAL_FB)
         CASE (NSCARC_PRECISION_DOUBLE)
            ALLOCATE(VAL(1: NVAL_CURRENT), STAT = IERROR)
            VAL(1:NVAL_CURRENT) = A%VAL(1:NVAL_CURRENT)
            CALL SCARC_DEALLOCATE_REAL1 (A%VAL, 'A%VAL', CSCOPE)
            CALL SCARC_ALLOCATE_REAL1(A%VAL, 1, NVAL_CURRENT, NSCARC_INIT_NONE, 'A%VAL', CSCOPE)
            A%VAL(1:NVAL_CURRENT) = VAL(1:NVAL_CURRENT)
            DEALLOCATE(VAL)
         END SELECT
   ENDIF
   A%N_VAL = NVAL_CURRENT

ENDIF

END SUBROUTINE SCARC_REDUCE_CMATRIX


! ------------------------------------------------------------------------------------------------
!> \brief Allocate matrix in bandwise storage format
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_ALLOCATE_BMATRIX(A, NL, CNAME, CSCOPE)
TYPE (SCARC_BMATRIX_TYPE), INTENT(INOUT) :: A
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE
INTEGER, INTENT(IN) :: NL
CHARACTER(40) :: CINFO

A%CNAME = CNAME

CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_BMATRIX, NSCARC_STORAGE_CREATE, -1, -1, -1, -1, -1, -1, -1, -1, CNAME, CSCOPE)

WRITE(CINFO,'(A,A,I2.2,A)') TRIM(CNAME),'_LEV',NL,'.AUX'
CALL SCARC_ALLOCATE_REAL1(A%AUX, 1, A%N_DIAG, NSCARC_INIT_ZERO, CINFO, CSCOPE)
WRITE(CINFO,'(A,A,I2.2,A)') TRIM(CNAME),'_LEV',NL,'.VAL'
CALL SCARC_ALLOCATE_REAL2(A%VAL, 1, A%N_DIAG, 1, A%N_STENCIL, NSCARC_INIT_ZERO, CINFO, CSCOPE)

END SUBROUTINE SCARC_ALLOCATE_BMATRIX


! ------------------------------------------------------------------------------------------------
!> \brief Deallocate matrix in bandwise storage format
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DEALLOCATE_BMATRIX(A, CNAME, CSCOPE)
TYPE (SCARC_BMATRIX_TYPE), INTENT(INOUT) :: A
CHARACTER(*), INTENT(IN) :: CNAME, CSCOPE

A%N_STENCIL   = 0
A%N_CONDENSED = 0
A%N_VAL       = 0
A%N_DIAG      = 0

CALL SCARC_UPDATE_STORAGE(NSCARC_DATA_BMATRIX, NSCARC_STORAGE_REMOVE, -1, -1, -1, -1, -1, -1, -1, -1, CNAME, CSCOPE)

IF (ALLOCATED(A%AUX))    CALL SCARC_DEALLOCATE_REAL1 (A%AUX, 'A%AUX', CSCOPE)
IF (ALLOCATED(A%VAL))    CALL SCARC_DEALLOCATE_REAL2 (A%VAL, 'A%VAL', CSCOPE)
IF (ALLOCATED(A%RELAX))  CALL SCARC_DEALLOCATE_REAL2 (A%RELAX, 'A%RELAX', CSCOPE)
IF (ALLOCATED(A%RELAXD)) CALL SCARC_DEALLOCATE_REAL1 (A%RELAXD, 'A%RELAXD', CSCOPE)
A%STENCIL = 0

A%OFFSET = 0
A%LENGTH = 0
A%SOURCE = 0
A%TARGET = 0

END SUBROUTINE SCARC_DEALLOCATE_BMATRIX

END MODULE SCARC_STORAGE


