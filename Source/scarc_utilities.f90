!//////////////////////////////////////////////////////////////////////////////////////////////////////
!
! MODULE SCARC_UTILITIES
!
!> \brief Provide a set of helper routines that are needed at different points in the code.
!
!//////////////////////////////////////////////////////////////////////////////////////////////////////
MODULE SCARC_UTILITIES
  
USE GLOBAL_CONSTANTS
USE MESH_VARIABLES, ONLY: MESHES
USE SCARC_CONSTANTS
USE SCARC_TYPES
USE SCARC_VARIABLES
USE MPI

IMPLICIT NONE

CONTAINS

! -----------------------------------------------------------------------------
!> \brief Check if two meshes are neighbors
! -----------------------------------------------------------------------------
LOGICAL FUNCTION ARE_NEIGHBORS(NM, NOM)
USE SCARC_POINTERS, ONLY: OM
INTEGER, INTENT(IN) :: NM, NOM

ARE_NEIGHBORS = .TRUE.

OM => MESHES(NM)%OMESH(NOM)
IF (OM%NIC_R == 0 .AND. OM%NIC_S == 0) ARE_NEIGHBORS = .FALSE.

END FUNCTION ARE_NEIGHBORS


! ----------------------------------------------------------------------------------------------------
!> \brief Assign handles to currently used grid type
!  This routine assumes, that L already points to the correct level NL of mesh NL and
!  additionally sets the requested discretization type
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SET_GRID_TYPE(NTYPE)
INTEGER, INTENT(IN) :: NTYPE

SELECT CASE (NTYPE)
   CASE (NSCARC_GRID_STRUCTURED)
      PRES_ON_WHOLE_DOMAIN = .TRUE.
      TYPE_GRID = NSCARC_GRID_STRUCTURED
      IS_STRUCTURED   = .TRUE.
      IS_UNSTRUCTURED = .FALSE.
   CASE (NSCARC_GRID_UNSTRUCTURED)
      PRES_ON_WHOLE_DOMAIN = .FALSE.
      TYPE_GRID = NSCARC_GRID_UNSTRUCTURED
      IS_STRUCTURED   = .FALSE.
      IS_UNSTRUCTURED = .TRUE.
END SELECT

END SUBROUTINE SCARC_SET_GRID_TYPE


! ----------------------------------------------------------------------------------------------------
!> \brief Assign handles to currently used grid type
!  This routine assumes, that L already points to the correct level NL of mesh NL and
!  additionally sets the requested discretization type
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SET_SYSTEM_TYPE(NGRID, NMATRIX)
INTEGER, INTENT(IN) :: NGRID, NMATRIX

SELECT CASE (NMATRIX)
   CASE (NSCARC_MATRIX_POISSON)
      IS_POISSON = .TRUE.
      IS_LAPLACE = .FALSE.
   CASE (NSCARC_MATRIX_LAPLACE)
      IS_POISSON = .FALSE.
      IS_LAPLACE = .TRUE.
END SELECT

SELECT CASE (NGRID)
   CASE (NSCARC_GRID_STRUCTURED)
      PRES_ON_WHOLE_DOMAIN = .TRUE.
      TYPE_GRID = NSCARC_GRID_STRUCTURED
      IS_STRUCTURED   = .TRUE.
      IS_UNSTRUCTURED = .FALSE.
   CASE (NSCARC_GRID_UNSTRUCTURED)
      PRES_ON_WHOLE_DOMAIN = .FALSE.
      TYPE_GRID = NSCARC_GRID_UNSTRUCTURED
      IS_STRUCTURED   = .FALSE.
      IS_UNSTRUCTURED = .TRUE.
END SELECT

END SUBROUTINE SCARC_SET_SYSTEM_TYPE


! ------------------------------------------------------------------------------------------------
!> \brief Get full text information about the data type of the currently processed array
! ------------------------------------------------------------------------------------------------
CHARACTER(10) FUNCTION SCARC_GET_DATA_TYPE(NDATA)
INTEGER, INTENT(IN) :: NDATA

SELECT CASE (NDATA)
   CASE (NSCARC_DATA_INTEGER)
      SCARC_GET_DATA_TYPE = 'INTEGER'
   CASE (NSCARC_DATA_REAL_EB)
      SCARC_GET_DATA_TYPE = 'REAL_EB'
   CASE (NSCARC_DATA_REAL_FB)
      SCARC_GET_DATA_TYPE = 'REAL_FB'
   CASE (NSCARC_DATA_LOGICAL)
      SCARC_GET_DATA_TYPE = 'LOGICAL'
   CASE (NSCARC_DATA_CMATRIX)
      SCARC_GET_DATA_TYPE = 'CMATRIX'
   CASE (NSCARC_DATA_BMATRIX)
      SCARC_GET_DATA_TYPE = 'BMATRIX'
   CASE DEFAULT
      SCARC_GET_DATA_TYPE = ' '
END SELECT

END FUNCTION SCARC_GET_DATA_TYPE


! ------------------------------------------------------------------------------------------------
!> \brief Get full text information about the dimension of the currently processed array
! ------------------------------------------------------------------------------------------------
CHARACTER(10) FUNCTION SCARC_GET_DIMENSION(NDIM)
INTEGER, INTENT(IN) :: NDIM

SELECT CASE (NDIM)
   CASE (1)
      SCARC_GET_DIMENSION = '1'
   CASE (2)
      SCARC_GET_DIMENSION = '2'
   CASE (3)
      SCARC_GET_DIMENSION = '3'
   CASE DEFAULT
      SCARC_GET_DIMENSION = '0'
END SELECT

END FUNCTION SCARC_GET_DIMENSION


! ------------------------------------------------------------------------------------------------
!> \brief Get full text information about the initialization type of the currently processed array
! ------------------------------------------------------------------------------------------------
CHARACTER(10) FUNCTION SCARC_GET_INIT_TYPE(NINIT, NDATA, NSTATE)
INTEGER, INTENT(IN) :: NINIT, NDATA, NSTATE

SELECT CASE (NINIT)
   CASE (NSCARC_INIT_UNDEF)
      SCARC_GET_INIT_TYPE = 'UNDEF'
   CASE (NSCARC_INIT_NONE)
      SCARC_GET_INIT_TYPE = 'NONE'
   CASE (NSCARC_INIT_MINUS)
      SCARC_GET_INIT_TYPE = 'MINUS'
   CASE (NSCARC_INIT_ZERO)
      SCARC_GET_INIT_TYPE = 'ZERO'
   CASE (NSCARC_INIT_ONE)
      SCARC_GET_INIT_TYPE = 'ONE'
   CASE (NSCARC_INIT_TRUE)
      SCARC_GET_INIT_TYPE = 'TRUE'
   CASE (NSCARC_INIT_FALSE)
      SCARC_GET_INIT_TYPE = 'FALSE'
   CASE (NSCARC_INIT_HUGE)
      SCARC_GET_INIT_TYPE = 'HUGE'
   CASE DEFAULT
      SCARC_GET_INIT_TYPE = ' '
END SELECT

IF (NDATA == NSCARC_DATA_CMATRIX .OR. NDATA == NSCARC_DATA_BMATRIX) SCARC_GET_INIT_TYPE = ' '
IF (NSTATE == NSCARC_STORAGE_REMOVE) SCARC_GET_INIT_TYPE = ' '

END FUNCTION SCARC_GET_INIT_TYPE


! ------------------------------------------------------------------------------------------------
!> \brief Get type of matrix storage scheme for specified grid level
! ------------------------------------------------------------------------------------------------
INTEGER FUNCTION SCARC_GET_MATRIX_TYPE(NL)
INTEGER, INTENT(IN) :: NL

IF (NL == NLEVEL_MAX .AND. TYPE_COARSE == NSCARC_COARSE_DIRECT) THEN
   SCARC_GET_MATRIX_TYPE = NSCARC_MATRIX_COMPACT
ELSE
   SCARC_GET_MATRIX_TYPE = TYPE_MATRIX
ENDIF

END FUNCTION SCARC_GET_MATRIX_TYPE


! ------------------------------------------------------------------------------------------------
!> \brief Check if a subdiagonal entry must be computed in a specified coordinate direction
! If a structured discretization is used, then subdiagonals are built in every direction
! Else check the type of the neighboring cell in direction IOR0
! ------------------------------------------------------------------------------------------------
LOGICAL FUNCTION IS_VALID_DIRECTION(IX, IY, IZ, IOR0)
USE SCARC_POINTERS, ONLY: L, G
INTEGER, INTENT(IN)  :: IX, IY, IZ, IOR0
INTEGER :: IC_INDEX, IW_INDEX

IS_VALID_DIRECTION = .FALSE.
IF (TWO_D .AND. ABS(IOR0) == 2) RETURN

SELECT CASE (TYPE_GRID)
   CASE (NSCARC_GRID_STRUCTURED)
      IS_VALID_DIRECTION = .TRUE.                                                ! always build subdiagonals
      RETURN
   CASE (NSCARC_GRID_UNSTRUCTURED)
      IC_INDEX = L%CELL_INDEX_PTR(IX, IY, IZ)                               ! cell index of corresponding cell
      IW_INDEX = 0
      IF (IC_INDEX /= 0) IW_INDEX  = L%WALL_INDEX_PTR(IC_INDEX, -IOR0)      ! check its wall index

      IF (IW_INDEX == 0) THEN                                               ! if zero, build subdiagonal
         IS_VALID_DIRECTION = .TRUE.
         RETURN
      ELSE                                                                  ! if not, only build along interfaces
         IF (TYPE_SCOPE(0) == NSCARC_SCOPE_GLOBAL .AND. G%WALL(IW_INDEX)%BOUNDARY_TYPE== INTERPOLATED_BOUNDARY) THEN
            IS_VALID_DIRECTION = .TRUE.
            RETURN
         ENDIF
      ENDIF
END SELECT
RETURN

END FUNCTION IS_VALID_DIRECTION


! ----------------------------------------------------------------------------------------------------
!> \brief Get grid permutation (MGM only)
! ----------------------------------------------------------------------------------------------------
INTEGER FUNCTION GET_PERM(JC)
USE SCARC_POINTERS, ONLY : G
INTEGER, INTENT(IN) :: JC
GET_PERM = -1
IF (JC > 0 .AND. JC <= G%NC) GET_PERM = G%PERM_FW(JC)
END FUNCTION GET_PERM


! ----------------------------------------------------------------------------------------------------
!> \brief Filter out mean value
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_FILTER_MEANVALUE(NV, NL)
USE SCARC_POINTERS, ONLY: L, G, VC, SCARC_POINT_TO_GRID, SCARC_POINT_TO_VECTOR
INTEGER, INTENT(IN) :: NV, NL
INTEGER :: NM, IC, I, J, K

MESH_REAL = 0.0_EB
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
   VC => SCARC_POINT_TO_VECTOR(NM, NL, NV)
   DO IC = 1, G%NC
      MESH_REAL(NM) = MESH_REAL(NM) + VC(IC)
   ENDDO
ENDDO

IF (N_MPI_PROCESSES > 1) &
   CALL MPI_ALLGATHERV(MPI_IN_PLACE,1,MPI_INTEGER,MESH_REAL,COUNTS,DISPLS,&
                       MPI_DOUBLE_PRECISION,MPI_COMM_WORLD,IERROR)

GLOBAL_REAL = SUM(MESH_REAL(1:NMESHES))/REAL(NC_GLOBAL(NL))

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
   VC  => SCARC_POINT_TO_VECTOR(NM, NL, NV)
   DO K = 1, L%NZ
      DO J = 1, L%NY
         DO I = 1, L%NX
            IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE
            IC = G%CELL_NUMBER(I,J,K)
            VC(IC) = VC(IC) - GLOBAL_REAL
         ENDDO
      ENDDO
   ENDDO
ENDDO

END SUBROUTINE SCARC_FILTER_MEANVALUE


! ------------------------------------------------------------------------------------------------
!> \brief Restore last cell of last mesh
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_RESTORE_LAST_CELL (XX, NL)
USE SCARC_POINTERS, ONLY: S, VC, SCARC_POINT_TO_VECTOR
INTEGER, INTENT(IN) :: XX, NL

IF (UPPER_MESH_INDEX /= NMESHES .OR. TYPE_RELAX == NSCARC_RELAX_FFT) RETURN
S => SCARC(UPPER_MESH_INDEX)

VC => SCARC_POINT_TO_VECTOR (UPPER_MESH_INDEX, NL, XX)
VC(S%NC) = S%RHS_END

END SUBROUTINE SCARC_RESTORE_LAST_CELL


! ------------------------------------------------------------------------------------------------
!> \brief Determine if cell should be considered during packing of zone numbers
! ------------------------------------------------------------------------------------------------
LOGICAL FUNCTION SCARC_FORBIDDEN_ZONE(SEND_BUFFER_INT, IZ, ICG1, ICG2)
INTEGER, DIMENSION(:), INTENT(IN) :: SEND_BUFFER_INT
INTEGER, INTENT(IN) :: IZ, ICG1, ICG2
INTEGER :: LL, ICG

SCARC_FORBIDDEN_ZONE = .FALSE.
LL = 5
DO ICG = ICG1, ICG2
   IF (SEND_BUFFER_INT(LL) == IZ) THEN
      SCARC_FORBIDDEN_ZONE = .TRUE.
      RETURN
   ENDIF
   LL = LL + 4
ENDDO
END FUNCTION SCARC_FORBIDDEN_ZONE



! ------------------------------------------------------------------------------------------------
!> \brief Check if difference of two values is less than a given tolerance
! ------------------------------------------------------------------------------------------------
LOGICAL FUNCTION MATCH (VAL1, VAL2)
REAL (EB), INTENT(IN) :: VAL1, VAL2
REAL (EB) :: TOL
TOL = 1.0E-10_EB
MATCH = .FALSE.
IF (Abs(VAL1-VAL2) <= TOL) MATCH = .TRUE.
RETURN
END FUNCTION MATCH


! ------------------------------------------------------------------------------------------------
!> \brief Control multigrid cycling (F/V/W)
! Note: NLEVEL_MIN corresponds to finest level, NLEVEL_MAX to coarsest level
! ------------------------------------------------------------------------------------------------
INTEGER FUNCTION SCARC_CYCLING_CONTROL(NTYPE, NL)
USE SCARC_POINTERS, ONLY: MG
INTEGER, INTENT(IN) :: NTYPE, NL
INTEGER :: NM, IL, ICYCLE

SELECT CASE (NTYPE)

   ! Initialize cycle counts at beginning of multigrid method
 
   CASE (NSCARC_CYCLING_SETUP)
   
      DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   
         MG => SCARC(NM)%LEVEL(NL)%MG
         MG%CYCLING(2)=1
   
         DO IL = NLEVEL_MIN+1, NLEVEL_MAX - 1
            MG => SCARC(NM)%LEVEL(IL)%MG
            IF (TYPE_CYCLING==NSCARC_CYCLING_F) THEN
               MG%CYCLING(2)=2
            ELSE
               MG%CYCLING(2)=TYPE_CYCLING
            ENDIF
         ENDDO
      ENDDO
   
      ICYCLE = NSCARC_CYCLING_NEXT
   
 
   ! Reset cycle counts at beginning of each new multigrid iteration
 
   CASE (NSCARC_CYCLING_RESET)
   
      DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
         DO IL = NLEVEL_MIN, NLEVEL_MAX
            MG => SCARC(NM)%LEVEL(IL)%MG
            MG%CYCLING(1)=MG%CYCLING(2)
         ENDDO
      ENDDO
      ICYCLE = NSCARC_CYCLING_NEXT
   
 
   ! Determine where to proceed with cycling
 
   CASE (NSCARC_CYCLING_PROCEED)
   
      DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   
         MG => SCARC(NM)%LEVEL(NL)%MG
         MG%CYCLING(1)=MG%CYCLING(1)-1
   
         IF (MG%CYCLING(1)==0) THEN
            IF (TYPE_CYCLING==NSCARC_CYCLING_F) THEN
               MG%CYCLING(1)=1
            ELSE
               MG%CYCLING(1)=MG%CYCLING(2)
            ENDIF
            IF (NL == NLEVEL_MIN) THEN
               ICYCLE = NSCARC_CYCLING_EXIT
            ELSE
               ICYCLE = NSCARC_CYCLING_POSTSMOOTH
            ENDIF
         ELSE
            IF (NL == NLEVEL_MIN) THEN
               ICYCLE = NSCARC_CYCLING_EXIT
            ELSE
               ICYCLE = NSCARC_CYCLING_NEXT
            ENDIF
         ENDIF
   
      ENDDO
   
END SELECT

SCARC_CYCLING_CONTROL = ICYCLE
RETURN

END FUNCTION SCARC_CYCLING_CONTROL

END MODULE SCARC_UTILITIES


