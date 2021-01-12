!//////////////////////////////////////////////////////////////////////////////////////////////////////
!
! MODULE SCARC_POINTERS
!
!> \brief Define and organize a series of pointers to specify the different meshes, grid levels, 
!   discretizations and matrices, etc. in combination with corresponding methods to set them
!
!//////////////////////////////////////////////////////////////////////////////////////////////////////
MODULE SCARC_POINTERS

USE GLOBAL_CONSTANTS
USE PRECISION_PARAMETERS, ONLY: EB, FB
USE MESH_VARIABLES, ONLY: MESHES, MESH_TYPE, OMESH_TYPE, WALL_TYPE, EXTERNAL_WALL_TYPE
USE SCARC_CONSTANTS
USE SCARC_TYPES
USE SCARC_VARIABLES


IMPLICIT NONE

TYPE (MESH_TYPE), POINTER :: M=>NULL()                  !< Pointer to specified mesh (based on MESHES from base code)
TYPE (OMESH_TYPE), POINTER :: OM=>NULL()                !< Pointer to specified neighboring mesh (based on OMESH from base code)
TYPE (WALL_TYPE), POINTER :: MWC=>NULL()                !< Pointer to specified wall cell (based on WALL from base code)

TYPE (EXTERNAL_WALL_TYPE), POINTER :: EWC=>NULL()       !< Pointer to specified external wall cell

TYPE (SCARC_ALLOCATION_TYPE), POINTER :: AL=>NULL()     !< Pointer to allocated structure within ScaRC memory management

TYPE (SCARC_TYPE), POINTER :: S=>NULL()                 !< Pointer to ScaRC-structure on a specified mesh
TYPE (SCARC_OSCARC_TYPE), POINTER :: OS=>NULL()         !< Pointer to ScaRC-structure on a specified neighboring

TYPE (SCARC_LEVEL_TYPE), POINTER :: L=>NULL()           !< Pointer to specified level
TYPE (SCARC_LEVEL_TYPE), POINTER :: LF=>NULL()          !< Pointer to specified fine level
TYPE (SCARC_LEVEL_TYPE), POINTER :: LC=>NULL()          !< Pointer to specified coarse level
TYPE (SCARC_LEVEL_TYPE), POINTER :: OL=>NULL()          !< Pointer to specified level on neighboring mesh
TYPE (SCARC_LEVEL_TYPE), POINTER :: OLF=>NULL()         !< Pointer to specified fine level on neighboring mesh
TYPE (SCARC_LEVEL_TYPE), POINTER :: OLC=>NULL()         !< Pointer to specified coarse level on neighboring mesh

TYPE (SCARC_GRID_TYPE), POINTER :: G=>NULL()            !< Pointer to specified grid discretization
TYPE (SCARC_GRID_TYPE), POINTER :: GF=>NULL()           !< Pointer to specified fine grid discretization
TYPE (SCARC_GRID_TYPE), POINTER :: GC=>NULL()           !< Pointer to specified coarse grid discretization
TYPE (SCARC_GRID_TYPE), POINTER :: OG=>NULL()           !< Pointer to specified grid discretization
TYPE (SCARC_GRID_TYPE), POINTER :: OGF=>NULL()          !< Pointer to specified fine grid discretization on neighboring mesh
TYPE (SCARC_GRID_TYPE), POINTER :: OGC=>NULL()          !< Pointer to specified coarse grid discretization on neighboring mesh

TYPE (SCARC_FACE_TYPE), POINTER :: F=>NULL()            !< Pointer to specified face of grid
TYPE (SCARC_FACE_TYPE), POINTER :: FF=>NULL()           !< Pointer to specified face of fine grid level
TYPE (SCARC_FACE_TYPE), POINTER :: FC=>NULL()           !< Pointer to specified face of coarse grid level

TYPE (SCARC_OBST_TYPE), POINTER :: OB=>NULL()           !< Pointer to specified obstruction
TYPE (SCARC_OBST_TYPE), POINTER :: OBF=>NULL()          !< Pointer to specified obstruction of fine grid level
TYPE (SCARC_OBST_TYPE), POINTER :: OBC=>NULL()          !< Pointer to specified obstruction of coarse grid level

TYPE (SCARC_WALL_TYPE), POINTER :: GWC=>NULL()          !< Pointer to specified wall cell

TYPE (SCARC_WALL_TYPE), DIMENSION(:), POINTER :: W=>NULL()    !< Pointer to complete wall structure
TYPE (SCARC_WALL_TYPE), DIMENSION(:), POINTER :: WF=>NULL()   !< Pointer to wall structure on fine grid level
TYPE (SCARC_WALL_TYPE), DIMENSION(:), POINTER :: WC=>NULL()   !< Pointer to wall structure on coarse grid level

TYPE (SCARC_SUBDIVISION_TYPE), POINTER :: SUB=>NULL()         !< Pointer to subdivision structure (only shortcut)

TYPE (SCARC_SOLVER_TYPE), POINTER :: SV=>NULL()               !< Pointer to ScaRC solver structure
TYPE (SCARC_SOLVER_TYPE), POINTER :: SVP=>NULL()              !< Pointer to parent ScaRC solver structure

TYPE (SCARC_STAGE_TYPE),  POINTER :: ST=>NULL()               !< Pointer to solver stage structure
TYPE (SCARC_STAGE_TYPE),  POINTER :: STP=>NULL()              !< Pointer to parent solver stage structure

TYPE (SCARC_FFT_TYPE), POINTER :: FFT=>NULL()                 !< Pointer to FFT structure
TYPE (SCARC_MGM_TYPE), POINTER :: MGM=>NULL()                 !< Pointer to McKeeney-Greengard-Mayo structure

TYPE (SCARC_BMATRIX_TYPE), POINTER :: AB=>NULL()       !< Pointer to bandwise matrix structure
TYPE (SCARC_BMATRIX_TYPE), POINTER :: OAB=>NULL()      !< Pointer to neighboring bandwise matrix structure

TYPE (SCARC_CMATRIX_TYPE), POINTER :: A=>NULL()        !< Pointer to compactly stored matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: A1=>NULL()       !< Pointer to compactly stored matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: A2=>NULL()       !< Pointer to compactly stored matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: AC=>NULL()       !< Pointer to compactly stored coarse matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: AF=>NULL()       !< Pointer to compactly stored fine matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: OA=>NULL()       !< Pointer to compactly stored neighboring matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: OAC=>NULL()      !< Pointer to compactly stored coarse neighboring matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: OAF=>NULL()      !< Pointer to compactly stored fine neighboring matrix

TYPE (SCARC_CMATRIX_TYPE), POINTER :: LM=>NULL()     !< Pointer to compactly stored matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: UM=>NULL()     !< Pointer to compactly stored matrix

TYPE (SCARC_CMATRIX_TYPE), POINTER :: P=>NULL()        !< Pointer to compactly stored Prolongation matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: PC=>NULL()       !< Pointer to compactly stored coarse Prolongation matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: PF=>NULL()       !< Pointer to compactly stored fine matrix on coarse grid
TYPE (SCARC_CMATRIX_TYPE), POINTER :: OP=>NULL()       !< Pointer to compactly stored neighboring Prolongation matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: OPC=>NULL()      !< Pointer to compactly stored coarse neighboring Prolongation matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: OPF=>NULL()      !< Pointer to compactly stored fine neighboring Prolongation matrix

TYPE (SCARC_CMATRIX_TYPE), POINTER :: R=>NULL()        !< Pointer to compactly stored Restriction matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: RC=>NULL()       !< Pointer to compactly stored fine Restriction matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: RF=>NULL()       !< Pointer to compactly stored coarse Restriction matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: OR=>NULL()       !< Pointer to compactly stored neighboring Restriction matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: ORC=>NULL()      !< Pointer to compactly stored coarse neighboring Restriction matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: ORF=>NULL()      !< Pointer to compactly stored fine neighboring Restriction matrix

TYPE (SCARC_CMATRIX_TYPE), POINTER :: C=>NULL()        !< Pointer to compactly stored connection matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: CC=>NULL()       !< Pointer to compactly stored coarse connection matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: CF=>NULL()       !< Pointer to compactly stored fine connection matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: OC=>NULL()       !< Pointer to compactly stored neighboring connection matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: OCC=>NULL()      !< Pointer to compactly stored coarse neighboring connection matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: OCF=>NULL()      !< Pointer to compactly stored fine neighboring connection matrix

TYPE (SCARC_CMATRIX_TYPE), POINTER :: Z=>NULL()        !< Pointer to compactly stored Zones matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: ZC=>NULL()       !< Pointer to compactly stored coarse Zones matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: ZF=>NULL()       !< Pointer to compactly stored fine Zones matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: OZ=>NULL()       !< Pointer to compactly stored neighboring Zones matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: OZC=>NULL()      !< Pointer to compactly stored neighboring coarse Zones matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: OZF=>NULL()      !< Pointer to compactly stored neighboring fine Zones matrix

TYPE (SCARC_CMATRIX_TYPE), POINTER :: PP=>NULL()       !< Pointer to compactly stored Poisson-times-Prolongation matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: PPF=>NULL()      !< Pointer to compactly stored fine Poisson-times-Prolongation matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: PPC=>NULL()      !< Pointer to compactly stored coarse Poisson-times-Prolongation matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: OPP=>NULL()      !< Pointer to compactly stored neighboring Poisson-times-Prolongation matrix

TYPE (SCARC_MATRIX_COMPACT_CONDENSED_TYPE),  POINTER :: ACO =>NULL()    !< Pointer to compactly stored condensed Poisson matrix
TYPE (SCARC_MATRIX_BANDWISE_CONDENSED_TYPE), POINTER :: ABCO=>NULL()    !< Pointer to bandwise stored condensed Poisson matrix

TYPE (SCARC_MULTIGRID_TYPE), POINTER :: MG =>NULL()                     !< Pointer to multigrid type

REAL(EB), POINTER, DIMENSION(:) :: XCOR=>NULL()        !< Pointer to vector of node coordinates in x-direction
REAL(EB), POINTER, DIMENSION(:) :: YCOR=>NULL()        !< Pointer to vector of node coordinates in x-direction
REAL(EB), POINTER, DIMENSION(:) :: ZCOR=>NULL()        !< Pointer to vector of node coordinates in x-direction

REAL(EB), POINTER, DIMENSION(:) :: XMID=>NULL()        !< Pointer to vector of cell midpoints in x-direction
REAL(EB), POINTER, DIMENSION(:) :: YMID=>NULL()        !< Pointer to vector of cell midpoints in y-direction
REAL(EB), POINTER, DIMENSION(:) :: ZMID=>NULL()        !< Pointer to vector of cell midpoints in z-direction

REAL(EB), POINTER, DIMENSION(:) :: VC=>NULL()          !< Pointer to vector on coarse grid level
REAL(EB), POINTER, DIMENSION(:) :: VF=>NULL()          !< Pointer to vector on fine grid level
REAL(EB), POINTER, DIMENSION(:) :: V1=>NULL()          !< Pointer to first vector
REAL(EB), POINTER, DIMENSION(:) :: V2=>NULL()          !< Pointer to second vector

REAL(EB), POINTER, DIMENSION(:) :: OH1=>NULL()         !< Pointer to other Poisson solution
REAL(EB), POINTER, DIMENSION(:) :: OH2=>NULL()         !< Pointer to other Laplace solution
REAL(EB), POINTER, DIMENSION(:) :: OH3=>NULL()         !< Pointer to other MGM solution

REAL(EB), POINTER, DIMENSION(:,:,:) :: HP=>NULL()      !< Pointer to pressure vector
REAL(EB), POINTER, DIMENSION(:,:,:) :: H1=>NULL()      !< Pointer to Poisson solution
REAL(EB), POINTER, DIMENSION(:,:,:) :: H2=>NULL()      !< Pointer to Laplace solution
REAL(EB), POINTER, DIMENSION(:,:,:) :: H3=>NULL()      !< Pointer to MGM solution
REAL(EB), POINTER, DIMENSION(:,:,:) :: PRHS=>NULL()    !< Pointer to right hand side vector
REAL(EB), POINTER, DIMENSION(:,:,:) :: UU=>NULL()      !< Pointer to u-velocity vector
REAL(EB), POINTER, DIMENSION(:,:,:) :: VV=>NULL()      !< Pointer to v-velocity vector
REAL(EB), POINTER, DIMENSION(:,:,:) :: WW=>NULL()      !< Pointer to w-velocity vector

REAL(EB), POINTER, DIMENSION(:) ::  RECV_BUFFER_REAL   !< Pointer to double precision receive vector
INTEGER,  POINTER, DIMENSION(:) ::  RECV_BUFFER_INT    !< Pointer to inter receive vector

TYPE (SCARC_MKL_TYPE), POINTER :: MKL=>NULL()          !< Pointer to MKL type
TYPE (SCARC_CMATRIX_TYPE), POINTER :: AS=>NULL()       !< Pointer to symmetric Poisson matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: ACS=>NULL()      !< Pointer to coarse symmetric Poisson matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: AFS=>NULL()      !< Pointer to fine symmetric Poisson matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: OAS=>NULL()      !< Pointer to neighboring symmetric Poisson matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: OACS=>NULL()     !< Pointer to coarse neighboring symmetric Poisson matrix
TYPE (SCARC_CMATRIX_TYPE), POINTER :: OAFS=>NULL()     !< Pointer to fine neighboring symmetric Poisson matrix
REAL(FB), DIMENSION(:), POINTER :: V1_FB=>NULL()       !< Pointer to first single precision vector
REAL(FB), DIMENSION(:), POINTER :: V2_FB=>NULL()       !< Pointer to second single precision vector

!TYPE (SCARC_PRESSURE_TYPE), POINTER :: PR=>NULL()                       !< Pointer to pressure type

CONTAINS




! -----------------------------------------------------------------------------
!> \brief Point to specified mesh
! -----------------------------------------------------------------------------
SUBROUTINE SCARC_POINT_TO_MESH(NM)
!USE SCARC_POINTERS, ONLY: M, S
INTEGER, INTENT(IN) :: NM
M => MESHES(NM)
S => SCARC(NM)
END SUBROUTINE SCARC_POINT_TO_MESH


! -----------------------------------------------------------------------------
!> \brief Point to specified combination of mesh and grid level
! -----------------------------------------------------------------------------
SUBROUTINE SCARC_POINT_TO_LEVEL(NM, NL)
!USE SCARC_POINTERS, ONLY: M, S, L
INTEGER, INTENT(IN) :: NM, NL
M => MESHES(NM)
S => SCARC(NM)
L => S%LEVEL(NL)
END SUBROUTINE SCARC_POINT_TO_LEVEL


! ----------------------------------------------------------------------------------------------------
!> \brief Unset ScaRC pointers
! mainly used to test the correctness of the pointer settings in the different routines
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_POINT_TO_NONE
!USE SCARC_POINTERS, ONLY: M, S, L, G, F, A, P, R, C, Z, W, &
!                                LC, GC, FC, AC, PC, RC, CC, ZC, &
!                                LF, GF, FF, AF, PF, RF, CF, ZF
M => NULL()
S => NULL()
W => NULL()
L => NULL();  LF => NULL();  LC => NULL()
G => NULL();  GF => NULL();  GC => NULL()
F => NULL();  FF => NULL();  FC => NULL()
A => NULL();  AF => NULL();  AC => NULL()
P => NULL();  PF => NULL();  PC => NULL()
R => NULL();  RF => NULL();  RC => NULL()
C => NULL();  CF => NULL();  CC => NULL()
Z => NULL();  ZF => NULL();  ZC => NULL()

END SUBROUTINE SCARC_POINT_TO_NONE


! ----------------------------------------------------------------------------------------------------
!> \brief Point to specified combination of a mesh level and discretization type
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_POINT_TO_GRID (NM, NL)
!USE SCARC_POINTERS, ONLY: M, S, L, G, W
INTEGER, INTENT(IN) ::  NM, NL

CALL SCARC_POINT_TO_NONE

M => MESHES(NM)
S => SCARC(NM)
L => S%LEVEL(NL)
SELECT CASE(TYPE_GRID)
   CASE (NSCARC_GRID_STRUCTURED)
      G => L%STRUCTURED
      G%NW = L%N_WALL_CELLS_EXT                     ! TODO: set it elsewhere
   CASE (NSCARC_GRID_UNSTRUCTURED)
      G => L%UNSTRUCTURED
      G%NW = L%N_WALL_CELLS_EXT + L%N_WALL_CELLS_INT
END SELECT
W => G%WALL

END SUBROUTINE SCARC_POINT_TO_GRID


! ----------------------------------------------------------------------------------------------------
!> \brief Point to specified pairing of mesh levels and discretization types
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_POINT_TO_MULTIGRID(NM, NL1, NL2)
!USE SCARC_POINTERS, ONLY: M, S, LF, LC, GF, GC, WC, WF
INTEGER, INTENT(IN) ::  NM, NL1, NL2

CALL SCARC_POINT_TO_NONE

M => MESHES(NM)
S => SCARC(NM)
LF => SCARC(NM)%LEVEL(NL1)
LC => SCARC(NM)%LEVEL(NL2)
SELECT CASE(TYPE_GRID)
   CASE (NSCARC_GRID_STRUCTURED)
      GF => LF%STRUCTURED
      GC => LC%STRUCTURED
      GF%NW = LF%N_WALL_CELLS_EXT
      GC%NW = LC%N_WALL_CELLS_EXT
   CASE (NSCARC_GRID_UNSTRUCTURED)
      GF => LF%UNSTRUCTURED
      GC => LC%UNSTRUCTURED
      GF%NW = LF%N_WALL_CELLS_EXT + LF%N_WALL_CELLS_INT
      GC%NW = LC%N_WALL_CELLS_EXT + LC%N_WALL_CELLS_INT
END SELECT
WC => GC%WALL
WF => GF%WALL

END SUBROUTINE SCARC_POINT_TO_MULTIGRID


! ---------------------------------------------------------------------------------------------------
!> \brief Point to specified combination of a neighboring mesh level and discretization type
! ---------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_POINT_TO_OTHER_GRID(NM, NOM, NL)
!USE SCARC_POINTERS, ONLY : OS, OL, OLF, OG, OGF
INTEGER, INTENT(IN) :: NM, NOM, NL

OS  => SCARC(NM)%OSCARC(NOM)
OL  => OS%LEVEL(NL)
OLF => OS%LEVEL(NL)

SELECT CASE(TYPE_GRID)
   CASE (NSCARC_GRID_STRUCTURED)
      OG  => OL%STRUCTURED
      OGF => OLF%STRUCTURED
   CASE (NSCARC_GRID_UNSTRUCTURED)
      OG  => OL%UNSTRUCTURED
      OGF => OLF%UNSTRUCTURED
END SELECT

END SUBROUTINE SCARC_POINT_TO_OTHER_GRID


! -----------------------------------------------------------------------------------------------------
!> \brief Point to specified combination of a neighboring mesh level and a discretization type
! -----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_POINT_TO_OTHER_MULTIGRID(NM, NOM, NL1, NL2)
!USE SCARC_POINTERS, ONLY : OS, OLC, OLF, OGC, OGF
INTEGER, INTENT(IN) :: NM, NOM, NL1, NL2

OS  => SCARC(NM)%OSCARC(NOM)
OLF => OS%LEVEL(NL1)
OLC => OS%LEVEL(NL2)

SELECT CASE(TYPE_GRID)
   CASE (NSCARC_GRID_STRUCTURED)
      OGF => OLF%STRUCTURED
      OGC => OLC%STRUCTURED
   CASE (NSCARC_GRID_UNSTRUCTURED)
      OGF => OLF%UNSTRUCTURED
      OGC => OLC%UNSTRUCTURED
END SELECT

END SUBROUTINE SCARC_POINT_TO_OTHER_MULTIGRID


! ----------------------------------------------------------------------------------------------------
!> \brief Point to specified matrix in compact storage technique
! ----------------------------------------------------------------------------------------------------
FUNCTION SCARC_POINT_TO_CMATRIX(G, NTYPE)
TYPE(SCARC_CMATRIX_TYPE), POINTER :: SCARC_POINT_TO_CMATRIX
TYPE(SCARC_GRID_TYPE), POINTER, INTENT(IN) :: G
INTEGER, INTENT(IN) :: NTYPE

SELECT CASE(NTYPE)
   CASE (NSCARC_MATRIX_POISSON_PROL)
      SCARC_POINT_TO_CMATRIX => G%POISSON_PROL
   CASE (NSCARC_MATRIX_CONNECTION)
      SCARC_POINT_TO_CMATRIX => G%CONNECTION
   CASE (NSCARC_MATRIX_POISSON)
      SCARC_POINT_TO_CMATRIX => G%POISSON
#ifdef WITH_SCARC_MKL
   CASE (NSCARC_MATRIX_POISSON_SYM)
      SCARC_POINT_TO_CMATRIX => G%POISSON_SYM
#endif
   CASE (NSCARC_MATRIX_LAPLACE)
      SCARC_POINT_TO_CMATRIX => G%LAPLACE
   CASE (NSCARC_MATRIX_PROLONGATION)
      SCARC_POINT_TO_CMATRIX => G%PROLONGATION
   CASE (NSCARC_MATRIX_RESTRICTION)
      SCARC_POINT_TO_CMATRIX => G%RESTRICTION
   CASE (NSCARC_MATRIX_ZONES)
      SCARC_POINT_TO_CMATRIX => G%ZONES
   CASE (NSCARC_MATRIX_LM)
      SCARC_POINT_TO_CMATRIX => G%LM
   CASE (NSCARC_MATRIX_UM)
      SCARC_POINT_TO_CMATRIX => G%UM
END SELECT

END FUNCTION SCARC_POINT_TO_CMATRIX


! ----------------------------------------------------------------------------------------------------
!> \brief Point to specified matrix in bandwise storage technique
! ----------------------------------------------------------------------------------------------------
FUNCTION SCARC_POINT_TO_BMATRIX(G, NTYPE)
TYPE(SCARC_BMATRIX_TYPE), POINTER :: SCARC_POINT_TO_BMATRIX
TYPE(SCARC_GRID_TYPE), POINTER, INTENT(IN) :: G
INTEGER, INTENT(IN) :: NTYPE

SELECT CASE(NTYPE)
   CASE (NSCARC_MATRIX_POISSON)
      SCARC_POINT_TO_BMATRIX => G%POISSONB
   CASE DEFAULT
      WRITE(*,*) 'No other bandwise matrix available yet except of POISSONB'
END SELECT

END FUNCTION SCARC_POINT_TO_BMATRIX


! ----------------------------------------------------------------------------------------------------
!> \brief Point to specified neighboring matrix in compact storage technique
! ----------------------------------------------------------------------------------------------------
FUNCTION SCARC_POINT_TO_OTHER_CMATRIX(OG, NTYPE)
TYPE(SCARC_CMATRIX_TYPE), POINTER :: SCARC_POINT_TO_OTHER_CMATRIX
TYPE(SCARC_GRID_TYPE), POINTER, INTENT(IN) :: OG
INTEGER, INTENT(IN) :: NTYPE

SELECT CASE(NTYPE)
   CASE (NSCARC_MATRIX_POISSON_PROL)
      SCARC_POINT_TO_OTHER_CMATRIX => OG%POISSON_PROL
   CASE (NSCARC_MATRIX_CONNECTION)
      SCARC_POINT_TO_OTHER_CMATRIX => OG%CONNECTION
   CASE (NSCARC_MATRIX_POISSON)
      SCARC_POINT_TO_OTHER_CMATRIX => OG%POISSON
#ifdef WITH_SCARC_MKL
   CASE (NSCARC_MATRIX_POISSON_SYM)
      SCARC_POINT_TO_OTHER_CMATRIX => OG%POISSON_SYM
#endif
   CASE (NSCARC_MATRIX_PROLONGATION)
      SCARC_POINT_TO_OTHER_CMATRIX => OG%PROLONGATION
   CASE (NSCARC_MATRIX_RESTRICTION)
      SCARC_POINT_TO_OTHER_CMATRIX => OG%RESTRICTION
   CASE (NSCARC_MATRIX_ZONES)
      SCARC_POINT_TO_OTHER_CMATRIX => OG%ZONES
END SELECT

END FUNCTION SCARC_POINT_TO_OTHER_CMATRIX


! ----------------------------------------------------------------------------------------------------
!> \brief Point to specified neighboring matrix in bandwise storage technique
! ----------------------------------------------------------------------------------------------------
FUNCTION SCARC_POINT_TO_OTHER_BMATRIX(OG, NTYPE)
TYPE(SCARC_BMATRIX_TYPE), POINTER :: SCARC_POINT_TO_OTHER_BMATRIX
TYPE(SCARC_GRID_TYPE), POINTER, INTENT(IN) :: OG
INTEGER, INTENT(IN) :: NTYPE

SELECT CASE(NTYPE)
   CASE (NSCARC_MATRIX_POISSON)
      SCARC_POINT_TO_OTHER_BMATRIX => OG%POISSONB
   CASE DEFAULT
      WRITE(*,*) 'No other bandwise matrix available yet except of POISSONB'
END SELECT

END FUNCTION SCARC_POINT_TO_OTHER_BMATRIX


! -----------------------------------------------------------------------------
!> \brief Point to specified integer receive buffer for data exchanges
! -----------------------------------------------------------------------------
FUNCTION SCARC_POINT_TO_BUFFER_INT(NM, NOM, NTYPE)
INTEGER, DIMENSION(:), POINTER :: SCARC_POINT_TO_BUFFER_INT
INTEGER, INTENT(IN) ::  NM, NOM, NTYPE

SCARC_POINT_TO_BUFFER_INT => NULL()
SELECT CASE (NTYPE)
   CASE (0)
      IF (RNODE/=SNODE) THEN
         SCARC_POINT_TO_BUFFER_INT => SCARC(NM)%OSCARC(NOM)%RECV_BUFFER_INT0
      ELSE
         SCARC_POINT_TO_BUFFER_INT => SCARC(NOM)%OSCARC(NM)%SEND_BUFFER_INT0
      ENDIF
   CASE (1)
      IF (RNODE/=SNODE) THEN
         SCARC_POINT_TO_BUFFER_INT => SCARC(NM)%OSCARC(NOM)%RECV_BUFFER_INT
      ELSE
         SCARC_POINT_TO_BUFFER_INT => SCARC(NOM)%OSCARC(NM)%SEND_BUFFER_INT
      ENDIF
END SELECT

END FUNCTION SCARC_POINT_TO_BUFFER_INT


! -----------------------------------------------------------------------------
!> \brief Point to specified integer receive buffer for data exchanges
! -----------------------------------------------------------------------------
FUNCTION SCARC_POINT_TO_BUFFER_REAL(NM, NOM, NTYPE)
REAL(EB), DIMENSION(:), POINTER :: SCARC_POINT_TO_BUFFER_REAL
INTEGER, INTENT(IN) ::  NM, NOM, NTYPE

SCARC_POINT_TO_BUFFER_REAL => NULL()
SELECT CASE (NTYPE)
   CASE (0)
      IF (RNODE/=SNODE) THEN
         SCARC_POINT_TO_BUFFER_REAL => SCARC(NM)%OSCARC(NOM)%RECV_BUFFER_REAL0
      ELSE
         SCARC_POINT_TO_BUFFER_REAL => SCARC(NOM)%OSCARC(NM)%SEND_BUFFER_REAL0
      ENDIF
   CASE (1)
      IF (RNODE/=SNODE) THEN
         SCARC_POINT_TO_BUFFER_REAL => SCARC(NM)%OSCARC(NOM)%RECV_BUFFER_REAL
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'POINT_TO BUFFER_REAL:A:'
WRITE(MSG%LU_DEBUG,'(8E14.6)') SCARC_POINT_TO_BUFFER_REAL(1:16)
#endif
      ELSE
         SCARC_POINT_TO_BUFFER_REAL => SCARC(NOM)%OSCARC(NM)%SEND_BUFFER_REAL
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'POINT_TO BUFFER_REAL:B:'
WRITE(MSG%LU_DEBUG,'(8E14.6)') SCARC_POINT_TO_BUFFER_REAL(1:16)
#endif
      ENDIF
END SELECT

END FUNCTION SCARC_POINT_TO_BUFFER_REAL


! ------------------------------------------------------------------------------------------------
!> \brief Point to specified vector on a given grid level
! ------------------------------------------------------------------------------------------------
FUNCTION SCARC_POINT_TO_VECTOR (NM, NL, NV)
REAL(EB), POINTER, DIMENSION(:) :: SCARC_POINT_TO_VECTOR
INTEGER, INTENT(IN) :: NM, NL, NV

SELECT CASE (NV)

   ! Stage one vectors (for methods on first hierarchical level)

   CASE (NSCARC_VECTOR_ONE_X)
      SCARC_POINT_TO_VECTOR => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_ONE)%X
   CASE (NSCARC_VECTOR_ONE_B)
      SCARC_POINT_TO_VECTOR => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_ONE)%B
   CASE (NSCARC_VECTOR_ONE_D)
      SCARC_POINT_TO_VECTOR => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_ONE)%D
   CASE (NSCARC_VECTOR_ONE_R)
      SCARC_POINT_TO_VECTOR => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_ONE)%R
   CASE (NSCARC_VECTOR_ONE_V)
      SCARC_POINT_TO_VECTOR => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_ONE)%V
   CASE (NSCARC_VECTOR_ONE_Y)
      SCARC_POINT_TO_VECTOR => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_ONE)%Y
   CASE (NSCARC_VECTOR_ONE_Z)
      SCARC_POINT_TO_VECTOR => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_ONE)%Z
#ifdef WITH_SCARC_DEBUG
   CASE (NSCARC_VECTOR_ONE_E)
      SCARC_POINT_TO_VECTOR => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_ONE)%E
#endif


   ! Stage two vectors (for methods on second hierarchical level)

   CASE (NSCARC_VECTOR_TWO_X)
      SCARC_POINT_TO_VECTOR => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_TWO)%X
   CASE (NSCARC_VECTOR_TWO_B)
      SCARC_POINT_TO_VECTOR => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_TWO)%B
   CASE (NSCARC_VECTOR_TWO_D)
      SCARC_POINT_TO_VECTOR => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_TWO)%D
   CASE (NSCARC_VECTOR_TWO_R)
      SCARC_POINT_TO_VECTOR => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_TWO)%R
   CASE (NSCARC_VECTOR_TWO_V)
      SCARC_POINT_TO_VECTOR => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_TWO)%V
   CASE (NSCARC_VECTOR_TWO_Y)
      SCARC_POINT_TO_VECTOR => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_TWO)%Y
   CASE (NSCARC_VECTOR_TWO_Z)
      SCARC_POINT_TO_VECTOR => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_TWO)%Z
#ifdef WITH_SCARC_DEBUG
   CASE (NSCARC_VECTOR_TWO_E)
      SCARC_POINT_TO_VECTOR => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_TWO)%E
#endif
END SELECT

END FUNCTION SCARC_POINT_TO_VECTOR


! ------------------------------------------------------------------------------------------------
!> \brief Point to specified vector on a given grid level (single precision version)
! ------------------------------------------------------------------------------------------------
FUNCTION SCARC_POINT_TO_VECTOR_FB(NM, NL, NV)
REAL(FB), POINTER, DIMENSION(:) :: SCARC_POINT_TO_VECTOR_FB
INTEGER, INTENT(IN) :: NM, NL, NV


SELECT CASE (NV)

   ! Stage one vectors (for methods on first hierarchical level)

   CASE (NSCARC_VECTOR_ONE_X)
      SCARC_POINT_TO_VECTOR_FB => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_ONE)%X_FB
   CASE (NSCARC_VECTOR_ONE_B)
      SCARC_POINT_TO_VECTOR_FB => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_ONE)%B_FB
   CASE (NSCARC_VECTOR_ONE_R)
      SCARC_POINT_TO_VECTOR_FB => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_ONE)%R_FB
   CASE (NSCARC_VECTOR_ONE_V)
      SCARC_POINT_TO_VECTOR_FB => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_ONE)%V_FB


   ! Stage two vectors (for methods on second hierarchical level)

   CASE (NSCARC_VECTOR_TWO_X)
      SCARC_POINT_TO_VECTOR_FB => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_TWO)%X_FB
   CASE (NSCARC_VECTOR_TWO_B)
      SCARC_POINT_TO_VECTOR_FB => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_TWO)%B_FB
   CASE (NSCARC_VECTOR_TWO_R)
      SCARC_POINT_TO_VECTOR_FB => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_TWO)%R_FB
   CASE (NSCARC_VECTOR_TWO_V)
      SCARC_POINT_TO_VECTOR_FB => SCARC(NM)%LEVEL(NL)%STAGE(NSCARC_STAGE_TWO)%V_FB
END SELECT

END FUNCTION SCARC_POINT_TO_VECTOR_FB


! ------------------------------------------------------------------------------------------------
!> \brief Point to pressure vector in predictor or corrector
! ------------------------------------------------------------------------------------------------
FUNCTION SCARC_POINT_TO_HVECTOR(NM, NV)
REAL(EB), POINTER, DIMENSION(:,:,:) :: SCARC_POINT_TO_HVECTOR
INTEGER, INTENT(IN) :: NM, NV
SELECT CASE (NV)
   CASE (NSCARC_VECTOR_H)
      SCARC_POINT_TO_HVECTOR => MESHES(NM)%H
   CASE (NSCARC_VECTOR_HS)
      SCARC_POINT_TO_HVECTOR => MESHES(NM)%HS
END SELECT
END FUNCTION SCARC_POINT_TO_HVECTOR

END MODULE SCARC_POINTERS


