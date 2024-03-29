!=======================================================================================================================
!
! MODULE SCARC_MATRICES
!
!> \brief Setup and organize the matrix types needed for the different ScaRC/UscaRC solvers
!
!   This inlcudes local/global Poisson and Laplace matrices, their boundary conditions and 
!   a corresponding condensing in the purely Neumann case
!
!=======================================================================================================================
MODULE SCARC_MATRICES
  
USE GLOBAL_CONSTANTS
USE PRECISION_PARAMETERS, ONLY: EB, FB
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
USE COMP_FUNCTIONS, ONLY: CURRENT_TIME
USE SCARC_CONSTANTS
USE SCARC_UTILITIES
USE SCARC_STORAGE
USE SCARC_MPI, ONLY: SCARC_EXCHANGE
USE SCARC_GRIDS, ONLY: SCARC_SETUP_GLOBAL_CELL_MAPPING

IMPLICIT NONE (TYPE,EXTERNAL)

CONTAINS


! ------------------------------------------------------------------------------------------------------------------
!> \brief Setup sizes for Poisson matrices on requested grid levels
! ------------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_POISSON_REQUIREMENTS
INTEGER :: NL

SELECT CASE (TYPE_METHOD)

   ! -------- Global Krylov method

   CASE (NSCARC_METHOD_KRYLOV)
   
      CALL SCARC_SET_GRID_TYPE (TYPE_GRID)                      ! process specified discretization type
      CALL SCARC_SETUP_POISSON_SIZES (NLEVEL_MIN)               ! setup sizes on finest level
   
      IF (HAS_TWO_LEVELS .AND. .NOT.HAS_AMG_LEVELS) &
         CALL SCARC_SETUP_POISSON_SIZES (NLEVEL_MAX)            ! twolevel-precon: also setup size for coarse level
   
      IF (IS_CG_GMG) THEN                                                   
         DO NL=NLEVEL_MIN+1, NLEVEL_MAX
            CALL SCARC_SETUP_POISSON_SIZES (NL)                 ! GMG-precon: also setup size for all other levels
         ENDDO
      ENDIF
   
   ! -------- Global Multigrid method

   CASE (NSCARC_METHOD_MULTIGRID)
   
      CALL SCARC_SET_GRID_TYPE (TYPE_GRID)                      ! process specified discretization type
      SELECT CASE (TYPE_MULTIGRID)
         CASE (NSCARC_MULTIGRID_GEOMETRIC)                                   
            DO NL=NLEVEL_MIN, NLEVEL_MAX
               CALL SCARC_SETUP_POISSON_SIZES (NL)              ! GMG: setup size for all levels
            ENDDO
         CASE (NSCARC_MULTIGRID_ALGEBRAIC)
            CALL SCARC_SETUP_POISSON_SIZES (NLEVEL_MIN)         ! AMG: setup sizes only on finest level
      END SELECT
   
   ! -------- Global MGM method - currently just proof of concept

   CASE (NSCARC_METHOD_MGM)
   
      CALL SCARC_SET_GRID_TYPE (NSCARC_GRID_STRUCTURED)         ! First process structured discretization
      CALL SCARC_SETUP_POISSON_SIZES (NLEVEL_MIN)        
   
      CALL SCARC_SET_GRID_TYPE (NSCARC_GRID_UNSTRUCTURED)       ! Then process unstructured discretization
      IF (SCARC_MGM_CHECK_LAPLACE .OR. SCARC_MGM_EXACT_INITIAL) &
         CALL SCARC_SETUP_POISSON_SIZES (NLEVEL_MIN)            ! ... for global Poisson matrix (only if requested)
      CALL SCARC_SETUP_LOCAL_LAPLACE_SIZES (NLEVEL_MIN)         ! ... for local Laplace matrices
   
END SELECT 

END SUBROUTINE SCARC_SETUP_POISSON_REQUIREMENTS


! ------------------------------------------------------------------------------------------------------------------
!> \brief Assemble separable Poisson matrices on requested grid levels and set boundary conditions
! ------------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_POISSON_SEPARABLE
INTEGER :: NM, NL

MESHES_POISSON_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   SELECT_SCARC_METHOD: SELECT CASE (TYPE_METHOD)

      ! ---------- Krylov method (CG) as main solver, different preconditioners possible

      CASE (NSCARC_METHOD_KRYLOV)

         ! For all different possible Krylov variants, first setup Poisson matrix on finest level including BC's 

         CALL SCARC_SETUP_POISSON (NM, NLEVEL_MIN)
         CALL SCARC_SETUP_BOUNDARY(NM, NLEVEL_MIN)

         ! Depending on the requested preconditioner, also assemble the Poisson matrix with BC's on specific coarser levels

         SELECT_KRYLOV_PRECON: SELECT CASE (TYPE_PRECON)

            ! In case of multigrid as preconditioner:
            ! only build higher level structures in case of geometric multigrid (algebraic variant is done elsewhere)

            CASE (NSCARC_RELAX_GMG)

               IF (IS_CG_GMG) THEN
                  DO NL = NLEVEL_MIN+1, NLEVEL_MAX
                     CALL SCARC_SETUP_POISSON (NM, NL)
                     CALL SCARC_SETUP_BOUNDARY(NM, NL)
                  ENDDO
               ENDIF

#ifdef WITH_MKL
            ! In case of LU-decomposition as preconditioner
            ! locally acting: PARDISO from MKL as preconditioners on fine level with possible coarse grid correction

            CASE (NSCARC_RELAX_MKL)

               IF (TYPE_SCOPE(1) == NSCARC_SCOPE_LOCAL .AND. HAS_TWO_LEVELS .AND. .NOT.HAS_AMG_LEVELS) THEN
                  CALL SCARC_SETUP_POISSON (NM, NLEVEL_MAX)
                  CALL SCARC_SETUP_BOUNDARY(NM, NLEVEL_MAX)
               ENDIF
#endif

            ! in case of default preconditioners (JACOBI/SSOR/FFT/...):
            ! if there is an additional coarse grid correction which is NOT AMG-based, 
            ! then also assemble matrix on coarse grid level

            CASE DEFAULT
   
               IF (HAS_TWO_LEVELS .AND. .NOT.HAS_AMG_LEVELS) THEN
                  CALL SCARC_SETUP_POISSON (NM, NLEVEL_MAX)
                  CALL SCARC_SETUP_BOUNDARY(NM, NLEVEL_MAX)
               ENDIF

         END SELECT SELECT_KRYLOV_PRECON

      ! ---------- Multigrid as main solver

      CASE (NSCARC_METHOD_MULTIGRID)

         ! For all different possible multigrid-variants, first setup Poisson matrix on finest level including BC's 

         CALL SCARC_SETUP_POISSON (NM, NLEVEL_MIN)
         CALL SCARC_SETUP_BOUNDARY(NM, NLEVEL_MIN)

         ! On case of a  geometric multigrid, assemble standard n-point-matrix hierarchy on all coarser levels, too
         ! Note: in case of an algebraic multigrid, this will be done in a separate routine later

         IF (TYPE_MULTIGRID == NSCARC_MULTIGRID_GEOMETRIC) THEN
            DO NL = NLEVEL_MIN + 1, NLEVEL_MAX
               CALL SCARC_SETUP_POISSON (NM, NL)
               CALL SCARC_SETUP_BOUNDARY(NM, NL)
            ENDDO
         ENDIF

      ! ---------- McKenny-Greengard-Mayo method:
      ! Solving for the structured and unstructured Poisson matrix
      ! Assemble both, the structured and unstructured Poisson matrix
      ! temporarily they will be stored separately in matrices AC and ACU due to the different
      ! settings along internal boundary cells,
      ! in the medium term, a toggle mechanism will be implemented which only switches the corresponding
      ! entries while keeping the entries which are the same for both discretization types

      CASE (NSCARC_METHOD_MGM)
   
         ! First assemble structured matrix with inhomogeneous boundary conditions

         TYPE_SCOPE(0) = NSCARC_SCOPE_GLOBAL
         CALL SCARC_SET_GRID_TYPE (NSCARC_GRID_STRUCTURED)
         CALL SCARC_SETUP_POISSON (NM, NLEVEL_MIN)
         CALL SCARC_SETUP_BOUNDARY(NM, NLEVEL_MIN)

         ! Then assemble unstructured matrix with homogeneous Dirichlet boundary conditions along
         ! external boundaries and special MGM BC-settings along mesh interfaces

         CALL SCARC_SET_GRID_TYPE (NSCARC_GRID_UNSTRUCTURED)
         IF (SCARC_MGM_CHECK_LAPLACE .OR. SCARC_MGM_EXACT_INITIAL) THEN
            CALL SCARC_SETUP_POISSON (NM, NLEVEL_MIN)
            CALL SCARC_SETUP_BOUNDARY(NM, NLEVEL_MIN)
         ENDIF

         TYPE_SCOPE(0) = NSCARC_SCOPE_LOCAL
         CALL SCARC_SETUP_LAPLACE (NM, NLEVEL_MIN)
         CALL SCARC_SETUP_BOUNDARY_WITH_INTERFACES(NM, NLEVEL_MIN) 
         CALL SCARC_SET_GRID_TYPE (NSCARC_GRID_STRUCTURED)

   END SELECT SELECT_SCARC_METHOD

ENDDO MESHES_POISSON_LOOP

END SUBROUTINE SCARC_SETUP_POISSON_SEPARABLE


! ------------------------------------------------------------------------------------------------------------------
!> \brief Assemble separable Poisson matrices on requested grid levels and set boundary conditions
! ------------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_POISSON_INSEPARABLE
INTEGER :: NM, NL

MESHES_POISSON_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   SELECT_SCARC_METHOD: SELECT CASE (TYPE_METHOD)

      ! ---------- Krylov method (CG) as main solver, different preconditioners possible

      CASE (NSCARC_METHOD_KRYLOV)

         ! For all different possible Krylov variants, first setup Poisson matrix on finest level including BC's 

         CALL SCARC_SETUP_POISSON_VAR (NM, NLEVEL_MIN)
         CALL SCARC_SETUP_BOUNDARY_VAR(NM, NLEVEL_MIN)

         ! Depending on the requested preconditioner, also assemble the Poisson matrix with BC's on specific coarser levels

         SELECT_KRYLOV_PRECON: SELECT CASE (TYPE_PRECON)

            ! In case of multigrid as preconditioner:
            ! only build higher level structures in case of geometric multigrid (algebraic variant is done elsewhere)

            CASE (NSCARC_RELAX_GMG)

               IF (IS_CG_GMG) THEN
                  DO NL = NLEVEL_MIN+1, NLEVEL_MAX
                     CALL SCARC_SETUP_POISSON_VAR (NM, NL)
                     CALL SCARC_SETUP_BOUNDARY_VAR(NM, NL)
                  ENDDO
               ENDIF

            ! in case of default preconditioners (JACOBI/SSOR/FFT/...):
            ! if there is an additional coarse grid correction which is NOT AMG-based, 
            ! then also assemble matrix on coarse grid level

            CASE DEFAULT
   
               IF (HAS_TWO_LEVELS .AND. .NOT.HAS_AMG_LEVELS) THEN
                  CALL SCARC_SETUP_POISSON_VAR (NM, NLEVEL_MAX)
                  CALL SCARC_SETUP_BOUNDARY_VAR(NM, NLEVEL_MAX)
               ENDIF

         END SELECT SELECT_KRYLOV_PRECON

      ! ---------- Multigrid as main solver

      CASE (NSCARC_METHOD_MULTIGRID)

         ! For all different possible multigrid-variants, first setup Poisson matrix on finest level including BC's 

         CALL SCARC_SETUP_POISSON_VAR (NM, NLEVEL_MIN)
         CALL SCARC_SETUP_BOUNDARY_VAR(NM, NLEVEL_MIN)

         ! On case of a  geometric multigrid, assemble standard n-point-matrix hierarchy on all coarser levels, too
         ! Note: in case of an algebraic multigrid, this will be done in a separate routine later

         IF (TYPE_MULTIGRID == NSCARC_MULTIGRID_GEOMETRIC) THEN
            DO NL = NLEVEL_MIN + 1, NLEVEL_MAX
               CALL SCARC_SETUP_POISSON_VAR (NM, NL)
               CALL SCARC_SETUP_BOUNDARY_VAR(NM, NL)
            ENDDO
         ENDIF

      ! ---------- McKenny-Greengard-Mayo method:
      ! Solving for the structured and unstructured Poisson matrix
      ! Assemble both, the structured and unstructured Poisson matrix
      ! temporarily they will be stored separately in matrices AC and ACU due to the different
      ! settings along internal boundary cells,
      ! in the medium term, a toggle mechanism will be implemented which only switches the corresponding
      ! entries while keeping the entries which are the same for both discretization types

      CASE (NSCARC_METHOD_MGM)
   
         ! First assemble structured matrix with inhomogeneous boundary conditions

         TYPE_SCOPE(0) = NSCARC_SCOPE_GLOBAL
         CALL SCARC_SET_GRID_TYPE (NSCARC_GRID_STRUCTURED)
         CALL SCARC_SETUP_POISSON_VAR (NM, NLEVEL_MIN)
         CALL SCARC_SETUP_BOUNDARY_VAR(NM, NLEVEL_MIN)

         ! Then assemble unstructured matrix with homogeneous Dirichlet boundary conditions along
         ! external boundaries and special MGM BC-settings along mesh interfaces

         CALL SCARC_SET_GRID_TYPE (NSCARC_GRID_UNSTRUCTURED)
         IF (SCARC_MGM_CHECK_LAPLACE .OR. SCARC_MGM_EXACT_INITIAL) THEN
            CALL SCARC_SETUP_POISSON_VAR (NM, NLEVEL_MIN)
            CALL SCARC_SETUP_BOUNDARY_VAR(NM, NLEVEL_MIN)
         ENDIF

         TYPE_SCOPE(0) = NSCARC_SCOPE_LOCAL
         CALL SCARC_SETUP_LAPLACE_VAR (NM, NLEVEL_MIN)
         CALL SCARC_SETUP_BOUNDARY_WITH_INTERFACES(NM, NLEVEL_MIN) 
         CALL SCARC_SET_GRID_TYPE (NSCARC_GRID_STRUCTURED)

   END SELECT SELECT_SCARC_METHOD

ENDDO MESHES_POISSON_LOOP

END SUBROUTINE SCARC_SETUP_POISSON_INSEPARABLE


! --------------------------------------------------------------------------------------------------------------
!> \brief Make Poisson matrix globally acting, that is, setup all overlapping information of global matrix
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_POISSON_GLOBAL
INTEGER :: NL
 
! Setup mappings for the global numbering of vectors and the Poisson matrix (compact storage technique only)

IF (TYPE_MATRIX == NSCARC_MATRIX_COMPACT) THEN
   IF (IS_MGM) THEN

      TYPE_SCOPE = NSCARC_SCOPE_GLOBAL
      CALL SCARC_SET_GRID_TYPE(NSCARC_GRID_STRUCTURED)
      CALL SCARC_SETUP_GLOBAL_CELL_MAPPING(NLEVEL_MIN)
      CALL SCARC_SETUP_GLOBAL_POISSON_COLUMNS(NLEVEL_MIN)

      IF (SCARC_MGM_CHECK_LAPLACE .OR. SCARC_MGM_EXACT_INITIAL) THEN
         CALL SCARC_SET_GRID_TYPE(NSCARC_GRID_UNSTRUCTURED)
         CALL SCARC_SETUP_GLOBAL_CELL_MAPPING(NLEVEL_MIN)
         CALL SCARC_SETUP_GLOBAL_POISSON_COLUMNS(NLEVEL_MIN)
      ENDIF

   ELSE

      CALL SCARC_SETUP_GLOBAL_CELL_MAPPING(NLEVEL_MIN)
      CALL SCARC_SETUP_GLOBAL_POISSON_COLUMNS(NLEVEL_MIN)

   ENDIF
ENDIF
 
! If there is more than one mesh, exchange matrix values in overlapping parts
! This must be done for all multilevel methods at least at the finest grid level
! Furthermore also at all higher levels except for the AMG method,
! in this case it will be done later in routine SETUP_ALGEBRAIC_MULTIGRID

IF (SET_MATRIX_TYPE(NLEVEL_MIN) == NSCARC_MATRIX_COMPACT) CALL SCARC_SETUP_GLOBAL_POISSON_OVERLAPS(NLEVEL_MIN)

MULTI_LEVEL_IF: IF (HAS_MULTIPLE_LEVELS .AND. .NOT.HAS_AMG_LEVELS) THEN
   DO NL = NLEVEL_MIN+1, NLEVEL_MAX
      IF (SET_MATRIX_TYPE(NL) /= NSCARC_MATRIX_COMPACT) CYCLE
      CALL SCARC_SETUP_GLOBAL_CELL_MAPPING(NL)
      CALL SCARC_SETUP_GLOBAL_POISSON_COLUMNS(NL)
      CALL SCARC_SETUP_GLOBAL_POISSON_OVERLAPS(NL)
   ENDDO 
ENDIF MULTI_LEVEL_IF

END SUBROUTINE SCARC_SETUP_POISSON_GLOBAL


#ifdef WITH_MKL
! --------------------------------------------------------------------------------------------------------------
!> \brief Setup symmetric version of Poisson matrix needed for all types of IntelMKL preconditioning (MKL only)
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_POISSON_SYMMETRIC
USE SCARC_POINTERS, ONLY: L, SCARC_POINT_TO_GRID
INTEGER :: NM, NL, TYPE_MKL_SAVE(0:1), TYPE_SCOPE_SAVE(0:1)

IF (.NOT. IS_MGM) THEN

   DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

      CALL SCARC_POINT_TO_GRID (NM, NLEVEL_MIN)
      IF (TYPE_PRECON == NSCARC_RELAX_OPTIMIZED .OR. TYPE_SMOOTH == NSCARC_RELAX_OPTIMIZED) THEN
         IF (.NOT.L%HAS_OBSTRUCTIONS) TYPE_MKL(NLEVEL_MIN) = NSCARC_MKL_NONE
      ENDIF
      IF (TYPE_MKL(NLEVEL_MIN) /= NSCARC_MKL_NONE) THEN
         CALL SCARC_SETUP_MATRIX_MKL(NSCARC_MATRIX_POISSON, NM, NLEVEL_MIN)
         CALL SCARC_SETUP_BOUNDARY_MKL(NSCARC_MATRIX_POISSON, NM, NLEVEL_MIN)
      ENDIF

      IF (HAS_GMG_LEVELS) THEN
         DO NL = NLEVEL_MIN+1, NLEVEL_MIN
            CALL SCARC_POINT_TO_GRID (NM, NLEVEL_MIN)
            IF (TYPE_PRECON == NSCARC_RELAX_OPTIMIZED .OR. TYPE_SMOOTH == NSCARC_RELAX_OPTIMIZED) THEN
               IF (.NOT.L%HAS_OBSTRUCTIONS) TYPE_MKL(NL) = NSCARC_MKL_NONE
            ENDIF
            IF (TYPE_MKL(NL) /= NSCARC_MKL_NONE)  CALL SCARC_SETUP_MATRIX_MKL  (NSCARC_MATRIX_POISSON, NM, NL)
            IF (TYPE_MKL(NL) == NSCARC_MKL_LOCAL) CALL SCARC_SETUP_BOUNDARY_MKL(NSCARC_MATRIX_POISSON, NM, NL)
         ENDDO
      ENDIF

   ENDDO 

ELSE 

   CALL SCARC_SET_GRID_TYPE (NSCARC_GRID_UNSTRUCTURED)

   TYPE_SCOPE_SAVE(0:1) = TYPE_SCOPE(0:1)
   TYPE_MKL_SAVE(0:1)   = TYPE_MKL(0:1)
   IF (SCARC_MGM_CHECK_LAPLACE .OR. SCARC_MGM_EXACT_INITIAL) THEN
      TYPE_SCOPE(0) = NSCARC_SCOPE_GLOBAL
      IF (TRIM(SCARC_PRECON) == 'CLUSTER') THEN
         TYPE_MKL(NLEVEL_MIN) = NSCARC_MKL_GLOBAL
      ELSE
         TYPE_MKL(NLEVEL_MIN) = NSCARC_MKL_LOCAL
      ENDIF
      DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
         CALL SCARC_POINT_TO_GRID (NM, NLEVEL_MIN)
         CALL SCARC_SETUP_MATRIX_MKL(NSCARC_MATRIX_POISSON, NM, NLEVEL_MIN)
         CALL SCARC_SETUP_BOUNDARY_MKL(NSCARC_MATRIX_POISSON, NM, NLEVEL_MIN)
      ENDDO 
   ENDIF

   TYPE_SCOPE(0:1) = NSCARC_SCOPE_LOCAL
   TYPE_MKL(0:1)   = NSCARC_MKL_LOCAL
   SELECT CASE (TYPE_MGM_LAPLACE)
      CASE (NSCARC_MGM_LAPLACE_CG, NSCARC_MGM_LAPLACE_PARDISO) 
         DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
            CALL SCARC_POINT_TO_GRID (NM, NLEVEL_MIN)
            CALL SCARC_SETUP_MATRIX_MKL (NSCARC_MATRIX_LAPLACE, NM, NLEVEL_MIN)
         ENDDO 
      CASE (NSCARC_MGM_LAPLACE_OPTIMIZED) 
         DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
            CALL SCARC_POINT_TO_GRID (NM, NLEVEL_MIN)
            IF (L%STRUCTURED%NC > L%UNSTRUCTURED%NC) CALL SCARC_SETUP_MATRIX_MKL (NSCARC_MATRIX_LAPLACE, NM, NLEVEL_MIN)
         ENDDO 
   END SELECT
   
   TYPE_SCOPE(0:1) = TYPE_SCOPE_SAVE(0:1)
   TYPE_MKL(0:1)   = TYPE_MKL_SAVE(0:1)
   CALL SCARC_SET_GRID_TYPE (NSCARC_GRID_STRUCTURED)

ENDIF

END SUBROUTINE SCARC_SETUP_POISSON_SYMMETRIC
#endif


! --------------------------------------------------------------------------------------------------------------
!> \brief Define sizes for system matrix A (including extended regions related to overlaps)
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_POISSON_SIZES(NL)
USE SCARC_POINTERS, ONLY: S, L, G, OG, A, OA, AB, OAB, &
                          SCARC_POINT_TO_GRID, SCARC_POINT_TO_OTHER_GRID, &
                          SCARC_POINT_TO_CMATRIX, SCARC_POINT_TO_OTHER_CMATRIX, &
                          SCARC_POINT_TO_BMATRIX, SCARC_POINT_TO_OTHER_BMATRIX
INTEGER, INTENT(IN) :: NL
INTEGER :: NM, NOM, INBR

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   
   CALL SCARC_POINT_TO_GRID (NM, NL)                                    
   
   SELECT_MATRIX_TYPE: SELECT CASE (SET_MATRIX_TYPE(NL))
 
      ! -------- Matrix in compact storage technique
 
      CASE (NSCARC_MATRIX_COMPACT)

         A => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_POISSON)

         ! Assign IOR settings to corresponding positions in stencil

         IF (TWO_D) THEN
            A%N_STENCIL = 5
            A%POS(-3:3) = (/1,0,2,3,4,0,5/)     
         ELSE
            A%N_STENCIL = 7
            A%POS(-3:3) = (/1,2,3,4,5,6,7/)
         ENDIF

         A%N_VAL  = G%NCE * A%N_STENCIL
         A%N_ROW  = G%NCE + 1

         ! Allocate matrices on overlapping parts for later data exchanges with neighbors

         DO INBR = 1, SCARC(NM)%N_NEIGHBORS
            NOM = S%NEIGHBORS(INBR)
            CALL SCARC_POINT_TO_OTHER_GRID (NM, NOM, NL)
            OA => SCARC_POINT_TO_OTHER_CMATRIX (NSCARC_MATRIX_POISSON)
            OA%N_STENCIL = A%N_STENCIL
            OA%N_VAL = 4 * OG%NCG * A%N_STENCIL            ! TODO: CHECK LENGTH
            OA%N_ROW = OG%NCG + 1
         ENDDO
 
      ! -------- Matrix in bandwise storage technique
 
      CASE (NSCARC_MATRIX_BANDWISE)

         AB => SCARC_POINT_TO_BMATRIX (NSCARC_MATRIX_POISSON)

         IF (TWO_D) THEN
   
            AB%N_STENCIL   = 5                      ! 5-point Laplacian
            AB%POS(-3:3)   = (/5,0,4,3,2,0,1/)      ! assignment of IOR settings to columns in matrix array
   
            AB%OFFSET( 3)  = -L%NX                  ! lower z
            AB%OFFSET( 1)  = -1                     ! lower x
            AB%OFFSET( 0)  =  0                     ! diag
            AB%OFFSET(-1)  =  1                     ! upper x
            AB%OFFSET(-3)  =  L%NX                  ! upper z
   
            AB%SOURCE( 3)   =  1                    ! lower z
            AB%SOURCE( 1)   =  1                    ! lower x
            AB%SOURCE( 0)   =  1                    ! diag
            AB%SOURCE(-1)   =  2                    ! upper x
            AB%SOURCE(-3)   =  L%NX+1               ! upper z
   
            AB%TARGET( 3)   =  L%NX+1               ! lower z
            AB%TARGET( 1)   =  2                    ! lower x
            AB%TARGET( 0)   =  1                    ! diag
            AB%TARGET(-1)   =  1                    ! upper x
            AB%TARGET(-3)   =  1                    ! upper z
   
            AB%LENGTH( 3)  =  G%NC - L%NX           ! lower z
            AB%LENGTH( 1)  =  G%NC - 1              ! lower x
            AB%LENGTH( 0)  =  G%NC                  ! diag
            AB%LENGTH(-1)  =  G%NC - 1              ! upper x
            AB%LENGTH(-3)  =  G%NC - L%NX           ! upper z
   
         ELSE
   
            AB%N_STENCIL   = 7                      ! 7-point Laplacian
            AB%POS(-3:3)   = (/7,6,5,4,3,2,1/)      ! assignment of IOR settings to columns in matrix array
   
            AB%OFFSET( 3)  = -L%NX*L%NY             ! lower z
            AB%OFFSET( 2)  = -L%NX                  ! lower y
            AB%OFFSET( 1)  = -1                     ! lower x
            AB%OFFSET( 0)  =  0                     ! diag
            AB%OFFSET(-1)  =  1                     ! upper x
            AB%OFFSET(-2)  =  L%NX                  ! upper y
            AB%OFFSET(-3)  =  L%NX*L%NY             ! upper z
   
            AB%SOURCE( 3)  =  1                     ! lower z
            AB%SOURCE( 2)  =  1                     ! lower y
            AB%SOURCE( 1)  =  1                     ! lower x
            AB%SOURCE( 0)  =  1                     ! diag
            AB%SOURCE(-1)  =  2                     ! upper x
            AB%SOURCE(-2)  =  L%NX+1                ! upper y
            AB%SOURCE(-3)  =  L%NX*L%NY+1           ! upper z
   
            AB%TARGET( 3)  =  L%NX*L%NY+1           ! lower z
            AB%TARGET( 2)  =  L%NX+1                ! lower y
            AB%TARGET( 1)  =  2                     ! lower x
            AB%TARGET( 0)  =  1                     ! diag
            AB%TARGET(-1)  =  1                     ! upper x
            AB%TARGET(-2)  =  1                     ! upper y
            AB%TARGET(-3)  =  1                     ! upper z
   
            AB%LENGTH( 3)  =  G%NC - L%NX*L%NY      ! lower z
            AB%LENGTH( 2)  =  G%NC - L%NX           ! lower y
            AB%LENGTH( 1)  =  G%NC - 1              ! lower x
            AB%LENGTH( 0)  =  G%NC                  ! diag
            AB%LENGTH(-1)  =  G%NC - 1              ! upper x
            AB%LENGTH(-2)  =  G%NC - L%NX           ! upper y
            AB%LENGTH(-3)  =  G%NC - L%NX*L%NY      ! upper z
   
         ENDIF

         AB%N_VAL  = G%NC * AB%N_STENCIL
         AB%N_DIAG = G%NC

         ! Determine sizes of overlapping parts for later communication with corresponding neighbors

         DO INBR = 1, SCARC(NM)%N_NEIGHBORS
            NOM = S%NEIGHBORS(INBR)
            CALL SCARC_POINT_TO_OTHER_GRID (NM, NOM, NL)
            OAB => SCARC_POINT_TO_OTHER_BMATRIX (NSCARC_MATRIX_POISSON)
            OAB%N_STENCIL = AB%N_STENCIL
            OAB%N_VAL     = OG%NCG * AB%N_STENCIL
            OAB%N_DIAG    = OG%NCG 
         ENDDO

   END SELECT SELECT_MATRIX_TYPE
   
ENDDO MESHES_LOOP
   
! -------- Exchange matrix sizes in case of a multi-mesh geometry
 
IF (NMESHES > 1) CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MATRIX_SIZES, NSCARC_MATRIX_POISSON, NL)

END SUBROUTINE SCARC_SETUP_POISSON_SIZES


! --------------------------------------------------------------------------------------------------------------
!> \brief Define sizes for local unstructured Laplace matrices
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_LOCAL_LAPLACE_SIZES(NL)
USE SCARC_POINTERS, ONLY: G, A, SCARC_POINT_TO_GRID, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NL
INTEGER :: NM

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
  
   CALL SCARC_POINT_TO_GRID (NM, NL)
   A => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_LAPLACE)

   IF (TWO_D) THEN
      A%N_STENCIL = 5
      A%POS(-3:3) = (/1,0,2,3,4,0,5/)
   ELSE
      A%N_STENCIL = 7
      A%POS(-3:3) = (/1,2,3,4,5,6,7/)
   ENDIF

   A%N_VAL  = G%NC * A%N_STENCIL
   A%N_ROW  = G%NC + 1

ENDDO MESHES_LOOP
  
END SUBROUTINE SCARC_SETUP_LOCAL_LAPLACE_SIZES


! -------------------------------------------------------------------------------------------------------------
!> \brief Get global numberings for compact column vector of Poisson matrix 
! -------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_GLOBAL_POISSON_COLUMNS(NL)
USE SCARC_POINTERS, ONLY: G, A, SCARC_POINT_TO_GRID, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NL
INTEGER :: NM, IC, ICOL, JC

IF (NMESHES == 1 .OR. TYPE_SCOPE(0) == NSCARC_SCOPE_LOCAL) THEN
   DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
      CALL SCARC_POINT_TO_GRID (NM, NL)                                    
      A => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_POISSON)
      A%COLG = A%COL
   ENDDO
ELSE
   DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
      CALL SCARC_POINT_TO_GRID (NM, NL)                                    
      A => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_POISSON)
      DO IC = 1, G%NC
         DO ICOL = A%ROW(IC), A%ROW(IC+1)-1
            JC = A%COL(ICOL)
            A%COLG(ICOL) = G%LOCAL_TO_GLOBAL(JC)
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'A%COLG(', ICOL,')=', IC, ICOL, JC, A%COLG(ICOL)
#endif
         ENDDO
      ENDDO
   ENDDO
ENDIF

END SUBROUTINE SCARC_SETUP_GLOBAL_POISSON_COLUMNS


! -------------------------------------------------------------------------------------------------------------
!> \brief Make Poisson matrix global by exchanging adjacent overlaps
! -------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_GLOBAL_POISSON_OVERLAPS(NL)
USE SCARC_POINTERS, ONLY: S, A, OA, SCARC_POINT_TO_GRID, SCARC_POINT_TO_OTHER_GRID, &
                          SCARC_POINT_TO_CMATRIX, SCARC_POINT_TO_OTHER_CMATRIX
INTEGER, INTENT(IN) :: NL
INTEGER :: NM, INBR, NOM

IF (NMESHES == 1 .OR. TYPE_SCOPE(0) == NSCARC_SCOPE_LOCAL) RETURN

CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MATRIX_COLS,  NSCARC_MATRIX_POISSON, NL)
CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MATRIX_COLSG, NSCARC_MATRIX_POISSON, NL)
CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MATRIX_VALS,  NSCARC_MATRIX_POISSON, NL)

CALL SCARC_EXTRACT_MATRIX_OVERLAPS(NSCARC_MATRIX_POISSON, 1, NL)

MESHES_FINE_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)    
   A => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_POISSON)
   CALL SCARC_REDUCE_CMATRIX (A, 'G%POISSON', CROUTINE)

   OMESHES_FINE_LOOP: DO INBR = 1, S%N_NEIGHBORS
      NOM = S%NEIGHBORS(INBR)
      CALL SCARC_POINT_TO_OTHER_GRID (NM, NOM, NL)
      OA => SCARC_POINT_TO_OTHER_CMATRIX (NSCARC_MATRIX_POISSON)
      CALL SCARC_REDUCE_CMATRIX (OA, 'OG%POISSON', CROUTINE)
   ENDDO OMESHES_FINE_LOOP

ENDDO MESHES_FINE_LOOP
    
END SUBROUTINE SCARC_SETUP_GLOBAL_POISSON_OVERLAPS


! --------------------------------------------------------------------------------------------------------------
!> \brief Check if specified cell is within a given mesh
! --------------------------------------------------------------------------------------------------------------
LOGICAL FUNCTION SCARC_CELL_WITHIN_MESH(G, NM, IC)
TYPE (SCARC_GRID_TYPE), POINTER, INTENT(IN) :: G
INTEGER, INTENT(IN) :: NM, IC
INTEGER :: IC_START, IC_STOP

SCARC_CELL_WITHIN_MESH = .FALSE.
IC_START = G%NC_OFFSET(NM) + 1
IF (NM < NMESHES) THEN
   IC_STOP  = G%NC_OFFSET(NM+1)
ELSE
   IC_STOP  = G%NC_GLOBAL
ENDIF
IF (IC_START <=  IC .AND. IC <= IC_STOP) SCARC_CELL_WITHIN_MESH = .TRUE.
RETURN

END FUNCTION SCARC_CELL_WITHIN_MESH


! --------------------------------------------------------------------------------------------------------------
!> \brief Allocate Poisson matrix for the usual 5-point-stencil (2D) or 7-point-stencil (3D)
! Compact storage technique (POISSON)
!    Compression technique to store sparse matrices, non-zero entries are stored
!    in a 1D-vector B(.), row after row,
!    Each row starts with its diagonal entry followed by the other non-zero entries
!    In order to identify each element, pointer arrays ROW and COL are needed,
!    ROW points to the several diagonal entries in vector B(.),
!    COL points to the columns which non-zero entries in the matrix stencil
! Bandwise storage technique (POISSONB)
!    explanation to come ...
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_POISSON (NM, NL)
USE SCARC_POINTERS, ONLY: M, S, L, G, A, AB, OA, OAB, RDX, RDY, RDZ, RDXN, RDYN, RDZN, &
                          SCARC_POINT_TO_GRID,    SCARC_POINT_TO_OTHER_GRID, &
                          SCARC_POINT_TO_CMATRIX, SCARC_POINT_TO_OTHER_CMATRIX, &
                          SCARC_POINT_TO_BMATRIX, SCARC_POINT_TO_OTHER_BMATRIX
INTEGER, INTENT(IN) :: NM, NL
INTEGER :: IX, IY, IZ, IC, IP, INBR, NOM

CROUTINE = 'SCARC_SETUP_POISSON'
 
! Compute single matrix entries and corresponding row and column pointers
! Along internal boundaries use placeholders for the neighboring matrix entries
! which will be communicated in a following step
 
SELECT_STORAGE_TYPE: SELECT CASE (SET_MATRIX_TYPE(NL))

 
   ! ---------- COMPACT Storage technique
 
   CASE (NSCARC_MATRIX_COMPACT)
   
      ! Allocate main matrix on non-overlapping part of mesh

      CALL SCARC_POINT_TO_GRID (NM, NL)                                    
      A => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_POISSON)
      CALL SCARC_ALLOCATE_CMATRIX (A, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_FULL, 'G%POISSON', CROUTINE)

      IF (NL == NLEVEL_MIN) THEN
         RDX  => M%RDX ;  RDY  => M%RDY ;  RDZ  => M%RDZ
         RDXN => M%RDXN;  RDYN => M%RDYN;  RDZN => M%RDZN
      ELSE
         RDX  => L%RDX ;  RDY  => L%RDY ;  RDZ  => L%RDZ
         RDXN => L%RDXN;  RDYN => L%RDYN;  RDZN => L%RDZN
      ENDIF

      ! For every neighbor allocate small matrix on overlapping part of mesh

      DO INBR = 1, SCARC(NM)%N_NEIGHBORS
         NOM = S%NEIGHBORS(INBR)
         CALL SCARC_POINT_TO_OTHER_GRID (NM, NOM, NL)
         OA => SCARC_POINT_TO_OTHER_CMATRIX (NSCARC_MATRIX_POISSON)
         CALL SCARC_ALLOCATE_CMATRIX (OA, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_FULL, 'OG%POISSON', CROUTINE)
      ENDDO

      IP = 1
      DO IZ = 1, L%NZ
         DO IY = 1, L%NY
            DO IX = 1, L%NX
   
               IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(IX, IY, IZ)) CYCLE
               IC = G%CELL_NUMBER(IX, IY, IZ)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '================ IC = ', IC
#endif
               ! Main diagonal 

               CALL SCARC_SETUP_MAINDIAG (IC, IX, IY, IZ, IP)
   
               ! Lower subdiagonals

               IF (IS_VALID_DIRECTION(IX, IY, IZ,  3)) CALL SCARC_SETUP_SUBDIAG(IC, IX, IY, IZ, IX  , IY  , IZ-1, IP,  3)
               IF (IS_VALID_DIRECTION(IX, IY, IZ,  2)) CALL SCARC_SETUP_SUBDIAG(IC, IX, IY, IZ, IX  , IY-1, IZ  , IP,  2)
               IF (IS_VALID_DIRECTION(IX, IY, IZ,  1)) CALL SCARC_SETUP_SUBDIAG(IC, IX, IY, IZ, IX-1, IY  , IZ  , IP,  1)
   
               ! Upper subdiagonals

               IF (IS_VALID_DIRECTION(IX, IY, IZ, -1)) CALL SCARC_SETUP_SUBDIAG(IC, IX, IY, IZ, IX+1, IY  , IZ  , IP, -1)
               IF (IS_VALID_DIRECTION(IX, IY, IZ, -2)) CALL SCARC_SETUP_SUBDIAG(IC, IX, IY, IZ, IX  , IY+1, IZ  , IP, -2)
               IF (IS_VALID_DIRECTION(IX, IY, IZ, -3)) CALL SCARC_SETUP_SUBDIAG(IC, IX, IY, IZ, IX  , IY  , IZ+1, IP, -3)
   
            ENDDO
         ENDDO
      ENDDO
   
      A%ROW(G%NC+1) = IP
      A%N_VAL = IP
   
      CALL SCARC_GET_MATRIX_STENCIL_MAX(A, G%NC)
      CALL SCARC_MATRIX_CHECK_NEUMANN(A, G%NC)

#ifdef WITH_SCARC_DEBUG2
CALL SCARC_DEBUG_CMATRIX (A, 'POISSON', 'SETUP_POISSON: NO BDRY')
#endif
 
   ! ---------- bandwise storage technique
 
   CASE (NSCARC_MATRIX_BANDWISE)
   
      ! Allocate main matrix on non-overlapping part of mesh

      CALL SCARC_POINT_TO_GRID (NM, NL)                                    
      AB => SCARC_POINT_TO_BMATRIX (NSCARC_MATRIX_POISSON)
      CALL SCARC_ALLOCATE_BMATRIX (AB, NL, 'G%POISSONB', CROUTINE)
   
      ! For every neighbor allocate little matrix on overlapping part of mesh

      DO INBR = 1, SCARC(NM)%N_NEIGHBORS
         NOM = S%NEIGHBORS(INBR)
         CALL SCARC_POINT_TO_OTHER_GRID (NM, NOM, NL)
         OAB => SCARC_POINT_TO_BMATRIX (NSCARC_MATRIX_POISSON)
         CALL SCARC_ALLOCATE_BMATRIX (OAB, NL, 'OG%POISSONB', CROUTINE)
      ENDDO
   
      IP  = 1
      DO IZ = 1, L%NZ
         DO IY = 1, L%NY
            DO IX = 1, L%NX
   
               IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(IX, IY, IZ)) CYCLE
               IC = G%CELL_NUMBER(IX, IY, IZ)
   
               ! Lower subdiagonals

               IF (IS_VALID_DIRECTION(IX, IY, IZ,  3)) CALL SCARC_SETUP_SUBDIAGB(IC, IX, IY, IZ, IX  , IY  , IZ-1,  3)
               IF (IS_VALID_DIRECTION(IX, IY, IZ,  2)) CALL SCARC_SETUP_SUBDIAGB(IC, IX, IY, IZ, IX  , IY-1, IZ  ,  2)
               IF (IS_VALID_DIRECTION(IX, IY, IZ,  1)) CALL SCARC_SETUP_SUBDIAGB(IC, IX, IY, IZ, IX-1, IY  , IZ  ,  1)
   
               ! Main diagonal

               CALL SCARC_SETUP_MAINDIAGB (IC, IX, IY, IZ)

               ! Upper subdiagonals

               IF (IS_VALID_DIRECTION(IX, IY, IZ, -1)) CALL SCARC_SETUP_SUBDIAGB(IC, IX, IY, IZ, IX+1, IY  , IZ  , -1)
               IF (IS_VALID_DIRECTION(IX, IY, IZ, -2)) CALL SCARC_SETUP_SUBDIAGB(IC, IX, IY, IZ, IX  , IY+1, IZ  , -2)
               IF (IS_VALID_DIRECTION(IX, IY, IZ, -3)) CALL SCARC_SETUP_SUBDIAGB(IC, IX, IY, IZ, IX  , IY  , IZ+1, -3)
   
            ENDDO
         ENDDO
      ENDDO
   
END SELECT SELECT_STORAGE_TYPE

END SUBROUTINE SCARC_SETUP_POISSON


! --------------------------------------------------------------------------------------------------------------
!> \brief Allocate Poisson matrix for the usual 5-point-stencil (2D) or 7-point-stencil (3D)
! Compact storage technique (POISSON)
!    Compression technique to store sparse matrices, non-zero entries are stored
!    in a 1D-vector B(.), row after row,
!    Each row starts with its diagonal entry followed by the other non-zero entries
!    In order to identify each element, pointer arrays ROW and COL are needed,
!    ROW points to the several diagonal entries in vector B(.),
!    COL points to the columns which non-zero entries in the matrix stencil
! Bandwise storage technique (POISSONB)
!    explanation to come ...
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_POISSON_VAR (NM, NL)
USE SCARC_POINTERS, ONLY: M, S, L, G, A, OA, RDX, RDY, RDZ, RDXN, RDYN, RDZN, &     ! AB, OAB - not yet implemented for bandwise
                          SCARC_POINT_TO_GRID,    SCARC_POINT_TO_OTHER_GRID, &
                          SCARC_POINT_TO_CMATRIX, SCARC_POINT_TO_OTHER_CMATRIX, &
                          SCARC_POINT_TO_BMATRIX, SCARC_POINT_TO_OTHER_BMATRIX
USE SCARC_CONVERGENCE, ONLY: ITE_PRES
INTEGER, INTENT(IN) :: NM, NL
INTEGER :: IX, IY, IZ, IC, IP, INBR, NOM

CROUTINE = 'SCARC_SETUP_POISSON'
 
! Compute single matrix entries and corresponding row and column pointers
! Along internal boundaries use placeholders for the neighboring matrix entries
! which will be communicated in a following step
 
SELECT_STORAGE_TYPE: SELECT CASE (SET_MATRIX_TYPE(NL))
 
   ! ---------- COMPACT Storage technique
 
   CASE (NSCARC_MATRIX_COMPACT)
   
      ! Allocate main matrix on non-overlapping part of mesh
 
      CALL SCARC_POINT_TO_GRID (NM, NL)                                    
      A => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_POISSON)
      IF (ITE_PRES == 1) &
         CALL SCARC_ALLOCATE_CMATRIX (A, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_FULL, 'G%POISSON', CROUTINE)

      IF (NL == NLEVEL_MIN) THEN
         RDX  => M%RDX ;  RDY  => M%RDY ;  RDZ  => M%RDZ
         RDXN => M%RDXN;  RDYN => M%RDYN;  RDZN => M%RDZN
      ELSE
         RDX  => L%RDX ;  RDY  => L%RDY ;  RDZ  => L%RDZ
         RDXN => L%RDXN;  RDYN => L%RDYN;  RDZN => L%RDZN
      ENDIF

      ! For every neighbor allocate small matrix on overlapping part of mesh

      DO INBR = 1, SCARC(NM)%N_NEIGHBORS
         NOM = S%NEIGHBORS(INBR)
         CALL SCARC_POINT_TO_OTHER_GRID (NM, NOM, NL)
         OA => SCARC_POINT_TO_OTHER_CMATRIX (NSCARC_MATRIX_POISSON)
         IF (ITE_PRES == 1) &
            CALL SCARC_ALLOCATE_CMATRIX (OA, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_FULL, 'OG%POISSON', CROUTINE)
      ENDDO

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'SETUP POISSON_VAR: TPI', TOTAL_PRESSURE_ITERATIONS
IF (PREDICTOR) THEN
   CALL SCARC_DEBUG_VECTOR3_BIG (M%RHO, NM, 'RHO BEFORE SETUP_POISSON_VAR')
ELSE
   CALL SCARC_DEBUG_VECTOR3_BIG (M%RHOS, NM, 'RHOS BEFORE SETUP_POISSON_VAR')
ENDIF
#endif
      IP = 1
      DO IZ = 1, L%NZ
         DO IY = 1, L%NY
            DO IX = 1, L%NX
   
               IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(IX, IY, IZ)) CYCLE
               IC = G%CELL_NUMBER(IX, IY, IZ)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '================ IC = ', IC
#endif
               ! Main diagonal 

               CALL SCARC_SETUP_MAINDIAG_VAR (IC, IX, IY, IZ, IP)
   
               ! Lower subdiagonals

               IF (IS_VALID_DIRECTION(IX, IY, IZ,  3)) CALL SCARC_SETUP_SUBDIAG_VAR(IC, IX, IY, IZ, IX  , IY  , IZ-1, IP,  3)
               IF (IS_VALID_DIRECTION(IX, IY, IZ,  2)) CALL SCARC_SETUP_SUBDIAG_VAR(IC, IX, IY, IZ, IX  , IY-1, IZ  , IP,  2)
               IF (IS_VALID_DIRECTION(IX, IY, IZ,  1)) CALL SCARC_SETUP_SUBDIAG_VAR(IC, IX, IY, IZ, IX-1, IY  , IZ  , IP,  1)
   
               ! Upper subdiagonals

               IF (IS_VALID_DIRECTION(IX, IY, IZ, -1)) CALL SCARC_SETUP_SUBDIAG_VAR(IC, IX, IY, IZ, IX+1, IY  , IZ  , IP, -1)
               IF (IS_VALID_DIRECTION(IX, IY, IZ, -2)) CALL SCARC_SETUP_SUBDIAG_VAR(IC, IX, IY, IZ, IX  , IY+1, IZ  , IP, -2)
               IF (IS_VALID_DIRECTION(IX, IY, IZ, -3)) CALL SCARC_SETUP_SUBDIAG_VAR(IC, IX, IY, IZ, IX  , IY  , IZ+1, IP, -3)
   
            ENDDO
         ENDDO
      ENDDO
   
      A%ROW(G%NC+1) = IP
      A%N_VAL = IP
   
      CALL SCARC_GET_MATRIX_STENCIL_MAX(A, G%NC)
      CALL SCARC_MATRIX_CHECK_NEUMANN(A, G%NC)

#ifdef WITH_SCARC_DEBUG2
CALL SCARC_DEBUG_CMATRIX (A, 'POISSON', 'SETUP_POISSON: NO BDRY')
#endif
 
   ! ---------- bandwise storage technique
 
   CASE (NSCARC_MATRIX_BANDWISE)
   
      WRITE(*,*) 'Bandwise storage technique for variable Poisson matrix not yet implemented'
   
END SELECT SELECT_STORAGE_TYPE

END SUBROUTINE SCARC_SETUP_POISSON_VAR


! --------------------------------------------------------------------------------------------------------------
!> \brief Assemble local unstructured Laplace matrices
! The grid numbering is permuted in such a way that all the nonzero entries of the RHS 
! are located of the end of the corresponding vector
! this concerns the entries along internal obstructions and in case of a multi-mesh computation
! also the entries along the internal interfaces
! All other entries of the RHS are zero for the local Laplace problems, such that the
! forward substitution process Ly=b only must start from the nonzero entries on
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_LAPLACE (NM, NL)
USE SCARC_POINTERS, ONLY: L, G, A, GWC, SCARC_POINT_TO_GRID, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NM, NL
INTEGER :: IX, IY, IZ, IC, JC, KC, IOR0, IW, IP, KKC(-3:3), JJC(-3:3)
INTEGER :: TYPE_SCOPE_SAVE

CROUTINE = 'SCARC_SETUP_LAPLACE'
TYPE_SCOPE_SAVE = TYPE_SCOPE(0)
TYPE_SCOPE(0) = NSCARC_SCOPE_LOCAL
 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SETUP_LAPLACE:B: TYPE_SCOPE:', TYPE_SCOPE(0)
#endif

! Point to unstructured grid

CALL SCARC_SET_GRID_TYPE(NSCARC_GRID_UNSTRUCTURED)
CALL SCARC_POINT_TO_GRID (NM, NL)              

! Allocate permutation vectors 

CALL SCARC_ALLOCATE_INT1 (G%PERM_FW , 1, G%NC, NSCARC_INIT_ZERO, 'G%PERM_FW', CROUTINE)
CALL SCARC_ALLOCATE_INT1 (G%PERM_BW , 1, G%NC, NSCARC_INIT_ZERO, 'G%PERM_BW', CROUTINE)
   
! Obstruction cells are numbered last such that they appear at the end of a vector

IF (TYPE_MGM_LAPLACE == NSCARC_MGM_LAPLACE_LUPERM) THEN

   JC = G%NC
   DO IW = L%N_WALL_CELLS_EXT+1, L%N_WALL_CELLS_EXT + L%N_WALL_CELLS_INT
      
      GWC => G%WALL(IW)
      
      IX = GWC%IXW ;  IY = GWC%IYW ;  IZ = GWC%IZW
      
      IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(IX, IY, IZ)) CYCLE
      
      IOR0 = GWC%IOR
      IC   = G%CELL_NUMBER(IX, IY, IZ)

#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG,*) 'IW, IX, IY, IZ, IOR0, IC:', IW, IX, IY, IZ, IOR0, IC
      WRITE(MSG%LU_DEBUG,*) 'OBSTRUCTION: PERM_FW(', IC, ')=', G%PERM_FW(IC),', PERM_BW(', JC, ')=', G%PERM_BW(JC)
#endif
      G%PERM_FW(IC) = JC
      G%PERM_BW(JC) = IC
      JC = JC - 1

   ENDDO
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'AFTER OBSTRUCTION: PERM_FW:'
WRITE(MSG%LU_DEBUG,'(8I4)') G%PERM_FW
WRITE(MSG%LU_DEBUG,*) 'AFTER OBSTRUCTION: PERM_BW:'
WRITE(MSG%LU_DEBUG,'(8I4)') G%PERM_BW
#endif

   ! Interface cells are numbered second last

   DO IW = 1, L%N_WALL_CELLS_EXT
      
      GWC => G%WALL(IW)
      IF (GWC%BTYPE /= INTERNAL) CYCLE
      
      IX = GWC%IXW ;  IY = GWC%IYW ;  IZ = GWC%IZW
      
      IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(IX, IY, IZ)) CYCLE
      
      IOR0 = GWC%IOR
      IC   = G%CELL_NUMBER(IX,IY,IZ)

#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG,*) 'IW, IX, IY, IZ, IOR0, IC:', IW, IX, IY, IZ, IOR0, IC
      WRITE(MSG%LU_DEBUG,*) 'INTERFACE: PERM_FW(', IC, ')=', G%PERM_FW(IC),', PERM_BW(', JC, ')=', G%PERM_BW(JC)
#endif
      IF (G%PERM_FW(IC) == 0) THEN
         G%PERM_FW(IC) = JC
         G%PERM_BW(JC) = IC
         JC = JC - 1
      ENDIF

   ENDDO
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'AFTER INTERFACE: PERM_FW:'
   WRITE(MSG%LU_DEBUG,'(8I4)') G%PERM_FW
   WRITE(MSG%LU_DEBUG,*) 'AFTER INTERFACE: PERM_BW:'
   WRITE(MSG%LU_DEBUG,'(8I4)') G%PERM_BW
#endif

   ! The rest is used from beginning to first interface cell

   KC = 1
   DO IC = 1, G%NC
      IF (G%PERM_FW(IC) /= 0) CYCLE
      G%PERM_BW(KC) = IC
      G%PERM_FW(IC) = KC
      KC = KC + 1
   ENDDO
   IF (KC /= JC + 1) THEN
      WRITE(*,*) 'KC =', KC,': JC =', JC
      CALL SCARC_ERROR(NSCARC_ERROR_MGM_PERMUTATION, SCARC_NONE, NSCARC_NONE)
   ENDIF

   G%NONZERO = KC

ELSE

   DO IC = 1, G%NC
      G%PERM_BW(IC) = IC
      G%PERM_FW(IC) = IC
   ENDDO

   G%NONZERO = 1

ENDIF
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'AFTER FINAL FILL: PERM_FW:'
WRITE(MSG%LU_DEBUG,'(8I4)') G%PERM_FW
WRITE(MSG%LU_DEBUG,*) 'AFTER FINAL FILL: PERM_BW:'
WRITE(MSG%LU_DEBUG,'(8I4)') G%PERM_BW
WRITE(MSG%LU_DEBUG,*) 'G%ICX'
WRITE(MSG%LU_DEBUG,'(8I4)') G%ICX
WRITE(MSG%LU_DEBUG,*) 'G%ICY'
WRITE(MSG%LU_DEBUG,'(8I4)') G%ICY
WRITE(MSG%LU_DEBUG,*) 'G%ICZ'
WRITE(MSG%LU_DEBUG,'(8I4)') G%ICZ
DO IZ = 1, L%NZ
   DO IY = 1, L%NY
      WRITE(MSG%LU_DEBUG,*) (L%IS_SOLID(IX, IY, IZ), IX=1, L%NX)
   ENDDO
ENDDO
#endif

! Allocate Laplace matrix on non-overlapping part of mesh

A => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_LAPLACE)
CALL SCARC_ALLOCATE_CMATRIX (A, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_FULL, 'G%LAPLACE', CROUTINE)

! Assemble Laplace matrix with grid permutation based on MGM-method 

IP = 1
IF (TYPE_MGM_LAPLACE == NSCARC_MGM_LAPLACE_LUPERM) THEN

   DO IC = 1, G%NC

      JJC = -1
      KKC = -1

      JJC(0) = G%PERM_BW(IC);  KKC(0) = G%PERM_FW(JJC(0))
      
      IX = G%ICX(JJC(0)); IY = G%ICY(JJC(0)); IZ = G%ICZ(JJC(0))

      JJC(-3) = G%CELL_NUMBER(IX  , IY, IZ+1)     ; KKC(-3) = GET_PERM(JJC(-3))  
      JJC(-1) = G%CELL_NUMBER(IX+1, IY, IZ  )     ; KKC(-1) = GET_PERM(JJC(-1))   
      JJC( 1) = G%CELL_NUMBER(IX-1, IY, IZ  )     ; KKC( 1) = GET_PERM(JJC( 1))    
      JJC( 3) = G%CELL_NUMBER(IX  , IY, IZ-1)     ; KKC( 3) = GET_PERM(JJC( 3))     
      IF (.NOT.TWO_D) THEN
        JJC(-2) = G%CELL_NUMBER(IX, IY+1, IZ)     ; KKC(-2) = GET_PERM(JJC(-2))     
        JJC( 2) = G%CELL_NUMBER(IX, IY-1, IZ)     ; KKC( 2) = GET_PERM(JJC( 2))     
      ENDIF

      ! Main diagonal 
      CALL SCARC_SETUP_MAINDIAG (IC, IX, IY, IZ, IP)
#ifdef WITH_SCARC_DEBUG
         WRITE(MSG%LU_DEBUG,*) '======================================='
         WRITE(MSG%LU_DEBUG,*) 'JJC = ', JJC
         WRITE(MSG%LU_DEBUG,*) 'KKC = ', KKC
         WRITE(MSG%LU_DEBUG,*) 'IX, IY, IZ=', IX, IY, IZ
#endif
         
      ! Lower subdiagonals

      IF (IS_VALID_DIRECTION(IX, IY, IZ,  3)) CALL SCARC_SETUP_SUBDIAG_PERM(IX, IY, IZ, IX  , IY  , IZ-1, KKC( 3), IP,  3)
      IF (IS_VALID_DIRECTION(IX, IY, IZ,  2)) CALL SCARC_SETUP_SUBDIAG_PERM(IX, IY, IZ, IX  , IY-1, IZ  , KKC( 2), IP,  2)
      IF (IS_VALID_DIRECTION(IX, IY, IZ,  1)) CALL SCARC_SETUP_SUBDIAG_PERM(IX, IY, IZ, IX-1, IY  , IZ  , KKC( 1), IP,  1)

      ! Upper subdiagonals

      IF (IS_VALID_DIRECTION(IX, IY, IZ, -1)) CALL SCARC_SETUP_SUBDIAG_PERM(IX, IY, IZ, IX+1, IY  , IZ  , KKC(-1), IP, -1)
      IF (IS_VALID_DIRECTION(IX, IY, IZ, -2)) CALL SCARC_SETUP_SUBDIAG_PERM(IX, IY, IZ, IX  , IY+1, IZ  , KKC(-2), IP, -2)
      IF (IS_VALID_DIRECTION(IX, IY, IZ, -3)) CALL SCARC_SETUP_SUBDIAG_PERM(IX, IY, IZ, IX  , IY  , IZ+1, KKC(-3), IP, -3)

   ENDDO
      
! Assemble Laplace matrix without grid permutation 

ELSE

   DO IZ = 1, L%NZ
      DO IY = 1, L%NY
         DO IX = 1, L%NX

            IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(IX, IY, IZ)) CYCLE
            IC = G%CELL_NUMBER(IX, IY, IZ)

            ! Main diagonal 

            CALL SCARC_SETUP_MAINDIAG (IC, IX, IY, IZ, IP)

            ! Lower subdiagonals

            IF (IS_VALID_DIRECTION(IX, IY, IZ,  3)) CALL SCARC_SETUP_SUBDIAG(IC, IX, IY, IZ, IX  , IY  , IZ-1, IP,  3)
            IF (IS_VALID_DIRECTION(IX, IY, IZ,  2)) CALL SCARC_SETUP_SUBDIAG(IC, IX, IY, IZ, IX  , IY-1, IZ  , IP,  2)
            IF (IS_VALID_DIRECTION(IX, IY, IZ,  1)) CALL SCARC_SETUP_SUBDIAG(IC, IX, IY, IZ, IX-1, IY  , IZ  , IP,  1)

            ! Upper subdiagonals

            IF (IS_VALID_DIRECTION(IX, IY, IZ, -1)) CALL SCARC_SETUP_SUBDIAG(IC, IX, IY, IZ, IX+1, IY  , IZ  , IP, -1)
            IF (IS_VALID_DIRECTION(IX, IY, IZ, -2)) CALL SCARC_SETUP_SUBDIAG(IC, IX, IY, IZ, IX  , IY+1, IZ  , IP, -2)
            IF (IS_VALID_DIRECTION(IX, IY, IZ, -3)) CALL SCARC_SETUP_SUBDIAG(IC, IX, IY, IZ, IX  , IY  , IZ+1, IP, -3)

         ENDDO
      ENDDO
   ENDDO
      
ENDIF

A%ROW(G%NC+1) = IP
A%N_VAL = IP
      
CALL SCARC_GET_MATRIX_STENCIL_MAX(A, G%NC)

TYPE_SCOPE(0) = TYPE_SCOPE_SAVE

#ifdef WITH_SCARC_DEBUG2
CALL SCARC_DEBUG_CMATRIX (A, 'LAPLACE', 'SETUP_LAPLACE: NO BDRY')
#endif
 
END SUBROUTINE SCARC_SETUP_LAPLACE


! --------------------------------------------------------------------------------------------------------------
!> \brief Assemble local unstructured Laplace matrices
! The grid numbering is permuted in such a way that all the nonzero entries of the RHS 
! are located of the end of the corresponding vector
! this concerns the entries along internal obstructions and in case of a multi-mesh computation
! also the entries along the internal interfaces
! All other entries of the RHS are zero for the local Laplace problems, such that the
! forward substitution process Ly=b only must start from the nonzero entries on
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_LAPLACE_VAR (NM, NL)
USE SCARC_POINTERS, ONLY: L, G, A, GWC, SCARC_POINT_TO_GRID, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NM, NL
INTEGER :: IX, IY, IZ, IC, JC, KC, IOR0, IW, IP, KKC(-3:3), JJC(-3:3)
INTEGER :: TYPE_SCOPE_SAVE

CROUTINE = 'SCARC_SETUP_LAPLACE_VAR'
TYPE_SCOPE_SAVE = TYPE_SCOPE(0)
TYPE_SCOPE(0) = NSCARC_SCOPE_LOCAL
 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SETUP_LAPLACE_VAR:B: TYPE_SCOPE:', TYPE_SCOPE(0)
#endif

! Point to unstructured grid

CALL SCARC_SET_GRID_TYPE(NSCARC_GRID_UNSTRUCTURED)
CALL SCARC_POINT_TO_GRID (NM, NL)              

! Allocate permutation vectors 

CALL SCARC_ALLOCATE_INT1 (G%PERM_FW , 1, G%NC, NSCARC_INIT_ZERO, 'G%PERM_FW', CROUTINE)
CALL SCARC_ALLOCATE_INT1 (G%PERM_BW , 1, G%NC, NSCARC_INIT_ZERO, 'G%PERM_BW', CROUTINE)
   
! Obstruction cells are numbered last such that they appear at the end of a vector

IF (TYPE_MGM_LAPLACE == NSCARC_MGM_LAPLACE_LUPERM) THEN

   JC = G%NC
   DO IW = L%N_WALL_CELLS_EXT+1, L%N_WALL_CELLS_EXT + L%N_WALL_CELLS_INT
      
      GWC => G%WALL(IW)
      
      IX = GWC%IXW ;  IY = GWC%IYW ;  IZ = GWC%IZW
      
      IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(IX, IY, IZ)) CYCLE
      
      IOR0 = GWC%IOR
      IC   = G%CELL_NUMBER(IX, IY, IZ)

#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG,*) 'IW, IX, IY, IZ, IOR0, IC:', IW, IX, IY, IZ, IOR0, IC
      WRITE(MSG%LU_DEBUG,*) 'OBSTRUCTION: PERM_FW(', IC, ')=', G%PERM_FW(IC),', PERM_BW(', JC, ')=', G%PERM_BW(JC)
#endif
      G%PERM_FW(IC) = JC
      G%PERM_BW(JC) = IC
      JC = JC - 1

   ENDDO
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'AFTER OBSTRUCTION: PERM_FW:'
WRITE(MSG%LU_DEBUG,'(8I4)') G%PERM_FW
WRITE(MSG%LU_DEBUG,*) 'AFTER OBSTRUCTION: PERM_BW:'
WRITE(MSG%LU_DEBUG,'(8I4)') G%PERM_BW
#endif

   ! Interface cells are numbered second last

   DO IW = 1, L%N_WALL_CELLS_EXT
      
      GWC => G%WALL(IW)
      IF (GWC%BTYPE /= INTERNAL) CYCLE
      
      IX = GWC%IXW ;  IY = GWC%IYW ;  IZ = GWC%IZW
      
      IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(IX, IY, IZ)) CYCLE
      
      IOR0 = GWC%IOR
      IC   = G%CELL_NUMBER(IX,IY,IZ)

#ifdef WITH_SCARC_DEBUG
      WRITE(MSG%LU_DEBUG,*) 'IW, IX, IY, IZ, IOR0, IC:', IW, IX, IY, IZ, IOR0, IC
      WRITE(MSG%LU_DEBUG,*) 'INTERFACE: PERM_FW(', IC, ')=', G%PERM_FW(IC),', PERM_BW(', JC, ')=', G%PERM_BW(JC)
#endif
      IF (G%PERM_FW(IC) == 0) THEN
         G%PERM_FW(IC) = JC
         G%PERM_BW(JC) = IC
         JC = JC - 1
      ENDIF

   ENDDO
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'AFTER INTERFACE: PERM_FW:'
   WRITE(MSG%LU_DEBUG,'(8I4)') G%PERM_FW
   WRITE(MSG%LU_DEBUG,*) 'AFTER INTERFACE: PERM_BW:'
   WRITE(MSG%LU_DEBUG,'(8I4)') G%PERM_BW
#endif

   ! The rest is used from beginning to first interface cell

   KC = 1
   DO IC = 1, G%NC
      IF (G%PERM_FW(IC) /= 0) CYCLE
      G%PERM_BW(KC) = IC
      G%PERM_FW(IC) = KC
      KC = KC + 1
   ENDDO
   IF (KC /= JC + 1) THEN
      WRITE(*,*) 'KC =', KC,': JC =', JC
      CALL SCARC_ERROR(NSCARC_ERROR_MGM_PERMUTATION, SCARC_NONE, NSCARC_NONE)
   ENDIF

   G%NONZERO = KC

ELSE

   DO IC = 1, G%NC
      G%PERM_BW(IC) = IC
      G%PERM_FW(IC) = IC
   ENDDO

   G%NONZERO = 1

ENDIF
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'AFTER FINAL FILL: PERM_FW:'
WRITE(MSG%LU_DEBUG,'(8I4)') G%PERM_FW
WRITE(MSG%LU_DEBUG,*) 'AFTER FINAL FILL: PERM_BW:'
WRITE(MSG%LU_DEBUG,'(8I4)') G%PERM_BW
WRITE(MSG%LU_DEBUG,*) 'G%ICX'
WRITE(MSG%LU_DEBUG,'(8I4)') G%ICX
WRITE(MSG%LU_DEBUG,*) 'G%ICY'
WRITE(MSG%LU_DEBUG,'(8I4)') G%ICY
WRITE(MSG%LU_DEBUG,*) 'G%ICZ'
WRITE(MSG%LU_DEBUG,'(8I4)') G%ICZ
DO IZ = 1, L%NZ
   DO IY = 1, L%NY
      WRITE(MSG%LU_DEBUG,*) (L%IS_SOLID(IX, IY, IZ), IX=1, L%NX)
   ENDDO
ENDDO
#endif

! Allocate Laplace matrix on non-overlapping part of mesh

A => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_LAPLACE)
CALL SCARC_ALLOCATE_CMATRIX (A, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_FULL, 'G%LAPLACE', CROUTINE)

! Assemble Laplace matrix with grid permutation based on MGM-method 

IP = 1
IF (TYPE_MGM_LAPLACE == NSCARC_MGM_LAPLACE_LUPERM) THEN

   DO IC = 1, G%NC

      JJC = -1
      KKC = -1

      JJC(0) = G%PERM_BW(IC);  KKC(0) = G%PERM_FW(JJC(0))
      
      IX = G%ICX(JJC(0)); IY = G%ICY(JJC(0)); IZ = G%ICZ(JJC(0))

      JJC(-3) = G%CELL_NUMBER(IX  , IY, IZ+1)     ; KKC(-3) = GET_PERM(JJC(-3))  
      JJC(-1) = G%CELL_NUMBER(IX+1, IY, IZ  )     ; KKC(-1) = GET_PERM(JJC(-1))   
      JJC( 1) = G%CELL_NUMBER(IX-1, IY, IZ  )     ; KKC( 1) = GET_PERM(JJC( 1))    
      JJC( 3) = G%CELL_NUMBER(IX  , IY, IZ-1)     ; KKC( 3) = GET_PERM(JJC( 3))     
      IF (.NOT.TWO_D) THEN
        JJC(-2) = G%CELL_NUMBER(IX, IY+1, IZ)     ; KKC(-2) = GET_PERM(JJC(-2))     
        JJC( 2) = G%CELL_NUMBER(IX, IY-1, IZ)     ; KKC( 2) = GET_PERM(JJC( 2))     
      ENDIF

      ! Main diagonal 
      CALL SCARC_SETUP_MAINDIAG (IC, IX, IY, IZ, IP)
#ifdef WITH_SCARC_DEBUG
         WRITE(MSG%LU_DEBUG,*) '======================================='
         WRITE(MSG%LU_DEBUG,*) 'JJC = ', JJC
         WRITE(MSG%LU_DEBUG,*) 'KKC = ', KKC
         WRITE(MSG%LU_DEBUG,*) 'IX, IY, IZ=', IX, IY, IZ
#endif
         
      ! Lower subdiagonals

      IF (IS_VALID_DIRECTION(IX, IY, IZ,  3)) CALL SCARC_SETUP_SUBDIAG_PERM(IX, IY, IZ, IX  , IY  , IZ-1, KKC( 3), IP,  3)
      IF (IS_VALID_DIRECTION(IX, IY, IZ,  2)) CALL SCARC_SETUP_SUBDIAG_PERM(IX, IY, IZ, IX  , IY-1, IZ  , KKC( 2), IP,  2)
      IF (IS_VALID_DIRECTION(IX, IY, IZ,  1)) CALL SCARC_SETUP_SUBDIAG_PERM(IX, IY, IZ, IX-1, IY  , IZ  , KKC( 1), IP,  1)

      ! Upper subdiagonals

      IF (IS_VALID_DIRECTION(IX, IY, IZ, -1)) CALL SCARC_SETUP_SUBDIAG_PERM(IX, IY, IZ, IX+1, IY  , IZ  , KKC(-1), IP, -1)
      IF (IS_VALID_DIRECTION(IX, IY, IZ, -2)) CALL SCARC_SETUP_SUBDIAG_PERM(IX, IY, IZ, IX  , IY+1, IZ  , KKC(-2), IP, -2)
      IF (IS_VALID_DIRECTION(IX, IY, IZ, -3)) CALL SCARC_SETUP_SUBDIAG_PERM(IX, IY, IZ, IX  , IY  , IZ+1, KKC(-3), IP, -3)

   ENDDO
      
! Assemble Laplace matrix without grid permutation 

ELSE

   DO IZ = 1, L%NZ
      DO IY = 1, L%NY
         DO IX = 1, L%NX

            IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(IX, IY, IZ)) CYCLE
            IC = G%CELL_NUMBER(IX, IY, IZ)

            ! Main diagonal 

            CALL SCARC_SETUP_MAINDIAG (IC, IX, IY, IZ, IP)

            ! Lower subdiagonals

            IF (IS_VALID_DIRECTION(IX, IY, IZ,  3)) CALL SCARC_SETUP_SUBDIAG(IC, IX, IY, IZ, IX  , IY  , IZ-1, IP,  3)
            IF (IS_VALID_DIRECTION(IX, IY, IZ,  2)) CALL SCARC_SETUP_SUBDIAG(IC, IX, IY, IZ, IX  , IY-1, IZ  , IP,  2)
            IF (IS_VALID_DIRECTION(IX, IY, IZ,  1)) CALL SCARC_SETUP_SUBDIAG(IC, IX, IY, IZ, IX-1, IY  , IZ  , IP,  1)

            ! Upper subdiagonals

            IF (IS_VALID_DIRECTION(IX, IY, IZ, -1)) CALL SCARC_SETUP_SUBDIAG(IC, IX, IY, IZ, IX+1, IY  , IZ  , IP, -1)
            IF (IS_VALID_DIRECTION(IX, IY, IZ, -2)) CALL SCARC_SETUP_SUBDIAG(IC, IX, IY, IZ, IX  , IY+1, IZ  , IP, -2)
            IF (IS_VALID_DIRECTION(IX, IY, IZ, -3)) CALL SCARC_SETUP_SUBDIAG(IC, IX, IY, IZ, IX  , IY  , IZ+1, IP, -3)

         ENDDO
      ENDDO
   ENDDO
      
ENDIF

A%ROW(G%NC+1) = IP
A%N_VAL = IP
      
CALL SCARC_GET_MATRIX_STENCIL_MAX(A, G%NC)

TYPE_SCOPE(0) = TYPE_SCOPE_SAVE

#ifdef WITH_SCARC_DEBUG2
CALL SCARC_DEBUG_CMATRIX (A, 'LAPLACE', 'SETUP_LAPLACE_VAR: NO BDRY')
#endif
 
END SUBROUTINE SCARC_SETUP_LAPLACE_VAR


! --------------------------------------------------------------------------------------------------------------
!> \brief Determine if cell has a neighbor and, if yes, return corresponding wall cell index
! --------------------------------------------------------------------------------------------------------------
INTEGER FUNCTION SCARC_ASSIGN_SUBDIAG_TYPE (IC, IOR0)
USE SCARC_POINTERS, ONLY: L, G, F, GWC
INTEGER, INTENT(IN) :: IC, IOR0
INTEGER :: IXW, IYW, IZW
INTEGER :: IXG, IYG, IZG
INTEGER :: IW

SCARC_ASSIGN_SUBDIAG_TYPE = -1

F => L%FACE(IOR0)
SEARCH_WALL_CELLS_LOOP: DO IW = F%NCW0, F%NCW0 + F%NCW - 1

   GWC => G%WALL(IW)

   IF (GWC%NOM == 0) CYCLE
   IXW = GWC%IXW
   IYW = GWC%IYW
   IZW = GWC%IZW

   IF (G%CELL_NUMBER(IXW, IYW, IZW) /= IC) CYCLE
   IXG = GWC%IXG
   IYG = GWC%IYG
   IZG = GWC%IZG

   IF (IS_UNSTRUCTURED.AND.L%IS_SOLID(IXG, IYG, IZG)) RETURN
   SCARC_ASSIGN_SUBDIAG_TYPE = IW
   RETURN

ENDDO SEARCH_WALL_CELLS_LOOP

END FUNCTION SCARC_ASSIGN_SUBDIAG_TYPE


! --------------------------------------------------------------------------------------------------------------
!> \brief Set main diagonal entry for Poisson matrix in compact storage technique
! These values correspond to the full matrix of the global problem
! In case of an equidistant grid, we get the usual 5-point (2d) and 7-point (3d) stencil
! If two meshes with different step sizes meet, we get a weighted stencil along internal wall cells
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MAINDIAG (IC, IX, IY, IZ, IP)
USE SCARC_POINTERS, ONLY: A, RDX, RDY, RDZ, RDXN, RDYN, RDZN
INTEGER, INTENT(IN) :: IC, IX, IY, IZ
INTEGER, INTENT(INOUT) :: IP

A%VAL(IP) = - RDX(IX)*(RDXN(IX) + RDXN(IX-1))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,5I4, 3E12.4)') 'MAIN-X: IC, IX , IY , IZ , IP, A%VAL(IP):', IC, IX, IY, IZ, IP, A%VAL(IP)
#endif
IF (.NOT.TWO_D) A%VAL(IP) = A%VAL(IP) - RDY(IY)*(RDYN(IY) + RDYN(IY-1))
A%VAL(IP) = A%VAL(IP) - RDZ(IZ)*(RDZN(IZ) + RDZN(IZ-1))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,5I4, 3E12.4)') 'MAIN-Z: IC, IX , IY , IZ , IP, A%VAL(IP):', IC, IX, IY, IZ, IP, A%VAL(IP)
#endif

A%ROW(IC) = IP
A%COL(IP) = IC

A%STENCIL(0) = A%VAL(IP)

IP = IP + 1
END SUBROUTINE SCARC_SETUP_MAINDIAG


! --------------------------------------------------------------------------------------------------------------
!> \brief Set subdigonal entries for Poisson matrix in compact storage technique on specified face
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_SUBDIAG (IC, IX1, IY1, IZ1, IX2, IY2, IZ2, IP, IOR0)
USE SCARC_POINTERS, ONLY: L, G, A, RDX, RDY, RDZ, RDXN, RDYN, RDZN
USE SCARC_UTILITIES, ONLY: IS_INTERNAL_CELL
INTEGER, INTENT(IN) :: IC, IX1, IY1, IZ1, IX2, IY2, IZ2, IOR0
INTEGER, INTENT(INOUT) :: IP
INTEGER :: IW

! If IC is an internal cell of the mesh, compute usual matrix contribution for corresponding subdiagonal
IF (IS_INTERNAL_CELL(IX1, IY1, IZ1, IOR0)) THEN

   IF (IS_STRUCTURED .OR. .NOT.L%IS_SOLID(IX2, IY2, IZ2)) THEN
      SELECT CASE(IOR0)
         CASE (1)
            A%VAL(IP) = RDX(IX1)*RDXN(IX1-1)
         CASE (-1)
            A%VAL(IP) = RDX(IX1)*RDXN(IX1)
         CASE (2)
            A%VAL(IP) = RDY(IY1)*RDYN(IY1-1)
         CASE (-2)
            A%VAL(IP) = RDY(IY1)*RDYN(IY1)
         CASE (3)
            A%VAL(IP) = RDZ(IZ1)*RDZN(IZ1-1)
         CASE (-3)
            A%VAL(IP) = RDZ(IZ1)*RDZN(IZ1)
      END SELECT
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,6I4, 3E12.4)') 'SUB-I : IC, IX1, IY1, IZ1, IP, IOR0, A%VAL(IP):', IC, IX1, IY1, IZ1, IP, IOR0, A%VAL(IP)
#endif
      A%COL(IP) = G%CELL_NUMBER(IX2, IY2, IZ2)
      A%STENCIL(-IOR0) = A%VAL(IP)
      IP = IP + 1
   ELSE
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'IX1, IY1, IZ1, IX2, IY2, IZ2, L%IS_SOLID(IX2, IY2, IZ2):', &
                       IX1, IY1, IZ1, IX2, IY2, IZ2, L%IS_SOLID(IX2, IY2, IZ2)
#endif
      CALL SCARC_ERROR(NSCARC_ERROR_MATRIX_SUBDIAG, SCARC_NONE, NSCARC_NONE)
   ENDIF

! If IC is a boundary cell of the mesh, compute matrix contribution only if there is a neighbor for that cell

ELSE IF (TYPE_SCOPE(0) == NSCARC_SCOPE_GLOBAL .AND. L%FACE(IOR0)%N_NEIGHBORS /= 0) THEN

   IW = SCARC_ASSIGN_SUBDIAG_TYPE (IC, IOR0)           ! get IW of a possibly suitable neighbor at face IOR0
   IF (IW > 0) then                                    ! if available, build corresponding subdiagonal entry
      SELECT CASE(IOR0)
         CASE (1)
            A%VAL(IP) = RDX(IX1)*RDXN(IX1-1)
         CASE (-1)
            A%VAL(IP) = RDX(IX1)*RDXN(IX1)
         CASE (2)
            A%VAL(IP) = RDY(IY1)*RDYN(IY1-1)
         CASE (-2)
            A%VAL(IP) = RDY(IY1)*RDYN(IY1)
         CASE (3)
            A%VAL(IP) = RDZ(IZ1)*RDZN(IZ1-1)
         CASE (-3)
            A%VAL(IP) = RDZ(IZ1)*RDZN(IZ1)
      END SELECT
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,6I4, 3E12.4)') 'SUB-E : IC, IX1, IY1, IZ1, IP, IOR0, A%VAL(IP):', IC, IX1, IY1, IZ1, IP, IOR0, A%VAL(IP)
#endif
      A%COL(IP) = G%WALL(IW)%ICE                       ! store its extended number in matrix column pointers
      A%STENCIL(-IOR0) = A%VAL(IP)
      IP = IP + 1
   ENDIF

ENDIF

END SUBROUTINE SCARC_SETUP_SUBDIAG


! --------------------------------------------------------------------------------------------------------------
!> \brief Set main diagonal entry for Poisson matrix in compact storage technique
! These values correspond to the full matrix of the global problem
! In case of an equidistant grid, we get the usual 5-point (2d) and 7-point (3d) stencil
! If two meshes with different step sizes meet, we get a weighted stencil along internal wall cells
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MAINDIAG_VAR (IC, IX, IY, IZ, IP)
USE SCARC_POINTERS, ONLY: M, A, RDX, RDY, RDZ, RDXN, RDYN, RDZN
REAL(EB), DIMENSION(:,:,:), POINTER :: RHOP
INTEGER, INTENT(IN) :: IC, IX, IY, IZ
INTEGER, INTENT(INOUT) :: IP
REAL(EB) :: RXP, RXM, RYP, RYM, RZP, RZM

IF (PREDICTOR) THEN
   RHOP => M%RHO
ELSE
   RHOP => M%RHOS
ENDIF
RXP = 2.0_EB /(RHOP(IX+1,IY,IZ) + RHOP(IX  ,IY,IZ))
RXM = 2.0_EB /(RHOP(IX  ,IY,IZ) + RHOP(IX-1,IY,IZ))
A%VAL(IP) = - RDX(IX)*(RDXN(IX)*RXP + RDXN(IX-1)*RXM)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,5I4, 3E12.4)') 'MAIN-X: IC, IX , IY , IZ , IP, RXP, RXM, A%VAL(IP):', IC, IX, IY, IZ, IP, RXP, RXM, A%VAL(IP)
#endif

IF (.NOT.TWO_D) THEN
   RYP = 2.0_EB /(RHOP(IX,IY+1,IZ) + RHOP(IX,IY  ,IZ))
   RYM = 2.0_EB /(RHOP(IX,IY  ,IZ) + RHOP(IX,IY-1,IZ))
   A%VAL(IP) = A%VAL(IP) - RDY(IY)*(RDYN(IY)*RYP + RDYN(IY-1)*RYM)
ENDIF

RZP = 2.0_EB /(RHOP(IX,IY,IZ+1) + RHOP(IX,IY,IZ  ))
RZM = 2.0_EB /(RHOP(IX,IY,IZ  ) + RHOP(IX,IY,IZ-1))
A%VAL(IP) = A%VAL(IP) - RDZ(IZ)*(RDZN(IZ)*RZP + RDZN(IZ-1)*RZM)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,5I4, 3E12.4)') 'MAIN-Z: IC, IX , IY , IZ , IP, RZP, RZM, A%VAL(IP):', IC, IX, IY, IZ, IP, RZP, RZM, A%VAL(IP)
#endif
A%ROW(IC) = IP
A%COL(IP) = IC

A%STENCIL(0) = A%VAL(IP)
IP = IP + 1

END SUBROUTINE SCARC_SETUP_MAINDIAG_VAR


! --------------------------------------------------------------------------------------------------------------
!> \brief Set subdigonal entries for Poisson matrix in compact storage technique on specified face
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_SUBDIAG_VAR (IC, IX1, IY1, IZ1, IX2, IY2, IZ2, IP, IOR0)
USE SCARC_POINTERS, ONLY: M, L, G, A, RDX, RDY, RDZ, RDXN, RDYN, RDZN
USE SCARC_UTILITIES, ONLY: IS_INTERNAL_CELL
REAL(EB), DIMENSION(:,:,:), POINTER :: RHOP
INTEGER, INTENT(IN) :: IC, IX1, IY1, IZ1, IX2, IY2, IZ2, IOR0
INTEGER, INTENT(INOUT) :: IP
INTEGER :: IW
REAL(EB) :: RHOM

! If IC is an internal cell of the mesh, compute usual matrix contribution for corresponding subdiagonal
! E.g.: ABS(IOR0)=1: then IX2 is either IX1-1 or IX1+1, so build corresponding meanvalue for RHO

IF (PREDICTOR) THEN
   RHOP => M%RHO
ELSE
   RHOP => M%RHOS
ENDIF

RHOM = 2.0_EB /(RHOP(IX1, IY1, IZ1) + RHOP(IX2, IY2, IZ2))

IF (IS_INTERNAL_CELL(IX1, IY1, IZ1, IOR0)) THEN

   IF (IS_STRUCTURED .OR. .NOT.L%IS_SOLID(IX2, IY2, IZ2)) THEN
      SELECT CASE(IOR0)
         CASE (1)
            A%VAL(IP) = RDX(IX1)*RDXN(IX1-1)*RHOM
         CASE (-1)
            A%VAL(IP) = RDX(IX1)*RDXN(IX1)  *RHOM
         CASE (2)
            A%VAL(IP) = RDY(IY1)*RDYN(IY1-1)*RHOM
         CASE (-2)
            A%VAL(IP) = RDY(IY1)*RDYN(IY1)  *RHOM
         CASE (3)
            A%VAL(IP) = RDZ(IZ1)*RDZN(IZ1-1)*RHOM
         CASE (-3)
            A%VAL(IP) = RDZ(IZ1)*RDZN(IZ1)  *RHOM
      END SELECT
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,5I4, E12.4,I4, E12.4)') 'SUB-I : IC, IX1, IY1, IZ1, IP, RHOM, IOR0, A%VAL(IP):', &
                                               IC, IX1, IY1, IZ1, IP, RHOM, IOR0, A%VAL(IP)
#endif
      A%COL(IP) = G%CELL_NUMBER(IX2, IY2, IZ2)
      A%STENCIL(-IOR0) = A%VAL(IP)
      IP = IP + 1
   ELSE
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'IX1, IY1, IZ1, IX2, IY2, IZ2, L%IS_SOLID(IX2, IY2, IZ2):', &
                       IX1, IY1, IZ1, IX2, IY2, IZ2, L%IS_SOLID(IX2, IY2, IZ2)
#endif
      CALL SCARC_ERROR(NSCARC_ERROR_MATRIX_SUBDIAG, SCARC_NONE, NSCARC_NONE)
   ENDIF

! If IC is a boundary cell of the mesh, compute matrix contribution only if there is a neighbor for that cell

ELSE IF (TYPE_SCOPE(0) == NSCARC_SCOPE_GLOBAL .AND. L%FACE(IOR0)%N_NEIGHBORS /= 0) THEN

   IW = SCARC_ASSIGN_SUBDIAG_TYPE (IC, IOR0)           ! get IW of a possibly suitable neighbor at face IOR0
   IF (IW > 0) then                                    ! if available, build corresponding subdiagonal entry
      SELECT CASE(IOR0)
         CASE (1)
            A%VAL(IP) = RDX(IX1)*RDXN(IX1-1)*RHOM
         CASE (-1)
            A%VAL(IP) = RDX(IX1)*RDXN(IX1)  *RHOM
         CASE (2)
            A%VAL(IP) = RDY(IY1)*RDYN(IY1-1)*RHOM
         CASE (-2)
            A%VAL(IP) = RDY(IY1)*RDYN(IY1)  *RHOM
         CASE (3)
            A%VAL(IP) = RDZ(IZ1)*RDZN(IZ1-1)*RHOM
         CASE (-3)
            A%VAL(IP) = RDZ(IZ1)*RDZN(IZ1)  *RHOM
      END SELECT
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,5I4, E12.4,I4, E12.4)') 'SUB-E : IC, IX1, IY1, IZ1, IP, RHOM, IOR0, A%VAL(IP):', &
                                               IC, IX1, IY1, IZ1, IP, RHOM, IOR0, A%VAL(IP)
#endif
      A%COL(IP) = G%WALL(IW)%ICE                       ! store its extended number in matrix column pointers
      A%STENCIL(-IOR0) = A%VAL(IP)
      IP = IP + 1
   ENDIF

ENDIF

END SUBROUTINE SCARC_SETUP_SUBDIAG_VAR


! --------------------------------------------------------------------------------------------------------------
!> \brief Set subdigonal entries for permuted matrix in compact storage technique on specified face
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_SUBDIAG_PERM (IX1, IY1, IZ1, IX2, IY2, IZ2, ICOL, IP, IOR0)
USE SCARC_POINTERS, ONLY: L, F, A
INTEGER, INTENT(IN) :: IX1, IY1, IZ1, IX2, IY2, IZ2, IOR0, ICOL
INTEGER, INTENT(INOUT) :: IP
LOGICAL :: IS_INTERNAL_CELL

!A => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_POISSON)

! Decide wheter cell is interior or exterior cell

F => L%FACE(IOR0)
SELECT CASE (IOR0)
   CASE ( 1)
      IS_INTERNAL_CELL = IX1 > 1
   CASE (-1)
      IS_INTERNAL_CELL = IX1 < F%NOP
   CASE ( 2)
      IS_INTERNAL_CELL = IY1 > 1
   CASE (-2)
      IS_INTERNAL_CELL = IY1 < F%NOP
   CASE ( 3)
      IS_INTERNAL_CELL = IZ1 > 1
   CASE (-3)
      IS_INTERNAL_CELL = IZ1 < F%NOP
END SELECT

! If IC is an internal cell of the mesh, compute usual matrix contribution for corresponding subdiagonal
IF (IS_INTERNAL_CELL .AND. .NOT.L%IS_SOLID(IX2, IY2, IZ2)) THEN
   A%VAL(IP) = A%VAL(IP) + F%INCR_INSIDE
   A%COL(IP) = ICOL
   A%STENCIL(-IOR0) = A%VAL(IP)
   IP = IP + 1
ENDIF

END SUBROUTINE SCARC_SETUP_SUBDIAG_PERM


! --------------------------------------------------------------------------------------------------------------
!> \brief Set boundary conditions including the interfaces between the meshes
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_BOUNDARY_WITH_INTERFACES (NM, NL)
USE SCARC_POINTERS, ONLY: L, G, F, GWC, A, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NM, NL
INTEGER :: I, J, K, IOR0, IW, IC, NOM, IP
INTEGER :: ICXM, ICXP, ICYM, ICYP, ICZM, ICZP

CALL SCARC_POINT_TO_GRID (NM, NL)       

A => G%LAPLACE

SELECT CASE (TYPE_MGM_BC)

   ! --------------------------------------------------------------------------
   CASE (NSCARC_MGM_BC_TAYLOR)

      !DO IW = MGM%NW1, MGM%NW2
      DO IW = 1, G%NW

         GWC => G%WALL(IW)
         IOR0 = GWC%IOR
         IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE       

         F  => L%FACE(IOR0)

         I = GWC%IXW
         J = GWC%IYW
         K = GWC%IZW

         IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE

         NOM = GWC%NOM
         IC  = G%CELL_NUMBER(I, J, K)

         ICXM  = G%CELL_NUMBER(I-1, J, K)
         ICXP  = G%CELL_NUMBER(I+1, J, K)
         IF (.NOT. TWO_D) THEN
            ICYM  = G%CELL_NUMBER(I, J-1, K)
            ICYP  = G%CELL_NUMBER(I, J+1, K)
         ENDIF
         ICZM  = G%CELL_NUMBER(I, J, K-1)
         ICZP  = G%CELL_NUMBER(I, J, K+1)

         GWC%ICW = IC

         IP = A%ROW(IC)
         A%VAL(IP) = 0.0_EB
         SELECT CASE(ABS(IOR0))
            CASE (1)
               A%VAL(IP) = A%VAL(IP) - L%DXI2 - 5.0_EB/2.0_EB*L%DZI2
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,7I6,E14.6)') 'A :MGM-TAYLOR: IOR0, IW, I, J, K, IC, IC  , A%VAL :', IOR0, IW, I, J, K, IC, IC, A%VAL(IP)
#endif
               DO IP = A%ROW(IC)+1, A%ROW(IC+1)-1
                  IF (ICXM <= G%NC .AND. A%COL(IP) == ICXM) THEN
                     A%VAL(IP) = A%VAL(IP) + L%DXI2 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,7I6,E14.6)') 'A :MGM-TAYLOR:   " , IW, I, J, K, IC, ICXM, A%VAL :', IOR0, IW, I, J, K, IC, ICXM, A%VAL(IP)
#endif
                  ENDIF
                  IF (ICXP <= G%NC .AND. A%COL(IP) == ICXP) THEN
                     A%VAL(IP) = A%VAL(IP) + L%DXI2 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,7I6,E14.6)') 'A :MGM-TAYLOR:   " , IW, I, J, K, IC, ICXP, A%VAL :', IOR0, IW, I, J, K, IC, ICXP, A%VAL(IP)
#endif
                  ENDIF
                  IF (ICZM <= G%NC .AND. A%COL(IP) == ICZM) THEN
                     A%VAL(IP) = A%VAL(IP) + 5.0_EB/4.0_EB*L%DZI2
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,7I6,E14.6)') 'A :MGM-TAYLOR:   " , IW, I, J, K, IC, ICZM, A%VAL :', IOR0, IW, I, J, K, IC, ICZM, A%VAL(IP)
#endif
                  ENDIF
                  IF (ICZP <= G%NC .AND. A%COL(IP) == ICZP) THEN
                        A%VAL(IP) = A%VAL(IP) + 5.0_EB/4.0_EB*L%DZI2
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,7I6,E14.6)') 'A :MGM-TAYLOR:   " , IW, I, J, K, IC, ICZP, A%VAL :', IOR0, IW, I, J, K, IC, ICZP, A%VAL(IP)
#endif
                  ENDIF
               ENDDO
            CASE (2)
               IF (.NOT. TWO_D) THEN
                  WRITE(*,*) 'TAYLOR-3D: Not yet finished!'
               ENDIF
            CASE (3)
               A%VAL(IP) = A%VAL(IP) - L%DZI2 - 5.0_EB/2.0_EB*L%DXI2
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,7I6,E14.6)') 'A :MGM-TAYLOR: IOR0, IW, I, J, K, IC, IC  , A%VAL :', IOR0, IW, I, J, K, IC, IC, A%VAL(IP)
#endif
               DO IP = A%ROW(IC)+1, A%ROW(IC+1)-1
                  IF (ICZM <= G%NC .AND. A%COL(IP) == ICZM) THEN
                     A%VAL(IP) = A%VAL(IP) + L%DZI2 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,7I6,E14.6)') 'A :MGM-TAYLOR:   " , IW, I, J, K, IC, ICZM, A%VAL :', IOR0, IW, I, J, K, IC, ICZM, A%VAL(IP)
#endif
                  ENDIF
                  IF (ICZP <= G%NC .AND. A%COL(IP) == ICZP) THEN
                     A%VAL(IP) = A%VAL(IP) + L%DZI2 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,7I6,E14.6)') 'A :MGM-TAYLOR:   " , IW, I, J, K, IC, ICZM, A%VAL :', IOR0, IW, I, J, K, IC, ICZM, A%VAL(IP)
#endif
                  ENDIF
                  IF (ICXM <= G%NC .AND. A%COL(IP) == ICXM) THEN
                     A%VAL(IP) = A%VAL(IP) + 5.0_EB/4.0_EB*L%DXI2
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,7I6,E14.6)') 'A :MGM-TAYLOR:   " , IW, I, J, K, IC, ICXM, A%VAL :', IOR0, IW, I, J, K, IC, ICXM, A%VAL(IP)
#endif
                  ENDIF
                  IF (ICXP <= G%NC .AND. A%COL(IP) == ICXP) THEN
                     A%VAL(IP) = A%VAL(IP) + 5.0_EB/4.0_EB*L%DXI2
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,7I6,E14.6)') 'A :MGM-TAYLOR:   " , IW, I, J, K, IC, ICXP, A%VAL :', IOR0, IW, I, J, K, IC, ICXP, A%VAL(IP)
#endif
                  ENDIF
               ENDDO
         END SELECT
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*)
#endif

      ENDDO 

   ! --------------------------------------------------------------------------
   CASE DEFAULT

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'BBB: BTYPE FOR LEVEL ', NL
WRITE(MSG%LU_DEBUG,'(8I6)') G%WALL(1:G%NW)%BTYPE
#endif
      DO IW = 1, G%NW

         GWC => G%WALL(IW)
         IOR0 = GWC%IOR
         IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE       

         F  => L%FACE(IOR0)

         I = GWC%IXW
         J = GWC%IYW
         K = GWC%IZW

         IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE

         NOM = GWC%NOM
         IF (TYPE_MGM_LAPLACE == NSCARC_MGM_LAPLACE_LUPERM) THEN
            IC  = G%PERM_FW(G%CELL_NUMBER(I, J, K))
         ELSE
            IC  = G%CELL_NUMBER(I, J, K)
         ENDIF
         !GWC%ICW = IC

         IP = A%ROW(IC)
         SELECT CASE (GWC%BTYPE)
            CASE (DIRICHLET, INTERNAL)
               A%VAL(IP) = A%VAL(IP) - F%INCR_BOUNDARY
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,6I6,E14.6)') 'BBB :DIRICHLET: IW, I, J, K, NOM, IC, A%VAL:', IW, I, J, K, NOM, IC, A%VAL(IP)
#endif
            CASE (NEUMANN)
               A%VAL(IP) = A%VAL(IP) + F%INCR_BOUNDARY
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,6I6,E14.6)') 'BBB :NEUMANN  : IW, I, J, K, NOM, IC, A%VAL:', IW, I, J, K, NOM, IC, A%VAL(IP)
#endif
         END SELECT

      ENDDO 

END SELECT 

#ifdef WITH_SCARC_DEBUG2
CALL SCARC_DEBUG_CMATRIX(A, 'LAPLACE', 'LAPLACE AFTER MGM_SETUP_BOUNDARY')
#endif

END SUBROUTINE SCARC_SETUP_BOUNDARY_WITH_INTERFACES


! --------------------------------------------------------------------------------------------------------------
!> \brief Set main diagonal entry for Poisson matrix in bandwise storage technique
! These values correspond to the full matrix of the global problem
! In case of an equidistant grid, we get the usual 5-point (2d) and 7-point (3d) stencil
! If two meshes with different step sizes meet, we get a weighted stencil along internal wall cells
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MAINDIAGB (IC, IX, IY, IZ)
USE SCARC_POINTERS, ONLY: L, AB, SCARC_POINT_TO_BMATRIX
INTEGER, INTENT(IN)  :: IC, IX, IY, IZ
INTEGER :: ID

AB => SCARC_POINT_TO_BMATRIX (NSCARC_MATRIX_POISSON)
ID = AB%POS(0)               ! get column vector corresponding to matrix diagonal

AB%VAL(IC, ID) = AB%VAL(IC, ID) - 2.0_EB/(L%DXL(IX-1)*L%DXL(IX))
IF (.NOT.TWO_D)  AB%VAL(IC, ID) = AB%VAL(IC, ID) - 2.0_EB/(L%DYL(IY-1)*L%DYL(IY))
AB%VAL(IC, ID) = AB%VAL(IC, ID) - 2.0_EB/(L%DZL(IZ-1)*L%DZL(IZ))

AB%STENCIL(0) = AB%VAL(IC, ID)

END SUBROUTINE SCARC_SETUP_MAINDIAGB


! --------------------------------------------------------------------------------------------------------------
!> \brief Set subdigonal entries for Poisson matrix in bandwise storage technique on specified face
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_SUBDIAGB (IC, IX1, IY1, IZ1, IX2, IY2, IZ2, IOR0)
USE SCARC_POINTERS, ONLY: L, F, AB, SCARC_POINT_TO_BMATRIX
INTEGER, INTENT(IN) :: IC, IX1, IY1, IZ1, IX2, IY2, IZ2, IOR0
INTEGER :: IW, ID
LOGICAL  :: IS_INTERNAL_CELL

F => L%FACE(IOR0)

! Decide wheter cell is interior or exterior cell
 
AB => SCARC_POINT_TO_BMATRIX (NSCARC_MATRIX_POISSON)
ID = AB%POS(IOR0)                                
SELECT CASE (IOR0)
   CASE ( 1)
      IS_INTERNAL_CELL = IX1 > 1
   CASE (-1)
      IS_INTERNAL_CELL = IX1 < F%NOP
   CASE ( 2)
      IS_INTERNAL_CELL = IY1 > 1
   CASE (-2)
      IS_INTERNAL_CELL = IY1 < F%NOP
   CASE ( 3)
      IS_INTERNAL_CELL = IZ1 > 1
   CASE (-3)
      IS_INTERNAL_CELL = IZ1 < F%NOP
END SELECT

! If IC is an internal cell of the mesh, compute usual matrix contribution for corresponding subdiagonal
 
IF (IS_INTERNAL_CELL) THEN

   IF (IS_STRUCTURED .OR. .NOT.L%IS_SOLID(IX2, IY2, IZ2)) THEN
      AB%VAL(IC, ID)   = AB%VAL(IC, ID) + F%INCR_INSIDE
      AB%STENCIL(IOR0) = AB%VAL(IC, ID)
   ELSE
      CALL SCARC_ERROR(NSCARC_ERROR_MATRIX_SUBDIAG, SCARC_NONE, NSCARC_NONE)
   ENDIF

! If IC is a boundary cell of the mesh, compute matrix contribution only if there is a neighbor for that cell
 
!ELSE IF (L%FACE(IOR0)%N_NEIGHBORS /= 0) THEN
ELSE IF (L%FACE(IOR0)%N_NEIGHBORS == 123456) THEN       ! CAUTION: TO FIX AGAIN, ONLY FOR TESTING, IMPOSSIBLE CONDITION

   IW = SCARC_ASSIGN_SUBDIAG_TYPE (IC, IOR0)            ! get IW of a possibly suitable neighbor at face IOR0
   IF (IW /= 0) THEN
      AB%VAL(IC, ID)   = AB%VAL(IC, ID) + F%INCR_FACE
      AB%STENCIL(IOR0) = AB%VAL(IC, ID)
   ENDIF

ENDIF

END SUBROUTINE SCARC_SETUP_SUBDIAGB


! --------------------------------------------------------------------------------------------------------------
!> \brief Get maximum stencil size in specified matrix 
! This is known to be 7 for the 3D-Poisson matrix on finest level
! In algebraic multigrid-method this size results only in the course and can be much larger
! (required for dimensioning the coarse-level matrices)
! If NTYPE == 0, only internal matrix part is considered, if NTYPE == 1, also the overlap
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_GET_MATRIX_STENCIL_MAX (A, NLEN)
TYPE (SCARC_CMATRIX_TYPE), INTENT(INOUT) :: A
INTEGER, INTENT(IN) :: NLEN
INTEGER :: IC

A%N_STENCIL_MAX = 0
DO IC = 1, NLEN
   A%N_STENCIL_MAX = MAX(A%N_STENCIL_MAX, A%ROW(IC+1)-A%ROW(IC)+1)
ENDDO
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'GET_STENCIL_MAX:', A%N_STENCIL_MAX
#endif

END SUBROUTINE SCARC_GET_MATRIX_STENCIL_MAX


SUBROUTINE SCARC_MATRIX_CHECK_NEUMANN (A, NC)
TYPE (SCARC_CMATRIX_TYPE), INTENT(INOUT) :: A
REAL(EB):: ROW_SUM
INTEGER, INTENT(IN) :: NC
INTEGER :: IC, ICOL

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'ROW_SUM: NC:', NC, TWO_EPSILON_EB
#endif
A%CONDENSING_REQUIRED = .TRUE.
ROW_LOOP: DO IC = 1, NC
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) (A%VAL(ICOL), ICOL=A%ROW(IC), A%ROW(IC+1)-1)
#endif
   ROW_SUM = 0.0_EB
   DO ICOL = A%ROW(IC), A%ROW(IC+1)-1
      ROW_SUM = ROW_SUM + A%VAL(ICOL)
   ENDDO
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'ROW_SUM: IC:', IC, ROW_SUM
#endif
   IF (ROW_SUM > TWO_EPSILON_EB) THEN
      A%CONDENSING_REQUIRED = .FALSE.
      EXIT ROW_LOOP
   ENDIF
ENDDO ROW_LOOP

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'A%CONDENSING_REQUIRED = ', A%CONDENSING_REQUIRED
#endif

END SUBROUTINE SCARC_MATRIX_CHECK_NEUMANN


#ifdef WITH_MKL
! --------------------------------------------------------------------------------------------------------------
!> \brief Setup symmetric version of Poisson matrix for MKL solver in double precision
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_MATRIX_MKL (NMATRIX, NM, NL)
USE SCARC_POINTERS, ONLY: G, A, AS, SCARC_POINT_TO_GRID, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NMATRIX, NM, NL
INTEGER :: IC, JC, JC0, ICS, JCS, JCG
INTEGER :: ICOL, JCOL, IAS
INTEGER :: ISYM, JSYM, NSYM
REAL(EB) :: VAL = 0.0_EB, VALS = 0.0_EB, DIFF
LOGICAL  :: BSYM, BCHECK_SYMMETRY = .FALSE.
INTEGER, DIMENSION(:), ALLOCATABLE :: ICOL_AUX, IC_AUX
INTEGER, POINTER, DIMENSION(:) :: ACOLG, ASCOLG

CROUTINE = 'SCARC_SETUP_MATRIX_MKL'

CALL SCARC_POINT_TO_GRID (NM, NL)                                    

SELECT CASE (NMATRIX)
   CASE (NSCARC_MATRIX_POISSON)
      A  => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_POISSON)
      AS => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_POISSON_SYM)
   CASE (NSCARC_MATRIX_LAPLACE)
      A  => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_LAPLACE)
      AS => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_LAPLACE_SYM)
END SELECT

IF (NMESHES == 1 .OR. TYPE_SCOPE(0) == NSCARC_SCOPE_LOCAL) THEN
   ACOLG  => A%COL
ELSE
   ACOLG  => A%COLG
ENDIF
#ifdef WITH_SCARC_DEBUG2
CALL SCARC_DEBUG_CMATRIX (A, 'POISSON', 'SETUP_MATRIX_MKL: BEGIN')
WRITE(MSG%LU_DEBUG,*) 'TYPE_SCOPE(',0,')=', TYPE_SCOPE(0), NMESHES
WRITE(MSG%LU_DEBUG,*) 'TYPE_MKL(',NL,')=', TYPE_MKL(NL)
WRITE(MSG%LU_DEBUG,*) 'IS_MKL_LEVEL(',NL,') =', IS_MKL_LEVEL(NL)
WRITE(MSG%LU_DEBUG,*) 'ACOLG:', ACOLG
#endif
  
! ---------- Store only symmetric parts of matrix (diagonal and upper part)
  
IF (SCARC_MKL_MTYPE == 'SYMMETRIC') THEN

   IF (BCHECK_SYMMETRY) THEN
      ! First check whether symmetry of system matrix is guaranteed
      DO IC = 1, G%NC
         COLUMN_LOOP: DO ICOL = A%ROW(IC)+1, A%ROW(IC+1)-1
            ICS = ACOLG(ICOL)
            VAL = A%VAL(ICOL)
            IF (ICS > IC .AND. ICS <= G%NC) THEN
               BSYM = .FALSE.
               DO JCOL = A%ROW(ICS)+1, A%ROW(ICS+1)-1
                  JCS = ACOLG(JCOL)
                  IF (JCS == IC) THEN
                     VALS = A%VAL(JCOL)
                     DIFF = ABS(VAL-VALS)
                     IF (ABS(VAL - VALS) < 1E-6) THEN
                        BSYM=.TRUE.
                        CYCLE COLUMN_LOOP
                     ENDIF
                  ENDIF
               ENDDO
               IF (.NOT.BSYM) CALL SCARC_ERROR(NSCARC_ERROR_MATRIX_SYMMETRY, SCARC_NONE, NM)
            ENDIF
         ENDDO COLUMN_LOOP
      ENDDO
   ENDIF

 
   ! Compute number of entries in symmetric matrix
 
   AS%N_VAL = 0
   DO IC = 1, G%NC
      DO ICOL = A%ROW(IC), A%ROW(IC+1)-1
         IF (TYPE_MKL(NL) == NSCARC_MKL_LOCAL) THEN
            JC = ACOLG(ICOL)
            IF (JC >= IC .AND. JC <= G%NC) AS%N_VAL = AS%N_VAL+1
         ELSE IF (TYPE_MKL(NL) == NSCARC_MKL_GLOBAL) THEN
            IF (NL == NLEVEL_MIN) THEN
               JCG = G%LOCAL_TO_GLOBAL(ACOLG(ICOL))
            ELSE
               JCG = ACOLG(ICOL)
            ENDIF
            IF (JCG >= IC + G%NC_OFFSET(NM)) AS%N_VAL = AS%N_VAL+1
         ELSE
            CALL SCARC_ERROR(NSCARC_ERROR_MATRIX_SETUP, SCARC_NONE, TYPE_MKL(NL))
         ENDIF
      ENDDO
   ENDDO

ELSE
   AS%N_VAL = A%N_VAL
ENDIF

! Allocate storage for symmetric matrix and its column and row pointers
  
CALL SCARC_GET_MATRIX_STENCIL_MAX(A, G%NC)
AS%N_ROW = G%NC + 1
AS%N_VAL = A%N_STENCIL_MAX * G%NC
CALL SCARC_ALLOCATE_CMATRIX (AS, NL, TYPE_MKL_PRECISION, NSCARC_MATRIX_FULL, 'G%AS', CROUTINE)

IF (NMESHES == 1 .OR. TYPE_SCOPE(0) == NSCARC_SCOPE_LOCAL) THEN
   ASCOLG  => AS%COL
ELSE
   ASCOLG  => AS%COLG
ENDIF

! If global MKL method is used, also allocate auxiliary space for computation of global numbering

IF (IS_MKL_LEVEL(NL)) THEN
   CALL SCARC_ALLOCATE_INT1 (ICOL_AUX, 1, A%N_STENCIL_MAX, NSCARC_HUGE_INT, 'ICOL_AUX', CROUTINE)
   CALL SCARC_ALLOCATE_INT1 (IC_AUX  , 1, A%N_STENCIL_MAX, NSCARC_HUGE_INT, 'IC_AUX', CROUTINE)
ENDIF
  
! Subtract symmetric matrix part from usual system matrix
  
IAS = 1
DO IC = 1, AS%N_ROW - 1
   AS%ROW(IC) = IAS

   TYPE_MKL_SELECT: SELECT CASE (TYPE_MKL(NL)) 

      ! Blockwise use of local MKL solvers - no global numbering required

      CASE(NSCARC_MKL_LOCAL) 

         DO ICOL = A%ROW(IC), A%ROW(IC+1)-1
            JC = A%COL(ICOL)
            IF (JC >= IC .AND. JC <= G%NC) THEN
               AS%COL(IAS) = A%COL(ICOL)
               ASCOLG(IAS) = A%COL(ICOL)
               SELECT CASE (TYPE_MKL_PRECISION)
                  CASE (NSCARC_PRECISION_DOUBLE)
                     AS%VAL(IAS) = A%VAL(ICOL)
                  CASE (NSCARC_PRECISION_SINGLE)
                     AS%VAL_FB(IAS) = REAL(A%VAL(ICOL),FB)
                  END SELECT
               IAS = IAS + 1
            ENDIF
         ENDDO
         AS%ROW(IC+1) = IAS

      ! Global use of MKL solver - get global numbering of matrix elements

      CASE(NSCARC_MKL_GLOBAL) 

         ! Store indices of all diagonal and upper-diagonal entries

         ICOL_AUX = 0
         IC_AUX   = NSCARC_HUGE_INT
         ISYM = 1
         JC0 = ACOLG(A%ROW(IC))
         DO ICOL = A%ROW(IC), A%ROW(IC+1)-1
            JC = ACOLG(ICOL)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'MKL: IC, ICOL, JC:', IC, ICOL, JC
#endif
            IF (SCARC_MKL_MTYPE == 'SYMMETRIC') THEN
               IF (JC >= JC0) THEN
                  ICOL_AUX(ISYM) = ICOL
                  IC_AUX(ISYM) = JC
                  ISYM  = ISYM  + 1
               ENDIF
            ELSE
               ICOL_AUX(ISYM) = ICOL
               IC_AUX(ISYM) = JC
               ISYM  = ISYM  + 1
            ENDIF
         ENDDO
         AS%ROW(IC+1) = IAS

         NSYM = ISYM - 1
         JSYM = 1

         ! Sort them in increasing order (for the use of Cluster_Sparse_Solver and PARDISO functionality)

         SORT_LOOP: DO WHILE (JSYM <= NSYM)
            DO ISYM = 1, NSYM
               JC = IC_AUX(ISYM)
               IF (JC == NSCARC_HUGE_INT) CYCLE
               IF (JC <= MINVAL(ABS(IC_AUX(1:NSYM)))) THEN
                  ICOL = ICOL_AUX(ISYM)
                  SELECT CASE (TYPE_MKL_PRECISION)
                     CASE (NSCARC_PRECISION_DOUBLE)
                        AS%VAL(IAS) = A%VAL(ICOL)
                     CASE (NSCARC_PRECISION_SINGLE)
                        AS%VAL_FB(IAS) = REAL(A%VAL(ICOL), FB)
                  END SELECT
                  AS%COL(IAS) = ACOLG(ICOL)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'MKL: ISYM, JC, IAS, ICOL:', ISYM, JC, IAS, ICOL, ASCOLG(ICOL)
#endif
                  IC_AUX(ISYM) = NSCARC_HUGE_INT            ! mark entry as already used
                  IAS  = IAS  + 1
               ENDIF
            ENDDO
            JSYM = JSYM + 1
         ENDDO SORT_LOOP

   END SELECT TYPE_MKL_SELECT
ENDDO

AS%ROW(AS%N_ROW) = IAS

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'ASCOLG:'
WRITE(MSG%LU_DEBUG,*) (ASCOLG(IC), IC=1,9)
#endif
IF (IS_MKL_LEVEL(NL)) THEN
   CALL SCARC_DEALLOCATE_INT1 (ICOL_AUX, 'COL_AUX', CROUTINE)
   CALL SCARC_DEALLOCATE_INT1 (IC_AUX,  'IC_AUX', CROUTINE)
ENDIF

CALL SCARC_REDUCE_CMATRIX (AS, 'AS', CROUTINE)

#ifdef WITH_SCARC_DEBUG2
CALL SCARC_DEBUG_CMATRIX(AS, 'AS', 'SETUP_MATRIX_MKL: END')
#endif
END SUBROUTINE SCARC_SETUP_MATRIX_MKL
#endif


! --------------------------------------------------------------------------------------------------------------
!> \brief Insert correct boundary conditions into system matrix

! If A is a pure Neumann matrix, get neighboring cell indices of communicated stencil legs for 
! condensed system, also save values and column indices of last matrix row of last mesh

! Set correct boundary conditions for system matrix
! Take care of whether the structured or unstructured discretization is used

! If there are no Dirichlet BC's transform sytem into condensed one by replacing the
! matrix entries in last column and row by the stored ones (zeros and one at diaonal position)
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_BOUNDARY (NM, NL)
USE SCARC_POINTERS, ONLY: L, G, F, GWC, A, AB, ACO, ABCO, &
                          SCARC_POINT_TO_GRID, SCARC_POINT_TO_CMATRIX, SCARC_POINT_TO_BMATRIX
INTEGER, INTENT(IN) :: NM, NL
INTEGER :: I, J, K, IOR0, IW, IC, NOM, IP, ICO, ICOL

CALL SCARC_POINT_TO_GRID (NM, NL)                                    

SELECT CASE (SET_MATRIX_TYPE(NL))

   ! ---------- Matrix in compact storage technique
 
   CASE (NSCARC_MATRIX_COMPACT)

      A => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_POISSON)

#ifdef WITH_SCARC_DEBUG
      CALL SCARC_DEBUG_CMATRIX(A, 'POISSON', 'POISSON WITHOUT BDRY')
#endif
      ! Setup condensing if there are no Dirichlet BC's 

      IF (IS_PURE_NEUMANN) CALL SCARC_SETUP_CMATRIX_CONDENSED(NM)

      ! Set correct boundary conditions 

      DO IW = 1, G%NW

         GWC => G%WALL(IW)
         IOR0 = GWC%IOR
         IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE       

         F  => L%FACE(IOR0)

         I = GWC%IXW
         J = GWC%IYW
         K = GWC%IZW

         IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE

         NOM = GWC%NOM
         IC  = G%CELL_NUMBER(I, J, K)
         GWC%ICW = IC

         ! SPD-matrix with mixture of Dirichlet and Neumann BC's according to BTYPE

         IF (N_DIRIC_GLOBAL(NLEVEL_MIN) > 0) THEN

            IP = A%ROW(IC)
            SELECT CASE (GWC%BTYPE)
               CASE (DIRICHLET)
                  A%VAL(IP) = A%VAL(IP) - F%INCR_BOUNDARY
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,6I6,E14.6)') 'B :DIRICHLET: IW, I, J, K, NOM, IC, A%VAL:', IW, I, J, K, NOM, IC, A%VAL(IP)
#endif
               CASE (NEUMANN)
                  A%VAL(IP) = A%VAL(IP) + F%INCR_BOUNDARY
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,6I6,E14.6)') 'B :NEUMANN  : IW, I, J, K, NOM, IC, A%VAL:', IW, I, J, K, NOM, IC, A%VAL(IP)
#endif
            END SELECT

         ! Purely Neumann matrix

         ELSE IF (GWC%BTYPE == NEUMANN) THEN
            IP = A%ROW(IC)
            A%VAL(IP) = A%VAL(IP) + F%INCR_BOUNDARY
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,6I6,E14.6)') 'C :PURE_NEUMANN  : IW, I, J, K, NOM, IC, A%VAL:', IW, I, J, K, NOM, IC, A%VAL(IP)
#endif
         ENDIF

      ENDDO 

#ifdef WITH_SCARC_DEBUG
      CALL SCARC_DEBUG_CMATRIX(A, 'POISSON', 'POISSON WITH BDRY PLAIN')
#endif
      ! Transform into condensed system, if there are no Dirichlet BC's 

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'IS_PURE_NEUMANN=', IS_PURE_NEUMANN,': A%N_CONDENSED=', A%N_CONDENSED
#endif

      IF (IS_PURE_NEUMANN) THEN
         DO ICO = 1, A%N_CONDENSED
            ACO => A%CONDENSED(ICO)
            DO ICOL = 1, ACO%N_COL
               IP = ACO%PTR(ICOL)
               A%VAL(IP) = ACO%VAL2(ICOL)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,6I6,E14.6)') 'D :CONDENSED  : IW, I, J, K, NOM, IC, A%VAL:', IW, I, J, K, NOM, IC, A%VAL(IP)
#endif
            ENDDO
         ENDDO
      ENDIF 

#ifdef WITH_SCARC_DEBUG
      CALL SCARC_DEBUG_CMATRIX(A, 'POISSON', 'POISSON WITH BDRY AND POSSIBLE CONDENSING')
#endif
 
   ! ---------- Matrix in bandwise storage technique
 
   CASE (NSCARC_MATRIX_BANDWISE)

      ! Preset matrix switch if no Dirichlet BC's available

      AB => SCARC_POINT_TO_BMATRIX (NSCARC_MATRIX_POISSON)
      IF (IS_PURE_NEUMANN) CALL SCARC_SETUP_BMATRIX_CONDENSED(NM)

      ! Set right boundary conditions 

      DO IW = 1, G%NW

         GWC => G%WALL(IW)
         IOR0 = GWC%IOR
         IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE     

         F  => L%FACE(IOR0)

         I = GWC%IXW
         J = GWC%IYW
         K = GWC%IZW

         IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE

         NOM  = GWC%NOM
         GWC%ICW =G%CELL_NUMBER(I, J, K)
         IC = G%CELL_NUMBER(I, J, K)

         ! SPD-matrix with mixture of Dirichlet and Neumann BC's according to the SETTING of BTYPE

         IF (N_DIRIC_GLOBAL(NLEVEL_MIN) > 0) THEN

            SELECT CASE (GWC%BTYPE)
               CASE (DIRICHLET)
                  AB%VAL(IC, AB%POS(0)) = AB%VAL(IC, AB%POS(0)) - F%INCR_BOUNDARY
               CASE (NEUMANN)
                  AB%VAL(IC, AB%POS(0)) = AB%VAL(IC, AB%POS(0)) + F%INCR_BOUNDARY
            END SELECT

         ! Purely Neumann matrix

         ELSE
            IF (GWC%BTYPE == NEUMANN) AB%VAL(IC, AB%POS(0)) = AB%VAL(IC, AB%POS(0)) + F%INCR_BOUNDARY
         ENDIF

      ENDDO 
   
      ! Transform into condensed system, if there are no Dirichlet BC's 

      IF (IS_PURE_NEUMANN) THEN
         DO ICO = 1, AB%N_CONDENSED
            ABCO => AB%CONDENSED(ICO)
            IF (ICO == 1) THEN
               AB%VAL(ABCO%ICO, 1:AB%N_STENCIL) = ABCO%VAL2(1:AB%N_STENCIL)
            ELSE
               IP = AB%POS(ABCO%IOR0)
               AB%VAL(ABCO%ICO, IP) = ABCO%VAL2(IP)
            ENDIF
         ENDDO
      ENDIF 
 
END SELECT 

END SUBROUTINE SCARC_SETUP_BOUNDARY

! --------------------------------------------------------------------------------------------------------------
!> \brief Insert correct boundary conditions into system matrix

! If A is a pure Neumann matrix, get neighboring cell indices of communicated stencil legs for 
! condensed system, also save values and column indices of last matrix row of last mesh

! Set correct boundary conditions for system matrix
! Take care of whether the structured or unstructured discretization is used

! If there are no Dirichlet BC's transform sytem into condensed one by replacing the
! matrix entries in last column and row by the stored ones (zeros and one at diaonal position)
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_BOUNDARY_VAR (NM, NL)
USE SCARC_POINTERS, ONLY: M, L, G, F, GWC, A, ACO, & !AB, ABCO
                          SCARC_POINT_TO_GRID, SCARC_POINT_TO_CMATRIX, SCARC_POINT_TO_BMATRIX
USE SCARC_POINTERS, ONLY: RDX, RDY, RDZ, RDXN, RDYN, RDZN
REAL(EB), DIMENSION(:,:,:), POINTER :: RHOP
INTEGER, INTENT(IN) :: NM, NL
INTEGER :: I, J, K, IOR0, IW, IC, NOM, IP, ICO, ICOL
REAL(EB) :: SCAL=1.0_EB, ASAVE

CALL SCARC_POINT_TO_GRID (NM, NL)                                    
IF (PREDICTOR) THEN
   RHOP => M%RHO
ELSE
   RHOP => M%RHOS
ENDIF

SELECT CASE (SET_MATRIX_TYPE(NL))

   ! ---------- Matrix in compact storage technique
 
   CASE (NSCARC_MATRIX_COMPACT)

      A => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_POISSON)

#ifdef WITH_SCARC_DEBUG
      CALL SCARC_DEBUG_CMATRIX(A, 'POISSON', 'POISSON WITHOUT BDRY')
#endif
      ! Setup condensing if there are no Dirichlet BC's 

      IF (IS_PURE_NEUMANN) CALL SCARC_SETUP_CMATRIX_CONDENSED(NM)

      ! Set correct boundary conditions 

      DO IW = 1, G%NW

         GWC => G%WALL(IW)
         IOR0 = GWC%IOR
         IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE       

         F  => L%FACE(IOR0)
         RDX  => M%RDX
         RDXN => M%RDXN
         RDY  => M%RDY
         RDYN => M%RDYN
         RDZ  => M%RDZ
         RDZN => M%RDZN

         I = GWC%IXW
         J = GWC%IYW
         K = GWC%IZW

         IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE

         NOM = GWC%NOM
         IC  = G%CELL_NUMBER(I, J, K)
         GWC%ICW = IC

         ! SPD-matrix with mixture of Dirichlet and Neumann BC's according to BTYPE

         IF (N_DIRIC_GLOBAL(NLEVEL_MIN) > 0) THEN

            IP = A%ROW(IC)
            ! Either subtract or add contribution depending on boundary type
            SELECT CASE (GWC%BTYPE)
               CASE (DIRICHLET)
                  SCAL =  1.0_EB
               CASE (NEUMANN)
                  SCAL = -1.0_EB
            END SELECT

            IF (GWC%BTYPE == DIRICHLET .OR. GWC%BTYPE == NEUMANN) THEN
               IF (TWO_D) THEN
                  SELECT CASE (IOR0)
                     CASE (1)
                        ASAVE = A%VAL(IP)
                        A%VAL(IP) = A%VAL(IP) + SCAL*(-RDX(I)*RDXN(I-1)*2.0_EB /(RHOP(I-1,J,K) + RHOP(I,J,K)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A, I4, 4E12.4)') ' 1: IC, ASAVE, SCAL, ADD, A%VAL(IP):', &
                                     IC, ASAVE, SCAL, -RDX(I)*RDXN(I-1)*2.0_EB /(RHOP(I-1,J,K) + RHOP(I,J,K)),A%VAL(IP)
#endif
                     CASE (-1)
                        ASAVE = A%VAL(IP)
                        A%VAL(IP) = A%VAL(IP) + SCAL*(-RDX(I)*RDXN(I)*2.0_EB /(RHOP(I,J,K) + RHOP(I+1,J,K)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A, I4, 4E12.4)') '-1: IC, ASAVE, SCAL, ADD, A%VAL(IP):', &
                                     IC, ASAVE, SCAL, -RDX(I)*RDXN(I)*2.0_EB /(RHOP(I,J,K) + RHOP(I+1,J,K)), A%VAL(IP)
#endif
                     CASE (3)
                        ASAVE = A%VAL(IP)
                        A%VAL(IP) = A%VAL(IP) + SCAL*(-RDZ(K)*RDZN(K-1)*2.0_EB /(RHOP(I,J,K-1) + RHOP(I,J,K)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A, I4, 4E12.4)') ' 3: IC, ASAVE, SCAL, ADD, A%VAL(IP):', &
                                     IC, ASAVE, SCAL, -RDZ(K)*RDZN(K-1)*2.0_EB /(RHOP(I,J,K-1) + RHOP(I,J,K)), A%VAL(IP)
#endif
                     CASE (-3)
                        ASAVE = A%VAL(IP)
                        A%VAL(IP) = A%VAL(IP) + SCAL*(-RDZ(K)*RDZN(K)*2.0_EB /(RHOP(I,J,K) + RHOP(I,J,K+1)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A, I4, 4E12.4)') '-3: IC, ASAVE, SCAL, ADD, A%VAL(IP):', &
                                     IC, ASAVE, SCAL, -RDZ(K)*RDZN(K)*2.0_EB /(RHOP(I,J,K) + RHOP(I,J,K+1)), A%VAL(IP)
#endif
                  END SELECT
               ELSE
                  SELECT CASE (IOR0)
                     CASE (1)
                        A%VAL(IP) = A%VAL(IP) + SCAL*(-RDX(I)*RDXN(I-1)*2.0_EB /(RHOP(I-1,J,K) + RHOP(I,J,K)))
                     CASE (-1)
                        A%VAL(IP) = A%VAL(IP) + SCAL*(-RDX(I)*RDXN(I)*2.0_EB /(RHOP(I,J,K) + RHOP(I+1,J,K)))
                     CASE (2)
                        A%VAL(IP) = A%VAL(IP) + SCAL*(-RDY(J)*RDYN(J-1)*2.0_EB /(RHOP(I,J-1,K) + RHOP(I,J,K)))
                     CASE (-2)
                        A%VAL(IP) = A%VAL(IP) + SCAL*(-RDY(J)*RDYN(J)*2.0_EB /(RHOP(I,J,K) + RHOP(I,J+1,K)))
                     CASE (3)
                        A%VAL(IP) = A%VAL(IP) + SCAL*(-RDZ(K)*RDZN(K-1)*2.0_EB /(RHOP(I,J,K-1) + RHOP(I,J,K)))
                     CASE (-3)
                        A%VAL(IP) = A%VAL(IP) + SCAL*(-RDZ(K)*RDZN(K)*2.0_EB /(RHOP(I,J,K) + RHOP(I,J,K+1)))
                  END SELECT
               ENDIF
            ENDIF

         ! Purely Neumann matrix

         ELSE IF (GWC%BTYPE == NEUMANN) THEN
            IP = A%ROW(IC)
            SCAL = -1.0_EB
            IF (TWO_D) THEN
               SELECT CASE (IOR0)
                  CASE (1)
                     ASAVE = A%VAL(IP)
                     A%VAL(IP) = A%VAL(IP) + SCAL*(-RDX(I)*RDXN(I-1)*2.0_EB /(RHOP(I-1,J,K) + RHOP(I,J,K)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A, I4, 4E12.4)') 'PURE:N 1: IC, ASAVE, SCAL, ADD, A%VAL(IP):', &
                                  IC, ASAVE, SCAL, -RDX(I)*RDXN(I-1)*2.0_EB /(RHOP(I-1,J,K) + RHOP(I,J,K)),A%VAL(IP)
#endif
                  CASE (-1)
                     ASAVE = A%VAL(IP)
                     A%VAL(IP) = A%VAL(IP) + SCAL*(-RDX(I)*RDXN(I)*2.0_EB /(RHOP(I,J,K) + RHOP(I+1,J,K)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A, I4, 4E12.4)') 'PURE:N-1: IC, ASAVE, SCAL, ADD, A%VAL(IP):', &
                                  IC, ASAVE, SCAL, -RDX(I)*RDXN(I)*2.0_EB /(RHOP(I,J,K) + RHOP(I+1,J,K)), A%VAL(IP)
#endif
                  CASE (3)
                     ASAVE = A%VAL(IP)
                     A%VAL(IP) = A%VAL(IP) + SCAL*(-RDZ(K)*RDZN(K-1)*2.0_EB /(RHOP(I,J,K-1) + RHOP(I,J,K)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A, I4, 4E12.4)') 'PURE:N 3: IC, ASAVE, SCAL, ADD, A%VAL(IP):', &
                                  IC, ASAVE, SCAL, -RDZ(K)*RDZN(K-1)*2.0_EB /(RHOP(I,J,K-1) + RHOP(I,J,K)), A%VAL(IP)
#endif
                  CASE (-3)
                     ASAVE = A%VAL(IP)
                     A%VAL(IP) = A%VAL(IP) + SCAL*(-RDZ(K)*RDZN(K)*2.0_EB /(RHOP(I,J,K) + RHOP(I,J,K+1)))
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A, I4, 4E12.4)') 'PURE:N-3: IC, ASAVE, SCAL, ADD, A%VAL(IP):', &
                                  IC, ASAVE, SCAL, -RDZ(K)*RDZN(K)*2.0_EB /(RHOP(I,J,K) + RHOP(I,J,K+1)), A%VAL(IP)
#endif
               END SELECT
            ELSE
               SELECT CASE (IOR0)
                  CASE (1)
                     A%VAL(IP) = A%VAL(IP) + SCAL*(-RDX(I)*RDXN(I-1)*2.0_EB /(RHOP(I-1,J,K) + RHOP(I,J,K)))
                  CASE (-1)
                     A%VAL(IP) = A%VAL(IP) + SCAL*(-RDX(I)*RDXN(I)*2.0_EB /(RHOP(I,J,K) + RHOP(I+1,J,K)))
                  CASE (2)
                     A%VAL(IP) = A%VAL(IP) + SCAL*(-RDY(J)*RDYN(J-1)*2.0_EB /(RHOP(I,J-1,K) + RHOP(I,J,K)))
                  CASE (-2)
                     A%VAL(IP) = A%VAL(IP) + SCAL*(-RDY(J)*RDYN(J)*2.0_EB /(RHOP(I,J,K) + RHOP(I,J+1,K)))
                  CASE (3)
                     A%VAL(IP) = A%VAL(IP) + SCAL*(-RDZ(K)*RDZN(K-1)*2.0_EB /(RHOP(I,J,K-1) + RHOP(I,J,K)))
                  CASE (-3)
                     A%VAL(IP) = A%VAL(IP) + SCAL*(-RDZ(K)*RDZN(K)*2.0_EB /(RHOP(I,J,K) + RHOP(I,J,K+1)))
               END SELECT
            ENDIF
         ENDIF

      ENDDO 

#ifdef WITH_SCARC_DEBUG 
WRITE(MSG%LU_DEBUG,*) 'IS_PURE_NEUMANN=', IS_PURE_NEUMANN,': A%N_CONDENSED=', A%N_CONDENSED
!WRITE(MSG%LU_DEBUG,*) 'RHO(1,1,1) =', MESHES(1)%RHO(1,1,1)
!WRITE(MSG%LU_DEBUG,*) 'RHOS(1,1,1) =', MESHES(1)%RHOS(1,1,1)
!WRITE(MSG%LU_DEBUG,*) '1/RHO(1,1,1) =', 1.0_EB/MESHES(1)%RHO(1,1,1)
!WRITE(MSG%LU_DEBUG,*) '1/RHOS(1,1,1) =', 1.0_EB/MESHES(1)%RHOS(1,1,1)
!WRITE(MSG%LU_DEBUG,*) '200.0_EB/RHO(1,1,1) =', 200.0_EB/MESHES(1)%RHO(1,1,1)
!WRITE(MSG%LU_DEBUG,*) '300.0_EB/RHO(1,1,1) =', 300.0_EB/MESHES(1)%RHO(1,1,1)
!WRITE(MSG%LU_DEBUG,*) '400.0_EB/RHO(1,1,1) =', 400.0_EB/MESHES(1)%RHO(1,1,1)
!WRITE(MSG%LU_DEBUG,*) '200.0_EB/RHOS(1,1,1) =', 200.0_EB/MESHES(1)%RHOS(1,1,1)
!WRITE(MSG%LU_DEBUG,*) '300.0_EB/RHOS(1,1,1) =', 300.0_EB/MESHES(1)%RHOS(1,1,1)
!WRITE(MSG%LU_DEBUG,*) '400.0_EB/RHOS(1,1,1) =', 400.0_EB/MESHES(1)%RHOS(1,1,1)
      CALL SCARC_DEBUG_CMATRIX(A, 'POISSON', 'POISSON WITH BDRY PLAIN')
#endif
      ! Transform into condensed system, if there are no Dirichlet BC's 

      IF (IS_PURE_NEUMANN) THEN
         DO ICO = 1, A%N_CONDENSED
            ACO => A%CONDENSED(ICO)
            DO ICOL = 1, ACO%N_COL
               IP = ACO%PTR(ICOL)
               A%VAL(IP) = ACO%VAL2(ICOL)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,6I6,E14.6)') 'D :CONDENSED  : IW, I, J, K, NOM, IC, A%VAL:', IW, I, J, K, NOM, IC, A%VAL(IP)
#endif
            ENDDO
         ENDDO
      ENDIF 

#ifdef WITH_SCARC_DEBUG
      CALL SCARC_DEBUG_CMATRIX(A, 'POISSON', 'POISSON WITH BDRY AND POSSIBLE CONDENSING')
      !CALL SCARC_DEBUG_CMATRIX_SCALED(A, MESHES(1)%RHO(1,1,1),'POISSON', 'SCALED POISSON WITH BDRY AND POSSIBLE CONDENSING')
#endif
 
   ! ---------- Matrix in bandwise storage technique
 
   CASE (NSCARC_MATRIX_BANDWISE)

      WRITE(*,*) 'Variable boundary conditions vor BMATRIX not yet implemented'

END SELECT 

END SUBROUTINE SCARC_SETUP_BOUNDARY_VAR


#ifdef WITH_MKL
! --------------------------------------------------------------------------------------------------------------
!> \brief Insert internal Dirichlet boundary conditions to local MKL preconditioning matrices
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_BOUNDARY_MKL (NMATRIX, NM, NL)
USE SCARC_POINTERS, ONLY: L, G, F, GWC, AS, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NMATRIX, NM, NL
INTEGER :: I, J, K, IOR0, IW, IC, NOM, IP

CALL SCARC_POINT_TO_GRID (NM, NL)                                    

IF (NMATRIX == NSCARC_MATRIX_POISSON) THEN
   AS => G%POISSON_SYM
ELSE
   AS => G%LAPLACE_SYM
ENDIF

SELECT CASE (TYPE_MKL_PRECISION) 

   CASE (NSCARC_PRECISION_DOUBLE)

      DO IW = 1, G%NW
      
         GWC => G%WALL(IW)
         IOR0 = GWC%IOR
         IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE       
         IF (GWC%BTYPE /= INTERNAL) CYCLE
      
         F  => L%FACE(IOR0)
      
         I = GWC%IXW
         J = GWC%IYW
         K = GWC%IZW
      
         IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE
      
         NOM = GWC%NOM
         IC  = G%CELL_NUMBER(I, J, K)
      
         IP = AS%ROW(IC)
         AS%VAL(IP) = AS%VAL(IP) - F%INCR_BOUNDARY
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,6I6,E14.6)') 'B :DIRICHLET: IW, I, J, K, NOM, IC, AS%VAL:', IW, I, J, K, NOM, IC, AS%VAL(IP)
#endif
      
      ENDDO 
      
   CASE (NSCARC_PRECISION_SINGLE)

      DO IW = 1, G%NW
      
         GWC => G%WALL(IW)
         IOR0 = GWC%IOR
         IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE       
         IF (GWC%BTYPE /= INTERNAL) CYCLE
      
         F  => L%FACE(IOR0)
      
         I = GWC%IXW
         J = GWC%IYW
         K = GWC%IZW
      
         IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE
      
         NOM = GWC%NOM
         IC  = G%CELL_NUMBER(I, J, K)
      
         IP = AS%ROW(IC)
         AS%VAL_FB(IP) = AS%VAL_FB(IP) - REAL(F%INCR_BOUNDARY, FB)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,6I6,E14.6)') 'B :DIRICHLET: IW, I, J, K, NOM, IC, AS%VAL_FB:', IW, I, J, K, NOM, IC, AS%VAL_FB(IP)
#endif
      
      ENDDO 
END SELECT

#ifdef WITH_SCARC_DEBUG
   CALL SCARC_DEBUG_CMATRIX(AS, 'POISSON_SYM', 'POISSON_MKL WITH DIRICHLET BDRY')
#endif
 
END SUBROUTINE SCARC_SETUP_BOUNDARY_MKL
#endif


! --------------------------------------------------------------------------------------------------------------
!> \brief Setup condensed system for compact matrix storage technique
! Define switch entries for toggle between original and condensed values
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_CMATRIX_CONDENSED (NM)
USE SCARC_POINTERS, ONLY: L, G, A, ACO, GWC, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NM
INTEGER :: ICO, NC, NOM, IP, IC, JC, ICE, ICN, ICOL, IOR0, IW, I, J, K

A => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_POISSON)
ICO = 0
LAST_CELL_IN_LAST_MESH_IF: IF (NM == NMESHES) THEN

   NC = G%NC_LOCAL(NMESHES)
   IP = A%ROW(NC)

   ! Store column indices and values of diagonal and all off-diagonal entries in last row
   ! index '1' corresponds to main diagonal entry

   ICO = ICO + 1
   ACO => A%CONDENSED(ICO)

   ICOL = 1
   ACO%PTR(ICOL)  = IP
   ACO%COL(ICOL)  = A%COL(IP)
   ACO%VAL1(ICOL) = A%VAL(IP)
   ACO%VAL2(ICOL) = 1.0_EB
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'CONDENSING A:         IP, ICOL, ACO%COL, ACO%VAL1:', IP, ICOL, ACO%COL(ICOL), ACO%VAL1(ICOL)
#endif

   DO IP = A%ROW(NC)+1, A%ROW(NC+1)-1
      ICOL = ICOL + 1
      ACO%PTR(ICOL)  = IP
      ACO%COL(ICOL)  = A%COL(IP)
      ACO%VAL1(ICOL) = A%VAL(IP)
      ACO%VAL2(ICOL) = 0.0_EB
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'CONDENSING A:         IP, ICOL, ACO%COL, ACO%VAL1:', IP, ICOL, ACO%COL(ICOL), ACO%VAL1(ICOL)
#endif
   ENDDO
   ACO%N_COL = ICOL                                ! number of stored columns

   ! Within last mesh: check which other cells have a connection to the last cell;
   ! in each corresponding matrix row store the column index and value of just that matrix entry
   ! for each direction only one value has to be stored
 
   JC = NC - 1
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '===========> ACO1: JC, ROW1, ROW2:', JC, A%ROW(JC)+1, A%ROW(JC+1)-1
#endif
   DO IP = A%ROW(JC)+1, A%ROW(JC+1)-1
      IF (A%COL(IP) == NC) THEN
         ICO = ICO + 1
         ACO => A%CONDENSED(ICO)
         ACO%PTR(1)  = IP
         ACO%COL(1)  = JC
         ACO%VAL1(1) = A%VAL(IP)                     ! store original value of system matrix
         ACO%VAL2(1) = 0.0_EB                        ! store new value of condensed system matrix
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'CONDENSING B:         IP, A%COL(IP), ICO, ACO%VAL1:', IP, A%COL(IP), ICO, ACO%VAL1(1)
#endif
         ACO%N_COL   = 1
         EXIT
      ENDIF
   ENDDO

   JC = NC - L%NX
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '===========> ACO2: JC, ROW1, ROW2:', JC, A%ROW(JC)+1, A%ROW(JC+1)-1
#endif
   DO IP = A%ROW(JC)+1, A%ROW(JC+1)-1
      IF (A%COL(IP) == NC) THEN
         ICO = ICO + 1
         ACO => A%CONDENSED(ICO)
         ACO%PTR(1)  = IP
         ACO%COL(1)  = JC
         ACO%VAL1(1) = A%VAL(IP)                     ! store original value of system matrix
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'CONDENSING B:         IP, A%COL(IP), ICO, ACO%VAL1', IP, A%COL(IP), ICO, ACO%VAL1(1)
#endif
         ACO%VAL2(1) = 0.0_EB                        ! store new value of condensed system matrix
         ACO%N_COL   = 1
         EXIT
      ENDIF
   ENDDO

   IF (.NOT.TWO_D) THEN
      JC = NC - L%NX * L%NY
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '===========> ACO3: JC, ROW1, ROW2:', JC, A%ROW(JC)+1, A%ROW(JC+1)-1
#endif
      DO IP = A%ROW(JC)+1, A%ROW(JC+1)-1
         IF (A%COL(IP) == NC) THEN
            ICO = ICO + 1
            ACO => A%CONDENSED(ICO)
            ACO%PTR(1)  = IP
            ACO%COL(1)  = JC
            ACO%VAL1(1) = A%VAL(IP)                  ! store original value of system matrix
            ACO%VAL2(1) = 0.0_EB                     ! store new value of condensed system matrix
            ACO%N_COL   = 1
            EXIT
         ENDIF
      ENDDO
   ENDIF

ENDIF LAST_CELL_IN_LAST_MESH_IF

! Cycle boundary cells to check if there is a periodic communication partner whose stencil is coupled
! with the last cell of last mesh;
! this can be a cell on the opposite side of the own mesh or on a different mesh
! if such a cell exists, store corresponding matrix entry
 
DO IW = 1, G%NW

   GWC => G%WALL(IW)

   IOR0 = GWC%IOR
   IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE

   I = GWC%IXW
   J = GWC%IYW
   K = GWC%IZW

   IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE

   NOM = GWC%NOM
   IC  = G%CELL_NUMBER(I, J, K)
   GWC%ICW = IC

   IF (NOM == NMESHES) THEN

      ICE = GWC%ICE                               ! adjacent ghost cell number
      ICN = G%ICE_TO_ICN(ICE)                     ! get column index of neighboring offdiagonal matrix entry
      IF (ICN /= SCARC(NMESHES)%NC) CYCLE         ! if no relation to last cell in last mesh, cycle

      DO IP = A%ROW(IC)+1, A%ROW(IC+1)-1
         IF (A%COL(IP) == ICE) THEN
            ICO = ICO + 1
            ACO => A%CONDENSED(ICO)
            ACO%PTR(1)  = IP
            ACO%COL(1)  = ICN
            ACO%VAL1(1) = A%VAL(IP)
            ACO%VAL2(1) = 0.0_EB
            ACO%N_COL   = 1
            EXIT
         ENDIF
      ENDDO

   ENDIF 
ENDDO 

A%N_CONDENSED = ICO

END SUBROUTINE SCARC_SETUP_CMATRIX_CONDENSED


! --------------------------------------------------------------------------------------------------------------
!> \brief Setup condensed system for bandwise matrix storage technique
! Define switch entries for toggle between original and condensed values
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_BMATRIX_CONDENSED (NM)
USE SCARC_POINTERS, ONLY: L, G, AB, ABCO, GWC, SCARC_POINT_TO_BMATRIX
INTEGER, INTENT(IN) :: NM
INTEGER :: ICO = 0, NC, NOM, IOR0, IC, JC, ICE, ICN, IW, I, J, K

AB => SCARC_POINT_TO_BMATRIX (NSCARC_MATRIX_POISSON)
LAST_CELL_IN_LAST_MESH_BANDWISE_IF: IF (NM == NMESHES) THEN

   NC = G%NC_LOCAL(NMESHES)

   ! Store column indices and values of diagonal and all off-diagonal entries in last row
   ! index '1' corresponds to main diagonal entry

   ICO = ICO + 1
   ABCO => AB%CONDENSED(ICO)

   ABCO%IOR0 = 0
   ABCO%ICO  = NC
   ABCO%VAL1(1:AB%N_STENCIL) = AB%VAL(NC, 1:AB%N_STENCIL)
   ABCO%VAL2(1:AB%N_STENCIL) = 0.0_EB
   ABCO%VAL2(AB%POS(0)) = 1.0_EB

   ! Within last mesh: check which other cells have a connection to the last cell;
   ! in each corresponding matrix row store the column index and value of just that matrix entry
   ! for each direction only one value has to be stored
 
   JC = NC - 1
   DO IOR0 = -3, 3
      IF (JC + AB%OFFSET(IOR0) == NC) THEN
         ICO = ICO + 1
         ABCO => AB%CONDENSED(ICO)
         ABCO%IOR0 = IOR0
         ABCO%ICO  = JC
         ABCO%VAL1(1:AB%N_STENCIL) = AB%VAL(JC, 1:AB%N_STENCIL)
         ABCO%VAL2(1:AB%N_STENCIL) = AB%VAL(JC, 1:AB%N_STENCIL)
         ABCO%VAL2(AB%POS(ABCO%IOR0)) = 0.0_EB
         EXIT
      ENDIF
   ENDDO

   JC = NC - L%NX
   DO IOR0 = -3, 3
      IF (JC + AB%OFFSET(IOR0) == NC) THEN
         ICO = ICO + 1
         ABCO => AB%CONDENSED(ICO)
         ABCO%IOR0 = IOR0
         ABCO%ICO  = JC
         ABCO%VAL1(1:AB%N_STENCIL) = AB%VAL(JC, 1:AB%N_STENCIL)
         ABCO%VAL2(1:AB%N_STENCIL) = AB%VAL(JC, 1:AB%N_STENCIL)
         ABCO%VAL2(AB%POS(ABCO%IOR0)) = 0.0_EB
         EXIT
      ENDIF
   ENDDO

   IF (.NOT.TWO_D) THEN
      JC = NC - L%NX * L%NY
      DO IOR0 = -3, 3
         IF (JC + AB%OFFSET(IOR0) == NC) THEN
            ICO = ICO + 1
            ABCO => AB%CONDENSED(ICO)
            ABCO%IOR0 = IOR0
            ABCO%ICO  = JC
            ABCO%VAL1(1:AB%N_STENCIL) = AB%VAL(JC, 1:AB%N_STENCIL)
            ABCO%VAL2(1:AB%N_STENCIL) = AB%VAL(JC, 1:AB%N_STENCIL)
            ABCO%VAL2(AB%POS(ABCO%IOR0)) = 0.0_EB
            EXIT
         ENDIF
      ENDDO
   ENDIF

ENDIF LAST_CELL_IN_LAST_MESH_BANDWISE_IF
 
! Cycle boundary cells to check if there is a periodic communication partner whose stencil is coupled
! with the last cell of last mesh;
! this can be a cell on the opposite side of the own mesh or a cell on a different mesh
! if such a cell exists, store corresponding matrix entry
 
DO IW = 1, G%NW

   GWC => G%WALL(IW)

   IOR0 = GWC%IOR
   IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE

   I    = GWC%IXW
   J    = GWC%IYW
   K    = GWC%IZW

   IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE

   NOM = GWC%NOM
   IC  = G%CELL_NUMBER(I, J, K)
   GWC%ICW = IC

   IF (NOM == NMESHES) THEN
      ICE = GWC%ICE                               ! adjacent ghost cell number
      ICN = G%ICE_TO_ICN(ICE)                     ! get column index of neighboring offdiagonal matrix entry
      IF (ICN /= SCARC(NMESHES)%NC) CYCLE         ! if no relation to last cell in last mesh, cycle
      ICO = ICO + 1
      ABCO => AB%CONDENSED(ICO)
      ABCO%IOR0 = IOR0
      ABCO%ICO  = IC
      ABCO%VAL1(1:AB%N_STENCIL) = AB%VAL(IC, 1:AB%N_STENCIL)
      ABCO%VAL2(1:AB%N_STENCIL) = AB%VAL(IC, 1:AB%N_STENCIL)
      ABCO%VAL2(AB%POS(ABCO%IOR0)) = 0.0_EB
      EXIT
   ENDIF 
ENDDO 

AB%N_CONDENSED = ICO

END SUBROUTINE SCARC_SETUP_BMATRIX_CONDENSED


! --------------------------------------------------------------------------------------------------------------
!> \brief Setup condensed system in case of periodic or pure Neumann boundary conditions
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_SYSTEM_CONDENSED (NV, NL, ITYPE)
USE SCARC_POINTERS, ONLY: L, G, OG, F, OL, VC, A, ACO, AB, ABCO, &
                          SCARC_POINT_TO_GRID, SCARC_POINT_TO_OTHER_GRID, &
                          SCARC_POINT_TO_CMATRIX, SCARC_POINT_TO_BMATRIX, SCARC_POINT_TO_VECTOR
INTEGER, INTENT(IN) :: NV, NL, ITYPE
INTEGER :: NM, NOM, IFACE, ICN, ICE, ICW, JC, NC, ICO, IOR0, IP, ICG, INBR
REAL(EB) :: VC_SAVE

IF (N_DIRIC_GLOBAL(NLEVEL_MIN) > 0 .OR. &
    TYPE_PRECON == NSCARC_RELAX_FFT .OR. TYPE_PRECON == NSCARC_RELAX_FFTO) RETURN

 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SETUP_SYSTEM_CONDENSED'
#endif
! In last mesh:  subtract B*RHS(end) for internal legs of stencil
 
MESH_REAL = 0.0_EB
IF (UPPER_MESH_INDEX == NMESHES) THEN

   CALL SCARC_POINT_TO_GRID (NMESHES, NL)

   NC =  G%NC_LOCAL(NMESHES)
   VC => SCARC_POINT_TO_VECTOR(NMESHES, NL, NV)

   ! Process last column entries of all rows except of last one
   ! for those rows only one matrix entry was stored, namely that one which connects to the last cell
 
   SELECT CASE (SET_MATRIX_TYPE(NL))

      CASE (NSCARC_MATRIX_COMPACT)
         A => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_POISSON)
         DO ICO = 2, A%N_CONDENSED
            ACO => A%CONDENSED(ICO)
            JC = ACO%COL(1)
            IF (JC < NC) THEN
               VC_SAVE = VC(JC)
               VC(JC) = VC(JC) - ACO%VAL1(1)*VC(NC)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A, 2I5, 4E18.10)') 'CONDENSING: ICO, JC, VC_SAVE, VC, ACO%VAL1: ', &
                                   ICO, JC, ACO%VAL1(1), VC_SAVE, VC(NC)*ACO%VAL1(1), VC(JC)
#endif
            ENDIF
         ENDDO

      CASE (NSCARC_MATRIX_BANDWISE)
         AB => SCARC_POINT_TO_BMATRIX (NSCARC_MATRIX_POISSON)
         DO ICO = 2, AB%N_CONDENSED
            ABCO => AB%CONDENSED(ICO)
            IP = AB%POS(ABCO%IOR0)
            JC = ABCO%ICO
            IF (JC < NC) VC(JC) = VC(JC) - ABCO%VAL1(IP)*VC(NC)
        ENDDO

   END SELECT

   MESH_REAL(NMESHES) = VC(NC)     ! store last entry of RHS
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'CONDENSING:  STORE LAST ENTRY OF RHS ', VC(NC)
#endif
   VC(NC) = 0.0_EB                 ! set last entry of last mesh to zero

ENDIF
IF (ITYPE == 0) RETURN

! Broadcast last RHS-value of last cell in last mesh to all meshes
 
IF (N_MPI_PROCESSES > 1) &
   CALL MPI_ALLGATHER(MPI_IN_PLACE, 1, MPI_DOUBLE_PRECISION, MESH_REAL, 1, MPI_DOUBLE_PRECISION,&
                      MPI_COMM_WORLD, IERROR)

DO NM = 1, NMESHES
   SCARC(NM)%RHS_END = MESH_REAL(NMESHES)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SCARC(',NM,')%RHS_END :', SCARC(NM)%RHS_END
#endif
ENDDO

! Only in case of periodic BC's:
! Subtract B*RHS(end) for corresponding entries of all periodic communication partners
 
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                    

   SNODE = PROCESS(NM)
   RNODE = PROCESS(NMESHES)

   IF (.NOT. ARE_FACE_NEIGHBORS(NM, NMESHES)) CYCLE

   CALL SCARC_POINT_TO_OTHER_GRID (NM, NMESHES, NL)
   VC => SCARC_POINT_TO_VECTOR (NM, NL, NV)

 
   ! Subtract B*RHS(end) at corresponding positions
 
   DO IFACE = 1, 6                                         ! check if this face has connection to last cell

      IOR0 = FACE_ORIENTATION(IFACE)
      F => L%FACE(IOR0)

      DO INBR = 1, F%N_NEIGHBORS

         NOM = F%NEIGHBORS(INBR)
         IF (NOM /= NMESHES) CYCLE                         ! only check for common matrix entries with last mesh
         CALL SCARC_POINT_TO_OTHER_GRID (NM, NOM, NL)

         DO ICG = OL%GHOST_FIRSTW(IOR0), OL%GHOST_LASTW(IOR0)

            ICW = OG%ICG_TO_ICW(ICG, 1)
            ICE = OG%ICG_TO_ICE(ICG, 1)
            ICN = G%ICE_TO_ICN(ICE)                        ! get column index of neighboring offdiagonal matrix entry

            IF (ICN /= SCARC(NMESHES)%NC) CYCLE            ! if no relation to last cell in last mesh, cycle

            VC(ICW) = VC(ICW) - F%INCR_FACE * SCARC(NM)%RHS_END          ! TODO: check size for non-equidistant grid
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) ' HALLO I AM HERE TOO ', IFACE, ICG, ICW
#endif

         ENDDO

      ENDDO
   ENDDO
ENDDO

END SUBROUTINE SCARC_SETUP_SYSTEM_CONDENSED


! ----------------------------------------------------------------------------------------------------------------------
!> \brief Extract overlapping matrix parts after data exchange with neighbors and add them to main matrix
! ----------------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_EXTRACT_MATRIX_OVERLAPS (NMATRIX, NTYPE, NL)
USE SCARC_POINTERS, ONLY: G, F, OL, OG, A, OA, &
                          SCARC_POINT_TO_GRID, SCARC_POINT_TO_OTHER_GRID, &
                          SCARC_POINT_TO_CMATRIX, SCARC_POINT_TO_OTHER_CMATRIX
INTEGER, INTENT(IN) :: NL, NMATRIX, NTYPE
INTEGER :: NM, IFACE, NOM, IOR0, ICG, ICE, IP, ICOL, INBR, ICN, ICE1, ICE2

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                 
   A => SCARC_POINT_TO_CMATRIX (NMATRIX)

   IP = A%ROW(G%NC+1)
   FACES_LOOP: DO IFACE = 1, 6               

      IOR0 = FACE_ORIENTATION(IFACE)
      F => SCARC(NM)%LEVEL(NLEVEL_MIN)%FACE(IOR0)
   
      DO INBR = 1, F%N_NEIGHBORS

         NOM = F%NEIGHBORS(INBR)
         CALL SCARC_POINT_TO_OTHER_GRID (NM, NOM, NL)
         OA => SCARC_POINT_TO_OTHER_CMATRIX (NMATRIX)

         ICOL = 1
         DO ICG = OL%GHOST_FIRSTE(IOR0), OL%GHOST_LASTE(IOR0)
  
            ICE = OG%ICG_TO_ICE(ICG, 1)
            A%ROW(ICE) = IP 

            IF (NTYPE == 1) THEN
               ICOL = OA%ROW(ICG)
               ICN = ABS(OA%COLG(ICOL))
               A%COL(IP)  = ICE
               A%COLG(IP) = ICN
               A%VAL(IP) = OA%VAL(ICOL)
               IP = IP + 1
               DO ICOL = OA%ROW(ICG)+1, OA%ROW(ICG+1)-1
                  ICN = OA%COLG(ICOL)
                  IF (SCARC_CELL_WITHIN_MESH(G, NM, ICN)) THEN
                     A%COL(IP) = ABS(OA%COLG(ICOL)) - G%NC_OFFSET(NM)     
                  ELSE
                     A%COL(IP) = -ABS(OA%COLG(ICOL))
                     IF (ICG == OL%GHOST_FIRSTE(IOR0)) THEN
                        ICE2 = OG%ICG_TO_ICE(ICG+1, 1)
                        IF (G%LOCAL_TO_GLOBAL(ICE2) == ICN) A%COL(IP) = ICE2
                     ELSE IF (ICG == OL%GHOST_LASTW(IOR0)) THEN
                        ICE1 = OG%ICG_TO_ICE(ICG-1, 1)
                        IF (G%LOCAL_TO_GLOBAL(ICE1) == ICN) A%COL(IP) = ICE1
                     ELSE
                        ICE1 = OG%ICG_TO_ICE(ICG-1, 1)
                        ICE2 = OG%ICG_TO_ICE(ICG+1, 1)
                        IF (G%LOCAL_TO_GLOBAL(ICE1) == ICN) A%COL(IP) = ICE1
                        IF (G%LOCAL_TO_GLOBAL(ICE2) == ICN) A%COL(IP) = ICE2
                     ENDIF
                  ENDIF
                  A%COLG(IP) = ABS(OA%COLG(ICOL))      
                  A%VAL(IP)  = OA%VAL(ICOL)
                  IP = IP + 1
               ENDDO
            ELSE
               DO ICOL = OA%ROW(ICG), OA%ROW(ICG+1)-1
                  A%COL(IP) = -OA%COL(ICOL)   
                  A%COLG(IP) = ABS(OA%COLG(ICOL))      
                  A%VAL(IP) = OA%VAL(ICOL)
                  IP = IP + 1
               ENDDO
            ENDIF
         ENDDO

         A%ROW(ICE+1) = IP 
         A%N_ROW = ICE + 1
         A%N_VAL = IP - 1

      ENDDO
   ENDDO FACES_LOOP

#ifdef WITH_SCARC_DEBUG2
CALL SCARC_DEBUG_CMATRIX (A, 'A', 'AFTER EXTRACT_MATRIX_OVERLAPS')
#endif

ENDDO MESHES_LOOP

END SUBROUTINE SCARC_EXTRACT_MATRIX_OVERLAPS


! --------------------------------------------------------------------------------------------------------------------
!> \brief Extract diagonal of Poisson matrix and store it in a separate vector for further use
! --------------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_EXTRACT_MATRIX_DIAGONAL(NL)
USE SCARC_POINTERS, ONLY: G, A, SCARC_POINT_TO_GRID, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NL
INTEGER :: NM, IC, JC, ICOL

CROUTINE = 'SCARC_EXTRACT_MATRIX_DIAGONAL'

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                    
   A => SCARC_POINT_TO_CMATRIX (NSCARC_MATRIX_POISSON)

   CALL SCARC_ALLOCATE_REAL1 (G%DIAG, 1, G%NCE2, NSCARC_INIT_ZERO, 'G%DIAG', CROUTINE)
   DO IC = 1, G%NC
      DO ICOL = A%ROW(IC), A%ROW(IC+1) - 1
         JC = A%COL(ICOL)
         IF (JC == IC) G%DIAG(IC) = A%VAL(ICOL)
      ENDDO
   ENDDO

ENDDO MESHES_LOOP

! If there are multiple meshes exchange diagonal matrix on overlapping parts

IF (NMESHES > 1) CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MATRIX_DIAGS, NSCARC_NONE, NL)

END SUBROUTINE SCARC_EXTRACT_MATRIX_DIAGONAL

END MODULE SCARC_MATRICES


