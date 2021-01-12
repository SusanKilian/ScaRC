!//////////////////////////////////////////////////////////////////////////////////////////////////////
!
! MODULE SCARC_DISCRETIZATION
!
!> \brief Setup all structures related to the different grid types (structured/unstructured) and
!   the different grid resolution levels
!
!   This includes information w.r.t the mesh faces and the wall cells along external, interface and
!   internal boundaries
!
!//////////////////////////////////////////////////////////////////////////////////////////////////////
MODULE SCARC_DISCRETIZATION
  
USE GLOBAL_CONSTANTS
USE PRECISION_PARAMETERS, ONLY: EB, FB
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
USE MESH_VARIABLES, ONLY: MESHES, MESH_TYPE
USE MPI
USE SCARC_CONSTANTS
USE SCARC_TYPES, ONLY: SCARC_LEVEL_TYPE, SCARC_GRID_TYPE
USE SCARC_VARIABLES
USE SCARC_MESSAGES, ONLY: MSG
#ifdef WITH_SCARC_DEBUG
USE SCARC_MESSAGES, ONLY: SCARC_DEBUG_QUANTITY
#endif
USE SCARC_STORAGE, ONLY: SCARC_ALLOCATE_INT1, SCARC_DEALLOCATE_INT1, &
                         SCARC_ALLOCATE_INT2, SCARC_ALLOCATE_INT3, &
                         SCARC_ALLOCATE_REAL1, SCARC_ALLOCATE_LOG3
USE SCARC_UTILITIES, ONLY: ARE_NEIGHBORS, SCARC_SET_GRID_TYPE
USE SCARC_TIMINGS, ONLY: CPU
USE SCARC_ERRORS, ONLY: SCARC_SHUTDOWN
USE SCARC_MPI, ONLY: SCARC_SETUP_EXCHANGE_DIMENSIONS

IMPLICIT NONE

CONTAINS

! ------------------------------------------------------------------------------------------------
!> \brief Determine number of grid levels 
! NLEVEL_MIN corresponds to finest grid resolution, NLEVEL_MAX to coarsest resolution
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_LEVELS
#ifdef WITH_SCARC_MKL
INTEGER :: NL
#endif

SELECT_SCARC_METHOD: SELECT CASE (TYPE_METHOD)

 
   ! ---------- Global data-parallel Krylov method 
 
   CASE (NSCARC_METHOD_KRYLOV)

      SELECT_KRYLOV_PRECON: SELECT CASE (TYPE_PRECON)

#ifdef WITH_SCARC_MKL
 
         ! Preconditioning by defect correction based on LU-decomposition
         ! If two-level method, also use coarse grid level, otherwise only use single (finest) grid level
         ! Either using a global CLUSTER_SPARSE_SOLVER or local PARDISO solvers from MKL
 
         CASE (NSCARC_RELAX_MKL)

            IF (HAS_TWO_LEVELS) THEN
               CALL SCARC_GET_NUMBER_OF_LEVELS(NSCARC_LEVEL_MULTI)
               IF (TYPE_COARSE == NSCARC_COARSE_DIRECT) TYPE_MKL(NLEVEL_MAX) = NSCARC_MKL_GLOBAL
            ELSE
               CALL SCARC_GET_NUMBER_OF_LEVELS(NSCARC_LEVEL_SINGLE)
            ENDIF

            IF (TYPE_SCOPE(1) == NSCARC_SCOPE_GLOBAL) THEN
               TYPE_MKL(NLEVEL_MIN) = NSCARC_MKL_GLOBAL
            ELSE IF (TYPE_SCOPE(1) == NSCARC_SCOPE_LOCAL) THEN
               TYPE_MKL(NLEVEL_MIN) = NSCARC_MKL_LOCAL
            ENDIF

#endif

         ! Preconditioning by defect correction based on geometric multigrid method,
         ! use specified hierarchy of grid levels
 
         CASE (NSCARC_RELAX_MULTIGRID)
            CALL SCARC_GET_NUMBER_OF_LEVELS(NSCARC_LEVEL_MULTI)
#ifdef WITH_SCARC_MKL
            IF (TYPE_COARSE == NSCARC_COARSE_DIRECT) TYPE_MKL(NLEVEL_MAX) = NSCARC_MKL_GLOBAL
#endif

 
         ! Preconditioning by defect correction based on local basic iterations (JACOBI/SSOR),
         ! if two-level method, also use coarse grid, otherwise only use single (finest) grid level
 
         CASE DEFAULT
            IF (HAS_TWO_LEVELS) THEN
               CALL SCARC_GET_NUMBER_OF_LEVELS(NSCARC_LEVEL_MULTI)
#ifdef WITH_SCARC_MKL
               IF (TYPE_COARSE == NSCARC_COARSE_DIRECT) TYPE_MKL(NLEVEL_MAX) = NSCARC_MKL_GLOBAL
#endif
            ELSE
               CALL SCARC_GET_NUMBER_OF_LEVELS(NSCARC_LEVEL_SINGLE)
            ENDIF

      END SELECT SELECT_KRYLOV_PRECON

 
   ! ---------- Global data-parallel Multigrid method 
 
   CASE (NSCARC_METHOD_MULTIGRID)

         ! If not specified by user, determine number of possible grid levels

         CALL SCARC_GET_NUMBER_OF_LEVELS(NSCARC_LEVEL_MULTI)

#ifdef WITH_SCARC_MKL

         ! In case of smoothing by different MKL solvers, mark levels for the use of MKL,
         ! either by locally acting PARDISO solvers or Globally acting CLUSTER_SPARSE_SOLVER

         IF (TYPE_SMOOTH == NSCARC_RELAX_MKL) THEN

            IF (TYPE_SCOPE(2) == NSCARC_SCOPE_LOCAL) THEN
               TYPE_MKL(NLEVEL_MIN:NLEVEL_MAX-1) = NSCARC_MKL_LOCAL
            ELSE IF (TYPE_SCOPE(2) == NSCARC_SCOPE_GLOBAL) THEN
               TYPE_MKL(NLEVEL_MIN:NLEVEL_MAX-1) = NSCARC_MKL_GLOBAL
            ENDIF

         ENDIF

         IF (TYPE_MKL(0) == NSCARC_MKL_COARSE) TYPE_MKL(NLEVEL_MAX) = NSCARC_MKL_GLOBAL
#endif


 
   ! ---------- Global LU-decomposition 
 
   CASE (NSCARC_METHOD_LU)

      ! Only use single (finest) grid level and mark this level for the use of MKL methods

      CALL SCARC_GET_NUMBER_OF_LEVELS(NSCARC_LEVEL_SINGLE)
      TYPE_MKL(NLEVEL_MIN) = TYPE_MKL(0)

 
   ! ---------- Global McKenney-Greengard-Mayo method - only finest level 
 
   CASE (NSCARC_METHOD_MGM)

      CALL SCARC_GET_NUMBER_OF_LEVELS(NSCARC_LEVEL_SINGLE)


END SELECT SELECT_SCARC_METHOD


#ifdef WITH_SCARC_MKL
 
! Define MKL related logical short names based on number of levels
 
DO NL = NLEVEL_MIN, NLEVEL_MAX
   IS_MKL_LEVEL(NL) = (TYPE_MKL(0)  == NSCARC_MKL_GLOBAL .AND. NL == NLEVEL_MIN) .OR. &
                      (TYPE_MKL(0)  == NSCARC_MKL_COARSE .AND. NL == NLEVEL_MAX) .OR. &
                      (TYPE_MKL(NL) == NSCARC_MKL_GLOBAL)
ENDDO
#endif

END SUBROUTINE SCARC_SETUP_LEVELS


! ------------------------------------------------------------------------------------------------
!> \brief Setup single level in case of default Krylov method
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_GET_NUMBER_OF_LEVELS(NTYPE)
USE SCARC_POINTERS, ONLY: M
INTEGER, INTENT(IN) :: NTYPE
INTEGER :: KLEVEL(3), KLEVEL_MIN, NM, NLEVEL

SELECT_LEVEL_TYPE: SELECT CASE (NTYPE)

   ! only use finest grid level
   CASE(NSCARC_LEVEL_SINGLE)
   
      NLEVEL     = 1
      NLEVEL_MIN = 1
      NLEVEL_MAX = 1
   
   ! determine maximum number of possible levels based on number of grid cells (based on doubling)
   CASE(NSCARC_LEVEL_MULTI)
   
      NLEVEL = NSCARC_LEVEL_MAX
      KLEVEL = NSCARC_LEVEL_MAX
   
      DO NM=1,NMESHES
         M => MESHES(NM)
         KLEVEL(1)=SCARC_GET_MAX_LEVEL(M%IBAR,1)
         IF (.NOT.TWO_D) KLEVEL(2)=SCARC_GET_MAX_LEVEL(M%JBAR,2)
         KLEVEL(3)=SCARC_GET_MAX_LEVEL(M%KBAR,3)
         KLEVEL_MIN = MINVAL(KLEVEL)
         IF (KLEVEL_MIN<NLEVEL) NLEVEL=KLEVEL_MIN
      ENDDO
   
      NLEVEL_MIN  = 1
      IF (IS_MG .OR. IS_CG_MG) THEN
         IF (SCARC_MULTIGRID_LEVEL /= -1) THEN
            NLEVEL_MAX  = NLEVEL_MIN + SCARC_MULTIGRID_LEVEL - 1
         ELSE
            NLEVEL_MAX  = NLEVEL
         ENDIF
      ELSE IF (HAS_TWO_LEVELS) THEN
         IF (SCARC_COARSE_LEVEL /= -1) THEN
            NLEVEL_MAX  = NLEVEL_MIN + SCARC_COARSE_LEVEL - 1
         ELSE
            NLEVEL_MAX  = NLEVEL
         ENDIF
      ENDIF
   
END SELECT SELECT_LEVEL_TYPE

END SUBROUTINE SCARC_GET_NUMBER_OF_LEVELS


! ------------------------------------------------------------------------------------------------
!> \brief Determine maximum number of possible levels 
! In case of GMG- or 2-Level-method, NC must be divisable by 2 at least one time
! ------------------------------------------------------------------------------------------------
INTEGER FUNCTION SCARC_GET_MAX_LEVEL(NC, IOR0)
INTEGER, INTENT(IN) :: NC, IOR0
INTEGER :: NC0, NL

! Print error message if not divisable by 2
IF (IS_GMG .AND. SCARC_MULTIGRID_LEVEL > 1 .AND. MOD(NC,2)/=0) THEN
   SELECT CASE (ABS(IOR0))
      CASE (1)
         CALL SCARC_SHUTDOWN(NSCARC_ERROR_GRID_NUMBERX, SCARC_NONE, NC)
      CASE (2)
         CALL SCARC_SHUTDOWN(NSCARC_ERROR_GRID_NUMBERY, SCARC_NONE, NC)
      CASE (3)
         CALL SCARC_SHUTDOWN(NSCARC_ERROR_GRID_NUMBERZ, SCARC_NONE, NC)
   END SELECT
ENDIF

! Divide by 2 as often as possible or until user defined max-level is reached
IF (SCARC_MULTIGRID_LEVEL > 1) THEN
   NC0=NC
   DO NL=1,NSCARC_LEVEL_MAX
      NC0=NC0/2
      IF (MOD(NC0,2)/=0) EXIT                ! if no longer divisable by two, leave loop ...
      IF (NL==SCARC_MULTIGRID_LEVEL) EXIT    ! if max possible number of levels reached, leave loop ...
      IF (NC0==1) EXIT                       ! if corresponding power of two has been found, leave loop ...
   ENDDO
   SCARC_GET_MAX_LEVEL=NL
ELSE
   SCARC_GET_MAX_LEVEL=NLEVEL_MIN
ENDIF

RETURN
END FUNCTION SCARC_GET_MAX_LEVEL


! -----------------------------------------------------------------------------
!> \brief Setup discretization information
! -----------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_GRIDS
USE SCARC_POINTERS, ONLY: M, S, L, G, XCOR, YCOR, ZCOR, XMID, YMID, ZMID, &
                          SCARC_POINT_TO_MESH, SCARC_POINT_TO_GRID
INTEGER :: NL, NM, NC, IX, IY, IZ, IO
INTEGER :: IBAR, JBAR, KBAR

CROUTINE = 'SCARC_SETUP_GRIDS'

! ---------- On all grid levels 
! Specify general mesh related geometry information

MESHES_LOOP1: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MESH(NM)

   ! Store bounds of mesh in SCARC-structure

   S%XS = M%XS;  S%XF = M%XF
   S%YS = M%YS;  S%YF = M%YF
   S%ZS = M%ZS;  S%ZF = M%ZF

   S%IBAR = M%IBAR;  S%JBAR = M%JBAR;  S%KBAR = M%KBAR
   IBAR   = M%IBAR;  JBAR   = M%JBAR;  KBAR   = M%KBAR

   LEVEL_LOOP1: DO NL = NLEVEL_MIN, NLEVEL_MAX

      CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

      L%NX = IBAR;  L%NY = JBAR;  L%NZ = KBAR

      L%N_CELLS = L%NX*L%NY*L%NZ

      L%N_WALL_CELLS_EXT = M%N_EXTERNAL_WALL_CELLS
      L%N_WALL_CELLS_INT = M%N_INTERNAL_WALL_CELLS
      L%N_WALL_CELLS     = L%N_WALL_CELLS_EXT + L%N_WALL_CELLS_INT

      ! Get coordination information

      L%DX = (S%XF-S%XS)/REAL(L%NX,EB) ; L%DXH = 0.5_EB*L%DX ; L%DX2 = L%DX
      L%DY = (S%YF-S%YS)/REAL(L%NY,EB) ; L%DYH = 0.5_EB*L%DY ; L%DY2 = L%DY
      L%DZ = (S%ZF-S%ZS)/REAL(L%NZ,EB) ; L%DZH = 0.5_EB*L%DZ ; L%DZ2 = L%DZ

      L%DXI  = 1.0_EB/L%DX;  L%DYI  = 1.0_EB/L%DY;  L%DZI =  1.0_EB/L%DZ
      L%DXI2 = L%DXI**2;     L%DYI2 = L%DYI**2;     L%DZI2 = L%DZI**2

      ! Needed in case of GMG with multiple grid levels

      IBAR=IBAR/2
      IF (.NOT.TWO_D) JBAR=JBAR/2
      KBAR=KBAR/2

      IF (NL == NLEVEL_MIN) THEN

         ! On finest level store information about obstructions

         L%N_OBST = M%N_OBST
         ALLOCATE(L%OBST(L%N_OBST), STAT=IERROR)
         CALL ChkMemErr('SCARC_SETUP_GRIDS','OBST',IERROR)

         DO IO = 1, L%N_OBST
            L%OBST(IO)%I1  = M%OBSTRUCTION(IO)%I1
            L%OBST(IO)%I2  = M%OBSTRUCTION(IO)%I2
            L%OBST(IO)%J1  = M%OBSTRUCTION(IO)%J1
            L%OBST(IO)%J2  = M%OBSTRUCTION(IO)%J2
            L%OBST(IO)%K1  = M%OBSTRUCTION(IO)%K1
            L%OBST(IO)%K2  = M%OBSTRUCTION(IO)%K2
         ENDDO

         ! Point to already existing arrays from main FDS program

         XCOR => M%X ;  YCOR => M%Y ;  ZCOR => M%Z
         XMID => M%XC;  YMID => M%YC;  ZMID => M%ZC

#ifdef WITH_SCARC_DEBUG
         IF (L%NX < 10) THEN
            MSG%CFORM1 = "( E14.6)"  ; WRITE(MSG%CFORM1(2:2),'(I1.1)') L%NX
            MSG%CFORM4 = "( I5)"  ; WRITE(MSG%CFORM4(2:2),'(I1.1)') L%NX
         ELSE
            MSG%CFORM1 = "(  E14.6)" ; WRITE(MSG%CFORM1(2:3),'(I2.2)') L%NX
            MSG%CFORM4 = "(  I5)" ; WRITE(MSG%CFORM4(2:3),'(I2.2)') L%NX
         ENDIF
         IF (L%NX+1 < 10) THEN
            MSG%CFORM2 = "( E14.6)"  ; WRITE(MSG%CFORM2(2:2),'(I1.1)') L%NX+1
         ELSE
            MSG%CFORM2 = "(  E14.6)" ; WRITE(MSG%CFORM2(2:3),'(I2.2)') L%NX+1
         ENDIF
         IF (L%NX+2 < 10) THEN
            MSG%CFORM3 = "( E14.6)"  ; WRITE(MSG%CFORM3(2:2),'(I1.1)') L%NX+2
         ELSE
            MSG%CFORM3 = "(  E14.6)" ; WRITE(MSG%CFORM3(2:3),'(I2.2)') L%NX+2
         ENDIF
#endif

      ELSE

         ! Allocate and compute coordinate information for coarser levels

         CALL SCARC_ALLOCATE_REAL1(L%XCOR, 0, L%NX, NSCARC_INIT_NONE, 'L%XCOR', CROUTINE)
         CALL SCARC_ALLOCATE_REAL1(L%YCOR, 0, L%NY, NSCARC_INIT_NONE, 'L%YCOR', CROUTINE)
         CALL SCARC_ALLOCATE_REAL1(L%ZCOR, 0, L%NZ, NSCARC_INIT_NONE, 'L%ZCOR', CROUTINE)

         DO IX = 0, L%NX
            L%XCOR(IX) = S%XS + IX*L%DX
         ENDDO
         DO IY = 0, L%NY
            L%YCOR(IY) = S%YS + IY*L%DY
         ENDDO
         DO IZ = 0, L%NZ
            L%ZCOR(IZ) = S%ZS + IZ*L%DZ
         ENDDO

         ! Allocate and compute midpoint information for coarser levels

         CALL SCARC_ALLOCATE_REAL1(L%XMID, 0, L%NX+1, NSCARC_INIT_NONE, 'L%XMID', CROUTINE)
         CALL SCARC_ALLOCATE_REAL1(L%YMID, 0, L%NY+1, NSCARC_INIT_NONE, 'L%YMID', CROUTINE)
         CALL SCARC_ALLOCATE_REAL1(L%ZMID, 0, L%NZ+1, NSCARC_INIT_NONE, 'L%ZMID', CROUTINE)

         L%XMID(0) = S%XS - 0.5_EB*L%DX
         DO IX = 1, L%NX
            L%XMID(IX) = 0.5_EB*(L%XCOR(IX-1) + L%XCOR(IX))
         ENDDO
         L%XMID(L%NX+1) = S%XF + 0.5_EB*L%DX

         L%YMID(0) = S%YS - 0.5_EB*L%DY
         DO IY = 1, L%NY
            L%YMID(IY) = 0.5_EB*(L%YCOR(IY-1) + L%YCOR(IY))
         ENDDO
         L%YMID(L%NY+1) = S%YF + 0.5_EB*L%DY

         L%ZMID(0) = S%ZS - 0.5_EB*L%DZ
         DO IZ = 1, L%NZ
            L%ZMID(IZ) = 0.5_EB*(L%ZCOR(IZ-1) + L%ZCOR(IZ))
         ENDDO
         L%ZMID(L%NZ+1) = S%ZF + 0.5_EB*L%DZ

         XCOR => L%XCOR;  YCOR => L%YCOR;  ZCOR => L%ZCOR
         XMID => L%XMID;  YMID => L%YMID;  ZMID => L%ZMID

      ENDIF

      ! Allocate vectors for step sizes in different directions

      CALL SCARC_ALLOCATE_REAL1(L%DXL, 0, L%NX, NSCARC_INIT_ZERO, 'L%DXL', CROUTINE)
      CALL SCARC_ALLOCATE_REAL1(L%DYL, 0, L%NY, NSCARC_INIT_ZERO, 'L%DYL', CROUTINE)
      CALL SCARC_ALLOCATE_REAL1(L%DZL, 0, L%NZ, NSCARC_INIT_ZERO, 'L%DZL', CROUTINE)

      ! Set step sizes between cell midpoints, use interior step sizes for ghost cells as initial values
      ! correct sizes for ghost cells are exchanged later

      DO IX = 1, L%NX-1
         L%DXL(IX) = XMID(IX+1) - XMID(IX)
      ENDDO
      L%DXL(0)    = L%DXL(1)
      L%DXL(L%NX) = L%DXL(L%NX-1)

      DO IY = 1, L%NY-1
         L%DYL(IY) = YMID(IY+1) - YMID(IY)
      ENDDO
      L%DYL(0)    = L%DYL(1)
      L%DYL(L%NY) = L%DYL(L%NY-1)

      DO IZ = 1, L%NZ-1
         L%DZL(IZ) = ZMID(IZ+1) - ZMID(IZ)
      ENDDO
      L%DZL(0)    = L%DZL(1)
      L%DZL(L%NZ) = L%DZL(L%NZ-1)

   ENDDO LEVEL_LOOP1
ENDDO MESHES_LOOP1

 
! ---------------------- On finest grid level -------------------------------------------------
! Allocate several arrays for the administration of discretization related data
 
MESHES_LOOP2: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID(NM, NLEVEL_MIN)               ! sets pointers M, L and G

   ! Set pointers to already existing cell and wall index arrays from main program (on finest level)
   CALL SCARC_SETUP_CELL_INDEX(L, M, NLEVEL_MIN)
   CALL SCARC_SETUP_WALL_INDEX(L, G, M, NLEVEL_MIN)

   ! Allocate and initialize IS_SOLID array which indicates the state of a cell (gasphase/solid)

   CALL SCARC_ALLOCATE_LOG3(L%IS_SOLID, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_TRUE, 'L%IS_SOLID', CROUTINE)
   L%IS_SOLID (1:L%NX, 1:L%NY, 1:L%NZ) = .FALSE.

   ! Identify and mark solid obstruction cells in IS_SOLID-part of the discretization

   DO IZ = 1, L%NZ
      DO IY = 1, L%NY
         DO IX = 1, L%NX
            IF (M%SOLID(M%CELL_INDEX(IX, IY, IZ))) L%IS_SOLID(IX, IY, IZ) = .TRUE.
         ENDDO
      ENDDO
   ENDDO

 
   ! If both discretization types (structured/unstructured) must be administrated (MGM method only):
   ! allocate all arrays which are related to a specific discretization type
 
   IF (HAS_MULTIPLE_GRIDS) THEN
 
      ! ---------- First process structured discretization
 
      CALL SCARC_SET_GRID_TYPE(NSCARC_GRID_STRUCTURED)
      CALL SCARC_POINT_TO_GRID (NM, NLEVEL_MIN)

      ! Number of local cells per mesh

      CALL SCARC_ALLOCATE_INT1(G%NC_LOCAL , 1, NMESHES, NSCARC_INIT_ZERO, 'G%NC_LOCAL', CROUTINE)
      CALL SCARC_ALLOCATE_INT1(G%NC_OFFSET, 1, NMESHES, NSCARC_INIT_ZERO, 'G%NC_OFFSET', CROUTINE)

      ! Allocate wall information array

      ALLOCATE(G%WALL(L%N_WALL_CELLS), STAT=IERROR)
      CALL ChkMemErr('SCARC_SETUP_GRIDS','WALL',IERROR)

      ! Allocate and preset cell numbers array

      CALL SCARC_ALLOCATE_INT3(G%CELL_NUMBER, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, &
                               NSCARC_INIT_UNDEF, 'CELL_NUMBER', CROUTINE)

      ! Allocate index array which specifies I, J, K components for all degrees of freedom

      NC = L%NX * L%NY * L%NZ
      CALL SCARC_ALLOCATE_INT1(G%ICX, 1, NC, NSCARC_INIT_UNDEF, 'G%ICX', CROUTINE)
      CALL SCARC_ALLOCATE_INT1(G%ICY, 1, NC, NSCARC_INIT_UNDEF, 'G%ICY', CROUTINE)
      CALL SCARC_ALLOCATE_INT1(G%ICZ, 1, NC, NSCARC_INIT_UNDEF, 'G%ICZ', CROUTINE)

      ! Define local cell numbers for Poisson equation

      DO IZ=1,L%NZ
         DO IY=1,L%NY
            DO IX=1,L%NX
               G%NC_LOCAL(NM) = G%NC_LOCAL(NM) + 1
               G%CELL_NUMBER(IX,IY,IZ) = G%NC_LOCAL(NM)
               G%ICX(G%NC_LOCAL(NM)) = IX
               G%ICY(G%NC_LOCAL(NM)) = IY
               G%ICZ(G%NC_LOCAL(NM)) = IZ
            ENDDO
         ENDDO
      ENDDO
      G%NC   = G%NC_LOCAL(NM)
      G%NCE  = G%NC_LOCAL(NM)
      G%NCE2 = G%NC_LOCAL(NM)


#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SETUP_GRIDS: STRUCTURED: NC, NCE, NCE2:', G%NC, G%NCE, G%NCE2
#endif
 
      ! ---------------- Then process unstructured discretization
 
      CALL SCARC_SET_GRID_TYPE(NSCARC_GRID_UNSTRUCTURED)
      CALL SCARC_POINT_TO_GRID (NM, NLEVEL_MIN)

      ! Also allocate and preset cell numbers and state arrays for unstructured discretization

      CALL SCARC_ALLOCATE_INT3(G%CELL_NUMBER, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, &
                               NSCARC_INIT_UNDEF, 'G%CELL_NUMBER', CROUTINE)

      ! Allocate index array which specifies I, J, K components for all degrees of freedom

      NC = L%NX * L%NY * L%NZ
      CALL SCARC_ALLOCATE_INT1(G%ICX, 1, NC, NSCARC_INIT_UNDEF, 'G%ICX', CROUTINE)
      CALL SCARC_ALLOCATE_INT1(G%ICY, 1, NC, NSCARC_INIT_UNDEF, 'G%ICY', CROUTINE)
      CALL SCARC_ALLOCATE_INT1(G%ICZ, 1, NC, NSCARC_INIT_UNDEF, 'G%ICZ', CROUTINE)

      ! Number of local cells per mesh

      CALL SCARC_ALLOCATE_INT1(G%NC_LOCAL , 1, NMESHES, NSCARC_INIT_ZERO, 'G%NC_LOCAL', CROUTINE)
      CALL SCARC_ALLOCATE_INT1(G%NC_OFFSET, 1, NMESHES, NSCARC_INIT_ZERO, 'G%NC_OFFSET', CROUTINE)

      ! Allocate wall information array

      ALLOCATE(G%WALL(L%N_WALL_CELLS), STAT=IERROR)
      CALL ChkMemErr('SCARC_SETUP_GRIDS','WALL',IERROR)

      ! Define local cell numbers for Poisson equation

      DO IZ=1,L%NZ
         DO IY=1,L%NY
            DO IX=1,L%NX
               IF (.NOT.L%IS_SOLID(IX,IY,IZ)) THEN
                  G%NC_LOCAL(NM) = G%NC_LOCAL(NM) + 1
                  G%CELL_NUMBER(IX,IY,IZ) = G%NC_LOCAL(NM)
                  G%ICX(G%NC_LOCAL(NM)) = IX
                  G%ICY(G%NC_LOCAL(NM)) = IY
                  G%ICZ(G%NC_LOCAL(NM)) = IZ
               ENDIF
            ENDDO
         ENDDO
      ENDDO
      G%NC   = G%NC_LOCAL(NM)
      G%NCE  = G%NC_LOCAL(NM)
      G%NCE2 = G%NC_LOCAL(NM)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SETUP_GRIDS: UNSTRUCTURED: NC, NCE, NCE2:', G%NC, G%NCE, G%NCE2
#endif

      IF (IS_MGM .AND. SCARC_MGM_USE_LU) CALL SCARC_SETUP_GRID_PERMUTATION

 
   ! If only one specified type of discretization must be admistrated:
   ! allocate and preset cell numbers and state arrays for requested type of discretization
 
   ELSE

      ! ---------------- Only process specified type of discretization

      CALL SCARC_SET_GRID_TYPE(TYPE_GRID)
      CALL SCARC_POINT_TO_GRID (NM, NLEVEL_MIN)

      ! Also allocate and preset cell numbers and state arrays for unstructured discretization

      CALL SCARC_ALLOCATE_INT3(G%CELL_NUMBER, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, &
                               NSCARC_INIT_UNDEF, 'CELL_NUMBER', CROUTINE)

      ! Allocate index array which specifies I, J, K components for all degrees of freedom

      NC = L%NX * L%NY * L%NZ
      CALL SCARC_ALLOCATE_INT1(G%ICX, 1, NC, NSCARC_INIT_UNDEF, 'G%ICX', CROUTINE)
      CALL SCARC_ALLOCATE_INT1(G%ICY, 1, NC, NSCARC_INIT_UNDEF, 'G%ICY', CROUTINE)
      CALL SCARC_ALLOCATE_INT1(G%ICZ, 1, NC, NSCARC_INIT_UNDEF, 'G%ICZ', CROUTINE)

      ! Number of local cells per mesh

      CALL SCARC_ALLOCATE_INT1(G%NC_LOCAL , 1, NMESHES, NSCARC_INIT_ZERO, 'G%NC_LOCAL', CROUTINE)
      CALL SCARC_ALLOCATE_INT1(G%NC_OFFSET, 1, NMESHES, NSCARC_INIT_ZERO, 'G%NC_OFFSET', CROUTINE)

      ! Allocate wall information array

      ALLOCATE(G%WALL(L%N_WALL_CELLS), STAT=IERROR)
      CALL ChkMemErr('SCARC_SETUP_GRIDS','WALL',IERROR)

      ! Define local cell numbers for Poisson equation

      DO IZ=1,L%NZ
         DO IY=1,L%NY
            DO IX=1,L%NX
               IF (IS_STRUCTURED .OR. .NOT.L%IS_SOLID(IX,IY,IZ)) THEN

                  G%NC_LOCAL(NM) = G%NC_LOCAL(NM) + 1
                  G%CELL_NUMBER(IX, IY, IZ) = G%NC_LOCAL(NM)

                  G%ICX(G%NC_LOCAL(NM)) = IX
                  G%ICY(G%NC_LOCAL(NM)) = IY
                  G%ICZ(G%NC_LOCAL(NM)) = IZ

               ENDIF
            ENDDO
         ENDDO
      ENDDO
      G%NC   = G%NC_LOCAL(NM)
      G%NCE  = G%NC_LOCAL(NM)
      G%NCE2 = G%NC_LOCAL(NM)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SETUP_GRIDS: ', TYPE_GRID,' : NC, NCE, NCE2:', G%NC, G%NCE, G%NCE2
#endif
   ENDIF

ENDDO MESHES_LOOP2

END SUBROUTINE SCARC_SETUP_GRIDS

! -----------------------------------------------------------------------------
!> \brief Setup permutation of grid cells (MGM only)
! -----------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_GRID_PERMUTATION()
USE SCARC_POINTERS, ONLY: L, G, GWC
INTEGER :: IW, I, J, K, IOR0, IC, JC, KC

!
! Allocate permutation vectors 
!
CALL SCARC_ALLOCATE_INT1 (G%PERM_FW , 1, G%NC, NSCARC_INIT_ZERO, 'G%PERM_FW', CROUTINE)
CALL SCARC_ALLOCATE_INT1 (G%PERM_BW , 1, G%NC, NSCARC_INIT_ZERO, 'G%PERM_BW', CROUTINE)
   
!
! Obstruction cells are numbered last such that they appear at the end of a vector
!
G%PERM_FW = 0
JC = G%NC

DO IW = L%N_WALL_CELLS_EXT+1, L%N_WALL_CELLS_EXT + L%N_WALL_CELLS_INT
   
   GWC => G%WALL(IW)
   
   I = GWC%IXW
   J = GWC%IYW
   K = GWC%IZW
   
   IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE
   
   IOR0 = GWC%IOR
   IC   = G%CELL_NUMBER(I,J,K)

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'IW, I, J, K, IOR0, IC:', IW, I, J, K, IOR0, IC
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

!
! Interface cells are numbered second last
!
DO IW = 1, L%N_WALL_CELLS_EXT
   
   GWC => G%WALL(IW)
   IF (GWC%BTYPE /= INTERNAL) CYCLE
   
   I = GWC%IXW
   J = GWC%IYW
   K = GWC%IZW
   
   IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE
   
   IOR0 = GWC%IOR
   IC   = G%CELL_NUMBER(I,J,K)

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'IW, I, J, K, IOR0, IC:', IW, I, J, K, IOR0, IC
   WRITE(MSG%LU_DEBUG,*) 'INTERFACE: PERM_FW(', IC, ')=', G%PERM_FW(IC),', PERM_BW(', JC, ')=', G%PERM_BW(JC)
#endif
   G%PERM_FW(IC) = JC
   G%PERM_BW(JC) = IC
   JC = JC - 1

ENDDO

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'AFTER INTERFACE: PERM_FW:'
WRITE(MSG%LU_DEBUG,'(8I4)') G%PERM_FW
WRITE(MSG%LU_DEBUG,*) 'AFTER INTERFACE: PERM_BW:'
WRITE(MSG%LU_DEBUG,'(8I4)') G%PERM_BW
#endif

!
! The rest is used from beginning to first interface cell
!
KC = 1
DO IC = 1, G%NC
   IF (G%PERM_FW(IC) /= 0) CYCLE
   G%PERM_BW(KC) = IC
   G%PERM_FW(IC) = KC
   KC = KC + 1
ENDDO
IF (KC /= JC + 1) WRITE(*,*) 'ERROR IN MGM PERMUTATION: KC=', KC,': JC=', JC

G%NONZERO = KC

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'AFTER FINAL FILL: PERM_FW:', KC, JC
WRITE(MSG%LU_DEBUG,'(8I4)') G%PERM_FW
WRITE(MSG%LU_DEBUG,*) 'AFTER FINAL FILL: PERM_BW:'
WRITE(MSG%LU_DEBUG,'(8I4)') G%PERM_BW
WRITE(MSG%LU_DEBUG,*) 'G%ICX'
WRITE(MSG%LU_DEBUG,'(8I4)') G%ICX
WRITE(MSG%LU_DEBUG,*) 'G%ICY'
WRITE(MSG%LU_DEBUG,'(8I4)') G%ICY
WRITE(MSG%LU_DEBUG,*) 'G%ICZ'
WRITE(MSG%LU_DEBUG,'(8I4)') G%ICZ
DO K = 1, L%NZ
   DO J = 1, L%NY
      WRITE(MSG%LU_DEBUG,*) (L%IS_SOLID(I, J, K), I=1, L%NX)
   ENDDO
ENDDO
#endif

END SUBROUTINE SCARC_SETUP_GRID_PERMUTATION

! -----------------------------------------------------------------------------
!> \brief Setup discretization information on coarser levels
! -----------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_GRID_LEVEL(NL)
USE SCARC_POINTERS, ONLY: LF, LC, GC, SCARC_POINT_TO_MULTIGRID
INTEGER, INTENT(IN) :: NL
INTEGER :: NM, NC, IXF, IYF, IZF, IX, IY, IZ, NSTEP

CROUTINE = 'SCARC_SETUP_GRID_LEVEL'

MESHES_LOOP1: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MULTIGRID (NM, NLEVEL_MIN, NL)

   CALL SCARC_ALLOCATE_LOG3(LC%IS_SOLID, 0, LC%NX+1, 0, LC%NY+1, 0, LC%NZ+1, NSCARC_INIT_FALSE, 'LC%IS_SOLID', CROUTINE)
   LC%IS_SOLID (1:LC%NX, 1:LC%NY, 1:LC%NZ)  = .FALSE.

   NSTEP = 2**(NL - NLEVEL_MIN)

   SELECT CASE(TYPE_GRID)

 
      ! Get cell numberings for coarser grid in case of structured discretization
 
      CASE (NSCARC_GRID_STRUCTURED)

         CALL SCARC_ALLOCATE_INT1(GC%NC_LOCAL , 1, NMESHES, NSCARC_INIT_ZERO, 'GC%NC_LOCAL', CROUTINE)
         CALL SCARC_ALLOCATE_INT1(GC%NC_OFFSET, 1, NMESHES, NSCARC_INIT_ZERO, 'GC%NC_OFFSET', CROUTINE)

         CALL SCARC_ALLOCATE_INT3(GC%CELL_NUMBER, 0, LC%NX+1, 0, LC%NY+1, 0, LC%NZ+1, &
                                  NSCARC_INIT_UNDEF, 'GC%CELL_NUMBER', CROUTINE)

         NC = LC%NX * LC%NY * LC%NZ
         CALL SCARC_ALLOCATE_INT1(GC%ICX , 1, NC, NSCARC_INIT_UNDEF, 'GC%ICX', CROUTINE)
         CALL SCARC_ALLOCATE_INT1(GC%ICY , 1, NC, NSCARC_INIT_UNDEF, 'GC%ICY', CROUTINE)
         CALL SCARC_ALLOCATE_INT1(GC%ICZ , 1, NC, NSCARC_INIT_UNDEF, 'GC%ICZ', CROUTINE)

         GC%CELL_NUMBER = NSCARC_UNDEF_INT

         DO IZ = 1, LC%NZ
            IZF = (IZ-1)*NSTEP + 1
            DO IY = 1, LC%NY
               IYF = (IY-1)*NSTEP + 1
               DO IX = 1, LC%NX

                  IXF = (IX-1)*NSTEP + 1
                  LC%IS_SOLID(IX,IY,IZ) = LF%IS_SOLID(IXF, IYF, IZF)

                  GC%NC_LOCAL(NM) = GC%NC_LOCAL(NM) + 1
                  GC%CELL_NUMBER(IX,IY,IZ) = GC%NC_LOCAL(NM)

                  GC%ICX(GC%NC_LOCAL(NM)) = IX
                  GC%ICY(GC%NC_LOCAL(NM)) = IY
                  GC%ICZ(GC%NC_LOCAL(NM)) = IZ

               ENDDO
            ENDDO
         ENDDO

         GC%NC = GC%NC_LOCAL(NM)

 
      ! Get cell numberings for coarser grid in case of unstructured discretization
 
      CASE (NSCARC_GRID_UNSTRUCTURED)

         CALL SCARC_ALLOCATE_INT1(GC%NC_LOCAL , 1, NMESHES, NSCARC_INIT_ZERO, 'GC%NC_LOCAL', CROUTINE)
         CALL SCARC_ALLOCATE_INT1(GC%NC_OFFSET, 1, NMESHES, NSCARC_INIT_ZERO, 'GC%NC_OFFSET', CROUTINE)

         CALL SCARC_ALLOCATE_INT3(GC%CELL_NUMBER, 0, LC%NX+1, 0, LC%NY+1, 0, LC%NZ+1, &
                                  NSCARC_INIT_UNDEF, 'GC%CELL_NUMBER', CROUTINE)

         NC = LC%NX * LC%NY * LC%NZ
         CALL SCARC_ALLOCATE_INT1(GC%ICX , 1, NC, NSCARC_INIT_UNDEF, 'GC%ICX', CROUTINE)
         CALL SCARC_ALLOCATE_INT1(GC%ICY , 1, NC, NSCARC_INIT_UNDEF, 'GC%ICY', CROUTINE)
         CALL SCARC_ALLOCATE_INT1(GC%ICZ , 1, NC, NSCARC_INIT_UNDEF, 'GC%ICZ', CROUTINE)

         GC%CELL_NUMBER = NSCARC_UNDEF_INT

         DO IZ = 1, LC%NZ
            IZF = (IZ-1)*NSTEP + 1
            DO IY = 1, LC%NY
               IYF = (IY-1)*NSTEP + 1
               DO IX = 1, LC%NX

                  IXF = (IX-1)*NSTEP + 1
                  LC%IS_SOLID(IX,IY,IZ) = LF%IS_SOLID(IXF, IYF, IZF)

                  IF (.NOT.LF%IS_SOLID(IXF, IYF, IZF)) THEN

                     GC%NC_LOCAL(NM) = GC%NC_LOCAL(NM) + 1
                     GC%CELL_NUMBER(IX,IY,IZ) = GC%NC_LOCAL(NM)

                     GC%ICX(GC%NC_LOCAL(NM)) = IX
                     GC%ICY(GC%NC_LOCAL(NM)) = IY
                     GC%ICZ(GC%NC_LOCAL(NM)) = IZ

                  ENDIF

               ENDDO
            ENDDO
         ENDDO

         GC%NC = GC%NC_LOCAL(NM)

   END SELECT

ENDDO MESHES_LOOP1

END SUBROUTINE SCARC_SETUP_GRID_LEVEL


! ----------------------------------------------------------------------------------------------------------
!> \brief Get information about global numbers of unknowns for unstructured discretization
! ----------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_DIMENSIONS(NL)
USE SCARC_POINTERS, ONLY: G, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NL
INTEGER :: NM, NM2

! Preset communication array MESH_INT with local numbers of cells for all meshes depending on type of discretization
MESHES_LOOP1: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
   MESH_INT(NM) = G%NC_LOCAL(NM)

!   DO NM2 = LOWER_MESH_INDEX, UPPER_MESH_INDEX
!      MESH_INT(NM2) = G%NC_LOCAL(NM2)
!   ENDDO

ENDDO MESHES_LOOP1


! Broadcast number of local mesh cells on level NL to all and build global sum
IF (N_MPI_PROCESSES > 1) &
   CALL MPI_ALLGATHERV(MPI_IN_PLACE,1,MPI_INTEGER,MESH_INT,COUNTS,DISPLS, MPI_INTEGER,MPI_COMM_WORLD,IERROR)
NC_GLOBAL(NL) = SUM(MESH_INT(1:NMESHES))

! Store information on local and global cells numbers on data structure of corresponding discretization type
MESHES_LOOP2: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

   G%NC_LOCAL(1:NMESHES) = MESH_INT(1:NMESHES)
   G%NC_GLOBAL = SUM(MESH_INT(1:NMESHES))

   ! compute offset between local grid numberings
   IF (NMESHES > 1) THEN
      DO NM2=2,NMESHES
         G%NC_OFFSET(NM2) = G%NC_OFFSET(NM2-1) + G%NC_LOCAL(NM2-1)
      ENDDO
   ENDIF

ENDDO MESHES_LOOP2

IF (NL == NLEVEL_MIN) THEN
   DO NM = 1, NMESHES
      SCARC(NM)%NC = MESH_INT(NM)
   ENDDO
ENDIF

END SUBROUTINE SCARC_SETUP_DIMENSIONS

! ----------------------------------------------------------------------------------------------------
!> \brief Setup structures related to mesh faces on finest grid level
!   - get dimensions for each of the 6 faces of a mesh
!   - get grid width vector along face
!   - get information for adjacent neighbors
!   - allocate pointer arrays for data exchanges with neighbors
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_FACES
USE SCARC_POINTERS, ONLY: M, S, L, LC, F, OL, SCARC_POINT_TO_GRID, SCARC_POINT_TO_OTHER_GRID
INTEGER :: NL, NM, NOM
INTEGER :: IFACE, IOR0, JOR0, INBR, IWG, ICW
LOGICAL :: IS_KNOWN(-3:3)
INTEGER :: FACE_NEIGHBORS(-3:3, NSCARC_MAX_FACE_NEIGHBORS)
INTEGER :: MESH_NEIGHBORS(6*NSCARC_MAX_FACE_NEIGHBORS)
INTEGER :: N_FACE_NEIGHBORS(-3:3)
INTEGER :: N_MESH_NEIGHBORS

CROUTINE = 'SCARC_SETUP_FACES'

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID(NM, NLEVEL_MIN)             ! consider only finest grid level

   ! Allocate FACE arrays on different grid levels

   ALLOCATE(L%FACE(-3:3), STAT=IERROR)
   CALL ChkMemErr('SCARC_SETUP_FACES','FACE',IERROR)

   IF (NLEVEL_MAX > NLEVEL_MIN) THEN
      DO NL = NLEVEL_MIN+1, NLEVEL_MAX
         LC => SCARC(NM)%LEVEL(NL)
         ALLOCATE(LC%FACE(-3:3), STAT=IERROR)
         CALL ChkMemErr('SCARC_SETUP_FACES','FACE',IERROR)
      ENDDO
   ENDIF

   FACE_NEIGHBORS = -1
   MESH_NEIGHBORS = -1

   N_FACE_NEIGHBORS = 0
   N_MESH_NEIGHBORS = 0

   CALL SCARC_SETUP_FACE_BASICS(L)
 
   ! Store first wall cell number for each face
 
   ICW = 1
   FACE_ORDER_LOOP: DO IFACE = 1, 6
      IOR0 = FACE_ORIENTATION(IFACE)
      F => L%FACE(IOR0)
      F%NCW0 = ICW
      ICW = ICW + F%NCW
   ENDDO FACE_ORDER_LOOP

   ! Loop over external wall cells:
   ! store basic data and determine number of adajacent neighbors to each face
 
   EXTERNAL_WALL_CELLS_LOOP: DO IWG = 1, L%N_WALL_CELLS_EXT

      NOM  = M%EXTERNAL_WALL(IWG)%NOM
      IOR0 = M%WALL(IWG)%ONE_D%IOR

      IF (NOM /= 0) THEN
         IS_KNOWN = .FALSE.
         DO JOR0 = -3, 3                                              ! neighbor already known?
            IF (JOR0 == 0) CYCLE
            DO INBR = 1, N_FACE_NEIGHBORS(JOR0)
               IF (FACE_NEIGHBORS(JOR0, INBR) == NOM) THEN
                  IS_KNOWN(JOR0) = .TRUE.
                  EXIT
               ENDIF
            ENDDO
         ENDDO
         IF (.NOT.IS_KNOWN(IOR0)) THEN
            N_FACE_NEIGHBORS(IOR0) = N_FACE_NEIGHBORS(IOR0) + 1       ! increase neighbor counter for face
            FACE_NEIGHBORS(IOR0, N_FACE_NEIGHBORS(IOR0)) = NOM        ! store number of neighbor for face
         ENDIF
         IF (.NOT.ANY(IS_KNOWN)) THEN
            N_MESH_NEIGHBORS = N_MESH_NEIGHBORS + 1                   ! increase neighbor counter for mesh
            MESH_NEIGHBORS(N_FACE_NEIGHBORS(IOR0)) = NOM              ! store number of neighbor for mesh
         ENDIF
      ENDIF

   ENDDO EXTERNAL_WALL_CELLS_LOOP

 
   ! Allocate array which stores numbers of all neighboring meshes
 
   IF (N_MESH_NEIGHBORS /= 0) &
      CALL SCARC_ALLOCATE_INT1(S%NEIGHBORS, 1, N_MESH_NEIGHBORS, NSCARC_INIT_UNDEF, 'S%NEIGHBORS', CROUTINE)
   S%N_NEIGHBORS = N_MESH_NEIGHBORS

   NEIGHBORS_OF_FACE_LOOP: DO IOR0 = -3, 3

      IF (IOR0 == 0) CYCLE NEIGHBORS_OF_FACE_LOOP

      ! If there are neighbors at face IOR0 store information about them

      F => L%FACE(IOR0)
      IF (N_FACE_NEIGHBORS(IOR0) /= 0) THEN

         ! Allocate array for storing the numbers of the single neighbors

         F%N_NEIGHBORS = N_FACE_NEIGHBORS(IOR0)
         CALL SCARC_ALLOCATE_INT1(F%NEIGHBORS, 1, N_FACE_NEIGHBORS(IOR0), NSCARC_INIT_NONE, 'F%NEIGHBORS', CROUTINE)

         ! Store every neighbor and allocate corresponding administration arrays on finest level

         DO INBR = 1, N_FACE_NEIGHBORS(IOR0)

            NOM = FACE_NEIGHBORS(IOR0, INBR)
            F%NEIGHBORS(INBR) = NOM                          ! store NOM as a neighbor of that face and if
            CALL SCARC_STORE_NEIGHBOR(NM, NOM)               ! not already done also as mesh neighbor itself

            CALL SCARC_POINT_TO_OTHER_GRID(NM, NOM, NLEVEL_MIN)
            IF (.NOT.ALLOCATED(OL%FACE)) THEN
               ALLOCATE(OL%FACE(-3:3), STAT=IERROR)
               CALL ChkMemErr('SCARC_SETUP_FACES','OL%FACE',IERROR)
            ENDIF

         ENDDO

      ENDIF
   ENDDO NEIGHBORS_OF_FACE_LOOP

ENDDO MESHES_LOOP

END SUBROUTINE SCARC_SETUP_FACES


! ------------------------------------------------------------------------------------------------
!> \brief Setup subdivision information 
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_SUBDIVISION
USE SCARC_POINTERS, ONLY: SUB
INTEGER, ALLOCATABLE, DIMENSION(:) :: BUFFER_INT
INTEGER, ALLOCATABLE, DIMENSION(:) :: COUNTS_NBR   
INTEGER, ALLOCATABLE, DIMENSION(:) :: DISPLS_NBR    
INTEGER :: N, NM, INBR, IP, MAX_NBR

CROUTINE = 'SCARC_SETUP_SUBDIVISION'

! Determine number of neighbors for each mesh and make them available in a global array
SUB => SUBDIVISION

CALL SCARC_ALLOCATE_INT1 (SUB%N_NEIGHBORS, 1, NMESHES, NSCARC_INIT_ZERO, 'SUB%N_NEIGHBORS', CROUTINE)
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   SUB%N_NEIGHBORS(NM) = SCARC(NM)%N_NEIGHBORS
ENDDO

IF (N_MPI_PROCESSES > 1) &
   CALL MPI_ALLGATHERV(MPI_IN_PLACE,1,MPI_INTEGER,SUB%N_NEIGHBORS,COUNTS,DISPLS, MPI_INTEGER,MPI_COMM_WORLD,IERROR)
SUB%N_NEIGHBORS_TOTAL = SUM(SUB%N_NEIGHBORS)

CALL SCARC_ALLOCATE_INT1 (COUNTS_NBR, 0, N_MPI_PROCESSES-1, NSCARC_INIT_ZERO, 'COUNTS_NBR', CROUTINE)
CALL SCARC_ALLOCATE_INT1 (DISPLS_NBR, 0, N_MPI_PROCESSES-1, NSCARC_INIT_ZERO, 'DISPLS_NBR', CROUTINE)

DO N = 0, N_MPI_PROCESSES - 1
   DO NM = 1, NMESHES
      IF (PROCESS(NM) == N) COUNTS_NBR(N) = COUNTS_NBR(N) + SUB%N_NEIGHBORS(NM)
   ENDDO
ENDDO
DO N = 1, N_MPI_PROCESSES -1
   DISPLS_NBR(N) = COUNTS_NBR(N-1) + DISPLS_NBR(N-1)
ENDDO

CALL SCARC_ALLOCATE_INT1 (BUFFER_INT, 1, SUB%N_NEIGHBORS_TOTAL, NSCARC_INIT_ZERO, 'BUFFER_INT', CROUTINE)
IP = 1
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   DO INBR = 1, SCARC(NM)%N_NEIGHBORS
      BUFFER_INT(DISPLS_NBR(PROCESS(NM)) + IP) = SCARC(NM)%NEIGHBORS(INBR)
      IP = IP + 1
   ENDDO
ENDDO

IF (N_MPI_PROCESSES > 1) &
   CALL MPI_ALLGATHERV(MPI_IN_PLACE,1,MPI_INTEGER,BUFFER_INT,COUNTS_NBR,DISPLS_NBR, MPI_INTEGER,MPI_COMM_WORLD,IERROR)
MAX_NBR = MAXVAL(SUB%N_NEIGHBORS)

CALL SCARC_ALLOCATE_INT2 (SUB%NEIGHBORS, 1, MAX_NBR, 1, NMESHES,  NSCARC_INIT_ZERO, 'SUB%NEIGHBORS', CROUTINE)

DO NM = 1, NMESHES
   DO INBR = 1, SUB%N_NEIGHBORS(NM)
      SUB%NEIGHBORS(INBR, NM) = BUFFER_INT(DISPLS_NBR(PROCESS(NM)) + INBR)
   ENDDO
ENDDO

CALL SCARC_DEALLOCATE_INT1(COUNTS_NBR, 'COUNTS_NBR', CROUTINE)
CALL SCARC_DEALLOCATE_INT1(DISPLS_NBR, 'DISPLS_NBR', CROUTINE)
CALL SCARC_DEALLOCATE_INT1(BUFFER_INT, 'BUFFER_INT', CROUTINE)

END SUBROUTINE SCARC_SETUP_SUBDIVISION


! ------------------------------------------------------------------------------------------------
!> \brief Setup neighborship structure for data exchanges along mesh interfaces
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_NEIGHBORS
USE SCARC_POINTERS, ONLY: OS, OLF, OLC
INTEGER :: NM, NOM, NL

!> Setup information about global numbers of unknowns 
CALL SCARC_SETUP_DIMENSIONS(NLEVEL_MIN)

! Initialize level structures on neighboring meshes
LEVEL_MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   LEVEL_NEIGHBOR_LOOP: DO NOM = 1, NMESHES



      ! On finest level point to exchange structures from surrounding FDS 
 
      IF (.NOT. ARE_NEIGHBORS(NM, NOM)) CYCLE LEVEL_NEIGHBOR_LOOP

      N_EXCHANGES = N_EXCHANGES+1                                         ! count number of exchanges

      OS => SCARC(NM)%OSCARC(NOM)
      ALLOCATE (OS%LEVEL(NLEVEL_MIN:NLEVEL_MAX), STAT=IERROR)             ! allocate neighboring structures
      CALL CHKMEMERR ('SCARC_SETUP_NEIGHBORS', 'OS%LEVEL', IERROR)

      OLF => SCARC(NM)%OSCARC(NOM)%LEVEL(NLEVEL_MIN)                      ! point to neighbor on finest grid level

      OLF%NX = MESHES(NOM)%IBAR                                           ! number of cells in x-direction on neighbor
      OLF%NY = MESHES(NOM)%JBAR                                           ! number of cells in y-direction on neighbor
      OLF%NZ = MESHES(NOM)%KBAR                                           ! number of cells in z-direction on neighbor

      OLF%N_WALL_CELLS_EXT = MESHES(NOM)%N_EXTERNAL_WALL_CELLS            ! number of external wall cells on neighbor
      OLF%N_WALL_CELLS_INT = MESHES(NOM)%N_INTERNAL_WALL_CELLS            ! number of external wall cells on neighbor
      OLF%N_WALL_CELLS     = OLF%N_WALL_CELLS_EXT + OLF%N_WALL_CELLS_INT  ! number of walls cell on neighbor

      OLF%N_CELLS = OLF%NX*OLF%NY*OLF%NZ                                  ! number of cells on neighbor (structured)

 
      ! In case of GMG with a predefined grid hierarchy define corresponding level-structures
 
      IF (NLEVEL_MAX > NLEVEL_MIN) THEN     

         DO NL=NLEVEL_MIN+1,NLEVEL_MAX

            OLC => SCARC(NM)%OSCARC(NOM)%LEVEL(NL)                        ! OLF points to finer, OLC to coarser level

            OLC%NX = OLF%NX/2                                             ! use double grid width
            IF (TWO_D) THEN 
               OLC%NY = 1
            ELSE 
               OLC%NY = OLF%NY/2
            ENDIF
            OLC%NZ = OLF%NZ/2

            OLC%N_CELLS          = OLC%NX * OLC%NY * OLC%NZ               ! set new number of cells
            OLC%N_WALL_CELLS     = OLC%N_WALL_CELLS_EXT                   ! set new number of wall cells
            OLC%N_WALL_CELLS_EXT = 2 * (OLC%NX*OLC%NZ + OLC%NX*OLC%NY + OLC%NY*OLC%NZ)    ! TODO: CHECK!

         ENDDO
      ENDIF

   ENDDO LEVEL_NEIGHBOR_LOOP
ENDDO LEVEL_MESHES_LOOP

END SUBROUTINE SCARC_SETUP_NEIGHBORS


! ----------------------------------------------------------------------------------------------------
!> \brief Determine basic data for single faces (orientation, dimensions, numbers)
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_FACE_BASICS(L)
USE SCARC_POINTERS, ONLY: F
TYPE (SCARC_LEVEL_TYPE), POINTER, INTENT(IN) :: L
INTEGER:: IOR0

FACES_OF_MESH_LOOP: DO IOR0 = -3, 3

   IF (IOR0 == 0) CYCLE FACES_OF_MESH_LOOP

   F => L%FACE(IOR0)
   
   SELECT CASE (ABS(IOR0))

      ! ---------- Faces in x-direction
 
      CASE (1)

         F%NOP =  L%NX                           ! number of cells between opposite mesh faces

         F%NX  =  1                              ! number of cells in x-direction
         F%NY  =  L%NY                           ! number of cells in y-direction
         F%NZ  =  L%NZ                           ! number of cells in z-direction

         F%NCW =  L%NY*L%NZ                      ! number of wall cells at that face
         F%DH  => L%DXL                          ! step size vector between opposite mesh faces

         F%INCR_BOUNDARY  = L%DXI2               ! contribution due to boundary condition 
         F%SCAL_DIRICHLET = -2.0_EB * L%DXI2

         F%INCRY =  0
         F%INCRZ =  0
         IF (IOR0 > 0) THEN
            F%SCAL_NEUMANN = L%DXI
            F%INCR_FACE    = 2.0_EB/(F%DH(0)*(F%DH(0)+F%DH(1)))
            F%INCR_INSIDE  = 2.0_EB/(F%DH(1)*(F%DH(1)+F%DH(2)))
            F%INCRX =  1                           ! offset to next internal cell in that direction
         ELSE
            F%SCAL_NEUMANN = -L%DXI
            F%INCR_FACE    = 2.0_EB/(F%DH(F%NOP)  *(F%DH(F%NOP-1)+F%DH(F%NOP)))
            F%INCR_INSIDE  = 2.0_EB/(F%DH(F%NOP-1)*(F%DH(F%NOP-2)+F%DH(F%NOP-1)))
            F%INCRX = -1
         ENDIF     
         IF (TWO_D) THEN
            F%INCRS = (/ F%NY, 0, 0, 0, 0,  0, -F%NY /)
         ELSE
            F%INCRS = (/ F%NY, 1, 0, 0, 0, -1, -F%NY /)
         ENDIF
 
      ! ---------- Faces in y-direction
 
      CASE (2)

         F%NOP =  L%NY                   ! dito

         F%NX  =  L%NX
         F%NY  =  1
         F%NZ  =  L%NZ

         F%NCW =  L%NX*L%NZ
         F%DH  => L%DYL

         F%INCR_BOUNDARY  = L%DYI2
         F%SCAL_DIRICHLET = -2.0_EB * L%DYI2

         F%INCRX =  0                           ! offset to next internal cell in that direction
         F%INCRY =  0
         F%INCRZ =  0
         IF (IOR0>0) THEN
            F%SCAL_NEUMANN = L%DYI
            IF (.NOT.TWO_D) THEN
               F%INCR_FACE   = 2.0_EB/(F%DH(0)*(F%DH(0)+F%DH(1)))
               F%INCR_INSIDE = 2.0_EB/(F%DH(1)*(F%DH(1)+F%DH(2)))
               F%INCRY = 1
            ENDIF
         ELSE
            F%SCAL_NEUMANN = -L%DYI
            IF (.NOT.TWO_D) THEN
               F%INCR_FACE   =  2.0_EB/(F%DH(F%NOP)  *(F%DH(F%NOP-1)+F%DH(F%NOP)))
               F%INCR_INSIDE =  2.0_EB/(F%DH(F%NOP-1)*(F%DH(F%NOP-2)+F%DH(F%NOP-1)))
               F%INCRY = -1
            ENDIF
         ENDIF
         IF (TWO_D) THEN
            F%INCRS = (/ 0   , 0, 0, 0,  0, 0, 0     /)             ! special case, not used
         ELSE
            F%INCRS = (/ F%NX, 0, 1, 0, -1, 0, -F%NX /)
         ENDIF

      ! ---------- Faces in z-direction
 
      CASE (3)

         F%NOP =  L%NZ                   ! dito

         F%NX  =  L%NX
         F%NY  =  L%NY
         F%NZ  =  1

         F%NCW =  L%NX*L%NY
         F%DH  => L%DZL

         F%NX  = L%NX
         F%INCR_BOUNDARY  = L%DZI2
         F%SCAL_DIRICHLET = -2.0_EB * L%DZI2

         F%INCRX =  0
         F%INCRY =  0
         IF (IOR0>0) THEN
            F%SCAL_NEUMANN = L%DZI
            F%INCR_FACE    = 2.0_EB/(F%DH(0)*(F%DH(0)+F%DH(1)))
            F%INCR_INSIDE  = 2.0_EB/(F%DH(1)*(F%DH(1)+F%DH(2)))
            F%INCRZ =  1
         ELSE
            F%SCAL_NEUMANN = -L%DZI
            F%INCR_FACE    =  2.0_EB/(F%DH(F%NOP)  *(F%DH(F%NOP-1)+F%DH(F%NOP)))
            F%INCR_INSIDE  =  2.0_EB/(F%DH(F%NOP-1)*(F%DH(F%NOP-2)+F%DH(F%NOP-1)))
            F%INCRZ = -1
         ENDIF
         IF (TWO_D) THEN
            F%INCRS = (/ 0, 0   , 1, 0, -1,     0, 0 /)
         ELSE
            F%INCRS = (/ 0, F%NX, 1, 0, -1, -F%NX, 0 /)
         ENDIF

   END SELECT

ENDDO FACES_OF_MESH_LOOP

END SUBROUTINE SCARC_SETUP_FACE_BASICS


! ----------------------------------------------------------------------------------------------------
!> \brief Setup wall related structures and boundary conditions
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_WALLS
USE SCARC_POINTERS, ONLY: M, L, LF, LC, FF, FC, OL, OLF, OLC, G, GC, GF, OG, OGC, OGF, GWC, MWC, EWC, &
                          SCARC_POINT_TO_GRID, SCARC_POINT_TO_OTHER_GRID, SCARC_POINT_TO_MULTIGRID, &  
                          SCARC_POINT_TO_OTHER_MULTIGRID
INTEGER :: NL, NM, NOM
INTEGER :: IREFINE, IFACE, IOR0, JOR0, INBR, IWG, IWC, ICW, IW
LOGICAL :: IS_KNOWN(-3:3), IS_DIRIC, IS_OPEN

CROUTINE = 'SCARC_SETUP_WALLS'
 
! -------- Get dimensionings for wall cells
 
MESHES_LOOP1: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NLEVEL_MIN)

 
   ! First loop over external wall cells:
   ! Determine number of adajacent neighbors to each face with corresponding number of IW's
   ! Store neighbors, orientation and number of couplings for a single wall cell
 
   EXTERNAL_WALL_CELLS_LOOP1: DO IWG = 1, L%N_WALL_CELLS_EXT

      MWC => M%WALL(IWG)
      EWC => M%EXTERNAL_WALL(IWG)

      NOM  =  EWC%NOM
      IOR0 =  MWC%ONE_D%IOR

      GWC => G%WALL(IWG)
      GWC%NOM  = NOM                                    ! store number of neighbor in wall cell
      GWC%IOR  = IOR0                                   ! store orientation of that cell

      IF (NOM /= 0) THEN

         IS_KNOWN = .FALSE.
         DO JOR0 = -3, 3
            IF (JOR0 == 0) CYCLE
            DO INBR = 1, L%FACE(JOR0)%N_NEIGHBORS
               IF (L%FACE(JOR0)%NEIGHBORS(INBR) == NOM) THEN
                  IS_KNOWN(JOR0) = .TRUE.
                  EXIT
               ENDIF
            ENDDO
         ENDDO

         G%NCE  = G%NCE  + 1                                                ! increase number of extended grid cells
         IF (HAS_AMG_LEVELS) G%NCE2 = G%NCE2 + 2                            ! increase number of extended grid cells type2
         CALL SCARC_POINT_TO_OTHER_GRID(NM, NOM, NLEVEL_MIN)

         IF (ANY(IS_KNOWN)) OG%NCG = OG%NCG + 1                             ! increase counter for local ghost cells
         IF (OL%GHOST_FIRSTW(IOR0) == 0) OL%GHOST_FIRSTW(IOR0) = OG%NCG     ! save first ghost cell for -IOR0
         IF (OL%GHOST_FIRSTE(IOR0) == 0) OL%GHOST_FIRSTE(IOR0) = OG%NCG     ! save first extended cell for -IOR0
         OL%GHOST_LASTW(IOR0) = OG%NCG                                     
         OL%GHOST_LASTE(IOR0) = OG%NCG                                     

      ENDIF

   ENDDO EXTERNAL_WALL_CELLS_LOOP1
   IF (HAS_AMG_LEVELS) G%ICE2 = G%NCE                                       ! initialize counter for second layer ghost cells

 
   ! Then process internal wall cells
 
   INTERNAL_WALL_CELLS_LOOP1: DO IWG = L%N_WALL_CELLS_EXT+1, L%N_WALL_CELLS_EXT+L%N_WALL_CELLS_INT

      MWC => M%WALL(IWG)
      GWC => G%WALL(IWG)

      GWC%IOR  = MWC%ONE_D%IOR
      GWC%NOM  = 0

      GWC%BTYPE = NEUMANN
      GWC%BOUNDARY_TYPE = M%WALL(IWG)%BOUNDARY_TYPE

      GWC%IXG =  MWC%ONE_D%II                        ! ghost cell indices
      GWC%IYG =  MWC%ONE_D%JJ
      GWC%IZG =  MWC%ONE_D%KK

      GWC%IXW =  MWC%ONE_D%IIG                       ! (internal) wall cell indices
      GWC%IYW =  MWC%ONE_D%JJG
      GWC%IZW =  MWC%ONE_D%KKG

   ENDDO INTERNAL_WALL_CELLS_LOOP1

 
   ! Allocate corresponding pointer arrays for data exchanges with neighbors
 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'TYPE_GRID =', TYPE_GRID,': SETTING ICE_TO_IWG in length ', G%NCE
#endif
   IF (G%NCE > G%NC) THEN
      G%N_FINE = G%NCE
      CALL SCARC_ALLOCATE_INT1 (G%ICE_TO_IWG, G%NC+1, G%NCE, NSCARC_INIT_ZERO, 'G%ICE_TO_IWG', CROUTINE)
      CALL SCARC_ALLOCATE_INT1 (G%ICE_TO_ICN, G%NC+1, G%NCE, NSCARC_INIT_ZERO, 'G%ICE_TO_IWG', CROUTINE)
   ENDIF

   FACE_NEIGHBORS_LOOP: DO IOR0 = -3, 3

      IF (IOR0 == 0) CYCLE FACE_NEIGHBORS_LOOP
      DO INBR = 1, L%FACE(IOR0)%N_NEIGHBORS

         NOM = L%FACE(IOR0)%NEIGHBORS(INBR)
         CALL SCARC_POINT_TO_OTHER_GRID(NM, NOM, NLEVEL_MIN)

         CALL SCARC_ALLOCATE_INT1 (OG%ICG_TO_IWG, 1, OG%NCG, NSCARC_INIT_ZERO, 'OG%ICG_TO_IWG', CROUTINE)

         IF (HAS_AMG_LEVELS) THEN
            CALL SCARC_ALLOCATE_INT2 (OG%ICG_TO_ICW, 1, OG%NCG, 1, 2, NSCARC_INIT_ZERO, 'OG%ICG_TO_ICW', CROUTINE)
            CALL SCARC_ALLOCATE_INT2 (OG%ICG_TO_ICE, 1, OG%NCG, 1, 2, NSCARC_INIT_ZERO, 'OG%ICG_TO_ICE', CROUTINE)
         ELSE
            CALL SCARC_ALLOCATE_INT2 (OG%ICG_TO_ICW, 1, OG%NCG, 1, 1, NSCARC_INIT_ZERO, 'OG%ICG_TO_ICW', CROUTINE)
            CALL SCARC_ALLOCATE_INT2 (OG%ICG_TO_ICE, 1, OG%NCG, 1, 1, NSCARC_INIT_ZERO, 'OG%ICG_TO_ICE', CROUTINE)
         ENDIF

      ENDDO

   ENDDO FACE_NEIGHBORS_LOOP

 
   ! Second loop over external wall cells:
   ! Store detailed coordinate and cell data and get type of boundary condition
 
   G%ICE = G%NC
   WALL_CELLS_LOOP2: DO IWG = 1, L%N_WALL_CELLS_EXT

      IOR0 = G%WALL(IWG)%IOR
      NOM  = G%WALL(IWG)%NOM

      MWC => M%WALL(IWG)
      EWC => M%EXTERNAL_WALL(IWG)

 
      ! Preset ScaRC's boundary type indicator BTYPE
      ! INTERNAL  : the global Poisson problem is solved, so no BC's along mesh interfaces are needed
      ! DIRICHLET : - in the structured case face-wise BC-settings are used ccording to original FFT-solver
      !               (this also allows to use FFT as local preconditioner)
      !             - in the unstructured case Dirichlet BC's are only used for open boundary cells
      ! NEUMANN   : is used for the rest
 
      IS_DIRIC = MWC%PRESSURE_BC_INDEX == DIRICHLET
      IS_OPEN  = MWC%BOUNDARY_TYPE     == OPEN_BOUNDARY

      GWC => G%WALL(IWG)

      IF (EWC%NOM /= 0) THEN
         GWC%BTYPE = INTERNAL
      ELSE IF ((IS_STRUCTURED .AND. IS_DIRIC) .OR. (IS_UNSTRUCTURED .AND. IS_OPEN)) THEN
         GWC%BTYPE = DIRICHLET
         G%N_DIRIC = G%N_DIRIC + 1
      ELSE
         GWC%BTYPE = NEUMANN
         G%N_NEUMANN = G%N_NEUMANN + 1
      ENDIF

      GWC%BOUNDARY_TYPE = MWC%BOUNDARY_TYPE

      GWC%IXG = MWC%ONE_D%II                                 ! ghost cell indices
      GWC%IYG = MWC%ONE_D%JJ
      GWC%IZG = MWC%ONE_D%KK

      GWC%IXW = MWC%ONE_D%IIG                                ! (internal) wall cell indices
      GWC%IYW = MWC%ONE_D%JJG
      GWC%IZW = MWC%ONE_D%KKG

      ! If there exists a neighbor for that wall cell, setup corresponding neighborship information
      IF (NOM /= 0) CALL SCARC_SETUP_WALL_NEIGHBOR(G, OG, &
                                                   EWC%IIO_MIN, EWC%IIO_MAX, &
                                                   EWC%JJO_MIN, EWC%JJO_MAX, &
                                                   EWC%KKO_MIN, EWC%KKO_MAX, &
                                                   IWG, NM, NOM, NLEVEL_MIN)

   ENDDO WALL_CELLS_LOOP2

ENDDO MESHES_LOOP1

 
! Set dimensions on finest level for requested type(s) of discretization
! and mapping from local to global cell numbering
 
CALL SCARC_SETUP_DIMENSIONS(NLEVEL_MIN)

 
! -------- For multi-level variants get discretization information and dimensions on coarser levels
 
DO NL = NLEVEL_MIN+1, NLEVEL_MAX
   CALL SCARC_SETUP_GRID_LEVEL(NL)
   CALL SCARC_SETUP_DIMENSIONS(NL)
ENDDO

 
! -------- Check whether there are no Dirichlet BC's available - TODO: Check !!!
 
MESH_INT = 0                            
RANK_INT = 0

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   CALL SCARC_POINT_TO_GRID (NM, NLEVEL_MIN)
   MESH_INT(NM) = G%N_DIRIC   
   RANK_INT = RANK_INT + MESH_INT(NM)
ENDDO

IF (N_MPI_PROCESSES>1) &
   CALL MPI_ALLREDUCE(MPI_IN_PLACE, RANK_INT, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, IERROR)
N_DIRIC_GLOBAL(NLEVEL_MIN) = RANK_INT

IS_PURE_NEUMANN = N_DIRIC_GLOBAL(NLEVEL_MIN) == 0 .AND. &
                  (TYPE_PRECON /= NSCARC_RELAX_FFT .OR. TYPE_PRECON /= NSCARC_RELAX_FFTO)


 
! -------- Only for multi-level variants 
! (twolevel-CG or GMG method as main solver or preconditioner):
! Determine WALL, FACE and OSCARC types for coarser levels
 
MULTI_LEVEL_IF: IF (HAS_GMG_LEVELS) THEN

   MESHES_LOOP3: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

      IREFINE=1
      MULTI_LEVELS_LOOP: DO NL = NLEVEL_MIN+1, NLEVEL_MAX

         CALL SCARC_POINT_TO_MULTIGRID(NM, NL-1, NL)

         IREFINE=IREFINE*2
         IF (IS_GMG) CALL SCARC_CHECK_DIVISIBILITY(GF%NCE-GF%NC, 'GF%NCE')

         ! Initialize counts for overlapping and wall cells
 
         GC%NCE = GC%NC + (GF%NCE-GF%NC)/2
         GC%ICE = GC%NC

         LC%N_WALL_CELLS_EXT = SCARC_COUNT_EXTERNAL_WALL_CELLS(LF, LC, GF)
         LC%N_WALL_CELLS_INT = SCARC_COUNT_INTERNAL_WALL_CELLS(LF, LC, GC)

         LC%N_WALL_CELLS = LC%N_WALL_CELLS_EXT + LC%N_WALL_CELLS_INT

         SELECT CASE(TYPE_GRID)
            CASE (NSCARC_GRID_STRUCTURED)
               GC%NW = LC%N_WALL_CELLS_EXT
            CASE (NSCARC_GRID_UNSTRUCTURED)
               GC%NW = LC%N_WALL_CELLS_EXT + LC%N_WALL_CELLS_INT
         END SELECT

         ALLOCATE(GC%WALL(LC%N_WALL_CELLS), STAT=IERROR)
         CALL ChkMemErr('SCARC_SETUP_NEIGHBORS','WALL',IERROR)

         ! First allocate administrative mapping arrays for own mesh if there is an overlap
 
         IF (GC%NCE > GC%NC) THEN
            CALL SCARC_ALLOCATE_INT1 (GC%ICE_TO_IWG , GC%NC+1, GC%NCE, NSCARC_INIT_ZERO, 'GC%ICE_TO_IWG', CROUTINE)
            CALL SCARC_ALLOCATE_INT1 (GC%ICE_TO_ICN , GC%NC+1, GC%NCE, NSCARC_INIT_ZERO, 'GC%ICE_TO_ICN', CROUTINE)
         ENDIF

         ! Setup basic face information for coarser mesh
 
         CALL SCARC_SETUP_FACE_BASICS(LC)

         IWC = 1
         IWG = 1
         ICW = 1
         FACES_LOOP: DO IFACE = 1, 6

            IOR0 = FACE_ORIENTATION(IFACE)

            FF => LF%FACE(IOR0)
            FC => LC%FACE(IOR0)

            ! initialize FACE type for coarser mesh
            FC%NCW0 = ICW
            FC%N_NEIGHBORS = FF%N_NEIGHBORS

            IF (FC%N_NEIGHBORS /= 0) &
               CALL SCARC_ALLOCATE_INT1(FC%NEIGHBORS, 1, FC%N_NEIGHBORS, NSCARC_INIT_NONE, 'FC%FACE_NEIGHBORS', CROUTINE)
            DO INBR= 1, FC%N_NEIGHBORS
               FC%NEIGHBORS(INBR) = FF%NEIGHBORS(INBR)
            ENDDO
            
            FC%NCW = FC%NX * FC%NY * FC%NZ                                ! get number of wall cells for that face
            ICW = ICW + FC%NCW                                            ! increase global wall cell counter

            ! Get related data and pointer structures for every mesh neighbor

            IF (LF%FACE(IOR0)%N_NEIGHBORS /= 0) THEN
               DO INBR = 1, LF%FACE(IOR0)%N_NEIGHBORS

                  NOM = LF%FACE(IOR0)%NEIGHBORS(INBR)
                  CALL SCARC_POINT_TO_OTHER_MULTIGRID(NM, NOM, NL-1, NL)

                  IF (IS_GMG) THEN
                     CALL SCARC_CHECK_DIVISIBILITY(OLF%N_WALL_CELLS_LOCAL, 'OLF%N_WALL_CELLS_LOCAL')
                     CALL SCARC_CHECK_DIVISIBILITY(OGF%NCG, 'OGF%NCG')
                  ENDIF

                  IF (.NOT.TWO_D) THEN
                     OLC%N_WALL_CELLS_LOCAL = OLF%N_WALL_CELLS_LOCAL/4
                     OGC%NCG = OGF%NCG/4
                  ELSE
                     OLC%N_WALL_CELLS_LOCAL = OLF%N_WALL_CELLS_LOCAL/2
                     OGC%NCG = OGF%NCG/2
                  ENDIF

                  CALL SCARC_SETUP_EXCHANGE_DIMENSIONS(GC, OGC, NOM, IREFINE)

                  CALL SCARC_ALLOCATE_INT1(OGC%ICG_TO_IWG, 1, OGC%NCG, NSCARC_INIT_ZERO, 'OGC%ICG_TO_IWG', CROUTINE)
                  CALL SCARC_ALLOCATE_INT2(OGC%ICG_TO_ICW, 1, OGC%NCG, 1, 1, NSCARC_INIT_ZERO, 'OGC%ICG_TO_ICW', CROUTINE)
                  CALL SCARC_ALLOCATE_INT2(OGC%ICG_TO_ICE, 1, OGC%NCG, 1, 1, NSCARC_INIT_ZERO, 'OGC%ICG_TO_ICE', CROUTINE)

               ENDDO
            ENDIF

            ! Setup complete wall information on coarser mesh level

            CALL SCARC_SETUP_WALL_LEVEL(LF, LC, GF, GC, IOR0, IWC, IREFINE, NM, NL)

         ENDDO FACES_LOOP

         CALL SCARC_SETUP_CELL_INDEX (LC, M, NL)
         CALL SCARC_SETUP_WALL_COORDS(LC, GC)
         CALL SCARC_SETUP_WALL_INDEX (LC, GC, M, NL)

         ! Setup order in which ghost cells are processed during data exchanges

         WALLCELLS_LOOP: DO IW = 1, LC%N_WALL_CELLS
         
           NOM = GC%WALL(IW)%NOM
           IF (NOM /= 0) THEN
              IOR0 = GC%WALL(IW)%IOR
              CALL SCARC_POINT_TO_OTHER_MULTIGRID(NM, NOM, NL-1, NL)
              OGC%ICG2 = OGC%ICG2 + 1
              IF (OLC%GHOST_FIRSTW(IOR0) == 0) OLC%GHOST_FIRSTW(IOR0) = OGC%ICG2
              IF (OLC%GHOST_FIRSTE(IOR0) == 0) OLC%GHOST_FIRSTE(IOR0) = OGC%ICG2
              OLC%GHOST_LASTW(IOR0) = OGC%ICG2 
              OLC%GHOST_LASTE(IOR0) = OGC%ICG2 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'NL, IOR0, NOM, FIRSTW, LASTW, FIRSTE, LASTE:', NL, IOR0, NOM, &
                      OLC%GHOST_FIRSTW(IOR0),OLC%GHOST_LASTW(IOR0),OLC%GHOST_FIRSTE(IOR0),OLC%GHOST_LASTE(IOR0)
#endif
           ENDIF
         
         ENDDO WALLCELLS_LOOP

      ENDDO MULTI_LEVELS_LOOP
   ENDDO MESHES_LOOP3
ENDIF MULTI_LEVEL_IF


! Correct boundary types for cells adjacent to obstructions on ghost cells

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   CALL SCARC_POINT_TO_GRID(NM, NLEVEL_MIN)                  ! sets level and grid pointers L and G
   IF (IS_UNSTRUCTURED) THEN
      CALL SCARC_IDENTIFY_INTERNAL_NEUMANNS(L, G)
      IF (.NOT.HAS_AMG_LEVELS) THEN
         DO NL = NLEVEL_MIN+1, NLEVEL_MAX
            CALL SCARC_IDENTIFY_INTERNAL_NEUMANNS(L, G)
         ENDDO
      ENDIF
   ENDIF
ENDDO

! Debug FACE, WALL and DISCRET structures - only if directive SCARC_DEBUG is set

#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_QUANTITY (NSCARC_DEBUG_STACK, NLEVEL_MIN, 'STACK')
CALL SCARC_DEBUG_QUANTITY (NSCARC_DEBUG_FACE , NLEVEL_MIN, 'FACE')
CALL SCARC_DEBUG_QUANTITY (NSCARC_DEBUG_WALL , NLEVEL_MIN, 'WALL')
CALL SCARC_DEBUG_QUANTITY (NSCARC_DEBUG_GRID , NLEVEL_MIN, 'DISCRET')
#endif

END SUBROUTINE SCARC_SETUP_WALLS


! -----------------------------------------------------------------------------------------
!> \brief Store all neighbors of a mesh
! -----------------------------------------------------------------------------------------
SUBROUTINE SCARC_STORE_NEIGHBOR(NM, NOM)
INTEGER, INTENT(IN) :: NM, NOM
INTEGER :: INBR
DO INBR = 1, SCARC(NM)%N_NEIGHBORS
   IF (SCARC(NM)%NEIGHBORS(INBR) == NSCARC_UNDEF_INT) EXIT      ! not found, to be stored
   IF (SCARC(NM)%NEIGHBORS(INBR) == NOM) RETURN                 ! nothing to do, already stored
ENDDO
SCARC(NM)%NEIGHBORS(INBR) = NOM
RETURN
END SUBROUTINE SCARC_STORE_NEIGHBOR


! -----------------------------------------------------------------------------------------
!> \brief Setup cells indexing array on coarser grid levels in case of MG method
! -----------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_CELL_INDEX(L, M, NL)
USE SCARC_POINTERS, ONLY: OB
TYPE (MESH_TYPE), POINTER, INTENT(IN) :: M
TYPE (SCARC_LEVEL_TYPE), POINTER, INTENT(IN) :: L
INTEGER, INTENT(IN) :: NL
INTEGER :: I, J, K, NOBST

CROUTINE = 'SCARC_SETUP_CELL_INDEX'

! If finest level, the corresponding CELL_INDEX array is already available by surrounding routines
! on coarser levels, it must still be computed

IF (NL == NLEVEL_MIN) THEN

   L%CELL_INDEX_PTR => M%CELL_INDEX

ELSE

   CALL SCARC_ALLOCATE_INT3(L%CELL_INDEX, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'L%CELL_INDEX', CROUTINE)
   L%CELL_INDEX_PTR => L%CELL_INDEX
   L%N_CELL_INDEX = 0

   ! Preset it for all grid cells
 
   DO K=0,L%NZ+1
      DO J=0,L%NY+1
         DO I=0,1
            IF (L%CELL_INDEX(I,J,K)==0) THEN
               L%N_CELL_INDEX = L%N_CELL_INDEX + 1
               L%CELL_INDEX(I,J,K) = L%N_CELL_INDEX
            ENDIF
         ENDDO
         DO I=L%NX,L%NX+1
            IF (L%CELL_INDEX(I,J,K)==0) THEN
               L%N_CELL_INDEX = L%N_CELL_INDEX + 1
               L%CELL_INDEX(I,J,K) = L%N_CELL_INDEX
            ENDIF
         ENDDO
      ENDDO
   ENDDO

   DO K=0,L%NZ+1
      DO I=0,L%NX+1
         DO J=0,1
            IF (L%CELL_INDEX(I,J,K)==0) THEN
               L%N_CELL_INDEX = L%N_CELL_INDEX + 1
               L%CELL_INDEX(I,J,K) = L%N_CELL_INDEX
            ENDIF
         ENDDO
         DO J=L%NY,L%NY+1
            IF (L%CELL_INDEX(I,J,K)==0) THEN
               L%N_CELL_INDEX = L%N_CELL_INDEX + 1
               L%CELL_INDEX(I,J,K) = L%N_CELL_INDEX
            ENDIF
         ENDDO
      ENDDO
   ENDDO

   DO J=0,L%NY+1
      DO I=0,L%NX+1
         DO K=0,1
            IF (L%CELL_INDEX(I,J,K)==0) THEN
               L%N_CELL_INDEX = L%N_CELL_INDEX + 1
               L%CELL_INDEX(I,J,K) = L%N_CELL_INDEX
            ENDIF
         ENDDO
         DO K=L%NZ,L%NZ+1
            IF (L%CELL_INDEX(I,J,K)==0) THEN
               L%N_CELL_INDEX = L%N_CELL_INDEX + 1
               L%CELL_INDEX(I,J,K) = L%N_CELL_INDEX
            ENDIF
         ENDDO
      ENDDO
   ENDDO

 
   ! Consider cells in obstructions
 
   DO NOBST=1,L%N_OBST
      OB => L%OBST(NOBST)
      DO K=OB%K1,OB%K2+1
         DO J=OB%J1,OB%J2+1
            DO I=OB%I1,OB%I2+1
               IF (L%CELL_INDEX(I,J,K)==0) THEN
                  L%N_CELL_INDEX = L%N_CELL_INDEX + 1
                  L%CELL_INDEX(I,J,K) = L%N_CELL_INDEX
               ENDIF
            ENDDO
         ENDDO
      ENDDO
   ENDDO

ENDIF

END SUBROUTINE SCARC_SETUP_CELL_INDEX


! -----------------------------------------------------------------------------------------
!> \brief Setup wall cells indexing array on coarser grid levels
! -----------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_WALL_INDEX(L, G, M, NL)
TYPE (MESH_TYPE), POINTER, INTENT(IN) :: M
TYPE (SCARC_LEVEL_TYPE), POINTER, INTENT(IN) :: L
TYPE (SCARC_GRID_TYPE),  POINTER, INTENT(IN) :: G
INTEGER, INTENT(IN) :: NL
INTEGER :: I, J, K, ICG, IW, IOR0

CROUTINE = 'SCARC_SETUP_WALL_INDEX'

! if on finest level, the array WALL_INDEX is already available by surrounding routines
IF (NL == NLEVEL_MIN) THEN

   L%WALL_INDEX_PTR => M%WALL_INDEX

   ! if on coarser levels, it must still be computed
ELSE

   CALL SCARC_ALLOCATE_INT2(L%WALL_INDEX, 1, L%N_CELL_INDEX, -3, 3, NSCARC_INIT_ZERO, 'L%WALL_INDEX', CROUTINE)
   L%WALL_INDEX_PTR => L%WALL_INDEX

   DO IW = 1, L%N_WALL_CELLS_EXT

      I = G%WALL(IW)%IXW
      J = G%WALL(IW)%IYW
      K = G%WALL(IW)%IZW

      IOR0 = G%WALL(IW)%IOR
      ICG  = L%CELL_INDEX(I,J,K)

      L%WALL_INDEX(ICG,-IOR0) = IW

   ENDDO

ENDIF

END SUBROUTINE SCARC_SETUP_WALL_INDEX


! -------------------------------------------------------------------------------------------------
!> \brief Setup all necessary information for a wall cell with neighbor in case of MG method
! Number of obstructions on coarse level is the same as on fine level
! TODO: Only works for special cases which run for GMG, must still be extended!!
! -------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_WALL_COORDS(L, G)
USE SCARC_POINTERS, ONLY: OB, GWC
TYPE (SCARC_LEVEL_TYPE), POINTER, INTENT(IN) :: L
TYPE (SCARC_GRID_TYPE), POINTER, INTENT(IN)  :: G
INTEGER :: IC, IO, IWC
INTEGER :: I, J, K

IWC = L%N_WALL_CELLS_EXT + 1
DO IO = 1, L%N_OBST

   OB => L%OBST(IO)

   ! Analyze IOR = 1

   I = OB%I1
   DO K = OB%K1+1, OB%K2
      DO J = OB%J1+1, OB%J2
         IC = G%CELL_NUMBER(I, J, K)
         IF (IC == NSCARC_UNDEF_INT .OR. IC > G%NC) CYCLE
         GWC => G%WALL(IWC)
         GWC%IXW = I+1; GWC%IYW = J; GWC%IZW = K
         GWC%IXG = I  ; GWC%IYG = J; GWC%IZG = K
         GWC%IOR = 1
         GWC%BTYPE = NEUMANN
         GWC%BOUNDARY_TYPE = SOLID_BOUNDARY
         IWC = IWC + 1
      ENDDO
   ENDDO

   ! Analyze IOR = -1
   I = OB%I2
   DO K = OB%K1+1, OB%K2
      DO J = OB%J1+1, OB%J2
         IC = G%CELL_NUMBER(I+1, J, K)
         IF (IC == NSCARC_UNDEF_INT .OR. IC > G%NC) CYCLE
         GWC => G%WALL(IWC)
         GWC%IXW = I  ; GWC%IYW = J; GWC%IZW = K
         GWC%IXG = I+1; GWC%IYG = J; GWC%IZG = K
         GWC%IOR =-1
         GWC%BTYPE = NEUMANN
         GWC%BOUNDARY_TYPE = SOLID_BOUNDARY
         IWC = IWC + 1
      ENDDO
   ENDDO

   ! Analyze IOR = 2

   J = OB%J1
   DO K = OB%K1+1, OB%K2
      DO I = OB%I1+1, OB%I2
         IC = G%CELL_NUMBER(I, J, K)
         IF (IC == NSCARC_UNDEF_INT .OR. IC > G%NC) CYCLE
         GWC => G%WALL(IWC)
         GWC%IXW = I; GWC%IYW = J+1; GWC%IZW = K
         GWC%IXG = I; GWC%IYG = J  ; GWC%IZG = K
         GWC%IOR = 2
         GWC%BTYPE = NEUMANN
         GWC%BOUNDARY_TYPE = SOLID_BOUNDARY
         IWC = IWC + 1
      ENDDO
   ENDDO

   ! Analyze IOR = -2

   J = OB%J2
   DO K = OB%K1+1, OB%K2
      DO I = OB%I1+1, OB%I2
         IC = G%CELL_NUMBER(I, J+1, K)
         IF (IC == NSCARC_UNDEF_INT .OR. IC > G%NC) CYCLE
         GWC => G%WALL(IWC)
         GWC%IXW = I; GWC%IYW = J  ; GWC%IZW = K
         GWC%IXG = I; GWC%IYG = J+1; GWC%IZG = K
         GWC%IOR =-2
         GWC%BTYPE = NEUMANN
         GWC%BOUNDARY_TYPE = SOLID_BOUNDARY
         IWC = IWC + 1
      ENDDO
   ENDDO

   ! Analyze IOR = 3

   K = OB%K1
   DO J = OB%J1+1, OB%J2
      DO I = OB%I1+1, OB%I2
         IC = G%CELL_NUMBER(I, J, K)
         IF (IC == NSCARC_UNDEF_INT .OR. IC > G%NC) CYCLE
         GWC => G%WALL(IWC)
         GWC%IXW = I; GWC%IYW = J; GWC%IZW = K+1
         GWC%IXG = I; GWC%IYG = J; GWC%IZG = K
         GWC%IOR = 3
         GWC%BTYPE = NEUMANN
         GWC%BOUNDARY_TYPE = SOLID_BOUNDARY
         IWC = IWC + 1
      ENDDO
   ENDDO

   ! Analyze IOR = -3

   K = OB%K2
   DO J = OB%J1+1, OB%J2
      DO I = OB%I1+1, OB%I2
         IC = G%CELL_NUMBER(I, J, K+1)
         IF (IC == NSCARC_UNDEF_INT .OR. IC > G%NC) CYCLE
         GWC => G%WALL(IWC)
         GWC%IXW = I; GWC%IYW = J; GWC%IZW = K
         GWC%IXG = I; GWC%IYG = J; GWC%IZG = K+1
         GWC%IOR =-3
         GWC%BTYPE = NEUMANN
         GWC%BOUNDARY_TYPE = SOLID_BOUNDARY
         IWC = IWC + 1
      ENDDO
   ENDDO

ENDDO

END SUBROUTINE SCARC_SETUP_WALL_COORDS


! -------------------------------------------------------------------------------------------------
!> \brief Correct boundary type array related to internal obstructions on ghost cells
! -------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_IDENTIFY_INTERNAL_NEUMANNS(L, G) 
USE SCARC_POINTERS, ONLY: GWC
TYPE (SCARC_LEVEL_TYPE), POINTER, INTENT(IN) :: L
TYPE (SCARC_GRID_TYPE),  POINTER, INTENT(IN) :: G
INTEGER :: IWG
INTEGER :: IX, IY, IZ, IOR0, BTYPE0

DO IWG = 1, L%N_WALL_CELLS_EXT

   GWC => G%WALL(IWG)
   IF (GWC%NOM == 0) CYCLE                    ! TODO: equal or not equal ??

   IX = GWC%IXW
   IY = GWC%IYW
   IZ = GWC%IZW

   IOR0   = GWC%IOR
   BTYPE0 = GWC%BTYPE

   ! ICG = L%CELL_INDEX_PTR(IX, IY, IZ)
   ! IWG = L%WALL_INDEX_PTR(ICG, IOR0)
   ! IF (GWC%BOUNDARY_TYPE /= INTERPOLATED_BOUNDARY) GWC%BTYPE=NEUMANN
   ! IF (L%IS_SOLID(IX, IY, IZ)) GWC%BTYPE=NEUMANN

   IF (GWC%BOUNDARY_TYPE == SOLID_BOUNDARY) GWC%BTYPE=NEUMANN

ENDDO

END SUBROUTINE SCARC_IDENTIFY_INTERNAL_NEUMANNS


! -------------------------------------------------------------------------------------------------
!> \brief Setup all necessary information for a wall cell with neighbor
! -------------------------------------------------------------------------------------------------
INTEGER FUNCTION SCARC_COUNT_EXTERNAL_WALL_CELLS(LF, LC, GF)
TYPE (SCARC_LEVEL_TYPE), POINTER, INTENT(IN) :: LF, LC
TYPE (SCARC_GRID_TYPE),  POINTER, INTENT(IN) :: GF
INTEGER :: IXC, IYC, IZC
INTEGER :: IXF, IYF, IZF
INTEGER :: IWC, ICF(4)=0, IWF(4)=0, IOR0

ICF = 0
IWC = 0
IWF = 0

IF (TWO_D) THEN
   IYC = 1
   IYF = 1

   ! IOR = 1

   IOR0 = 1
   DO IZC = 1, LC%NZ
      IZF = 2*IZC - 1
      ICF(1) = LF%CELL_INDEX_PTR(1  , IYF  , IZF  )
      ICF(2) = LF%CELL_INDEX_PTR(1  , IYF  , IZF+1)
      IF (IS_EXTERNAL_WALLCELL(LF, GF, IOR0, ICF, 2)) IWC = IWC + 1
   ENDDO

   ! IOR = -1

   IOR0 = -1
   DO IZC = 1, LC%NZ
      IZF = 2*IZC - 1
      ICF(1) = LF%CELL_INDEX_PTR(LF%NX, IYF  , IZF  )
      ICF(2) = LF%CELL_INDEX_PTR(LF%NX, IYF  , IZF+1)
      IF (IS_EXTERNAL_WALLCELL(LF, GF, IOR0, ICF, 2)) IWC = IWC + 1
   ENDDO

   ! IOR = 2

   IOR0 = 2
   DO IZC = 1, LC%NZ
      IZF = 2*IZC - 1
      DO IXC = 1, LC%NX
         IXF = 2*IXC - 1
         ICF(1) = LF%CELL_INDEX_PTR(IXF  , IYF, IZF  )
         ICF(2) = LF%CELL_INDEX_PTR(IXF+1, IYF, IZF  )
         ICF(3) = LF%CELL_INDEX_PTR(IXF  , IYF, IZF+1)
         ICF(4) = LF%CELL_INDEX_PTR(IXF+1, IYF, IZF+1)
         IF (IS_EXTERNAL_WALLCELL(LF, GF, IOR0, ICF, 4)) IWC = IWC + 1
      ENDDO
   ENDDO

   ! IOR = -2

   IOR0 = -2
   IXF  = 1
   DO IZC = 1, LC%NZ
      IZF = 2*IZC - 1
      DO IXC = 1, LC%NX
         IXF = 2*IXC - 1
         ICF(1) = LF%CELL_INDEX_PTR(IXF    , IYF, IZF  )
         ICF(2) = LF%CELL_INDEX_PTR(IXF+1  , IYF, IZF  )
         ICF(3) = LF%CELL_INDEX_PTR(IXF    , IYF, IZF+1)
         ICF(4) = LF%CELL_INDEX_PTR(IXF+1  , IYF, IZF+1)
         IF (IS_EXTERNAL_WALLCELL(LF, GF, IOR0, ICF, 4)) IWC = IWC + 1
      ENDDO
   ENDDO

   ! IOR = 3

   IOR0 = 3
   DO IXC = 1, LC%NX
      IXF = 2*IXC - 1
      ICF(1) = LF%CELL_INDEX_PTR(IXF    , IYF  , 1)
      ICF(2) = LF%CELL_INDEX_PTR(IXF+1  , IYF  , 1)
      IF (IS_EXTERNAL_WALLCELL(LF, GF, IOR0, ICF, 2)) IWC = IWC + 1
   ENDDO

   ! IOR = -3

   IOR0 = -3
   DO IXC = 1, LC%NX
      IXF = 2*IXC - 1
      ICF(1) = LF%CELL_INDEX_PTR(IXF  , IYF  , LF%NZ)
      ICF(2) = LF%CELL_INDEX_PTR(IXF+1, IYF  , LF%NZ)
      IF (IS_EXTERNAL_WALLCELL(LF, GF, IOR0, ICF, 2)) IWC = IWC + 1
   ENDDO

ELSE

   ! IOR = 1

   IOR0 = 1
   DO IZC = 1, LC%NZ
      IZF = 2*IZC - 1
      DO IYC = 1, LC%NY
         IYF = 2*IYC - 1
         ICF(1) = LF%CELL_INDEX_PTR(1  , IYF  , IZF  )
         ICF(2) = LF%CELL_INDEX_PTR(1  , IYF+1, IZF  )
         ICF(3) = LF%CELL_INDEX_PTR(1  , IYF  , IZF+1)
         ICF(4) = LF%CELL_INDEX_PTR(1  , IYF+1, IZF+1)
         IF (IS_EXTERNAL_WALLCELL(LF, GF, IOR0, ICF, 4)) IWC = IWC + 1
      ENDDO
   ENDDO

   ! IOR = -1

   IOR0 = -1
   DO IZC = 1, LC%NZ
      IZF = 2*IZC - 1
      DO IYC = 1, LC%NY
         IYF = 2*IYC - 1
         ICF(1) = LF%CELL_INDEX_PTR(LF%NX, IYF  , IZF  )
         ICF(2) = LF%CELL_INDEX_PTR(LF%NX, IYF+1, IZF  )
         ICF(3) = LF%CELL_INDEX_PTR(LF%NX, IYF  , IZF+1)
         ICF(4) = LF%CELL_INDEX_PTR(LF%NX, IYF+1, IZF+1)
         IF (IS_EXTERNAL_WALLCELL(LF, GF, IOR0, ICF, 4)) IWC = IWC + 1
      ENDDO
   ENDDO

   ! IOR = 2

   IOR0 = 2
   DO IZC = 1, LC%NZ
      IZF = 2*IZC - 1
      DO IXC = 1, LC%NX
         IXF = 2*IXC - 1
         ICF(1) = LF%CELL_INDEX_PTR(IXF  , 1, IZF  )
         ICF(2) = LF%CELL_INDEX_PTR(IXF+1, 1, IZF  )
         ICF(3) = LF%CELL_INDEX_PTR(IXF  , 1, IZF+1)
         ICF(4) = LF%CELL_INDEX_PTR(IXF+1, 1, IZF+1)
         IF (IS_EXTERNAL_WALLCELL(LF, GF, IOR0, ICF, 4)) IWC = IWC + 1
      ENDDO
   ENDDO

   ! IOR = -2

   IOR0 = -2
   IXF  = 1
   DO IZC = 1, LC%NZ
      IZF = 2*IZC - 1
      DO IXC = 1, LC%NX
         IXF = 2*IXC - 1
         ICF(1) = LF%CELL_INDEX_PTR(IXF    , LF%NY, IZF  )
         ICF(2) = LF%CELL_INDEX_PTR(IXF+1  , LF%NY, IZF  )
         ICF(3) = LF%CELL_INDEX_PTR(IXF    , LF%NY, IZF+1)
         ICF(4) = LF%CELL_INDEX_PTR(IXF+1  , LF%NY, IZF+1)
         IF (IS_EXTERNAL_WALLCELL(LF, GF, IOR0, ICF, 4)) IWC = IWC + 1
      ENDDO
   ENDDO

   ! IOR = 3

   IOR0 = 3
   DO IYC = 1, LC%NY
      IYF = 2*IYC - 1
      DO IXC = 1, LC%NX
         IXF = 2*IXC - 1
         ICF(1) = LF%CELL_INDEX_PTR(IXF    , IYF  , 1)
         ICF(2) = LF%CELL_INDEX_PTR(IXF+1  , IYF  , 1)
         ICF(3) = LF%CELL_INDEX_PTR(IXF    , IYF+1, 1)
         ICF(4) = LF%CELL_INDEX_PTR(IXF+1  , IYF+1, 1)
         IF (IS_EXTERNAL_WALLCELL(LF, GF, IOR0, ICF, 4)) IWC = IWC + 1
      ENDDO
   ENDDO

   ! IOR = -3

   IOR0 = -3
   DO IYC = 1, LC%NY
      IYF = 2*IYC - 1
      DO IXC = 1, LC%NX
         IXF = 2*IXC - 1
         ICF(1) = LF%CELL_INDEX_PTR(IXF  , IYF  , LF%NZ)
         ICF(2) = LF%CELL_INDEX_PTR(IXF+1, IYF  , LF%NZ)
         ICF(3) = LF%CELL_INDEX_PTR(IXF  , IYF+1, LF%NZ)
         ICF(4) = LF%CELL_INDEX_PTR(IXF+1, IYF+1, LF%NZ)
         IF (IS_EXTERNAL_WALLCELL(LF, GF, IOR0, ICF, 4)) IWC = IWC + 1
      ENDDO
   ENDDO

ENDIF

SCARC_COUNT_EXTERNAL_WALL_CELLS = IWC
END FUNCTION SCARC_COUNT_EXTERNAL_WALL_CELLS


! -------------------------------------------------------------------------------------------------
!> \brief Setup all necessary information for a wall cell with neighbor
! -------------------------------------------------------------------------------------------------
INTEGER FUNCTION SCARC_COUNT_INTERNAL_WALL_CELLS(LF, LC, GC)
USE SCARC_POINTERS, ONLY: OBF, OBC
TYPE (SCARC_LEVEL_TYPE), POINTER, INTENT(IN) :: LF, LC
TYPE (SCARC_GRID_TYPE),  POINTER, INTENT(IN) :: GC
INTEGER :: IWC, NW_INT
INTEGER :: IC, IO
INTEGER :: I, J, K

LC%N_OBST = LF%N_OBST                   ! Number of obstructions is the same on all levels

ALLOCATE(LC%OBST(LC%N_OBST), STAT=IERROR)
CALL ChkMemErr('SCARC_COUNT_INTERNAL_WALL_CELLS','OBST',IERROR)

NW_INT = 0
IWC = LC%N_WALL_CELLS_EXT + 1

DO IO = 1, LF%N_OBST

   OBF => LF%OBST(IO)
   OBC => LC%OBST(IO)

   OBC%I1 = (OBF%I1+1)/2
   OBC%I2 =  OBF%I2/2

   IF (TWO_D) THEN
      OBC%J1 = 0
      OBC%J2 = 1
   ELSE
      OBC%J1 = (OBF%J1+1)/2
      OBC%J2 =  OBF%J2/2
   ENDIF

   OBC%K1 = (OBF%K1+1)/2
   OBC%K2 =  OBF%K2/2

   ! Analyze IOR = 1

   I = OBC%I1
   DO K = OBC%K1+1, OBC%K2
      DO J = OBC%J1+1, OBC%J2
         IC = GC%CELL_NUMBER(I, J, K)
         IF (IC == NSCARC_UNDEF_INT) CYCLE
         NW_INT = NW_INT + 1
      ENDDO
   ENDDO

   ! Analyze IOR = -1

   I = OBC%I2
   DO K = OBC%K1+1, OBC%K2
      DO J = OBC%J1+1, OBC%J2
         IC = GC%CELL_NUMBER(I+1, J, K)
         IF (IC == NSCARC_UNDEF_INT) CYCLE
         NW_INT = NW_INT + 1
      ENDDO
   ENDDO

   ! Analyze IOR = 2

   J = OBC%J1
   DO K = OBC%K1+1, OBC%K2
      DO I = OBC%I1+1, OBC%I2
         IC = GC%CELL_NUMBER(I, J, K)
         IF (IC == NSCARC_UNDEF_INT) CYCLE
         NW_INT = NW_INT + 1
      ENDDO
   ENDDO

   ! Analyze IOR = -2

   J = OBC%J2
   DO K = OBC%K1+1, OBC%K2
      DO I = OBC%I1+1, OBC%I2
         IC = GC%CELL_NUMBER(I, J+1, K)
         IF (IC == NSCARC_UNDEF_INT) CYCLE
         NW_INT = NW_INT + 1
      ENDDO
   ENDDO

   ! Analyze IOR = 3

   K = OBC%K1
   DO J = OBC%J1+1, OBC%J2
      DO I = OBC%I1+1, OBC%I2
         IC = GC%CELL_NUMBER(I, J, K)
         IF (IC == NSCARC_UNDEF_INT) CYCLE
         NW_INT = NW_INT + 1
      ENDDO
   ENDDO

   ! Analyze IOR = -3

   K = OBC%K2
   DO J = OBC%J1+1, OBC%J2
      DO I = OBC%I1+1, OBC%I2
         IC = GC%CELL_NUMBER(I, J, K+1)
         IF (IC == NSCARC_UNDEF_INT) CYCLE
         NW_INT = NW_INT + 1
      ENDDO
   ENDDO

ENDDO

SCARC_COUNT_INTERNAL_WALL_CELLS = NW_INT
END FUNCTION SCARC_COUNT_INTERNAL_WALL_CELLS


! -------------------------------------------------------------------------------------------------
!> \brief Count external wall cells on specified face if mesh
! -------------------------------------------------------------------------------------------------
LOGICAL FUNCTION IS_EXTERNAL_WALLCELL(L, G, IOR0, ICF, NCNT)
TYPE (SCARC_LEVEL_TYPE), POINTER, INTENT(IN) :: L
TYPE (SCARC_GRID_TYPE),  POINTER, INTENT(IN) :: G
INTEGER, INTENT(IN) :: IOR0, NCNT
INTEGER, DIMENSION(:), INTENT(IN) :: ICF
INTEGER :: I, IWF_LAST, IWF(4)=0
REAL(EB) :: BSUM

IS_EXTERNAL_WALLCELL = .FALSE.

DO I = 1, NCNT
   IWF(I) = L%WALL_INDEX_PTR(ICF(I), -IOR0)
ENDDO

BSUM = 0.0_EB
IWF_LAST = 0

DO I = 1, NCNT
   IF (IWF(I)>0) THEN
      BSUM = BSUM + REAL(G%WALL(IWF(I))%BTYPE,EB)
      IWF_LAST = IWF(I)
   ENDIF
ENDDO

IF (IWF_LAST == 0) RETURN
IF (ABS(BSUM/REAL(NCNT,EB) - REAL(G%WALL(IWF_LAST)%BTYPE,EB)) < 1E-12) THEN
   IS_EXTERNAL_WALLCELL = .TRUE.
   RETURN
ELSE
   CALL SCARC_SHUTDOWN(NSCARC_ERROR_BOUNDARY_SUM, SCARC_NONE, IOR0)
ENDIF

END FUNCTION IS_EXTERNAL_WALLCELL


! -------------------------------------------------------------------------------------------------
!> \brief Setup all necessary information for a wall cell with neighbor
! -------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_WALL_NEIGHBOR(G, OG, NX1, NX2, NY1, NY2, NZ1, NZ2, IWG, NM, NOM, NL)
USE SCARC_POINTERS, ONLY: GWC, SCARC_POINT_TO_OTHER_GRID
TYPE (SCARC_GRID_TYPE),  POINTER, INTENT(IN) :: G, OG
INTEGER, INTENT(IN) :: NX1, NX2, NY1, NY2, NZ1, NZ2
INTEGER, INTENT(IN) :: IWG, NM, NOM, NL
INTEGER :: NOMX, NOMY, NOMZ
INTEGER :: ICG, ICE, IX, IY, IZ, JL, IXW, IYW, IZW, IXG, IYG, IZG

CALL SCARC_POINT_TO_OTHER_GRID (NM, NOM, NL)

ICE  = G%ICE
ICG  = OG%ICG

! set neighboring coordinates
GWC => G%WALL(IWG)

GWC%IXN(1) = NX1
GWC%IXN(2) = NX2
GWC%IYN(1) = NY1
GWC%IYN(2) = NY2
GWC%IZN(1) = NZ1
GWC%IZN(2) = NZ2

NOMX = MESHES(NOM)%IBAR
NOMY = MESHES(NOM)%JBAR
NOMZ = MESHES(NOM)%KBAR

IF (NL > 1) THEN
   DO JL = 2, NL
      NOMX = MESHES(NOM)%IBAR/NL
      IF (.NOT.TWO_D) NOMY = MESHES(NOM)%JBAR/NL
      NOMZ = MESHES(NOM)%KBAR/NL
   ENDDO
ENDIF

! store information about overlapped cells and set mapping arrays
DO IZ = NZ1, NZ2
   DO IY = NY1, NY2
      DO IX = NX1, NX2

         ICG  = ICG  + 1
         ICE  = ICE  + 1

         GWC%ICE = ICE                                         ! number of extended grid cell
         GWC%ICG = ICG                                         ! number of ghost grid cell

         G%ICE_TO_IWG(ICE) = IWG                               ! map extended cell to global wall cell
         
         IXG = G%WALL(IWG)%IXG
         IYG = G%WALL(IWG)%IYG
         IZG = G%WALL(IWG)%IZG

         IXW = G%WALL(IWG)%IXW
         IYW = G%WALL(IWG)%IYW
         IZW = G%WALL(IWG)%IZW

         G%CELL_NUMBER(IXG, IYG, IZG) = ICE

         OG%ICG_TO_IWG(ICG)    = IWG                              ! map ghost cell to global wall cell
         OG%ICG_TO_ICW(ICG, 1) = G%CELL_NUMBER(IXW, IYW, IZW)     ! get cell number of adjacent internal cell

      ENDDO
   ENDDO
ENDDO

G%ICE  = ICE                                                   ! store extended cell counter
OG%ICG = ICG                                                   ! store ghost cell counter
OG%ICG_TO_IWG(ICG) = IWG                                       ! map local wall cell to global wall cell
OG%ICG_TO_ICE(ICG, 1) = ICE                                    ! map local wall cell to global wall cell

END SUBROUTINE SCARC_SETUP_WALL_NEIGHBOR


! ----------------------------------------------------------------------------------------------------
!> \brief Check divisibility by 2 of a given number of elements (in one grid direction)
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_CHECK_DIVISIBILITY(NN, CDIR)
INTEGER, INTENT(IN) :: NN
CHARACTER(*) , INTENT(IN) :: CDIR
IF (MOD(NN,2) /= 0) CALL SCARC_SHUTDOWN(NSCARC_ERROR_GRID_NUMBER, CDIR, NSCARC_NONE)
END SUBROUTINE SCARC_CHECK_DIVISIBILITY


! ----------------------------------------------------------------------------------------------------
!> \brief Set wall cell information on coarse level
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_WALL_LEVEL(LF, LC, GF, GC, IOR0, IWC, IREFINE, NM, NL)
USE SCARC_POINTERS, ONLY: FF, FC, WF, WC, OGC, SCARC_POINT_TO_OTHER_MULTIGRID
TYPE (SCARC_LEVEL_TYPE), POINTER, INTENT(IN) :: LF, LC
TYPE (SCARC_GRID_TYPE),  POINTER, INTENT(IN) :: GF, GC
INTEGER, INTENT(INOUT) :: IWC
INTEGER, INTENT(IN) :: IOR0, IREFINE, NM, NL
INTEGER :: IWF(4) , IBCF(4), NOMF(4)
INTEGER :: IX,  IY,  IZ, I
INTEGER :: NX1, NY1, NZ1
INTEGER :: NX2, NY2, NZ2
INTEGER :: IX1, IY1, IZ1
INTEGER :: IX2, IY2, IZ2
INTEGER :: IDIFF, JDIFF, KDIFF

WC => GC%WALL
WF => GF%WALL

! set coordinate dimensions for correspoding face

SELECT CASE (ABS(IOR0))
   CASE (1)
      IF (IOR0 > 0) THEN                                           ! set dimensions for wall cell counting
         NX1 = 0; NX2 = 0
      ELSE
         NX1 = LC%NX+1; NX2 = LC%NX+1
      ENDIF
      NY1 = 1;  NY2 = LC%NY
      NZ1 = 1;  NZ2 = LC%NZ
   CASE (2)
      NX1 = 1; NX2 = LC%NX
      IF (IOR0 > 0) THEN
         NY1 = 0; NY2 = 0
      ELSE
         NY1 = LC%NY+1; NY2 = LC%NY+1
      ENDIF
      NZ1 = 1; NZ2 = LC%NZ
   CASE (3)
      NX1 = 1; NX2 = LC%NX
      NY1 = 1; NY2 = LC%NY
      IF (IOR0 > 0) THEN
         NZ1 = 0; NZ2 = 0
      ELSE
         NZ1 =LC%NZ+1; NZ2 =LC%NZ+1
      ENDIF
END SELECT

 
! Loop over all wall cells of face IOR0
 
DO IZ = NZ1, NZ2
   DO IY = NY1, NY2
      DO IX = NX1, NX2

         ! Set orientation of neiboring face, indices of ghost and adjacent cell for coarse IW

         WC(IWC)%IOR = IOR0

         FF => LF%FACE(IOR0)
         FC => LC%FACE(IOR0)

         SELECT CASE (IOR0)
            CASE (1)
               WC(IWC)%ICW = (IZ-1)*LC%NX*LC%NY + (IY-1)*LC%NX + IX + 1
            CASE (-1)
               WC(IWC)%ICW = (IZ-1)*LC%NX*LC%NY + (IY-1)*LC%NX + IX - 1
            CASE (2)
               WC(IWC)%ICW = (IZ-1)*LC%NX*LC%NY +  IY   *LC%NX + IX
            CASE (-2)
               WC(IWC)%ICW = (IZ-1)*LC%NX*LC%NY + (IY-2)*LC%NX + IX
            CASE (3)
               WC(IWC)%ICW =  IZ   *LC%NX*LC%NY + (IY-1)*LC%NX + IX
            CASE (-3)
               WC(IWC)%ICW = (IZ-2)*LC%NX*LC%NY + (IY-1)*LC%NX + IX
         END SELECT

         WC(IWC)%IOR = IOR0

         WC(IWC)%IXG = IX
         WC(IWC)%IYG = IY
         WC(IWC)%IZG = IZ

         SELECT CASE (IOR0)
            CASE (1)
               WC(IWC)%IXW = IX+1
               WC(IWC)%IYW = IY
               WC(IWC)%IZW = IZ
            CASE (-1)
               WC(IWC)%IXW = IX-1
               WC(IWC)%IYW = IY
               WC(IWC)%IZW = IZ
            CASE (2)
               WC(IWC)%IXW = IX
               WC(IWC)%IYW = IY+1
               WC(IWC)%IZW = IZ
            CASE (-2)
               WC(IWC)%IXW = IX
               WC(IWC)%IYW = IY-1
               WC(IWC)%IZW = IZ
            CASE (3)
               WC(IWC)%IXW = IX
               WC(IWC)%IYW = IY
               WC(IWC)%IZW = IZ+1
            CASE (-3)
               WC(IWC)%IXW = IX
               WC(IWC)%IYW = IY
               WC(IWC)%IZW = IZ-1
         END SELECT

         ! ------------------------------------------------------------
         !  2D-version
         ! ------------------------------------------------------------
         IF (TWO_D) THEN

            ! determine fine IW's, which must be merged to one coarse IW

            SELECT CASE (ABS(IOR0))
               CASE ( 1)
                  IWF(1) = FF%NCW0 + 2*(IZ-1)
               CASE ( 2)
                  IWF(1) = FF%NCW0 + 2*(IZ-1)*LF%NX + 2*(IX - 1)
               CASE ( 3)
                  IWF(1) = FF%NCW0 + 2*(IX-1)
            END SELECT
            IWF(2) = IWF(1)+1

            ! set fine cell neighbors (they must be the same for all fine IW's)

            NOMF(1) = WF(IWF(1))%NOM
            NOMF(2) = WF(IWF(2))%NOM
            IF (NOMF(1) /= NOMF(2)) CALL SCARC_SHUTDOWN(NSCARC_ERROR_NEIGHBOR_TYPE, SCARC_NONE, NOMF(1))

            WC(IWC)%NOM = NOMF(1)

            ! set corresponding pressure_bc_index on coarser level

            IBCF(1) = WF(IWF(1))%BTYPE
            IBCF(2) = WF(IWF(2))%BTYPE
            IF (IBCF(1) == INTERNAL .OR. IBCF(2) == INTERNAL) THEN
               WC(IWC)%BTYPE = INTERNAL
            ELSE IF (IBCF(1) == DIRICHLET .OR. IBCF(2) == DIRICHLET) THEN
               WC(IWC)%BTYPE = DIRICHLET
            ELSE
               WC(IWC)%BTYPE = NEUMANN
            ENDIF

            ! set corresponding pressure_bc_index on coarser level

            IBCF(1) = WF(IWF(1))%BOUNDARY_TYPE
            IBCF(2) = WF(IWF(2))%BOUNDARY_TYPE
            IF (IBCF(1)==NULL_BOUNDARY .OR. IBCF(2)==NULL_BOUNDARY) THEN
               WC(IWC)%BOUNDARY_TYPE = NULL_BOUNDARY
            ELSE IF (IBCF(1)==SOLID_BOUNDARY .OR. IBCF(2)==SOLID_BOUNDARY) THEN
               WC(IWC)%BOUNDARY_TYPE = SOLID_BOUNDARY
            ELSE IF (IBCF(1)==OPEN_BOUNDARY .OR. IBCF(2)==OPEN_BOUNDARY) THEN
               WC(IWC)%BOUNDARY_TYPE = OPEN_BOUNDARY
            ELSE IF (IBCF(1)==INTERPOLATED_BOUNDARY .OR. IBCF(2)==INTERPOLATED_BOUNDARY) THEN
               WC(IWC)%BOUNDARY_TYPE = INTERPOLATED_BOUNDARY
            ELSE IF (IBCF(1)==MIRROR_BOUNDARY .OR. IBCF(2)==MIRROR_BOUNDARY) THEN
               WC(IWC)%BOUNDARY_TYPE = MIRROR_BOUNDARY
            ELSE
               CALL SCARC_SHUTDOWN(NSCARC_ERROR_BOUNDARY_TYPE, SCARC_NONE, IBCF(1))
            ENDIF

            ! in case of an internal boundary set neighboring wall cells

            IF (NOMF(1) > 0) THEN

               CALL SCARC_POINT_TO_OTHER_MULTIGRID (NM, NOMF(1), NL-1, NL)

               IY1 = 1
               IY2 = 1
               SELECT CASE (ABS(IOR0))
                  CASE (1)
                     KDIFF = WF(IWF(2))%IZN(1) - WF(IWF(1))%IZN(1)
                     IF (KDIFF == 1) THEN
                        IZ1 = WF(IWF(2))%IZN(2)/2
                        IZ2 = IZ1
                     ELSE IF (KDIFF == 2) THEN
                        IZ1 = WF(IWF(1))%IZN(2)/2
                        IZ2 = WF(IWF(2))%IZN(2)/2
                     ELSE IF (KDIFF == 0) THEN
                        IZ1 = (WF(IWF(1))%IZN(2)+1)/2
                        IZ2 = IZ1
                     ELSE
                        CALL SCARC_SHUTDOWN(NSCARC_ERROR_GRID_RESOLUTION, SCARC_NONE, IOR0)
                     ENDIF
                  CASE (3)
                     IDIFF = WF(IWF(2))%IXN(1) - WF(IWF(1))%IXN(1)
                     IF (IDIFF == 1) THEN
                        IX1 = WF(IWF(2))%IXN(2)/2
                        IX2 = IX1
                     ELSE IF (IDIFF == 2) THEN
                        IX1 = WF(IWF(1))%IXN(2)/2
                        IX2 = WF(IWF(2))%IXN(2)/2
                     ELSE IF (IDIFF == 0) THEN
                        IX1 = (WF(IWF(1))%IXN(2)+1)/2
                        IX2 = IX1
                     ELSE
                        CALL SCARC_SHUTDOWN(NSCARC_ERROR_GRID_RESOLUTION, SCARC_NONE, IOR0)
                     ENDIF
               END SELECT

               SELECT CASE (IOR0)
                  CASE (1)
                     IX1 = MESHES(NOMF(1))%IBAR/IREFINE
                     IX2 = IX1
                  CASE (-1)
                     IX1 = 1
                     IX2 = 1
                  CASE (3)
                     IZ1 = MESHES(NOMF(1))%KBAR/IREFINE
                     IZ2 = IZ1
                  CASE (-3)
                     IZ1 = 1
                     IZ2 = 1
               END SELECT

               WC(IWC)%IXN(1) = IX1
               WC(IWC)%IYN(1) = 1
               WC(IWC)%IZN(1) = IZ1
               WC(IWC)%IXN(2) = IX2
               WC(IWC)%IYN(2) = 1
               WC(IWC)%IZN(2) = IZ2

                
               ! Allocate and specify ICN and ICE arrays for OC
                
               CALL SCARC_SETUP_WALL_NEIGHBOR(GC, OGC, IX1, IX2, 1, 1, IZ1, IZ2, IWC, NM, NOMF(1), NL)

            ENDIF

         ! ------------------------------------------------------------
         ! 3D-version
         ! ------------------------------------------------------------
         ELSE

            ! determine fine IW's, which must be merged to one coarse IW

            SELECT CASE (ABS(IOR0))
               CASE (1)
                  IWF(1) = FF%NCW0 + (2*IZ-2)*LF%NY + 2*IY - 2
                  IWF(3) = FF%NCW0 + (2*IZ-1)*LF%NY + 2*IY - 2
               CASE (2)
                  IWF(1) = FF%NCW0 + (2*IZ-2)*LF%NX + 2*IX - 2
                  IWF(3) = FF%NCW0 + (2*IZ-1)*LF%NX + 2*IX - 2
               CASE (3)
                  IWF(1) = FF%NCW0 + (2*IY-2)*LF%NX + 2*IX - 2
                  IWF(3) = FF%NCW0 + (2*IY-1)*LF%NX + 2*IX - 2
            END SELECT
            IWF(2) = IWF(1)+1
            IWF(4) = IWF(3)+1

            ! set fine cell neighbors (they must be the same for all fine IW's)

            DO I=1,4
               NOMF(I) = WF(IWF(I))%NOM
            ENDDO

            IF (NOMF(1) /= NOMF(2) .OR. NOMF(1) /= NOMF(3) .OR. NOMF(1) /= NOMF(4)) &
               CALL SCARC_SHUTDOWN(NSCARC_ERROR_NEIGHBOR_TYPE, SCARC_NONE, IOR0)
            WC(IWC)%NOM = NOMF(1)

            ! set corresponding pressure_bc_index on coarser level

            DO I=1,4
               IBCF(I) = WF(IWF(I))%BTYPE
            ENDDO
            IF (IBCF(1)==INTERNAL.OR.IBCF(2)==INTERNAL.OR.&
               IBCF(3)==INTERNAL.OR.IBCF(4)==INTERNAL) THEN
               WC(IWC)%BTYPE =INTERNAL
            ELSE IF (IBCF(1)==DIRICHLET.OR.IBCF(2)==DIRICHLET.OR.&
               IBCF(3)==DIRICHLET.OR.IBCF(4)==DIRICHLET) THEN
               WC(IWC)%BTYPE =DIRICHLET
            ELSE
               WC(IWC)%BTYPE =NEUMANN
            ENDIF

            ! set corresponding pressure_bc_index on coarser level

            DO I=1,4
               IBCF(I) = WF(IWF(I))%BOUNDARY_TYPE
            ENDDO
            IF (IBCF(1)==NULL_BOUNDARY.OR.IBCF(2)==NULL_BOUNDARY.OR.&
               IBCF(3)==NULL_BOUNDARY.OR.IBCF(4)==NULL_BOUNDARY) THEN
               WC(IWC)%BOUNDARY_TYPE = NULL_BOUNDARY
            ELSE IF (IBCF(1)==SOLID_BOUNDARY.OR.IBCF(2)==SOLID_BOUNDARY.OR.&
               IBCF(3)==SOLID_BOUNDARY.OR.IBCF(4)==SOLID_BOUNDARY) THEN
               WC(IWC)%BOUNDARY_TYPE = SOLID_BOUNDARY
            ELSE IF (IBCF(1)==OPEN_BOUNDARY.OR.IBCF(2)==OPEN_BOUNDARY.OR.&
               IBCF(3)==OPEN_BOUNDARY.OR.IBCF(4)==OPEN_BOUNDARY) THEN
               WC(IWC)%BOUNDARY_TYPE = OPEN_BOUNDARY
            ELSE IF (IBCF(1)==INTERPOLATED_BOUNDARY.OR.IBCF(2)==INTERPOLATED_BOUNDARY.OR.&
               IBCF(3)==INTERPOLATED_BOUNDARY.OR.IBCF(4)==INTERPOLATED_BOUNDARY) THEN
               WC(IWC)%BOUNDARY_TYPE = INTERPOLATED_BOUNDARY
            ELSE IF (IBCF(1)==MIRROR_BOUNDARY.OR.IBCF(2)==MIRROR_BOUNDARY.OR.&
               IBCF(3)==MIRROR_BOUNDARY.OR.IBCF(4)==MIRROR_BOUNDARY) THEN
               WC(IWC)%BOUNDARY_TYPE = MIRROR_BOUNDARY
            ELSE
               CALL SCARC_SHUTDOWN(NSCARC_ERROR_BOUNDARY_TYPE, SCARC_NONE, -999)
            ENDIF

            ! in case of an internal boundary set WALL(10:15,IWC)

            IF (NOMF(1) > 0) THEN

               SELECT CASE (ABS(IOR0))

                  CASE (1)
                     JDIFF = WF(IWF(2))%IYN(1) - WF(IWF(1))%IYN(1)
                     KDIFF = WF(IWF(3))%IZN(1) - WF(IWF(1))%IZN(1)
                     IF (JDIFF==1 .AND. KDIFF==1) THEN
                        IY1 = WF(IWF(2))%IYN(2)/2
                        IY2 = IY1
                        IZ1 = WF(IWF(3))%IZN(2)/2
                        IZ2 = IZ1
                     ELSE IF (JDIFF==2 .AND. KDIFF==2) THEN
                        IY1 = WF(IWF(1))%IYN(2)/2
                        IY2 = WF(IWF(2))%IYN(2)/2
                        IZ1 = WF(IWF(1))%IZN(2)/2
                        IZ2 = WF(IWF(3))%IZN(2)/2
                     ELSE IF (JDIFF==0 .AND. KDIFF==0) THEN
                        IY1 = WF(IWF(1))%IYN(1)/2
                        IY2 = IY1
                        IZ1 = WF(IWF(1))%IZN(1)/2
                        IZ2 = IZ1
                     ELSE
                        CALL SCARC_SHUTDOWN(NSCARC_ERROR_GRID_RESOLUTION, SCARC_NONE, IOR0)
                     ENDIF

                  CASE (2)
                     IDIFF = WF(IWF(2))%IXN(1) - WF(IWF(1))%IXN(1)
                     KDIFF = WF(IWF(3))%IZN(1) - WF(IWF(1))%IZN(1)
                     IF (IDIFF==1 .AND. KDIFF==1) THEN
                        IX1 = WF(IWF(2))%IXN(2)/2
                        IX2 = IX1
                        IZ1 = WF(IWF(3))%IZN(2)/2
                        IZ2 = IZ1
                     ELSE IF (IDIFF==2 .AND. KDIFF==2) THEN
                        IX1 = WF(IWF(1))%IXN(2)/2
                        IX2 = WF(IWF(2))%IXN(2)/2
                        IZ1 = WF(IWF(1))%IZN(2)/2
                        IZ2 = WF(IWF(3))%IZN(2)/2
                     ELSE IF (IDIFF==0 .AND. KDIFF==0) THEN
                        IX1 = WF(IWF(1))%IXN(2)/2
                        IX2 = IX1
                        IZ1 = WF(IWF(1))%IZN(2)/2
                        IZ2 = IZ1
                     ELSE
                        CALL SCARC_SHUTDOWN(NSCARC_ERROR_GRID_RESOLUTION, SCARC_NONE, IOR0)
                     ENDIF

                  CASE (3)
                     IDIFF = WF(IWF(2))%IXN(1) - WF(IWF(1))%IXN(1)
                     JDIFF = WF(IWF(3))%IYN(1) - WF(IWF(1))%IYN(1)
                     IF (IDIFF==1 .AND. JDIFF==1) THEN
                        IX1 = WF(IWF(2))%IXN(2)/2
                        IX2 = IX1
                        IY1 = WF(IWF(3))%IYN(2)/2
                        IY2 = IY1
                     ELSE IF (IDIFF==2 .AND. JDIFF==2) THEN
                        IX1 = WF(IWF(1))%IXN(2)/2
                        IX2 = WF(IWF(2))%IXN(2)/2
                        IY1 = WF(IWF(1))%IYN(2)/2
                        IY2 = WF(IWF(3))%IYN(2)/2
                     ELSE IF (IDIFF==0 .AND. JDIFF==0) THEN
                        IX1 = WF(IWF(2))%IXN(2)/2
                        IX2 = IX1
                        IY1 = WF(IWF(3))%IYN(2)/2
                        IY2 = IY1
                     ELSE
                        CALL SCARC_SHUTDOWN(NSCARC_ERROR_GRID_RESOLUTION, SCARC_NONE, IOR0)
                     ENDIF

               END SELECT

               SELECT CASE (IOR0)
                  CASE (1)
                     IX1 = MESHES(NOMF(1))%IBAR/IREFINE
                     IX2 = IX1
                  CASE (-1)
                     IX1 = 1
                     IX2 = IX1
                  CASE (2)
                     IY1 = MESHES(NOMF(1))%JBAR/IREFINE
                     IY2 = IY1
                  CASE (-2)
                     IY1 = 1
                     IY2 = IY1
                  CASE (3)
                     IZ1 = MESHES(NOMF(1))%KBAR/IREFINE
                     IZ2 = IZ1
                  CASE (-3)
                     IZ1 = 1
                  IZ2 = IZ1
               END SELECT

               WC(IWC)%IXN(1) = IX1
               WC(IWC)%IYN(1) = IY1
               WC(IWC)%IZN(1) = IZ1
               WC(IWC)%IXN(2) = IX2
               WC(IWC)%IYN(2) = IY2
               WC(IWC)%IZN(2) = IZ2

               CALL SCARC_SETUP_WALL_NEIGHBOR(GC, OGC, IX1, IX2, IY1, IY2, IZ1, IZ2, IWC, NM, NOMF(1), NL)

            ENDIF
         ENDIF
         IWC = IWC + 1
      ENDDO
   ENDDO
ENDDO

END SUBROUTINE SCARC_SETUP_WALL_LEVEL

! -------------------------------------------------------------------------------------------
!> \brief Setup mapping from local to global cell numbering
! -------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_GLOBAL_CELL_MAPPING(NL)
USE SCARC_POINTERS, ONLY: G, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NL
INTEGER :: NM, NOM, IC, IW, ICE, ICN

!IF (NMESHES == 1) RETURN
CROUTINE = 'SCARC_SETUP_GLOBAL_CELL_MAPPING'

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SETUP_GLOBAL_CELL_MAPPING:1:', TYPE_GRID, TYPE_SCOPE(0), TYPE_MATVEC
#endif
!IF (IS_MGM .AND. IS_UNSTRUCTURED) RETURN
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SETUP_GLOBAL_CELL_MAPPING:2'
#endif

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)      

   IF (HAS_AMG_LEVELS) THEN
      CALL SCARC_ALLOCATE_INT1 (G%LOCAL_TO_GLOBAL, 1, G%NCE2, NSCARC_INIT_ZERO, 'G%LOCAL_TO_GLOBAL', CROUTINE)  
   ELSE
      CALL SCARC_ALLOCATE_INT1 (G%LOCAL_TO_GLOBAL, 1, G%NCE , NSCARC_INIT_ZERO, 'G%LOCAL_TO_GLOBAL', CROUTINE) 
   ENDIF

   DO IC = 1, G%NC
      G%LOCAL_TO_GLOBAL(IC) = IC + G%NC_OFFSET(NM)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'LOCAL_TO_GLOBAL(', IC,')=', G%LOCAL_TO_GLOBAL(IC), G%NCE
#endif
   ENDDO

   DO IW = 1, G%NW
      NOM = G%WALL(IW)%NOM
      IF (NOM == 0) CYCLE
      ICE = G%WALL(IW)%ICE
      ICN = G%ICE_TO_ICN(ICE)
      G%LOCAL_TO_GLOBAL(ICE) = ICN + G%NC_OFFSET(NOM)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'LOCAL_TO_GLOBAL(', ICE,')=', G%LOCAL_TO_GLOBAL(ICE), IW, ICE, ICN, NOM
#endif
   ENDDO

ENDDO

END SUBROUTINE SCARC_SETUP_GLOBAL_CELL_MAPPING

END MODULE SCARC_DISCRETIZATION


