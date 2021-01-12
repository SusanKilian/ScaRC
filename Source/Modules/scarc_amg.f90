!//////////////////////////////////////////////////////////////////////////////////////////////////////
!
! MODULE SCARC_AMG
!
!> \brief Setup algebraic multigrid structures
!   Allocate needed workspace for hierarchy of system matrices, prolongation, restriction, etc.
!   Note: all used pointers end with either 'F' or 'C' where:
!       'F' corresponds to fine   level NL
!       'C' corresponds to coarse level NL+1
!   Determine mesh hierarchy based on smoothed aggregation
!   Compute QR-decomposition of nullspace vector in order to determine tentative prolongator 
!   Set nullspace for next level and perform Jacobi relaxation to get the final prolongator
!   If the maximum allowed level is not yet reached, set dimensions for next coarser level, 
!   define its nullspace and perform relaxation to define the respective Prolongation matrix
!   Define Poisson matrix on coarser level by Galerkin approach: A_coarse = R * A_fine * P
!
!//////////////////////////////////////////////////////////////////////////////////////////////////////
MODULE SCARC_AMG
  
USE GLOBAL_CONSTANTS
USE PRECISION_PARAMETERS, ONLY: EB, FB
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
USE COMP_FUNCTIONS, ONLY: CURRENT_TIME
USE MPI
USE SCARC_CONSTANTS
USE SCARC_TYPES
USE SCARC_VARIABLES
USE SCARC_UTILITIES
USE SCARC_MESSAGES
USE SCARC_TIMINGS, ONLY: CPU
USE SCARC_STACK
USE SCARC_CONVERGENCE
USE SCARC_VECTORS
USE SCARC_MATRICES

IMPLICIT NONE

CONTAINS

! ----------------------------------------------------------------------------------------------------
!> \brief Setup structures needed for the use of the algebraic multigrid method
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_ALGEBRAIC_MULTIGRID
INTEGER :: NL
LOGICAL :: FURTHER_COARSENING_REQUIRED

IF (.NOT.HAS_AMG_LEVELS) RETURN
FURTHER_COARSENING_REQUIRED = .TRUE.

NL = NLEVEL_MIN
!CALL  SCARC_PYTHON_MATRIX(NL, 'A')

COARSENING_LOOP: DO WHILE (FURTHER_COARSENING_REQUIRED)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '========================================================'
WRITE(MSG%LU_DEBUG,*) ' ALGEBRAIC MULTIGRID : LEVEL ', NL
WRITE(MSG%LU_DEBUG,*) '========================================================'
#endif

   ! Determine the aggregation order among the meshes
   CALL SCARC_SETUP_AGGREGATION_ORDER                    

   ! Extract matrix diagonal from Poisson matrix A, determine strength of connection matrix and store inverted matrix diagonal 
   CALL SCARC_EXTRACT_MATRIX_DIAGONAL(NL)           
   CALL SCARC_SETUP_CONNECTION(NL)                  
   CALL SCARC_INVERT_MATRIX_DIAGONAL(NL)            

   ! Apply smoothed aggregation heuristic to specify aggregation zones and improve near null space by Jacobi relaxation step
   CALL SCARC_SETUP_AGGREGATION_ZONES(NL)           
   CALL SCARC_RELAX_NULLSPACE(NL)                   

   ! Setup final aggregation Zones matrix Z and Prolongation matrix P based on QR-decomposition
   CALL SCARC_SETUP_ZONE_OPERATOR(NL)               

      ! Setup restriction and prolongation matrices for GMG-like coarsening
   IF (TYPE_COARSENING == NSCARC_COARSENING_GMG) THEN
      CALL SCARC_SETUP_TRANSFER_GMG(NL)

   ! Setup Prolongation matrix P based on QR-decomposition, near nullspace on coarser level and corresponding Restriction matrix R
   ELSE
      CALL SCARC_SETUP_PROLONGATION_AMG(NL)
      CALL SCARC_SETUP_NULLSPACE_COARSE(NL)
      CALL SCARC_SETUP_RESTRICTION(NL)
   ENDIF

   ! First setup A*P matrix to finally build the Galerkin matrix R*A*P
   CALL SCARC_SETUP_POISSON_PROL(NL)             
   CALL SCARC_SETUP_GALERKIN(NL)             

   ! Remove workspace which is no longer used and get the next coarsening round on the wa
   CALL SCARC_CLEAN_WORKSPACE_AMG(NL)
      
   NL = NL + 1
   IF (NL == NLEVEL_MAX) FURTHER_COARSENING_REQUIRED = .FALSE.

ENDDO COARSENING_LOOP

END SUBROUTINE SCARC_SETUP_ALGEBRAIC_MULTIGRID


! ------------------------------------------------------------------------------------------------------
!> \brief  Setup order in which aggregation is performed over mesh decomposition
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_AGGREGATION_ORDER
USE SCARC_POINTERS, ONLY: SUB, S
LOGICAL, ALLOCATABLE, DIMENSION(:) :: NOT_AGGREGATED
INTEGER :: NM, NOM, INBR, ICYCLE

CROUTINE = 'SCARC_SETUP_AGGREGATION_ORDER'
SUB => SUBDIVISION

CALL SCARC_ALLOCATE_INT2(SUB%ORDER, 1, NMESHES, 1, NMESHES, NSCARC_INIT_NONE, 'SUB%ORDER', CROUTINE)
CALL SCARC_ALLOCATE_LOG1(NOT_AGGREGATED, 1, NMESHES, NSCARC_INIT_TRUE, 'NOT_AGGREGATED', CROUTINE)

ICYCLE = 1
DO WHILE (ANY(NOT_AGGREGATED)) 
   SUB%ORDER(1:NMESHES, ICYCLE) = NSCARC_ORDER_UNASSIGNED
   DO NM = 1, NMESHES
      S => SCARC(NM)
      IF (NOT_AGGREGATED(NM) .AND. SUB%ORDER(NM, ICYCLE) /= NSCARC_ORDER_LOCKED) THEN
         SUB%ORDER(NM, ICYCLE) = NSCARC_ORDER_ACTIVE
         DO INBR = 1, SUB%N_NEIGHBORS(NM)
            NOM = SUB%NEIGHBORS(INBR, NM)
            SUB%ORDER(NOM, ICYCLE) = NSCARC_ORDER_LOCKED
         ENDDO
         NOT_AGGREGATED(NM) = .FALSE.
      ENDIF
   ENDDO
   ICYCLE = ICYCLE + 1
ENDDO

SUB%N_CYCLES = ICYCLE - 1

CALL SCARC_DEALLOCATE_LOG1 (NOT_AGGREGATED, 'NOT_AGGREGATED', CROUTINE)

END SUBROUTINE SCARC_SETUP_AGGREGATION_ORDER


! ------------------------------------------------------------------------------------------------------
!> \brief  Invert matrix diagonal which is already stored in DIAG-vector (reuse workspace)
! Scale each matrix element with inverse of diagonal and approximate spectral radius (currently disabled) 
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_INVERT_MATRIX_DIAGONAL(NL)
USE SCARC_POINTERS, ONLY: G, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NL
INTEGER :: NM, IC

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
   DO IC = 1, G%NCE
      G%DIAG(IC) = 1.0_EB/G%DIAG(IC)
   ENDDO
ENDDO MESHES_LOOP

END SUBROUTINE SCARC_INVERT_MATRIX_DIAGONAL


! ------------------------------------------------------------------------------------------------------
!> \brief  Compute a strength of connection matrix based on symmetric smoothed aggregation heuristic. 
! A nonzero connection A[i,j] is considered strong if:
!
!     abs(A[i,j]) >= theta * sqrt( abs(A[i,i]) * abs(A[j,j]) )
!
! The strength matrix S corresponds to the set of nonzero entries of A that are strong connections
! based on a strength of connection tolerance theta
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_CONNECTION(NL)
USE SCARC_POINTERS, ONLY: G, A, S, C, OG, OA, OC, &
                          SCARC_POINT_TO_GRID, SCARC_POINT_TO_OTHER_GRID, &
                          SCARC_POINT_TO_CMATRIX, SCARC_POINT_TO_OTHER_CMATRIX
INTEGER, INTENT(IN) :: NL
REAL(EB):: VAL, EPS, SCAL, CVAL_MAX, THETA
INTEGER :: NM, NOM, IC, JC, ICOL, IZONE, INBR

IF (TYPE_COARSENING == NSCARC_COARSENING_CUBIC) RETURN

CROUTINE = 'SCARC_SETUP_CONNECTION'

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   
   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

   A => SCARC_POINT_TO_CMATRIX (G, NSCARC_MATRIX_POISSON)
   C => SCARC_POINT_TO_CMATRIX (G, NSCARC_MATRIX_CONNECTION)

#ifdef WITH_SCARC_DEBUG
   CALL SCARC_DEBUG_CMATRIX(A, 'A','POISSON')
   WRITE(MSG%LU_DEBUG,*) 'CONNECTION: DIAG:', SIZE(G%DIAG)
   WRITE(MSG%LU_DEBUG,'(8E14.6)') G%DIAG
#endif

   ! Allocate workspace for strength of connection matrix (use same size as Poisson matrix)
   C%N_VAL = A%N_VAL                         
   C%N_ROW = A%N_ROW
   CALL SCARC_ALLOCATE_CMATRIX(C, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_LIGHT, 'G%CONNECTION', CROUTINE)

   IF (NL == NLEVEL_MIN) THEN
      THETA = 0.10E+0_EB
   ELSE
      THETA = SCARC_MULTIGRID_THETA
   ENDIF
   THETA = SCARC_MULTIGRID_THETA
   
   DO INBR = 1, SCARC(NM)%N_NEIGHBORS

      NOM = S%NEIGHBORS(INBR)
      CALL SCARC_POINT_TO_OTHER_GRID(NM, NOM, NL)

      OA => SCARC_POINT_TO_OTHER_CMATRIX(OG, NSCARC_MATRIX_POISSON)
      OC => SCARC_POINT_TO_OTHER_CMATRIX(OG, NSCARC_MATRIX_CONNECTION)

      OC%N_VAL = 2*OA%N_VAL                   ! use double layers
      OC%N_ROW = OA%N_ROW           
      CALL SCARC_ALLOCATE_CMATRIX(OC, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_LIGHT, 'OG%CONNECTION', CROUTINE)
      
   ENDDO 

   ! Check strength-of-connection criterion  
   IZONE = 1
   C%ROW(1) = 1
   DO IC = 1, G%NC
   
      EPS = THETA**2 * ABS(G%DIAG(IC))                  ! EPS = theta**2 * A_ii
      DO ICOL = A%ROW(IC), A%ROW(IC+1) - 1
         JC  = A%COL(ICOL)
         IF (JC == 0) CYCLE                                             ! omit second layer
         VAL = A%VAL(ICOL)                                              ! VAL = A_ij
   
         ! Always add the diagonal: |A_ii|  >= THETA * sqrt(|A_ii| * |A_ii|)     true!
         IF (IC == JC) THEN
            C%COL(IZONE) = JC
            C%VAL(IZONE) = VAL
            IZONE = IZONE + 1

         ! Check subdiagonal entry: |A_ij|  >= THETA * sqrt(|A_ii| * |A_jj|)     ??
         ELSE IF (VAL**2 >= EPS * ABS(G%DIAG(JC))) THEN
            C%COL(IZONE) = JC
            C%VAL(IZONE) = VAL
            IZONE = IZONE + 1
#ifdef WITH_SCARC_VERBOSE
         ELSE
            WRITE(MSG%LU_VERBOSE,'(I6,A,4I6,6E14.6)') MYID+1,': CONNECTION: NO NEIGHBORS ', &
                                                      IC, ICOL, JC, IZONE, THETA, EPS, &
                                                      G%DIAG(IC), G%DIAG(JC), EPS*ABS(G%DIAG(JC)), VAL**2
#endif
         ENDIF

      ENDDO
      C%ROW(IC+1) = IZONE
   ENDDO
   
#ifdef WITH_SCARC_DEBUG
   CALL SCARC_DEBUG_CMATRIX(C, 'CONNECTION', 'STRENGTH OF CNNECTION - PASS 1')
#endif
   
   DO IC = 1, G%NC
      CVAL_MAX = 0.0_EB
      DO ICOL = C%ROW(IC), C%ROW(IC+1) - 1
         CVAL_MAX = MAX(ABS(C%VAL(ICOL)), CVAL_MAX)
      ENDDO
      SCAL = 1.0_EB/CVAL_MAX
      DO ICOL = C%ROW(IC), C%ROW(IC+1) - 1
         C%VAL(ICOL) = ABS(C%VAL(ICOL))*SCAL
      ENDDO
   ENDDO
   
#ifdef WITH_SCARC_DEBUG
   CALL SCARC_DEBUG_CMATRIX(C, 'C','CONNECTION')
#endif
   
ENDDO MESHES_LOOP
   
! If there are multiple meshes, exchange strength matrix on overlapping parts
IF (NMESHES > 1) THEN
   CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MATRIX_COLS,  NSCARC_MATRIX_CONNECTION, NL)
   CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MATRIX_VALS,  NSCARC_MATRIX_CONNECTION, NL)
ENDIF

END SUBROUTINE SCARC_SETUP_CONNECTION
 

! ------------------------------------------------------------------------------------------------------
!> \brief Setup aggregation zones for Smoothed Aggregation Algebraic Multigrid Method
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_AGGREGATION_ZONES(NL)
USE SCARC_POINTERS, ONLY: SUB, C, CF, G, LF, LC, GF, GC, &
                          SCARC_POINT_TO_GRID, SCARC_POINT_TO_CMATRIX, SCARC_POINT_TO_MULTIGRID
INTEGER, INTENT(IN) :: NL
INTEGER :: NM, NM2, ICYCLE, IC, IZL

CROUTINE = 'SCARC_SETUP_AGGREGATION_ZONES'

SUB => SUBDIVISION
MESH_INT = -1

! Allocate workspaces for coarse points, global and local aggregation zones 
MESHES_ALLOCATION_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

   CALL SCARC_ALLOCATE_INT1 (G%ZONE_CENTERS,  1, G%NCE,  NSCARC_INIT_ZERO, 'G%ZONE_CENTERS', CROUTINE)
   CALL SCARC_ALLOCATE_INT1 (G%ZONES_GLOBAL, 1, G%NCE2, NSCARC_INIT_ZERO, 'G%ZONES_GLOBAL', CROUTINE)
   CALL SCARC_ALLOCATE_INT1 (G%ZONES_LOCAL,  1, G%NCE2, NSCARC_INIT_ZERO, 'G%ZONES_LOCAL', CROUTINE)

ENDDO MESHES_ALLOCATION_LOOP


COARSENING_TYPE_SELECT: SELECT CASE (TYPE_COARSENING)

 
   ! ---- Default aggregation procedure for SAMG
 
   CASE (NSCARC_COARSENING_AGGREGATED)

   WRITE(*,*) 'COARSENING_AGGREGATED'
      DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
         CALL SCARC_POINT_TO_GRID(NM, NL)
         C => SCARC_POINT_TO_CMATRIX (G, NSCARC_MATRIX_CONNECTION)
         CALL SCARC_SETUP_COARSENING_AGGREGATION(G, C)
         MESH_INT (NM) = G%N_ZONES
      ENDDO
      
      ! Exchange overlapping information of active meshes

      IF (NMESHES > 1)  CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_ZONE_TYPES, NSCARC_NONE, NL)
   
   ! ---- Staggered aggregation procedure for SAMG
 
   CASE (NSCARC_COARSENING_AGGREGATED_S)

      CYCLES_LOOP1: DO ICYCLE = 1, SUB%N_CYCLES
   
         ! First aggregate on active meshes

         DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   
            IF (SUB%ORDER(NM, ICYCLE) == NSCARC_ORDER_ACTIVE) THEN
               CALL SCARC_POINT_TO_GRID(NM, NL)
               C => SCARC_POINT_TO_CMATRIX (G, NSCARC_MATRIX_CONNECTION)
               CALL SCARC_SETUP_COARSENING_AGGREGATION(G, C)
               MESH_INT (NM) = G%N_ZONES
            ENDIF
   
         ENDDO
      
         ! Exchange overlapping information of active meshes

         IF (NMESHES > 1)  CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_ZONE_TYPES, NSCARC_NONE, NL)
   
         ! Then aggregate on passive meshes (taking into account overlapping aggregate information of active meshes)

         DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
      
            IF (SUB%ORDER (NM, ICYCLE) /= NSCARC_ORDER_ACTIVE) THEN
               CALL SCARC_POINT_TO_GRID(NM, NL)
               C => SCARC_POINT_TO_CMATRIX (G, NSCARC_MATRIX_CONNECTION)
               CALL SCARC_SETUP_COARSENING_AGGREGATION(G, C)
               MESH_INT (NM) = G%N_ZONES
            ENDIF
   
         ENDDO
   
         ! Exchange overlapping information of passive meshes

         IF (NMESHES > 1)  CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_ZONE_TYPES, NSCARC_NONE, NL)
   
      ENDDO CYCLES_LOOP1
   
 
   ! ---- GMG-like aggregation procedure 
   !      In case of even cell numbers this process corresponds to the usual GMG coarsening
   !      in case of uneven cell number in a coordinate direction, on patch with 3 cells is used, the rest with patches of 2
 
   CASE (NSCARC_COARSENING_CUBIC)

   WRITE(*,*) 'COARSENING_CUBIC'
      DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
         CALL SCARC_POINT_TO_MULTIGRID(NM, NL, NL+1)
         CALL SCARC_SETUP_COARSENING_CUBIC(LF, LC, GF, GC)
         MESH_INT(NM) = GF%N_ZONES
      ENDDO
      
      ! Exchange overlapping information 

      IF (NMESHES > 1)  CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_ZONE_TYPES, NSCARC_NONE, NL)
   
   CASE (NSCARC_COARSENING_GMG)

   WRITE(*,*) 'COARSENING_GMG'
      DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
         CALL SCARC_POINT_TO_MULTIGRID(NM, NL, NL+1)
         CALL SCARC_SETUP_COARSENING_GMG(LF, LC, GF, GC)
         MESH_INT(NM) = GF%N_ZONES
      ENDDO
      
END SELECT COARSENING_TYPE_SELECT


! Broadcast number of zones of all meshes

IF (N_MPI_PROCESSES>1) &
   CALL MPI_ALLGATHERV(MPI_IN_PLACE, 1, MPI_INTEGER, MESH_INT, COUNTS, DISPLS, MPI_INTEGER, MPI_COMM_WORLD, IERROR)
      

! Prepare grid dimensions of coarse grid level
 
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MULTIGRID(NM, NL, NL+1)         

   ! Setup grid dimensions on coarse level 
   GC%NC_LOCAL(1:NMESHES) = MESH_INT(1:NMESHES)
   GC%NC_GLOBAL = SUM(MESH_INT(1:NMESHES))
   GC%NC  = GC%NC_LOCAL(NM)
   GC%NCE = GC%NC_LOCAL(NM)
   IF (NMESHES > 1) THEN
      DO NM2 = 2, NMESHES
         GC%NC_OFFSET(NM2) = GC%NC_OFFSET(NM2-1) + GC%NC_LOCAL(NM2-1)
      ENDDO
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '============== NM=',NM
WRITE(MSG%LU_DEBUG,*) 'GC%NC_LOCAL(1:NMESHES) =', GC%NC_LOCAL(1:NMESHES)
WRITE(MSG%LU_DEBUG,*) 'GC%NC_GLOBAL ', GC%NC_GLOBAL
WRITE(MSG%LU_DEBUG,*) 'GC%NC ', GC%NC
WRITE(MSG%LU_DEBUG,*) 'GC%NCE', GC%NCE
WRITE(MSG%LU_DEBUG,*) 'GC%NC_OFFSET(1:NMESHES) ', GC%NC_OFFSET(1:NMESHES)
WRITE(MSG%LU_DEBUG,*) 'GF%NCE', GF%NCE
#endif
   ENDIF                   

   ! Setup mapping from local zones to global zones

   CALL SCARC_ALLOCATE_INT1(GC%LOCAL_TO_GLOBAL, 1, GF%NCE, NSCARC_INIT_ZERO, 'G%LOCAL_TO_GLOBAL', CROUTINE)
   DO IZL = 1, GC%NC
      GC%LOCAL_TO_GLOBAL(IZL) = IZL + GC%NC_OFFSET(NM)
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'NM=',NM,':GC%LOCAL_TO_GLOBAL(',IZL,')=',GC%LOCAL_TO_GLOBAL(IZL)
#endif
   ENDDO

   DO IC = 1, GF%NC
      GF%ZONES_GLOBAL(IC) = GF%ZONES_LOCAL(IC) + GC%NC_OFFSET(NM)
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'NM=',NM,':GF%ZONES_GLOBAL(',IC,')=',GF%ZONES_GLOBAL(IC)
#endif
   ENDDO

ENDDO

! Exchange zones information between meshes

IF (NMESHES > 1)  THEN
   CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_CELL_NEIGHBORS, NSCARC_NONE, NL)
   CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_ZONE_NEIGHBORS, NSCARC_NONE, NL)
   CALL SCARC_EXTRACT_ZONE_OVERLAPS(NL)
   CALL SCARC_EXTRACT_ZONE_POINTERS(NL)
ENDIF

! Determine final grid dimensions on coarser level and reduce zone arrays to correct length

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MULTIGRID(NM, NL, NL+1)         

   GC%NCE  = GF%N_ZONES
   GC%NCE2 = GF%N_ZONES

   CALL SCARC_REDUCE_INT1(GC%LOCAL_TO_GLOBAL, GC%NCE2, 'GC%LOCAL_TO_GLOBAL', CROUTINE)
   CALL SCARC_REDUCE_INT1(GC%ZONE_CENTERS, GC%NCE2, 'GC%ZONE_CENTERS', CROUTINE)

   GF%N_COARSE = GF%N_ZONES

   CALL SCARC_REDUCE_INT1(GF%ZONES_LOCAL,  GF%NCE2, 'GC%LOCAL_TO_GLOBAL', CROUTINE)
   CALL SCARC_REDUCE_INT1(GF%ZONES_GLOBAL, GF%NCE2, 'GC%LOCAL_TO_GLOBAL', CROUTINE)

   CF => SCARC_POINT_TO_CMATRIX (GF, NSCARC_MATRIX_CONNECTION)
   CALL SCARC_DEALLOCATE_CMATRIX(CF, 'STRENGTH OF CONNECTION', CROUTINE)

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,': ================== END OF SETUP AGGREGATION_ZONES '
   WRITE(MSG%LU_DEBUG,*) 'GC%NC =', GC%NC
   WRITE(MSG%LU_DEBUG,*) 'GC%NCE =', GC%NCE
   WRITE(MSG%LU_DEBUG,*) 'GC%NCE2=', GC%NCE2
   WRITE(MSG%LU_DEBUG,*) 'GC%NC_GLOBAL =', GC%NC_GLOBAL
   WRITE(MSG%LU_DEBUG,*) 'GC%NC_LOCAL(1:NMESHES) =', GC%NC_LOCAL(1:NMESHES)
   WRITE(MSG%LU_DEBUG,*) 'GC%NC_OFFSET(1:NMESHES) =', GC%NC_OFFSET(1:NMESHES)
   WRITE(MSG%LU_DEBUG,*) 'GF%ZONES_LOCAL  ='
   WRITE(MSG%LU_DEBUG,'(8I6)') GF%ZONES_LOCAL
   WRITE(MSG%LU_DEBUG,*) 'GF%ZONES_GLOBAL  ='
   WRITE(MSG%LU_DEBUG,'(8I6)') GF%ZONES_GLOBAL
   WRITE(MSG%LU_DEBUG,*) 'GC%LOCAL_TO_GLOBAL  ='
   WRITE(MSG%LU_DEBUG,'(8I6)') GC%LOCAL_TO_GLOBAL
#endif
#ifdef WITH_SCARC_VERBOSE
   CALL SCARC_VERBOSE_BLENDER_ZONES(NM, NL)
#endif

ENDDO

END SUBROUTINE SCARC_SETUP_AGGREGATION_ZONES



! -------------------------------------------------------------------------------------------
!> \brief Extract overlapping zone information (including second layers)
! -------------------------------------------------------------------------------------------
SUBROUTINE SCARC_EXTRACT_ZONE_OVERLAPS(NL)
USE SCARC_POINTERS, ONLY: GC, GF, OLF, OGF, F, &
                          SCARC_POINT_TO_MULTIGRID, SCARC_POINT_TO_OTHER_MULTIGRID
INTEGER, INTENT(IN) :: NL
INTEGER :: NM, INBR, IOR0, NOM, IZ, ICG, ICE, ICE2, IFOUND, IZL_CURRENT
INTEGER :: IZL1, IZL2, IZG1, IZG2, IFACE

CROUTINE = 'SCARC_EXTRACT_ZONE_OVERLAPS'

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MULTIGRID (NM, NL, NL+1)    

   ! Clear overlapping parts and fill them with the recently exchanged data
   GF%ZONES_LOCAL (GF%NC+1: GF%NCE2) = 0
   GF%ZONES_GLOBAL(GF%NC+1: GF%NCE2) = 0

   IZL_CURRENT = GF%N_ZONES + 1

   DO IFACE = 1, 6                                        

      IOR0 = FACE_ORIENTATION(IFACE)
      F => SCARC(NM)%LEVEL(NLEVEL_MIN)%FACE(IOR0)

      DO INBR = 1, F%N_NEIGHBORS

         NOM = F%NEIGHBORS(INBR)
         CALL SCARC_POINT_TO_OTHER_MULTIGRID(NM, NOM, NL, NL+1)

         IZ = 0
         DO ICG = OLF%GHOST_FIRSTE(IOR0), OLF%GHOST_LASTE(IOR0)

            ICE  = OGF%ICG_TO_ICE(ICG, 1)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,6I6)') 'ZONE_OVERLAPS: NM, INBR, NOM, IOR0, ICG, ICE:', NM, INBR, NOM, IOR0, ICG, ICE
#endif

            IZG1 = OGF%ICG_TO_GZONE(ICG)
            IFOUND = FINDLOC (GF%ZONES_GLOBAL, VALUE = IZG1, DIM = 1)
            IF (IFOUND == 0) THEN
               IZ = IZ + 1
               GF%N_ZONES = GF%N_ZONES + 1
               IZL1 = GF%N_ZONES
            ELSE
               IZL1 = GF%ZONES_LOCAL(IFOUND)
            ENDIF
            GF%ZONES_LOCAL(ICE)  = IZL1
            GF%ZONES_GLOBAL(ICE) = IZG1
            GC%LOCAL_TO_GLOBAL(IZL1) = IZG1

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,9I6)') 'A: NM, ICG, IFOUND, N_ZONES IZL1, ICE,  LOCAL(ICE),  GLOBAL(ICE) :', &
          NM, ICG, IFOUND, GF%N_ZONES, IZL1, ICE, GF%ZONES_LOCAL(ICE), GF%ZONES_GLOBAL(ICE), GC%LOCAL_TO_GLOBAL(IZL1)
#endif
 
            IF (NL /= NLEVEL_MIN) CYCLE

            ICE2 = OGF%ICG_TO_ICE(ICG, 2)
            IZG2 = OGF%ICG_TO_GZONE(ICG + OGF%NCG)
            IFOUND = FINDLOC (GF%ZONES_GLOBAL, VALUE = IZG2, DIM = 1)
            IF (IFOUND == 0) THEN
               GF%N_ZONES = GF%N_ZONES + 1
               IZL2 = GF%N_ZONES
            ELSE
               IZL2 = GF%ZONES_LOCAL(IFOUND)
            ENDIF
            GF%ZONES_LOCAL(ICE2)  = IZL2
            GF%ZONES_GLOBAL(ICE2) = IZG2
            GC%LOCAL_TO_GLOBAL(IZL2) = IZG2

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,9I6)') 'B: NM, ICG, IFOUND, N_ZONES IZL2, ICE2, LOCAL(ICE2), GLOBAL(ICE2):', &
          NM, ICG, IFOUND, GF%N_ZONES, IZL2, ICE2, GF%ZONES_LOCAL(ICE2), GF%ZONES_GLOBAL(ICE2), GC%LOCAL_TO_GLOBAL(IZL2)
#endif

         ENDDO
      ENDDO
   ENDDO 

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '===================== EXTRACT_ZONE_OVERLAPS: NM=',NM
WRITE(MSG%LU_DEBUG,*) 'LOCAL_TO_GLOBAL'
WRITE(MSG%LU_DEBUG,'(8I6)') GC%LOCAL_TO_GLOBAL
CALL SCARC_DEBUG_ZONES(GF, -1, 1, 'AFTER EXTRACT_ZONES')
CALL SCARC_DEBUG_ZONES(GF, -1, 2, 'AFTER EXTRAXT_ZONES')
#endif

ENDDO MESHES_LOOP

END SUBROUTINE SCARC_EXTRACT_ZONE_OVERLAPS


! -------------------------------------------------------------------------------------------
!> \brief Setup pointers for overlapping zones for a pair of grid levels
! -------------------------------------------------------------------------------------------
SUBROUTINE SCARC_EXTRACT_ZONE_POINTERS(NL)
USE SCARC_POINTERS, ONLY : F, GF, GC, OLF, OLC, OGF, OGC, &
                           SCARC_POINT_TO_MULTIGRID, SCARC_POINT_TO_OTHER_MULTIGRID
INTEGER, INTENT(IN) :: NL
INTEGER :: INBR, IOR0, IZ, ICW1, ICW2, ICE1, ICE2, IZL1, IZL2, ICG, IZW, IZE, IFACE
INTEGER :: NM, NOM, NCGE_TOTAL = 0

CROUTINE = 'SCARC_EXTRACT_ZONE_POINTERS'

MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MULTIGRID (NM, NL, NL+1)    
   
   DO IFACE = 1, 6                                        

      IOR0 = FACE_ORIENTATION(IFACE)
      F => SCARC(NM)%LEVEL(NLEVEL_MIN)%FACE(IOR0)

      DO INBR = 1, F%N_NEIGHBORS

         NOM = F%NEIGHBORS(INBR)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '============== ZONE_POINTERS: PROCESSING IFACE =', IFACE,' INBR, NOM =', INBR, NOM, NL
#endif
         CALL SCARC_POINT_TO_OTHER_MULTIGRID(NM, NOM, NL, NL+1)

         CALL SCARC_ALLOCATE_INT1(OGF%ICG_TO_IZONE, 1, 2*OGF%NCG, NSCARC_INIT_ZERO, 'OGF%ICG_TO_IZONE', CROUTINE)
         CALL SCARC_ALLOCATE_INT1(OGF%ICG_TO_EZONE, 1, 2*OGF%NCG, NSCARC_INIT_ZERO, 'OGF%ICG_TO_EZONE', CROUTINE)

         IZ  = 0
         INTERNAL_ZONES_LOOP: DO ICG = OLF%GHOST_FIRSTW(IOR0), OLF%GHOST_LASTW(IOR0)

            ICW1 = OGF%ICG_TO_ICW(ICG, 1)
            IZL1 = GF%ZONES_LOCAL(ICW1)
            IF (FINDLOC(OGF%ICG_TO_IZONE, VALUE = IZL1, DIM = 1) == 0) THEN
               IZ = IZ + 1
               OGF%ICG_TO_IZONE(IZ) = IZL1
            ENDIF

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A, 8I6)') 'NM, INBR, NOM, IOR0, ICG, ICW1, IZL1, IZ :', NM, INBR, NOM, IOR0, ICG, ICW1, IZL1, IZ
#endif
            IF (NL /= NLEVEL_MIN) CYCLE

            ICW2 = OGF%ICG_TO_ICW(ICG, 2)
            IZL2 = GF%ZONES_LOCAL(ICW2)
            IF (FINDLOC(OGF%ICG_TO_IZONE, VALUE = IZL2, DIM = 1) == 0) THEN
               IZ = IZ + 1
               OGF%ICG_TO_IZONE(IZ) = IZL2
            ENDIF

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A, 8I6)') 'NM, ,NBR, NOM, IOR0, ICG, ICW2, IZL2, IZ :', NM, INBR, NOM, IOR0, ICG, ICW2, IZL2, IZ
#endif
         ENDDO INTERNAL_ZONES_LOOP
         OGF%NCGI = IZ
         CALL SCARC_REDUCE_INT1(OGF%ICG_TO_IZONE, OGF%NCGI, 'OGF%ICG_TO_IZONE', CROUTINE)

         !First allocate in fine cell related length

         CALL SCARC_ALLOCATE_INT2(OGC%ICG_TO_ICW, 1, OGF%NCGI, 1, 1, NSCARC_INIT_ZERO, 'OGF%ICG_TO_ICW', CROUTINE)
         
         IZ = 0
         DO ICG = 1, OGF%NCGI
            IZW = OGF%ICG_TO_IZONE(ICG)
            IF (FINDLOC(OGC%ICG_TO_ICW(1:OGF%NCGI,1), VALUE = IZW, DIM = 1) == 0) THEN
               IZ = IZ + 1
               OGC%ICG_TO_ICW(IZ, 1) = IZW
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'NM=',NM,': OGC%ICG_TO_ICW(',IZ,',1)=',IZW
#endif
            ENDIF
         ENDDO
         OGC%NCG  = IZ
         OGC%NCGI = IZ

         OLC%GHOST_FIRSTW(IOR0) = 1
         OLC%GHOST_LASTW(IOR0)  = IZ

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'NM=',NM,': GHOST_FIRSTW(',IOR0,')=', OLC%GHOST_FIRSTW(IOR0)
WRITE(MSG%LU_DEBUG,*) 'NM=',NM,': GHOST_LASTW(',IOR0,') =', OLC%GHOST_LASTW(IOR0)
#endif
         ! Then reduce to zone related length 

         CALL SCARC_REDUCE_INT2(OGC%ICG_TO_ICW, OGC%NCGI, 1, 'OGC%ICG_TO_ICW', CROUTINE)


         IZ  = 0 
         EXTERNAL_ZONES_LOOP: DO ICG = OLF%GHOST_FIRSTE(IOR0), OLF%GHOST_LASTE(IOR0)

            ICE1 = OGF%ICG_TO_ICE(ICG, 1)
            IZL1 = GF%ZONES_LOCAL(ICE1)
            IF (FINDLOC(OGF%ICG_TO_EZONE, VALUE = IZL1, DIM = 1) == 0) THEN
               IZ = IZ + 1
               OGF%ICG_TO_EZONE(IZ) = IZL1
            ENDIF

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A, 8I6)') 'NM, INBR, NOM, IOR0, ICG, ICE1, IZL1, IZ :', NM, INBR, NOM, IOR0, ICG, ICE1, IZL1, IZ
#endif
            IF (NL /= NLEVEL_MIN) CYCLE

            ICE2 = OGF%ICG_TO_ICE(ICG, 2)
            IZL2 = GF%ZONES_LOCAL(ICE2)
            IF (FINDLOC(OGF%ICG_TO_EZONE, VALUE = IZL2, DIM = 1) == 0) THEN
               IZ = IZ + 1
               OGF%ICG_TO_EZONE(IZ) = IZL2
            ENDIF

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A, 8I6)') 'NM, INBR, NOM, IOR0, ICG, ICE2, IZL2, IZ :', NM, INBR, NOM, IOR0, ICG, ICE2, IZL2, IZ
#endif
         ENDDO EXTERNAL_ZONES_LOOP
         OGF%NCGE = IZ
         CALL SCARC_REDUCE_INT1(OGF%ICG_TO_EZONE, OGF%NCGE, 'OGF%ICG_TO_EZONE', CROUTINE)

         ! First allocate in fine cell related length

         CALL SCARC_ALLOCATE_INT2 (OGC%ICG_TO_ICE, 1, 2*OGF%NCGE, 1, 1, NSCARC_INIT_ZERO, 'OGC%ICG_TO_ICE', CROUTINE)
         
         IZ = 0
         DO ICG = 1, OGF%NCGE
            IZE = OGF%ICG_TO_EZONE(ICG)
            IF (FINDLOC(OGC%ICG_TO_ICE(1:OGF%NCGE,1), VALUE = IZE, DIM = 1) == 0) THEN
               IZ = IZ + 1
               OGC%ICG_TO_ICE(IZ, 1) = IZE
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'NM=',NM,': OGC%ICG_TO_ICE(',IZ,',1)=',IZE
#endif
            ENDIF
         ENDDO
         OGC%NCGE = IZ
         NCGE_TOTAL = NCGE_TOTAL + OGC%NCGE

         OLC%GHOST_FIRSTE(IOR0) = 1
         OLC%GHOST_LASTE(IOR0)  = IZ

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'NM=',NM,': GHOST_FIRSTE(',IOR0,')=', OLC%GHOST_FIRSTE(IOR0)
WRITE(MSG%LU_DEBUG,*) 'NM=',NM,': GHOST_LASTE(',IOR0,')= ', OLC%GHOST_LASTE(IOR0)
#endif
         ! Then reduce to zone related length 

         CALL SCARC_REDUCE_INT2(OGC%ICG_TO_ICE, OGC%NCGE, 1, 'OGC%ICG_TO_ICE', CROUTINE)

         GC%N_STENCIL_MAX = 25                  ! TODO: ONLY TEMPORARILY
         OGC%NLEN_BUFFER_LAYER1  = MAX(OGC%NCGI, OGC%NCGE)
         OGC%NLEN_BUFFER_LAYER2  = OGC%NLEN_BUFFER_LAYER1 * 2
         OGC%NLEN_BUFFER_LAYER4  = OGC%NLEN_BUFFER_LAYER1 * 4
         OGC%NLEN_BUFFER_STENCIL = OGC%NLEN_BUFFER_LAYER1 * GC%N_STENCIL_MAX
         OGC%NLEN_BUFFER_FULL    = OGC%NLEN_BUFFER_LAYER1 * GC%N_STENCIL_MAX * 2


#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '===================== EXTRACT_ZONE_POINTERS: NM', NM
WRITE(MSG%LU_DEBUG,'(A,3I6)') 'ZONE_POINTERS INBR, NOM, IOR0:', INBR, NOM, IOR0
WRITE(MSG%LU_DEBUG,'(A,2I6)') 'EXCHANGE LENGTH WITH NEIGHBOR ',NOM, OGC%NLEN_BUFFER_LAYER1
WRITE(MSG%LU_DEBUG,*) '------------------------------------------------'
WRITE(MSG%LU_DEBUG,'(A,I6)') 'ICG_TO_IZONE(.): OGF%NCGI', OGF%NCGI
WRITE(MSG%LU_DEBUG,'(8I6)')  (OGF%ICG_TO_IZONE(IZ), IZ=1, OGF%NCGI)
WRITE(MSG%LU_DEBUG,*) '------------------------------------------------'
WRITE(MSG%LU_DEBUG,'(A,I6)') 'OGF:ICG_TO_EZONE(.):  OGF%NCGE', OGF%NCGE
WRITE(MSG%LU_DEBUG,'(8I6)') (OGF%ICG_TO_EZONE(IZ), IZ=1, OGF%NCGE)
WRITE(MSG%LU_DEBUG,*) '------------------------------------------------'
WRITE(MSG%LU_DEBUG,'(A,I6)') 'OGC%ICG_TO_ICW: OGC%NCGI', OGC%NCGI
WRITE(MSG%LU_DEBUG,'(8I6)')  OGC%ICG_TO_ICW(1:OGC%NCGI,1)
WRITE(MSG%LU_DEBUG,*) '------------------------------------------------'
WRITE(MSG%LU_DEBUG,'(A,I6)') 'OGC%ICG_TO_ICE: OGC%NCGE', OGC%NCGE
WRITE(MSG%LU_DEBUG,'(8I6)')  OGC%ICG_TO_ICE(1:OGC%NCGE,1)
WRITE(MSG%LU_DEBUG,*) '------------------------------------------------'
WRITE(MSG%LU_DEBUG,*) 'NCGE_TOTAL = ', NCGE_TOTAL
WRITE(MSG%LU_DEBUG,*) 'OGC%NLEN_BUFFER_LAYER1 = ', OGC%NLEN_BUFFER_LAYER1
WRITE(MSG%LU_DEBUG,*) 'OGC%NLEN_BUFFER_LAYER2 = ', OGC%NLEN_BUFFER_LAYER2
WRITE(MSG%LU_DEBUG,*) 'OGC%NLEN_BUFFER_LAYER4 = ', OGC%NLEN_BUFFER_LAYER4
WRITE(MSG%LU_DEBUG,*) 'OGC%NLEN_BUFFER_STENCIL= ', OGC%NLEN_BUFFER_STENCIL
WRITE(MSG%LU_DEBUG,*) 'OGC%NLEN_BUFFER_FULL   = ', OGC%NLEN_BUFFER_FULL
#endif

      ENDDO 
   ENDDO 
ENDDO MESHES_LOOP

END SUBROUTINE SCARC_EXTRACT_ZONE_POINTERS

! -------------------------------------------------------------------------------------------
!> \brief Identify cells on second layer
! -------------------------------------------------------------------------------------------
SUBROUTINE SCARC_IDENTIFY_LAYER2(NL)
USE SCARC_POINTERS, ONLY : S, A, G, OL, OG, &
                           SCARC_POINT_TO_GRID, SCARC_POINT_TO_OTHER_GRID, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NL
INTEGER :: NM, NOM, ICW, ICOL, JC, JCG, INBR, IOR0, ICG, IS

CROUTINE = 'SCARC_IDENTIFY_LAYER2'
 
MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)    
   A => SCARC_POINT_TO_CMATRIX (G, NSCARC_MATRIX_POISSON)

   NEIGHBORS_LOOP: DO INBR = 1, SCARC(NM)%N_NEIGHBORS

      NOM = S%NEIGHBORS(INBR)
      CALL SCARC_POINT_TO_OTHER_GRID(NM, NOM, NL)
    
      CALL SCARC_ALLOCATE_INT1(OG%ICG_TO_ELAYER2, 1, 2*OG%NCG, NSCARC_INIT_ZERO, 'OG%ICG_TO_ELAYER2', CROUTINE)

      IS = 1
      FACE_LOOP: DO IOR0 = -3, 3
         IF (OL%GHOST_LASTW(IOR0) == 0) CYCLE
         GHOST_CELL_LOOP: DO ICG = OL%GHOST_FIRSTW(IOR0), OL%GHOST_LASTW(IOR0)
            ICW = OG%ICG_TO_ICW(ICG, 1)
            DO ICOL = A%ROW(ICW), A%ROW(ICW+1) - 1
               JC = A%COL(ICOL)
               IF (JC == 0) THEN
                  JCG = A%COLG(ICOL)
                  IF (FINDLOC (OG%ICG_TO_ELAYER2(1:2*OG%NCG), VALUE = JCG, DIM = 1) == 0) THEN 
                  OG%ICG_TO_ELAYER2(IS) = JCG
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,3I6)') '----------> JCG, IS, ICG_TO_ELAYER2 : ', JCG, IS, OG%ICG_TO_ELAYER2(IS)
#endif
                  IS = IS + 1
                  ENDIF
               ENDIF
            ENDDO
         ENDDO GHOST_CELL_LOOP
      ENDDO FACE_LOOP
      OL%N_LAYER2 = IS - 1

   ENDDO NEIGHBORS_LOOP
ENDDO MESHES_LOOP

END SUBROUTINE SCARC_IDENTIFY_LAYER2



! ------------------------------------------------------------------------------------------------------
!> \brief  Standard aggregation prodecure based on strength of connection matrix
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_COARSENING_AGGREGATION(G, C)
TYPE (SCARC_CMATRIX_TYPE), POINTER, INTENT(IN) :: C
TYPE (SCARC_GRID_TYPE), POINTER, INTENT(IN) :: G
INTEGER :: IC, ICOL, JC, IZONE, JZONE
LOGICAL :: HAS_NEIGHBORS, HAS_AGGREGATED_NEIGHBORS

CROUTINE = 'SCARC_SETUP_COARSENING_AGGREGATION'

! 
! Pass 1 of aggregation:  Setup aggregation zones on internal cells of active mesh
! 
G%ZONES_LOCAL = 0
G%ZONES_GLOBAL = 0
G%ZONE_CENTERS = 0

PASS1_LOOP: DO IC = 1, G%NC

   IF (G%ZONES_LOCAL(IC) /= 0) CYCLE                           ! has cell already been aggregated?

   HAS_NEIGHBORS = .FALSE.
   HAS_AGGREGATED_NEIGHBORS = .FALSE.

   DO ICOL = C%ROW(IC), C%ROW(IC+1)-1                          ! are all neighbors free (not already aggregated)?
      JC = C%COL(ICOL)
      IF (JC /= 0 .AND. IC /= JC .AND. JC <= G%NC) THEN        ! only consider internal cells here
         HAS_NEIGHBORS = .TRUE.
         IF (G%ZONES_LOCAL(JC) /= 0) THEN
            HAS_AGGREGATED_NEIGHBORS = .TRUE.
            EXIT
         ENDIF
      ENDIF
   ENDDO

   IF (.NOT. HAS_NEIGHBORS) THEN                               ! do not aggregate isolated cells
      G%ZONES_LOCAL(IC) = NSCARC_HUGE_INT
   ELSE IF (.NOT. HAS_AGGREGATED_NEIGHBORS) THEN               ! build aggregate of this cell and its neighbors
      G%N_ZONES = G%N_ZONES + 1
      G%ZONES_LOCAL(IC) = G%N_ZONES
      G%ZONE_CENTERS(G%N_ZONES) = IC                
      DO ICOL = C%ROW(IC), C%ROW(IC+1)-1 
         JC = C%COL(ICOL)
         IF (JC /= 0 .AND. JC <= G%NC) G%ZONES_LOCAL(C%COL(ICOL)) = G%N_ZONES
      ENDDO
   ENDIF

#ifdef WITH_SCARC_DEBUG2
CALL SCARC_DEBUG_ZONES(G, IC, 1, 'AFTER ACTIVE PASS1')
#endif

ENDDO PASS1_LOOP


! 
! Pass 2 of Aggregation:  Add unaggregated nodes to neighboring aggregate
! 
PASS2_LOOP: DO IC = 1, G%NC

   IF (G%ZONES_LOCAL(IC) /= 0) CYCLE            
   DO ICOL = C%ROW(IC), C%ROW(IC+1)-1
      JC = C%COL(ICOL)
      JZONE = G%ZONES_LOCAL(JC)
      IF (JZONE > 0) THEN
         IF (JC >= G%NC .OR. G%ZONE_CENTERS(JZONE)>0) THEN
            G%ZONES_LOCAL(IC) = -JZONE
            EXIT
         ENDIF
      ENDIF
   ENDDO
ENDDO PASS2_LOOP
!G%N_ZONES = G%N_ZONES - 1                         !TODO: check
      
#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_ZONES(G, -1, 1, 'AFTER ACTIVE PASS2')
#endif


! 
! Pass 3 of Aggregation:  Process remaining nodes which have not been aggregated yet
! 
PASS3_LOOP: DO IC = 1, G%NC

   IZONE = G%ZONES_LOCAL(IC)

   ! cell IC has not been aggregated
   IF (IZONE /= 0) THEN
      IF (IZONE > 0) THEN
         G%ZONES_LOCAL(IC) = IZONE 
      ELSE IF (IZONE == NSCARC_HUGE_INT ) THEN
         G%ZONES_LOCAL(IC) = -1
      ELSE
         G%ZONES_LOCAL(IC) = -IZONE 
      ENDIF
      CYCLE PASS3_LOOP
   ENDIF

   G%ZONES_LOCAL(IC) = G%N_ZONES
   G%ZONE_CENTERS(G%N_ZONES) = IC

   DO ICOL = C%ROW(IC), C%ROW(IC+1)-1
      JC = C%COL(ICOL)
      IF (JC <= G%NC .AND. G%ZONES_LOCAL(JC) == 0) G%ZONES_LOCAL(JC) = G%N_ZONES
   ENDDO
   G%N_ZONES = G%N_ZONES + 1

ENDDO PASS3_LOOP

IF (MINVAL(G%ZONES_LOCAL) < 0) THEN
   WRITE(*,*) MYID+1, ':CAUTION: CELL ',MINLOC(G%ZONES_LOCAL),' HAS NOT BEEN AGGREGATED DURING AGGREGATION'
#ifdef WITH_SCARC_VERBOSE
   WRITE(*,*) MYID+1, ':CAUTION: CELL ',MINLOC(G%ZONES_LOCAL),' HAS NOT BEEN AGGREGATED DURING AGGREGATION'
   WRITE(MSG%LU_VERBOSE,*) 'G%ZONES_LOCAL:'
   WRITE(MSG%LU_VERBOSE,'(8I12)') G%ZONES_LOCAL(1:G%NCE)
#endif
   CALL MPI_FINALIZE(IERROR)
   STOP
ENDIF
      
#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_ZONES(G, -1, 1, 'AFTER ACTIVE PASS3')
#endif

END SUBROUTINE SCARC_SETUP_COARSENING_AGGREGATION


! ------------------------------------------------------------------------------------------------------
!> \brief Selfdefined geometric motivated aggregation procedure using cubic zones
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_COARSENING_CUBIC(LF, LC, GF, GC)
TYPE (SCARC_LEVEL_TYPE), POINTER, INTENT(IN) :: LF, LC
TYPE (SCARC_GRID_TYPE), POINTER, INTENT(IN) :: GF, GC
INTEGER :: NXM, NYM, NZM, NXD, NYD, NZD, NXI, NYI, NZI
INTEGER :: IX, IY, IZ, IXZ, IYZ, IZZ, IXP, IYP, IZP, IX0, IY0, IZ0, IC
INTEGER, DIMENSION(:), ALLOCATABLE :: OFFX, OFFY, OFFZ
LOGICAL :: BFIRST

CROUTINE = 'SCARC_SETUP_COARSENING_CUBIC'

NXM = MOD(LF%NX,2)
NXD = LF%NX/2

IF (TWO_D) THEN
   NYM = 0
   NYD = 1
ELSE
   NYM = MOD(LF%NY,2)
   NYD = LF%NY/2
ENDIF

NZM = MOD(LF%NZ,2)
NZD = LF%NZ/2

! Temporarily - to prevent failure of following algorithm

IF ((LF%NX < 4) .OR. (.NOT.TWO_D .AND. LF%NY < 4) .OR. (LF%NZ < 4)) THEN 
   WRITE(*,*) 'Grid dimensions too small für GMG-like aggregation'
   CALL MPI_FINALIZE(IERROR)
   STOP
ENDIF

CALL SCARC_ALLOCATE_INT1 (OFFX, 1, NXD, NSCARC_INIT_ZERO, 'OFFX', CROUTINE)
CALL SCARC_ALLOCATE_INT1 (OFFY, 1, NYD, NSCARC_INIT_ZERO, 'OFFY', CROUTINE)
CALL SCARC_ALLOCATE_INT1 (OFFZ, 1, NZD, NSCARC_INIT_ZERO, 'OFFZ', CROUTINE)

! If even number of cells in x-direction, use patch length of 2 as in default GMG
! else insert a patch of 3 after the first quarter of cells in x-direction

IF (NXM == 0) THEN
   OFFX = 2
ELSE
   NXI = MAX(NXD/4,1)
   DO IX = 1, NXI
      OFFX(IX) = 2
   ENDDO
   OFFX(NXI+1) = 3
   DO IX = NXI+2, NXD
      OFFX(IX) = 2
   ENDDO
ENDIF

! If even number of cells in y-direction, use patch length of 2 as in default GMG
! else insert a patch of 3 after the first third of cells in x-direction

IF (TWO_D) THEN
   OFFY = 0
ELSE
   IF (NYM == 0) THEN
      OFFY = 2
   ELSE
      NYI = MAX(NYD/3,1)
      DO IY = 1, NYI
         OFFY(IY) = 2
      ENDDO
      OFFY(NYI+1) = 3
      DO IY = NYI+2, NYD
         OFFY(IY) = 2
      ENDDO
   ENDIF
ENDIF

! If even number of cells in x-direction, use patch length of 2 as in default GMG
! else insert a patch of 3 after the first half of cells in x-direction
! the idea is to use different portions in the different coordinate direction to prevent local concentrations

IF (NZM == 0) THEN
   OFFZ = 2
ELSE
   NZI = MAX(NZD/2,1)
   DO IZ = 1, NZI
      OFFZ(IZ) = 2
   ENDDO
   OFFZ(NZI+1) = 3
   DO IZ = NZI+2, NZD
      OFFZ(IZ) = 2
   ENDDO
ENDIF

LC%NX = NXD
LC%NY = NYD
LC%NZ = NZD

CALL SCARC_ALLOCATE_INT3 (GC%CELL_NUMBER, 0, LC%NX+1, 0, LC%NY+1, 0, LC%NZ+1, NSCARC_INIT_UNDEF, 'GC%CELL_NUMBER', CROUTINE)
CALL SCARC_ALLOCATE_LOG3 (LC%IS_SOLID, 0, LC%NX+1, 0, LC%NY+1, 0, LC%NZ+1, NSCARC_INIT_TRUE, 'LC%IS_SOLID', CROUTINE)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'OFFX:'
WRITE(MSG%LU_DEBUG,'(16I6)') OFFX
WRITE(MSG%LU_DEBUG,*) 'OFFY:'
WRITE(MSG%LU_DEBUG,'(16I6)') OFFY
WRITE(MSG%LU_DEBUG,*) 'OFFZ:'
WRITE(MSG%LU_DEBUG,'(16I6)') OFFZ
WRITE(MSG%LU_DEBUG,*) 'NXD=',NXD
WRITE(MSG%LU_DEBUG,*) 'NYD=',NYD
WRITE(MSG%LU_DEBUG,*) 'NZD=',NZD
#endif

GF%ZONES_LOCAL = 0
GF%ZONES_GLOBAL = 0
GF%ZONE_CENTERS = 0
DIMENSION_IF: IF (TWO_D) THEN

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'TWO_D'
#endif
   IZ0 = 1
   DO IZ = 1, NZD
      IX0 = 1
      DO IX = 1, NXD

         BFIRST = .TRUE.
         DO IZZ = 0, OFFZ(IZ)-1
            DO IXZ = 0, OFFX(IX)-1
               IXP = IX0 + IXZ
               IZP = IZ0 + IZZ
               IF (IS_UNSTRUCTURED .AND. LF%IS_SOLID(IXP, 1, IZP)) CYCLE
               IC = GF%CELL_NUMBER(IXP, 1, IZP)
               IF (BFIRST) THEN
                  GF%N_ZONES = GF%N_ZONES + 1
                  BFIRST = .FALSE. 
                  GF%ZONE_CENTERS(GF%N_ZONES) = IC
                  GC%CELL_NUMBER(IX, 1, IZ) = GF%N_ZONES
                  LC%IS_SOLID(IX, 1, IZ) = .FALSE.
               ENDIF
               GF%ZONES_LOCAL(IC) = GF%N_ZONES
            ENDDO
         ENDDO
         IX0 = IX0 + OFFX(IX)
      ENDDO
      IZ0 = IZ0 + OFFZ(IZ)
   ENDDO

ELSE

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'THREE_D'
#endif
   IZ0 = 1
   DO IZ = 1, NZD
      IY0 = 1
      DO IY = 1, NYD
         IX0 = 1
         DO IX = 1, NXD

            BFIRST = .TRUE.
            DO IZZ = 0, OFFZ(IZ)-1
               DO IYZ = 0, OFFY(IY)-1
                  DO IXZ = 0, OFFX(IX)-1
                     IXP = IX0 + IXZ
                     IYP = IY0 + IYZ
                     IZP = IZ0 + IZZ
                     IF (IS_UNSTRUCTURED .AND. LF%IS_SOLID(IXP, IYP, IZP)) CYCLE
                     IC = GF%CELL_NUMBER(IXP, IYP, IZP)
                     IF (BFIRST) THEN
                        GF%N_ZONES = GF%N_ZONES + 1
                        BFIRST = .FALSE. 
                        GF%ZONE_CENTERS(GF%N_ZONES) = IC
                        GC%CELL_NUMBER(IX, IY, IZ) = GF%N_ZONES
                        LC%IS_SOLID(IX, IY, IZ) = .FALSE.
                     ENDIF
                     IC = GF%CELL_NUMBER(IXP, IYP, IZP)
                     GF%ZONES_LOCAL(IC) = GF%N_ZONES
                  ENDDO
               ENDDO
            ENDDO
            IX0 = IX0 + OFFX(IX)
         ENDDO
         IY0 = IY0 + OFFY(IY)
      ENDDO
      IZ0 = IZ0 + OFFZ(IZ)
   ENDDO
ENDIF DIMENSION_IF

CALL SCARC_DEALLOCATE_INT1 (OFFX, 'OFFX', CROUTINE)
CALL SCARC_DEALLOCATE_INT1 (OFFY, 'OFFY', CROUTINE)
CALL SCARC_DEALLOCATE_INT1 (OFFZ, 'OFFZ', CROUTINE)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'LC%NX=',LC%NX
WRITE(MSG%LU_DEBUG,*) 'LC%NY=',LC%NY
WRITE(MSG%LU_DEBUG,*) 'LC%NZ=',LC%NZ
WRITE(MSG%LU_DEBUG,*) 'LC%IS_SOLID:'
DO IZ = 1, LC%NZ
   WRITE(MSG%LU_DEBUG,*)
   DO IY = 1, LC%NY
      WRITE(MSG%LU_DEBUG,*) (LC%IS_SOLID(IX, IY, IZ), IX=1, LC%NX)
   ENDDO
ENDDO
WRITE(MSG%LU_DEBUG,*) 'GC%CELL_NUMBER:'
DO IZ = 1, LC%NZ
   WRITE(MSG%LU_DEBUG,*)
   DO IY = 1, LC%NY
      WRITE(MSG%LU_DEBUG,'(8I4)') (GC%CELL_NUMBER(IX, IY, IZ), IX=1, LC%NX)
   ENDDO
ENDDO
CALL SCARC_DEBUG_ZONES(GF, -1, 1, 'AFTER ACTIVE PASS3')
#endif

END SUBROUTINE SCARC_SETUP_COARSENING_CUBIC

! ------------------------------------------------------------------------------------------------------
!> \brief Selfdefined geometric motivated aggregation procedure using cubic zones
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_COARSENING_GMG(LF, LC, GF, GC)
TYPE (SCARC_LEVEL_TYPE), POINTER, INTENT(IN) :: LF, LC
TYPE (SCARC_GRID_TYPE),  POINTER, INTENT(IN) :: GF, GC
INTEGER :: MODX, MODY, MODZ
INTEGER :: RELX, RELY, RELZ
INTEGER :: IX, IY, IZ, IXZ, IYZ, IZZ, IXP, IYP, IZP, IX0, IY0, IZ0, IC
INTEGER, DIMENSION(:), ALLOCATABLE :: OFFX, OFFY, OFFZ
LOGICAL :: BFIRST

CROUTINE = 'SCARC_SETUP_COARSENING_GMG'

MODX = MOD(LF%NX,2)
LC%NX = INT(FLOOR(REAL(LF%NX/2),EB),4) + MODX

IF (TWO_D) THEN
   MODY = 0
   LC%NY = 1
ELSE
   MODY = MOD(LF%NY,2)
   LC%NY = INT(FLOOR(REAL(LF%NY/2),EB),4) + MODY
ENDIF

MODZ = MOD(LF%NZ,2)
LC%NZ = INT(FLOOR(REAL(LF%NZ/2),EB),4) + MODZ

CALL SCARC_ALLOCATE_INT1 (OFFX, 1, LC%NX, NSCARC_INIT_ZERO, 'OFFX', CROUTINE)
CALL SCARC_ALLOCATE_INT1 (OFFY, 1, LC%NY, NSCARC_INIT_ZERO, 'OFFY', CROUTINE)
CALL SCARC_ALLOCATE_INT1 (OFFZ, 1, LC%NZ, NSCARC_INIT_ZERO, 'OFFZ', CROUTINE)

OFFX = 2 ;  IF (MODX /= 0) OFFX(LC%NX) = 1
IF (TWO_D) THEN
   OFFY = 0 ;  LC%NY = 1
ELSE
   OFFY = 2 ;  IF (MODY /= 0) OFFY(LC%NY) = 1
ENDIF
OFFZ = 2 ;  IF (MODZ /= 0) OFFZ(LC%NZ) = 1

RELX = INT(CEILING(REAL(LF%NX/LC%NX),EB), 4)
RELY = INT(CEILING(REAL(LF%NY/LC%NY),EB), 4)
RELZ = INT(CEILING(REAL(LF%NZ/LC%NZ),EB), 4)

CALL SCARC_ALLOCATE_INT3 (GC%CELL_NUMBER, 0, LC%NX+1, 0, LC%NY+1, 0, LC%NZ+1, NSCARC_INIT_UNDEF, 'GC%CELL_NUMBER', CROUTINE)
CALL SCARC_ALLOCATE_LOG3 (LC%IS_SOLID,    0, LC%NX+1, 0, LC%NY+1, 0, LC%NZ+1, NSCARC_INIT_TRUE,  'LC%IS_SOLID', CROUTINE)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'MODX, RELX, LC%NX=', MODX, RELX, LC%NX
WRITE(MSG%LU_DEBUG,*) 'MODY, RELY, LC%NY=', MODY, RELY, LC%NY
WRITE(MSG%LU_DEBUG,*) 'MODZ, RELZ, LC%NZ=', MODZ, RELZ, LC%NZ
#endif

GF%ZONES_LOCAL  = 0
GF%ZONES_GLOBAL = 0
GF%ZONE_CENTERS = 0

DIMENSION_IF: IF (TWO_D) THEN

   IZ0 = 1
   DO IZ = 1, LC%NZ
      IX0 = 1
      DO IX = 1, LC%NX

         BFIRST = .TRUE.
         DO IZZ = 0, OFFZ(IZ)-1
            DO IXZ = 0, OFFX(IX)-1
               IXP = IX0 + IXZ
               IZP = IZ0 + IZZ
               IF (IS_UNSTRUCTURED .AND. LF%IS_SOLID(IXP, 1, IZP)) CYCLE
               IC = GF%CELL_NUMBER(IXP, 1, IZP)
               IF (BFIRST) THEN
                  GF%N_ZONES = GF%N_ZONES + 1
                  BFIRST = .FALSE. 
                  GF%ZONE_CENTERS(GF%N_ZONES) = IC
                  GC%CELL_NUMBER(IX, 1, IZ) = GF%N_ZONES
                  LC%IS_SOLID(IX, 1, IZ) = .FALSE.
               ENDIF
               GF%ZONES_LOCAL(IC) = GF%N_ZONES
            ENDDO
         ENDDO
         IX0 = IX0 + OFFX(IX)
      ENDDO
      IZ0 = IZ0 + OFFZ(IZ)
   ENDDO

ELSE

   IZ0 = 1
   DO IZ = 1, LC%NZ
      IY0 = 1
      DO IY = 1, LC%NY
         IX0 = 1
         DO IX = 1, LC%NX

            BFIRST = .TRUE.
            DO IZZ = 0, OFFZ(IZ)-1
               DO IYZ = 0, OFFY(IY)-1
                  DO IXZ = 0, OFFX(IX)-1
                     IXP = IX0 + IXZ
                     IYP = IY0 + IYZ
                     IZP = IZ0 + IZZ
                     IF (IS_UNSTRUCTURED .AND. LF%IS_SOLID(IXP, IYP, IZP)) CYCLE
                     IC = GF%CELL_NUMBER(IXP, IYP, IZP)
                     IF (BFIRST) THEN
                        GF%N_ZONES = GF%N_ZONES + 1
                        BFIRST = .FALSE. 
                        GF%ZONE_CENTERS(GF%N_ZONES) = IC
                        GC%CELL_NUMBER(IX, IY, IZ) = GF%N_ZONES
                        LC%IS_SOLID(IX, IY, IZ) = .FALSE.
                     ENDIF
                     IC = GF%CELL_NUMBER(IXP, IYP, IZP)
                     GF%ZONES_LOCAL(IC) = GF%N_ZONES
                  ENDDO
               ENDDO
            ENDDO
            IX0 = IX0 + OFFX(IX)
         ENDDO
         IY0 = IY0 + OFFY(IY)
      ENDDO
      IZ0 = IZ0 + OFFZ(IZ)
   ENDDO

ENDIF DIMENSION_IF

CALL SCARC_DEALLOCATE_INT1 (OFFX, 'OFFX', CROUTINE)
CALL SCARC_DEALLOCATE_INT1 (OFFY, 'OFFY', CROUTINE)
CALL SCARC_DEALLOCATE_INT1 (OFFZ, 'OFFZ', CROUTINE)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'LC%NX=',LC%NX
WRITE(MSG%LU_DEBUG,*) 'LC%NY=',LC%NY
WRITE(MSG%LU_DEBUG,*) 'LC%NZ=',LC%NZ
WRITE(MSG%LU_DEBUG,*) 'LC%IS_SOLID:'
DO IZ = 1, LC%NZ
   WRITE(MSG%LU_DEBUG,*)
   DO IY = 1, LC%NY
      WRITE(MSG%LU_DEBUG,*) (LC%IS_SOLID(IX, IY, IZ), IX=1, LC%NX)
   ENDDO
ENDDO
WRITE(MSG%LU_DEBUG,*) 'GC%CELL_NUMBER:'
DO IZ = 1, LC%NZ
   WRITE(MSG%LU_DEBUG,*)
   DO IY = 1, LC%NY
      WRITE(MSG%LU_DEBUG,'(8I4)') (GC%CELL_NUMBER(IX, IY, IZ), IX=1, LC%NX)
   ENDDO
ENDDO
CALL SCARC_DEBUG_ZONES(GF, -1, 1, 'AFTER ACTIVE PASS3')
#endif

END SUBROUTINE SCARC_SETUP_COARSENING_GMG

! ------------------------------------------------------------------------------------------------------
!> \brief Remove workspace on specified grid level which will no longer be needed after matrix setup
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_CLEAN_WORKSPACE_SYSTEM(NL)
USE SCARC_POINTERS, ONLY: SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NL
INTEGER:: NM

! TODO: deallocate arrays which are no longer used
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   CALL SCARC_POINT_TO_GRID (NM, NL)
ENDDO

END SUBROUTINE SCARC_CLEAN_WORKSPACE_SYSTEM


! ------------------------------------------------------------------------------------------------------
!> \brief Remove workspace on specified grid level which will no longer be needed in SAMG method
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_CLEAN_WORKSPACE_AMG(NL)
USE SCARC_POINTERS, ONLY: G, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NL
INTEGER:: NM

CROUTINE = 'SCARC_CLEAN_WORKSPACE_AMG'

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   CALL SCARC_POINT_TO_GRID (NM, NL)
   IF (ALLOCATED(G%ZONE_CENTERS)) CALL SCARC_DEALLOCATE_INT1 (G%ZONE_CENTERS, 'G%ZONE_CENTERS', CROUTINE)
   IF (ALLOCATED(G%AUX1)) CALL SCARC_DEALLOCATE_REAL1 (G%AUX1, 'G%AUX1', CROUTINE)
   IF (ALLOCATED(G%AUX2)) CALL SCARC_DEALLOCATE_REAL1 (G%AUX2, 'G%AUX2', CROUTINE)
   IF (ALLOCATED(G%RR)) CALL SCARC_DEALLOCATE_REAL1 (G%RR, 'G%RR', CROUTINE)
   IF (ALLOCATED(G%QQ)) CALL SCARC_DEALLOCATE_REAL1 (G%QQ, 'G%QQ', CROUTINE)
ENDDO

END SUBROUTINE SCARC_CLEAN_WORKSPACE_AMG


! ------------------------------------------------------------------------------------------------------
!> \brief Perform relaxation of nullspac
! Perform AMG Jacobi :.. x = x - omega D^{-1} (Ax-b)
! Near-null space vector is given in vector G%NULLSPACE --> corresponds to x
! vector b is zero
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_RELAX_NULLSPACE(NL)
USE SCARC_POINTERS, ONLY: L, G, A, MG, SCARC_POINT_TO_GRID, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NL
INTEGER :: IC, ICOL, NM, JC, JCG
#ifdef WITH_SCARC_MKL
EXTERNAL :: DAXPBY
#endif

CROUTINE = 'SCARC_RELAX_NULLSPACE'

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   CALL SCARC_POINT_TO_GRID (NM, NL) 
   CALL SCARC_ALLOCATE_REAL1 (G%AUX1, 1, G%NCE2+1, NSCARC_INIT_ZERO, 'G%AUX1', CROUTINE)
   CALL SCARC_ALLOCATE_REAL1 (G%AUX2, 1, G%NCE2+1, NSCARC_INIT_ZERO, 'G%AUX2', CROUTINE)
ENDDO

! For coarser levels exchange numbers and values of second layer cells with are needed for nullspace computation

IF (SCARC_MULTIGRID_RELAXING .AND. NL > NLEVEL_MIN) THEN
   CALL SCARC_IDENTIFY_LAYER2(NL)
   CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_LAYER2_NUMS, NSCARC_NONE, NL)
   CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_LAYER2_VALS, NSCARC_NONE, NL)
ENDIF

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)         
   A => SCARC_POINT_TO_CMATRIX (G, NSCARC_MATRIX_POISSON)

   MG => L%MG
   MG%OMEGA = 1.0_EB/MG%APPROX_SPECTRAL_RADIUS
   !MG%OMEGA = 4.0_EB/(3.0_EB*MG%APPROX_SPECTRAL_RADIUS)

#ifdef WITH_SCARC_DEBUG
IF (SCARC_MULTIGRID_RELAXING) THEN
WRITE(MSG%LU_DEBUG,*) 'Using A', G%NCE2, NL
CALL SCARC_DEBUG_CMATRIX(A, 'A','A IN NULLSPACE')
WRITE(MSG%LU_DEBUG,*) 'Using LOCAL_TO_GLOBAL:', G%NCE2, NL
WRITE(MSG%LU_DEBUG,'(8I6)') G%ZONES_LOCAL
WRITE(MSG%LU_DEBUG,*) 'Using ZONES_GLOBAL:', G%NCE2, NL
WRITE(MSG%LU_DEBUG,'(8I6)') G%ZONES_GLOBAL
IF (NL > NLEVEL_MIN .AND. TYPE_COARSENING == NSCARC_COARSENING_AGGREGATED) THEN
   WRITE(MSG%LU_DEBUG,*) 'Using LAYER2_NUMS:', G%NCE2, NL, L%N_LAYER2_TOTAL
   WRITE(MSG%LU_DEBUG,'(8I6)') G%ELAYER2_NUMS(1: L%N_LAYER2_TOTAL)
ENDIF
ENDIF
#endif

   ! Compute defect to near-null-space vector: d = Ax - b, in this case
   !    'x' corresponds to nullspace vector consisting of only '1'-entries 
   !    'b' corresponds to zero vector 

   ! On finest level NULLSPACE vector is preset with 1, so matrix entries can simply be added
   IF (NL == NLEVEL_MIN) THEN

      CALL SCARC_ALLOCATE_REAL1 (G%NULLSPACE, 1, G%NCE2, NSCARC_INIT_ONE, 'G%NULLSPACE', CROUTINE)
      FINE_CELLS_LOOP: DO IC = 1, G%NC
         G%AUX2(IC) = 0.0_EB
         DO ICOL = A%ROW(IC), A%ROW(IC+1)-1                          
            G%AUX2(IC) = G%AUX2(IC) + A%VAL(ICOL)
         ENDDO
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,1I6, 2E14.6)') &
   'FINE LEVEL: IC, A%VAL(ICOL), AUX2:', IC, A%VAL(ICOL), G%AUX2(IC)
#endif
      ENDDO FINE_CELLS_LOOP

   ! on coarser levels NULLSPACE vector was set in preceding coarsening loop to R-vector from QR-decomposition 
   ELSE

      G%AUX1 = G%NULLSPACE
      COARSE_CELLS_LOOP: DO IC = 1, G%NC
   
         G%AUX2(IC) = 0.0_EB
         DO ICOL = A%ROW(IC), A%ROW(IC+1)-1                          
            JC = A%COL(ICOL)
            IF (JC /= 0) THEN
               G%AUX2(IC) =  G%AUX2(IC) + A%VAL(ICOL) * G%AUX1(JC)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,3I6, E14.6)') &
   'RELAXING NULLSPACE:A: IC, ICOL, JC, AUX2:', IC, ICOL, JC, G%AUX2(IC)
#endif
            ELSE 
               JCG = A%COLG(ICOL)
               JC = FINDLOC (G%LOCAL_TO_GLOBAL(1:G%NCE2), VALUE = JCG, DIM = 1)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,3I6,A,I6)') &
   'RELAXING NULLSPACE:B: IC, ICOL, JCG     :', IC, ICOL, JCG, ' Searching, found ', JC
#endif
               IF (JC /= 0) THEN
                  G%AUX2(IC) = G%AUX2(IC) + A%VAL(ICOL) * G%AUX1(JC)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,3I6, E14.6, I6, 2E14.6)') &
   'RELAXING NULLSPACE:C: IC, ICOL, JC, AUX2:', IC, ICOL, JC, G%AUX2(IC), JCG, A%VAL(ICOL), G%AUX1(JC)
#endif
               ELSE
#ifdef WITH_SCARC_VERBOSE
WRITE(MSG%LU_VERBOSE,*) 'RELAX_NULLSPACE: STENCIL FOR IC = ', IC,': GLOBAL LEG CELL ', JCG, ' NOT FOUND!'
#endif
                  JC = FINDLOC (G%ELAYER2_NUMS(1: L%N_LAYER2_TOTAL), VALUE = JCG, DIM = 1)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'RELAX_NULLSPACE: STENCIL FOR IC = ', IC,': GLOBAL LEG CELL ', JCG, ' NOT FOUND, but found ', JC
#endif
                  IF (JC /= 0) G%AUX2(IC) = G%AUX2(IC) + A%VAL(ICOL) * G%ELAYER2_VALS(JC)
#ifdef WITH_SCARC_DEBUG
                  IF (JC /= 0) WRITE(MSG%LU_DEBUG,'(A,3I6, E14.6, I6, 2E14.6)') &
   'RELAXING NULLSPACE:D: IC, ICOL, JC, AUX2:', IC, ICOL, JC, G%AUX2(IC), JCG, A%VAL(ICOL), G%ELAYER2_VALS(JC)
#endif
               ENDIF
            ENDIF
         ENDDO
   
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '======================= IC, G%AUX2(IC) =', IC, G%AUX2(IC)
#endif
      ENDDO COARSE_CELLS_LOOP

   ENDIF
   
   ! Scale it by parameter omega and inverse of diagonal:   d = omega D^{-1} d

   DO IC = 1, G%NC
      G%AUX2(IC) = MG%OMEGA * G%DIAG(IC) * G%AUX2(IC) 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'RELAXING NULLSPACE: IC, DIAG, AUX2:', IC, G%DIAG(IC), G%AUX2(IC)
#endif
   ENDDO
   
  ! Get new iterate:   x = x - d

#ifdef WITH_SCARC_MKL
  CALL DAXPBY(G%NC, -1.0_EB, G%AUX2, 1, 1.0_EB, G%NULLSPACE, 1)
#else
  CALL SCARC_DAXPY_CONSTANT_DOUBLE(G%NC, -1.0_EB, G%AUX2, 1.0_EB, G%NULLSPACE)
#endif
   
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,': ======================================================='
   WRITE(MSG%LU_DEBUG,*) 'OMEGA=',MG%OMEGA
   WRITE(MSG%LU_DEBUG,*) 'RELAX_NULLSPACE: AUX1: '
   WRITE(MSG%LU_DEBUG,'(6E14.6)') G%AUX1(1: G%NC)
   WRITE(MSG%LU_DEBUG,*) '---------------------------------'
   WRITE(MSG%LU_DEBUG,'(6E14.6)') G%AUX1(G%NC+1:G%NCE2)
   WRITE(MSG%LU_DEBUG,*) '======================================================='
   WRITE(MSG%LU_DEBUG,*) 'RELAX_NULLSPACE: AUX2: '
   WRITE(MSG%LU_DEBUG,'(6E14.6)') G%AUX2(1: G%NC)
   WRITE(MSG%LU_DEBUG,*) '---------------------------------'
   WRITE(MSG%LU_DEBUG,'(6E14.6)') G%AUX2(G%NC+1:G%NCE2)
   WRITE(MSG%LU_DEBUG,*) '======================================================='
   WRITE(MSG%LU_DEBUG,*) 'RELAX_NULLSPACE: NULLSPACE: '
   WRITE(MSG%LU_DEBUG,'(6E14.6)') G%NULLSPACE(1: G%NC)
   WRITE(MSG%LU_DEBUG,*) '---------------------------------'
   WRITE(MSG%LU_DEBUG,'(6E14.6)') G%NULLSPACE(G%NC+1:G%NCE2)
#endif

ENDDO

IF (NMESHES > 1) CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_NULLSPACE, NSCARC_NONE, NL)

END SUBROUTINE SCARC_RELAX_NULLSPACE


! ------------------------------------------------------------------------------------------------------
!> \brief Setup basic structure of Prolongation matrix
! This concerns the setting of the number of rows and the column pointers
! The values are still missing and are set in a later step
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_ZONE_OPERATOR(NL)
USE SCARC_POINTERS, ONLY: GF, GC, AF, ZF, SCARC_POINT_TO_MULTIGRID, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NL
INTEGER :: NM, ICC, ICCL, ICCG, IC, IP, N_ROW, N_VAL
LOGICAL :: IS_INCLUDED 

CROUTINE = 'SCARC_SETUP_ZONE_OPERATOR'

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MULTIGRID (NM, NL, NL+1)                       ! Sets grid pointer GF and GC

   AF => SCARC_POINT_TO_CMATRIX (GF, NSCARC_MATRIX_POISSON)
   ZF => SCARC_POINT_TO_CMATRIX (GF, NSCARC_MATRIX_ZONES)

   ! First use very conservative bounds for the size of the zones operator matrix 
   ! reduce it later once the real size is known

   ZF%N_VAL = AF%N_VAL                  
   ZF%N_ROW = AF%N_VAL                  
   CALL SCARC_ALLOCATE_CMATRIX(ZF, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_MINIMAL, 'GF%ZONES', CROUTINE)
  
   ! Again conservative upper bound for length - to be reduced later 

   CALL SCARC_ALLOCATE_INT1(GF%ZONES_LOCAL, 1, GF%NCE2, NSCARC_INIT_ZERO, 'GF%ZONES_LOCAL', CROUTINE)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'NM=',NM,': =================== SETUP_ZONE_OPERATOR:'
WRITE(MSG%LU_DEBUG,*) 'ZF%ROW:', ZF%N_ROW, ZF%N_VAL, SIZE(ZF%ROW), SIZE(ZF%VAL)
WRITE(MSG%LU_DEBUG,*) 'ZONES_LOCAL:'
WRITE(MSG%LU_DEBUG,'(8I6)') GF%ZONES_LOCAL(1:GF%NCE2)
WRITE(MSG%LU_DEBUG,*) 'ZONES_GLOBAL:'
WRITE(MSG%LU_DEBUG,'(8I6)') GF%ZONES_GLOBAL(1:GF%NCE2)
WRITE(MSG%LU_DEBUG,*) '=================== SETUP_ZONE_OPERATOR: FINE'
WRITE(MSG%LU_DEBUG,*) 'SETUP_ZONE_OPERATOR: GF%N_FINE:', GF%N_FINE
WRITE(MSG%LU_DEBUG,*) 'SETUP_ZONE_OPERATOR: GF%N_COARSE:', GF%N_COARSE
WRITE(MSG%LU_DEBUG,*) 'SETUP_ZONE_OPERATOR: GF%NC:', GF%NC
WRITE(MSG%LU_DEBUG,*) 'SETUP_ZONE_OPERATOR: GF%NCE:', GF%NCE
WRITE(MSG%LU_DEBUG,*) 'SETUP_ZONE_OPERATOR: GF%NCE2:', GF%NCE2
WRITE(MSG%LU_DEBUG,*) '=================== SETUP_ZONE_OPERATOR: COARSE'
WRITE(MSG%LU_DEBUG,*) 'SETUP_ZONE_OPERATOR: GC%NC_LOCAL:', GC%NC_LOCAL
WRITE(MSG%LU_DEBUG,*) 'SETUP_ZONE_OPERATOR: GC%NC:', GC%NC
WRITE(MSG%LU_DEBUG,*) 'SETUP_ZONE_OPERATOR: GC%NCE:', GC%NCE
WRITE(MSG%LU_DEBUG,*) 'SETUP_ZONE_OPERATOR: GC%NCE2:', GC%NCE2
WRITE(MSG%LU_DEBUG,*) 'SETUP_ZONE_OPERATOR: GC%NC_OFFSET:', GC%NC_OFFSET
WRITE(MSG%LU_DEBUG,*) 'SETUP_ZONE_OPERATOR: GC%LOCAL_TO_GLOBAL:'
WRITE(MSG%LU_DEBUG,'(8I6)') GC%LOCAL_TO_GLOBAL(1:GC%NCE2)
#endif

   ! Based on global zone numbers determine local zone numbers within mesh

   IP = 1
   ICC = 1
   ZF%ROW(ICC) = 1
   DO ICCL = 1, GC%NCE2
      ICCG = GC%LOCAL_TO_GLOBAL(ICCL)
      IS_INCLUDED = .FALSE.
      DO IC = 1, GF%NCE2
         IF (GF%ZONES_LOCAL(IC) /= ICCL) CYCLE
         IS_INCLUDED = .TRUE.
         ZF%COL(IP)  = IC
         IP = IP + 1
      ENDDO
      IF (IS_INCLUDED) THEN
         ICC = ICC + 1
         ZF%ROW(ICC) = IP
      ENDIF
   ENDDO

   N_ROW = ICC
   N_VAL = IP - 1

   ZF%N_ROW=N_ROW
   ZF%N_VAL=N_VAL

   CALL SCARC_REDUCE_CMATRIX(ZF, 'ZF', CROUTINE)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '------------- NM=',NM
WRITE(MSG%LU_DEBUG,*) 'GF%NCE=',GF%NCE
WRITE(MSG%LU_DEBUG,*) 'GF%N_COARSE=',GF%N_COARSE
WRITE(MSG%LU_DEBUG,*) 'GC%NCE=',GC%NCE
WRITE(MSG%LU_DEBUG,*) 'ZF%N_ROW=',ZF%N_ROW
WRITE(MSG%LU_DEBUG,*) 'ZF%N_VAL=',ZF%N_VAL
CALL SCARC_DEBUG_CMATRIX (ZF, 'ZONES','AFTER SETUP AGGREGATION ZONES 2')
#endif

ENDDO

END SUBROUTINE SCARC_SETUP_ZONE_OPERATOR


! ------------------------------------------------------------------------------------------------------
!> \brief Determine tentative prolongator for current level by computing QR-decomposition of smoothed 
! nullspace vector and set nullspace for next level
! Compute the tentative prolongator, T, which is a tentative interpolation
! matrix from the coarse-grid to the fine-grid.  T exactly interpolates  B_fine = T B_coarse.
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_PROLONGATION_AMG(NL)
USE SCARC_POINTERS, ONLY: S, L, G, OG, A, OA, P, OP, GF, OGF, GC, PF, OPF, Z, MG, &
                          SCARC_POINT_TO_GRID, SCARC_POINT_TO_OTHER_GRID, SCARC_POINT_TO_MULTIGRID, &
                          SCARC_POINT_TO_CMATRIX, SCARC_POINT_TO_OTHER_CMATRIX
INTEGER, INTENT(IN) :: NL
REAL(EB):: DSUM, SCAL !, TOL = 1.0E-12_EB
INTEGER :: NM, NOM, IC, JC, ICC, ICC0, ICOL, ICCOL, JCCOL, IP0, IP, JCC, IQ, INBR, NLEN

CROUTINE = 'SCARC_SETUP_PROLONGATION'

! Allocate several workspaces (with conservative bounds which will be reduced later)
!    - Prolongation matrix on internal part of mesh 
!    - for every neighbor small Prolongation matrix for corresponding overlap
!    - vectors Q and R for QR-decomposition of aggregation zones operator
! Initialize QR-decomposition
 
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

   A => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_POISSON)
   P => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_PROLONGATION)
   Z => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_ZONES)

   P%N_VAL = A%N_VAL + 1        
   P%N_ROW = G%NCE + 1                  
   CALL SCARC_ALLOCATE_CMATRIX(P, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_FULL, 'G%PROLONGATION', CROUTINE)

   DO INBR = 1, S%N_NEIGHBORS

      NOM = S%NEIGHBORS(INBR)
      CALL SCARC_POINT_TO_OTHER_GRID(NM, NOM, NL)

      OA => SCARC_POINT_TO_OTHER_CMATRIX(OG, NSCARC_MATRIX_POISSON)
      OP => SCARC_POINT_TO_OTHER_CMATRIX(OG, NSCARC_MATRIX_PROLONGATION)

      OP%N_VAL = OA%N_VAL + 1              ! TODO : CHECK : MUCH TOO BIG !!!
      OP%N_ROW = G%NCE + 1                 ! TODO : CHECK : MUCH TOO BIG !!!
      CALL SCARC_ALLOCATE_CMATRIX(OP, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_FULL, 'OG%PROLONGATION', CROUTINE)

   ENDDO

   CALL SCARC_ALLOCATE_REAL1(G%QQ, 1, G%NCE2+1, NSCARC_INIT_ZERO, 'G%QQ', CROUTINE)
   CALL SCARC_ALLOCATE_REAL1(G%RR, 1, G%NCE2+1, NSCARC_INIT_ZERO, 'G%RR', CROUTINE)     ! TODO check length!

 
   ! Copy blocks into Q according to aggregation zones and compute norms for single ZONES
   ! In each cell corresponding to a single zone, store square-sum of entries
 
   IQ = 1
   G%AUX1 = 0.0_EB
   DO ICC = 1, Z%N_ROW-1
      DSUM = 0.0_EB
      DO ICOL = Z%ROW(ICC), Z%ROW(ICC+1)-1
         IC = Z%COL(ICOL)
         G%QQ(IQ) = G%NULLSPACE(IC)
         DSUM = DSUM + G%QQ(IQ)**2
         IQ = IQ + 1
      ENDDO
      DO ICOL = Z%ROW(ICC), Z%ROW(ICC+1)-1
         G%AUX1(Z%COL(ICOL)) = DSUM
      ENDDO
      G%RR(ICC) = DSUM
   ENDDO

   CALL SCARC_REDUCE_REAL1(G%QQ, IQ, 'G%QQ', CROUTINE)
   CALL SCARC_REDUCE_REAL1(G%RR, Z%N_ROW, 'G%RR', CROUTINE)

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,': ================================ PART 0 :'
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,':Z%N_ROW:', Z%N_ROW
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,':G%NULLSPACE:'
   WRITE(MSG%LU_DEBUG,'(6E14.6)') G%NULLSPACE
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,':G%AUX2:'
   WRITE(MSG%LU_DEBUG,'(6E14.6)') G%AUX2
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,':G%QQ:'
   WRITE(MSG%LU_DEBUG,'(6E14.6)') G%QQ
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,':G%AUX1:'
   WRITE(MSG%LU_DEBUG,'(6E14.6)') G%AUX1
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,':G%RR:'
   WRITE(MSG%LU_DEBUG,'(6E14.6)') G%RR
#endif

ENDDO

 
! Exchange sums of nullspace entries within single aggregation zones
 
IF (NMESHES > 1) CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_AUXILIARY, NSCARC_NONE, NL)
   
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,':================================ PART 1 :'
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,':G%QQ:'
   WRITE(MSG%LU_DEBUG,'(6E14.6)') G%QQ
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,':G%AUX1:'
   WRITE(MSG%LU_DEBUG,'(6E14.6)') G%AUX1
#endif

 
! Build norms over single zones and scale Q-entries by norms
 
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                          
   Z => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_ZONES)

   DO ICC = 1, Z%N_ROW-1
      ICOL = Z%ROW(ICC)
      IC = Z%COL(ICOL)
      G%RR(ICC) = SQRT(G%AUX1(IC))
   ENDDO

   IQ = 1
   DO ICC = 1, Z%N_ROW-1
      DO ICOL = Z%ROW(ICC), Z%ROW(ICC+1)-1
         IC = Z%COL(ICOL)
         G%QQ(IQ) = G%QQ(IQ)/G%RR(ICC)
         IQ = IQ + 1
      ENDDO
   ENDDO

ENDDO
   
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,':================================ PART 2 :'
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,':G%QQ:'
   WRITE(MSG%LU_DEBUG,'(6E14.6)') G%QQ
#endif

! ----------- Relax prolongator:
! Smooth the tentative prolongator, so that it's accuracy is greatly improved for algebraically smooth error.
! Compute:                P =  P - A_Dinv * P   
! with:                   A_Dinv = 4/3 * 1/rho * D^{-1} A   
 
! First step: Compute P_0: = A_Dinv * Q
 
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                ! Sets grid pointer G

   A => SCARC_POINT_TO_CMATRIX (G, NSCARC_MATRIX_POISSON)
   P => SCARC_POINT_TO_CMATRIX (G, NSCARC_MATRIX_PROLONGATION)
   Z => SCARC_POINT_TO_CMATRIX (G, NSCARC_MATRIX_ZONES)

   MG => L%MG
   MG%OMEGA = 4.0_EB/3.0_EB                                         ! currently used default
   IF (SCARC_MULTIGRID_RELAXING) THEN
      SCAL = MG%OMEGA/MG%APPROX_SPECTRAL_RADIUS                     ! for testing purposes rho is set to 2 currently
   ELSE
      SCAL = 0.0_EB
   ENDIF
   
   IP = 1
   IP0 = IP
   P%ROW(1) = IP
   DO IC = 1, G%NC
      DO ICC = 1, Z%N_ROW-1
   
         DSUM = 0.0_EB
         DO ICCOL = Z%ROW(ICC), Z%ROW(ICC+1)-1
            JC = Z%COL(ICCOL)
            ICOL = SCARC_MATCH_MATRIX_COLUMN(A, IC, JC)
            IF (ICOL /= -1) THEN
               ICC0 = ICC
               DSUM = DSUM - SCAL * G%DIAG(IC) * A%VAL(ICOL) * G%QQ(ICCOL)
            ENDIF
         ENDDO
   
         IF (ABS(DSUM) /= 0.0_EB) THEN
            P%VAL(IP) = DSUM
            P%COL(IP) = ICC
            IP = IP + 1
         ENDIF
   
      ENDDO
 
      ! take care that at least one entry per fine cell is generated
      IF (IP == IP0) THEN
         P%VAL(IP) = 0.0_EB
         P%COL(IP) = ICC0
         IP = IP + 1
      ENDIF
      IP0 = IP

      P%ROW(IC+1) = IP
   ENDDO
 
   P%N_VAL = IP - 1

ENDDO
   
 
! Second step: Compute P: = P - P_0
 
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
   P => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_PROLONGATION)
   Z => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_ZONES)

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'P%N_VAL=',P%N_VAL
   WRITE(MSG%LU_DEBUG,*) 'P%N_ROW=',P%N_ROW
   CALL SCARC_DEBUG_CMATRIX(Z, 'ZONES','AFTER RESORT PROL ')
   CALL SCARC_DEBUG_CMATRIX(P, 'PROLONGATION','AFTER RESORT PROL ')
#endif

   DO ICC = 1, Z%N_ROW-1
      DO ICCOL = Z%ROW(ICC), Z%ROW(ICC+1) - 1
         IC = Z%COL(ICCOL)

         IF (IC > G%NC) CYCLE
         DO JCCOL = P%ROW(IC), P%ROW(IC+1) - 1
            JCC = P%COL(JCCOL)
            IF (JCC == ICC) THEN
               P%VAL(JCCOL) = P%VAL(JCCOL) + G%QQ(ICCOL)
               CYCLE
            ENDIF
         ENDDO
      ENDDO
   ENDDO
   
#ifdef WITH_SCARC_DEBUG
   CALL SCARC_DEBUG_CMATRIX(P, 'G%PROLONGATION','SETUP_PROLONGATION: AFTER RELAX STEP, BEFORE EXCHANGE ')
#endif
ENDDO

! Determine global columns array for Prolongation matrix
 
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MULTIGRID (NM, NL, NL+1)                
   PF => SCARC_POINT_TO_CMATRIX(GF, NSCARC_MATRIX_PROLONGATION)

   DO IC = 1, GF%NC
      DO JCCOL = PF%ROW(IC), PF%ROW(IC+1) - 1
         JCC = PF%COL(JCCOL)
         PF%COLG(JCCOL) = GC%LOCAL_TO_GLOBAL(JCC)
      ENDDO
   ENDDO
   
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '----------- NM =',NM,': NL=',NL
   CALL SCARC_DEBUG_CMATRIX(PF, 'G%PROLONGATION','SETUP_PROLONGATION: AFTER LAST EXCHANGE')
#endif

ENDDO

 
! Exchange resulting columns and values of Prolongation matrix and extract exchanged data from 
! overlapping parts with single neighbors and attach them to main matrix
 
IF (NMESHES > 1) THEN
   CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MATRIX_COLS,  NSCARC_MATRIX_PROLONGATION, NL)
   CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MATRIX_COLSG, NSCARC_MATRIX_PROLONGATION, NL)
   CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MATRIX_VALS,  NSCARC_MATRIX_PROLONGATION, NL)
   CALL SCARC_EXTRACT_MATRIX_OVERLAPS(NSCARC_MATRIX_PROLONGATION, 0, NL)
ENDIF
   
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MULTIGRID (NM, NL, NL+1) 
   PF => SCARC_POINT_TO_CMATRIX(GF, NSCARC_MATRIX_PROLONGATION)

   NLEN = MAX(GC%NCE2, 4 * (GC%NCE2 - GC%NC + 2) + 50)           ! TODO check length
   CALL SCARC_ALLOCATE_INT1 (GC%CELLS_LOCAL,  1, NLEN, NSCARC_INIT_ZERO, 'GC%CELLS_LOCAL',  CROUTINE)
   CALL SCARC_ALLOCATE_INT1 (GC%CELLS_GLOBAL, 1, NLEN, NSCARC_INIT_ZERO, 'GC%CELLS_GLOBAL', CROUTINE)

   CALL SCARC_GET_CELL_DEPENDENCIES_GALERKIN(GC, GF, PF, NLEN)

   CALL SCARC_REDUCE_INT1(GC%CELLS_LOCAL, GC%NC_GALERKIN, 'GC%CELLS_LOCAL', CROUTINE)
   CALL SCARC_REDUCE_INT1(GC%CELLS_GLOBAL, GC%NC_GALERKIN, 'GC%CELLS_GLOBAL', CROUTINE)

   CALL SCARC_REDUCE_CMATRIX(PF, 'P%PROLONGATION', CROUTINE)
   DO INBR = 1, S%N_NEIGHBORS
      NOM = S%NEIGHBORS(INBR)
      CALL SCARC_POINT_TO_OTHER_GRID(NM, NOM, NL)
      OPF => SCARC_POINT_TO_OTHER_CMATRIX(OGF, NSCARC_MATRIX_PROLONGATION)
      CALL SCARC_REDUCE_CMATRIX(OPF, 'OP%PROLONGATION', CROUTINE)
   ENDDO

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '======================= LEVEL ', NL
   CALL SCARC_DEBUG_CMATRIX(PF, 'GF%PROLONGATION','SETUP_PROLONGATION: FINAL')
#endif

ENDDO

END SUBROUTINE SCARC_SETUP_PROLONGATION_AMG


! -------------------------------------------------------------------------------------------
!> \brief Setup restriction and prolongation matrices in case of GMG-like coarsening
! -------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_TRANSFER_GMG(NL)
USE SCARC_POINTERS, ONLY: S, LC, LF, GF, GC, AF, RF, ZF, PF, OPF, OGF, &
                          SCARC_POINT_TO_OTHER_GRID, SCARC_POINT_TO_MULTIGRID, &
                          SCARC_POINT_TO_CMATRIX, SCARC_POINT_TO_OTHER_CMATRIX
INTEGER, INTENT(IN) :: NL
INTEGER :: NM, IC, JC, ICC, ICF, ICCOL, IRCOL, IP, JCC, JCCOL, NOM, INBR, NLEN
INTEGER :: IS, IXC, IZC, IXF, IZF, IOFFX, IOFFZ, ICC0(4)
INTEGER :: STENCIL(16) = 0
INTEGER :: RN, RE, RS, RW

CROUTINE = 'SCARC_SETUP_PROLONGATION_GMG'

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MULTIGRID (NM, NL, NL + 1)                                   ! Sets grid pointer G

   AF => SCARC_POINT_TO_CMATRIX(GF, NSCARC_MATRIX_POISSON)
   ZF => SCARC_POINT_TO_CMATRIX(GF, NSCARC_MATRIX_ZONES)
   RF => SCARC_POINT_TO_CMATRIX(GF, NSCARC_MATRIX_RESTRICTION)
   PF => SCARC_POINT_TO_CMATRIX(GF, NSCARC_MATRIX_PROLONGATION)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'TRANSFER_GMG: GF%NC=', GF%NC
WRITE(MSG%LU_DEBUG,*) 'TRANSFER_GMG: GC%NC=', GC%NC
WRITE(MSG%LU_DEBUG,*) 'TRANSFER_GMG: TYPE_INTERPOL=', TYPE_INTERPOL
#endif

   SELECT CASE (TYPE_INTERPOL)

      CASE (NSCARC_INTERPOL_CONSTANT)

         RF%N_VAL = GF%NCE2
         RF%N_ROW = GC%NCE2 + 1
         CALL SCARC_ALLOCATE_CMATRIX(RF, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_FULL, 'GF%RESTRICTION', CROUTINE)

         PF%N_VAL = AF%N_VAL + 1        
         PF%N_ROW = GF%NCE2 + 1
         CALL SCARC_ALLOCATE_CMATRIX(PF, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_FULL, 'GF%PROLONGATION', CROUTINE)

         IP = 1
         RF%ROW(1) = IP
         DO ICC = 1, ZF%N_ROW-1
            DO ICCOL = ZF%ROW(ICC), ZF%ROW(ICC+1)-1
               JC = ZF%COL(ICCOL)
               RF%COL(IP) = ZF%COL(ICCOL)
               RF%COLG(IP) = GF%LOCAL_TO_GLOBAL(ZF%COL(ICCOL))
               RF%VAL(IP) = 1.00_EB
               IP = IP + 1
            ENDDO
            RF%ROW(ICC + 1) = IP
            !WRITE(*,*) ICC,':SUM(RF)=',SUM(RF%VAL(RF%ROW(ICC):RF%ROW(ICC+1)-1))
         ENDDO
      
         IP = 1
         PF%ROW(1) = IP
         DO IC = 1, GF%NC
            DO ICC = 1, ZF%N_ROW -1
               COLUMN_LOOP: DO ICCOL = ZF%ROW(ICC), ZF%ROW(ICC+1)-1
                  IF (ZF%COL(ICCOL) == IC) THEN
                     PF%COL(IP) = ICC
                     PF%COLG(IP) = GC%LOCAL_TO_GLOBAL(ICC)
                     PF%VAL(IP) = 0.25_EB
                     IP = IP + 1
                     EXIT COLUMN_LOOP
                  ENDIF
               ENDDO COLUMN_LOOP
            ENDDO
            PF%ROW(IC + 1) = IP
            !WRITE(*,*) ICC,':SUM(PF)=',SUM(PF%VAL(PF%ROW(IC):PF%ROW(IC+1)-1))
         ENDDO

      CASE (NSCARC_INTERPOL_BILINEAR)

         RF%N_VAL = 16*GF%NC
         RF%N_ROW = GC%NC + 1
         CALL SCARC_ALLOCATE_CMATRIX(RF, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_FULL, 'GF%RESTRICTION', CROUTINE)

         PF%N_VAL = AF%N_VAL + 1        
         PF%N_ROW = GF%NC + 1
         CALL SCARC_ALLOCATE_CMATRIX(PF, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_FULL, 'GF%PROLONGATION', CROUTINE)

         IP = 1
         RF%ROW(1) = IP
         DO IZC = 1, LC%NZ
            DO IXC = 1, LC%NX

               ICC = (IZC - 1) * LC%NX + IXC

               IXF = 2 * IXC
               IZF = 2 * IZC
               
               IF (IXC == 1 .AND. IZC == 1) THEN
                  RN = 1; RE = 1; RS = 0; RW = 0
               ELSE IF (IXC == 1 .AND. IZC == LC%NZ) THEN
                  RN = 0; RE = 1; RS = 1; RW = 0
               ELSE IF (IXC == LC%NX .AND. IZC == 1) THEN
                  RN = 1; RE = 0; RS = 0; RW = 1
               ELSE IF (IXC == LC%NX .AND. IZC == LC%NZ) THEN
                  RN = 0; RE = 0; RS = 1; RW = 1
               ELSE IF (IXC == 1) THEN
                  RN = 1; RE = 1; RS = 1; RW = 0
               ELSE IF (IXC == LC%NX) THEN
                  RN = 1; RE = 0; RS = 1; RW = 1
               ELSE IF (IZC == 1) THEN
                  RN = 1; RE = 1; RS = 0; RW = 1
               ELSE IF (IZC == LC%NZ) THEN
                  RN = 0; RE = 1; RS = 1; RW = 1
               ELSE
                  RN = 1; RE = 1; RS = 1; RW = 1
               ENDIF

               STENCIL(13:16) =  (/    RN *RW,    RN *(2+RW),    RN *(2+RE),    RN *RE /)
               STENCIL( 9:12) =  (/ (2+RN)*RW, (2+RN)*(2+RW), (2+RN)*(2+RE), (2+RN)*RE /)
               STENCIL( 5: 8) =  (/ (2+RS)*RW, (2+RS)*(2+RW), (2+RS)*(2+RE), (2+RS)*RE /)
               STENCIL( 1: 4) =  (/    RS *RW,    RS *(2+RW),    RS *(2+RE),    RS *RE /)

               IS = 1
               DO IOFFZ = -2, 1
                  DO IOFFX = -2, 1
                     CALL PROCESS_FINE_CELL (IXF + IOFFX, 1, IZF + IOFFZ, IP, STENCIL(IS))
                     IS = IS + 1
                  ENDDO
               ENDDO

               RF%ROW(ICC + 1) = IP
               !WRITE(*,*) ICC,':SUM(RF)=',SUM(RF%VAL(RF%ROW(ICC):RF%ROW(ICC+1)-1))


            ENDDO
         ENDDO

         IP = 1
         PF%ROW(1) = IP
         DO IC = 1, GF%NC
            DO ICC = 1, GC%NC 
               COLUMN_LOOP2: DO IRCOL = RF%ROW(ICC), RF%ROW(ICC+1)-1
                  IF (RF%COL(IRCOL) == IC) THEN
                     PF%COL(IP) = ICC
                     PF%COLG(IP) = GC%LOCAL_TO_GLOBAL(ICC)
                     PF%VAL(IP) = RF%VAL(IRCOL)
                     IP = IP + 1
                     EXIT COLUMN_LOOP2
                  ENDIF
               ENDDO COLUMN_LOOP2
            ENDDO
            PF%ROW(IC + 1) = IP
            !WRITE(*,*) ICC,':SUM(PF)=',SUM(PF%VAL(PF%ROW(IC):PF%ROW(IC+1)-1))
         ENDDO

         PF%VAL = PF%VAL/16.0_EB
         IF (TWO_D) THEN
            RF%VAL = RF%VAL/4.0_EB
         ELSE
            RF%VAL = RF%VAL/2.0_EB
         ENDIF

      CASE (NSCARC_INTERPOL_BILINEAR2)

         RF%N_VAL = 16*GF%NC
         RF%N_ROW = GC%NC + 1
         CALL SCARC_ALLOCATE_CMATRIX(RF, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_FULL, 'GF%RESTRICTION', CROUTINE)

         PF%N_VAL = 4* GF%NC
         PF%N_ROW = GF%NC + 1
         CALL SCARC_ALLOCATE_CMATRIX(PF, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_FULL, 'GF%PROLONGATION', CROUTINE)

         IP = 1
         PF%ROW(1) = IP
         DO IZF = 1, LF%NZ
            DO IXF = 1, LF%NX

               ICF = GF%CELL_NUMBER(IXF, 1, IZF)

               !IXC = CEILING(REAL((IXF+1)/2),EB) 
               !IZC = CEILING(REAL((IZF+1)/2),EB)
               IXC = MOD(IXF,2) + 1
               IZC = MOD(IZF,2) + 1

               ICC0(1) = (IZC - 1) * LC%NX + IXC
               ICC0(2) = (IZC - 1) * LC%NX + IXC + 1
               ICC0(3) = IZC * LC%NX + IXC
               ICC0(4) = IZC * LC%NX + IXC + 1

#ifdef WITH_SCARC_DEBUG
 WRITE(MSG%LU_DEBUG,*) 'IXF, IZF, ICF, IXC, IZC, ICC0(1:4):', IXF, IZF, ICF, IXC, IZC, ICC0(1:4)
#endif
 WRITE(*,*) 'IXF, IZF, ICF, IXC, IZC, ICC0(1:4):', IXF, IZF, ICF, IXC, IZC, ICC0(1:4)
               
 IF (IXC /= 0 .AND. IZC /= 0) CALL PROCESS_COARSE_CELL (ICC0(1), IP, 9)
 IF (IXC /= 0 .AND. IZC /= 0) CALL PROCESS_COARSE_CELL (ICC0(2), IP, 3)
 IF (IXC /= 0 .AND. IZC /= 0) CALL PROCESS_COARSE_CELL (ICC0(3), IP, 3)
 IF (IXC /= 0 .AND. IZC /= 0) CALL PROCESS_COARSE_CELL (ICC0(4), IP, 1)

               PF%ROW(ICF + 1) = IP

            ENDDO
         ENDDO

         IP = 1
         PF%ROW(1) = IP
         DO IC = 1, GF%NC
            DO ICC = 1, GC%NC 
               COLUMN_LOOP3: DO IRCOL = RF%ROW(ICC), RF%ROW(ICC+1)-1
                  IF (RF%COL(IRCOL) == IC) THEN
                     PF%COL(IP) = ICC
                     PF%COLG(IP) = GC%LOCAL_TO_GLOBAL(ICC)
                     PF%VAL(IP) = RF%VAL(IRCOL)
                     IP = IP + 1
                     EXIT COLUMN_LOOP3
                  ENDIF
               ENDDO COLUMN_LOOP3
            ENDDO
            PF%ROW(IC + 1) = IP
            WRITE(*,*) ICC,':SUM(PF)=',SUM(PF%VAL(PF%ROW(IC):PF%ROW(IC+1)-1))
         ENDDO

         PF%VAL = PF%VAL/16.0_EB
         IF (TWO_D) THEN
            RF%VAL = RF%VAL/4.0_EB
         ELSE
            RF%VAL = RF%VAL/2.0_EB
         ENDIF

   END SELECT

#ifdef WITH_SCARC_DEBUG
   CALL SCARC_DEBUG_CMATRIX(RF, 'RESTRICTION','IN TRANSFER GMG')
   CALL SCARC_DEBUG_CMATRIX(PF, 'PROLONGATION','IN TRANSFER GMG')
#endif

ENDDO

! Determine global columns array for Prolongation matrix
 
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MULTIGRID (NM, NL, NL+1)                
   PF => SCARC_POINT_TO_CMATRIX(GF, NSCARC_MATRIX_PROLONGATION)

   DO IC = 1, GF%NC
      DO JCCOL = PF%ROW(IC), PF%ROW(IC+1) - 1
         JCC = PF%COL(JCCOL)
         PF%COLG(JCCOL) = GC%LOCAL_TO_GLOBAL(JCC)
      ENDDO
   ENDDO
   
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '----------- NM =',NM,': NL=',NL
   CALL SCARC_DEBUG_CMATRIX(PF, 'G%PROLONGATION','SETUP_PROLONGATION: AFTER LAST EXCHANGE')
#endif

ENDDO

 
! Exchange resulting columns and values of Prolongation matrix and extract exchanged data from 
! overlapping parts with single neighbors and attach them to main matrix
 
IF (NMESHES > 1 .AND. TYPE_COARSENING /= NSCARC_COARSENING_GMG) THEN
   CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MATRIX_COLS,  NSCARC_MATRIX_PROLONGATION, NL)
   CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MATRIX_COLSG, NSCARC_MATRIX_PROLONGATION, NL)
   CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MATRIX_VALS,  NSCARC_MATRIX_PROLONGATION, NL)
   CALL SCARC_EXTRACT_MATRIX_OVERLAPS(NSCARC_MATRIX_PROLONGATION, 0, NL)
ENDIF
   
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MULTIGRID (NM, NL, NL+1) 
   PF => SCARC_POINT_TO_CMATRIX(GF, NSCARC_MATRIX_PROLONGATION)

   NLEN = MAX(GC%NCE2, 4 * (GC%NCE2 - GC%NC + 2) + 50)           ! TODO check length
   CALL SCARC_ALLOCATE_INT1 (GC%CELLS_LOCAL,  1, NLEN, NSCARC_INIT_ZERO, 'GC%CELLS_LOCAL',  CROUTINE)
   CALL SCARC_ALLOCATE_INT1 (GC%CELLS_GLOBAL, 1, NLEN, NSCARC_INIT_ZERO, 'GC%CELLS_GLOBAL', CROUTINE)

   CALL SCARC_GET_CELL_DEPENDENCIES_GALERKIN(GC, GF, PF, NLEN)

   CALL SCARC_REDUCE_INT1(GC%CELLS_LOCAL, GC%NC_GALERKIN, 'GC%CELLS_LOCAL', CROUTINE)
   CALL SCARC_REDUCE_INT1(GC%CELLS_GLOBAL, GC%NC_GALERKIN, 'GC%CELLS_GLOBAL', CROUTINE)

   CALL SCARC_REDUCE_CMATRIX(PF, 'P%PROLONGATION', CROUTINE)
   IF (TYPE_COARSENING /= NSCARC_COARSENING_GMG) THEN
      DO INBR = 1, S%N_NEIGHBORS
         NOM = S%NEIGHBORS(INBR)
         CALL SCARC_POINT_TO_OTHER_GRID(NM, NOM, NL)
         OPF => SCARC_POINT_TO_OTHER_CMATRIX(OGF, NSCARC_MATRIX_PROLONGATION)
         CALL SCARC_REDUCE_CMATRIX(OPF, 'OP%PROLONGATION', CROUTINE)
      ENDDO
   ENDIF

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '======================= LEVEL ', NL
   CALL SCARC_DEBUG_CMATRIX(PF, 'GF%PROLONGATION','SETUP_PROLONGATION: FINAL')
#endif

ENDDO

END SUBROUTINE SCARC_SETUP_TRANSFER_GMG

SUBROUTINE PROCESS_FINE_CELL(IXF, IYF, IZF, IP, VAL)
USE SCARC_POINTERS, ONLY: RF, GF
INTEGER, INTENT(IN) :: IXF, IYF, IZF, VAL
INTEGER, INTENT(INOUT) :: IP
IF (VAL == 0) RETURN
RF%COL(IP) = ICF (IXF, IYF, IZF)
RF%COLG(IP) = GF%LOCAL_TO_GLOBAL(RF%COL(IP))
RF%VAL(IP) = VAL
IP = IP + 1
END SUBROUTINE PROCESS_FINE_CELL

SUBROUTINE PROCESS_COARSE_CELL(ICC, IP, VAL)
USE SCARC_POINTERS, ONLY: PF, GC
INTEGER, INTENT(IN) :: ICC, VAL
INTEGER, INTENT(INOUT) :: IP
IF (VAL == 0) RETURN
PF%COL(IP) = ICC
PF%COLG(IP) = GC%LOCAL_TO_GLOBAL(ICC)
PF%VAL(IP) = VAL
IP = IP + 1
END SUBROUTINE PROCESS_COARSE_CELL

INTEGER FUNCTION ICF (IXF, IYF, IZF)
USE SCARC_POINTERS, ONLY : LF
INTEGER, INTENT(IN) :: IXF, IYF, IZF
ICF = (IZF - 1) * LF%NX * LF%NY + (IYF - 1) * LF%NX + IXF
RETURN
END FUNCTION

INTEGER FUNCTION ICC (IXC, IYC, IZC)
USE SCARC_POINTERS, ONLY : LC
INTEGER, INTENT(IN) :: IXC, IYC, IZC
ICC = (IZC - 1) * LC%NX * LC%NY + (IYC - 1) * LC%NX + IXC
RETURN
END FUNCTION

! -------------------------------------------------------------------------------------------
!> \brief Determine on which overlapping global coarse cells are given mesh depends (also considering diagonal connections)
! -------------------------------------------------------------------------------------------
SUBROUTINE SCARC_GET_CELL_DEPENDENCIES_GALERKIN(GC, GF, PF, NLEN)
TYPE (SCARC_GRID_TYPE), POINTER, INTENT(IN) :: GC, GF
TYPE (SCARC_CMATRIX_TYPE), POINTER, INTENT(IN) :: PF
INTEGER, INTENT(IN) :: NLEN
INTEGER :: IZL, IZG, IP, ICOL, IC, IFOUND1, IFOUND2, IFOUND3

IP = 1
PROLONGATION_CELLS_LOOP: DO IC = 1, GF%NCE

   ! Check if zone number used in given row of Prolongation matrix is already accounted for
   DO ICOL = PF%ROW(IC), PF%ROW(IC+1) - 1

      IZL = PF%COL(ICOL)
      IZG = PF%COLG(ICOL)

      IFOUND1 = FINDLOC (GC%CELLS_GLOBAL(1:NLEN),  VALUE = IZG, DIM = 1)
      IFOUND2 = FINDLOC (GF%ZONES_GLOBAL(1:GF%NC), VALUE = IZG, DIM = 1)
      IFOUND3 = FINDLOC (GC%LOCAL_TO_GLOBAL(1:GC%NCE2), VALUE = IZG, DIM = 1)
      IF (IFOUND1 <= 0 .AND. IFOUND2 <= 0) THEN  
         GC%CELLS_LOCAL(IP)  = IFOUND3
         GC%CELLS_GLOBAL(IP) = IZG
         IP = IP + 1
      ENDIF
   ENDDO

ENDDO PROLONGATION_CELLS_LOOP
GC%NC_GALERKIN = IP - 1

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'GET_CELL_DEPENDENCIES: GC%NC=',GC%NC
WRITE(MSG%LU_DEBUG,*) 'GET_CELL_DEPENDENCIES: GC%NCE=',GC%NCE
WRITE(MSG%LU_DEBUG,*) 'GET_CELL_DEPENDENCIES: GC%NCE2=',GC%NCE2
WRITE(MSG%LU_DEBUG,*) 'GET_CELL_DEPENDENCIES: GC%NC_GALERKIN=', GC%NC_GALERKIN
WRITE(MSG%LU_DEBUG,*) 'GET_CELL_DEPENDENCIES: GC%CELLS_LOCAL'
WRITE(MSG%LU_DEBUG,'(8I6)') GC%CELLS_LOCAL(1:NLEN)
WRITE(MSG%LU_DEBUG,*) 'GET_CELL_DEPENDENCIES: GC%CELLS_GLOBAL'
WRITE(MSG%LU_DEBUG,'(8I6)') GC%CELLS_GLOBAL(1:NLEN)
#endif

END SUBROUTINE SCARC_GET_CELL_DEPENDENCIES_GALERKIN


! ------------------------------------------------------------------------------------------------------
!> \brief Define nullspace for next coarser level, if coarsest level isn't reached yet
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_NULLSPACE_COARSE(NL)
USE SCARC_POINTERS, ONLY: GC, GF, ZF, SCARC_POINT_TO_MULTIGRID, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NL
INTEGER :: NM

CROUTINE = 'SCARC_SETUP_NULLSPACE_COARSE'

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MULTIGRID(NM, NL, NL+1)                   ! Sets pointers GC and GF
   ZF => SCARC_POINT_TO_CMATRIX (GF, NSCARC_MATRIX_ZONES)

   IF (NL < NLEVEL_MAX) THEN
      GC%N_FINE = GC%NC_LOCAL(NM)
      CALL SCARC_ALLOCATE_REAL1(GC%NULLSPACE, 1, GC%NCE, NSCARC_INIT_ZERO, 'GC%NULLSPACE', CROUTINE)
      GC%NULLSPACE(1:GC%NCE) = GF%RR(1:GC%NCE)
      CALL SCARC_REDUCE_INT1(GC%LOCAL_TO_GLOBAL, GC%NCE, 'GC%LOCAL_TO_GLOBAL', CROUTINE)
   ENDIF

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,'============== SETUP_NULLSPACE_COARSE ================='
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,': GF%NULLSPACE:'
   WRITE(MSG%LU_DEBUG,'(6E14.6)') GF%NULLSPACE
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,': GF%RR'
   WRITE(MSG%LU_DEBUG,'(6E14.6)') GF%RR
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,': GF%QR:'
   WRITE(MSG%LU_DEBUG,'(6E14.6)') GF%QQ
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,': GC%LOCAL_TO_GLOBAL:'
   WRITE(MSG%LU_DEBUG,'(8I6)') GC%LOCAL_TO_GLOBAL
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,': GC%N_FINE:', GC%N_FINE
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,': GC%NC:', GC%NC
   WRITE(MSG%LU_DEBUG,*) 'NM=',NM,': GC%NCE:', GC%NCE
   WRITE(MSG%LU_DEBUG,*) '==============================================='
#endif

ENDDO
   
END SUBROUTINE SCARC_SETUP_NULLSPACE_COARSE


! ------------------------------------------------------------------------------------------------------
!> \brief Determine which columns of system matrix are involved in multiplication with tentative prolongator
! ------------------------------------------------------------------------------------------------------
INTEGER FUNCTION SCARC_MATCH_MATRIX_COLUMN(A, IC, JC)
TYPE (SCARC_CMATRIX_TYPE), POINTER :: A
INTEGER, INTENT(IN) :: IC, JC
INTEGER :: ICOL
SCARC_MATCH_MATRIX_COLUMN = -1
DO ICOL = A%ROW(IC), A%ROW(IC+1)-1
   IF (A%COL(ICOL) == JC) THEN
      SCARC_MATCH_MATRIX_COLUMN = ICOL
      RETURN 
   ENDIF
ENDDO
END FUNCTION SCARC_MATCH_MATRIX_COLUMN


! ------------------------------------------------------------------------------------------------------
!> \brief Setup Restriction matrix: Build transpose of Prolongation matrix
! Compute the Restriction matrix, R, which interpolates from the fine-grid to the coarse-grid.
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_RESTRICTION(NL)
USE SCARC_POINTERS, ONLY: GC, GF, RF, PF, SCARC_POINT_TO_MULTIGRID, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NL
INTEGER :: NM, IC, IRROW, IRCOL, IPCOL, IPC, ICCL, ICCG, IFOUND 
LOGICAL :: IS_INCLUDED

CROUTINE = 'SCARC_SETUP_RESTRICTION'

! Allocate Restriction matrix R on internal mesh part
MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MULTIGRID (NM, NL, NL+1)            ! Sets pointers GF and GC to fine and coarse level

   PF => SCARC_POINT_TO_CMATRIX (GF, NSCARC_MATRIX_PROLONGATION)
   RF => SCARC_POINT_TO_CMATRIX (GF, NSCARC_MATRIX_RESTRICTION)

   RF%N_VAL = PF%N_VAL + 100
   RF%N_ROW = PF%N_ROW + 100

   CALL SCARC_ALLOCATE_CMATRIX(RF, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_FULL, 'GF%RESTRICTION', CROUTINE)

   IRROW = 1                             ! counter of current row of Restriction matrix - corresponds to coarse cells
   IRCOL = 1                             ! counter of current column of Restriction matrix
   RF%ROW(IRROW) = IRCOL

   LOCAL_COARSE_CELLS_LOOP: DO ICCL = 1, GC%NCE

      ICCG = GC%LOCAL_TO_GLOBAL(ICCL)                     ! corresponding global coarse cell

      IFOUND = -1
      IFOUND = FINDLOC(PF%COLG, VALUE = ICCG, DIM=1)
      IF (IFOUND == -1) CYCLE

      IS_INCLUDED = .FALSE.

      FINE_CELLS_LOOP: DO IC = 1, GF%NCE                  ! counter of fine cell (including overlaps)

         ROW_LOOP: DO IPCOL = PF%ROW(IC), PF%ROW(IC+1)-1
            IPC = PF%COLG(IPCOL)
            IF (IPC == ICCG) THEN
               IS_INCLUDED = .TRUE.
               RF%VAL(IRCOL) = PF%VAL(IPCOL)
               RF%COLG(IRCOL) = IC
               IRCOL = IRCOL + 1
               EXIT ROW_LOOP
            ENDIF
         ENDDO ROW_LOOP
      ENDDO FINE_CELLS_LOOP

      IF (IS_INCLUDED) THEN 
         RF%ROW(IRROW+1) = IRCOL
         IRROW = IRROW + 1
      ENDIF

      RF%N_ROW = IRROW 
      RF%N_VAL = IRCOL - 1

   ENDDO LOCAL_COARSE_CELLS_LOOP

   CALL SCARC_REDUCE_CMATRIX (RF, 'GF%RESTRICTION', CROUTINE)

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '--------------> NM=',NM
   CALL SCARC_DEBUG_CMATRIX (RF, 'GF%RESTRICTION','AFTER SETUP_RESTRICTION')
#endif

ENDDO MESHES_LOOP

END SUBROUTINE SCARC_SETUP_RESTRICTION


! ------------------------------------------------------------------------------------------------------
!> \brief Find matching column index during matrix-matrix multiplication of compact matrices
! ------------------------------------------------------------------------------------------------------
INTEGER FUNCTION SCARC_FIND_MATCHING_COLUMN(P, JC, ICCG)
TYPE (SCARC_CMATRIX_TYPE), POINTER :: P
INTEGER, INTENT(IN) :: JC, ICCG
INTEGER :: IPCOL

SCARC_FIND_MATCHING_COLUMN = -1
IF (JC == 0) RETURN
DO IPCOL = P%ROW(JC), P%ROW(JC+1)-1
   IF (P%COLG(IPCOL) == ICCG) THEN
      SCARC_FIND_MATCHING_COLUMN = IPCOL
      RETURN 
   ENDIF
ENDDO

END FUNCTION SCARC_FIND_MATCHING_COLUMN


! ------------------------------------------------------------------------------------------------------
!> \brief Find matching components to multiply row of Poisson matrix with column of Prolongation matrix
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_MULTIPLY_POISSON_PROL(A, P, PP, ICC, ICC0, IP, IC)
TYPE (SCARC_CMATRIX_TYPE), POINTER, INTENT(IN) :: A, P, PP
INTEGER, INTENT(IN) :: ICC, IC
INTEGER, INTENT(INOUT) :: IP, ICC0
REAL(EB) :: DSUM, TOL = 1E-12_EB
INTEGER :: IACOL, IPCOL, JC

DSUM = 0.0_EB
DO IACOL = A%ROW(IC), A%ROW(IC+1)-1
   JC = A%COL(IACOL)
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'MULTIPLY_POISSON_PROL: IC, ICC, IP, JC:', IC, ICC, IP, JC
#endif
   IF (JC == 0) CYCLE
   IPCOL = SCARC_FIND_MATCHING_COLUMN(P, JC, ICC)
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'MULTIPLY_POISSON_PROL: IPCOL =', IPCOL
#endif
   IF (JC < 0 .OR. IPCOL <= 0) CYCLE
   ICC0 = ICC
   DSUM = DSUM + A%VAL(IACOL) * P%VAL(IPCOL)
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'MULTIPLY_POISSON_PROL: DSUM, A%VAL, P%VAL:', DSUM, A%VAL(IACOL), P%VAL(IPCOL)
#endif
ENDDO

IF (ABS(DSUM) > TOL) THEN
   PP%COL(IP)  = ICC
   PP%COLG(IP) = ICC
   PP%VAL(IP)  = DSUM
   IP = IP + 1
ENDIF

END SUBROUTINE SCARC_MULTIPLY_POISSON_PROL


! ------------------------------------------------------------------------------------------------------
!> \brief Perform matrix multiplication between fine Poisson matrix and Prolongation matrix 
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_POISSON_PROL(NL)
USE SCARC_POINTERS, ONLY: GC, GF, AF, PF, PPF, OA, OPP, OGF, &
                          SCARC_POINT_TO_OTHER_GRID, SCARC_POINT_TO_MULTIGRID, &
                          SCARC_POINT_TO_CMATRIX, SCARC_POINT_TO_OTHER_CMATRIX
INTEGER, INTENT(IN) :: NL
INTEGER  :: NM, NOM, IC, IP, IP0, INBR, ICC, ICC0 = -1
REAL(EB) :: TNOW, TSUM = 0.0_EB

CROUTINE = 'SCARC_SETUP_POISSON_PROL'

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MULTIGRID (NM, NL, NL+1)

   AF  => SCARC_POINT_TO_CMATRIX (GF, NSCARC_MATRIX_POISSON)          
   PF  => SCARC_POINT_TO_CMATRIX (GF, NSCARC_MATRIX_PROLONGATION)     
   PPF => SCARC_POINT_TO_CMATRIX (GF, NSCARC_MATRIX_POISSON_PROL)         

   !PPF%N_ROW = AF%N_ROW
   PPF%N_ROW = GF%NCE+1
   PPF%N_VAL = PPF%N_ROW*30            ! TODO: only temporarily
   CALL SCARC_ALLOCATE_CMATRIX(PPF, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_FULL, 'GF%PPF', CROUTINE)

   DO INBR = 1, SCARC(NM)%N_NEIGHBORS

      NOM = SCARC(NM)%NEIGHBORS(INBR)
      CALL SCARC_POINT_TO_OTHER_GRID(NM, NOM, NL)

      OA  => SCARC_POINT_TO_OTHER_CMATRIX(OGF, NSCARC_MATRIX_POISSON)
      OPP => SCARC_POINT_TO_OTHER_CMATRIX(OGF, NSCARC_MATRIX_POISSON_PROL)

      OPP%N_VAL = AF%N_VAL              ! TODO : CHECK : MUCH TOO BIG !!!
      OPP%N_ROW = GF%NCE + 1            
      CALL SCARC_ALLOCATE_CMATRIX(OPP, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_FULL, 'OGF%PP', CROUTINE)

   ENDDO

   IP = 1
   IP0 = IP
   PPF%ROW(1) = IP

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'START OF PPF LOOP'
WRITE(MSG%LU_DEBUG,*) '  GF%NC=',GF%NC
WRITE(MSG%LU_DEBUG,*) '  GC%NC=',GC%NC
WRITE(MSG%LU_DEBUG,*) '  GC%NCE2=',GC%NCE2
WRITE(MSG%LU_DEBUG,*) '  GC%NC_GALERKIN=',GC%NC_GALERKIN
WRITE(MSG%LU_DEBUG,*) '  GC%LOCAL_TO_GLOBAL=',GC%LOCAL_TO_GLOBAL
WRITE(MSG%LU_DEBUG,*) '  GC%CELLS_GLOBAL=',GC%CELLS_GLOBAL(1:GC%NC_GALERKIN)
#endif

   FINE_CELLS_LOOP: DO IC = 1, GF%NC

      ! TODO: Better time measurement!
      TNOW = CURRENT_TIME()
      IF (MYID == 0 .AND. MOD(IC,1000) == 0) WRITE(*,*) 'ScaRC-AMG-Setup: Processing cell ',IC,' of ',GF%NC

      INTERNAL_COARSE_CELLS_LOOP: DO ICC = 1, GC%NC
         CALL SCARC_MULTIPLY_POISSON_PROL(AF, PF, PPF, GC%LOCAL_TO_GLOBAL(ICC), ICC0, IP, IC)
      ENDDO INTERNAL_COARSE_CELLS_LOOP

      EXTERNAL_COARSE_CELLS_LOOP: DO ICC = 1, GC%NC_GALERKIN
         CALL SCARC_MULTIPLY_POISSON_PROL(AF, PF, PPF, GC%CELLS_GLOBAL(ICC), ICC0, IP, IC)
      ENDDO EXTERNAL_COARSE_CELLS_LOOP

      ! take care that at least one entry per fine cell is generated
      IF (IP == IP0) THEN
         PPF%VAL(IP)  = 0.0_EB
         PPF%COL(IP)  = ICC0
         PPF%COLG(IP) = ICC0
         IP = IP + 1
      ENDIF
      IP0 = IP

      PPF%ROW(IC+1) = IP

CPU(MYID)%AMG =CPU(MYID)%AMG+CURRENT_TIME()-TNOW
TSUM = TSUM + CPU(MYID)%AMG

   ENDDO FINE_CELLS_LOOP

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '--------------> NM=',NM
   CALL SCARC_DEBUG_CMATRIX (PPF, 'PPF-FINE','AFTER MULTIPLY')
#endif

ENDDO

! Exchange overlapping parts of Prolongation matrix and extract exchanged data
IF (NMESHES > 1) THEN
   CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MATRIX_COLS,  NSCARC_MATRIX_POISSON_PROL, NL)
   CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MATRIX_COLSG, NSCARC_MATRIX_POISSON_PROL, NL)
   CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_MATRIX_VALS,  NSCARC_MATRIX_POISSON_PROL, NL)
   CALL SCARC_EXTRACT_MATRIX_OVERLAPS(NSCARC_MATRIX_POISSON_PROL, 0, NL)
ENDIF

! Reduce workspace for Prolongation matrix to really needed size
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MULTIGRID (NM, NL, NL+1)
   PPF => SCARC_POINT_TO_CMATRIX (GF, NSCARC_MATRIX_POISSON_PROL)         

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '--------------> NM=',NM
   CALL SCARC_DEBUG_CMATRIX (PPF, 'AP-FINE','END SETUP_POISSON_PROL')
#endif

   CALL SCARC_REDUCE_CMATRIX (PPF, 'GF%POISSON-PROL', CROUTINE)
   DO INBR = 1, SCARC(NM)%N_NEIGHBORS
      NOM = SCARC(NM)%NEIGHBORS(INBR)
      CALL SCARC_POINT_TO_OTHER_GRID(NM, NOM, NL)
      OPP => SCARC_POINT_TO_OTHER_CMATRIX(OGF, NSCARC_MATRIX_POISSON_PROL)
      CALL SCARC_REDUCE_CMATRIX(OPP, 'OGF%POISSON_PROL', CROUTINE)
   ENDDO

ENDDO

END SUBROUTINE SCARC_SETUP_POISSON_PROL


! ------------------------------------------------------------------------------------------------------
!> \brief Find matching components to multiply row of Restriction matrix with column of Poisson-Prol matrix
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_MULTIPLY_GALERKIN(PPF, RF, AC, ICCL, JCCL, JCCG, JCCG0, IP)
USE SCARC_POINTERS, ONLY: GF
TYPE (SCARC_CMATRIX_TYPE), POINTER, INTENT(IN) :: PPF, RF, AC
INTEGER, INTENT(IN) :: ICCL, JCCL, JCCG
INTEGER, INTENT(INOUT) :: IP, JCCG0
INTEGER :: IAPCOL, IRCOL, JC
REAL(EB) :: DSUM, TOL = 1E-12_EB

DSUM = 0.0_EB
DO IRCOL = RF%ROW(ICCL), RF%ROW(ICCL+1)-1
   JC = RF%COLG(IRCOL)
   IF (TYPE_COARSENING == NSCARC_COARSENING_GMG .AND. JC > GF%NC) RETURN
   IAPCOL = SCARC_FIND_MATCHING_COLUMN(PPF, JC, JCCG) 
   IF (IAPCOL > 0) THEN
      JCCG0 = JCCG
      DSUM = DSUM + RF%VAL(IRCOL) * PPF%VAL(IAPCOL)
   ENDIF
ENDDO

IF (ABS(DSUM) > TOL) THEN
   AC%COL(IP)  = JCCL
   AC%COLG(IP) = JCCG
   AC%VAL(IP) = DSUM
   IP = IP + 1
ENDIF

END SUBROUTINE SCARC_MULTIPLY_GALERKIN


! ------------------------------------------------------------------------------------------------------
!> \brief Setup Galerkin matrix on coarser grid level (AMG only)
! Note: Matrix POISPROL corresponds to POISSON x PROLONGATION  ~ AP
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_GALERKIN(NL)
USE SCARC_POINTERS, ONLY: GF, GC, PPF, RF, AC, OAC, OGC, &
                          SCARC_POINT_TO_MULTIGRID, SCARC_POINT_TO_OTHER_MULTIGRID, &
                          SCARC_POINT_TO_CMATRIX, SCARC_POINT_TO_OTHER_CMATRIX
INTEGER, INTENT(IN) :: NL
INTEGER  :: NM, IP, IP0, INBR, NOM, NLEN, ICCL, ICCG, JCC, JCCL, JCCG, JCCG0 = -1
REAL(EB) :: TNOW, TSUM = 0.0_EB

CROUTINE = 'SCARC_SETUP_GALERKIN'

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MULTIGRID (NM, NL, NL+1)

   PPF => SCARC_POINT_TO_CMATRIX (GF, NSCARC_MATRIX_POISSON_PROL)         
   RF  => SCARC_POINT_TO_CMATRIX (GF, NSCARC_MATRIX_RESTRICTION)         
   AC  => SCARC_POINT_TO_CMATRIX (GC, NSCARC_MATRIX_POISSON)         

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '--------------> NM=',NM
   CALL SCARC_DEBUG_CMATRIX (RF, 'RESTRICTION-FINE', 'START OF SETUP_GALERKIN')
   WRITE(MSG%LU_DEBUG,*) '--------------> NM=',NM
   CALL SCARC_DEBUG_CMATRIX (PPF, 'PPF-FINE', 'START OF SETUP_GALERKIN')
#endif

   IF (.NOT.ALLOCATED (AC%VAL)) THEN
      AC%N_ROW = GC%NCE+1
      AC%N_VAL = AC%N_ROW**2             ! only temporarily TODO TOO BIG
      CALL SCARC_ALLOCATE_CMATRIX(AC, NL+1, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_FULL, 'GC%POISSON', CROUTINE)
   ENDIF

   DO INBR = 1, SCARC(NM)%N_NEIGHBORS

      NOM = SCARC(NM)%NEIGHBORS(INBR)
      CALL SCARC_POINT_TO_OTHER_MULTIGRID(NM, NOM, NL, NL+1)

      OAC => SCARC_POINT_TO_OTHER_CMATRIX(OGC, NSCARC_MATRIX_POISSON)

      OAC%N_VAL = AC%N_VAL              ! TODO : CHECK : MUCH TOO BIG !!!
      OAC%N_ROW = GC%NCE2 + 1           ! TODO : CHECK : MUCH TOO BIG !!!
      CALL SCARC_ALLOCATE_CMATRIX(OAC, NL, NSCARC_PRECISION_DOUBLE, NSCARC_MATRIX_FULL, 'OAC%POISSON', CROUTINE)

   ENDDO

   CALL SCARC_DEALLOCATE_INT1(GC%CELLS_LOCAL,  'GC%CELLS_LOCAL',  CROUTINE)
   CALL SCARC_DEALLOCATE_INT1(GC%CELLS_GLOBAL, 'GC%CELLS_GLOBAL', CROUTINE)

   NLEN = 4 * (GC%NCE2 - GC%NC + 2)
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'NL=',NL,': GC%NC=',GC%NC, ': GC%NCE=',GC%NCE,': GC%NCE2=',GC%NCE2,': NLEN=',NLEN
#endif

   CALL SCARC_ALLOCATE_INT1 (GC%CELLS_LOCAL, 1, NLEN, NSCARC_INIT_ZERO, 'GC%CELLS_LOCAL', CROUTINE)
   CALL SCARC_ALLOCATE_INT1 (GC%CELLS_GLOBAL, 1, NLEN, NSCARC_INIT_ZERO, 'GC%CELLS_GLOBAL', CROUTINE)

   CALL SCARC_GET_CELL_DEPENDENCIES_GALERKIN(GC, GF, PPF, NLEN)

   CALL SCARC_REDUCE_INT1(GC%CELLS_LOCAL, GC%NC_GALERKIN, 'GC%CELLS_LOCAL', CROUTINE)
   CALL SCARC_REDUCE_INT1(GC%CELLS_GLOBAL, GC%NC_GALERKIN, 'GC%CELLS_GLOBAL', CROUTINE)

ENDDO

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_MULTIGRID (NM, NL, NL+1)

   PPF => SCARC_POINT_TO_CMATRIX (GF, NSCARC_MATRIX_POISSON_PROL)         
   RF  => SCARC_POINT_TO_CMATRIX (GF, NSCARC_MATRIX_RESTRICTION)         
   AC  => SCARC_POINT_TO_CMATRIX (GC, NSCARC_MATRIX_POISSON)         

   IP = 1
   IP0 = IP
   AC%ROW(1) = IP

   LOCAL_COARSE_CELLS_LOOP: DO ICCL = 1, GC%NC

TNOW = CURRENT_TIME()
      ICCG = GC%LOCAL_TO_GLOBAL(ICCL)               ! corresponding global coarse cell

      INTERNAL_COARSE_CELLS_LOOP: DO JCCL = 1, GC%NC
         JCCG = GC%LOCAL_TO_GLOBAL(JCCL)
         CALL SCARC_MULTIPLY_GALERKIN(PPF, RF, AC, ICCL, JCCL, JCCG, JCCG0, IP)
      ENDDO INTERNAL_COARSE_CELLS_LOOP

      !EXTERNAL_COARSE_CELLS_LOOP: DO JCCL = GC%NC + 1, GC%NCE2
      EXTERNAL_COARSE_CELLS_LOOP: DO JCC = 1, GC%NC_GALERKIN
         JCCL = GC%CELLS_LOCAL(JCC)
         JCCG = GC%CELLS_GLOBAL(JCC)
         CALL SCARC_MULTIPLY_GALERKIN(PPF, RF, AC, ICCL, JCCL, JCCG, JCCG0, IP)
      ENDDO EXTERNAL_COARSE_CELLS_LOOP

      ! take care that at least one entry per fine cell is generated
      IF (IP == IP0) THEN
         AC%VAL(IP)  = 0.0_EB
         AC%COL(IP)  = JCCG0
         AC%COLG(IP) = JCCG0
         IP = IP + 1
      ENDIF
      IP0 = IP

      AC%ROW(ICCL + 1) = IP

! TODO: better time measurement
CPU(MYID)%AMG =CPU(MYID)%AMG+CURRENT_TIME()-TNOW
TSUM = TSUM + CPU(MYID)%AMG

   ENDDO LOCAL_COARSE_CELLS_LOOP
   AC%N_ROW = ICCL 

#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) '--------------> NM=',NM
   CALL SCARC_DEBUG_CMATRIX (AC, 'POISSON-COARSE','END OF RAP')
#endif

   CALL SCARC_REDUCE_CMATRIX (AC, 'POISSON-COARSE', CROUTINE)
   CALL SCARC_GET_MATRIX_STENCIL_MAX(AC, GC%NC)

ENDDO

CALL SCARC_RESORT_MATRIX_ROWS(NL+1)             

MESH_INT = 0                            
RANK_INT = 0

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   CALL SCARC_POINT_TO_MULTIGRID (NM, NL, NL+1)
   AC => SCARC_POINT_TO_CMATRIX (GC, NSCARC_MATRIX_POISSON)         
   MESH_INT(NM) = AC%N_STENCIL_MAX
   RANK_INT = MAX(RANK_INT, MESH_INT(NM))
ENDDO

IF (N_MPI_PROCESSES>1) &
   CALL MPI_ALLREDUCE(MPI_IN_PLACE, RANK_INT, 1, MPI_INTEGER, MPI_MAX, MPI_COMM_WORLD, IERROR)

#ifdef WITH_SCARC_MKL
IF (TYPE_MKL(NL+1) == NSCARC_MKL_LOCAL .OR. TYPE_MKL(NL+1) == NSCARC_MKL_GLOBAL) THEN
   DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
      CALL SCARC_SETUP_POISSON_MKL(NM, NL+1)
   ENDDO
ENDIF
#endif

END SUBROUTINE SCARC_SETUP_GALERKIN


! ------------------------------------------------------------------------------------------------------
!> \brief Compute entry of Poisson times Prolongation matrix at specified position
! This consists of a summation over the entries:    P(:,ICC)*A(IC,:) 
! Thus, it must be checked, if - for a given entry of A in row IC - the Prolongation matrix
! has a corresponding non-zero value
! ------------------------------------------------------------------------------------------------------
DOUBLE PRECISION FUNCTION SCARC_VALUE_RAP(IC, ICC)
USE SCARC_POINTERS, ONLY : GC, AF, PF
INTEGER, INTENT(IN) :: IC, ICC
INTEGER :: JC, IA, IP, JCC
REAL(EB) :: DSUM

DSUM = 0.0_EB
DO IA = AF%ROW(IC), AF%ROW(IC+1) - 1
   JC = AF%COL(IA)
   IF (JC < 0) CYCLE
   DO IP = PF%ROW(JC), PF%ROW(JC+1) -1
      JCC = PF%COL(IP) 
      IF (JCC == GC%LOCAL_TO_GLOBAL(ICC)) THEN
         DSUM = DSUM + AF%VAL(IA)*PF%VAL(IP)
         CYCLE
      ENDIF
   ENDDO
ENDDO
SCARC_VALUE_RAP = DSUM

#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'VALUE_AP: RETURN DSUM=', DSUM
#endif

END FUNCTION SCARC_VALUE_RAP


! ------------------------------------------------------------------------------------------------------
!> \brief Resort matrix entries such that diagonal entry comes first (compact storage technique only)
! ------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_RESORT_MATRIX_ROWS(NL)
USE SCARC_POINTERS, ONLY: G, A, SCARC_POINT_TO_GRID, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NL
INTEGER, ALLOCATABLE, DIMENSION(:) :: COL_AUX, COLG_AUX
REAL(EB), ALLOCATABLE, DIMENSION(:) :: VAL_AUX
INTEGER:: NM, NCOL, ICOL, JCOL, KCOL, IC
LOGICAL :: COLG_IS_DEFINED = .FALSE.

CROUTINE = 'SCARC_RESORT_MATRIX_ROWS'

! TODO: use correct length of COL
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   CALL SCARC_POINT_TO_GRID (NM, NL)
   A => SCARC_POINT_TO_CMATRIX (G, NSCARC_MATRIX_POISSON)

   CALL SCARC_ALLOCATE_INT1  (COL_AUX,  1, A%N_STENCIL_MAX, NSCARC_INIT_ZERO, 'COL_AUX',  CROUTINE)
   CALL SCARC_ALLOCATE_INT1  (COLG_AUX, 1, A%N_STENCIL_MAX, NSCARC_INIT_ZERO, 'COLG_AUX', CROUTINE)
   CALL SCARC_ALLOCATE_REAL1 (VAL_AUX,  1, A%N_STENCIL_MAX, NSCARC_INIT_ZERO, 'VAL_AUX',  CROUTINE)

   IF (ALLOCATED(A%COLG)) COLG_IS_DEFINED = .TRUE.

   !DO IC = 1, G%NCE2
   DO IC = 1, G%NC
      COL_AUX = 0
      JCOL = 1
      DO ICOL = A%ROW(IC), A%ROW(IC+1)-1
         COL_AUX(JCOL) = A%COL(ICOL)
         IF (COLG_IS_DEFINED)  COLG_AUX(JCOL) = A%COLG(ICOL)
         VAL_AUX(JCOL) = A%VAL(ICOL)
         JCOL = JCOL + 1
      ENDDO
      NCOL = JCOL - 1

      ! Find column index of diagonal element
      JCOL = 0
      DO WHILE (JCOL <= NCOL)
        JCOL = JCOL + 1
        IF (COL_AUX(JCOL) == IC) EXIT
      ENDDO

      ! Store corresponding index and value in first matrix element of that row
      ICOL = A%ROW(IC)
      A%COL(ICOL)  = COL_AUX(JCOL)
      A%COLG(ICOL) = COLG_AUX(JCOL)
      A%VAL(ICOL)  = VAL_AUX(JCOL)

      COL_AUX(JCOL) = 99999999
      COLG_AUX(JCOL) = 99999999

      IF (COLG_IS_DEFINED) THEN
         JCOL = MINLOC(COLG_AUX(1:NCOL), DIM=1)
      ELSE
         JCOL = MINLOC(COL_AUX(1:NCOL), DIM=1)
      ENDIF
      KCOL = 1
      ICOL = ICOL + 1
      DO WHILE (KCOL < NCOL)
         A%COLG(ICOL) = COLG_AUX(JCOL)
         A%COL(ICOL)  = COL_AUX(JCOL)
         A%VAL(ICOL)  = VAL_AUX(JCOL)
         IF (COLG_IS_DEFINED) THEN
            COLG_AUX(JCOL) = 99999999
            JCOL = MINLOC(COLG_AUX(1:NCOL), DIM=1)
         ELSE
            COL_AUX(JCOL) = 99999999
            JCOL = MINLOC(COL_AUX(1:NCOL), DIM=1)
         ENDIF
         KCOL = KCOL + 1
         ICOL = ICOL + 1
      ENDDO

      IF (ICOL /= A%ROW(IC+1)) WRITE(*,*) 'ERROR IN RESORT_MATRIX_ROWS'

   ENDDO

#ifdef WITH_SCARC_DEBUG
   CALL SCARC_DEBUG_CMATRIX (A, 'A','AFTER RESORT_MATRIX_ROWS')
#endif

   CALL SCARC_DEALLOCATE_INT1  (COL_AUX,  'COL_AUX',  CROUTINE)
   CALL SCARC_DEALLOCATE_INT1  (COLG_AUX, 'COLG_AUX', CROUTINE)
   CALL SCARC_DEALLOCATE_REAL1 (VAL_AUX,  'VAL_AUX',  CROUTINE)

ENDDO

END SUBROUTINE SCARC_RESORT_MATRIX_ROWS

END MODULE SCARC_AMG


