!//////////////////////////////////////////////////////////////////////////////////////////////////////
!
! MODULE SCARC_GMG
!
!> \brief Define environment needed for the use of the Geometric Multigrid Method
!
!//////////////////////////////////////////////////////////////////////////////////////////////////////
MODULE SCARC_GMG
  
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

! ------------------------------------------------------------------------------------------------
!> \brief Perform restriction from finer to coarser grid level
!    - 'VF' corresponds to vector on fine   grid
!    - 'VC' corresponds to vector on coarse grid
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_RESTRICTION (NVB, NVC, NLF, NLC)
USE SCARC_POINTERS, ONLY: LC, GF, GC, VF, VC, R, &
                          SCARC_POINT_TO_MULTIGRID, SCARC_POINT_TO_VECTOR, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NVB, NVC, NLF, NLC
REAL(EB) :: DSUM
INTEGER :: NM
INTEGER :: IXF, IYF, IZF, ICF(8)=0, ICFB(-2:2,-2:2)=0
INTEGER :: IXC, IYC, IZC, ICC, IC, ICOL

#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (NVB, 'RESTRICTION INIT: NVB', NLF)
CALL SCARC_DEBUG_LEVEL (NVC, 'RESTRICTION INIT: NVC', NLC)
#endif
IF (IS_GMG .OR. IS_CG_GMG .OR. IS_CG_ADD .OR. IS_CG_MUL) THEN
 
! ---------- Twolevel-CG or Geometric multigrid (as main solver or preconditioner) 
 
   IF (HAS_MULTIPLE_LEVELS) THEN
   
      DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   
         CALL SCARC_POINT_TO_MULTIGRID(NM, NLF, NLC)
   
         VF => SCARC_POINT_TO_VECTOR(NM, NLF, NVB)
         VC => SCARC_POINT_TO_VECTOR(NM, NLC, NVC)
   
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SIZE(VC)=', SIZE(VC)
WRITE(MSG%LU_DEBUG,*) 'GC%NC=', GC%NC
WRITE(MSG%LU_DEBUG,*) 'GC%NCE=', GC%NCE
#endif
         IF (TWO_D) THEN
   
            SELECT_INTERPOL: SELECT CASE (TYPE_INTERPOL)
   
               ! ---------- Constant Interpolation
   
               CASE (NSCARC_INTERPOL_CONSTANT)
      
                  !$OMP PARALLEL DO PRIVATE(IXC, IZC, IXF, IZF, ICC, ICF) SCHEDULE(STATIC)
                  DO IZC = 1, LC%NZ
                     DO IXC = 1, LC%NX
      
                        IF (IS_UNSTRUCTURED .AND. LC%IS_SOLID(IXC, 1, IZC)) CYCLE
      
                        IXF = 2*IXC
                        IZF = 2*IZC
      
                        ICC = GC%CELL_NUMBER(IXC, 1, IZC)
      
                        ICF(1) = GF%CELL_NUMBER(IXF-1, 1, IZF-1)
                        ICF(2) = GF%CELL_NUMBER(IXF-1, 1, IZF  )
                        ICF(3) = GF%CELL_NUMBER(IXF  , 1, IZF-1)
                        ICF(4) = GF%CELL_NUMBER(IXF  , 1, IZF  )
      
                        VC(ICC) = 0.25_EB * (  VF(ICF(1)) &
                                             + VF(ICF(2)) &
                                             + VF(ICF(3)) &
                                             + VF(ICF(4)) )
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,7I6, 5E14.6)') 'REST: IXC, IZC, ICC, ICF(1:4), VC(ICC), 0.25*VF(ICF):', &
 IXC, IZC, ICC, ICF(1), ICF(2), ICF(3), ICF(4), &
 VC(ICC), VF(ICF(1)), VF(ICF(2)), VF(ICF(3)), VF(ICF(4))
#endif
                     ENDDO
                  ENDDO
                  !$OMP END PARALLEL DO
      
               ! ---------- Bilinear Interpolation
   
               CASE (NSCARC_INTERPOL_BILINEAR)
      
                  VC=0.0_EB
      
                  !$OMP PARALLEL DO PRIVATE(IXC, IYC, IZC, IXF, IYF, IZF, ICC, ICFB) SCHEDULE(STATIC)
                  DO IZC = 1, LC%NZ
                     DO IXC = 1, LC%NX
      
                        IF (IS_UNSTRUCTURED .AND. LC%IS_SOLID(IXC, 1, IZC)) CYCLE
      
                        IXF = 2*IXC
                        IZF = 2*IZC
      
                        ICC = GC%CELL_NUMBER(IXC, 1, IZC)
      
                        ICFB(-2,-2) = GF%CELL_NUMBER(IXF-2, 1, IZF-2)
                        ICFB(-1,-2) = GF%CELL_NUMBER(IXF-1, 1, IZF-2)
                        ICFB( 1,-2) = GF%CELL_NUMBER(IXF  , 1, IZF-2)
                        ICFB( 2,-2) = GF%CELL_NUMBER(IXF+1, 1, IZF-2)
      
                        ICFB(-2,-1) = GF%CELL_NUMBER(IXF-2, 1, IZF-1)
                        ICFB(-1,-1) = GF%CELL_NUMBER(IXF-1, 1, IZF-1)
                        ICFB( 1,-1) = GF%CELL_NUMBER(IXF  , 1, IZF-1)
                        ICFB( 2,-1) = GF%CELL_NUMBER(IXF+1, 1, IZF-1)
      
                        ICFB(-2, 1) = GF%CELL_NUMBER(IXF-2, 1, IZF)
                        ICFB(-1, 1) = GF%CELL_NUMBER(IXF-1, 1, IZF)
                        ICFB( 1, 1) = GF%CELL_NUMBER(IXF  , 1, IZF)
                        ICFB( 2, 1) = GF%CELL_NUMBER(IXF+1, 1, IZF)
      
                        ICFB(-2, 2) = GF%CELL_NUMBER(IXF-2, 1, IZF+1)
                        ICFB(-1, 2) = GF%CELL_NUMBER(IXF-1, 1, IZF+1)
                        ICFB( 1, 2) = GF%CELL_NUMBER(IXF  , 1, IZF+1)
                        ICFB( 2, 2) = GF%CELL_NUMBER(IXF+1, 1, IZF+1)
      
                        IF (IXC==1.AND.IZC==1) THEN
                           VC(ICC) = SCALR*( &
                              W4 *VF(ICFB(-1, 2)) + W3 *VF(ICFB(1, 2)) + W1*VF(ICFB(2, 2)) + &
                              W12*VF(ICFB(-1, 1)) + W9 *VF(ICFB(1, 1)) + W3*VF(ICFB(2, 1)) + &
                              W16*VF(ICFB(-1,-1)) + W12*VF(ICFB(1,-1)) + W4*VF(ICFB(2,-1)) )
                        ELSE IF (IXC==LC%NX.AND.IZC==  1) THEN
                           VC(ICC) = SCALR*( &
                              W1 *VF(ICFB(-2, 2)) + W3 *VF(ICFB(-1, 2)) + W4 *VF(ICFB(1, 2)) + &
                              W3 *VF(ICFB(-2, 1)) + W9 *VF(ICFB(-1, 1)) + W12*VF(ICFB(1, 1)) + &
                              W4 *VF(ICFB(-2,-1)) + W12*VF(ICFB(-1,-1)) + W16*VF(ICFB(1,-1)) )
                        ELSE IF (IXC==  1.AND.IZC==LC%NZ) THEN
                           VC(ICC) = SCALR*( &
                              W16*VF(ICFB(-1, 1)) + W12*VF(ICFB(1, 1)) + W4*VF(ICFB(2, 1)) + &
                              W12*VF(ICFB(-1,-1)) + W9 *VF(ICFB(1,-1)) + W3*VF(ICFB(2,-1)) + &
                              W4 *VF(ICFB(-1,-2)) + W3 *VF(ICFB(1,-2)) + W1*VF(ICFB(2,-2)) )
                        ELSE IF (IXC==LC%NX.AND.IZC==LC%NZ) THEN
                           VC(ICC) = SCALR*( &
                              W4 *VF(ICFB(-2, 1)) + W12*VF(ICFB(-1, 1)) + W16*VF(ICFB(1, 1)) + &
                              W3 *VF(ICFB(-2,-1)) + W9 *VF(ICFB(-1,-1)) + W12*VF(ICFB(1,-1)) + &
                              W1 *VF(ICFB(-2,-2)) + W3 *VF(ICFB(-1,-2)) + W4 *VF(ICFB(1,-2)) )
                        ELSE IF (IZC==  1) THEN
                           VC(ICC) = SCALR*( &
                              W1*VF(ICFB(-2, 2)) + W3 *VF(ICFB(-1, 2)) + W3 *VF(ICFB(1, 2)) + W1*VF(ICFB(2, 2)) + &
                              W3*VF(ICFB(-2, 1)) + W9 *VF(ICFB(-1, 1)) + W9 *VF(ICFB(1, 1)) + W3*VF(ICFB(2, 1)) + &
                              W4*VF(ICFB(-2,-1)) + W12*VF(ICFB(-1,-1)) + W12*VF(ICFB(1,-1)) + W4*VF(ICFB(2,-1)) )
                        ELSE IF (IZC==LC%NZ) THEN
                           VC(ICC) = SCALR*( &
                              W4*VF(ICFB(-2, 1)) + W12*VF(ICFB(-1, 1)) + W12*VF(ICFB(1, 1)) + W4*VF(ICFB(2, 1)) + &
                              W3*VF(ICFB(-2,-1)) + W9 *VF(ICFB(-1,-1)) + W9 *VF(ICFB(1,-1)) + W3*VF(ICFB(2,-1)) + &
                              W1*VF(ICFB(-2,-2)) + W3 *VF(ICFB(-1,-2)) + W3 *VF(ICFB(1,-2)) + W1*VF(ICFB(2,-2)) )
                        ELSE IF (IXC==  1) THEN
                           VC(ICC) = SCALR*( &
                              W4 *VF(ICFB(-1, 2)) + W3*VF(ICFB(1, 2)) + W1*VF(ICFB(2, 2)) +&
                              W12*VF(ICFB(-1, 1)) + W9*VF(ICFB(1, 1)) + W3*VF(ICFB(2, 1)) +&
                              W12*VF(ICFB(-1,-1)) + W9*VF(ICFB(1,-1)) + W3*VF(ICFB(2,-1)) +&
                              W4 *VF(ICFB(-1,-2)) + W3*VF(ICFB(1,-2)) + W1*VF(ICFB(2,-2)) )
                        ELSE IF (IXC==LC%NX) THEN
                           VC(ICC) = SCALR*( &
                              W1*VF(ICFB(-2, 2)) + W3*VF(ICFB(-1, 2)) + W4 *VF(ICFB(1, 2)) + &
                              W3*VF(ICFB(-2, 1)) + W9*VF(ICFB(-1, 1)) + W12*VF(ICFB(1, 1)) +&
                              W3*VF(ICFB(-2,-1)) + W9*VF(ICFB(-1,-1)) + W12*VF(ICFB(1,-1)) +&
                              W1*VF(ICFB(-2,-2)) + W3*VF(ICFB(-1,-2)) + W4 *VF(ICFB(1,-2)) )
                        ELSE
                           VC(ICC) = SCALR*( &
                              W1*VF(ICFB(-2,-2)) + W3*VF(ICFB(-1,-2)) + W3*VF(ICFB(1,-2)) + W1*VF(ICFB(2,-2)) +&
                              W3*VF(ICFB(-2,-1)) + W9*VF(ICFB(-1,-1)) + W9*VF(ICFB(1,-1)) + W3*VF(ICFB(2,-1)) +&
                              W3*VF(ICFB(-2, 1)) + W9*VF(ICFB(-1, 1)) + W9*VF(ICFB(1, 1)) + W3*VF(ICFB(2, 1)) +&
                              W1*VF(ICFB(-2, 2)) + W3*VF(ICFB(-1, 2)) + W3*VF(ICFB(1, 2)) + W1*VF(ICFB(2, 2)) )
                        ENDIF
                     ENDDO
                  ENDDO
                  !$OMP END PARALLEL DO
      
            END SELECT SELECT_INTERPOL
   
         ! ---------- Constant Interpolation (Note: 3D-bilinear case is still missing)
   
         ELSE
   
            !$OMP PARALLEL DO PRIVATE(IXC, IYC, IZC, IXF, IYF, IZF, ICC, ICF) SCHEDULE(STATIC)
            DO IZC = 1, LC%NZ
               DO IYC = 1, LC%NY
                  DO IXC = 1, LC%NX
   
                     IF (IS_UNSTRUCTURED .AND. LC%IS_SOLID(IXC, IYC, IZC)) CYCLE
   
                     IXF = 2*IXC
                     IYF = 2*IYC
                     IZF = 2*IZC
   
                     ICC = GC%CELL_NUMBER(IXC, IYC, IZC)
   
                     ICF(1) = GF%CELL_NUMBER(IXF-1, IYF-1, IZF-1)
                     ICF(2) = GF%CELL_NUMBER(IXF-1, IYF-1, IZF  )
                     ICF(3) = GF%CELL_NUMBER(IXF-1, IYF  , IZF-1)
                     ICF(4) = GF%CELL_NUMBER(IXF-1, IYF  , IZF  )
                     ICF(5) = GF%CELL_NUMBER(IXF  , IYF-1, IZF-1)
                     ICF(6) = GF%CELL_NUMBER(IXF  , IYF-1, IZF  )
                     ICF(7) = GF%CELL_NUMBER(IXF  , IYF  , IZF-1)
                     ICF(8) = GF%CELL_NUMBER(IXF  , IYF  , IZF  )
   
                     VC(ICC) = 0.125_EB * (  VF(ICF(1)) &
                                           + VF(ICF(2)) &
                                           + VF(ICF(3)) &
                                           + VF(ICF(4)) &
                                           + VF(ICF(5)) &
                                           + VF(ICF(6)) &
                                           + VF(ICF(7)) &
                                           + VF(ICF(8)) )
   
                  ENDDO
               ENDDO
            ENDDO
            !$OMP END PARALLEL DO
   
         ENDIF
      ENDDO
   ENDIF
   
! ---------- Use restriction based on smoothed aggregation method

ELSE

   IF (NMESHES > 1) CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_VECTOR_PLAIN, NVB, NLF)

#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (NVB, 'RESTRICTION INIT2: NVB', NLF)
#endif
   DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

      CALL SCARC_POINT_TO_MULTIGRID(NM, NLF, NLC)

      VF => SCARC_POINT_TO_VECTOR(NM, NLF, NVB)
      VC => SCARC_POINT_TO_VECTOR(NM, NLC, NVC)
   
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'SIZE(VC)=', SIZE(VC)
WRITE(MSG%LU_DEBUG,*) 'GC%NC=', GC%NC
WRITE(MSG%LU_DEBUG,*) 'GC%NCE=', GC%NCE
WRITE(MSG%LU_DEBUG,*) 'GF%N_COARSE=', GF%N_COARSE
#endif
      R => SCARC_POINT_TO_CMATRIX(GF, NSCARC_MATRIX_RESTRICTION)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'RESTRICTION_AMG: NM, NLF, NLC, N_COARSE:', NM, NLF, NLC, GF%N_COARSE
#endif

      DO IC = 1, GF%N_COARSE
         DSUM = 0.0_EB
         DO ICOL = R%ROW(IC), R%ROW(IC+1)-1                            
            DSUM =  DSUM + R%VAL(ICOL) * VF(R%COLG(ICOL))
         ENDDO
         VC(IC) = DSUM
      ENDDO

   ENDDO

ENDIF
#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (NVC, 'RESTRICTION EXIT: NVC', NLC)
#endif

END SUBROUTINE SCARC_RESTRICTION


! ------------------------------------------------------------------------------------------------
!> \brief Perform prolongation from coarser to finer grid level
!    - 'VC' corresponds to coarser grid
!    - 'VF' corresponds to finer   grid
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_PROLONGATION (NVC, NVB, NLC, NLF)
USE SCARC_POINTERS, ONLY: LC, GF, GC, VF, VC, P, &
                          SCARC_POINT_TO_MULTIGRID, SCARC_POINT_TO_VECTOR, SCARC_POINT_TO_CMATRIX
INTEGER, INTENT(IN) :: NVC, NVB, NLC, NLF
REAL(EB) :: DSUM
INTEGER :: NM, I
INTEGER :: IXF, IYF, IZF, ICF(8)=0, ICFB(-1:1,-1:1)=0
INTEGER :: IXC, IYC, IZC, ICC, IC, ICOL

#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (NVB, 'PROLONGATION INIT: NVB', NLF)
CALL SCARC_DEBUG_LEVEL (NVC, 'PROLONGATION INIT: NVC', NLC)
#endif
IF (IS_GMG .OR. IS_CG_GMG .OR. IS_CG_ADD .OR. IS_CG_MUL) THEN
 
! ------------------ Twolevel CG or Geometric Multigrid 
 
   IF (HAS_MULTIPLE_LEVELS) THEN
   
      DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   
         CALL SCARC_POINT_TO_MULTIGRID(NM, NLF, NLC)
   
         VC => SCARC_POINT_TO_VECTOR(NM, NLC, NVC)
         VF => SCARC_POINT_TO_VECTOR(NM, NLF, NVB)
   
         IF (TWO_D) THEN
   
            SELECT_INTERPOL: SELECT CASE (TYPE_INTERPOL)
   
               CASE (NSCARC_INTERPOL_CONSTANT)
      
                  !!$OMP PARALLEL DO PRIVATE(IXC, IZC, IXF, IZF, ICC, ICF) SCHEDULE(STATIC)
                  DO IZC = 1, LC%NZ
                     DO IXC = 1, LC%NX
      
                        IF (IS_UNSTRUCTURED .AND. LC%IS_SOLID(IXC, 1, IZC)) CYCLE
      
                        IXF = 2*IXC
                        IZF = 2*IZC
      
                        ICC = GC%CELL_NUMBER(IXC, 1, IZC)
      
                        ICF(1) = GF%CELL_NUMBER(IXF-1, 1, IZF-1)
                        ICF(2) = GF%CELL_NUMBER(IXF-1, 1, IZF  )
                        ICF(3) = GF%CELL_NUMBER(IXF  , 1, IZF-1)
                        ICF(4) = GF%CELL_NUMBER(IXF  , 1, IZF  )
      
                        DO I = 1, 4
                           VF(ICF(I)) = VC(ICC)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,4I6, 2E14.6)') 'PROL: IXC, IZC, ICC, ICF(I), VF(ICF(I)),VC(ICC):', &
   IXC, IZC, ICC, ICF(I), VF(ICF(I)), VC(ICC)
#endif
                        ENDDO
                     ENDDO
                  ENDDO
                  !!$OMP END PARALLEL DO 
      
               CASE (NSCARC_INTERPOL_BILINEAR)
      
                  !!$OMP PARALLEL DO PRIVATE(IXC, IZC, IXF, IZF, ICC, ICFB) SCHEDULE(STATIC)
                  DO IZC = 1, LC%NZ
                     DO IXC = 1, LC%NX
      
                        IF (IS_UNSTRUCTURED .AND. LC%IS_SOLID(IXC, 1, IZC)) CYCLE
      
                        IXF = 2*IXC
                        IZF = 2*IZC
      
                        ICC = GC%CELL_NUMBER(IXC, 1, IZC)
      
                        ICFB(-1,-1) = GF%CELL_NUMBER(IXF-1, 1, IZF-1)
                        ICFB(-1, 1) = GF%CELL_NUMBER(IXF-1, 1, IZF  )
                        ICFB( 1,-1) = GF%CELL_NUMBER(IXF  , 1, IZF-1)
                        ICFB( 1, 1) = GF%CELL_NUMBER(IXF  , 1, IZF  )
      
                        IF (IXC==1.AND.IZC==1) THEN
                           VF(ICFB(-1,-1)) = VC(ICC)
                           VF(ICFB(-1, 1)) = SCALP*(W12*VC(ICC)+W4*VC(ICC+LC%NX))
                           VF(ICFB( 1,-1)) = SCALP*(W12*VC(ICC)+W4*VC(ICC+1))
                           VF(ICFB( 1, 1)) = SCALP*(W9 *VC(ICC)+W3*VC(ICC+1)+W3*VC(ICC+LC%NX)+W1*VC(ICC+LC%NX+1))
                        ELSE IF (IXC==1 .AND. IZC==LC%NZ) THEN
                           VF(ICFB(-1,-1)) = SCALP*(W12*VC(ICC)+W4*VC(ICC-LC%NX))
                           VF(ICFB(-1, 1)) = VC(ICC)
                           VF(ICFB( 1,-1)) = SCALP*(W9 *VC(ICC)+W3*VC(ICC+1)+W3*VC(ICC-LC%NX)+W1*VC(ICC-LC%NX+1))
                           VF(ICFB( 1, 1)) = SCALP*(W12*VC(ICC)+W4*VC(ICC+1))
                        ELSE IF (IXC==LC%NX .AND. IZC==1) THEN
                           VF(ICFB(-1,-1)) = SCALP*(W12*VC(ICC)+W4*VC(ICC-1))
                           VF(ICFB(-1, 1)) = SCALP*(W9 *VC(ICC)+W3*VC(ICC-1)+W3*VC(ICC+LC%NX)+W1*VC(ICC+LC%NX-1))
                           VF(ICFB( 1,-1)) = VC(ICC)
                           VF(ICFB( 1, 1)) = SCALP*(W12*VC(ICC)+W4*VC(ICC+LC%NX))
                        ELSE IF (IXC==LC%NX .AND. IZC==LC%NZ) THEN
                           VF(ICFB(-1,-1)) = SCALP*(W9 *VC(ICC)+W3*VC(ICC-1)+W3*VC(ICC-LC%NX)+W1*VC(ICC-LC%NX-1))
                           VF(ICFB(-1, 1)) = SCALP*(W12*VC(ICC)+W4*VC(ICC-1))
                           VF(ICFB( 1,-1)) = SCALP*(W12*VC(ICC)+W4*VC(ICC-LC%NX))
                           VF(ICFB( 1, 1)) = VC(ICC)
                        ELSE IF (IZC==1) THEN
                           VF(ICFB(-1,-1)) = SCALP*(W12*VC(ICC)+W4*VC(ICC-1))
                           VF(ICFB(-1, 1)) = SCALP*(W9 *VC(ICC)+W3*VC(ICC-1)+W3*VC(ICC+LC%NX)+W1*VC(ICC+LC%NX-1))
                           VF(ICFB( 1,-1)) = SCALP*(W12*VC(ICC)+W4*VC(ICC+1))
                           VF(ICFB( 1, 1)) = SCALP*(W9 *VC(ICC)+W3*VC(ICC+1)+W3*VC(ICC+LC%NX)+W1*VC(ICC+LC%NX+1))
                        ELSE IF (IZC==LC%NZ) THEN
                           VF(ICFB(-1,-1)) = SCALP*(W9 *VC(ICC)+W3*VC(ICC-1)+W3*VC(ICC-LC%NX)+W1*VC(ICC-LC%NX-1))
                           VF(ICFB(-1, 1)) = SCALP*(W12*VC(ICC)+W4*VC(ICC-1))
                           VF(ICFB( 1,-1)) = SCALP*(W9 *VC(ICC)+W3*VC(ICC+1)+W3*VC(ICC-LC%NX)+W1*VC(ICC-LC%NX+1))
                           VF(ICFB( 1, 1)) = SCALP*(W12*VC(ICC)+W4*VC(ICC+1))
                        ELSE IF (IXC==1) THEN
                           VF(ICFB(-1,-1)) = SCALP*(W12*VC(ICC)+W4*VC(ICC-LC%NX))
                           VF(ICFB(-1, 1)) = SCALP*(W12*VC(ICC)+W4*VC(ICC+LC%NX))
                           VF(ICFB( 1,-1)) = SCALP*(W9 *VC(ICC)+W3*VC(ICC+1)+W3*VC(ICC-LC%NX)+W1*VC(ICC-LC%NX+1))
                           VF(ICFB( 1, 1)) = SCALP*(W9 *VC(ICC)+W3*VC(ICC+1)+W3*VC(ICC+LC%NX)+W1*VC(ICC+LC%NX+1))
                        ELSE IF (IXC==LC%NX) THEN
                           VF(ICFB(-1,-1)) = SCALP*(W9 *VC(ICC)+W3*VC(ICC-1)+W3*VC(ICC-LC%NX)+W1*VC(ICC-LC%NX-1))
                           VF(ICFB(-1, 1)) = SCALP*(W9 *VC(ICC)+W3*VC(ICC-1)+W3*VC(ICC+LC%NX)+W1*VC(ICC+LC%NX-1))
                           VF(ICFB( 1,-1)) = SCALP*(W12*VC(ICC)+W4*VC(ICC-LC%NX))
                           VF(ICFB( 1, 1)) = SCALP*(W12*VC(ICC)+W4*VC(ICC+LC%NX))
                        ELSE
                           VF(ICFB(-1,-1)) = SCALP*(W9*VC(ICC)+W3*VC(ICC-1)+W3*VC(ICC-LC%NX)+W1*VC(ICC-LC%NX-1))
                           VF(ICFB(-1, 1)) = SCALP*(W9*VC(ICC)+W3*VC(ICC-1)+W3*VC(ICC+LC%NX)+W1*VC(ICC+LC%NX-1))
                           VF(ICFB( 1,-1)) = SCALP*(W9*VC(ICC)+W3*VC(ICC+1)+W3*VC(ICC-LC%NX)+W1*VC(ICC-LC%NX+1))
                           VF(ICFB( 1, 1)) = SCALP*(W9*VC(ICC)+W3*VC(ICC+1)+W3*VC(ICC+LC%NX)+W1*VC(ICC+LC%NX+1))
                        ENDIF
                     ENDDO
                  ENDDO
                  !!$OMP END PARALLEL DO 
      
            END SELECT SELECT_INTERPOL
   
         ELSE
   
            ! ---------- Constant Interpolation (Note: 3D-bilinear case is still missing)
   
            !!$OMP PARALLEL DO PRIVATE(IXC, IYC, IZC, IXF, IYF, IZF, ICC, ICF) SCHEDULE(STATIC)
            DO IZC = 1, LC%NZ
               DO IYC = 1, LC%NY
                  DO IXC = 1, LC%NX
   
                     IF (IS_UNSTRUCTURED .AND. LC%IS_SOLID(IXC, IYC, IZC)) CYCLE
   
                     IXF = 2*IXC
                     IYF = 2*IYC
                     IZF = 2*IZC
   
                     ICC = GC%CELL_NUMBER(IXC, IYC, IZC)
   
                     ICF(1) = GF%CELL_NUMBER(IXF-1, IYF-1, IZF-1)
                     ICF(2) = GF%CELL_NUMBER(IXF-1, IYF-1, IZF  )
                     ICF(3) = GF%CELL_NUMBER(IXF-1, IYF  , IZF-1)
                     ICF(4) = GF%CELL_NUMBER(IXF-1, IYF  , IZF  )
                     ICF(5) = GF%CELL_NUMBER(IXF  , IYF-1, IZF-1)
                     ICF(6) = GF%CELL_NUMBER(IXF  , IYF-1, IZF  )
                     ICF(7) = GF%CELL_NUMBER(IXF  , IYF  , IZF-1)
                     ICF(8) = GF%CELL_NUMBER(IXF  , IYF  , IZF  )
   
                     DO I = 1, 8
                        VF(ICF(I)) = VC(ICC)
                     ENDDO
   
                  ENDDO
               ENDDO
            ENDDO
            !!$OMP END PARALLEL DO 
   
         ENDIF
      ENDDO
   ENDIF
   
! ---------- Use Prolongation matrix based on smoothed aggregation method
ELSE

   DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

      CALL SCARC_POINT_TO_MULTIGRID(NM, NLF, NLC)

      VC => SCARC_POINT_TO_VECTOR(NM, NLC, NVC)
      VF => SCARC_POINT_TO_VECTOR(NM, NLF, NVB)

      P => SCARC_POINT_TO_CMATRIX(GF, NSCARC_MATRIX_PROLONGATION)

      !DO IC = 1, GF%N_FINE
      DO IC = 1, GF%NC
         DSUM = 0.0_EB
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,'(A,30I6)') 'PROL: IC, P%COLS:', IC, (P%COL(ICOL), ICOL=P%ROW(IC), P%ROW(IC+1)-1)
#endif
         DO ICOL = P%ROW(IC), P%ROW(IC+1)-1                            
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,*) 'PROL: IC, ICOL, P%COL(ICOL):', IC, ICOL, P%COL(ICOL)
#endif
            DSUM = DSUM + P%VAL(ICOL) * VC(P%COL(ICOL))
         ENDDO
         VF(IC) = DSUM
      ENDDO

   ENDDO

   IF (NMESHES > 1) CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_VECTOR_PLAIN, NVB, NLF)

ENDIF
#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (NVC, 'PROLONGATION EXIT: NVC', NLC)
#endif

END SUBROUTINE SCARC_PROLONGATION

END MODULE SCARC_GMG


