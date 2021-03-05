!=======================================================================================================================
!
! MODULE SCRC
!
!  Basic setup and call of different variants of ScaRC/UScaRC 
!
!=======================================================================================================================
MODULE SCRC

USE PRECISION_PARAMETERS, ONLY: EB
USE GLOBAL_CONSTANTS
USE COMP_FUNCTIONS, ONLY: CURRENT_TIME
USE SCARC_CONSTANTS
USE SCARC_METHODS

IMPLICIT NONE

PUBLIC :: SCARC_SETUP, SCARC_SOLVER

CONTAINS

! --------------------------------------------------------------------------------------------------------------
!> \brief Initialize ScaRC structures based on SCARC-input parameters from &PRES namelist
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP
USE SCARC_PARSER
USE SCARC_GRIDS
USE SCARC_MATRICES, ONLY: SCARC_SETUP_SYSTEMS
#ifdef WITH_MKL
USE SCARC_MKL, ONLY: SCARC_SETUP_MKL_ENVIRONMENT
#endif
#ifdef WITH_SCARC_AMG
USE SCARC_AMG, ONLY: SCARC_SETUP_AMG_ENVIRONMENT
#endif
REAL(EB) :: TNOW

TNOW = CURRENT_TIME()

! Setup mechanisms for own memory management, different messaging services and CPU-time measurements
 
CALL SCARC_SETUP_STORAGE
CALL SCARC_SETUP_MESSAGES
CALL SCARC_SETUP_CPU

! Parse ScaRC related input parameters in &PRES namelist

CALL SCARC_PARSE_INPUT                      ; IF (STOP_STATUS==SETUP_STOP) RETURN

! Setup different components of ScaRC solver
 
CALL SCARC_SETUP_LEVELS                               ; IF (STOP_STATUS==SETUP_STOP) RETURN
CALL SCARC_SETUP_BASICS                               ; IF (STOP_STATUS==SETUP_STOP) RETURN
CALL SCARC_SETUP_GRIDS                                ; IF (STOP_STATUS==SETUP_STOP) RETURN
CALL SCARC_SETUP_GLOBALS                              ; IF (STOP_STATUS==SETUP_STOP) RETURN
CALL SCARC_SETUP_NEIGHBORS                            ; IF (STOP_STATUS==SETUP_STOP) RETURN
CALL SCARC_SETUP_FACES                                ; IF (STOP_STATUS==SETUP_STOP) RETURN
CALL SCARC_SETUP_SUBDIVISION                          ; IF (STOP_STATUS==SETUP_STOP) RETURN

! Setup wall information according to specified discretization type/method
 
IF (HAS_MULTIPLE_GRIDS) THEN
   CALL SCARC_SETUP_WALLS (NSCARC_GRID_STRUCTURED)    ; IF (STOP_STATUS==SETUP_STOP) RETURN
   CALL SCARC_SETUP_WALLS (NSCARC_GRID_UNSTRUCTURED)  ; IF (STOP_STATUS==SETUP_STOP) RETURN
ELSE
   CALL SCARC_SETUP_WALLS (TYPE_GRID)                 ; IF (STOP_STATUS==SETUP_STOP) RETURN
ENDIF

! Setup information for data exchanges and matrix systems
 
CALL SCARC_SETUP_EXCHANGES                            ; IF (STOP_STATUS==SETUP_STOP) RETURN
CALL SCARC_SETUP_SYSTEMS                              ; IF (STOP_STATUS==SETUP_STOP) RETURN

! Setup information for algebraic multigrid if needed as preconditioner or main solver

#ifdef WITH_SCARC_AMG
IF (HAS_AMG_LEVELS) CALL SCARC_SETUP_AMG_ENVIRONMENT          
#endif

! Setup environment for requested solver

SELECT_METHOD: SELECT CASE(TYPE_METHOD)

   ! Global Krylov method
   CASE (NSCARC_METHOD_KRYLOV)
      CALL SCARC_SETUP_KRYLOV_ENVIRONMENT

   ! Global multigrid method
    CASE (NSCARC_METHOD_MULTIGRID)
       CALL SCARC_SETUP_MULTIGRID_ENVIRONMENT

   ! Global McKeeney-Greengard-Mayo method
   CASE (NSCARC_METHOD_MGM)
       CALL SCARC_SETUP_MGM_ENVIRONMENT

#ifdef WITH_MKL
   ! Global Intel-MKL related method
   CASE (NSCARC_METHOD_LU)
       CALL SCARC_SETUP_MKL_ENVIRONMENT
#endif
END SELECT SELECT_METHOD

! Setup vector structures for requested solver

CALL SCARC_SETUP_VECTORS                              ; IF (STOP_STATUS==SETUP_STOP) RETURN
 
#ifdef WITH_SCARC_POSTPROCESSING
! Perform some error statistics for pressure if requested

IF (SCARC_DUMP) CALL SCARC_SETUP_PRESSURE             ; IF (STOP_STATUS==SETUP_STOP) RETURN
#endif
CPU(MY_RANK)%SETUP   = CPU(MY_RANK)%SETUP   + CURRENT_TIME() - TNOW
CPU(MY_RANK)%OVERALL = CPU(MY_RANK)%OVERALL + CURRENT_TIME() - TNOW

END SUBROUTINE SCARC_SETUP


! --------------------------------------------------------------------------------------------------------------------
!> \brief Call of requested ScaRC solver 
! --------------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SOLVER(DT_CURRENT)
USE SCARC_CONVERGENCE
REAL (EB), INTENT(IN) :: DT_CURRENT
REAL (EB) :: TNOW

TNOW = CURRENT_TIME()

CALL SCARC_SET_ITERATION_STATE (DT_CURRENT)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) 'STARTING', TYPE_METHOD, TOTAL_PRESSURE_ITERATIONS
CALL SCARC_DEBUG_METHOD('DEBUG: STARTING SCARC_SOLVER ',6)
#endif

IF (TUNNEL_PRECONDITIONER) CALL SCARC_SOLVER_COARSE(1)

SELECT_METHOD: SELECT CASE (TYPE_METHOD)

   CASE (NSCARC_METHOD_KRYLOV)
      CALL SCARC_METHOD_KRYLOV (NSCARC_STACK_ROOT, NSCARC_STACK_ZERO, NLEVEL_MIN)
   
   CASE (NSCARC_METHOD_MULTIGRID)
      CALL SCARC_METHOD_MULTIGRID(NSCARC_STACK_ROOT, NSCARC_STACK_ZERO, NLEVEL_MIN)
   
   CASE (NSCARC_METHOD_MGM)
      CALL SCARC_METHOD_MGM(NSCARC_STACK_ROOT)

#ifdef WITH_MKL
   CASE (NSCARC_METHOD_LU)
      CALL SCARC_METHOD_MKL(NSCARC_STACK_ROOT, NSCARC_STACK_ZERO, NLEVEL_MIN)
#endif
   
END SELECT SELECT_METHOD

IF (TUNNEL_PRECONDITIONER) CALL SCARC_SOLVER_COARSE(2)

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) 'LEAVING', TYPE_METHOD, TOTAL_PRESSURE_ITERATIONS
CALL SCARC_DEBUG_METHOD('SUSISUSISUSI: LEAVING SCARC',3)
#endif

IF (STOP_STATUS==SETUP_STOP) RETURN

T_USED(5)=T_USED(5)+CURRENT_TIME()-TNOW
CPU(MY_RANK)%SOLVER =CPU(MY_RANK)%SOLVER+CURRENT_TIME()-TNOW
CPU(MY_RANK)%OVERALL=CPU(MY_RANK)%OVERALL+CURRENT_TIME()-TNOW

#ifdef WITH_SCARC_DEBUG
1000 FORMAT(A10,' SCARC_SOLVER:  METHOD= ',I6,',  TPI= ', I6)
#endif
END SUBROUTINE SCARC_SOLVER


! --------------------------------------------------------------------------------------------------------------------
!> \brief Add 2-level preconditioner
! --------------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SOLVER_COARSE(NTYPE)
USE SCARC_POINTERS, ONLY: M, L, SCARC_POINT_TO_LEVEL
USE GLOBAL_CONSTANTS
USE MPI
INTEGER, INTENT(IN) :: NTYPE
REAL(EB), POINTER, DIMENSION(:,:,:) :: HP
INTEGER :: I,J,K, NM
REAL(EB) :: RR,BXS_BAR,BXF_BAR, SSS
INTEGER :: IERR,II


DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_LEVEL (NM, NLEVEL_MIN)
   
   IF (PREDICTOR) THEN
      HP => MESHES(NM)%H
   ELSE
      HP => MESHES(NM)%HS
   ENDIF
   
   SELECT CASE (NTYPE)
   
      CASE (1)
   
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'M%PRHS: BEFORE'
   DO K=M%KBAR+1,1,-1
      WRITE(MSG%LU_DEBUG,'(9E12.4)') (M%PRHS(I,1,K), I=1, M%IBAR+1)
   ENDDO
#endif
   
         DO I=1,M%IBAR
            II = I_OFFSET(NM) + I  ! Spatial index of the entire tunnel, not just this mesh
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) '============== I : ', I,' ============ II : ', II
#endif
            TP_CC(II) = 0._EB
            DO K=1,M%KBAR
               DO J=1,M%JBAR
                  SSS = M%PRHS(I,J,K)*M%DY(J)*M%DZ(K)
                  TP_CC(II) = TP_CC(II) + M%PRHS(I,J,K)*M%DY(J)*M%DZ(K)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,4I4,3E12.4)') 'I, J, K, II, PRHS(I,J,K), SUM, TP_CC(II):', &
                                      I,J,K, II, M%PRHS(I,J,K),SSS, TP_CC(II)
                  TP_CC(II) = TP_CC(II) + M%PRHS(I,J,K)*M%DY(J)*M%DZ(K)
#endif
               ENDDO
            ENDDO
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,I4,A,3E12.4)') '    ---> BEFORE: Final TP_CC(',II,')=', TP_CC(II), M%YF-M%YS, M%ZF-M%ZS
#endif
            TP_CC(II) = TP_CC(II)/((M%YF-M%YS)*(M%ZF-M%ZS))  ! RHS linear system of equations
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,I4,A,E12.4)') '    ---> AFTER : Final TP_CC(',II,')=', TP_CC(II)
#endif
            TP_DD(II) = -M%RDX(I)*(M%RDXN(I)+M%RDXN(I-1))  ! Diagonal of tri-diagonal matrix
            TP_AA(II) =  M%RDX(I)*M%RDXN(I)    ! Upper band of matrix
            TP_BB(II) =  M%RDX(I)*M%RDXN(I-1)  ! Lower band of matrix
            M%PRHS(I,1:M%JBAR,1:M%KBAR) = M%PRHS(I,1:M%JBAR,1:M%KBAR) - TP_CC(II)  ! New RHS of the 3-D Poisson equation
         ENDDO
   
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'I_OFFSET:', I_OFFSET
   WRITE(MSG%LU_DEBUG,*) 'TP_DD: DIAG'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') TP_DD
   WRITE(MSG%LU_DEBUG,*) 'TP_AA: UPPER'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') TP_AA
   WRITE(MSG%LU_DEBUG,*) 'TP_BB: LOWER'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') TP_BB
   WRITE(MSG%LU_DEBUG,*) 'TP_CC: RHS'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') TP_CC
   WRITE(MSG%LU_DEBUG,*) 'M%PRHS: AFTER'
   DO K=M%KBAR+1,1,-1
      WRITE(MSG%LU_DEBUG,'(9E12.4)') (M%PRHS(I,1,K), I=1, M%IBAR+1)
   ENDDO
   WRITE(MSG%LU_DEBUG,*) 'M%BXS'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') M%BXS
   WRITE(MSG%LU_DEBUG,*) 'M%BXF'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') M%BXF
   WRITE(MSG%LU_DEBUG,*) 'M%BYS'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') M%BYS
   WRITE(MSG%LU_DEBUG,*) 'M%BYF'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') M%BYF
   WRITE(MSG%LU_DEBUG,*) 'M%BZS'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') M%BZS
   WRITE(MSG%LU_DEBUG,*) 'M%BZF'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') M%BZF
#endif
         
         ! Create new left and right boundary condition arrays (M%BXS and M%BXF) for all meshes, including tunnel ends.
         
         BXS_BAR = 0._EB
         BXF_BAR = 0._EB
         DO K=1,M%KBAR
           DO J=1,M%JBAR
               BXS_BAR = BXS_BAR + M%BXS(J,K)*M%DY(J)*M%DZ(K)
               BXF_BAR = BXF_BAR + M%BXF(J,K)*M%DY(J)*M%DZ(K)
            ENDDO
         ENDDO
         BXS_BAR = BXS_BAR/((M%YF-M%YS)*(M%ZF-M%ZS))  ! Left boundary condition
         BXF_BAR = BXF_BAR/((M%YF-M%YS)*(M%ZF-M%ZS))  ! Right boundary condition
         M%BXS = M%BXS - BXS_BAR
         M%BXF = M%BXF - BXF_BAR
         
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'BXS_BAR'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') BXS_BAR
   WRITE(MSG%LU_DEBUG,*) 'BXF_BAR'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') BXF_BAR
#endif
         ! Apply boundary conditions at end of tunnel to the matrix components
         
         IF (NM==1) THEN
            IF (M%LBC==FISHPAK_BC_NEUMANN_NEUMANN .OR. M%LBC==FISHPAK_BC_NEUMANN_DIRICHLET) THEN  ! Neumann BC
               TP_CC(1) = TP_CC(1) + M%DXI*BXS_BAR*TP_BB(1)
               TP_DD(1) = TP_DD(1) + TP_BB(1)
            ELSE  ! Dirichlet BC
               TP_CC(1) = TP_CC(1) - 2._EB*BXS_BAR*TP_BB(1)
               TP_DD(1) = TP_DD(1) - TP_BB(1)
            ENDIF
         ENDIF
         
         IF (NM==NMESHES) THEN
            IF (M%LBC==FISHPAK_BC_NEUMANN_NEUMANN .OR. M%LBC==FISHPAK_BC_DIRICHLET_NEUMANN) THEN  ! Neumann BC
               TP_CC(TUNNEL_NXP) = TP_CC(TUNNEL_NXP) - M%DXI*BXF_BAR*TP_AA(TUNNEL_NXP)
               TP_DD(TUNNEL_NXP) = TP_DD(TUNNEL_NXP) + TP_AA(TUNNEL_NXP)
            ELSE  ! Dirichet BC
               TP_CC(TUNNEL_NXP) = TP_CC(TUNNEL_NXP) - 2._EB*BXF_BAR*TP_AA(TUNNEL_NXP)
               TP_DD(TUNNEL_NXP) = TP_DD(TUNNEL_NXP) - TP_AA(TUNNEL_NXP)
            ENDIF
         ENDIF
         
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'TP_DD: DIAG - AFTER BC'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') TP_DD
   WRITE(MSG%LU_DEBUG,*) 'TP_CC: RHS - AFTER BC'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') TP_CC
#endif
         IF (MY_RANK>0) THEN  ! MPI processes greater than 0 send their matrix components to MPI process 0
         
            CALL MPI_GATHERV(TP_AA(DISPLS_TP(MY_RANK)+1),COUNTS_TP(MY_RANK),MPI_DOUBLE_PRECISION,TP_AA,COUNTS_TP,DISPLS_TP,&
                             MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,IERR)
            CALL MPI_GATHERV(TP_BB(DISPLS_TP(MY_RANK)+1),COUNTS_TP(MY_RANK),MPI_DOUBLE_PRECISION,TP_BB,COUNTS_TP,DISPLS_TP,&
                             MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,IERR)
            CALL MPI_GATHERV(TP_CC(DISPLS_TP(MY_RANK)+1),COUNTS_TP(MY_RANK),MPI_DOUBLE_PRECISION,TP_CC,COUNTS_TP,DISPLS_TP,&
                             MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,IERR)
            CALL MPI_GATHERV(TP_DD(DISPLS_TP(MY_RANK)+1),COUNTS_TP(MY_RANK),MPI_DOUBLE_PRECISION,TP_DD,COUNTS_TP,DISPLS_TP,&
                             MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,IERR)
         
         ELSE  ! MPI process 0 receives matrix components and solves tri-diagonal linear system of equations.
         
            CALL MPI_GATHERV(MPI_IN_PLACE,0,MPI_DATATYPE_NULL,TP_AA,COUNTS_TP,DISPLS_TP,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,IERR)
            CALL MPI_GATHERV(MPI_IN_PLACE,0,MPI_DATATYPE_NULL,TP_BB,COUNTS_TP,DISPLS_TP,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,IERR)
            CALL MPI_GATHERV(MPI_IN_PLACE,0,MPI_DATATYPE_NULL,TP_CC,COUNTS_TP,DISPLS_TP,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,IERR)
            CALL MPI_GATHERV(MPI_IN_PLACE,0,MPI_DATATYPE_NULL,TP_DD,COUNTS_TP,DISPLS_TP,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,IERR)
         
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'TP_DD: DIAG - AFTER MPI'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') TP_DD
   WRITE(MSG%LU_DEBUG,*) 'TP_AA: UPPER - AFTER MPI'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') TP_AA
   WRITE(MSG%LU_DEBUG,*) 'TP_BB: LOWER - AFTER MPI'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') TP_BB
   WRITE(MSG%LU_DEBUG,*) 'TP_CC: RHS - AFTER MPI'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') TP_CC
#endif
   
            TRIDIAGONAL_SOLVER_1: DO I=2,TUNNEL_NXP
               RR    = TP_BB(I)/TP_DD(I-1)
               TP_DD(I) = TP_DD(I) - RR*TP_AA(I-1)
               TP_CC(I) = TP_CC(I) - RR*TP_CC(I-1)
            ENDDO TRIDIAGONAL_SOLVER_1
            TP_CC(TUNNEL_NXP)  = TP_CC(TUNNEL_NXP)/TP_DD(TUNNEL_NXP)
            TRIDIAGONAL_SOLVER_2: DO I=TUNNEL_NXP-1,1,-1
               TP_CC(I) = (TP_CC(I) - TP_AA(I)*TP_CC(I+1))/TP_DD(I)
            ENDDO TRIDIAGONAL_SOLVER_2
            
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'TP_DD: DIAG - AFTER SOLVE'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') TP_DD
   WRITE(MSG%LU_DEBUG,*) 'TP_CC: RHS - AFTER SOLVE'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') TP_CC
#endif
         ENDIF
         
         ! The solution to the tri-diagonal linear system is TP_CC. Broadcast this to all the MPI processes.
         
         CALL MPI_BCAST(TP_CC,TUNNEL_NXP,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,IERR)
         
         ! Contruct the 1-D solution H_BAR and add boundary conditions at the ends of the tunnel.
         
         H_BAR(1:TUNNEL_NXP) = TP_CC(1:TUNNEL_NXP)
         
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'H_BAR BEFORE BC'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') H_BAR
#endif
         IF (NM==1) THEN
            IF (M%LBC==FISHPAK_BC_NEUMANN_NEUMANN .OR. M%LBC==FISHPAK_BC_NEUMANN_DIRICHLET) THEN
               H_BAR(0) =  H_BAR(1) - M%DXI*BXS_BAR
            ELSE
               H_BAR(0) = -H_BAR(1) + 2._EB*BXS_BAR
            ENDIF
         ENDIF
         
         IF (NM==NMESHES) THEN
            IF (M%LBC==FISHPAK_BC_NEUMANN_NEUMANN .OR. M%LBC==FISHPAK_BC_DIRICHLET_NEUMANN) THEN
               H_BAR(TUNNEL_NXP+1) =  H_BAR(TUNNEL_NXP) + M%DXI*BXF_BAR
            ELSE
               H_BAR(TUNNEL_NXP+1) = -H_BAR(TUNNEL_NXP) + 2._EB*BXF_BAR
            ENDIF
         ENDIF
         
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'H_BAR AFTER BC'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') H_BAR
#endif
   
      CASE (2)
   
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'H_BAR BEFORE UPDATE GHOSTCELLS'
   WRITE(MSG%LU_DEBUG,'(8E12.4)') H_BAR
   WRITE(MSG%LU_DEBUG,*) 'HP BEFORE UPDATE GHOSTCELLS'
   DO K=M%KBAR+1,0,-1
      WRITE(MSG%LU_DEBUG,'(10E12.4)') (HP(I,1,K), I=0,M%KBAR+1)
   ENDDO
#endif
   
         DO I=1,M%IBAR
            HP(I,1:M%JBAR,1:M%KBAR) = HP(I,1:M%JBAR,1:M%KBAR) + H_BAR(I_OFFSET(NM)+I)
         ENDDO
         DO K=1,M%KBAR
            DO J=1,M%JBAR
               DO I=1,M%IBAR
                  IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) HP(I,J,K) = 0.0_EB
               ENDDO
            ENDDO
         ENDDO
         DO K=1,M%KBAR
            DO J=1,M%JBAR
               HP(0,J,K)      = HP(0,J,K)      + H_BAR(I_OFFSET(NM))
               HP(M%IBP1,J,K) = HP(M%IBP1,J,K) + H_BAR(I_OFFSET(NM)+M%IBP1)
            ENDDO
         ENDDO
         DO K=1,M%KBAR
            DO I=1,M%IBAR
               HP(I,0,K)      = HP(I,0,K)      + H_BAR(I_OFFSET(NM)+I)
               HP(I,M%JBP1,K) = HP(I,M%JBP1,K) + H_BAR(I_OFFSET(NM)+I)
            ENDDO
         ENDDO
         DO J=1,M%JBAR
            DO I=1,M%IBAR
               HP(I,J,0)      = HP(I,J,0)      + H_BAR(I_OFFSET(NM)+I)
               HP(I,J,M%KBP1) = HP(I,J,M%KBP1) + H_BAR(I_OFFSET(NM)+I)
            ENDDO
         ENDDO
   
#ifdef WITH_SCARC_DEBUG
   WRITE(MSG%LU_DEBUG,*) 'HP AFTER UPDATE GHOSTCELLS'
   DO K=M%KBAR+1,0,-1
      WRITE(MSG%LU_DEBUG,'(10E12.4)') (HP(I,1,K), I=0,M%KBAR+1)
   ENDDO
#endif
   END SELECT

ENDDO

END SUBROUTINE SCARC_SOLVER_COARSE

END MODULE SCRC

