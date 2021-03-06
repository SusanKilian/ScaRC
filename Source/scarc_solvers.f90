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

END MODULE SCRC

