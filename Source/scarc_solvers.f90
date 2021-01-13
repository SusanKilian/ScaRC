! ================================================================================================================
!
!> \brief Scalable Recursive Clustering (ScaRC): Collection of alternative solvers for the FDS pressure equation
!
!  Basic setup and call of different variants of ScaRC/UScaRC ---
!
! ================================================================================================================
!#include "Modules/scarc_constants.f90"
!#include "Modules/scarc_types.f90"
!#include "Modules/scarc_variables.f90"
!#include "Modules/scarc_pointers.f90"
!#include "Modules/scarc_messages.f90"
!#include "Modules/scarc_errors.f90"
!#include "Modules/scarc_utilities.f90"
!#include "Modules/scarc_memory.f90"
!#include "Modules/scarc_convergence.f90"
!#include "Modules/scarc_timings.f90"
!#include "Modules/scarc_stack.f90"
!#include "Modules/scarc_initialization.f90"
!#include "Modules/scarc_mpi.f90"
!#ifdef WITH_SCARC_AMG
!#include "Modules/scarc_mkl.f90"
!#endif
!#include "Modules/scarc_vectors.f90"
!#include "Modules/scarc_discretization.f90"
!#include "Modules/scarc_matrices.f90"
!#include "Modules/scarc_fft.f90"
!#include "Modules/scarc_gmg.f90"
!#ifdef WITH_SCARC_AMG
!#include "Modules/scarc_amg.f90"
!#endif
!#include "Modules/scarc_mgm.f90"
!#include "Modules/scarc_methods.f90"


MODULE SCRC

USE PRECISION_PARAMETERS, ONLY: EB
USE GLOBAL_CONSTANTS
USE COMP_FUNCTIONS, ONLY: CURRENT_TIME
USE SCARC_CONSTANTS
USE SCARC_ERRORS
USE SCARC_TIMINGS
#ifdef WITH_SCARC_MKL
USE SCARC_MKL
#endif
USE SCARC_STACK, ONLY: N_STACK_TOTAL
USE SCARC_TIMINGS
USE SCARC_MESSAGES
USE SCARC_INITIALIZATION
USE SCARC_DISCRETIZATION
USE SCARC_METHODS

IMPLICIT NONE

PUBLIC :: SCARC_SETUP, SCARC_SOLVER

CONTAINS

! ------------------------------------------------------------------------------------------------
!> \brief Initialize ScaRC structures based on SCARC-input parameters from &PRES namelist
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP
REAL(EB) :: TNOW

TNOW = CURRENT_TIME()

! Setup mechanisms for own memory management, different messaging services and CPU-time measurements
 
CALL SCARC_SETUP_STORAGE
CALL SCARC_SETUP_MESSAGES
CALL SCARC_SETUP_TIMINGS

! Parse all ScaRC parameters which have been read in read.f90
!WRITE(*,*) MYID
!IF (MYID == 0) THEN
  !WRITE(*,*) 'HALLO SUSI'
  !READ(*,*) NSUSI
!END IF

CALL SCARC_PARSE_INPUT                      ; IF (STOP_STATUS==SETUP_STOP) RETURN

! Setup different basic components of ScaRC solver
 
CALL SCARC_SETUP_LEVELS                     ; IF (STOP_STATUS==SETUP_STOP) RETURN
CALL SCARC_SETUP_TYPES                      ; IF (STOP_STATUS==SETUP_STOP) RETURN
CALL SCARC_SETUP_GRIDS                      ; IF (STOP_STATUS==SETUP_STOP) RETURN
CALL SCARC_SETUP_GLOBALS                    ; IF (STOP_STATUS==SETUP_STOP) RETURN
CALL SCARC_SETUP_NEIGHBORS                  ; IF (STOP_STATUS==SETUP_STOP) RETURN
CALL SCARC_SETUP_FACES                      ; IF (STOP_STATUS==SETUP_STOP) RETURN
CALL SCARC_SETUP_SUBDIVISION                ; IF (STOP_STATUS==SETUP_STOP) RETURN

! Setup wall information according to specified discretization type/method
 
IF (HAS_MULTIPLE_GRIDS) THEN
   CALL SCARC_SET_GRID_TYPE (NSCARC_GRID_STRUCTURED)
   CALL SCARC_SETUP_WALLS                   ; IF (STOP_STATUS==SETUP_STOP) RETURN
   CALL SCARC_SET_GRID_TYPE (NSCARC_GRID_UNSTRUCTURED)
   CALL SCARC_SETUP_WALLS                   ; IF (STOP_STATUS==SETUP_STOP) RETURN
ELSE
   CALL SCARC_SET_GRID_TYPE (TYPE_GRID)
   CALL SCARC_SETUP_WALLS                   ; IF (STOP_STATUS==SETUP_STOP) RETURN
ENDIF

! Setup information for data exchanges and matrix systems
 
CALL SCARC_SETUP_EXCHANGES                  ; IF (STOP_STATUS==SETUP_STOP) RETURN
CALL SCARC_SETUP_SYSTEMS                    ; IF (STOP_STATUS==SETUP_STOP) RETURN

! Setup information for algebraic multigrid if requested

#ifdef WITH_SCARC_AMG
IF (HAS_AMG_LEVELS) &
   CALL SCARC_SETUP_ALGEBRAIC_MULTIGRID     ; IF (STOP_STATUS==SETUP_STOP) RETURN
#endif

! Setup environments for the requested methods 

SELECT_METHOD: SELECT CASE(TYPE_METHOD)
   CASE (NSCARC_METHOD_KRYLOV)
      CALL SCARC_SETUP_KRYLOV_ENVIRONMENT(N_STACK_TOTAL)
    CASE (NSCARC_METHOD_MULTIGRID)
       CALL SCARC_SETUP_MULTIGRID_ENVIRONMENT(N_STACK_TOTAL)
   CASE (NSCARC_METHOD_MGM)
       CALL SCARC_SETUP_MGM_ENVIRONMENT(N_STACK_TOTAL)
#ifdef WITH_SCARC_MKL
   CASE (NSCARC_METHOD_LU)
       CALL SCARC_SETUP_MKL_ENVIRONMENT(N_STACK_TOTAL)
#endif
END SELECT SELECT_METHOD

! Setup vectors for the requested methods 

CALL SCARC_SETUP_VECTORS                    ; IF (STOP_STATUS==SETUP_STOP) RETURN

! Perform some error statistics for pressure if requested
 
#ifdef WITH_SCARC_POSTPROCESSING
CALL SCARC_SETUP_PRESSURE                   ; IF (STOP_STATUS==SETUP_STOP) RETURN
#endif

CPU(MYID)%SETUP   = CPU(MYID)%SETUP   + CURRENT_TIME() - TNOW
CPU(MYID)%OVERALL = CPU(MYID)%OVERALL + CURRENT_TIME() - TNOW
END SUBROUTINE SCARC_SETUP



! ------------------------------------------------------------------------------------------------------
!> \brief Call of requested ScaRC solver 
! ------------------------------------------------------------------------------------------------------
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
      CALL SCARC_METHOD_KRYLOV (NSCARC_STACK_ROOT, NSCARC_STACK_ZERO, NSCARC_RHS_INHOMOGENEOUS, NLEVEL_MIN)
   
   CASE (NSCARC_METHOD_MULTIGRID)
      CALL SCARC_METHOD_MULTIGRID(NSCARC_STACK_ROOT, NSCARC_STACK_ZERO, NSCARC_RHS_INHOMOGENEOUS, NLEVEL_MIN)
   
   CASE (NSCARC_METHOD_MGM)
      CALL SCARC_METHOD_MGM(NSCARC_STACK_ROOT)

#ifdef WITH_SCARC_MKL
   CASE (NSCARC_METHOD_LU)
      CALL SCARC_METHOD_MKL(NSCARC_STACK_ROOT, NSCARC_STACK_ZERO, NLEVEL_MIN)
#endif
   
END SELECT SELECT_METHOD

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,1000) 'LEAVING', TYPE_METHOD, TOTAL_PRESSURE_ITERATIONS
CALL SCARC_DEBUG_METHOD('DEBUG: LEAVING SCARC_SOLVER  ',6)
#endif

IF (STOP_STATUS==SETUP_STOP) RETURN

T_USED(5)=T_USED(5)+CURRENT_TIME()-TNOW
CPU(MYID)%SOLVER =CPU(MYID)%SOLVER+CURRENT_TIME()-TNOW
CPU(MYID)%OVERALL=CPU(MYID)%OVERALL+CURRENT_TIME()-TNOW

#ifdef WITH_SCARC_DEBUG
1000 FORMAT(A10,' SCARC_SOLVER:  METHOD= ',I6,',  TPI= ', I6
#endif
END SUBROUTINE SCARC_SOLVER

END MODULE SCRC


