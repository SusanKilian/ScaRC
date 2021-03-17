!=======================================================================================================================
!
! MODULE SCARC_CPU
!
!> \brief Measure and dump CPU timings for different parts of the ScaRC/UScaRC solvers
!
!=======================================================================================================================
MODULE SCARC_CPU
  
USE GLOBAL_CONSTANTS
USE PRECISION_PARAMETERS, ONLY: EB, FB
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
USE MPI_F08
USE SCARC_CONSTANTS
USE SCARC_MESSAGES

IMPLICIT NONE

CONTAINS

! --------------------------------------------------------------------------------------------------------------
!> \brief Setup time measurements
! --------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_CPU
ALLOCATE (CPU(0:N_MPI_PROCESSES-1), STAT = IERROR)
CALL CHKMEMERR ('SCARC_SETUP_CPU', 'CPU', IERROR)
END SUBROUTINE SCARC_SETUP_CPU

! ------------------------------------------------------------------------------------------------------------------
!> \brief Dump CPU times of several routines
! ------------------------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DUMP_TIMERS
INTEGER, PARAMETER :: LINE_LENGTH = 5 + 12*11
TYPE (MPI_STATUS) :: STATUS
INTEGER :: N
CHARACTER(LEN=LINE_LENGTH) :: LINE
CHARACTER(LEN=LINE_LENGTH), DIMENSION(0:N_MPI_PROCESSES-1) :: LINE_ARRAY

! All MPI processes except root send their timings to the root process. The root process then writes them out to a file.
WRITE(LINE,'(I5,12(",",ES10.3))') MY_RANK,                       &
                                  CPU(MY_RANK)%OVERALL,          &
                                  CPU(MY_RANK)%SETUP,            &
                                  CPU(MY_RANK)%SOLVER,           &
                                  CPU(MY_RANK)%ITERATION,        &
                                  CPU(MY_RANK)%MATVEC_PRODUCT,   &
                                  CPU(MY_RANK)%SCALAR_PRODUCT,   &
                                  CPU(MY_RANK)%RELAXATION,       &
                                  CPU(MY_RANK)%SMOOTHER,         &
                                  CPU(MY_RANK)%COARSE,           &
                                  CPU(MY_RANK)%EXCHANGE,         &
                                  CPU(MY_RANK)%BUFFER_PACKING,   &
                                  CPU(MY_RANK)%BUFFER_UNPACKING

IF (MY_RANK>0) THEN
   CALL MPI_SEND(LINE,LINE_LENGTH,MPI_CHARACTER,0,MY_RANK,MPI_COMM_WORLD,IERROR)
ELSE
   LINE_ARRAY(0) = LINE
   DO N=1,N_MPI_PROCESSES-1
      CALL MPI_RECV(LINE_ARRAY(N),LINE_LENGTH,MPI_CHARACTER,N,N,MPI_COMM_WORLD,STATUS,IERROR)
   ENDDO
   MSG%FILE_CPU = TRIM(CHID)//'_scarc_cpu.csv'
   OPEN (MSG%LU_CPU, FILE=MSG%FILE_CPU, STATUS='REPLACE',FORM='FORMATTED')
   WRITE(MSG%LU_CPU,'(A,A)') 'Rank,OVERALL,SETUP,SOLVER,ITERATION,MATVEC_PRODUCT,SCALAR_PRODUCT,',&
                             'RELAXATION,SMOOTHER,COARSE,EXCHANGE,PACKING,UNPACKING'
   DO N=0,N_MPI_PROCESSES-1
      WRITE(MSG%LU_CPU,'(A)') LINE_ARRAY(N)
   ENDDO
   CLOSE(MSG%LU_CPU)
ENDIF

END SUBROUTINE SCARC_DUMP_TIMERS

END MODULE SCARC_CPU

