!//////////////////////////////////////////////////////////////////////////////////////////////////////
!
! MODULE SCARC_FFT
!
!> \brief Define environment needed for the use of the FFT preconditioner based on Crayfishpak
!
!//////////////////////////////////////////////////////////////////////////////////////////////////////
MODULE SCARC_FFT
  
USE GLOBAL_CONSTANTS
USE PRECISION_PARAMETERS, ONLY: EB, FB
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
USE COMP_FUNCTIONS, ONLY: CURRENT_TIME
USE MPI
USE SCARC_CONSTANTS
USE SCARC_TYPES
USE SCARC_VARIABLES
USE SCARC_MESSAGES
USE SCARC_TIMINGS, ONLY: CPU
USE SCARC_ERRORS
USE SCARC_VECTORS
USE SCARC_MATRICES
USE SCARC_CONVERGENCE

IMPLICIT NONE

CONTAINS

! ----------------------------------------------------------------------------------------------------
!> \brief Setup data structures for the use of blockwise FFT methods as preconditioners
! New here: Perform own initialization of FFT based on H2CZIS/H3CZIS and use own SAVE and WORK arrays
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_FFT(NLMIN, NLMAX)
USE SCARC_POINTERS, ONLY: M, S, L, FFT, SCARC_POINT_TO_GRID
USE POIS, ONLY: H2CZIS, H3CZIS
INTEGER, INTENT(IN) :: NLMIN, NLMAX
INTEGER :: NM, NL

CROUTINE = 'SCARC_SETUP_FFT'
 
! Allocate working space for FFT routine
 
MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   LEVEL_LOOP: DO NL = NLMIN, NLMAX

      CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
      FFT => SCARC(NM)%LEVEL(NL)%FFT

      FFT%LBC = M%LBC
      FFT%MBC = M%MBC
      FFT%NBC = M%NBC

      FFT%XS = S%XS
      FFT%XF = S%XF
      FFT%YS = S%YS
      FFT%YF = S%YF
      FFT%ZS = S%ZS
      FFT%ZF = S%ZF

      FFT%IBAR = L%NX
      FFT%JBAR = L%NY
      FFT%KBAR = L%NZ

      FFT%ITRN = L%NX+1
      IF (TWO_D) THEN
         FFT%JTRN = 1
      ELSE
         FFT%JTRN = L%NY+1
      ENDIF
      FFT%KTRN = L%NZ+1

      FFT%LSAVE = (FFT%ITRN+1)*FFT%JTRN*FFT%KTRN+7*FFT%ITRN+5*FFT%JTRN+6*FFT%KTRN+56
      FFT%LWORK = (FFT%ITRN+1)*FFT%JTRN*FFT%KTRN

      CALL SCARC_ALLOCATE_REAL1(FFT%SAVE1, -3, FFT%LSAVE, NSCARC_INIT_ZERO, 'FFT%SAVE1', CROUTINE)
      CALL SCARC_ALLOCATE_REAL1(FFT%WORK ,  1, FFT%LWORK, NSCARC_INIT_ZERO, 'FFT%WORK', CROUTINE)

      ! Allocate stretching vector (set to 1)
      CALL SCARC_ALLOCATE_REAL1(FFT%HX, 0, FFT%ITRN, NSCARC_INIT_ONE, 'FFT%HX', CROUTINE)

      ! Allocate RHS vector for FFT routine
      IF (L%NY == 1) THEN
         CALL SCARC_ALLOCATE_REAL3(FFT%PRHS, 1, FFT%ITRN, 1,        1, 1, FFT%KTRN, NSCARC_INIT_ZERO, 'FFT%PRHS', CROUTINE)
      ELSE
         CALL SCARC_ALLOCATE_REAL3(FFT%PRHS, 1, FFT%ITRN, 1, FFT%JTRN, 1, FFT%KTRN, NSCARC_INIT_ZERO, 'FFT%PRHS', CROUTINE)
      ENDIF

      ! Allocate boundary data vector for XS
      IF (L%NZ>1) THEN
         IF (L%NY  > 1) CALL SCARC_ALLOCATE_REAL2(FFT%BXS, 1, FFT%JTRN, 1, FFT%KTRN, NSCARC_INIT_ZERO, 'FFT%BXS', CROUTINE)
         IF (L%NY == 1) CALL SCARC_ALLOCATE_REAL2(FFT%BXS, 1,        1, 1, FFT%KTRN, NSCARC_INIT_ZERO, 'FFT%BXS', CROUTINE)
      ELSE
         CALL SCARC_ALLOCATE_REAL2(FFT%BXS, 1, FFT%JTRN, 1, 1, NSCARC_INIT_ZERO, 'FFT%BXS', CROUTINE)
      ENDIF

      ! Allocate boundary data vector for XF
      IF (L%NZ>1) THEN
         IF (L%NY  > 1) CALL SCARC_ALLOCATE_REAL2(FFT%BXF, 1, FFT%JTRN, 1, FFT%KTRN, NSCARC_INIT_ZERO, 'FFT%BXF', CROUTINE)
         IF (L%NY == 1) CALL SCARC_ALLOCATE_REAL2(FFT%BXF, 1,        1, 1, FFT%KTRN, NSCARC_INIT_ZERO, 'FFT%BXF', CROUTINE)
      ELSE
         CALL SCARC_ALLOCATE_REAL2(FFT%BXF, 1, FFT%JTRN, 1, 1, NSCARC_INIT_ZERO, 'FFT%BXF', CROUTINE)
      ENDIF

      ! Allocate boundary data vector for YS
      IF (L%NZ > 1) THEN
         CALL SCARC_ALLOCATE_REAL2(FFT%BYS, 1, FFT%ITRN,1, FFT%KTRN, NSCARC_INIT_ZERO, 'FFT%BYS', CROUTINE)
      ELSE
         CALL SCARC_ALLOCATE_REAL2(FFT%BYS, 1, FFT%ITRN,1,        1, NSCARC_INIT_ZERO, 'FFT%BYS', CROUTINE)
      ENDIF

      ! Allocate boundary data vector for YF
      IF (L%NZ > 1) THEN
         CALL SCARC_ALLOCATE_REAL2(FFT%BYF, 1, FFT%ITRN,1, FFT%KTRN, NSCARC_INIT_ZERO, 'FFT%BYF', CROUTINE)
      ELSE
         CALL SCARC_ALLOCATE_REAL2(FFT%BYF, 1, FFT%ITRN,1,        1, NSCARC_INIT_ZERO, 'FFT%BYF', CROUTINE)
      ENDIF

      ! Allocate boundary data vector for ZS
      IF (L%NY  > 1) CALL SCARC_ALLOCATE_REAL2(FFT%BZS, 1, FFT%ITRN, 1, FFT%JTRN, NSCARC_INIT_ZERO, 'FFT%BZS', CROUTINE)
      IF (L%NY == 1) CALL SCARC_ALLOCATE_REAL2(FFT%BZS, 1, FFT%ITRN, 1,        1, NSCARC_INIT_ZERO, 'FFT%BZS', CROUTINE)

      ! Allocate boundary data vector for ZF
      IF (L%NY  >1)  CALL SCARC_ALLOCATE_REAL2(FFT%BZF, 1, FFT%ITRN, 1, FFT%JTRN, NSCARC_INIT_ZERO, 'FFT%BZF', CROUTINE)
      IF (L%NY == 1) CALL SCARC_ALLOCATE_REAL2(FFT%BZF, 1, FFT%ITRN, 1,        1, NSCARC_INIT_ZERO, 'FFT%BZF', CROUTINE)

      IF (TWO_D) THEN
         CALL H2CZIS(FFT%XS,FFT%XF,FFT%IBAR,FFT%LBC,&
                     FFT%ZS,FFT%ZF,FFT%KBAR,FFT%NBC,&
                     FFT%HX,FFT%XLM,FFT%ITRN,IERROR,FFT%SAVE1)
      ELSE
         CALL H3CZIS(FFT%XS,FFT%XF,FFT%IBAR,FFT%LBC,&
                     FFT%YS,FFT%YF,FFT%JBAR,FFT%MBC,&
                     FFT%ZS,FFT%ZF,FFT%KBAR,FFT%NBC,&
                     FFT%HX,FFT%XLM,FFT%ITRN,FFT%JTRN,IERROR,FFT%SAVE1)
      ENDIF

   ENDDO LEVEL_LOOP
ENDDO MESHES_LOOP

END SUBROUTINE SCARC_SETUP_FFT


! ----------------------------------------------------------------------------------------------------
!> \brief Allocate and initialize vectors for blockwise FFT methods with overlap
! New here: Perform own initialization of FFT based on H2CZIS/H3CZIS and use own SAVE and WORK arrays
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_FFTO(NLMIN, NLMAX)
USE SCARC_POINTERS, ONLY: M, S, L, FFT, SCARC_POINT_TO_GRID
USE POIS, ONLY: H2CZIS, H3CZIS
INTEGER, INTENT(IN) :: NLMIN, NLMAX
INTEGER :: NM, NL

CROUTINE = 'SCARC_SEtUP_FFTO'

! Allocate working space for FFT routine
 
MESHES_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   LEVEL_LOOP: DO NL = NLMIN, NLMAX

      CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
      FFT => SCARC(NM)%LEVEL(NL)%FFT

      FFT%LBC = M%LBC
      FFT%MBC = M%MBC
      FFT%NBC = M%NBC

      IF (NM == 1) THEN
         FFT%IBAR = S%IBAR+1
         FFT%XS = S%XS
         FFT%XF = S%XF + L%DX
      ELSE IF (NM == NMESHES) THEN
         FFT%IBAR = S%IBAR+1
         FFT%XS = S%XS - L%DX
         FFT%XF = S%XF
      ELSE 
         FFT%IBAR = S%IBAR+2
         FFT%XS = S%XS - L%DX
         FFT%XF = S%XF + L%DX
      ENDIF

      FFT%JBAR = S%JBAR
      FFT%KBAR = S%KBAR

      FFT%YS = S%YS
      FFT%YF = S%YF
      FFT%ZS = S%ZS
      FFT%ZF = S%ZF

      FFT%ITRN = FFT%IBAR+1
      IF (TWO_D) THEN
         FFT%JTRN = 1
      ELSE
         FFT%JTRN = L%NY+1
      ENDIF
      FFT%KTRN = L%NZ+1

      FFT%LSAVE = (FFT%ITRN+1)*FFT%JTRN*FFT%KTRN+7*FFT%ITRN+5*FFT%JTRN+6*FFT%KTRN+56
      FFT%LWORK = (FFT%ITRN+1)*FFT%JTRN*FFT%KTRN

      CALL SCARC_ALLOCATE_REAL1(FFT%SAVE1, -3, FFT%LSAVE, NSCARC_INIT_ZERO, 'FFT%SAVE1', CROUTINE)
      CALL SCARC_ALLOCATE_REAL1(FFT%WORK ,  1, FFT%LWORK, NSCARC_INIT_ZERO, 'FFT%WORK', CROUTINE)

      ! Allocate stretching vector (set to 1)

      CALL SCARC_ALLOCATE_REAL1(FFT%HX, 0, FFT%ITRN, NSCARC_INIT_ONE, 'FFT', CROUTINE)

      ! Allocate RHS vector for FFT routine

      IF (L%NY == 1) THEN
         CALL SCARC_ALLOCATE_REAL3(FFT%PRHS, 1, FFT%ITRN, 1,        1, 1, FFT%KTRN, NSCARC_INIT_ZERO, 'FFT%PRHS', CROUTINE)
      ELSE
         CALL SCARC_ALLOCATE_REAL3(FFT%PRHS, 1, FFT%ITRN, 1, FFT%JTRN, 1, FFT%KTRN, NSCARC_INIT_ZERO, 'FFT%PRHS', CROUTINE)
      ENDIF

      ! Allocate boundary data vector for XS

      IF (L%NZ>1) THEN
         IF (L%NY  > 1) CALL SCARC_ALLOCATE_REAL2(FFT%BXS, 1, FFT%JTRN, 1, FFT%KTRN, NSCARC_INIT_ZERO, 'FFT%BXS', CROUTINE)
         IF (L%NY == 1) CALL SCARC_ALLOCATE_REAL2(FFT%BXS, 1,        1, 1, FFT%KTRN, NSCARC_INIT_ZERO, 'FFT%BXS', CROUTINE)
      ELSE
         CALL SCARC_ALLOCATE_REAL2(FFT%BXS, 1, FFT%JTRN, 1, 1, NSCARC_INIT_ZERO, 'FFT%BXS', CROUTINE)
      ENDIF

      ! Allocate boundary data vector for XF

      IF (L%NZ>1) THEN
         IF (L%NY  > 1) CALL SCARC_ALLOCATE_REAL2(FFT%BXF, 1, FFT%JTRN, 1, FFT%KTRN, NSCARC_INIT_ZERO, 'FFT%BXF', CROUTINE)
         IF (L%NY == 1) CALL SCARC_ALLOCATE_REAL2(FFT%BXF, 1,        1, 1, FFT%KTRN, NSCARC_INIT_ZERO, 'FFT%BXF', CROUTINE)
      ELSE
         CALL SCARC_ALLOCATE_REAL2(FFT%BXF, 1, FFT%JTRN, 1, 1, NSCARC_INIT_ZERO, 'FFT%BXF', CROUTINE)
      ENDIF

      ! Allocate boundary data vector for YS

      IF (L%NZ > 1) THEN
         CALL SCARC_ALLOCATE_REAL2(FFT%BYS, 1, FFT%ITRN, 1, FFT%KTRN, NSCARC_INIT_ZERO, 'FFT%BYS', CROUTINE)
      ELSE
         CALL SCARC_ALLOCATE_REAL2(FFT%BYS, 1, FFT%ITRN, 1,        1, NSCARC_INIT_ZERO, 'FFT%BYS', CROUTINE)
      ENDIF

      ! Allocate boundary data vector for YF

      IF (L%NZ > 1) THEN
         CALL SCARC_ALLOCATE_REAL2(FFT%BYF, 1, FFT%ITRN,1, FFT%KTRN, NSCARC_INIT_ZERO, 'FFT%BYF', CROUTINE)
      ELSE
         CALL SCARC_ALLOCATE_REAL2(FFT%BYF, 1, FFT%ITRN,1,        1, NSCARC_INIT_ZERO, 'FFT%BYF', CROUTINE)
      ENDIF

      ! Allocate boundary data vector for ZS

      IF (L%NY  > 1) CALL SCARC_ALLOCATE_REAL2(FFT%BZS, 1, FFT%ITRN, 1, FFT%JTRN, NSCARC_INIT_ZERO, 'FFT%BZS', CROUTINE)
      IF (L%NY == 1) CALL SCARC_ALLOCATE_REAL2(FFT%BZS, 1, FFT%ITRN, 1,        1, NSCARC_INIT_ZERO, 'FFT%BZS', CROUTINE)

      ! Allocate boundary data vector for ZF

      IF (L%NY  >1)  CALL SCARC_ALLOCATE_REAL2(FFT%BZF, 1, FFT%ITRN, 1, FFT%JTRN, NSCARC_INIT_ZERO, 'FFT%BZF', CROUTINE)
      IF (L%NY == 1) CALL SCARC_ALLOCATE_REAL2(FFT%BZF, 1, FFT%ITRN, 1,        1, NSCARC_INIT_ZERO, 'FFT%BZF', CROUTINE)

      IF (TWO_D) THEN
         CALL H2CZIS(FFT%XS,FFT%XF,FFT%IBAR,FFT%LBC,&
                     FFT%ZS,FFT%ZF,FFT%KBAR,FFT%NBC,&
                     FFT%HX,FFT%XLM,FFT%ITRN,IERROR,FFT%SAVE1)
      ELSE
         CALL H3CZIS(FFT%XS,FFT%XF,FFT%IBAR,FFT%LBC,&
                     FFT%YS,FFT%YF,FFT%JBAR,FFT%MBC,&
                     FFT%ZS,FFT%ZF,FFT%KBAR,FFT%NBC,&
                     FFT%HX,FFT%XLM,FFT%ITRN,FFT%JTRN,IERROR,FFT%SAVE1)
      ENDIF

   ENDDO LEVEL_LOOP
ENDDO MESHES_LOOP

END SUBROUTINE SCARC_SETUP_FFTO

END MODULE SCARC_FFT


