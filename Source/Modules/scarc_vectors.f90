!//////////////////////////////////////////////////////////////////////////////////////////////////////
!
! MODULE SCARC_VECTORS
!
!> \brief Define a set of Linear Algebra operations based on vectors and matrices
!
!//////////////////////////////////////////////////////////////////////////////////////////////////////
MODULE SCARC_VECTORS
  
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
USE SCARC_TIMINGS
USE SCARC_MPI

IMPLICIT NONE

CONTAINS

! ------------------------------------------------------------------------------------------------
!> \brief Vector multiplied with a constant scalar is added to another vector 
!     DY(I) = DA * DX(I) + DY(I) 
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DAXPY_CONSTANT(N, DA, DX, DY)
REAL(EB), INTENT(IN):: DA
REAL(EB), INTENT(IN), DIMENSION(:):: DX
REAL(EB), INTENT(INOUT), DIMENSION(:):: DY
INTEGER, INTENT(IN)::  N
INTEGER::  I

!$OMP PARALLEL DO PRIVATE(I) SCHEDULE(STATIC)
DO I = 1, N
  DY(I) = DY(I) + DA * DX(I)
ENDDO
!$OMP END PARALLEL DO 

END SUBROUTINE SCARC_DAXPY_CONSTANT


! ------------------------------------------------------------------------------------------------
!> \brief Vector multiplied with a constant scalar is added to vector multiplied with another scalar
!     DY(I) = DA1 * DX(I) + DA2 * DY(I) 
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DAXPY_CONSTANT_DOUBLE(N, DA1, DX, DA2, DY)
REAL(EB), INTENT(IN):: DA1, DA2
REAL(EB), INTENT(IN), DIMENSION(:):: DX
REAL(EB), INTENT(INOUT), DIMENSION(:):: DY
INTEGER, INTENT(IN)::  N
INTEGER::  I

!$OMP PARALLEL DO PRIVATE(I) SCHEDULE(STATIC)
DO I = 1, N
  DY(I) = DA1 * DX(I) + DA2 * DY(I)
ENDDO
!$OMP END PARALLEL DO 

END SUBROUTINE SCARC_DAXPY_CONSTANT_DOUBLE


! ------------------------------------------------------------------------------------------------
!> \brief Vector multiplied with variable scalars (componentwise) is added to another vector 
!     DY(I) = DA(I)*DX(I) + DY(I)
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DAXPY_VARIABLE(N, DA, DX, DY)
REAL(EB), INTENT(IN), DIMENSION(:):: DA, DX
REAL(EB), INTENT(INOUT), DIMENSION(:):: DY
INTEGER, INTENT(IN)::  N
INTEGER::  I

!$OMP PARALLEL DO PRIVATE(I) SCHEDULE(STATIC)
DO I = 1, N
  DY(I) = DY(I) + DA(I) * DX(I)
#ifdef WITH_SCARC_DEBUG2
WRITE(MSG%LU_DEBUG,'(A, I6, 3E14.6)') 'I, DX, DA, DY:', I, DX(I), DA(I), DY(I)
#endif
ENDDO
!$OMP END PARALLEL DO 

END SUBROUTINE SCARC_DAXPY_VARIABLE


! ------------------------------------------------------------------------------------------------
!> \brief Vector is multiplied with a constant scalar 
!     DY(I) = DA(I)*DX(I) 
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SCALING_CONSTANT(N, SCAL, DX, DY)
REAL(EB), INTENT(IN), DIMENSION(:):: DX
REAL(EB), INTENT(INOUT), DIMENSION(:):: DY
REAL(EB), INTENT(IN) :: SCAL
INTEGER, INTENT(IN)::  N
INTEGER::  I

!$OMP PARALLEL DO PRIVATE(I) SCHEDULE(STATIC)
DO I = 1, N
  DY(I) =  SCAL * DX(I)
ENDDO
!$OMP END PARALLEL DO 

END SUBROUTINE SCARC_SCALING_CONSTANT


! ------------------------------------------------------------------------------------------------
!> \brief Vector is multiplied with variable scalars (componentwise)
!     DY(I) = DA(I)*DX(I) 
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SCALING_VARIABLE(N, DA, DX, DY)
REAL(EB), INTENT(IN), DIMENSION(:):: DA, DX
REAL(EB), INTENT(INOUT), DIMENSION(:):: DY
INTEGER, INTENT(IN)::  N
INTEGER::  I

!$OMP PARALLEL DO PRIVATE(I) SCHEDULE(STATIC)
DO I = 1, N
  DY(I) = DA(I) * DX(I)
ENDDO
!$OMP END PARALLEL DO

END SUBROUTINE SCARC_SCALING_VARIABLE


! ------------------------------------------------------------------------------------------------
!> \brief Compute global scalar-product including global data exchange
! ------------------------------------------------------------------------------------------------
REAL(EB) FUNCTION SCARC_SCALAR_PRODUCT(NV1, NV2, NL)
USE SCARC_POINTERS, ONLY: G, V1, V2, SCARC_POINT_TO_GRID, SCARC_POINT_TO_VECTOR
INTEGER, INTENT(IN) :: NV1, NV2, NL
REAL(EB) :: TNOW, RANK_REAL
INTEGER :: NM
#ifdef WITH_SCARC_MKL
REAL(EB) :: DDOT
EXTERNAL :: DDOT
#else
INTEGER :: IC
#endif

TNOW = CURRENT_TIME()

RANK_REAL = 0.0_EB
MESH_REAL = 0.0_EB

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

   V1 => SCARC_POINT_TO_VECTOR (NM, NL, NV1)
   V2 => SCARC_POINT_TO_VECTOR (NM, NL, NV2)

#ifdef WITH_SCARC_MKL
   MESH_REAL(NM) = DDOT(G%NC, V1, 1, V2, 1)
#else
   MESH_REAL(NM) = 0.0_EB
   !$OMP PARALLEL DO PRIVATE(IC) SCHEDULE(STATIC)
   DO IC = 1, G%NC
      MESH_REAL(NM) = MESH_REAL(NM) + V1(IC) * V2(IC)
   ENDDO
   !$OMP END PARALLEL DO 
#endif

   RANK_REAL = RANK_REAL + MESH_REAL(NM)

ENDDO

! Compute global scalar product as sum of local scalar products
 
IF (N_MPI_PROCESSES>1) & 
   CALL MPI_ALLREDUCE(MPI_IN_PLACE, RANK_REAL, 1, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, IERROR)

SCARC_SCALAR_PRODUCT = RANK_REAL

CPU(MYID)%SCALAR_PRODUCT = CPU(MYID)%SCALAR_PRODUCT + CURRENT_TIME()-TNOW
END FUNCTION SCARC_SCALAR_PRODUCT


! ------------------------------------------------------------------------------------------------
!> \brief Compute global L2-norm including global data exchange
! ------------------------------------------------------------------------------------------------
REAL(EB) FUNCTION SCARC_L2NORM(NV1, NL)
INTEGER, INTENT(IN) :: NV1, NL
REAL(EB) :: TNOW
TNOW = CURRENT_TIME()

GLOBAL_REAL = SCARC_SCALAR_PRODUCT(NV1, NV1, NL)
GLOBAL_REAL = SQRT (GLOBAL_REAL)

SCARC_L2NORM = GLOBAL_REAL

CPU(MYID)%L2NORM =CPU(MYID)%L2NORM+CURRENT_TIME()-TNOW
END FUNCTION SCARC_L2NORM


! ------------------------------------------------------------------------------------------------
!> \brief Compute linear combination of two vectors for bandwise storage technique
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_VECTOR_SUM(NV1, NV2, SCAL1, SCAL2, NL)
USE SCARC_POINTERS, ONLY: G, V1, V2, SCARC_POINT_TO_GRID, SCARC_POINT_TO_VECTOR
INTEGER, INTENT(IN) :: NV1, NV2, NL
REAL(EB), INTENT(IN) :: SCAL1, SCAL2
INTEGER :: NM
#ifdef WITH_SCARC_MKL
EXTERNAL :: DAXPBY
#endif

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

   V1 => SCARC_POINT_TO_VECTOR(NM, NL, NV1)
   V2 => SCARC_POINT_TO_VECTOR(NM, NL, NV2)

#ifdef WITH_SCARC_MKL
   CALL DAXPBY(G%NCE, SCAL1, V1, 1, SCAL2, V2, 1)
#else
   CALL SCARC_DAXPY_CONSTANT_DOUBLE(G%NCE, SCAL1, V1, SCAL2, V2)
#endif

ENDDO

END SUBROUTINE SCARC_VECTOR_SUM


! ------------------------------------------------------------------------------------------------
!> \brief Define vector2 to be a scaled copy of vector 1
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_VECTOR_COPY(NV1, NV2, SCAL1, NL)
USE SCARC_POINTERS, ONLY: G, V1, V2, SCARC_POINT_TO_GRID, SCARC_POINT_TO_VECTOR
INTEGER, INTENT(IN) :: NV1, NV2, NL
REAL(EB), INTENT(IN) :: SCAL1
INTEGER :: NM
#ifdef WITH_SCARC_MKL
EXTERNAL :: DCOPY, DSCAL
#endif

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

   V1 => SCARC_POINT_TO_VECTOR(NM, NL, NV1)
   V2 => SCARC_POINT_TO_VECTOR(NM, NL, NV2)

#ifdef WITH_SCARC_MKL
   CALL DCOPY(G%NCE, V1, 1, V2, 1)
   CALL DSCAL(G%NCE, SCAL1, V2, 1)
#else
   CALL SCARC_SCALING_CONSTANT(G%NCE, SCAL1, V1, V2)
#endif

ENDDO

END SUBROUTINE SCARC_VECTOR_COPY


! ------------------------------------------------------------------------------------------------
!> \brief Clear vector
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_VECTOR_CLEAR(NV, NL)
USE SCARC_POINTERS, ONLY: VC, SCARC_POINT_TO_GRID, SCARC_POINT_TO_VECTOR
INTEGER, INTENT(IN) :: NV, NL
INTEGER :: NM

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
   VC => SCARC_POINT_TO_VECTOR(NM, NL, NV)
   VC =  0.0_EB
ENDDO

END SUBROUTINE SCARC_VECTOR_CLEAR


! ------------------------------------------------------------------------------------------------
!> \brief Preset vector with specified value
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_VECTOR_RANDOM_INIT (NV, NL)
USE SCARC_POINTERS, ONLY: L, G, VC, SCARC_POINT_TO_GRID, SCARC_POINT_TO_VECTOR
INTEGER, INTENT(IN) :: NV, NL
INTEGER :: IC, NM, I, J, K
REAL (EB) :: VAL

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

   VC => SCARC_POINT_TO_VECTOR (NM, NL, NV)

   !$OMP PARALLEL DO PRIVATE(I, J, K, IC) SCHEDULE(STATIC)
   DO K = 1, L%NZ
      DO J = 1, L%NY
         DO I = 1, L%NX
            IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE
            IC = G%CELL_NUMBER(I,J,K)
            CALL RANDOM_NUMBER(VAL)
            VC(IC) = VAL
         ENDDO
      ENDDO
   ENDDO
   !$OMP END PARALLEL DO 

ENDDO

END SUBROUTINE SCARC_VECTOR_RANDOM_INIT


! ------------------------------------------------------------------------------------------------
!> \brief Preset vector with specified value
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_VECTOR_INIT (NV, VAL, NL)
USE SCARC_POINTERS, ONLY: L, G, VC, SCARC_POINT_TO_GRID, SCARC_POINT_TO_VECTOR
INTEGER, INTENT(IN) :: NV, NL
REAL (EB), INTENT(IN) :: VAL
INTEGER :: IC, NM, I, J, K

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G

   VC => SCARC_POINT_TO_VECTOR (NM, NL, NV)

   !$OMP PARALLEL DO PRIVATE(I, J, K, IC) SCHEDULE(STATIC)
   DO K = 1, L%NZ
      DO J = 1, L%NY
         DO I = 1, L%NX
            IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE
            IC = G%CELL_NUMBER(I,J,K)
            VC(IC) = VAL
         ENDDO
      ENDDO
   ENDDO
   !$OMP END PARALLEL DO 
   DO IC = G%NC+1, G%NCE
      VC(IC) = VAL
   ENDDO

ENDDO

END SUBROUTINE SCARC_VECTOR_INIT

! ------------------------------------------------------------------------------------------------
!> \brief Set exact solution according to specified function
! ------------------------------------------------------------------------------------------------
DOUBLE PRECISION FUNCTION EXACT(X,Z)
REAL (EB), INTENT(IN) :: X, Z
!EXACT = (X**2 - X**4) * (Z**4 - Z**2)                                    ! FUNCTION 1
!EXACT = (X**2 - 1) * (Z**2 - 1)                                         ! FUNCTION 2
!EXACT =  625.0_EB/16.0_EB * X * (0.8_EB - X) * Z * (0.8_EB - Z)        ! FUNCTION 3
EXACT = - X * (0.8_EB - X) * Z * (0.8_EB - Z)        ! FUNCTION 3
END FUNCTION EXACT


! ------------------------------------------------------------------------------------------------
!> \brief Set right hand side according to specified function
! ------------------------------------------------------------------------------------------------
DOUBLE PRECISION FUNCTION RHS(X,Z)
REAL (EB), INTENT(IN) :: X, Z
!RHS = 2.0_EB*((1.0_EB - 6.0_EB*X**2)*Z**2*(1.0_EB-Z**2)+(1.0_EB-6.0_EB*Z**2)*X**2*(1.0_EB-X**2))
!RHS = -X**2 - Z**2 +2
!RHS = 625.0_EB/8.0_EB * (X * (0.8_EB - X) + Z * (0.8_EB - Z))
RHS = 2.0_EB * (X * (0.8_EB - X) + Z * (0.8_EB - Z))
END FUNCTION RHS


! ------------------------------------------------------------------------------------------------
!> \brief Preset right hand side in such a way that exact solution is known
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_PRESET_EXACT (NE, NL)
USE SCARC_POINTERS, ONLY: M, L, G, VC, XMID, ZMID, SCARC_POINT_TO_GRID, SCARC_POINT_TO_VECTOR
USE SCARC_CONVERGENCE, ONLY: ITE_TOTAL
INTEGER, INTENT(IN) :: NE, NL
INTEGER :: IC, NM, I, K

IF (ITE_TOTAL == 0) WRITE(*,*) 'TODO: PRESET_EXACT is active !!!'

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
   VC => SCARC_POINT_TO_VECTOR (NM, NL, NE)

   DO K = 1, L%NZ
      DO I = 1, L%NX
         IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I,1,K)) CYCLE
         IC = G%CELL_NUMBER(I,1,K)
         IF (NL == NLEVEL_MIN) THEN
            XMID => M%XC
            ZMID => M%ZC
         ELSE
            XMID => L%XMID
            ZMID => L%ZMID
         ENDIF
         VC(IC) = EXACT(XMID(I),ZMID(K))
         !WRITE(MSG%LU_DEBUG,'(A,i3,a,e10.2,a,e10.2,a,E14.6)') 'IC=',IC,':X=',XMID(i),':Z=',ZMID(k),': RHS=',VC(IC)
      ENDDO
   ENDDO
ENDDO

END SUBROUTINE SCARC_PRESET_EXACT


! ------------------------------------------------------------------------------------------------
!> \brief Preset vector with specific values
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_PRESET_VECTOR (NV, NL)
USE SCARC_POINTERS, ONLY: M, L, G, VC, XMID, ZMID, SCARC_POINT_TO_GRID, SCARC_POINT_TO_VECTOR
INTEGER, INTENT(IN) :: NV, NL
INTEGER :: IC, NM, I, K

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
   VC => SCARC_POINT_TO_VECTOR (NM, NL, NV)

   VC = 0.0_EB
   DO K = 1, L%NZ
      DO I = 1, L%NX
         IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I,1,K)) CYCLE
         IC = G%CELL_NUMBER(I,1,K)
         IF (NL == NLEVEL_MIN) THEN
            XMID => M%XC
            ZMID => M%ZC
         ELSE
            XMID => L%XMID
            ZMID => L%ZMID
         ENDIF
         IF (XMID(I) < 0.1_EB) THEN
            VC(IC) = 1000.0_EB
         ELSE
            VC(IC) = 0.0_EB
         ENDIF
 ! WRITE(MSG%LU_DEBUG,*) 'IC=',IC,':X=',XMID,':Z=',ZMID,': RHS=',VC(IC)
      ENDDO
   ENDDO
ENDDO

END SUBROUTINE SCARC_PRESET_VECTOR


! ------------------------------------------------------------------------------------------------
!> \brief Preset right hand side in such a way that exact solution is known
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_PRESET_RHS (NV, NL)
USE SCARC_POINTERS, ONLY: M, L, G, VC, SCARC_POINT_TO_GRID, SCARC_POINT_TO_VECTOR
INTEGER, INTENT(IN) :: NV, NL
INTEGER :: IC, NM, I, K
REAL (EB) :: X, Z

IF (NL > NLEVEL_MIN) WRITE(*,*) 'Wrong level for presetting RHS '

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   M => MESHES(NM)
   M%BXS = 0.0_EB
   M%BXF = 0.0_EB
   M%BZS = 0.0_EB
   M%BZF = 0.0_EB

   CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
   VC => SCARC_POINT_TO_VECTOR (NM, NL, NV)

   DO K = 1, L%NZ
      DO I = 1, L%NX
         IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I,1,K)) CYCLE
         IC = G%CELL_NUMBER(I,1,K)
         X  = M%XC(I)
         Z  = M%ZC(K)
         !WRITE(MSG%LU_DEBUG,'(A,i3,a,e10.2,a,e10.2,a,E14.6)') 'IC=',IC,':X=',X,':Z=',Z,': RHS=',VC(IC)
         VC(IC) = RHS(X,Z)
      ENDDO
   ENDDO
ENDDO

END SUBROUTINE SCARC_PRESET_RHS

! ------------------------------------------------------------------------------------------------
!> \brief Compute global matrix-vector product A*x = y on grid level NL
! where NV1 is a reference to X and NV2 is a reference to Y
! including data exchange along internal boundaries
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_MATVEC_PRODUCT(NV1, NV2, NL)
USE SCARC_POINTERS, ONLY: L, OL, G, F, OG, GWC, A, AB, V1, V2, &
                          SCARC_POINT_TO_GRID, SCARC_POINT_TO_OTHER_GRID, &
                          SCARC_POINT_TO_VECTOR, SCARC_POINT_TO_CMATRIX, SCARC_POINT_TO_BMATRIX
INTEGER, INTENT(IN) :: NV1, NV2, NL           
REAL(EB) :: TNOW
INTEGER :: NM, NOM, IC, JC, IOR0, ICOL, INBR, ICE, ICW, ICG
INTEGER :: I, J, K, IW, IS=0, IT=0, IL=0, INUM1, INUM2
REAL(EB) :: TMP, VSAVE
#ifdef WITH_SCARC_MKL
EXTERNAL :: DAXPBY, DAXPY
#endif

TNOW = CURRENT_TIME()

! If this call is related to a globally acting solver, exchange internal boundary values of
! vector1 such that the ghost values contain the corresponding overlapped values of adjacent neighbor
 
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'CALLING MATVEC_PRODUCT FOR ', NV1, NV2, NL, TYPE_MATVEC
CALL SCARC_DEBUG_LEVEL (NV1, 'MATVEC: NV1 INIT0 ', NL)
CALL SCARC_DEBUG_LEVEL (NV2, 'MATVEC: NV2 INIT0 ', NL)
#endif

IF (TYPE_MATVEC == NSCARC_MATVEC_GLOBAL) CALL SCARC_EXCHANGE (NSCARC_EXCHANGE_VECTOR_PLAIN, NV1, NL)

! Perform global matrix-vector product:
! Note: - matrix already contains subdiagonal values from neighbor along internal boundaries
!       - if vector1 contains neighboring values, then correct values of global matvec are achieved
 
SELECT_MATRIX_TYPE: SELECT CASE (SCARC_GET_MATRIX_TYPE(NL))

   ! ------------- COMPACT storage technique
 
   CASE (NSCARC_MATRIX_COMPACT)
   

      MESHES_COMPACT_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
   
         CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
         IF (IS_LAPLACE) THEN
            A => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_LAPLACE)
         ELSE
            A => SCARC_POINT_TO_CMATRIX(G, NSCARC_MATRIX_POISSON)
         ENDIF
         
         V1 => SCARC_POINT_TO_VECTOR (NM, NL, NV1)
         V2 => SCARC_POINT_TO_VECTOR (NM, NL, NV2)
   
         IF (NL == NLEVEL_MIN) THEN
            DO IC = 1, G%NC
               ICOL = A%ROW(IC)                                                ! diagonal entry
               JC   = A%COL(ICOL)
               V2(IC) = A%VAL(ICOL)* V1(JC)
               DO ICOL = A%ROW(IC)+1, A%ROW(IC+1)-1                            ! subdiagonal entries
                  JC = A%COL(ICOL)
                  V2(IC) =  V2(IC) + A%VAL(ICOL) * V1(JC)
               ENDDO
            ENDDO
         ELSE
            DO IC = 1, G%NC
               ICOL = A%ROW(IC)                                                ! diagonal entry
               JC   = A%COL(ICOL)
               V2(IC) = A%VAL(ICOL)* V1(JC)
               DO ICOL = A%ROW(IC)+1, A%ROW(IC+1)-1                            ! subdiagonal entries
                  JC = A%COL(ICOL)
                  IF (JC == 0) CYCLE
                  V2(IC) =  V2(IC) + A%VAL(ICOL) * V1(JC)
               ENDDO
            ENDDO
         ENDIF
   
      ENDDO MESHES_COMPACT_LOOP
   
   ! ------------- bandwise storage technique
   ! matrix diagonals are supposed to be constant
   ! matrix-vector multiplication is based on daxpy-routines using the constant matrix stencil
   ! the 'wrong' entries due to boundary conditions and zero entries in subdiagonals are explicitly corrected 
 
   CASE (NSCARC_MATRIX_BANDWISE)
   
      SELECT_STENCIL_TYPE: SELECT CASE (TYPE_STENCIL)

         ! ---------- Variable entries with own implementation of daxpyv 
         !            matrix-vector multiplication is based on variable matrix stencil
 
         CASE (NSCARC_STENCIL_VARIABLE)
         
            MESHES_BANDWISE_VARIABLE_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
         
               CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
               AB => SCARC_POINT_TO_BMATRIX(G, NSCARC_MATRIX_POISSON)
         
               V1 => SCARC_POINT_TO_VECTOR (NM, NL, NV1)               ! point to X-vector
               V2 => SCARC_POINT_TO_VECTOR (NM, NL, NV2)               ! point to Y-vector
               V2 = 0.0_EB
         
               !!$OMP PARALLEL default(none) PRIVATE(IOR0, IL, IS, IT) SHARED(AB, V1, V2) 
               DO IOR0 = 3, -3, -1

                  IF (AB%POS(IOR0) == 0) CYCLE

                  IL = AB%LENGTH(IOR0) 
                  IS = AB%SOURCE(IOR0)
                  IT = AB%TARGET(IOR0)

                  CALL SCARC_DAXPY_VARIABLE(IL, AB%VAL(IT:IT+IL-1,AB%POS(IOR0)), V1(IS:IS+IL-1), V2(IT:IT+IL-1))

               ENDDO
               !!$OMP END PARALLEL

               IF (NM==NMESHES) VSAVE = V2(G%NC)                        ! save value in case of pure Neumann bdry

               DO IOR0 = 3, -3, -1
                  F => L%FACE(IOR0)
                  DO INBR = 1, F%N_NEIGHBORS
                     NOM = F%NEIGHBORS(INBR)
                     CALL SCARC_POINT_TO_OTHER_GRID(NM, NOM, NL)
                     DO ICG = OL%GHOST_FIRSTW(IOR0), OL%GHOST_LASTW(IOR0)
                        ICW = OG%ICG_TO_ICW(ICG, 1)
                        ICE = OG%ICG_TO_ICE(ICG, 1)
                        V2(ICW) = V2(ICW) + F%INCR_FACE * V1(ICE)
                     ENDDO
                  ENDDO
               ENDDO

               IF (IS_PURE_NEUMANN.AND.NM==NMESHES) V2(G%NC) = VSAVE   ! restore value in last cell

            ENDDO MESHES_BANDWISE_VARIABLE_LOOP

 
         ! ---------- Storage of constant matrix entries - with corrections at subdiagonals and diagonal (BC's)
 
         CASE (NSCARC_STENCIL_CONSTANT)

            MESHES_BANDWISE_CONSTANT_LOOP: DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX
         
               CALL SCARC_POINT_TO_GRID (NM, NL)                                   ! Sets grid pointer G
               AB => G%POISSONB
         
               V1 => SCARC_POINT_TO_VECTOR (NM, NL, NV1)               ! point to X-vector
               V2 => SCARC_POINT_TO_VECTOR (NM, NL, NV2)               ! point to Y-vector
               V2 = 0.0_EB
         
               DO IOR0 = 3, -3, -1

                  IF (AB%POS(IOR0) == 0) CYCLE
                  AB%AUX(1:G%NC)=0.0_EB
                  AB%AUX(1:AB%LENGTH(IOR0)) = V1(AB%SOURCE(IOR0):AB%SOURCE(IOR0)+AB%LENGTH(IOR0)-1)

                  IF (ABS(IOR0) == 1) THEN
                     DO IC = 1, G%NC
                        IF (MOD(IC,L%NX)==0) AB%AUX(IC)=0.0_EB
                     ENDDO
                  ELSE IF (ABS(IOR0) == 2) THEN
                     INUM1 = L%NX*L%NY
                     INUM2 = L%NX*L%NY - L%NX
                     DO IC = 1, G%NC
                        IF (MOD(IC,INUM1) > INUM2) AB%AUX(IC)=0.0_EB
                     ENDDO
                  ENDIF

                  IS = AB%SOURCE(IOR0)
                  IT = AB%TARGET(IOR0)
                  IL = AB%LENGTH(IOR0)

#ifdef WITH_SCARC_MKL
                  CALL DAXPY(IL, AB%STENCIL(IOR0), AB%AUX(1:IL), 1, V2(IT:IT+IL-1), 1)
#else
                  WRITE(*,*) 'TODO: MATVEC: CONSTANT: NO-MKL: CHECK HERE'
                  V2(IT:IT+IL-1) = AB%STENCIL(IOR0) * AB%AUX(1:IL)
#endif

               ENDDO
               WALL_CELLS_BANDWISE_LOOP: DO IW = 1, G%NW 

                  IOR0 = G%WALL(IW)%IOR
                  IF (TWO_D .AND. ABS(IOR0) == 2) CYCLE              ! cycle boundaries in y-direction for 2D-cases

                  GWC => G%WALL(IW)
                  F  => L%FACE(IOR0)
            
                  I = GWC%IXW
                  J = GWC%IYW
                  K = GWC%IZW
            
                  IF (IS_UNSTRUCTURED .AND. L%IS_SOLID(I, J, K)) CYCLE 
            
                  IC  = G%CELL_NUMBER(I, J, K) 
            
                  ! SPD-matrix with mixture of Dirichlet and Neumann BC's according to the SETTING of BTYPE

                  IF (N_DIRIC_GLOBAL(NLEVEL_MIN) > 0) THEN
                     TMP = V2(IC)
                     SELECT CASE (GWC%BTYPE)
                        CASE (DIRICHLET)
                           V2(IC) = V2(IC) - F%INCR_BOUNDARY * V1(IC)
                        CASE (NEUMANN)
                           V2(IC) = V2(IC) + F%INCR_BOUNDARY * V1(IC)
                     END SELECT
                  ENDIF 
            
               ENDDO WALL_CELLS_BANDWISE_LOOP
         
            ENDDO MESHES_BANDWISE_CONSTANT_LOOP

      END SELECT SELECT_STENCIL_TYPE
END SELECT SELECT_MATRIX_TYPE

#ifdef WITH_SCARC_DEBUG
CALL SCARC_DEBUG_LEVEL (NV1, 'MATVEC: NV1 EXIT1 ', NL)
CALL SCARC_DEBUG_LEVEL (NV2, 'MATVEC: NV2 EXIT1 ', NL)
#endif

CPU(MYID)%MATVEC_PRODUCT =CPU(MYID)%MATVEC_PRODUCT+CURRENT_TIME()-TNOW
END SUBROUTINE SCARC_MATVEC_PRODUCT

END MODULE SCARC_VECTORS


