!//////////////////////////////////////////////////////////////////////////////////////////////////////
!
! MODULE SCARC_POSTPROCESSING
!
!> \brief Dump out several structures to perform a postprocessing of selected data with
!   a separate stand-alone program
!
!//////////////////////////////////////////////////////////////////////////////////////////////////////
#ifdef WITH_SCARC_POSTPROCESSING
MODULE SCARC_POSTPROCESSING
  
USE GLOBAL_CONSTANTS
USE PRECISION_PARAMETERS, ONLY: EB, FB
USE MEMORY_FUNCTIONS, ONLY: CHKMEMERR
USE MPI
USE SCARC_CONSTANTS
USE SCARC_TYPES
USE SCARC_VARIABLES
USE SCARC_MESSAGES
USE SCARC_CONVERGENCE

IMPLICIT NONE

CONTAINS

! ------------------------------------------------------------------------------------------------
!> \brief POSTPROCESSING version only: Dump matrix and vectors belonging to pressure system 
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DUMP_SYSTEM (NSTACK, ITYPE)
USE SCARC_POINTERS, ONLY: SV, ST, G, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NSTACK, ITYPE
INTEGER  :: NM, IC, JC, JCG, IP, IW, IOR0, N
INTEGER  :: COLUMNSL(7), COLUMNSG(7)
REAL(EB) :: VALUES(7), VAL

SV  => STACK(NSTACK)%SOLVER

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   ST => SCARC(NM)%LEVEL(NLEVEL_MIN)%STAGE(SV%TYPE_STAGE)
   CALL SCARC_POINT_TO_GRID (NM, NLEVEL_MIN)

   SELECT CASE(ITYPE)

      CASE(NSCARC_DUMP_MESH)          

         MSG%LU_POST = GET_FILE_NUMBER()
         WRITE (MSG%FILE_POST, '(A,I3.3,A,I5.5)') 'CNN/M',NM,'/Mesh.dat'
         OPEN (MSG%LU_POST, FILE=MSG%FILE_POST, FORM = 'FORMATTED')
         WRITE(MSG%LU_POST,*) 'Total number of cells over all meshes'
         WRITE(MSG%LU_POST,*) NC_GLOBAL(NLEVEL_MIN)
         WRITE(MSG%LU_POST,*) 'Local number of cells in current mesh'
         WRITE(MSG%LU_POST,*) G%NC_LOCAL(NM)
         WRITE(MSG%LU_POST,*) 'Local number of cells in x, y and z in current mesh'
         WRITE(MSG%LU_POST,*) L%NX, L%NY, L%NZ
         WRITE(MSG%LU_POST,*) 'Section of local mesh in overall geometry with respect to global numbering'
         WRITE(MSG%LU_POST,*) G%NC_OFFSET(NM)+1, G%NC_OFFSET(NM)+G%NC_LOCAL(NM)
         WRITE(MSG%LU_POST,*) 'Coordinates of mesh x1, x2, y1, y2, z1, z2'
         WRITE(MSG%LU_POST,*) S%XS, S%XF, S%YS, S%YF, S%ZS, S%ZF
         WRITE(MSG%LU_POST,*) 'Local grid sizes in x, y and z in current mesh'
         WRITE(MSG%LU_POST,*) L%DX, L%DY, L%DZ
         CLOSE(MSG%LU_POST)

      CASE(NSCARC_DUMP_A)          

         A => G%POISSON
         MSG%LU_POST1 = GET_FILE_NUMBER() 
         MSG%LU_POST2 = GET_FILE_NUMBER() 
         MSG%LU_POST3 = GET_FILE_NUMBER() 
         WRITE (MSG%FILE_POST1, '(A,I3.3,A)') 'CNN/M',NM,'/A/values.dat'
         WRITE (MSG%FILE_POST2, '(A,I3.3,A)') 'CNN/M',NM,'/A/stencilLocal.dat'
         WRITE (MSG%FILE_POST3, '(A,I3.3,A)') 'CNN/M',NM,'/A/stencilGlobal.dat'
         OPEN (NEWUNIT=MSG%LU_POST1, FILE=MSG%FILE_POST1, ACTION='READWRITE', FORM = 'FORMATTED')
         OPEN (NEWUNIT=MSG%LU_POST2, FILE=MSG%FILE_POST2, ACTION='READWRITE', FORM = 'FORMATTED')
         OPEN (NEWUNIT=MSG%LU_POST3, FILE=MSG%FILE_POST3, ACTION='READWRITE', FORM = 'FORMATTED')
         DO IC = 1, A%N_ROW - 1
            COLUMNSL = 0
            COLUMNSG = 0
            VALUES = 0.0_EB
            DO IP = A%ROW(IC), A%ROW(IC+1)-1
               JC  = A%COL(IP)
               JCG = G%LOCAL_TO_GLOBAL(JC)
               VAL = A%VAL(IP)
               N = -1
               IW = -1
               IOR0 = 0
               IF (TWO_D) THEN
                  IF (IC - JC > 1 .AND. JC <= G%NC)  THEN
                     N = 1
                  ELSE IF (IC - JC == 1)  THEN
                     N = 2
                  ELSE IF (IC - JC == 0)  THEN
                     N = 3
                  ELSE IF (IC - JC == -1)  THEN
                     N = 4
                  ELSE IF ( JC <= G%NC .AND. IC - JC < -1)  THEN
                     N = 5
                  ELSE IF (JC > G%NC) THEN
                     IW = G%ICE_TO_IWG(JC)
                     IOR0 = G%WALL(IW)%IOR
                     SELECT CASE (IOR0)
                        CASE (-3)
                           N = 5
                        CASE (-1)
                           N = 4
                        CASE ( 1)
                           N = 2
                        CASE ( 3)
                           N = 1
                     END SELECT
                  ENDIF
               ELSE
                  IF (IC - JC == 0)  THEN
                     N = 4
                  ELSE IF (IC - JC == 1)  THEN
                     N = 3
                  ELSE IF (IC - JC == -1)  THEN
                     N = 5
                  ELSE IF (1 < IC - JC .AND. IC - JC <= L%NX)  THEN
                     N = 2
                  ELSE IF (-L%NX <= IC - JC  .AND. IC - JC < -1 )  THEN
                     N = 6
                  ELSE IF (L%NX < IC - JC .AND. JC <= G%NC)  THEN
                     N = 1
                  ELSE IF (JC <= G%NC .AND. IC - JC <= -L%NX)  THEN
                     N = 7
                  ELSE IF (JC > G%NC) THEN
                     IW = G%ICE_TO_IWG(JC)
                     IOR0 = G%WALL(IW)%IOR
                     SELECT CASE (IOR0)
                        CASE (-3)
                           N = 7
                        CASE (-2)
                           N = 6
                        CASE (-1)
                           N = 5
                        CASE ( 1)
                           N = 3
                        CASE ( 2)
                           N = 2
                        CASE ( 3)
                           N = 1
                     END SELECT
                  ENDIF
               ENDIF
               IF (N > 0) THEN
                  VALUES(N) = VAL
                  COLUMNSL(N) = JC
                  COLUMNSG(N) = JCG
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'IP, IC, JC, IW, IOR0, VAL, N:', IP, IC, JC, JCG, IW, IOR0, VAL, N
#endif
               ENDIF
            ENDDO
            IF (TWO_D) THEN
               WRITE(MSG%LU_POST1,*) VALUES(1:5)
               WRITE(MSG%LU_POST2,*) COLUMNSL(1:5)
               WRITE(MSG%LU_POST3,*) COLUMNSG(1:5)
            ELSE
               WRITE(MSG%LU_POST1,*) VALUES(1:7)
               WRITE(MSG%LU_POST2,*) COLUMNSL(1:7)
               WRITE(MSG%LU_POST3,*) COLUMNSG(1:7)
#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,'(A,7I84,7E14.6)') 'COLUMNS, VALUES:', COLUMNSL(1:7), VALUES(1:7)
#endif
            ENDIF
         ENDDO
         CLOSE(MSG%LU_POST1)
         CLOSE(MSG%LU_POST2)
         CLOSE(MSG%LU_POST3)

      CASE(NSCARC_DUMP_B)

         MSG%LU_POST = GET_FILE_NUMBER() 
         WRITE (MSG%FILE_POST, '(A,I3.3,A,I6.6,A)') 'CNN/M',NM,'/B/values_t',ITE_GLOBAL,'.dat'
         OPEN (MSG%LU_POST, FILE=MSG%FILE_POST, FORM = 'FORMATTED')
         DO IC = 1, G%NC
            WRITE(MSG%LU_POST,*) ST%B(IC)
         ENDDO
         CLOSE(MSG%LU_POST)

      CASE(NSCARC_DUMP_X)

         MSG%LU_POST = GET_FILE_NUMBER() 
         WRITE (MSG%FILE_POST, '(A,I3.3,A,I6.6,A)') 'CNN/M',NM,'/X/values_t',ITE_GLOBAL,'.dat'
         OPEN (MSG%LU_POST, FILE=MSG%FILE_POST, FORM = 'FORMATTED')
         DO IC = 1, G%NC
            WRITE(MSG%LU_POST,*) ST%X(IC)
         ENDDO
         CLOSE(MSG%LU_POST)

   END SELECT

ENDDO

END SUBROUTINE SCARC_DUMP_SYSTEM

! ------------------------------------------------------------------------------------------------
!> \brief POSTPROCESSING version only: Dump complete FDS environment needed for ScaRC-setup (only for developping purposes)
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_DUMP_ENVIRONMENT
USE SCARC_POINTERS, ONLY: M
INTEGER :: NM, NOM, I, J, K, IW

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   M => MESHES(NM)

   ! Open SCARC-file for mesh NM
   WRITE (MSG%FILE_SCARC, '(A,A,A,i3.3)') 'dump/',TRIM(CHID),'_elk.dump',NM
   MSG%LU_SCARC = GET_FILE_NUMBER()
   OPEN (MSG%LU_SCARC, FILE=MSG%FILE_SCARC, ACTION = 'readwrite')

   ! Current process number
   WRITE(MSG%LU_SCARC,*) 'PROCESS(NM)'
   WRITE(MSG%LU_SCARC,*) PROCESS(NM)

   ! Dump information about all necessary MESHES-quantities
   WRITE(MSG%LU_SCARC,*) 'M%IBAR, M%JBAR, M%KBAR'
   WRITE(MSG%LU_SCARC,*) M%IBAR, M%JBAR, M%KBAR
   WRITE(MSG%LU_SCARC,*) 'M%IBP1, M%JBP1, M%KBP1'
   WRITE(MSG%LU_SCARC,*) M%IBP1, M%JBP1, M%KBP1
   WRITE(MSG%LU_SCARC,*) 'M%ITRN, M%JTRN, M%KTRN'
   WRITE(MSG%LU_SCARC,*) M%ITRN, M%JTRN, M%KTRN
   WRITE(MSG%LU_SCARC,*) 'M%LBC, M%MBC, M%NBC'
   WRITE(MSG%LU_SCARC,*) M%LBC, M%MBC, M%NBC
   WRITE(MSG%LU_SCARC,*) 'M%N_WALL_CELLS'
   WRITE(MSG%LU_SCARC,*) M%N_WALL_CELLS
   WRITE(MSG%LU_SCARC,*) 'M%N_EXTERNAL_WALL_CELLS'
   WRITE(MSG%LU_SCARC,*) M%N_EXTERNAL_WALL_CELLS
   WRITE(MSG%LU_SCARC,*) 'M%N_INTERNAL_WALL_CELLS'
   WRITE(MSG%LU_SCARC,*) M%N_INTERNAL_WALL_CELLS
   WRITE(MSG%LU_SCARC,*) 'M%IPS, M%LSAVE, M%LWORK'
   WRITE(MSG%LU_SCARC,*) M%IPS, M%LSAVE, M%LWORK
   WRITE(MSG%LU_SCARC,*) 'M%POIS_PTB'
   WRITE(MSG%LU_SCARC,*) M%POIS_PTB
   WRITE(MSG%LU_SCARC,*) 'M%XS, M%XF, M%YS, M%YF, M%ZS, M%ZF'
   WRITE(MSG%LU_SCARC,*) M%XS, M%XF, M%YS, M%YF, M%ZS, M%ZF
   WRITE(MSG%LU_SCARC,*) 'M%DXI, M%DETA, M%DZETA'
   WRITE(MSG%LU_SCARC,*) M%DXI, M%DETA, M%DZETA
   WRITE(MSG%LU_SCARC,*) 'M%RDXI, M%RDETA, M%RDZETA'
   WRITE(MSG%LU_SCARC,*) M%RDXI, M%RDETA, M%RDZETA
   WRITE(MSG%LU_SCARC,*) 'M%X'
   WRITE(MSG%LU_SCARC,*) (M%X(I), I=0, M%IBAR)
   WRITE(MSG%LU_SCARC,*) 'M%Y'
   WRITE(MSG%LU_SCARC,*) (M%Y(I), I=0, M%JBAR)
   WRITE(MSG%LU_SCARC,*) 'M%Z'
   WRITE(MSG%LU_SCARC,*) (M%Z(I), I=0, M%KBAR)
   WRITE(MSG%LU_SCARC,*) 'M%XC'
   WRITE(MSG%LU_SCARC,*) (M%XC(I), I=0, M%IBP1)
   WRITE(MSG%LU_SCARC,*) 'M%YC'
   WRITE(MSG%LU_SCARC,*) (M%YC(I), I=0, M%JBP1)
   WRITE(MSG%LU_SCARC,*) 'M%ZC'
   WRITE(MSG%LU_SCARC,*) (M%ZC(I), I=0, M%KBP1)
   WRITE(MSG%LU_SCARC,*) 'M%HX'
   WRITE(MSG%LU_SCARC,*) (M%HX(I), I=0, M%IBP1)
   WRITE(MSG%LU_SCARC,*) 'M%HY'
   WRITE(MSG%LU_SCARC,*) (M%HY(I), I=0, M%JBP1)
   WRITE(MSG%LU_SCARC,*) 'M%HZ'
   WRITE(MSG%LU_SCARC,*) (M%HZ(I), I=0, M%KBP1)

   DO NOM = 1, NMESHES
      WRITE(MSG%LU_SCARC,*) 'M%OMESH(',NOM,')%NIC_R, NICS'
      WRITE(MSG%LU_SCARC,*) M%OMESH(NOM)%NIC_R, M%OMESH(NOM)%NIC_S
   ENDDO

   IF (PREDICTOR) THEN
      WRITE(MSG%LU_SCARC,*) 'M%H'
      WRITE(MSG%LU_SCARC,*) (((M%H(I,J,K), I=0, M%IBP1), J=0, M%JBP1), K=0, M%KBP1)
   ELSE
      WRITE(MSG%LU_SCARC,*) 'M%HS'
      WRITE(MSG%LU_SCARC,*) (((M%HS(I,J,K), I=0, M%IBP1), J=0, M%JBP1), K=0, M%KBP1)
   ENDIF

   WRITE(MSG%LU_SCARC,*) 'M%PRHS'
   IF (TWO_D) THEN
      WRITE(MSG%LU_SCARC,*) ((M%PRHS(I,1,K), I=1, M%IBP1), K=1, M%KBP1)
   ELSE
      WRITE(MSG%LU_SCARC,*) (((M%PRHS(I,J,K), I=1, M%IBP1), J=1, M%JBP1), K=1, M%KBP1)
   ENDIF

   IF (TWO_D) THEN
      WRITE(MSG%LU_SCARC,*) 'M%BXS'
      WRITE(MSG%LU_SCARC,*) (M%BXS(1,K), K=1, M%KBP1)
      WRITE(MSG%LU_SCARC,*) 'M%BXF'
      WRITE(MSG%LU_SCARC,*) (M%BXF(1,K), K=1, M%KBP1)
   ELSE
      WRITE(MSG%LU_SCARC,*) 'M%BXS'
      WRITE(MSG%LU_SCARC,*) ((M%BXS(J,K), J=1, M%JBP1), K=1, M%KBP1)
      WRITE(MSG%LU_SCARC,*) 'M%BXF'
      WRITE(MSG%LU_SCARC,*) ((M%BXF(J,K), J=1, M%JBP1), K=1, M%KBP1)
   ENDIF

   WRITE(MSG%LU_SCARC,*) 'M%BYS'
   WRITE(MSG%LU_SCARC,*) ((M%BYS(I,K), I=1, M%IBP1), K=1, M%KBP1)
   WRITE(MSG%LU_SCARC,*) 'M%BYF'
   WRITE(MSG%LU_SCARC,*) ((M%BYF(I,K), I=1, M%IBP1), K=1, M%KBP1)

   IF (TWO_D) THEN
      WRITE(MSG%LU_SCARC,*) 'M%BZS'
      WRITE(MSG%LU_SCARC,*) (M%BZS(I,1), I=1, M%IBP1)
      WRITE(MSG%LU_SCARC,*) 'M%BZF'
      WRITE(MSG%LU_SCARC,*) (M%BZF(I,1), I=1, M%IBP1)
   ELSE
      WRITE(MSG%LU_SCARC,*) 'M%BZS'
      WRITE(MSG%LU_SCARC,*) ((M%BZS(I,J), I=1, M%IBP1), J=1, M%JBP1)
      WRITE(MSG%LU_SCARC,*) 'M%BZF'
      WRITE(MSG%LU_SCARC,*) ((M%BZF(I,J), I=1, M%IBP1), J=1, M%JBP1)
   ENDIF

   WRITE(MSG%LU_SCARC,*) 'M%WALL(.)%PRESSURE_BC_INDEX'
   WRITE(MSG%LU_SCARC,*) (M%WALL(IW)%PRESSURE_BC_INDEX, IW=1, M%N_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%WALL(.)%BOUNDARY_TYPE'
   WRITE(MSG%LU_SCARC,*) (M%WALL(IW)%BOUNDARY_TYPE, IW=1, M%N_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%WALL(.)%WALL_INDEX'
   WRITE(MSG%LU_SCARC,*) (M%WALL(IW)%WALL_INDEX, IW=1, M%N_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%WALL(.)%SURF_INDEX'
   WRITE(MSG%LU_SCARC,*) (M%WALL(IW)%SURF_INDEX, IW=1, M%N_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%WALL(.)%BACK_INDEX'
   WRITE(MSG%LU_SCARC,*) (M%WALL(IW)%BACK_INDEX, IW=1, M%N_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%WALL(.)%OBST_INDEX'
   WRITE(MSG%LU_SCARC,*) (M%WALL(IW)%OBST_INDEX, IW=1, M%N_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%WALL(.)%VENT_INDEX'
   WRITE(MSG%LU_SCARC,*) (M%WALL(IW)%VENT_INDEX, IW=1, M%N_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%WALL(.)%ONE_D%IOR'
   WRITE(MSG%LU_SCARC,*) (M%WALL(IW)%ONE_D%IOR, IW=1, M%N_EXTERNAL_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%WALL(.)%ONE_D%II'
   WRITE(MSG%LU_SCARC,*) (M%WALL(IW)%ONE_D%II, IW=1, M%N_EXTERNAL_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%WALL(.)%ONE_D%JJ'
   WRITE(MSG%LU_SCARC,*) (M%WALL(IW)%ONE_D%JJ, IW=1, M%N_EXTERNAL_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%WALL(.)%ONE_D%KK'
   WRITE(MSG%LU_SCARC,*) (M%WALL(IW)%ONE_D%KK, IW=1, M%N_EXTERNAL_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%WALL(.)%ONE_D%IIG'
   WRITE(MSG%LU_SCARC,*) (M%WALL(IW)%ONE_D%IIG, IW=1, M%N_EXTERNAL_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%WALL(.)%ONE_D%JJG'
   WRITE(MSG%LU_SCARC,*) (M%WALL(IW)%ONE_D%JJG, IW=1, M%N_EXTERNAL_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%WALL(.)%ONE_D%KKG'
   WRITE(MSG%LU_SCARC,*) (M%WALL(IW)%ONE_D%KKG, IW=1, M%N_EXTERNAL_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%EXTERNAL_WALL(.)%NOM'
   WRITE(MSG%LU_SCARC,*) (M%EXTERNAL_WALL(IW)%NOM, IW=1, M%N_EXTERNAL_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%EXTERNAL_WALL(.)%IIO_MIN'
   WRITE(MSG%LU_SCARC,*) (M%EXTERNAL_WALL(IW)%IIO_MIN, IW=1, M%N_EXTERNAL_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%EXTERNAL_WALL(.)%IIO_MAX'
   WRITE(MSG%LU_SCARC,*) (M%EXTERNAL_WALL(IW)%IIO_MAX, IW=1, M%N_EXTERNAL_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%EXTERNAL_WALL(.)%JJO_MIN'
   WRITE(MSG%LU_SCARC,*) (M%EXTERNAL_WALL(IW)%JJO_MIN, IW=1, M%N_EXTERNAL_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%EXTERNAL_WALL(.)%JJO_MAX'
   WRITE(MSG%LU_SCARC,*) (M%EXTERNAL_WALL(IW)%JJO_MAX, IW=1, M%N_EXTERNAL_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%EXTERNAL_WALL(.)%KKO_MIN'
   WRITE(MSG%LU_SCARC,*) (M%EXTERNAL_WALL(IW)%KKO_MIN, IW=1, M%N_EXTERNAL_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%EXTERNAL_WALL(.)%KKO_MAX'
   WRITE(MSG%LU_SCARC,*) (M%EXTERNAL_WALL(IW)%KKO_MAX, IW=1, M%N_EXTERNAL_WALL_CELLS)

   WRITE(MSG%LU_SCARC,*) 'M%N_OBST'
   WRITE(MSG%LU_SCARC,*) M%N_OBST
   IF (M%N_OBST > 0) THEN
      WRITE(MSG%LU_SCARC,*) 'M%OBSTRUCTION(.)%I1, I2, J1, J2, K1, K2'
      DO I = 1, M%N_OBST
         WRITE(MSG%LU_SCARC,*) M%OBSTRUCTION(I)%I1, M%OBSTRUCTION(I)%I2, &
                               M%OBSTRUCTION(I)%J1, M%OBSTRUCTION(I)%J2, &
                               M%OBSTRUCTION(I)%K1, M%OBSTRUCTION(I)%K2
      ENDDO
   ENDIF

   WRITE(MSG%LU_SCARC,*) 'CELL_COUNT(',NM,')'
   WRITE(MSG%LU_SCARC,*) CELL_COUNT(NM)

   WRITE(MSG%LU_SCARC,*) 'M%SOLID'
   WRITE(MSG%LU_SCARC,*) (M%SOLID(I), I=1, CELL_COUNT(NM))

   WRITE(MSG%LU_SCARC,*) 'M%CELL_INDEX'
   WRITE(MSG%LU_SCARC,*) (((M%CELL_INDEX(I,J,K), I=0, M%IBAR+1), J=0, M%JBAR+1), K=0, M%KBAR+1)

   WRITE(MSG%LU_SCARC,*) 'M%WALL_INDEX'
   DO I = 0, CELL_COUNT(NM)
      WRITE(MSG%LU_SCARC,'(7I8)') (M%WALL_INDEX(I,J), J=-3,3)
   ENDDO

   WRITE(MSG%LU_SCARC,*) 'M%SAVE1'
   WRITE(MSG%LU_SCARC,*) (M%SAVE1(I), I=-3, M%LSAVE)

   WRITE(MSG%LU_SCARC,*) 'M%WORK'
   WRITE(MSG%LU_SCARC,*) (M%WORK(I), I=1, M%LWORK)

   CLOSE (MSG%LU_SCARC)

ENDDO

SCARC_DUMP = .FALSE.
END SUBROUTINE SCARC_DUMP_ENVIRONMENT


! ------------------------------------------------------------------------------------------------
!> \brief POSTPROCESSING version only: Dump several arrays and structures needed for ScaRC (only for developping purposes)
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_RESTORE_ENVIRONMENT
USE SCARC_POINTERS, ONLY: M
INTEGER :: NM, NOM, I, J, K, IW, LU

!CPATH = 'D:\GIT\HHP\A_ScaRC\VisualStudio\Cases\'
!CPATH = '../VisualStudio/Cases/'

ALLOCATE(CELL_COUNT(NMESHES), STAT= IERROR)
CALL CHKMEMERR ('SCARC_RESTORE_ENVIRONMENT', 'CELL_COUNT', IERROR)

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   M => MESHES(NM)

   MSG%LU_SCARC = 99
   WRITE (MSG%FILE_SCARC, '(A,A,A,i3.3)') 'dump/',TRIM(CHID),'.dump',NM
   write(*,*) MSG%FILE_SCARC
   MSG%FILE_SCARC = TRIM(MSG%FILE_SCARC)

   LU = MSG%LU_SCARC
   OPEN (LU, FILE=MSG%FILE_SCARC)

   ! Current process number
   READ(LU,*)
   READ(LU,*) PROCESS(NM)

   ! Dump information about all necessary MESHES-quantities
   READ(LU,*)
   READ(LU,*) M%IBAR, M%JBAR, M%KBAR
   READ(LU,*) 
   READ(LU,*) M%IBP1, M%JBP1, M%KBP1
   READ(LU,*) 
   READ(LU,*) M%ITRN, M%JTRN, M%KTRN
   READ(LU,*) 
   READ(LU,*) M%LBC, M%MBC, M%NBC
   READ(LU,*) 
   READ(LU,*) M%N_WALL_CELLS
   READ(LU,*)
   READ(LU,*) M%N_EXTERNAL_WALL_CELLS
   READ(LU,*)
   READ(LU,*) M%N_INTERNAL_WALL_CELLS
   READ(LU,*)
   READ(LU,*) M%IPS, M%LSAVE, M%LWORK
   READ(LU,*) 
   READ(LU,*) M%POIS_PTB
   READ(LU,*)
   READ(LU,*) M%XS, M%XF, M%YS, M%YF, M%ZS, M%ZF
   READ(LU,*) 
   READ(LU,*) M%DXI, M%DETA, M%DZETA
   READ(LU,*) 
   READ(LU,*) M%RDXI, M%RDETA, M%RDZETA

   ALLOCATE(M%X(0:M%IBAR), STAT= IERROR)
   CALL CHKMEMERR ('SCARC_RESTORE_ENVIRONMENT', 'M%X', IERROR)

   ALLOCATE(M%Y(0:M%JBAR), STAT= IERROR)
   CALL CHKMEMERR ('SCARC_RESTORE_ENVIRONMENT', 'M%Y', IERROR)

   ALLOCATE(M%Z(0:M%KBAR), STAT= IERROR)
   CALL CHKMEMERR ('SCARC_RESTORE_ENVIRONMENT', 'M%Z', IERROR)

   ALLOCATE(M%XC(0:M%IBP1), STAT= IERROR)
   CALL CHKMEMERR ('SCARC_RESTORE_ENVIRONMENT', 'M%XC', IERROR)

   ALLOCATE(M%YC(0:M%JBP1), STAT= IERROR)
   CALL CHKMEMERR ('SCARC_RESTORE_ENVIRONMENT', 'M%YC', IERROR)

   ALLOCATE(M%ZC(0:M%KBP1), STAT= IERROR)
   CALL CHKMEMERR ('SCARC_RESTORE_ENVIRONMENT', 'M%ZC', IERROR)

   ALLOCATE(M%HX(0:M%IBP1), STAT= IERROR)
   CALL CHKMEMERR ('SCARC_RESTORE_ENVIRONMENT', 'M%HX', IERROR)

   ALLOCATE(M%HY(0:M%JBP1), STAT= IERROR)
   CALL CHKMEMERR ('SCARC_RESTORE_ENVIRONMENT', 'M%HY', IERROR)

   ALLOCATE(M%HZ(0:M%KBP1), STAT= IERROR)
   CALL CHKMEMERR ('SCARC_RESTORE_ENVIRONMENT', 'M%HZ', IERROR)

   READ(LU,*)
   READ(LU,*) (M%X(I), I=0, M%IBAR)
   READ(LU,*)
   READ(LU,*) (M%Y(I), I=0, M%JBAR)
   READ(LU,*)
   READ(LU,*) (M%Z(I), I=0, M%KBAR)
   READ(LU,*)
   READ(LU,*) (M%XC(I), I=0, M%IBP1)
   READ(LU,*)
   READ(LU,*) (M%YC(I), I=0, M%JBP1)
   READ(LU,*)
   READ(LU,*) (M%ZC(I), I=0, M%KBP1)

   READ(LU,*) 
   READ(LU,*) (M%HX(I), I=0, M%IBP1)
   READ(LU,*) 
   READ(LU,*) (M%HY(I), I=0, M%JBP1)
   READ(LU,*) 
   READ(LU,*) (M%HZ(I), I=0, M%KBP1)

   ALLOCATE(M%OMESH(NMESHES), STAT= IERROR)
   CALL CHKMEMERR ('SCARC_RESTORE_ENVIRONMENT', 'M%OMESH', IERROR)

   DO NOM = 1, NMESHES
      READ(LU,*)
      READ(LU,*) M%OMESH(NOM)%NIC_R, M%OMESH(NOM)%NIC_S
   ENDDO

   IF (PREDICTOR) THEN

      ALLOCATE(M%H(0:M%IBP1, 0:M%JBP1, 0:M%KBP1), STAT= IERROR)
      CALL CHKMEMERR ('SCARC_RESTORE_ENVIRONMENT', 'M%H', IERROR)
      M%H = 0.0_EB

      READ(LU,*)
      READ(LU,*) (((M%H(I,J,K), I=0, M%IBP1), J=0, M%JBP1), K=0, M%KBP1)

   ELSE

      ALLOCATE(M%HS(0:M%IBP1, 0:M%JBP1, 0:M%KBP1), STAT= IERROR)
      CALL CHKMEMERR ('SCARC_RESTORE_ENVIRONMENT', 'M%H', IERROR)
      M%HS = 0.0_EB

      READ(LU,*)
      READ(LU,*) (((M%HS(I,J,K), I=0, M%IBP1), J=0, M%JBP1), K=0, M%KBP1)

   ENDIF
   IF (TWO_D) THEN

      ALLOCATE(M%PRHS(M%IBP1, 1, M%KBP1), STAT= IERROR)
      CALL CHKMEMERR ('SCARC_RESTORE_ENVIRONMENT', 'M%H', IERROR)

      READ(LU,*)
      READ(LU,*) ((M%PRHS(I,1,K), I=1, M%IBP1), K=1, M%KBP1)

   ELSE

      ALLOCATE(M%PRHS(M%IBP1, M%JBP1, M%KBP1), STAT= IERROR)
      CALL CHKMEMERR ('SCARC_RESTORE_ENVIRONMENT', 'M%H', IERROR)

      READ(LU,*)
      READ(LU,*) (((M%PRHS(I,J,K), I=1, M%IBP1), J=1, M%JBP1), K=1, M%KBP1)

   ENDIF

   IF (TWO_D) THEN

      ALLOCATE(M%BXS(1,M%KBP1), STAT= IERROR)
      CALL CHKMEMERR ('READ_SCARC', 'M%BXS', IERROR)

      ALLOCATE(M%BXF(1,M%KBP1), STAT= IERROR)
      CALL CHKMEMERR ('READ_SCARC', 'M%BXF', IERROR)

      ALLOCATE(M%BZS(M%IBP1,1), STAT= IERROR)
      CALL CHKMEMERR ('READ_SCARC', 'M%BZS', IERROR)

      ALLOCATE(M%BZF(M%IBP1,1), STAT= IERROR)
      CALL CHKMEMERR ('READ_SCARC', 'M%BZF', IERROR)

   ELSE

      ALLOCATE(M%BXS(M%JBP1,M%KBP1), STAT= IERROR)
      CALL CHKMEMERR ('READ_SCARC', 'M%BXS', IERROR)

      ALLOCATE(M%BXF(M%JBP1,M%KBP1), STAT= IERROR)
      CALL CHKMEMERR ('READ_SCARC', 'M%BXF', IERROR)

      ALLOCATE(M%BZS(M%IBP1,M%JBP1), STAT= IERROR)
      CALL CHKMEMERR ('READ_SCARC', 'M%BZS', IERROR)

      ALLOCATE(M%BZF(M%IBP1,M%JBP1), STAT= IERROR)
      CALL CHKMEMERR ('READ_SCARC', 'M%BZF', IERROR)

   ENDIF
   
   ALLOCATE(M%BYS(M%IBP1,M%KBP1), STAT= IERROR)
   CALL CHKMEMERR ('READ_SCARC', 'M%BYS', IERROR)

   ALLOCATE(M%BYF(M%IBP1,M%KBP1), STAT= IERROR)
   CALL CHKMEMERR ('READ_SCARC', 'M%BYF', IERROR)

   IF (TWO_D) THEN
      READ(LU,*)
      READ(LU,*) (M%BXS(1,K), K=1, M%KBP1)
      READ(LU,*)
      READ(LU,*) (M%BXF(1,K), K=1, M%KBP1)
   ELSE
      READ(LU,*)
      READ(LU,*) ((M%BXS(J,K), J=1, M%JBP1), K=1, M%KBP1)
      READ(LU,*)
      READ(LU,*) ((M%BXF(J,K), J=1, M%JBP1), K=1, M%KBP1)
   ENDIF
   
   READ(LU,*)
   READ(LU,*) ((M%BYS(I,K), I=1, M%IBP1), K=1, M%KBP1)
   READ(LU,*)
   READ(LU,*) ((M%BYF(I,K), I=1, M%IBP1), K=1, M%KBP1)

   IF (TWO_D) THEN
      READ(LU,*)
      READ(LU,*) (M%BZS(I,1), I=1, M%IBP1)
      READ(LU,*)
      READ(LU,*) (M%BZF(I,1), I=1, M%IBP1)
   ELSE
      READ(LU,*)
      READ(LU,*) ((M%BZS(I,J), I=1, M%IBP1), J=1, M%JBP1)
      READ(LU,*)
      READ(LU,*) ((M%BZF(I,J), I=1, M%IBP1), J=1, M%JBP1)
   ENDIF

   ALLOCATE(M%WALL(M%N_WALL_CELLS), STAT= IERROR)
   CALL CHKMEMERR ('READ_SCARC', 'M%WALL', IERROR)

   ALLOCATE(M%EXTERNAL_WALL(M%N_EXTERNAL_WALL_CELLS), STAT= IERROR)
   CALL CHKMEMERR ('READ_SCARC', 'M%EXTERNAL_WALL', IERROR)

   READ(LU,*)
   READ(LU,*) (M%WALL(IW)%PRESSURE_BC_INDEX, IW=1, M%N_WALL_CELLS)

   READ(LU,*)
   READ(LU,*) (M%WALL(IW)%BOUNDARY_TYPE, IW=1, M%N_WALL_CELLS)

   READ(LU,*)
   READ(LU,*) (M%WALL(IW)%WALL_INDEX, IW=1, M%N_WALL_CELLS)

   READ(LU,*)
   READ(LU,*) (M%WALL(IW)%SURF_INDEX, IW=1, M%N_WALL_CELLS)

   READ(LU,*)
   READ(LU,*) (M%WALL(IW)%BACK_INDEX, IW=1, M%N_WALL_CELLS)

   READ(LU,*)
   READ(LU,*) (M%WALL(IW)%OBST_INDEX, IW=1, M%N_WALL_CELLS)

   READ(LU,*)
   READ(LU,*) (M%WALL(IW)%VENT_INDEX, IW=1, M%N_WALL_CELLS)

   READ(LU,*)
   READ(LU,*) (M%WALL(IW)%ONE_D%IOR, IW=1, M%N_EXTERNAL_WALL_CELLS)

   READ(LU,*)
   READ(LU,*) (M%WALL(IW)%ONE_D%II, IW=1, M%N_EXTERNAL_WALL_CELLS)

   READ(LU,*)
   READ(LU,*) (M%WALL(IW)%ONE_D%JJ, IW=1, M%N_EXTERNAL_WALL_CELLS)

   READ(LU,*)
   READ(LU,*) (M%WALL(IW)%ONE_D%KK, IW=1, M%N_EXTERNAL_WALL_CELLS)

   READ(LU,*)
   READ(LU,*) (M%WALL(IW)%ONE_D%IIG, IW=1, M%N_EXTERNAL_WALL_CELLS)

   READ(LU,*)
   READ(LU,*) (M%WALL(IW)%ONE_D%JJG, IW=1, M%N_EXTERNAL_WALL_CELLS)

   READ(LU,*)
   READ(LU,*) (M%WALL(IW)%ONE_D%KKG, IW=1, M%N_EXTERNAL_WALL_CELLS)

   READ(LU,*) 
   READ(LU,*) (M%EXTERNAL_WALL(IW)%NOM, IW=1, M%N_EXTERNAL_WALL_CELLS)

   READ(LU,*)
   READ(LU,*) (M%EXTERNAL_WALL(IW)%IIO_MIN, IW=1, M%N_EXTERNAL_WALL_CELLS)

   READ(LU,*)
   READ(LU,*) (M%EXTERNAL_WALL(IW)%IIO_MAX, IW=1, M%N_EXTERNAL_WALL_CELLS)

   READ(LU,*) 
   READ(LU,*) (M%EXTERNAL_WALL(IW)%JJO_MIN, IW=1, M%N_EXTERNAL_WALL_CELLS)

   READ(LU,*) 
   READ(LU,*) (M%EXTERNAL_WALL(IW)%JJO_MAX, IW=1, M%N_EXTERNAL_WALL_CELLS)

   READ(LU,*) 
   READ(LU,*) (M%EXTERNAL_WALL(IW)%KKO_MIN, IW=1, M%N_EXTERNAL_WALL_CELLS)

   READ(LU,*)
   READ(LU,*) (M%EXTERNAL_WALL(IW)%KKO_MAX, IW=1, M%N_EXTERNAL_WALL_CELLS)

   READ(LU,*)
   READ(LU,*) M%N_OBST

   IF (M%N_OBST > 0) THEN

      ALLOCATE(M%OBSTRUCTION(M%N_OBST), STAT= IERROR)
      CALL CHKMEMERR ('READ_SCARC', 'M%OBSTRUCTION', IERROR)

      READ(LU,*)
      DO I = 1, M%N_OBST
         READ(LU,*) M%OBSTRUCTION(I)%I1, M%OBSTRUCTION(I)%I2, &
                    M%OBSTRUCTION(I)%J1, M%OBSTRUCTION(I)%J2, &
                    M%OBSTRUCTION(I)%K1, M%OBSTRUCTION(I)%K2
      ENDDO
   ENDIF

   READ(LU,*)
   READ(LU,*) CELL_COUNT(NM)

   ALLOCATE(M%SOLID(0:CELL_COUNT(NM)), STAT= IERROR)
   CALL CHKMEMERR ('READ_SCARC', 'M%SOLID', IERROR)
   M%SOLID=.FALSE.

   ALLOCATE(M%CELL_INDEX(0:M%IBP1,0:M%JBP1,0:M%KBP1), STAT= IERROR)
   CALL CHKMEMERR ('READ_SCARC', 'M%CELL_INDEX', IERROR)
   M%CELL_INDEX = 0

   ALLOCATE(M%WALL_INDEX(0:CELL_COUNT(NM),-3:3), STAT= IERROR)
   CALL CHKMEMERR ('READ_SCARC', 'M%WALL_INDEX', IERROR)

   READ(LU,*)
   READ(LU,*) (M%SOLID(I), I=1, CELL_COUNT(NM))

   READ(LU,*)
   READ(LU,*) (((M%CELL_INDEX(I,J,K), I=0, M%IBP1), J=0, M%JBP1), K=0, M%KBP1)

   READ(LU,*)
   DO I = 0, CELL_COUNT(NM)
      READ(LU,'(7I8)') (M%WALL_INDEX(I,J), J=-3,3)
   ENDDO

   ALLOCATE(M%SAVE1(-3:M%LSAVE), STAT= IERROR)
   CALL CHKMEMERR ('SCARC_RESTORE_ENVIRONMENT', 'M%SAVE1', IERROR)

   ALLOCATE(M%WORK(M%LWORK), STAT= IERROR)
   CALL CHKMEMERR ('SCARC_RESTORE_ENVIRONMENT', 'M%WORK', IERROR)

   READ(LU,*) 
   READ(LU,*) (M%SAVE1(I), I=-3, M%LSAVE)

   READ(LU,*) 
   READ(LU,*) (M%WORK(I), I=1, M%LWORK)

   CLOSE (LU)

ENDDO

END SUBROUTINE SCARC_RESTORE_ENVIRONMENT


! ----------------------------------------------------------------------------------------------------
!> \brief POSTPROCESSING version only: Allocate and initialize vectors pressure diagnostics (only for developping purposes)
! ----------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_SETUP_PRESSURE()
USE SCARC_POINTERS, ONLY: L, G, PR,SCARC_POINT_TO_GRID
INTEGER :: NM

CROUTINE = 'SCARC_SETUP_PRESSURE'

DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NLEVEL_MIN)
   PR => L%PRESSURE

   CALL SCARC_ALLOCATE_REAL1(PR%B_OLD, 1, G%NC, NSCARC_INIT_ZERO, 'PR%B_OLD', CROUTINE)
   CALL SCARC_ALLOCATE_REAL1(PR%B_NEW, 1, G%NC, NSCARC_INIT_ZERO, 'PR%B_NEW', CROUTINE)

   CALL SCARC_ALLOCATE_REAL3(PR%H_OLD, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'PR%H_OLD', CROUTINE)
   CALL SCARC_ALLOCATE_REAL3(PR%H_NEW, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'PR%H_NEW', CROUTINE)

   CALL SCARC_ALLOCATE_REAL3(PR%HS_OLD, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'PR%HS_OLD', CROUTINE)
   CALL SCARC_ALLOCATE_REAL3(PR%HS_NEW, 0, L%NX+1, 0, L%NY+1, 0, L%NZ+1, NSCARC_INIT_ZERO, 'PR%HS_NEW', CROUTINE)

   !CALL SCARC_ALLOCATE_REAL3(PR%PRHS, 1, L%NX+1, 1, L%NY+1, 1, L%NZ+1, NSCARC_INIT_ZERO, 'PR%PRHS', CROUTINE)

ENDDO

END SUBROUTINE SCARC_SETUP_PRESSURE


! ------------------------------------------------------------------------------------------------
!> \brief POSTPROCESSING version only: Compute Differences between old and new pressure solutions - only for developping purposes
! ------------------------------------------------------------------------------------------------
SUBROUTINE SCARC_PRESSURE_DIFFERENCE(NL)
USE SCARC_POINTERS, ONLY: L, PR, SCARC_POINT_TO_GRID
INTEGER, INTENT(IN) :: NL
INTEGER :: NM, IX, IY, IZ


! -----------------------------------------------------------------------------------------------
!> \brief POSTPROCESSING version only: Store new pressure vector for comparison in next time step
! -----------------------------------------------------------------------------------------------
DO NM = LOWER_MESH_INDEX, UPPER_MESH_INDEX

   CALL SCARC_POINT_TO_GRID (NM, NL)     
   PR => L%PRESSURE

   IF (PREDICTOR) THEN
   
      PR%H_NEW  = M%H
      PR%DIFF_H = 0.0_EB
      DO IZ = 1, L%NZ
         DO IY = 1, L%NY
            DO IX = 1, L%NX
               PR%DIFF_H = PR%DIFF_H + (PR%H_NEW(IX, IY, IZ) - PR%H_OLD(IX, IY, IZ))**2         
            ENDDO
         ENDDO
      ENDDO
      PR%DIFF_H = PR%DIFF_H / REAL(L%N_CELLS, EB)

   ELSE

      PR%HS_NEW = M%HS
      PR%DIFF_HS = 0.0_EB
      DO IZ = 1, L%NZ
         DO IY = 1, L%NY
            DO IX = 1, L%NX
               PR%DIFF_HS = PR%DIFF_HS + (PR%HS_NEW(IX, IY, IZ) - PR%HS_OLD(IX, IY, IZ))**2         
            ENDDO
         ENDDO
      ENDDO
      PR%DIFF_HS = PR%DIFF_HS / REAL(L%N_CELLS, EB)

   ENDIF

ENDDO

#ifdef WITH_SCARC_DEBUG
WRITE(MSG%LU_DEBUG,*) 'Differences of pressure vectors on mesh ', NM,' : ', PR%DIFF_H, PR%DIFF_HS
#endif

END SUBROUTINE SCARC_PRESSURE_DIFFERENCE


END MODULE SCARC_POSTPROCESSING
#endif


