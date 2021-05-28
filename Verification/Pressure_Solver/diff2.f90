program differences

   implicit none
   integer, parameter :: eb = SELECTED_REAL_KIND(12)

   integer :: i,k, isthere
   integer :: nx, nz
   integer :: max_diff1_i, max_diff1_k
   integer :: max_diff2_i, max_diff2_k
   integer :: max_diff3_i, max_diff3_k

   real(eb) :: sep(0:100,1,0:100)
   real(eb) :: insep(0:100,1,0:100)
   real(eb) :: insep2(0:100,1,0:100)
   real(eb) :: diff(0:100,1,0:100)
   real(eb) :: diff2(0:100,1,0:100)
   real(eb) :: diff3(0:100,1,0:100)
   real(eb) :: scal1(0:100,1,0:100)
   real(eb) :: scal2(0:100,1,0:100)
   real(eb) :: insep_add(0:100,1,0:100)
   real(eb) :: max_diff1, max_diff2, max_diff3, rho1, rho2

   character(40) :: cname, cform

   ! read number of cells in x and y
   open (20, file = 'diff2.dat')
   read (20,*) nx, nz
   write(*,*) 'nx=',nx, ' nz=',nz

   if (nx <10) then
      cform = ' E24.16'
      write(cform(1:1),'(i1)')  nx
   else
      cform = '  E24.16'
      write(cform(1:2),'(i2.2)')  nx
   endif
   write(*,*) 'cform=',cform

   ! read sep
   read (20,*) cname
   write (*,*) 'sep=',cname
   do k=nz+1,0,-1
      read(20,'(10E24.16)') (sep(i,1,k),i=0,nx+1)
   enddo

   ! read insep 
   read (20,*) cname
   write (*,*) 'insep=',cname
   do k=nz+1,0,-1
      read(20,'(10E24.16)') (insep(i,1,k),i=0,nx+1)
   enddo

   ! read insep2 
   read (20,*) cname, isthere
   if (isthere == 1) then
      write (*,*) 'insep2=',cname
      do k=nz+1,0,-1
         read(20,'(10E24.16)') (insep2(i,1,k),i=0,nx+1)
      enddo
   endif

   max_diff1 = -9999999.0_EB
   max_diff2 = -9999999.0_EB
   max_diff3 = -9999999.0_EB

   rho1 = 0.1195253017817356E+01
   rho2 = 1.0_EB/rho1

   do k=nz+1,0,-1
      do i=0,nx+1
         insep_add(i,1,k) = insep(i,1,k)+insep2(i,1,k)
         diff(i,1,k) = abs(sep(i,1,k)-insep(i,1,k))
         diff3(i,1,k) = abs(sep(i,1,k)-insep_add(i,1,k))
         scal1(i,1,k) = diff(i,1,k)*rho1
         scal2(i,1,k) = diff(i,1,k)*rho2
         if (diff(i,1,k) > max_diff1) then
            max_diff1 = diff(i,1,k)
            max_diff1_i = i
            max_diff1_k = k
         endif
         if (diff3(i,1,k) > max_diff3) then
            max_diff3 = diff3(i,1,k)
            max_diff3_i = i
            max_diff3_k = k
         endif
      enddo
   enddo

   do k=nz,1,-1
      do i=1,nx
         diff2(i,1,k) = abs(sep(i,1,k)-insep(i,1,k))
         if (diff2(i,1,k) > max_diff2) then
            max_diff2 = diff2(i,1,k)
            max_diff2_i = i
            max_diff2_k = k
         endif
      enddo
   enddo
   
   write (*,*) 'insep_add'
   do k=nz+1,0,-1
      write(*,'(10E24.16)') (insep_add(i,1,k),i=0,nx+1)
   enddo
   write (*,*) 
   write (*,*) 'diff'
   do k=nz+1,0,-1
      write(*,'(10E24.16)') (diff(i,1,k),i=0,nx+1)
   enddo
   write (*,'(a, e14.6, a, i4, a, i4)') 'max_diff1 = ', max_diff1, ' , max_i1 =', max_diff1_i, ' ,  max_k1 =', max_diff1_k
   write (*,'(a, e14.6, a, i4, a, i4)') 'max_diff2 = ', max_diff2, ' , max_i2 =', max_diff2_i, ' ,  max_k2 =', max_diff2_k
   write (*,'(a, e14.6, a, i4, a, i4)') 'max_diff3 = ', max_diff3, ' , max_i3 =', max_diff3_i, ' ,  max_k3 =', max_diff3_k

   !write (*,*) 'scal1'
   !do k=nz+1,0,-1
   !   write(*,'(10E24.16)') (scal1(i,1,k),i=0,nx+1)
   !enddo
!
!   write (*,*) 'scal2'
!   do k=nz+1,0,-1
!      write(*,'(10E24.16)') (scal2(i,1,k),i=0,nx+1)
!   enddo

end program differences
