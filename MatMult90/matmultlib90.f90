! f90 library for our matmult project
!
integer function ireadmatdim(io,nDim)

    integer::io                 ! input channel
    integer,dimension(2)::nDim  ! array dimension

!   read(io,'(i5,i5)')  column based input (not user friendly)

!   free format: item separated by white spaces
    read(io,*,iostat=ioerr) nDim(1),nDim(2)
    if (ioerr /= 0) then
        ireadmatdim = 1
        return
    endif

    ireadmatdim = 0

end function ireadmatdim

! read the matrix data from a text file
!
integer function ireadmat(io,mat,ndim)

    integer::io                             ! io channel
    integer,dimension(2)::ndim
    real(8),dimension(ndim(1),ndim(2))::mat ! matrix buffer

!   read(io,*,iostat=ioerr) ((mat(i,j),j=1,ndim(2)),i=1,nidm(1))

    ! read the values of the lines
    do i=1,ndim(1)
        read(io,*,iostat=ioerr) (mat(i,j),j=1,ndim(2))
        if (ioerr /= 0) then
            write (*,'(a,i2)') 'error: reading matrix data in line ',i
            ireadmat = 1
            return
        endif
    end do

    ireadmat = 0
end function ireadmat

! subprogram to print the matrix data to the screen
subroutine listmat(io,title,mat,ndim)

    character*(*) title

    integer::io
    integer,dimension(2)::ndim
    real(8),dimension(ndim(1),ndim(2))::mat ! matrix buffer

    character*(256) ofmt
    write(ofmt,'(a,i4,a)') '(',ndim(2),'(1x,f8.2))'

    ! write(*,*) 'ofmt: ',ofmt(1:len_trim(ofmt))

    write(io,*) title
    do i=1,ndim(1)
        write(io,ofmt) (mat(i,j),j=1,ndim(2))
    end do

end subroutine listmat

! function to multiply 2 matrices
! c = a x b
integer function imatmult(a,b,c,nDimA,nDimB)

    integer, dimension(2)::nDimA,nDimB          ! dimension of matrix a and b
    real(8), dimension(nDimA(1),nDimA(2))::a    ! matrix a
    real(8), dimension(nDimB(1),nDimB(2))::b    ! matrix b
    real(8), dimension(nDimA(1),nDimB(2))::c    ! matrix c

    ! check the dimensions
    if (nDimA(2) /= nDimB(1)) then
        imatmult = 1
        return
    endif

    ! - over all rows of a (c)
    do i=1,nDimA(1)

        ! - over all columns of b (c)
        do j=1,nDimB(2)

            c(i,j) = 0.
            do k=1,nDimA(2)
                c(i,j) = c(i,j) + a(i,k)*b(k,j)
            enddo
        enddo
    enddo
    imatmult = 0

end function

