! read 2 matrices form an input file
! multiply them and write the results to
! an output file
program MatMult90
    implicit none

    integer::ioin   = 10        ! input channel
    integer::ioout  = 11        ! output channel

    integer::ioerr              ! error handler for io
    integer::memerr             ! error handler for memory allocation

    ! static array
    integer,dimension(2)::nDimA ! dimension of matrix A
    integer,dimension(2)::nDimB ! dimension of matrix B
    integer,dimension(2)::nDimC ! dimension of matrix C

    ! dynamical arrays
    real(8), allocatable, dimension(:,:)::A
    real(8), allocatable, dimension(:,:)::B
    real(8), allocatable, dimension(:,:)::C

    ! functions declares
    integer::ireadmatdim, ireadmat, imatmult

    ! filename strings
    character(256)::infile
    character(256)::outfile

    ! iniatialize
    infile  = "matmult90.in"
    outfile = "matmult90.out"

    ! read the command options
    ! syntax: matmult90.exe [input file] [output file]
    if (iargc() > 0)    call getarg(1,infile)
    if (iargc() > 1)    call getarg(2,outfile)

    ! open the inputfile
    open(ioin,file=infile,status='old',iostat=ioerr)
    if (ioerr /= 0) then
        write(*,*)'error: file ',infile(1:len_trim(infile)),' not found!'
        stop
    endif

    ! open the output file
    open(ioout,file=outfile,status='replace',iostat=ioerr)
    if (ioerr /= 0) then
        write(*,*)'error: file ',outfile(1:len_trim(outfile)),' not found!'
        stop
    endif

    ! read Matrix A
    if (ireadmatdim(ioin,nDimA) /= 0) then
        write(*,*)'error: no dimension for matrix A!'
        stop
    endif

    ! allocate the memory for matrix A
    allocate(A(nDimA(1),nDimA(2)),stat=memerr)
    if (memerr /= 0) then
        write(*,*)'allocation error for matrix A!'
        stop
    endif

    ! read matrix A
    if (ireadmat(ioin,A,nDimA) /= 0) then
        write(*,*)'error reading matrix A'
        stop
    endif

    ! print the matrixdata of A
    call listmat(ioout,"matrix A",A,nDimA)

    ! read Matrix B
    if (ireadmatdim(ioin,nDimB) /= 0) then
        write(*,*)'error: no dimension for matrix B!'
        stop
    endif

    ! allocate the memory for matrix B
    allocate(B(nDimB(1),nDimB(2)),stat=memerr)
    if (memerr /= 0) then
        write(*,*)'allocation error for matrix B!'
        stop
    endif

    ! read matrix B
    if (ireadmat(ioin,B,nDimB) /= 0) then
        write(*,*)'error reading matrix B'
        stop
    endif

    ! print the matrixdata of B
    call listmat(ioout,"matrix B",B,nDimB)

    ! check the dimension
    if (nDimA(2) /= nDimB(1)) then
        write(*,*)'error: wrong matrix dimension!'
        stop
    endif

    ! allocate the memory for matrix C
    allocate(C(nDimA(1),nDimB(2)),stat=memerr)
    if (memerr /= 0) then
        write(*,*)'allocation error for matrix C!'
        stop
    endif

    ! multiply the matrices
    if (imatmult(A,B,C,nDimA,nDimB) /= 0) then
        write(*,*)'dimension error, no product available!'
        stop
    endif

    ! print the matrixdata of C
    nDimC(1) = nDimA(1)
    nDimC(2) = nDimB(2)
    call listmat(ioout,"matrix C",C,nDimC)

    ! deallocate the memory
    deallocate(A,stat=memerr)
    deallocate(B,stat=memerr)
    deallocate(C,stat=memerr)

    ! close the files
    close(ioin)
    close(ioout)

end program

