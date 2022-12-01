program Sudoku
  
  implicit none
  ! Defining the variables and stuff
  !
  integer :: n_elves,data,map,cals_tmp,lunit,elf_no,loop,n_snacks,dummy,snack_no,nlines
  integer :: counter,i,ios,s,max_elf,max_cals,temp1,temp2,j
  integer,allocatable :: cals(:),snacks_per_elf(:),cals_new(:,:)
  character*10 :: text,text1(10),next
  !
  next = ""
  !
  lunit = 55
  nlines = 0
!!!!  OPEN (lunit, file = 'data.dat')
!!!!  DO
!!!!     read (lunit,"(a)", size=s, advance='no', iostat=ios) text
!!!!     !READ (lunit,*) dummy
!!!!     nlines = nlines + 1
!!!!  END DO
!!!!  close(lunit)
  nlines = 2257

  OPEN (lunit, file = 'data.dat')
  n_elves = 0
  do loop = 1,nlines
     !
     write(*,*) "loop = ",loop
     read (lunit,"(a)", size=s, advance='no', iostat=ios) text
     write(*,*) "s = ",s
     !
     if ((text.eq.next).or.(loop.eq.nlines)) then
        n_elves = n_elves+1
        write(*,*) "n_elves = ",n_elves
     endif
     !
  enddo
  close(lunit)
  pause
  !
  allocate(snacks_per_elf(1:n_elves))
  snacks_per_elf = 0
  !
  elf_no = 1
  !
  OPEN (lunit, file = 'data.dat')
  n_snacks = 0
  do loop = 1,nlines
     !
     read (lunit,"(a)", size=s, advance='no', iostat=ios) text
     write(*,*) "text = ",text
     !if ((text.eq.next).or.(loop.eq.nlines)) then
     if ((text.eq.next)) then
        write(*,*) "elf_no",elf_no
        write(*,*) "n_snacks = ",n_snacks
        snacks_per_elf(elf_no) = n_snacks
        n_snacks = 0
        elf_no = elf_no + 1
     else
        n_snacks = n_snacks + 1
     endif
     !pause
     !
  enddo
  close(lunit)
  !
  pause
  !
  allocate(cals(1:n_elves))
  cals = 0
  !
  OPEN (lunit, file = 'data.dat')
  !
  do elf_no = 1,n_elves
     !
     write(*,*) "elf_no,snacks_per_elf = ",elf_no,snacks_per_elf(elf_no)
     !pause
     do snack_no = 1,snacks_per_elf(elf_no)
        !read (lunit,"(a)", size=s, advance='no', iostat=ios) text
        read (lunit,*), cals_tmp
        cals(elf_no) = cals(elf_no) + cals_tmp
        write(*,*) "totalcals,cals = ",cals(elf_no),cals_tmp
     enddo
     !
     if (elf_no.lt.n_elves) then
        read (lunit,"(a)", size=s, advance='no', iostat=ios) text
        !read (lunit,*), cals_tmp
     endif
     !
  enddo
  !
  max_cals = -100
  !
  do elf_no = 1,n_elves
     !
     if (cals(elf_no).gt.max_cals) then
        max_elf = elf_no
        max_cals = cals(elf_no)
        write(*,*) "max_elf,max_cals = ",max_elf,max_cals
     endif
     !
  enddo

  allocate(cals_new(1:n_elves,2))
  cals_new = 0
  !
  do elf_no = 1,n_elves
     !
     cals_new(elf_no,1) = cals(elf_no)
     cals_new(elf_no,2) = elf_no
     !
  enddo

  do i=1, n_elves
     do j=n_elves, i+1, -1
        if (cals_new(j-1,1)>cals_new(j,1)) then
           !
           temp1=cals_new(j-1,1)
           cals_new(j-1,1)=cals_new(j,1)
           cals_new(j,1)=temp1
           !
           temp2=cals_new(j-1,2)
           cals_new(j-1,2)=cals_new(j,2)
           cals_new(j,2)=temp2
           !
        endif
     enddo
  enddo

  do elf_no = 1,n_elves
     write(*,*) cals_new(elf_no,1:2)
  enddo

end program Sudoku 
