program Sudoku
  
  implicit none
  ! Defining the variables and stuff
  !
  integer :: n_elves,data,map,cals_tmp,lunit,elf_no,loop,n_snacks,dummy,snack_no,nlines
  integer :: counter,i,ios,s,max_elf,max_cals,temp1,temp2,j,points
  integer,allocatable :: cals(:),snacks_per_elf(:),cals_new(:,:)
  character*10 :: text,next
  !
  lunit = 55
  nlines = 2500
  points = 0
  !
!!!!!!  OPEN (lunit, file = 'data.dat')
!!!!!!  do loop = 1,nlines
!!!!!!     !
!!!!!!     write(*,*) "loop = ",loop
!!!!!!     read (lunit,"(a)", size=s, advance='no', iostat=ios) text(1:4)
!!!!!!     write(*,*) "text(1:3) = ",text(1:1),text(2:2),text(3:3)
!!!!!!     !
!!!!!!     if (text(3:3).eq."X") then
!!!!!!           points = points + 1
!!!!!!     elseif (text(3:3).eq."Y") then
!!!!!!           points = points + 2
!!!!!!     elseif (text(3:3).eq."Z") then
!!!!!!           points = points + 3
!!!!!!     endif
!!!!!!     !
!!!!!!     if (text(1:1).eq."A") then
!!!!!!        write(*,*) "opp rock "
!!!!!!        if (text(3:3).eq."X") then
!!!!!!           write(*,*) "me rock "
!!!!!!           points = points + 3
!!!!!!        elseif (text(3:3).eq."Y") then
!!!!!!           write(*,*) "me paper "
!!!!!!           points = points + 6
!!!!!!        elseif (text(3:3).eq."Z") then
!!!!!!           write(*,*) "me scizzors "
!!!!!!           points = points + 0
!!!!!!        endif
!!!!!!     elseif (text(1:1).eq."B") then
!!!!!!        write(*,*) "opp rock "
!!!!!!        if (text(3:3).eq."X") then
!!!!!!           write(*,*) "me rock "
!!!!!!           points = points + 0
!!!!!!        elseif (text(3:3).eq."Y") then
!!!!!!           write(*,*) "me paper "
!!!!!!           points = points + 3
!!!!!!        elseif (text(3:3).eq."Z") then
!!!!!!           write(*,*) "me scizzors "
!!!!!!           points = points + 6
!!!!!!        endif
!!!!!!     elseif (text(1:1).eq."C") then
!!!!!!        write(*,*) "opp rock "
!!!!!!        if (text(3:3).eq."X") then
!!!!!!           write(*,*) "me rock "
!!!!!!           points = points + 6
!!!!!!        elseif (text(3:3).eq."Y") then
!!!!!!           write(*,*) "me paper "
!!!!!!           points = points + 0
!!!!!!        elseif (text(3:3).eq."Z") then
!!!!!!           write(*,*) "me scizzors "
!!!!!!           points = points + 3
!!!!!!        endif
!!!!!!     endif
!!!!!!     !
!!!!!!     write(*,*) "points = ",points
!!!!!!     !pause
!!!!!!  enddo
!!!!!!  close(lunit)
!!!!!!  !pause
  !


  ! lose draw win
  OPEN (lunit, file = 'data.dat')
  do loop = 1,nlines
     !
     write(*,*) "loop = ",loop
     read (lunit,"(a)", size=s, advance='no', iostat=ios) text(1:4)
     write(*,*) "text(1:3) = ",text(1:1),text(2:2),text(3:3)
     !
     if (text(1:1).eq."A") then
        write(*,*) "opp rock "
        if (text(3:3).eq."X") then
           write(*,*) "me scizzors "
           points = points + 3
           points = points + 0
        elseif (text(3:3).eq."Y") then
           write(*,*) "me rock "
           points = points + 1
           points = points + 3
        elseif (text(3:3).eq."Z") then
           write(*,*) "me paper "
           points = points + 2
           points = points + 6
        endif
     elseif (text(1:1).eq."B") then
        write(*,*) "opp paper "
        if (text(3:3).eq."X") then
           write(*,*) "me rock"
           points = points + 1
           points = points + 0
        elseif (text(3:3).eq."Y") then
           write(*,*) "me paper "
           points = points + 2
           points = points + 3
        elseif (text(3:3).eq."Z") then
           write(*,*) "me scizzors "
           points = points + 3
           points = points + 6
        endif
     elseif (text(1:1).eq."C") then
        write(*,*) "opp scizzors "
        if (text(3:3).eq."X") then
           write(*,*) "me paper"
           points = points + 2
           points = points + 0
        elseif (text(3:3).eq."Y") then
           write(*,*) "me scizzors"
           points = points + 3
           points = points + 3
        elseif (text(3:3).eq."Z") then
           write(*,*) "me rock"
           points = points + 1
           points = points + 6
        endif
     endif
     !
     write(*,*) "points = ",points
     !pause
  enddo
  close(lunit)
  !pause
  !




end program Sudoku 
