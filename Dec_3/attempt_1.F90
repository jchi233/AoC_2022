program Sudoku
  
  implicit none
  ! Defining the variables and stuff
  !
  integer :: n_elves,data,map,cals_tmp,lunit,elf_no,loop,n_snacks,snack_no,nlines
  integer :: counter,i,ios,s,max_elf,max_cals,temp1,temp2,j,points,k,linesize,size1,size2,size3,n
  character*100 :: text,text1,text2,text3,set,dummy
  logical  :: repeat
  !
  lunit = 55
  nlines = 300
  points = 0
  set = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  !
!!!!  OPEN (lunit, file = 'data.dat')
!!!!  do loop = 1,nlines
!!!!     !
!!!!     write(*,*) "loop = ",loop
!!!!     !read (lunit,"(a)", size=s, advance='no', iostat=ios) dummy(1:4)
!!!!     !backspace(lunit)
!!!!     !linesize = s
!!!!     read (lunit,"(a)", size=s, advance='no', iostat=ios) text
!!!!     !
!!!!     write(*,*) "s = ",s
!!!!     write(*,*) "text = ",text
!!!!     !split text into halves
!!!!     do i = 1,s/2
!!!!        write(*,*) "i,text(i:i) = ",text(i:i)
!!!!        text1(i:i) = text(i:i)
!!!!     enddo
!!!!     !
!!!!     do i = 1,s/2
!!!!        write(*,*) "i,text(i:i) = ",text(i+s/2:i+s/2)
!!!!        text2(i:i) = text(i+s/2:i+s/2)
!!!!     enddo
!!!!     !
!!!!     repeat = .false.
!!!!     do i = 1,s/2
!!!!        do j = 1,s/2
!!!!           if (text1(i:i).eq.text2(j:j)) then
!!!!              do k = 1,52
!!!!                 if (text1(i:i).eq.set(k:k)) then
!!!!                    if (.not.repeat) then
!!!!                       write(*,*) "k,set(kk) = ",k,set(k:k)
!!!!                       points = points + k
!!!!                       repeat = .true.
!!!!                    endif
!!!!                 end if
!!!!              enddo
!!!!           endif
!!!!        enddo
!!!!     enddo
!!!!     !
!!!!     write(*,*) "points = ",points
!!!!     !pause
!!!!     !
!!!!  enddo
!!!!
!!!!
!!!!  close(lunit)
!!!!  !pause
!!!!  !



  OPEN (lunit, file = 'data.dat')
  do loop = 1,nlines/3
     !
     write(*,*) "loop = ",loop
     !read (lunit,"(a)", size=s, advance='no', iostat=ios) dummy(1:4)
     !backspace(lunit)
     !linesize = s
     read (lunit,"(a)", size=s, advance='no', iostat=ios) text1
     size1 = s
     read (lunit,"(a)", size=s, advance='no', iostat=ios) text2
     size2 = s
     read (lunit,"(a)", size=s, advance='no', iostat=ios) text3
     size3 = s
     !
     write(*,*) "text1 = ",text1
     write(*,*) "text2 = ",text2
     write(*,*) "text3 = ",text3
     !
     !
     repeat = .false.
     do i = 1,size1
        do j = 1,size2
           if (text1(i:i).eq.text2(j:j)) then
              do n = 1,size3
                 if (text1(i:i).eq.text3(n:n)) then
                    do k = 1,52
                       if (text1(i:i).eq.set(k:k)) then
                          if (.not.repeat) then
                             write(*,*) "k,set(kk) = ",k,set(k:k)
                             points = points + k
                             repeat = .true.
                          endif
                       end if
                    enddo
                 endif
              enddo
           endif
        enddo
     enddo
     !
     write(*,*) "points = ",points
     !pause
     !
  enddo


  close(lunit)
  pause
  !

end program Sudoku 
