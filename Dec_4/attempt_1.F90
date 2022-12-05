program Sudoku
  
  implicit none
  ! Defining the variables and stuff
  !
  integer :: n_elves,data,map,cals_tmp,lunit,elf_no,loop,n_snacks,snack_no,nlines
  integer :: counter,counter1,counter2,i,size1,size2,size3,n,ios,s,size
  integer :: set1(2),set2(2),loc(2),container
  character*100 :: text,text1,text2,text3,set,dummy
  logical  :: repeat,after
  !
  lunit = 55
  nlines = 1000



  OPEN (lunit, file = 'data.dat')
  container = 0
  do loop = 1,nlines
     !
     write(*,*) "loop = ",loop
     read (lunit,"(a)", size=s, advance='no', iostat=ios) text
     size = s
     write(*,*) "text = ",text
     !
     text1 = ""
     text2 = ""
     counter1 = 0
     counter2 = 0
     after = .false.
     do i = 1,size
        if (text(i:i).eq.",") then
           after = .true.
        elseif (.not.after) then
           counter1 = counter1+ 1
           text1(i:i) = text(i:i)
           if (text(i:i).eq."-") then
              loc(1) = counter1
           endif
           !
        elseif (after) then
           counter2 = counter2+ 1
           write(*,*) "i,counter1,text(i:i) = ",i,counter1,text(i:i)
           text2(i-counter1-1:i-counter1-1) = text(i:i)
           if (text(i:i).eq."-") then
              loc(2) = counter2
           endif
           !
        endif
        !
     enddo
     !
     write(*,*) "text1 = ",text1
     write(*,*) "counter1 = ",counter1
     write(*,*) "text2 = ",text2
     write(*,*) "counter2 = ",counter2
     !
     read(text1(1       :loc(1)-1),'(i2)') set1(1)
     read(text1(loc(1)+1:counter1),'(i2)') set1(2)
     read(text2(1       :loc(2)-1),'(i2)') set2(1)
     read(text2(loc(2)+1:counter2),'(i2)') set2(2)

     write(*,*) "set1 = ",set1
     write(*,*) "set2 = ",set2


     if ((set1(1).ge.set2(1)).and.(set1(1).le.set2(2))) then
        container = container + 1
     elseif ((set1(2).ge.set2(1)).and.(set1(2).le.set2(2))) then
        container = container + 1
     elseif ((set2(1).ge.set1(1)).and.(set2(1).le.set1(2))) then
        container = container + 1
     elseif ((set2(2).ge.set1(1)).and.(set2(2).le.set1(2))) then
        container = container + 1
     endif
     write(*,*) "container = ",container

     !pause
     !
  enddo


  close(lunit)
  pause
  !

end program Sudoku 
