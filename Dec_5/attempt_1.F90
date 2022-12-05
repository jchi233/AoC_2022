program Sudoku
  
  implicit none
  ! Defining the variables and stuff
  !
  integer :: n_elves,data,map,cals_tmp,lunit,elf_no,loop,n_snacks,snack_no,nlines
  integer :: counter,counter1,counter2,i,size1,size2,size3,n,ios,s,size
  integer :: pos,stack_counter(9),mv(3),dummy2,fuck_you
  character*100 :: text,text1,text2,text3,set
  character  :: stacks(9,1000),motion(6,100),dummy(100),dum(100)
  logical  :: repeat,after
  !
  lunit = 55
  nlines = 501
  !
  stacks = ""
  !
  ! column, row
  stacks(1,1) = "W"
  stacks(1,2) = "M"
  stacks(1,3) = "L"
  stacks(1,4) = "F"
  stack_counter(1)  = 4
  stacks(2,1) = "B"
  stacks(2,2) = "Z"
  stacks(2,3) = "V"
  stacks(2,4) = "M"
  stacks(2,5) = "F"
  stack_counter(2)  = 5
  stacks(3,1) = "H"
  stacks(3,2) = "V"
  stacks(3,3) = "R"
  stacks(3,4) = "S"
  stacks(3,5) = "L"
  stacks(3,6) = "Q"
  stack_counter(3)  = 6
  stacks(4,1) = "F"
  stacks(4,2) = "S"
  stacks(4,3) = "V"
  stacks(4,4) = "Q"
  stacks(4,5) = "P"
  stacks(4,6) = "M"
  stacks(4,7) = "T"
  stacks(4,8) = "J"
  stack_counter(4)  = 8
  stacks(5,1) = "L"
  stacks(5,2) = "S"
  stacks(5,3) = "W"
  stack_counter(5)  = 3
  stacks(6,1) = "F"
  stacks(6,2) = "V"
  stacks(6,3) = "P"
  stacks(6,4) = "M"
  stacks(6,5) = "R"
  stacks(6,6) = "J"
  stacks(6,7) = "W"
  stack_counter(6)  = 7
  stacks(7,1) = "J"
  stacks(7,2) = "Q"
  stacks(7,3) = "C"
  stacks(7,4) = "P"
  stacks(7,5) = "N"
  stacks(7,6) = "R"
  stacks(7,7) = "F"
  stack_counter(7)  = 7
  stacks(8,1) = "V"
  stacks(8,2) = "H"
  stacks(8,3) = "P"
  stacks(8,4) = "S"
  stacks(8,5) = "Z"
  stacks(8,6) = "W"
  stacks(8,7) = "R"
  stacks(8,8) = "B"
  stack_counter(8)  = 8
  stacks(9,1) = "B"
  stacks(9,2) = "M"
  stacks(9,3) = "J"
  stacks(9,4) = "C"
  stacks(9,5) = "G"
  stacks(9,6) = "H"
  stacks(9,7) = "Z"
  stacks(9,8) = "W"
  stack_counter(9)  = 8
  !
  !
  OPEN (lunit, file = 'data.dat')
  do loop = 1,nlines
     !
     write(*,*) "loop = ",loop
     read (lunit,"(a)", size=s, advance='no', iostat=ios) text
     size = s
     write(*,*) "text = ",text
     !
     motion = ""
     counter = 1
     do i = 1,size
        write(*,*) "i,text(i) = ",i,text(i:i)
        if(text(i:i).ne." ") then
           pos = pos + 1
           motion(counter,pos:pos) = text(i:i)
        else
           counter = counter + 1
           pos = 0
        endif
     enddo
     !
     write(*,*) "motion(1) = ",motion(1,:)
     write(*,*) "motion(2) = ",motion(2,:)
     write(*,*) "motion(3) = ",motion(3,:)
     write(*,*) "motion(4) = ",motion(4,:)
     write(*,*) "motion(5) = ",motion(5,:)
     write(*,*) "motion(6) = ",motion(6,:)
     !
     read(motion(2,2),'(i4)') fuck_you
     read(motion(2,1),'(i4)') mv(1)
     read(motion(4,1:10),'(i4)') mv(2)
     read(motion(6,1:10),'(i4)') mv(3)
     !
     if (motion(2,2).ne."") then
         mv(1) =  mv(1)*10 + fuck_you
     endif

     write(*,*) "mv(1:3) = ",mv(1:3),fuck_you

     write(*,*) "before"
     write(*,*) "stack_counter(:) = ",stack_counter(:)
     write(*,*) "stack1 = ",stacks(mv(3),:)
     write(*,*) "stack2 = ",stacks(mv(2),:)
     !
     ! PART 1
     !do i = 1,mv(1)
     !   stack_counter(mv(3)) = stack_counter(mv(3)) + 1
     !   stacks(mv(3),stack_counter(mv(3))) = stacks(mv(2),stack_counter(mv(2)))
     !   stacks(mv(2),stack_counter(mv(2))) = ""
     !   stack_counter(mv(2)) = stack_counter(mv(2)) - 1
     !enddo
     !
     ! PART 2
     do i = 1,mv(1)
        dum(mv(1)+1-i) = stacks(mv(2),stack_counter(mv(2)))
        stacks(mv(2),stack_counter(mv(2))) = ""
        stack_counter(mv(2)) = stack_counter(mv(2)) - 1
     enddo

     do i = 1,mv(1)
        stack_counter(mv(3)) = stack_counter(mv(3)) + 1
        stacks(mv(3),stack_counter(mv(3))) = dum(i)
     enddo
     !
     write(*,*) "after"
     write(*,*) "stack_counter(:) = ",stack_counter(:)
     write(*,*) "stack1 = ",stacks(mv(3),:)
     write(*,*) "stack2 = ",stacks(mv(2),:)
     !pause
     !
  enddo

  do i = 1,9
     write(*,*) i,stacks(i,:)
  enddo
  !
  close(lunit)
  !
  pause
  !

end program Sudoku 
