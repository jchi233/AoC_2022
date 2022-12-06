program Sudoku
  
  implicit none
  ! Defining the variables and stuff
  !
  integer :: n_elves,data,map,cals_tmp,lunit,elf_no,loop,n_snacks,snack_no,nlines
  integer :: counter,counter1,counter2,i,size1,size2,size3,n,ios,s,size
  integer :: pos,stack_counter(9),mv(3),dummy2,fuck_you,j,k
  character*100 :: text1,text2,text3,set,dummy
  character*5000 :: text
  logical  :: fake_bool
  !
  lunit = 55
  nlines = 501
  !
  !
  OPEN (lunit, file = 'data.dat')
  !do loop = 1,nlines
     !
     !write(*,*) "loop = ",loop
     read (lunit,"(a)", size=s, advance='no', iostat=ios) text
     size = s
     write(*,*) "size = ",size
     write(*,*) "text = ",text
     pause
     !
     ! part 1
!!!     counter = 4
!!!     do i = 1,size-4
!!!        write(*,*) "i,text(i) = ",i,text(i:i+3)
!!!        dummy(1:1) = text(i:i)
!!!        dummy(2:2) = text(i+1:i+1)
!!!        dummy(3:3) = text(i+2:i+2)
!!!        dummy(4:4) = text(i+3:i+3)
!!!        !
!!!        if(text(i:i).eq.text(i+1:i+1)) then
!!!           counter = counter + 1
!!!        elseif(text(i:i).eq.text(i+2:i+2)) then
!!!           counter = counter + 1
!!!        elseif(text(i:i).eq.text(i+3:i+3)) then
!!!           counter = counter + 1
!!!        elseif(text(i+1:i+1).eq.text(i+2:i+2)) then
!!!           counter = counter + 1
!!!        elseif(text(i+1:i+1).eq.text(i+3:i+3)) then
!!!           counter = counter + 1
!!!        elseif(text(i+2:i+2).eq.text(i+3:i+3)) then
!!!           counter = counter + 1
!!!        else
!!!           counter = counter + 1
!!!           write(*,*) "counter finished = ",counter
!!!           write(*,*) "string = ",dummy(1:4)
!!!           pause
!!!        endif
!!!        !pause
!!!     enddo
     !
     !part 2
     counter = 14
     do i = 1,size-14
        write(*,*) "i,text(i) = ",i,text(i:i+3)
        dummy(1:1) = text(i:i)
        dummy(2:2) = text(i+1:i+1)
        dummy(3:3) = text(i+2:i+2)
        dummy(4:4) = text(i+3:i+3)
        dummy(5:5) = text(i+4:i+4)
        dummy(6:6) = text(i+5:i+5)
        dummy(7:7) = text(i+6:i+6)
        dummy(8:8) = text(i+7:i+7)
        dummy(9:9) = text(i+8:i+8)
        dummy(10:10) = text(i+9:i+9)
        dummy(11:11) = text(i+10:i+10)
        dummy(12:12) = text(i+11:i+11)
        dummy(13:13) = text(i+12:i+12)
        dummy(14:14) = text(i+13:i+13)
        !

        fake_bool = .false.
        do j = 1,13
           do k = j+1,14
              if(dummy(j:j).eq.dummy(k:k)) then
                 fake_bool = .true.
              endif
           enddo
        enddo
        !
        if (fake_bool) then
           counter = counter + 1
        else
           write(*,*) "counter finished = ",counter
           write(*,*) "string = ",dummy(1:14)
           pause
        endif
        !pause
     enddo
     !
  !enddo
  pause



!!!!     !
!!!!     read(motion(2,2),'(i4)') fuck_you
!!!!     read(motion(2,1),'(i4)') mv(1)
!!!!     read(motion(4,1:10),'(i4)') mv(2)
!!!!     read(motion(6,1:10),'(i4)') mv(3)
!!!!     !
!!!!     if (motion(2,2).ne."") then
!!!!         mv(1) =  mv(1)*10 + fuck_you
!!!!     endif
!!!!
!!!!     write(*,*) "mv(1:3) = ",mv(1:3),fuck_you
!!!!
!!!!     write(*,*) "before"
!!!!     write(*,*) "stack_counter(:) = ",stack_counter(:)
!!!!     write(*,*) "stack1 = ",stacks(mv(3),:)
!!!!     write(*,*) "stack2 = ",stacks(mv(2),:)
!!!!     !
!!!!     ! PART 1
!!!!     !do i = 1,mv(1)
!!!!     !   stack_counter(mv(3)) = stack_counter(mv(3)) + 1
!!!!     !   stacks(mv(3),stack_counter(mv(3))) = stacks(mv(2),stack_counter(mv(2)))
!!!!     !   stacks(mv(2),stack_counter(mv(2))) = ""
!!!!     !   stack_counter(mv(2)) = stack_counter(mv(2)) - 1
!!!!     !enddo
!!!!     !
!!!!     ! PART 2
!!!!     do i = 1,mv(1)
!!!!        dum(mv(1)+1-i) = stacks(mv(2),stack_counter(mv(2)))
!!!!        stacks(mv(2),stack_counter(mv(2))) = ""
!!!!        stack_counter(mv(2)) = stack_counter(mv(2)) - 1
!!!!     enddo
!!!!
!!!!     do i = 1,mv(1)
!!!!        stack_counter(mv(3)) = stack_counter(mv(3)) + 1
!!!!        stacks(mv(3),stack_counter(mv(3))) = dum(i)
!!!!     enddo
!!!!     !
!!!!     write(*,*) "after"
!!!!     write(*,*) "stack_counter(:) = ",stack_counter(:)
!!!!     write(*,*) "stack1 = ",stacks(mv(3),:)
!!!!     write(*,*) "stack2 = ",stacks(mv(2),:)
!!!!     !pause
!!!!     !
!!!!  enddo
!!!!
!!!!  do i = 1,9
!!!!     write(*,*) i,stacks(i,:)
!!!!  enddo
!!!!  !
!!!!  close(lunit)
!!!!  !
!!!!  pause
!!!!  !

end program Sudoku 
