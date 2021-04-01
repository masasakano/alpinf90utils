module m_handle_argv
  use alpin_err_exit    ! for err_exit_with_msg() etc
  use alpin_misc_utils  ! for basename() etc
  use alpin_hash        ! for type(t_alpin_hash_char) etc.
  implicit none
  integer, parameter :: DIM_FLAGS = 1

contains
  ! Handle command-line arguments
  !
  ! Returns
  !
  !  1. basename of the command
  !  2. ARGUMENTS 1-*
  !  3. Index where the main arguments starts
  !  4. Array of Flags
  !
  subroutine handle_argv(basecomname, istart_main, allargv, flags)
    character(len=*), parameter :: Subname = 'handle_argv'
    character(len=1024), intent(out) :: basecomname
    integer, intent(out) :: istart_main
    type(t_alpin_hash_char),    dimension(:), allocatable, intent(out) :: allargv
    type(t_alpin_hash_logical), dimension(DIM_FLAGS),      intent(out) :: flags

    integer :: i, j, n_mainargs, nargs, status, i_verbose
    character(len=1024) :: arg, str_env = ''

    flags(1) = t_alpin_hash_logical(key='verbose', val=.false.)
    i_verbose = findloc1(flags%key, 'verbose')  ! => 1

    ! Check out the environmental variable: VERBOSE
    call GET_ENVIRONMENT_VARIABLE('VERBOSE', str_env, STATUS=status)
    if ((status == 1) .or. (status == 2)) then  ! 1 for non-existent, 2 for environment var. not-supported by the system
      flags(i_verbose)%val = .false.
    else if ((trim(str_env) == 'false') .or. (trim(str_env) == 'no')) then
      flags(i_verbose)%val = .false.
    else
      flags(i_verbose)%val = .true.
    end if

    nargs = command_argument_count()  ! F2003 standard  (_excluding_ $0)
    allocate(allargv(nargs))

    call get_command_argument(0, arg)  ! Fortran 2003;  i=0 for $0
    basecomname = trim(basename(trim(arg)))

    istart_main = -1  ! Index where the main arugment starts.
    do i=1, nargs
      call get_command_argument(i, arg)  ! Fortran 2003

      if ((trim(arg) == '-h') .or. (trim(arg) == '--help')) then
        print *, 'USAGE: '//trim(basecomname)//' [-h] [-v|--verbose] [DUMMY]'
        print *, ' -v: Verbose flag (can be specified with VERBOSE environmental variable)'
        call EXIT(0)
      end if

      allargv(i) = t_alpin_hash_char(key='arg'//trim(ladjusted_int(i)), val=trim(arg))

      if ((trim(arg) == '-v') .or. (trim(arg) == '--verbose')) then
        ! Verbose flag is set.
        flags(i_verbose)%val = .true.
        cycle
      end if

      if (istart_main < 0) istart_main = i
      j = i-istart_main+1
      select case(j)
      case(1)
        ! Accepted one. Modify the key.
        allargv(i)%key = 'main'//trim(ladjusted_int(j))
      case default
        ! Unexpected ones. Modify the key.
        allargv(i)%key = 'extra'//trim(ladjusted_int(j))
      end select
    end do

    ! Calculates the number of the main arguments, excluding the optional agurments.
    if (istart_main == -1) then
      n_mainargs = 0
    else
      n_mainargs = size(allargv) - istart_main + 1
    end if

    ! The number of the main arguments must be either 0 or 1.
    if (n_mainargs > 1) then
      call dump_all_argv(basecomname, allargv)
      call err_exit_with_msg( &
       'The number of the main arguments must be either 0 or 1, but given '//trim(ladjusted_int(n_mainargs))//'.')
    end if
  end subroutine handle_argv

  ! dump all_argv
  subroutine dump_all_argv(basecomname, rows, nargv)
    character(len=*), parameter :: Subname = 'dump_all_argv'
    character(len=*), intent(in) :: basecomname
    type(t_alpin_hash_char), dimension(:), intent(in) :: rows
    integer, intent(in), optional :: nargv  ! Optionally specify the size of the Array if it is not size(rows).
    integer :: i, nsize_rows

    nsize_rows = size(rows)
    if (present(nargv)) then
      if (nsize_rows < nargv) then
        call err_exit_with_msg('('//Subname//') given nargv=('//trim(ladjusted_int(nargv))//') > '//trim(ladjusted_int(nsize_rows)))
      end if
      nsize_rows = nargv
    else
    end if
    
    print *, '--------- Command='//trim(basecomname)//': all argv (size=', trim(ladjusted_int(size(rows))), ') ---------' 
    do i=1, nsize_rows
      print *, trim(ladjusted_int(i))//': ('//trim(rows(i)%key)//') ', trim(rows(i)%val)
    end do
  end subroutine dump_all_argv

end module m_handle_argv

! -------------------------------------------------------------------

! Sample program
!
program sample01
  use alpin_err_exit
  use alpin_misc_utils
  use alpin_hash        ! for type(t_alpin_hash_char), fetch_hashval() etc.
  use m_handle_argv

  implicit none
  integer :: status = 0
  character(len=1024) :: basecomname
  integer :: istart_main
  type(t_alpin_hash_char),    dimension(:), allocatable :: allargv
  type(t_alpin_hash_logical), dimension(DIM_FLAGS) :: flags

  call handle_argv(basecomname, istart_main, allargv, flags)

  ! If VERBOSE flag is set (specified with either a command-line argument or environmental variable), print the arguments:
  if (fetch_hashval('verbose', flags)) call dump_all_argv(basecomname, allargv)
  print *, '---------'

  write(*,'(A,L)') 'fetch_hashval(''verbose'', flags) => ', fetch_hashval('verbose', flags)
  write(*,'("--")')
  write(*,'(A,F10.5)') 'rad2deg(PI) => ', rad2deg(PI)
  write(*,'(A,F10.8)') 'deg2rad(180.0d0) => ', deg2rad(180.0d0)
  write(*,'("--")')
  write(*,'(A,A)') 'trim(dump_int_binary(127_1)) => ', trim(dump_int_binary(127_1))
  write(*,'(A,A)') 'trim(dump_int_binary(-1_1))  => ', trim(dump_int_binary(-1_1))
  write(*,'(A,A)') 'trim(dump_int_binary(127_2)) => ', trim(dump_int_binary(127_2))
  write(*,'(A,A)') 'trim(dump_int_binary(383_4)) => ', trim(dump_int_binary(383_4))
  write(*,'("--")')
  write(*,'(A,I3)') 'int4_to_unsigned1(255)  => ', int4_to_unsigned1(255)
  write(*,'(A,I3)') 'unsigned1_to_int4(-1_1) => ', unsigned1_to_int4(-1_1)
  write(*,'("--")')
  write(*,'(A,A)') 'trim(basename(''/a/b/c.d'')) => ', trim(basename('/a/b/c.d'))
  write(*,'(A,A)') 'trim(basename(''     c.d'')) => ', trim(basename('c.d'))
  write(*,'(A,A)') 'trim(basename(''/x/abc/''))   => ', trim(basename('/x/abc/'))
  write(*,'(A,A)') 'trim(basename(''/x/abc///'')) => ', trim(basename('/x/abc///'))
  write(*,'(A,A)') 'trim(basename(''/'')) => ', trim(basename('/'))
  write(*,'("--")')
  write(*,'(A,A)') 'trim(ladjusted_int(0_1))   => ', trim(ladjusted_int(0_1))
  write(*,'(A,A)') 'trim(ladjusted_int(-1234)) => ', trim(ladjusted_int(-1234))
  write(*,'("--")')
  write(*,'(A,A)') 'trim(join_ary([''ab  '',''cdef'',''ghi '']))           => ', trim(join_ary(['ab  ','cdef','ghi ']))
  write(*,'(A,A)') 'trim(join_ary([''ab  '',''cdef'',''ghi ''],quote=''$'')) => ', trim(join_ary(['ab  ','cdef','ghi '],quote='$'))
  write(*,'(A,A)') 'trim(join_ary([-123, 4, -56], sep=''; '')) => ', trim(join_ary([-123, 4, -56], sep='; '))
  write(*,'("--")')
  write(*,'(A,I1)') 'findloc1([''ab  '',''cdef'',''ghi ''], ''cdef'')  => ', findloc1(['ab  ','cdef','ghi '], 'cdef')
  write(*,'(A,I1)') 'findloc1([12, -56, 12], 12)               => ', findloc1([12, -56, 12], 12)
  write(*,'(A,I1)') 'findloc1([12, -56, 12], 12, BACK=.true.)  => ', findloc1([12, -56, 12], 12, BACK=.true.)
  write(*,'(A,I1)') 'findloc1([12, -56, 12], 12, MASK=[F,T,T]) => ', findloc1([12, -56, 12], 12, MASK=[.false.,.true.,.true.])

  if (allocated(allargv)) deallocate(allargv)

  write(*,'("---- call err_exit_with_msg(''This is my expected error.'') => exit_status=1")')
  call err_exit_with_msg('This is my expected error.')

  call err_exit_play_safe('The program would not reach this line.')
  call err_exit_play_safe()
end program sample01

