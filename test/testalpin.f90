!
! common routines for testing
!
module test_common
  implicit none
  integer, parameter, public :: dp = kind(1.d0)  ! Common in the test routines.

  character(len=*), parameter :: SAMPLE_DIR = '../samples'
  character(len=*), parameter :: DEF_FNAME_OUT       = SAMPLE_DIR//'/test_out.txt'

contains
  ! Print the stats info at the end of each subroutine-level testing
  subroutine print_teststats(subname, nsuccess, ifailed)
    character(*), intent(in) :: subname
    integer,      intent(in) :: nsuccess  ! Number of succesful trials, i-th failure
    integer,      intent(in), optional :: ifailed ! i-th failure

    if (present(ifailed)) then
      write(*, '(" Tests(", a, "): Only succeeded in ", i3, " tests before failed at ", i3, "-th.")') &
         trim(subname), nsuccess, ifailed
    else
      write(*, '(" Tests(", a, "): Succeeded in all ", i3, " tests.")') &
         trim(subname), nsuccess
    end if
  end subroutine print_teststats
end module test_common

module test_alpin_misc_utils
  use alpin_unittest
  use alpin_err_exit
  use test_common
  use alpin_misc_utils

  implicit none 
contains

  ! run tests of routines in alpin_misc_utils.f90
  !
  ! Returns 1 if error is raised (0 otherwise)
  integer function run_test_alpin_misc_utils() result(ret_status)
    character(*), parameter :: Subname = 'run_test_alpin_misc_utils'
    integer,      parameter :: TOT_NTESTS = 39
    logical, dimension(TOT_NTESTS) :: ress

    integer :: iloc

    ret_status = 0  ! normal ends (n.b., returned value)

    ress = [ &
        assert_in_delta(180.0d0, rad2deg(PI), 1.0e5, Subname, ' rad2deg(PI)') &
      , assert_in_delta(PI, deg2rad(180.0d0), 1.0e5, Subname, ' deg2rad(180.0)') &
      , assert_equal ('00000001', dump_int_binary(1_1),   Subname, 'dump_int_binary') &
      , assert_equal ('01111111', dump_int_binary(127_1), Subname, 'dump_int_binary') &
      , assert_equal ('11111111', dump_int_binary(-1_1),  Subname, 'dump_int_binary') &
      , assert_equal (3, int4_to_unsigned1(3),   Subname, 'for (3(int4=>1))') &
      , assert_equal(-1, int4_to_unsigned1(255), Subname, 'for (255(int4=>1))') &
      , assert_equal(255, unsigned1_to_int4(-1_1), Subname, 'for (255(int1=>4))') &
      , assert_equal ('00000000_00000000_00000000_01111111', dump_int_binary(127_4), Subname, 'dump_int_binary(127)') &
      , assert_equal ('00000000_00000000_00000001_01111111', dump_int_binary(383_4), Subname, 'dump_int_binary(383)') &
      , assert_equal ('11111111_11111111_11111111_11111111', dump_int_binary(-1_4),  Subname, 'dump_int_binary(-1)') &
      , assert_equal('c.d', trim(basename('/a/b/c.d')),  Subname, 'basename(/a/b/c.d)') &
      , assert_equal('c.d', trim(basename('c.d')),       Subname, 'basename(c.d)') &
      , assert_equal('abc', trim(basename('/x/abc/')),   Subname, 'basename(/x/abc/)') &
      , assert_equal('abc', trim(basename('/x/abc///')), Subname, 'basename(/x/abc///)') &
      , assert_equal('/',   trim(basename('/')),         Subname, 'basename(/)') &
      , assert_equal('/',   trim(basename('//')),        Subname, 'basename(//)') &
      , assert_equal('0',       trim(ladjusted_int(0_1)),     Subname, 'ladjusted_int(0_1)') &
      , assert_equal('-123',    trim(ladjusted_int(-123_1)),  Subname, 'ladjusted_int(-123_1)') &
      , assert_equal('-123',    trim(ladjusted_int(-123_2)),  Subname, 'ladjusted_int(-123_2)') &
      , assert_equal('-123',    trim(ladjusted_int(-123_4)),  Subname, 'ladjusted_int(-123_4)') &
      , assert_equal('-123',    trim(ladjusted_int(-123_8)),  Subname, 'ladjusted_int(-123_8)') &
      , assert_equal('-12345',  trim(ladjusted_int(-12345_2)),Subname, 'ladjusted_int(-12345_2)') &
      , assert_equal('-123456', trim(ladjusted_int(-123456)), Subname, 'ladjusted_int(-123456)') &
      , assert_equal('-123456', trim(ladjusted_int(-123456)), Subname, 'ladjusted_int(-123456)') &
      , assert_equal('''ab'', ''cdef'', ''ghi''', join_ary(['ab  ','cdef','ghi ']), Subname, 'join_ary_chars()') &
      , assert_equal('$ab$, $cdef$, $ghi$', join_ary(['ab  ','cdef','ghi '],quote='$'), Subname, 'join_ary_chars()') &
      , assert_equal('-123, 4, -56', join_ary([-123, 4, -56]), Subname, 'join_ary_int4()') &
      , assert_equal('-123; 4; -56', join_ary([-123_2, 4_2, -56_2], '; '), Subname, 'join_ary_int2()') &
      , assert_equal(1, findloc1(['X','Y','X'],'X'), Subname, 'findloc1_char') &
      , assert_equal(3, findloc1(['X','Y','X'],'X',MASK=[.false.,.true.,.true.]), Subname, 'findloc1_char-mask') &
      , assert_equal(3, findloc1(['X','Y','X'],'X',BACK=.true.), Subname, 'findloc1_char-back') &
      , assert_equal(1, findloc1(['X','Y','X'],'X',MASK=[.true.,.true.,.false.],BACK=.true.), Subname, 'findloc1_char-ma-ba') &
      , assert_equal(2, findloc1([9,8,7,6],8), Subname, 'findloc1_int4') &
      , assert_equal(2, findloc1(int([9,8,7,6],kind=2),8), Subname, 'findloc1_int2_4') &
      , assert_equal(2, findloc1(real([16,32,0,0,-1],kind=4),32.0),  Subname, 'findloc1_real4') &
      , assert_equal(2, findloc1(real([16,32,0,0,-1],kind=4),32.d0), Subname, 'findloc1_real4-2') &
      , assert_equal(2, findloc1(real([16,32,0,0,-1],kind=8),32.0),  Subname, 'findloc1_real8-1') &
      , assert_equal(2, findloc1(real([16,32,0,0,-1],kind=8),32.d0), Subname, 'findloc1_real8-2') &
      ]

    iloc  = findloc1(ress, .false.)  ! Similar to the standard way, but scalar:  ilocs(:)=findloc(ress, .false.)
    if (iloc == 0) then  ! All passed
      call print_teststats(Subname, nsuccess=TOT_NTESTS)
      return
    end if

    ! Failed
    call print_teststats(Subname, nsuccess=iloc-1, ifailed=iloc)
    ret_status = 1
  end function run_test_alpin_misc_utils
end module test_alpin_misc_utils


module test_alpin_hash
  use alpin_unittest
  use alpin_err_exit
  use test_common
  use alpin_misc_utils
  use alpin_hash

  implicit none 
contains

  ! run tests of routines in alpin_misc_utils.f90
  !
  ! Returns 1 if error is raised (0 otherwise)
  integer function run_test_alpin_hash() result(ret_status)
    character(*), parameter :: Subname = 'run_test_alpin_hash'
    integer,      parameter :: TOT_NTESTS = 11
    logical, dimension(TOT_NTESTS) :: ress

    integer :: iloc
    type(t_alpin_hash_logical), dimension(2) :: arlogi
    type(t_alpin_hash_char),    dimension(2) :: archar
    type(t_alpin_hash_int4),    dimension(2) :: arint4
    type(t_alpin_hash_real8),   dimension(2) :: arreal8
    logical, dimension(2) :: is_undef

    character(len=LEN_T_ALPIN_HASH), dimension(2) :: valchs

    ret_status = 0  ! normal ends (n.b., returned value)
    arlogi = [t_alpin_hash_logical(key='k1', val=.false.), t_alpin_hash_logical(key='k2', val=.true.)]
    archar = [t_alpin_hash_char( key='k1', val='v1'), t_alpin_hash_char( key='k2', val='v2')]
    arint4 = [t_alpin_hash_int4( key='k1', val=11),   t_alpin_hash_int4( key='k2', val=22)]
    arreal8= [t_alpin_hash_real8(key='k1', val=16.0), t_alpin_hash_real8(key='k2', val=32.0)]

    call hashval_status('k2', archar, valchs(1), is_undef(1))
    call hashval_status('naiyo', archar, valchs(2), is_undef(2))

    ress = [ &
        assert_not(is_undef(1), Subname, ' hashval_status()-1') &
      , assert(    is_undef(2), Subname, ' hashval_status()-2') &
      , assert_equal('v2', valchs(1), Subname, ' hashval_status("k2")') &
      , assert_equal('v2', trim(fetch_hashval('k2', archar)), Subname, ' fetch_hashval("k2")') &  ! NOTE: without trim(), this causes either "malloc: *** set a breakpoint in malloc_error_break to debug" or "SIGSEGV: Segmentation fault - invalid memory reference."
         ! It seems that if a character as a returned value from a function is passed directly (i.e., without trim()) to a different function, it may cause a memory-related error.
      , assert_equal('@', trim(fetch_hashval('naiyo', archar, undef='@')), Subname, ' fetch_hashval("naiyo","@")') &
      , assert_equal('',  trim(fetch_hashval('naiyo', archar)), Subname, ' fetch_hashval("naiyo")') &
      , assert_equal(22, fetch_hashval('k2', arint4), Subname, ' fetch_hashval_i4("k2")') &
      , assert_equal(79, fetch_hashval('naiyo', arint4, undef=79), Subname, ' fetch_hashval_i4("k2",79)') &
      , assert(fetch_hashval('k2', arlogi), Subname, ' fetch_hashval_logi("k2")') &
      , assert_in_delta(32.d0, fetch_hashval('k2', arreal8), 1e-8, Subname, ' fetch_hashval_real8("k2")') &
      , assert_in_delta(79.d0, fetch_hashval('naiyo', arreal8, undef=79.d0), 1e-8, Subname, ' fetch_hashval_real8("k2",79)') &
      ]

    iloc  = findloc1(ress, .false.)  ! Similar to the standard way, but scalar:  ilocs(:)=findloc(ress, .false.)
    if (iloc == 0) then  ! All passed
      call print_teststats(Subname, nsuccess=TOT_NTESTS)
      return
    end if

    ! Failed
    call print_teststats(Subname, nsuccess=iloc-1, ifailed=iloc)
    ret_status = 1
  end function run_test_alpin_hash
end module test_alpin_hash

! -------------------------------------------------------------------

program testalpin
  use test_alpin_misc_utils
  use test_alpin_hash

  implicit none
  integer :: status = 0

  status = status + run_test_alpin_misc_utils()
  status = status + run_test_alpin_hash()

  if (status .ne. 0) then
    call EXIT(status)  ! for gfortran, Lahey Fujitsu Fortran 95, etc
  end if
end program testalpin

