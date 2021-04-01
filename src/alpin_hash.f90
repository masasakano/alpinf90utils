! Fortran (Perl/Ruby) "hash" emulation
!
module alpin_hash
  use iso_fortran_env, only : STDERR=>ERROR_UNIT
  use alpin_misc_utils  ! for findloc1 etc.
  implicit none 

  integer, parameter :: LEN_READABLE_KEY = 64  ! Char-length of maximum human-readable key name
  integer, parameter :: LEN_T_ALPIN_HASH = 4096

  type t_alpin_hash_logical
    character(len=LEN_READABLE_KEY) :: key;
    logical :: val = .false.;
  end type t_alpin_hash_logical

  type t_alpin_hash_char
    character(len=LEN_READABLE_KEY) :: key;
    character(len=LEN_T_ALPIN_HASH) :: val = '';
  end type t_alpin_hash_char

  type t_alpin_hash_int1
    character(len=LEN_READABLE_KEY) :: key;
    integer(kind=1) :: val = -99;
  end type t_alpin_hash_int1

  type t_alpin_hash_int2
    character(len=LEN_READABLE_KEY) :: key;
    integer(kind=2) :: val = -999;
  end type t_alpin_hash_int2

  type t_alpin_hash_int4
    character(len=LEN_READABLE_KEY) :: key;
    integer(kind=4) :: val = -99999;
  end type t_alpin_hash_int4

  type t_alpin_hash_int8
    character(len=LEN_READABLE_KEY) :: key;
    integer(kind=8) :: val = -9999999999_8;
  end type t_alpin_hash_int8

  type t_alpin_hash_real4
    character(len=LEN_READABLE_KEY) :: key;
    real(kind=4) :: val = -1024.0;
  end type t_alpin_hash_real4

  type t_alpin_hash_real8
    character(len=LEN_READABLE_KEY) :: key;
    real(kind=8) :: val = -1024.d0;
  end type t_alpin_hash_real8

  type t_alpin_hash_real16
    character(len=LEN_READABLE_KEY) :: key;
    real(kind=16) :: val = -1024.d0;
  end type t_alpin_hash_real16

  interface hashval_status
    module procedure hashval_status_l, hashval_status_char, hashval_status_int4, hashval_status_real8
  end interface hashval_status

  interface fetch_hashval
    module procedure fetch_hashval_l, fetch_hashval_char, fetch_hashval_int4, fetch_hashval_real8
  end interface fetch_hashval

contains

  !-----------------------------------------
  ! interface hashval_status
  !   : to get the (representative) value of the first element of the array which has the key
  !   : If there is no matching key, is_undef is set to .true.
  !-----------------------------------------
  !
  subroutine hashval_status_l(key, ary, val, is_undef)
    character(len=*), intent(in) :: key
    type(t_alpin_hash_logical), dimension(:), intent(in) :: ary
    logical, intent(out) :: val
    logical, intent(out) :: is_undef
    integer :: iloc
    type(t_alpin_hash_logical) :: tmp

    is_undef = .false.

    iloc = findloc1(ary%key, trim(key))
    if (iloc == 0) then  ! no matching key is found.
      is_undef = .true.
      tmp = t_alpin_hash_logical(key='dummy')
      val = tmp%val
      return
    end if

    val = ary(iloc)%val
  end subroutine hashval_status_l

  subroutine hashval_status_char(key, ary, val, is_undef)
    character(len=*), intent(in) :: key
    type(t_alpin_hash_char), dimension(:), intent(in) :: ary
    character(len=*), intent(out) :: val
    logical,          intent(out) :: is_undef
    integer :: iloc
    type(t_alpin_hash_char) :: tmp

    is_undef = .false.

    iloc = findloc1(ary%key, trim(key))  ! findloc1() defined in alpin_misc_utils
    if (iloc == 0) then  ! no matching key is found.
      is_undef = .true.
      !tmp = t_alpin_hash_char(key=trim(key))
      tmp = t_alpin_hash_char(key='dummy')
      val = trim(tmp%val)
      return
    end if

    val = trim(ary(iloc)%val)
  end subroutine hashval_status_char

  subroutine hashval_status_int4(key, ary, val, is_undef)
    character(len=*), intent(in) :: key
    type(t_alpin_hash_int4), dimension(:), intent(in) :: ary
    integer(kind=4),  intent(out) :: val
    logical,          intent(out) :: is_undef
    integer :: iloc
    type(t_alpin_hash_int4) :: tmp

    is_undef = .false.

    iloc = findloc1(ary%key, trim(key))
    if (iloc == 0) then  ! no matching key is found.
      is_undef = .true.
      tmp = t_alpin_hash_int4(key='dummy')
      val = tmp%val
      return
    end if

    val = ary(iloc)%val
  end subroutine hashval_status_int4

  subroutine hashval_status_real8(key, ary, val, is_undef)
    character(len=*), intent(in) :: key
    type(t_alpin_hash_real8), dimension(:), intent(in) :: ary
    real(kind=8), intent(out) :: val
    logical,      intent(out) :: is_undef
    integer :: iloc
    type(t_alpin_hash_real8) :: tmp

    is_undef = .false.

    iloc = findloc1(ary%key, trim(key))
    if (iloc == 0) then  ! no matching key is found.
      is_undef = .true.
      tmp = t_alpin_hash_real8(key='dummy')
      val = tmp%val
      return
    end if

    val = ary(iloc)%val
  end subroutine hashval_status_real8

  !-----------------------------------------
  ! interface fetch_hashval
  !   : to get the (representative) value of the first element of the array which has the key
  !   : If there is no matching key and if undef is specified, it is returned.
  !   : If there is no matching key and if undef is NOT specified, the default value of the type is returned.
  !-----------------------------------------
  !
  function fetch_hashval_l(key, ary, undef) result(retobj)
    character(len=*), intent(in) :: key
    type(t_alpin_hash_logical), dimension(:), intent(in) :: ary
    logical, intent(in), optional :: undef
    logical :: retobj
    logical :: is_undef
    type(t_alpin_hash_logical) :: tmp

    call hashval_status_l(key, ary, retobj, is_undef)
    if (is_undef) then
      if (present(undef)) then
        retobj = undef
      else
        tmp = t_alpin_hash_logical(key='dummy')
        retobj = tmp%val
      end if
    end if
  end function fetch_hashval_l

  function fetch_hashval_char(key, ary, undef) result(retobj)
    character(len=*), intent(in) :: key
    type(t_alpin_hash_char), dimension(:), intent(in) :: ary
    character(len=*), intent(in), optional :: undef
    character(len=LEN_T_ALPIN_HASH) :: retobj
    logical :: is_undef
    type(t_alpin_hash_char) :: tmp

    call hashval_status_char(key, ary, retobj, is_undef)
    if (is_undef) then
      if (present(undef)) then
        retobj = trim(undef)  ! if this value is passed to a next function direclty, => malloc: *** set a breakpoint in malloc_error_break to debug
        !retobj = undef ! => SIGSEGV: Segmentation fault - invalid memory reference.
      else
        tmp = t_alpin_hash_char(key='dummy')
        retobj = trim(tmp%val)
      end if
    end if
  end function fetch_hashval_char

  function fetch_hashval_int4(key, ary, undef) result(retobj)
    character(len=*), intent(in) :: key
    type(t_alpin_hash_int4), dimension(:), intent(in) :: ary
    integer(kind=4), intent(in), optional :: undef
    integer(kind=4) :: retobj
    logical :: is_undef
    type(t_alpin_hash_int4) :: tmp

    call hashval_status_int4(key, ary, retobj, is_undef)
    if (is_undef) then
      if (present(undef)) then
        retobj = undef
      else
        tmp = t_alpin_hash_int4(key='dummy')
        retobj = tmp%val
      end if
    end if
  end function fetch_hashval_int4

  function fetch_hashval_real8(key, ary, undef) result(retobj)
    character(len=*), intent(in) :: key
    type(t_alpin_hash_real8), dimension(:), intent(in) :: ary
    real(kind=8), intent(in), optional :: undef
    real(kind=8) :: retobj
    logical :: is_undef
    type(t_alpin_hash_real8) :: tmp

    call hashval_status_real8(key, ary, retobj, is_undef)
    if (is_undef) then
      if (present(undef)) then
        retobj = undef
      else
        tmp = t_alpin_hash_real8(key='dummy')
        retobj = tmp%val
      end if
    end if
  end function fetch_hashval_real8
end module alpin_hash

