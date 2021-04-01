! Fortran general utility routines
!
module alpin_misc_utils
  use iso_fortran_env, only : STDERR=>ERROR_UNIT
  implicit none 

  real(kind=8), parameter, public :: PI = atan(1.0d0)*4
  !          == 3.141592653589793116 (in REAL64)
  ! True value: 3.1415926535897932384626433832795028841971693993751...

  integer, parameter, public :: LEN_JOIN_ARY = 4096
  integer, parameter, public :: LEN_JOIN_ARY_SEP = 16
  
  interface join_ary
    module procedure join_ary_chars, join_ary_i8, join_ary_i4, join_ary_i2, join_ary_i1
  end interface join_ary

  interface ladjusted_int
    module procedure ladjusted_int1, ladjusted_int2, ladjusted_int4, ladjusted_int8 
  end interface ladjusted_int

  ! Convert a (Scalar/Array) Integer*1 as a byte (namely unsigned) into the (default) Integer*4
  !
  ! DESCRIPTION:
  !   The integers in the range (0:127) are the same.
  !   (-127:-1) must be added with 256 (e.g., -1_1 is 255 in byte).
  interface unsigned1_to_int4
    module procedure unsigned1_to_int4_scalar, unsigned1_to_int4_array
  end interface unsigned1_to_int4

  interface int4_to_unsigned1
    module procedure int4_to_unsigned1_scalar
  end interface int4_to_unsigned1

  interface dump_int_binary
    module procedure dump_int1_binary, dump_int2_binary, dump_int4_binary
  end interface dump_int_binary

  ! The same as builtin FINDLOC() except this is for a 1-dimensional array only
  ! and hence returns a scalar integer.
  interface findloc1
    module procedure findloc1_l, findloc1_char
    module procedure findloc1_i1,   findloc1_i1_2, findloc1_i1_4, findloc1_i1_8
    module procedure findloc1_i2_1, findloc1_i2,   findloc1_i2_4, findloc1_i2_8
    module procedure findloc1_i4_1, findloc1_i4_2, findloc1_i4,   findloc1_i4_8
    module procedure findloc1_i8_1, findloc1_i8_2, findloc1_i8_4, findloc1_i8
    module procedure findloc1_r4,    findloc1_r4_8
    module procedure findloc1_r8_4,  findloc1_r8
    module procedure findloc1_r16_4, findloc1_r16_8, findloc1_r16
  end interface findloc1

  private :: priv_calc_sep_len
contains

  !-----------------------------------------
  ! Radian to degree
  real(kind=8) function rad2deg(rad)
    real(kind=8), intent(in) :: rad

    rad2deg = rad * 180.0d0/PI
  end function rad2deg

  !-----------------------------------------
  ! Radian to degree
  real(kind=8) function deg2rad(deg)
    real(kind=8), intent(in) :: deg

    deg2rad = deg * PI/180.0d0
  end function deg2rad


  !-----------------------------------------
  ! Return the basename of the given filename (with the same len())
  !-----------------------------------------
  recursive function basename(fname) result(retchar)
    character(len=1), parameter :: Delimeter = '/'
    character(len=*), intent(in) :: fname
    character(len=len(fname)) :: retchar

    integer :: pos

    retchar = fname
    if (trim(retchar) == Delimeter) return  ! basename('/') ! => '/'

    pos = scan(trim(retchar), Delimeter, BACK=.true.)
    if (pos .le. 0) return  ! No delimiter is found.
    if (pos < len_trim(retchar)) then
      retchar = fname(pos+1:len_trim(retchar))
      return
    end if

    ! Now, the last character of fname is Delimiter='/'.
    ! Then, for example, if fname=='abc//', this returns 'abc' (by recursive calls).
    retchar = basename(fname(1:pos-1))
  end function basename

  !-----------------------------------------
  ! Private subroutine (common among join_ary())
  !
  ! If sep is not given, the default ssep (=separator) and its length are set.
  ! If sep is given, ssep (=separator) and its length are set accordingly.
  !-----------------------------------------
  subroutine priv_calc_sep_len(len_sep, ssep, sep, subname)
    integer, intent(out) :: len_sep
    character(len=LEN_JOIN_ARY_SEP), intent(out) :: ssep
    character(len=*), intent(in), optional :: sep
    character(len=*), intent(in), optional :: subname ! potentially only used when sep is given.

    ssep = ', '  ! Default
    len_sep = 2  ! Default
    if (present(sep)) then
      if (len_trim(sep) > LEN_JOIN_ARY_SEP) write(STDERR, '("WARNING(",A,"): sep is too long and so is trimed: ''",A,"''")') &
         subname, trim(sep)
      ssep = trim(sep)
      len_sep = len(sep)
    end if
  end subroutine priv_calc_sep_len

  !-----------------------------------------
  ! Returns a character, join-ing a Character Array with a separator.
  !
  ! 'sep' is a separator. Make sure to specify the length correctly, because
  ! the length is taken into account;
  ! e.g., ', ' or sep(1:2), or else trim(',  ') would become ',' with no trailing spaces.
  !
  ! Example:
  !   join_ary(['ab ','cde']) ! => "'ab', 'cde'"
  function join_ary_chars(ary, sep, quote) result(retchar)
    character(*), parameter :: Subname = 'join_chars'
    character(len=*), dimension(:), intent(in) :: ary
    character(len=*), intent(in), optional :: sep
    character(len=*), intent(in), optional :: quote
    character(len=LEN_JOIN_ARY_SEP) :: ssep, squote
    character(len=LEN_JOIN_ARY) :: retchar
    integer :: i, len_sep = -999

    retchar = ''
    if (size(ary) == 0) return

    squote = ''''

    if (present(sep)) then
      call priv_calc_sep_len(len_sep, ssep, sep, Subname)
    else
      call priv_calc_sep_len(len_sep, ssep)
    end if

    if (present(quote)) then
      if (len_trim(quote) > LEN_JOIN_ARY_SEP) write(STDERR, '("WARNING(",A,"): quote is too long and so is trimed: ''",A,"''")') &
         Subname, trim(quote)
      squote = trim(quote)
    end if

    retchar = trim(squote)//trim(ary(1))//trim(squote)
    if (size(ary) == 1) return

    do i=2, size(ary)
      write(retchar,'(A,A,A,A,A)') trim(retchar), ssep(1:len_sep), trim(squote), trim(ary(i)), trim(squote)
    end do
  end function join_ary_chars

  !-----------------------------------------
  ! Returns a character, join-ing an Integer Array with a separator.
  !
  function join_ary_i8(ary, sep) result(retchar)
    character(*), parameter :: Subname = 'join_ary_i8'
    integer(kind=8), dimension(:), intent(in) :: ary
    character(len=*), intent(in), optional :: sep
    character(len=LEN_JOIN_ARY_SEP) :: ssep
    character(len=LEN_JOIN_ARY) :: retchar
    integer :: i, len_sep = -999

    retchar = ''
    if (size(ary) == 0) return

    if (present(sep)) then
      call priv_calc_sep_len(len_sep, ssep, sep, Subname)
    else
      call priv_calc_sep_len(len_sep, ssep)
    end if

    retchar = trim(ladjusted_int(ary(1)))
    if (size(ary) == 1) return

    do i=2, size(ary)
      retchar = trim(retchar)//ssep(1:len_sep)//trim(ladjusted_int(ary(i)))
    end do
  end function join_ary_i8

  !-----------------------------------------
  ! Returns a character, join-ing an Integer Array with a separator.
  !
  function join_ary_i4(ary, sep) result(retchar)
    character(*), parameter :: Subname = 'join_ary_i4'
    integer(kind=4), dimension(:), intent(in) :: ary
    character(len=*), intent(in), optional :: sep
    character(len=LEN_JOIN_ARY) :: retchar

    if (present(sep)) then
      retchar = join_ary_i8(int(ary, kind=8), sep)
    else
      retchar = join_ary_i8(int(ary, kind=8))
    end if
  end function join_ary_i4

  !-----------------------------------------
  ! Returns a character, join-ing an Integer Array with a separator.
  !
  function join_ary_i2(ary, sep) result(retchar)
    character(*), parameter :: Subname = 'join_ary_i2'
    integer(kind=2), dimension(:), intent(in) :: ary
    character(len=*), intent(in), optional :: sep
    character(len=LEN_JOIN_ARY) :: retchar

    if (present(sep)) then
      retchar = join_ary_i8(int(ary, kind=8), sep)
    else
      retchar = join_ary_i8(int(ary, kind=8))
    end if
  end function join_ary_i2

  !-----------------------------------------
  ! Returns a character, join-ing an Integer Array with a separator.
  !
  function join_ary_i1(ary, sep) result(retchar)
    character(*), parameter :: Subname = 'join_ary_i1'
    integer(kind=1), dimension(:), intent(in) :: ary
    character(len=*), intent(in), optional :: sep
    character(len=LEN_JOIN_ARY) :: retchar

    if (present(sep)) then
      retchar = join_ary_i8(int(ary, kind=8), sep)
    else
      retchar = join_ary_i8(int(ary, kind=8))
    end if
  end function join_ary_i1

  !-----------------------------------------
  ! Returns a character of left-adjusted integer (with prefix if specified)
  !
  ! if len_trim(prefix) is too long, the returned character is an error message!
  !
  ! NOTE: If this was a subroutine using intent(inout), the character length
  !       would be taken care of by the caller, though the caller would have
  !       a more job as a result.
  !-----------------------------------------
  function ladjusted_int8(i) result(retchar)
    integer, parameter :: Len_max_int = 20  ! Maximum length of chars required for Integer*8
      ! NOTE: if you change this value, make sure to change the value in write() in the code below.

    integer(kind=8), intent(in) :: i  ! Integer to be left-adjusted

    character(len=Len_max_int) :: chari
    character(len=Len_max_int) :: retchar ! OK up to Integer*8

    write(chari, '(I20)') i
    retchar = trim(adjustl(chari))
  end function ladjusted_int8

  !-----------------------------------------
  function ladjusted_int4(i) result(retchar)
    integer, parameter :: Len_max_int = 11  ! Maximum length of chars required for Integer*4

    integer(kind=4), intent(in) :: i  ! Integer to be left-adjusted
    character(len=Len_max_int) :: retchar

    retchar = trim(ladjusted_int8(int(i, kind=8)))
  end function ladjusted_int4

  !-----------------------------------------
  function ladjusted_int2(i) result(retchar)
    integer, parameter :: Len_max_int = 6  ! Maximum length of chars required for Integer*2

    integer(kind=2), intent(in) :: i  ! Integer to be left-adjusted
    character(len=Len_max_int) :: retchar

    retchar = trim(ladjusted_int8(int(i, kind=8)))
  end function ladjusted_int2

  !-----------------------------------------
  function ladjusted_int1(i) result(retchar)
    integer, parameter :: Len_max_int = 4  ! Maximum length of chars required for Integer*1

    integer(kind=1), intent(in) :: i  ! Integer to be left-adjusted
    character(len=Len_max_int) :: retchar

    retchar = trim(ladjusted_int8(int(i, kind=8)))
  end function ladjusted_int1


  !-----------------------------------------
  ! unsigned1_to_int4
  !-----------------------------------------

  ! Convert a Scalar Integer*1 as a byte (namely unsigned) into the default Integer*4
  integer function unsigned1_to_int4_scalar(ibyte) result(iret)
    integer(kind=1), intent(in) :: ibyte

    iret = -9999
    if (ibyte < 0) then
      iret = ibyte + 256
    else
      iret = ibyte
    end if
  end function unsigned1_to_int4_scalar

  ! Convert an Array Integer*1 as a byte (namely unsigned) into the default Integer*4
  function unsigned1_to_int4_array(ibytes) result(irets)
    integer(kind=1), dimension(:), intent(in) :: ibytes
    integer,         dimension(size(ibytes))  :: irets  ! return
    integer :: i

    do i=1, size(ibytes)
      irets(i) = unsigned1_to_int4_scalar(ibytes(i))
    end do
  end function unsigned1_to_int4_array

  !-----------------------------------------
  ! int4_to_unsigned1
  !-----------------------------------------

  ! Convert a Scalar Integer*4 into the Integer*1 AS A BYTE (namely unsigned)
  !
  ! Make sure the given argument is between 0 and 255.
  !
  ! The difference from  int(i, kind=1)  is how to convert the Integer*4
  ! number below 0 and between 128 and 255. This abnormally exits with the former
  ! whereas the standard int() would raise an error or warning with the latter.
  ! Also, this EXIT(1) when the argument is out of range.
  function int4_to_unsigned1_scalar(i4) result(ibyte)
    integer, intent(in) :: i4
    integer(kind=1) :: ibyte

    if ((i4 < 0) .or. (255 < i4)) then
      write(STDERR, &
         '("ERROR(int4_to_unsigned1_scalar): Value=(",I11,") is out of the range [0:255] to convert.")') i4
      call EXIT(1)  ! for gfortran, Lahey Fujitsu Fortran 95, etc
    end if

    if (i4 > 127) then
      ibyte = int(i4 - 256, kind=1)
    else
      ibyte = int(i4, kind=1)
    end if
  end function int4_to_unsigned1_scalar

  !-----------------------------------------
  ! dump_int_binary
  !-----------------------------------------

  ! Returns a character(len=8) of a binary for the given Integer*1
  !
  ! == Examples ==
  !
  !   ch = dump_int1_binary(127_1) ! => '01111111'
  !   ch = dump_int1_binary(-1_1)  ! => '11111111'
  function dump_int1_binary(i1) result(retc)
    integer(kind=1), intent(in) :: i1
    character(len=8) :: retc
    integer :: j, k

    retc(:) = '00000000'
    do j=0, 7
      k = 8-j
      if (btest(i1, j)) retc(k:k) = '1'
    end do
  end function dump_int1_binary

  ! Returns a character(len=17) of all 16 bits (8 bits x 2) of the given Integer*2
  !
  ! See dump_int4_binary for detail.
  function dump_int2_binary(i2) result(retc)
    integer, parameter :: Ikind = 2
    integer(kind=2), intent(in) :: i2
    character(len=8), dimension(Ikind) :: tmpc
    character(len=Ikind*8+Ikind-1) :: retc
    integer :: ir, j, k

    tmpc(:) = '00000000'
    do ir=1, Ikind
      do j=0, 7
        k = 8-j
        if (btest(i2, j+(ir-1)*8)) tmpc(ir)(k:k) = '1'
      end do
    end do

    retc = tmpc(Ikind)
    do j = Ikind-1, 1, -1
      retc = trim(retc)//'_'//tmpc(j)
    end do
  end function dump_int2_binary

  ! Returns a character(len=35) of all 32 bits (8 bits x 4) of the given Integer*4
  !
  ! == Examples ==
  !
  !   ch = dump_int1_binary(127_4) ! => '00000000_00000000_00000000_01111111'
  !   ch = dump_int1_binary(383_4) ! => '00000000_00000000_00000001_01111111' ! 383 == 256+127
  !   ch = dump_int1_binary(-1_1)  ! => '11111111_11111111_11111111_11111111'
  function dump_int4_binary(i4) result(retc)
    integer, parameter :: Ikind = 4
    integer(kind=4), intent(in) :: i4
    character(len=8), dimension(Ikind) :: tmpc
    character(len=Ikind*8+Ikind-1) :: retc
    integer :: ir, j, k

    tmpc(:) = '00000000'
    do ir=1, Ikind
      do j=0, 7
        k = 8-j
        if (btest(i4, j+(ir-1)*8)) tmpc(ir)(k:k) = '1'
      end do
    end do

    retc = tmpc(Ikind)
    do j = Ikind-1, 1, -1
      retc = trim(retc)//'_'//tmpc(j)
    end do
  end function dump_int4_binary

  !-----------------------------------------
  ! interface findloc1
  !
  ! cf. https://gcc.gnu.org/onlinedocs/gfortran/FINDLOC.html
  !-----------------------------------------
  !
  integer function findloc1_l(array, value, mask, back) result(iloc)
    logical, dimension(:), intent(in) :: array
    logical, intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_l

  integer function findloc1_char(array, value, mask, back) result(iloc)
    character(len=*), dimension(:), intent(in) :: array
    character(len=*), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_char

  integer function findloc1_i1(array, value, mask, back) result(iloc)
    integer(kind=1), dimension(:), intent(in) :: array
    integer(kind=1), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_i1

  integer function findloc1_i1_2(array, value, mask, back) result(iloc)
    integer(kind=1), dimension(:), intent(in) :: array
    integer(kind=2), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_i1_2

  integer function findloc1_i1_4(array, value, mask, back) result(iloc)
    integer(kind=1), dimension(:), intent(in) :: array
    integer(kind=4), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_i1_4

  integer function findloc1_i1_8(array, value, mask, back) result(iloc)
    integer(kind=1), dimension(:), intent(in) :: array
    integer(kind=8), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_i1_8

  integer function findloc1_i2_1(array, value, mask, back) result(iloc)
    integer(kind=2), dimension(:), intent(in) :: array
    integer(kind=1), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_i2_1

  integer function findloc1_i2(array, value, mask, back) result(iloc)
    integer(kind=2), dimension(:), intent(in) :: array
    integer(kind=2), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_i2

  integer function findloc1_i2_4(array, value, mask, back) result(iloc)
    integer(kind=2), dimension(:), intent(in) :: array
    integer(kind=4), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_i2_4

  integer function findloc1_i2_8(array, value, mask, back) result(iloc)
    integer(kind=2), dimension(:), intent(in) :: array
    integer(kind=8), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_i2_8

  integer function findloc1_i4_1(array, value, mask, back) result(iloc)
    integer(kind=4), dimension(:), intent(in) :: array
    integer(kind=1), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_i4_1

  integer function findloc1_i4_2(array, value, mask, back) result(iloc)
    integer(kind=4), dimension(:), intent(in) :: array
    integer(kind=2), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_i4_2

  integer function findloc1_i4(array, value, mask, back) result(iloc)
    integer(kind=4), dimension(:), intent(in) :: array
    integer(kind=4), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_i4

  integer function findloc1_i4_8(array, value, mask, back) result(iloc)
    integer(kind=4), dimension(:), intent(in) :: array
    integer(kind=8), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_i4_8

  integer function findloc1_i8_1(array, value, mask, back) result(iloc)
    integer(kind=8), dimension(:), intent(in) :: array
    integer(kind=1), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_i8_1

  integer function findloc1_i8_2(array, value, mask, back) result(iloc)
    integer(kind=8), dimension(:), intent(in) :: array
    integer(kind=2), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_i8_2

  integer function findloc1_i8_4(array, value, mask, back) result(iloc)
    integer(kind=8), dimension(:), intent(in) :: array
    integer(kind=4), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_i8_4

  integer function findloc1_i8(array, value, mask, back) result(iloc)
    integer(kind=8), dimension(:), intent(in) :: array
    integer(kind=8), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_i8

  integer function findloc1_r4(array, value, mask, back) result(iloc)
    real(kind=4), dimension(:), intent(in) :: array
    real(kind=4), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_r4

  integer function findloc1_r4_8(array, value, mask, back) result(iloc)
    real(kind=4), dimension(:), intent(in) :: array
    real(kind=8), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_r4_8

  integer function findloc1_r8_4(array, value, mask, back) result(iloc)
    real(kind=8), dimension(:), intent(in) :: array
    real(kind=4), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_r8_4

  integer function findloc1_r8(array, value, mask, back) result(iloc)
    real(kind=8), dimension(:), intent(in) :: array
    real(kind=8), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_r8

  integer function findloc1_r16_4(array, value, mask, back) result(iloc)
    real(kind=16), dimension(:), intent(in) :: array
    real(kind=4), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_r16_4

  integer function findloc1_r16_8(array, value, mask, back) result(iloc)
    real(kind=16), dimension(:), intent(in) :: array
    real(kind=8), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_r16_8

  integer function findloc1_r16(array, value, mask, back) result(iloc)
    real(kind=16), dimension(:), intent(in) :: array
    real(kind=16), intent(in) :: value
    logical, dimension(size(array)), intent(in), optional :: mask
    logical, intent(in), optional :: back

    integer, dimension(1) :: ilocs

    iloc = -999

    if (present(mask) .and. present(back)) then
      ilocs  = findloc(array, value, MASK=mask, BACK=back)
    else if (present(mask)) then
      ilocs  = findloc(array, value, MASK=mask)
    else if (present(back)) then
      ilocs  = findloc(array, value, BACK=back)
    else
      ilocs  = findloc(array, value)
    end if

    iloc = ilocs(1)
  end function findloc1_r16

end module alpin_misc_utils

