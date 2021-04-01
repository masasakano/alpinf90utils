! Subroutines for exit due to an error
!
module alpin_err_exit
  use iso_fortran_env, only : STDERR=>ERROR_UNIT
  implicit none 

contains
  ! exit(1)
  subroutine err_exit_with_msg(usermsg)
    implicit none
    character(len=*), parameter :: Prefix = 'FATAL: '
    character(len=*), intent(in) :: usermsg  ! User Error message (mandatory).

    write(STDERR,'(a, a)') Prefix, trim(usermsg)
    call EXIT(1)  ! for gfortran, Lahey Fujitsu Fortran 95, etc
    ! stop 1      ! redundant. Anyway not yet standard? (works for gfortran?)
    return ! redundant
  end subroutine err_exit_with_msg

  ! exit(1), playing safe
  subroutine err_exit_play_safe(usermsg)
    implicit none
    character(len=*), parameter :: Def_msg = 'Should not happen. Contact the code developer.'
    character(len=*), intent(in), optional :: usermsg  ! optional additional User Error message.

    if (present(usermsg)) then
      call err_exit_with_msg(Def_msg//' '//trim(usermsg))
    else
      call err_exit_with_msg(Def_msg)
    end if
    return ! redundant
  end subroutine err_exit_play_safe
end module alpin_err_exit

