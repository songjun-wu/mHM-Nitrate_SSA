!> \file mo_sa_evaluation.f90

!> \brief 

!> \details 

!> \authors 
!> \date

MODULE mo_error_songjun

  USE mo_kind, ONLY: i4, dp

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: RMSE_songjun
  PUBLIC :: bias_songjun


  ! ------------------------------------------------------------------

CONTAINS

  FUNCTION RMSE_songjun(x, y)
    REAL(dp), DIMENSION(:),           INTENT(IN)      :: x, y
    REAL(dp)                                          :: RMSE_songjun

    RMSE_songjun = sqrt(sum((y - x)**2_i4)/size(x))
  END FUNCTION RMSE_songjun

  FUNCTION bias_songjun(x, y)
    REAL(dp), DIMENSION(:),           INTENT(IN)      :: x, y
    REAL(dp)                                          :: bias_songjun
    
    bias_songjun = 100.0_dp*(sum(y) - sum(x))/sum(x)
  END FUNCTION bias_songjun

end module mo_error_songjun  
