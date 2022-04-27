!> \file mo_wqm_mpr.f90

!> \brief Upscale land-use dependent parameters to model levels

!> \details 

!> \author Xiaoqiang Yang
!> \date Jul 2017 

MODULE mo_wqm_mpr

 
  USE mo_kind, ONLY: i4, sp, dp


  IMPLICIT NONE

 
  PUBLIC :: wqm_mpr  ! parameters of water quality model


CONTAINS

  ! ------------------------------------------------------------------

  !     NAME
  !         wqm_mpr

  !     PURPOSE
  !>        \brief Water quality parameters 

  !>        \details Water quality parameters are previously read-in from "mhm_parameter.nml" and stored in "global_parameter".
  !>        This module assigns those values to parameter variables in each L1 cell. \n
  !>        Notes: WQ parameter regionalisation has not been developed yet. Same value is assigned to all L1 cells. 
  !>               Currently, we take the five soil phase parameters as land-use dependent parameter, and the land use 
  !>               differences are read-in as input initial values (see mo_wqm_read::wqm_readdata and "initial_value.txt").
  !     CALLING SEQUENCE
  !         

  !     INTENT(IN)
  !>        \param[in] "integer(i4), dimension(:,:)        :: procMat"     process setting dim1=11 for water quality
  !>        \param[in] "real(dp), dimension(:)             :: param"       global parameters from parameter namelist        

  !     INTENT(INOUT)
  !         None

  !     INTENT(OUT)
  !>        \param[out] "real(dp), dimension(:)             :: degradN_rate"   parameter 
  !>        \param[out] "real(dp), dimension(:)             :: mineraN_rate"   parameter   
  !>        \param[out] "real(dp), dimension(:)             :: dissolN_rate"   parameter   
  !>        \param[out] "real(dp), dimension(:)             :: sdenitr_rate"   parameter   
  !>        \param[out] "real(dp), dimension(:)             :: gwresid_time"   parameter  
  !>        \param[out] "real(dp), dimension(:)             :: adenitr_rate"   parameter   
  !>        \param[out] "real(dp), dimension(:)             :: priprod_rate"   parameter   

  !     INTENT(IN), OPTIONAL
  !         None

  !     INTENT(INOUT), OPTIONAL
  !         None

  !     INTENT(OUT), OPTIONAL
  !         None

  !     RETURN
  !>       

  !     RESTRICTIONS
  !         None

  !     EXAMPLE
  !         
  !         

  !     LITERATURE
  !         None

  !     HISTORY
  !>        \author Xiaoqiang Yang 
  !>        \date Jun 2017

  subroutine wqm_mpr( ncells1, nNodes, L1id_on_L11, L11id_on_L1, map_flag, procMat,param,parameterset_SSA, &
                     fLAI, fLAI_11, degradN_rate, &
                     mineraN_rate, dissolN_rate, sdenitr_rate, adenitr_rate, priprod_rate, nLink_from, &
                     areaCell1, areaCell11 )

  use mo_message,   only: message
  implicit none
  integer(i4),                       intent(in)     :: ncells1      ! number of cells at model level (L1)
  integer(i4),                       intent(in)     :: nNodes       ! number of cells at routing level (L11)
  integer(i4), dimension(:),         intent(in)     :: L1id_on_L11  ! L1 cell id on L11 level
  integer(i4), dimension(:),         intent(in)     :: L11id_on_L1  ! L11 cell id on L1 level
  logical,                           intent(in)     :: map_flag     ! L11 is bigger than L1 or not
  integer(i4), dimension(:,:),       intent(in)     :: procMat      ! process setting dim1=11 for water quality
  real(dp), dimension(:),            intent(in)     :: param        ! global parameters from parameter namelist
  real(dp), dimension(:),            intent(in)     :: parameterset_SSA        
  real(dp), dimension(:,:),          intent(in)     :: fLAI         ! area fraction of each landuse type
  real(dp), dimension(:,:),          intent(inout)  :: fLAI_11      ! area fraction of each landuse type at L11
  real(dp), dimension(:),            intent(out)    :: degradN_rate ! parameter 
  real(dp), dimension(:),            intent(out)    :: mineraN_rate ! parameter   
  real(dp), dimension(:),            intent(out)    :: dissolN_rate ! parameter   
  real(dp), dimension(:),            intent(out)    :: sdenitr_rate ! parameter    
  real(dp), dimension(:),            intent(out)    :: adenitr_rate ! parameter   
  real(dp), dimension(:),            intent(out)    :: priprod_rate ! parameter
  integer(i4), dimension(:),         intent(in)     :: nLink_from   ! id of Node where current reach connects from
  real(dp), dimension(:),            intent(in)     :: areaCell1    ! cell area at L1
  real(dp), dimension(:),            intent(in)     :: areaCell11   ! cell area at 	L11

  !local
  integer(i4)    :: istart, iend
  integer(i4)    :: k, nn, mm, ii
  !integer(i4), dimension(size(nLink_from))  :: locating  
  !real(dp), dimension(nNodes, size(fLAI,2))  :: tmp_fLAI_11   ! area fraction of different land use at L11, temporal variable


  !land use dependent parameters in soil phase
  do k=1, ncells1
  do nn=1, size(fLAI,2)  
     if (fLAI(k,nn) .gt. 0.0_dp) then
     degradN_rate(k) = degradN_rate(k) + fLAI(k,nn) * param(istart + 7 + nn)
     dissolN_rate(k) = dissolN_rate(k) + fLAI(k,nn) * param(istart + 21 + nn)
     end if
  end do
  end do



  ! in-stream phase, at L11 level
  do k =1, nNodes
     priprod_rate(k) = parameterset_SSA(k + 370)
     adenitr_rate(k) = parameterset_SSA(k)
  end do
    
  
  ! soil phase, at L1 level
  do k = 1, ncells1 
     mineraN_rate(k) = parameterset_SSA(k + 555) 
     sdenitr_rate(k) = parameterset_SSA(k + 185)  
  end do


  end subroutine wqm_mpr
END MODULE mo_wqm_mpr
