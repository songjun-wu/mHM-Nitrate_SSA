!> \file mo_sa_evaluation.f90

!> \brief 

!> \details 

!> \authors 
!> \date

MODULE mo_sa_evaluation

  USE mo_kind, ONLY: i4, dp

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: sa_evaluation
  PUBLIC :: sa_read_inputdata
  PUBLIC :: sa_extract_runoff
  PUBLIC :: sa_extract_nitrate

  ! ------------------------------------------------------------------

CONTAINS

  ! ------------------------------------------------------------------

  !      NAME
  !          sa_evaluation

  !>        \brief 

  !>        \details 

  !     INTENT(IN)
  !> 

  !     INTENT(INOUT)
  !         None

  !     INTENT(OUT)
  !         None

  !     INTENT(IN), OPTIONAL
  !         None

  !     INTENT(INOUT), OPTIONAL
  !         None

  !     INTENT(OUT), OPTIONAL
  !
  !     RETURN
  !         None

  !     RESTRICTIONS
  !         None

  !     EXAMPLE
  !         None

  !     LITERATURE
  !         None

  !     HISTORY
  !>        \author 
  !>        \date
  !         Modified

  SUBROUTINE sa_evaluation(global_param, paramid, param_value, sa_param)

!    use mo_init_states,         only : get_basin_info
!    use mo_init_states,         only : variables_default_init   ! default initalization of variables
!    use mo_julian,              only : caldat, julday
!    use mo_message,             only : message
!    use mo_mhm,                 only : mhm
    use mo_mrm_constants,        only : nodata_dp
    use mo_kind,                 only : i4, dp
    implicit none
  real(dp), dimension(:),      intent(in)    :: global_param   !global parmeters from input file: mhm_parameter.nml
    integer(i4), dimension(:),   intent(in)    :: paramid        !id of parameters that will be replaced by sa sampler 
    real(dp), dimension(:),      intent(in)    :: param_value    !param value of a sepcific sampler
    real(dp), dimension(:),      intent(inout) :: sa_param       ! new parameter set

    !local
    integer(i4)  :: ii

  
    !sa_param = nodata_dp 
    sa_param(1:size(global_param)) = global_param(:) 
    sa_param(paramid(:)) = param_value(:)
!  write(334,*) size(param_value), size(sa_param)
!  write(334,*) global_param
!  write(334,*) sa_param

!  if (any(sa_param .eq. nodata_dp)) then
!  print*, 'ERROR: None value existed in parameters'
!  stop
!  end if

  END SUBROUTINE sa_evaluation  
!----------------------------------------------------------
!---------------------------------------------------------- 
  SUBROUTINE sa_read_inputdata(global_param, nosamplers, param_value)
    USE mo_message,             ONLY : message
  USE mo_string_utils,        ONLY : num2str
    implicit none
    integer(i4),                 intent(inout) :: nosamplers
    real(dp), dimension(:),      intent(in)    :: global_param
    !integer(i4), dimension(:),  allocatable,    intent(inout) :: paramid
    real(dp),    dimension(:,:),allocatable,    intent(inout) :: param_value
    !real(dp),    dimension(:),  allocatable,    intent(inout) :: sa_parameters
    !local
    character(256)                :: fName,dummy
    integer(i4),  parameter       :: funit = 90  
    integer(i4)                   :: nTotalparam  !number of total model parameters       
    integer(i4)                   :: noparams  !number of parameters involved    
    integer(i4)                   :: i,j, idx, s
    !read input parameter values generated from sampling method
    ! "ASCII" files
  ! fName = "sa_samplers.txt"
    ! open(funit, file=fName, action='read')
    ! read(funit,*) dummy, nosamplers
    ! read(funit,*) dummy, noparams
    ! if (.not. allocated(param_value)) allocate(param_value(nosamplers, noparams) )
    ! !if (.not. allocated(paramid))    allocate(paramid(noparams))
    ! !read(funit,*) (paramid(j), j=1,noparams)
    ! do i=1, nosamplers
       ! read(funit,*) (param_value(i,j), j=1,noparams)
    ! end do
      
    ! close(funit)
    ! "Binary" files
    fName = "sa_input.bin"
    !should be specified for each SA if changed
  noparams = 1680_i4
  nosamplers = 134480_i4    ! r=5
  if (.not. allocated(param_value)) allocate(param_value(noparams + 50, nosamplers) )
    open(unit=funit, file=trim(fName), &
        form='unformatted', access='stream')
    !do idx = 1, 2!nosamplers
       do s = 1, nosamplers
         param_value(1:50,s) = global_param(1:50)
       end do
       read(funit) ((param_value(j + 50,idx), j = 1,noparams), idx = 1,nosamplers)
     !call message('No of samplers:',trim(num2str(param_value(1:10,idx))))
    !end do
    close(funit)
    !read initial values for all parameters
    ! fName = "sa_initialvalue.txt"
    ! open(funit, file=fName, action='read')
    ! read(funit,*) dummy, nTotalparam   !number of total parameters
    ! if (.not. allocated(sa_parameters))    allocate(sa_parameters(nTotalparam)) 
    ! do i=1, nTotalparam
    ! read(funit,*) idx, sa_parameters(i)
    ! end do
    ! close(funit)
  
  END SUBROUTINE sa_read_inputdata
 
  ! ------------------------------------------------------------------

  ! NAME
  !         extract_runoff

  !>        \brief extracts runoff data from global variables

  !>        \details extracts simulated and measured runoff from global variables,
  !>                 such that they overlay exactly. For measured runoff, only the runoff
  !>                 during the evaluation period are cut, not succeeding nodata values.
  !>                 For simulated runoff, warming days as well as succeeding nodata values
  !>                 are neglected and the simulated runoff is aggregated to the resolution
  !>                 of the observed runoff.\n

  !     INTENT(IN)
  !>        \param[in] "integer(i4) :: gaugeID"   - ID of the current gauge to process
  !>        \param[in] "real(dp)    :: runoff(:)" - simulated runoff at this gauge

  !     INTENT(INOUT)
  !         None

  !     INTENT(OUT)
  !>        \param[out] "real(dp)   :: runoff_agg(:)"      - aggregated simulated runoff at this gauge\n
  !>        \param[out] "real(dp)   :: runoff_obs(:)"      - extracted observed runoff\n
  !>        \param[out] "logical    :: runoff_obs_mask(:)" - masking non-negative values in runoff_obs\n

  !     INTENT(IN), OPTIONAL
  !         None

  !     INTENT(INOUT), OPTIONAL
  !         None

  !     INTENT(OUT), OPTIONAL
  !         None

  !     RETURN
  !         None

  !     RESTRICTIONS
  !         None

  !     EXAMPLE
  !         see use in this module above

  !     LITERATURE
  !         None

  !     HISTORY
  !>        \author Stephan Thober
  !>        \date Jan 2015

  ! ------------------------------------------------------------------
  subroutine sa_extract_runoff( gaugeId, runoff, runoff_agg, runoff_obs, runoff_obs_mask )

    use mo_mrm_global_variables, only: gauge, nMeasPerDay, evalPer, warmingDays_mrm, nTstepDay
    use mo_message,              only: message
    use mo_utils,                only: ge

    implicit none

    ! input variables
    integer(i4),               intent(in) :: gaugeId      ! current gauge Id
    real(dp),  dimension(:,:), intent(in) :: runoff       ! simulated runoff

    ! output variables
    real(dp), dimension(:), allocatable, intent(out) :: runoff_agg      ! aggregated simulated
    ! runoff to the resolution
    ! of the measurement
    real(dp), dimension(:), allocatable, intent(out) :: runoff_obs      ! extracted measured 
    ! runoff to exactly the
    ! evaluation period
    logical,  dimension(:), allocatable, intent(out) :: runoff_obs_mask ! mask of no data values
    ! in runoff_obs

    ! local variables
    integer(i4)                         :: iBasin  ! basin id
    integer(i4)                         :: tt      ! timestep counter
    integer(i4)                         :: length  ! length of extracted time series
    integer(i4)                         :: factor  ! between simulated and measured time scale
    integer(i4)                         :: TPD_sim ! simulated Timesteps per Day
    integer(i4)                         :: TPD_obs ! observed Timesteps per Day
    real(dp), dimension(:), allocatable :: dummy

    ! copy time resolution to local variables
    TPD_sim = nTstepDay
    TPD_obs = nMeasPerDay

    ! check if modelled timestep is an integer multiple of measured timesteps
    if ( modulo( TPD_sim, TPD_obs) .eq. 0 ) then
       factor = TPD_sim / TPD_obs
    else
       call message(' Error: Number of modelled datapoints is no multiple of measured datapoints per day')
       stop
    end if

    ! extract basin Id from gauge Id
    iBasin = gauge%basinId( gaugeId )

    ! get length of evaluation period times TPD_obs
    length = ( evalPer( iBasin )%julEnd - evalPer( iBasin )%julStart + 1 ) * TPD_obs

    ! extract measurements
    if ( allocated( runoff_obs ) ) deallocate( runoff_obs )
    allocate( runoff_obs( length ) )
    runoff_obs = gauge%Q( 1 : length, gaugeId )

    ! create mask of observed runoff
    if ( allocated( runoff_obs_mask ) ) deallocate( runoff_obs_mask )
    allocate( runoff_obs_mask( length ) )
    runoff_obs_mask = .false.
    forall(tt=1:length) runoff_obs_mask(tt) = ge( runoff_obs(tt), 0.0_dp)    

    ! extract and aggregate simulated runoff
    if ( allocated( runoff_agg ) ) deallocate( runoff_agg )
    allocate( runoff_agg( length ) )
    ! remove warming days
    length = ( evalPer( iBasin )%julEnd - evalPer( iBasin )%julStart + 1 ) * TPD_sim
    allocate( dummy( length ) )
    dummy = runoff( warmingDays_mrm(iBasin)*TPD_sim + 1:warmingDays_mrm(iBasin)*TPD_sim + length, gaugeId )
    ! aggregate runoff
    length = ( evalPer( iBasin )%julEnd - evalPer( iBasin )%julStart + 1 ) * TPD_obs
    forall(tt=1:length) runoff_agg(tt) = sum( dummy( (tt-1)*factor+1: tt*factor ) ) / &
         real(factor,dp)
    ! clean up
    deallocate( dummy )

  end subroutine sa_extract_runoff

  ! ------------------------------------------------------------------

  ! NAME
  !         extract_nitrate


  !     HISTORY
  !>        \author Xiaoqiang Yang (after Stephan)
  !>        \date Sep 2016

  ! ------------------------------------------------------------------
  subroutine sa_extract_nitrate( gaugeId, nitrate, nitrate_agg, nitrate_obs, nitrate_obs_mask )

    use mo_mrm_global_variables, only: gauge, nMeasPerDay, evalPer, warmingDays_mrm, nTstepDay
    use mo_wqm_global_variables, only: nEvalCmeasPerday, basin_wqm
    use mo_message,              only: message
    use mo_utils,                only: ge, ne
    use mo_mrm_constants,        only: nodata_dp

    implicit none

    ! input variables
    integer(i4),               intent(in) :: gaugeId      ! current gauge Id
    real(dp),  dimension(:,:), intent(in) :: nitrate       ! simulated nitrate

    ! output variables
    real(dp), dimension(:), allocatable, intent(out) :: nitrate_agg      ! aggregated simulated
    ! nitrate to the resolution
    ! of the measurement
    real(dp), dimension(:), allocatable, intent(out) :: nitrate_obs      ! extracted measured 
    ! nitrate to exactly the
    ! evaluation period
    logical,  dimension(:), allocatable, intent(out) :: nitrate_obs_mask ! mask of no data values
    ! in nitrate_obs

    ! local variables
    integer(i4)                         :: iBasin  ! basin id
    integer(i4)                         :: tt      ! timestep counter
    integer(i4)                         :: length  ! length of extracted time series
    integer(i4)                         :: factor  ! between simulated and measured time scale
    integer(i4)                         :: TPD_sim ! simulated Timesteps per Day
    integer(i4)                         :: TPD_obs ! observed Timesteps per Day
    real(dp), dimension(:), allocatable :: dummy

    ! copy time resolution to local variables
    TPD_sim = nTstepDay
    TPD_obs = nEvalCmeasPerday(gaugeId)

    ! check if modelled timestep is an integer multiple of measured timesteps
    if ( modulo( TPD_sim, TPD_obs) .eq. 0 ) then
       factor = TPD_sim / TPD_obs
    else
       call message(' Error: Number of modelled datapoints is no multiple of measured datapoints per day')
       stop
    end if

    ! extract basin Id from gauge Id
    iBasin = gauge%basinId( gaugeId )

    ! get length of evaluation period times TPD_obs
    length = ( evalPer( iBasin )%julEnd - evalPer( iBasin )%julStart + 1 ) * TPD_obs

    ! extract measurements
    if ( allocated( nitrate_obs ) ) deallocate( nitrate_obs )
    allocate( nitrate_obs( length ) )
    nitrate_obs = basin_wqm%GaugeConc( 1 : length, gaugeId, 1 )

    ! create mask of observed nitrate
    if ( allocated( nitrate_obs_mask ) ) deallocate( nitrate_obs_mask )
    allocate( nitrate_obs_mask( length ) )
    nitrate_obs_mask = .false.
    forall(tt=1:length) nitrate_obs_mask(tt) = ne( nitrate_obs(tt), nodata_dp)    

    ! extract and aggregate simulated nitrate
    if ( allocated( nitrate_agg ) ) deallocate( nitrate_agg )
    allocate( nitrate_agg( length ) )
    ! remove warming days
    length = ( evalPer( iBasin )%julEnd - evalPer( iBasin )%julStart + 1 ) * TPD_sim
    allocate( dummy( length ) )
    dummy = nitrate( warmingDays_mrm(iBasin)*TPD_sim + 1:warmingDays_mrm(iBasin)*TPD_sim + length, gaugeId)
    ! aggregate nitrate
    length = ( evalPer( iBasin )%julEnd - evalPer( iBasin )%julStart + 1 ) * TPD_obs
    forall(tt=1:length) nitrate_agg(tt) = sum( dummy( (tt-1)*factor+1: tt*factor ) ) / &
         real(factor,dp)
    ! clean up
    deallocate( dummy )

  end subroutine sa_extract_nitrate

end module mo_sa_evaluation  
