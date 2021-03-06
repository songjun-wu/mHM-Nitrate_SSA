!> \file mo_wqm_read.f90

!> \brief Reads water quality configure, input files and allocating WQ global variables.

!> \details This module is: firstly to read water qualtiy input data, including initial 
!>          pool values, cropdata, agricultural management and crop rotation classes 
!>          and spatial distribution; secondly to allocate global variables for water quality modeling.\n
!> 

!> \authors Xiaoqiang Yang
!> \date Sep 2017

MODULE mo_wqm_read


  USE mo_kind, ONLY: i4, sp, dp

  
  IMPLICIT NONE

  PUBLIC :: wqm_readconfig               ! read water qualtiy configure 
  PUBLIC :: wqm_readinputdata            ! read water quality input data
  PUBLIC :: wqm_variables_initalloc      ! initial and allocate variables of water quality model
  PUBLIC :: wqm_readobsdata              ! read observed water quality data (conc. at gauging stations)
  PUBLIC :: wqm_variables_default_init   ! default initial value for all variables especially for continuous running 

  ! ------------------------------------------------------------------

CONTAINS

  ! ------------------------------------------------------------------

  !     NAME
  !         wqm_readobsdata

  !     PURPOSE
  !>        \brief .

  !>        \details 

  !     CALLING SEQUENCE
  !         

  !     INTENT(IN)
  !>        None

  !     INTENT(INOUT)
  !         None

  !     INTENT(OUT)
  !         None

  !     INTENT(IN), OPTIONAL
  !         None

  !     INTENT(INOUT), OPTIONAL
  !         None

  !     INTENT(OUT), OPTIONAL
  !         None

  !     RETURN
  !>        None

  !     RESTRICTIONS
  !         None

  !     EXAMPLE
  !         None


  !     LITERATURE
  !         None

  !     HISTORY
  !>        \author Xiaoqiang Yang
  !>        \date Jul 2016

  subroutine wqm_readobsdata()
  
    use mo_mrm_global_variables,    only: &
         nBasins, nGaugesTotal, gauge,    &
         nInflowGaugesTotal, InflowGauge, &
         evalPer, simPer, nTstepDay      !&
    use mo_wqm_global_variables,    only: &
         basin_wqm, nEvalCmeasPerday,nAddinCmeasPerday, maxcols, &
         numEvalCol, numAddinCol,  &    !evalHead_str, inflowHead_str,
         WQM_nutrient
    use mo_common_variables,        only: &
         optimize, opti_function
    use mo_mrm_constants,           only: nodata_dp
    use mo_wqm_readtimeseries,      only: read_timeseries_conc
    use mo_wqm_paste,               only: paste_conc
	
    implicit none
  
    integer(i4)                  :: maxtimestep,i   !d1,d2,d3, 
    integer(i4)                  :: iGauge, iBasin
    integer(i4), dimension(3)    :: start_tmp, end_tmp
    character(256)               :: fName
    real(dp), dimension(:,:), allocatable  :: data_dp_conc   !dim1= number of observations, dim2= "maxcols"
    logical,  dimension(:,:), allocatable  :: mask_conc
	!file heading
!    integer(i4)                            :: numScol        ! number of wq data columns in gauge file
    character(256), dimension(:), allocatable :: head_str    ! name of each wq data column (heading string)
    character(256), dimension(:), allocatable :: Inhead_str    ! name of each wq data column (heading string)
    !allocate
    if (.not. allocated(numEvalCol))      allocate(numEvalCol(nGaugesTotal) )
!    if (.not. allocated(evalHead_str))     allocate(evalHead_str(nGaugesTotal, maxcols)) 
    if (.not. allocated(nEvalCmeasPerday)) allocate(nEvalCmeasPerday(nGaugesTotal) )
!---------------------------------------

	
    do iGauge = 1, nGaugesTotal
       ! get basin id
       iBasin = gauge%basinId(iGauge)
       ! get start and end dates
       start_tmp = (/evalPer(iBasin)%yStart, evalPer(iBasin)%mStart, evalPer(iBasin)%dStart/)
       end_tmp   = (/evalPer(iBasin)%yEnd,   evalPer(iBasin)%mEnd,   evalPer(iBasin)%dEnd  /)
       ! evaluation gauge
       fName = trim(adjustl(basin_wqm%fname(iGauge)))
       call read_timeseries_conc(trim(fName), 89, &
            start_tmp, end_tmp, optimize, opti_function, maxcols, numEvalCol(iGauge), head_str,&
            data_dp_conc, mask=mask_conc, nCmeasPerday=nEvalCmeasPerday(iGauge))
       data_dp_conc = merge(data_dp_conc, nodata_dp, mask_conc)


       !post-process of evaluation wq gauge data 
       !if (numEvalCol(iGauge).eq. 2) then   !only two columns in gauging data file
       !   if (all(data_dp_conc(:,1) .eq. nodata_dp)) then              !if "IN" is missing
       !      data_dp_conc(:,1) = data_dp_conc(:,3) - data_dp_conc(:,2)               
       !   elseif (all(data_dp_conc(:,2) .eq. nodata_dp)) then          !if "ON" is missing
       !      data_dp_conc(:,2) = data_dp_conc(:,3) - data_dp_conc(:,1)
       !   else                                                         !if "TN" is missing
       !      data_dp_conc(:,3) = data_dp_conc(:,1) + data_dp_conc(:,2)
       !   end if				
       !end if
       call paste_conc(basin_wqm%GaugeConc, data_dp_conc, nodata_dp )
       
       deallocate (data_dp_conc)       
    end do
    !*******************************************************************
    !allocate variable for simulated value (initialise "WQM_nutrient")
!    d1 = size(basin_wqm%GaugeConc,1)
!    d2 = size(basin_wqm%GaugeConc,2)
!    d3 = size(basin_wqm%GaugeConc,3)
    maxtimestep = maxval(simPer(1:nBasins)%julEnd - simPer(1:nBasins)%julStart + 1 ) * nTstepDay
    allocate(WQM_nutrient(maxtimestep,nGaugesTotal,maxcols)) 
    WQM_nutrient = nodata_dp
    !*******************************************************************
    ! additional inflow gauge
    ! upstream inflow and sewage plants input
    ! in mhm call InflowGauge%Q has to be initialized -- dummy allocation with period of basin 1 and initialization
    if (nInflowGaugesTotal .EQ. 0) then
       allocate( data_dp_conc( maxval( simPer(:)%julEnd  - simPer(:)%julStart + 1 ), maxcols ) )
       data_dp_conc = nodata_dp
       call paste_conc(basin_wqm%InflowGaugeConc, data_dp_conc, nodata_dp)
    else
    ! allocate
    if (.not. allocated(numAddinCol))   allocate(numAddinCol(nInflowGaugesTotal) )
    !if (.not. allocated(inflowHead_str))  allocate(inflowHead_str(nInflowGaugesTotal, maxcols)) 
    if (.not. allocated(nAddinCmeasPerday)) allocate(nAddinCmeasPerday(nInflowGaugesTotal) )

    do iGauge = 1, nInflowGaugesTotal
          ! get basin id
          iBasin = InflowGauge%basinId(iGauge)
          ! get start and end dates
          start_tmp = (/simPer(iBasin)%yStart, simPer(iBasin)%mStart, simPer(iBasin)%dStart/)
          end_tmp   = (/simPer(iBasin)%yEnd,   simPer(iBasin)%mEnd,   simPer(iBasin)%dEnd  /)
          ! inflow gauge
          fName = trim(adjustl(basin_wqm%Inflowfname(iGauge)))
          call read_timeseries_conc(trim(fName), 89, &
               start_tmp, end_tmp, optimize, opti_function, maxcols, numAddinCol(iGauge), Inhead_str,&
               data_dp_conc, mask=mask_conc, nCmeasPerday=nAddinCmeasPerday(iGauge) )
          data_dp_conc = merge(data_dp_conc, nodata_dp, mask_conc)

          !!post-process of inflow wq gauge data 
          !only two columns in inflow data file and these can only be IN and TN
          if (numAddinCol(iGauge).eq. 2) then   
             do i=1,size(data_dp_conc,1)
                if (mask_conc(i,1) .and. mask_conc(i,3)) then   !if measured IN and TN both have value
                   data_dp_conc(:,2) = data_dp_conc(:,3) - data_dp_conc(:,1)
                elseif (.not. mask_conc(i,1) .and. mask_conc(i,3)) then ! if only measured TN has value                      
                   data_dp_conc(i,1) = 0.8_dp * data_dp_conc(i,3)
                   data_dp_conc(i,2) = 0.2_dp * data_dp_conc(i,3)
                elseif (mask_conc(i,1) .and. (.not. mask_conc(i,3))) then !if only measured IN has value
                   data_dp_conc(i,2) = 0.25_dp * data_dp_conc(i,1)
                end if    
             end do
          end if
 
          !take care of a specific condition: only "TN" or "IN" was provided in inflow data
          if (numAddinCol(iGauge).eq. 1) then 
             if(any(mask_conc(:,3))) then
             data_dp_conc(:,1) = 0.8_dp * data_dp_conc(:,3)
             data_dp_conc(:,2) = 0.2_dp * data_dp_conc(:,3)
             !!NOTICE INFORMATION
             !print*, 'WARNING: Only total nitrogen data was provided at inflow gauges in basin (',iBasin,')! '
			 !print*, 'WARNING: 80% of TN was given to IN by default'
             else
             data_dp_conc(:,2) = 0.25_dp * data_dp_conc(:,1)
             !print*, 'WARNING: Only Inorganic nitrogen data was provided at inflow gauges in basin (',iBasin,')! '
			 !print*, 'WARNING: ON was given as 25% of IN by default'
             end if
          end if
		  
          call paste_conc(basin_wqm%InflowGaugeConc, data_dp_conc, nodata_dp)
          deallocate (data_dp_conc)
       end do
    end if

  end subroutine wqm_readobsdata
  ! ------------------------------------------------------------------

  !     NAME
  !         wqm_readconfig

  !     PURPOSE
  !>        \brief .

  !>        \details 

  !     CALLING SEQUENCE
  !         

  !     INTENT(IN)
  !>        None

  !     INTENT(INOUT)
  !         None

  !     INTENT(OUT)
  !         None

  !     INTENT(IN), OPTIONAL
  !         None

  !     INTENT(INOUT), OPTIONAL
  !         None

  !     INTENT(OUT), OPTIONAL
  !         None

  !     RETURN
  !>        None

  !     RESTRICTIONS
  !         None

  !     EXAMPLE
  !         None


  !     LITERATURE
  !         None

  !     HISTORY
  !>        \author Xiaoqiang Yang
  !>        \date Jul 2016

  subroutine wqm_readconfig()

    use mo_nml,                    only: open_nml, position_nml, close_nml
    use mo_wqm_global_variables,   only: &
         dirInputWQM, dirGaugesWQM,    &
         basin_wqm,   &
         outputFlxState_wqm, timeStep_model_outputs_wqm, nOUTstate_wqm
       !...
    use mo_global_variables,       only: &
         processMatrix

  use mo_common_variables,       only: &
       global_parameters,      &
       global_parameters_name

  use mo_mrm_global_variables,   only: basin_mrm, &
       nBasins, nGaugesTotal, nInflowGaugesTotal
       !...
  use mo_mrm_constants,          only: nodata_i4, maxNoGauges, maxNoBasins, &
       nColPars
  use mo_string_utils,           only: num2str
  use mo_message,                only: message 
  use mo_append,                 only: append
	   
  !local
  character(256)            :: file_namelist_wqm, file_param_wqm, file_stateoutput_wqm
  character(256), dimension(maxNoBasins)  ::dir_inputdata_wqm, dir_gauges_wqm
  character(256), dimension(maxNoBasins, maxNoGauges) :: wqm_evalgauge_filename, wqm_inflowgauge_filename
  integer(i4)    :: idx,iBasin,iGauge
  logical        :: fexist
  !parameter name (for orginal running)
  !real(dp), dimension(nColPars)   :: degradationN_rate         !rate for transformation from humus pool to fast pool
  !real(dp), dimension(nColPars)   :: degradationN_rate_agri    !rate for transformation from humus pool to fast pool  
  !real(dp), dimension(nColPars)   :: mineralisationN_rate        ![/d]rate for transformation from fast pool to inorgainc pool
  !real(dp), dimension(nColPars)   :: mineralisationN_rate_agri   ![/d]rate for transformation from fast pool to inorgainc pool   
  !real(dp), dimension(nColPars)   :: denitrification_soil        ![/d]IN denitrification rate in soils
  !real(dp), dimension(nColPars)   :: denitrification_agrisoil    ![/d]IN denitrification rate in agricultural soils
  !real(dp), dimension(nColPars)   :: dissolutionN_rate          ![mm/m]balance between fast pool and soil water orgainc pool
  !real(dp), dimension(nColPars)   :: dissolutionN_rate_agri     ![mm/m]balance between fast pool and soil water orgainc pool
  !real(dp), dimension(nColPars)   :: groundwater_residtime !reflecting resident time for calculating groundwater conc.
                                                           !(equation from INCA model)
  !real(dp), dimension(nColPars)   :: primaryprod_rate          ![kg.m^2/d]primary production in aquatic system (nuptake)
  !real(dp), dimension(nColPars)   :: primaryprod_rate_agri     ![kg.m^2/d]primary production in aquatic system (nuptake)
  !real(dp), dimension(nColPars)   :: denitrification_aquatic     ![kg.m^3/d]IN denitrification rate in aquatic system

  !**************************************************************
  !parameter name (for SA running, 86 parameters in total)
  !**************************************************************
  real(dp), dimension(nColPars)   :: denitrification_aquatic
  real(dp), dimension(nColPars)   :: primaryprod_rate1
  real(dp), dimension(nColPars)   :: primaryprod_rate2
  real(dp), dimension(nColPars)   :: primaryprod_rate3
  real(dp), dimension(nColPars)   :: primaryprod_rate4
  real(dp), dimension(nColPars)   :: primaryprod_rate5
  real(dp), dimension(nColPars)   :: primaryprod_rate6
  real(dp), dimension(nColPars)   :: primaryprod_rate7
  real(dp), dimension(nColPars)   :: degradationN_rate1
  real(dp), dimension(nColPars)   :: degradationN_rate2
  real(dp), dimension(nColPars)   :: degradationN_rate3
  real(dp), dimension(nColPars)   :: degradationN_rate4
  real(dp), dimension(nColPars)   :: degradationN_rate5
  real(dp), dimension(nColPars)   :: degradationN_rate6
  real(dp), dimension(nColPars)   :: degradationN_rate7
  real(dp), dimension(nColPars)   :: mineralisationN_rate1
  real(dp), dimension(nColPars)   :: mineralisationN_rate2
  real(dp), dimension(nColPars)   :: mineralisationN_rate3
  real(dp), dimension(nColPars)   :: mineralisationN_rate4
  real(dp), dimension(nColPars)   :: mineralisationN_rate5
  real(dp), dimension(nColPars)   :: mineralisationN_rate6
  real(dp), dimension(nColPars)   :: mineralisationN_rate7
  real(dp), dimension(nColPars)   :: dissolutionN_rate1
  real(dp), dimension(nColPars)   :: dissolutionN_rate2
  real(dp), dimension(nColPars)   :: dissolutionN_rate3
  real(dp), dimension(nColPars)   :: dissolutionN_rate4
  real(dp), dimension(nColPars)   :: dissolutionN_rate5
  real(dp), dimension(nColPars)   :: dissolutionN_rate6
  real(dp), dimension(nColPars)   :: dissolutionN_rate7
  real(dp), dimension(nColPars)   :: denitrification_soil1
  real(dp), dimension(nColPars)   :: denitrification_soil2
  real(dp), dimension(nColPars)   :: denitrification_soil3
  real(dp), dimension(nColPars)   :: denitrification_soil4
  real(dp), dimension(nColPars)   :: denitrification_soil5
  real(dp), dimension(nColPars)   :: denitrification_soil6
  real(dp), dimension(nColPars)   :: denitrification_soil7


  !define namelist
  !namelist directories for water quality model
  namelist /directories_wqm/ dir_inputdata_wqm, dir_gauges_wqm
  namelist /wqm_evaluation_gauges/ wqm_evalgauge_filename
  namelist /wqm_addinflow_gauges/ wqm_inflowgauge_filename
  !********************************************************
  !for SA
  !parameter namelist: mhm_parameter.nml
  !Nitrogen sub-model
  !********************************************************
  namelist /nutrientparameter/ denitrification_aquatic, primaryprod_rate1,primaryprod_rate2,primaryprod_rate3,&
  primaryprod_rate4,primaryprod_rate5,primaryprod_rate6,primaryprod_rate7,&
  degradationN_rate1,degradationN_rate2,degradationN_rate3,degradationN_rate4,degradationN_rate5,&
  degradationN_rate6,degradationN_rate7,&
  mineralisationN_rate1,mineralisationN_rate2,mineralisationN_rate3,mineralisationN_rate4,&
  mineralisationN_rate5,mineralisationN_rate6,mineralisationN_rate7,&
  dissolutionN_rate1,dissolutionN_rate2,dissolutionN_rate3,dissolutionN_rate4,dissolutionN_rate5,&
  dissolutionN_rate6,dissolutionN_rate7,&
  denitrification_soil1,denitrification_soil2,denitrification_soil3,denitrification_soil4,denitrification_soil5,&
  denitrification_soil6,denitrification_soil7
  namelist /statevarsoutput/timeStep_model_outputs_wqm, outputFlxState_wqm 
  !filenames are hard coded here
  file_namelist_wqm = "mhm.nml"
  file_param_wqm = "mhm_parameter.nml"  
  file_stateoutput_wqm = "wqm_outputs.nml"
  !allocation
  allocate(dirInputWQM(nBasins))
  allocate(dirGaugesWQM(nBasins))
!  allocate(wqm_evalgauge_filename(nBasins, nGaugesTotal) 
!  allocate(wqm_inflowgauge_filename(nBasins, nGaugesTotal)
  !initialise
  wqm_evalgauge_filename = num2str(nodata_i4)
  wqm_inflowgauge_filename = num2str(nodata_i4)

  !=================================================
  !READ namelist "mhm.nml"
  !=================================================
  call open_nml(file_namelist_wqm, 89, quiet =.true.)
  !------------------
  !read directories of water quality inputs
  !------------------
  call position_nml('directories_wqm', 89)
  read(89, nml=directories_wqm)

  dirInputWQM = dir_inputdata_wqm(1:nBasins)
  dirGaugesWQM  = dir_gauges_wqm(1:nBasins)
  
  !read WQ evaluation gauges, the same with discharge gauging station
  call position_nml('wqm_evaluation_gauges', 89)
  read(89, nml=wqm_evaluation_gauges)
  
  !allocation
  allocate(basin_wqm%fname(max(1, nGaugesTotal))) 
  basin_wqm%fname =num2str(nodata_i4)

  idx = 0
  do iBasin = 1, nBasins
     do iGauge = 1, basin_mrm%nGauges(iBasin)
        idx = idx + 1
        basin_wqm%fname(idx) = trim(dirGaugesWQM(iBasin))// trim(wqm_evalgauge_filename(iBasin, iGauge) )
     end do
  end do
  !read WQ additional inflow gauges(upstream inflow and point source), the same with discharge inflow gauges 
  call position_nml('wqm_addinflow_gauges', 89)
  read(89, nml=wqm_addinflow_gauges)

  !allocation
  allocate(basin_wqm%Inflowfname(max(1, nInflowGaugesTotal))) 
  basin_wqm%Inflowfname =num2str(nodata_i4)

  idx=0
  do iBasin = 1, nBasins
     do iGauge = 1, basin_mrm%nInflowGauges(iBasin)
        idx = idx + 1
        basin_wqm%Inflowfname(idx) = trim(dirGaugesWQM(iBasin))// trim(wqm_inflowgauge_filename(iBasin, iGauge) )
     end do
  end do  
  
  call close_nml(89)

  !======================================================
  !READ parameter namelist "mhm_parameter.nml"
  !======================================================
  call open_nml(file_param_wqm, 89, quiet =.true.)
  !read nitrate submodel parameters
  call position_nml('nutrientparameter', 89)
  read(89, nml=nutrientparameter)

  !processMatrix(11,1) = 1_i4                   ! if wqm_config is processing, then this process should be turned on (=1)
  !processMatrix(11,2) = 11_i4                   ! seven parameters are introduced              
  !processMatrix(11,3) = sum(processMatrix(1:11,2)) 

  !call append(global_parameters, reshape(denitrification_aquatic,(/1,nColPars/)))
  !call append(global_parameters, reshape(primaryprod_rate,(/1,nColPars/)))
  !call append(global_parameters, reshape(primaryprod_rate_agri,(/1,nColPars/)))
  !call append(global_parameters, reshape(degradationN_rate,(/1,nColPars/)))
  !call append(global_parameters, reshape(degradationN_rate_agri,(/1,nColPars/)))
  !call append(global_parameters, reshape(mineralisationN_rate,(/1,nColPars/)))
  !call append(global_parameters, reshape(mineralisationN_rate_agri,(/1,nColPars/)))
  !call append(global_parameters, reshape(dissolutionN_rate,(/1,nColPars/)))
  !call append(global_parameters, reshape(dissolutionN_rate_agri,(/1,nColPars/)))
  !call append(global_parameters, reshape(denitrification_soil,(/1,nColPars/)))
  !call append(global_parameters, reshape(denitrification_agrisoil,(/1,nColPars/)))
!  call append(global_parameters, reshape(groundwater_residtime,(/1,nColPars/)))

  !call append(global_parameters_name, (/&
  !          'denitrification_aquatic     ',&
  !          'primaryprod_rate            ',&
  !          'primaryprod_rate_agri       ',&
  !          'degradationN_rate           ',&
  !          'degradationN_rate_agri      ',&
  !          'mineralisationN_rate        ',&
  !          'mineralisationN_rate_agri   ',&
  !          'dissolutionN_rate           ',&
  !          'dissolutionN_rate_agri      ',&
  !          'denitrification_soil        ',&
  !          'denitrification_agrisoil    '/))

  !******************************************************
  !for SA
  !******************************************************
  processMatrix(11,1) = 1_i4                   ! if wqm_config is processing, then this process should be turned on (=1)
  processMatrix(11,2) = 36_i4                   ! seven parameters are introduced              
  processMatrix(11,3) = sum(processMatrix(1:11,2))
  
  call append(global_parameters, reshape(denitrification_aquatic,(/1,nColPars/)))
  call append(global_parameters, reshape(primaryprod_rate1,(/1,nColPars/)))
  call append(global_parameters, reshape(primaryprod_rate2,(/1,nColPars/)))
  call append(global_parameters, reshape(primaryprod_rate3,(/1,nColPars/)))
  call append(global_parameters, reshape(primaryprod_rate4,(/1,nColPars/)))
  call append(global_parameters, reshape(primaryprod_rate5,(/1,nColPars/)))
  call append(global_parameters, reshape(primaryprod_rate6,(/1,nColPars/)))
  call append(global_parameters, reshape(primaryprod_rate7,(/1,nColPars/)))
  call append(global_parameters, reshape(degradationN_rate1,(/1,nColPars/)))
  call append(global_parameters, reshape(degradationN_rate2,(/1,nColPars/)))
  call append(global_parameters, reshape(degradationN_rate3,(/1,nColPars/)))
  call append(global_parameters, reshape(degradationN_rate4,(/1,nColPars/)))
  call append(global_parameters, reshape(degradationN_rate5,(/1,nColPars/)))
  call append(global_parameters, reshape(degradationN_rate6,(/1,nColPars/)))
  call append(global_parameters, reshape(degradationN_rate7,(/1,nColPars/)))
  call append(global_parameters, reshape(mineralisationN_rate1,(/1,nColPars/)))
  call append(global_parameters, reshape(mineralisationN_rate2,(/1,nColPars/)))
  call append(global_parameters, reshape(mineralisationN_rate3,(/1,nColPars/)))
  call append(global_parameters, reshape(mineralisationN_rate4,(/1,nColPars/)))
  call append(global_parameters, reshape(mineralisationN_rate5,(/1,nColPars/)))
  call append(global_parameters, reshape(mineralisationN_rate6,(/1,nColPars/)))
  call append(global_parameters, reshape(mineralisationN_rate7,(/1,nColPars/)))
  call append(global_parameters, reshape(dissolutionN_rate1,(/1,nColPars/)))
  call append(global_parameters, reshape(dissolutionN_rate2,(/1,nColPars/)))
  call append(global_parameters, reshape(dissolutionN_rate3,(/1,nColPars/)))
  call append(global_parameters, reshape(dissolutionN_rate4,(/1,nColPars/)))
  call append(global_parameters, reshape(dissolutionN_rate5,(/1,nColPars/)))
  call append(global_parameters, reshape(dissolutionN_rate6,(/1,nColPars/)))
  call append(global_parameters, reshape(dissolutionN_rate7,(/1,nColPars/)))
  call append(global_parameters, reshape(denitrification_soil1,(/1,nColPars/)))
  call append(global_parameters, reshape(denitrification_soil2,(/1,nColPars/)))
  call append(global_parameters, reshape(denitrification_soil3,(/1,nColPars/)))
  call append(global_parameters, reshape(denitrification_soil4,(/1,nColPars/)))
  call append(global_parameters, reshape(denitrification_soil5,(/1,nColPars/)))
  call append(global_parameters, reshape(denitrification_soil6,(/1,nColPars/)))
  call append(global_parameters, reshape(denitrification_soil7,(/1,nColPars/)))

  call append(global_parameters_name, (/&
             'denitrification_aquatic      ',&
             'primaryprod_rate1            ',&
             'primaryprod_rate2            ',&
             'primaryprod_rate3            ',&
             'primaryprod_rate4            ',&
             'primaryprod_rate5            ',&
             'primaryprod_rate6            ',&
             'primaryprod_rate7            ',&
             'degradationN_rate1           ',&
             'degradationN_rate2           ',&
             'degradationN_rate3           ',&
             'degradationN_rate4           ',&
             'degradationN_rate5           ',&
             'degradationN_rate6           ',&
             'degradationN_rate7           ',&
             'mineralisationN_rate1        ',&
             'mineralisationN_rate2        ',&
             'mineralisationN_rate3        ',&
             'mineralisationN_rate4        ',&
             'mineralisationN_rate5        ',&
             'mineralisationN_rate6        ',&
             'mineralisationN_rate7        ',&
             'dissolutionN_rate1           ',&
             'dissolutionN_rate2           ',&
             'dissolutionN_rate3           ',&
             'dissolutionN_rate4           ',&
             'dissolutionN_rate5           ',&
             'dissolutionN_rate6           ',&
             'dissolutionN_rate7           ',&
             'denitrification_soil1        ',&
             'denitrification_soil2        ',&
             'denitrification_soil3        ',&
             'denitrification_soil4        ',&
             'denitrification_soil5        ',&
             'denitrification_soil6        ',&
             'denitrification_soil7        '/))

  ! check if parameter are in range
  if ( .not. in_bound(global_parameters(processMatrix(11, 3) - processMatrix(11, 2) + 1 : processMatrix(11, 3),:))) then
     call message('***ERROR: parameter in namelist "nitrateparameter" out of bound in ',&
          trim(adjustl(file_param_wqm)))
     stop
  end if

  call close_nml(89)
  
  !======================================================
  !READ parameter namelist "wqm_outputs.nml"
  !======================================================  
  nOUTstate_wqm =12
  allocate(outputFlxState_wqm(nOUTstate_wqm) )  
  outputFlxState_wqm = .FALSE.
  timeStep_model_outputs_wqm = -2
  inquire(file = file_stateoutput_wqm, exist = fexist)
  if (fexist) then
  call open_nml(file_stateoutput_wqm, 89, quiet =.true.)  
  call position_nml ('statevarsoutput', 89)
  read(89, nml = statevarsoutput)
  call close_nml(89)

  end if
  
  if (any(outputFlxState_wqm)) then  
     call message( '' )
     call message( 'Following state variables of water quality model will be written:' )
     if (outputFlxState_wqm(1)) then
        call message( '    Soil moisture concentration in each layer (L1_csoilMoist)     [mg/l]')
     end if  
     if (outputFlxState_wqm(2)) then
        call message( '    Near surface fast runoff concentration (L1_cfastRunoff)     [mg/l]')
     end if 
     if (outputFlxState_wqm(3)) then
        call message( '    Interflow concentration (L1_cslowRunoff)     [mg/l]')
     end if   
     if (outputFlxState_wqm(4)) then
        call message( '    Baseflow concentration (L1_cbaseflow)     [mg/l]')
     end if 
     if (outputFlxState_wqm(5)) then
        call message( '    Total runoff concentration (L1_ctotal_runoff)     [mg/l]')
     end if
     if (outputFlxState_wqm(6)) then
        call message( '    Total uptake amount in terrestrial phase (L1_soilUptakeN) [mg/m^2/timestep]')
     end if
     if (outputFlxState_wqm(7)) then
        call message( '    Total denitrification amount (L1_soilDenitri) [mg/m^2/timestep]')
     end if
     if (outputFlxState_wqm(8)) then
        call message( '    Total mineralisation amount (L1_soilMineralN) [mg/m^2/timestep]')
     end if
     if (outputFlxState_wqm(9)) then
        call message( '    Total frtman applied IN amount (L1_soilINfrtmanapp) [mg/m^2/timestep]')
     end if
     if (outputFlxState_wqm(10)) then
        call message( '    output concentration of each reach at routing level (L11_concMod) [mg/l]')
     end if
     if (outputFlxState_wqm(11)) then
        call message( '    Instream denitrification amount (L11_aquaticDenitri) [mg/m^2/timestep]')
     end if
     if (outputFlxState_wqm(12)) then
        call message( '    Instream assimilatory uptake amount (L11_aquaticAssimil) [mg/m^2/timestep]]')
     end if
	 
  end if
  
  
  end subroutine wqm_readconfig

  ! --------------------------------------------------------------------------------
  ! private funtions and subroutines, DUPLICATED FROM mo_read_config.f90
  ! --------------------------------------------------------------------------------

  function in_bound(params)
    real(dp), dimension(:,:), intent(in) :: params ! parameter:
    !                                              !   col_1=Lower bound,
    !                                              !   col_2=Upper bound
    !                                              !   col_3=initial
    logical :: in_bound

    if ( any(params(:,3) .lt. params(:,1)) .or. any(params(:,3) .gt. params(:,2)) ) then
       in_bound=.false.
    else
       in_bound=.true.
    end if

  end function in_bound
  
  ! ------------------------------------------------------------------

  !     NAME
  !         wqm_readinputdata

  !     PURPOSE
  !>        \brief Reads water quality information from input files.

  !>        \details Reads water quality information from input files.  \n
  !>        Four input files are located in "/input/water_quality/".

  !     CALLING SEQUENCE
  !         

  !     INTENT(IN)
  !>        None

  !     INTENT(INOUT)
  !         None

  !     INTENT(OUT)
  !         None

  !     INTENT(IN), OPTIONAL
  !         None

  !     INTENT(INOUT), OPTIONAL
  !         None

  !     INTENT(OUT), OPTIONAL
  !         None

  !     RETURN
  !>        None

  !     RESTRICTIONS
  !         None

  !     EXAMPLE
  !         None


  !     LITERATURE
  !         None

  !     HISTORY
  !>        \author Xiaoqiang Yang
  !>        \date Jun 2016

  subroutine wqm_readinputdata()

    use mo_read_spatial_data,   only: read_header_ascii,     &
                                      read_spatial_data_ascii
    use mo_append,              only: append
    use mo_message,             only: message
    use mo_global_variables,    only: nLAIclass, level0,  & !grid information
         resolutionHydrology,   & ! hydrology resolution (L1 scale)
         nBasins
    use mo_string_utils,        only: num2str
    use mo_wqm_global_variables, only: dirInputWQM, nCroptation, &
         num_crops,       & !number of crop types in total
         rotation,        & !rotation infos (type variables)
         cropdata,        & !crop infos (type variables)
         degradN_LAI,  & !
         mineraN_LAI,  & !
         dissolN_LAI,  & !
         init_concIN,  & !
         init_concON,  & !
         init_humusN,  & !
         init_fastN,   & !
         hnhalf,       & !
         L0_cover_rotation !
    !use mo_mrm_global_variables, only: evalPer
    use mo_mhm_constants,       only: nodata_i4
    use mo_julian,              only: julday


    implicit none
    !---
	!local
    integer(i4)      :: iBasin,i,j
    character(256)   :: fName, dummy  !, line
    integer(i4)      :: funit  
    integer(i4), dimension(:,:), allocatable   :: data_i4_2d
    logical, dimension(:,:), allocatable       :: mask_2d
	
    !integer(i4), dimension(3)    :: start_period, end_period, start_period_file, end_period_file !calendar date
    !integer(i4), dimension(3)    :: file_time
    !real(dp)                     :: nodata_file      ! no data value of data
    !integer(i4)                  :: start_jul,end_jul,start_jul_file,end_jul_file !julian date  
    !integer(i4)                  :: length_file, length_period ! number of days in file and period respectivly
    !integer(i4)                  :: idx_st_period, idx_en_period, idx_st_file, idx_en_file
                                    !index for merge file and period
	
    !allocate
    if (.not. allocated(degradN_LAI) ) allocate(degradN_LAI(nLAIclass) )
    if (.not. allocated(mineraN_LAI) ) allocate(mineraN_LAI(nLAIclass) )
    if (.not. allocated(dissolN_LAI) ) allocate(dissolN_LAI(nLAIclass) )
    if (.not. allocated(init_concIN) ) allocate(init_concIN(nLAIclass) )
    if (.not. allocated(init_concON) ) allocate(init_concON(nLAIclass) )
    if (.not. allocated(init_humusN) ) allocate(init_humusN(nLAIclass) )
    if (.not. allocated(init_fastN) )  allocate(init_fastN(nLAIclass) )
    if (.not. allocated(hnhalf) )      allocate(hnhalf(nLAIclass) )
	
	!COMMEN FILES FOR ALL BASINS
    !*****************
	!READ INITIA VALUES OF LAND-USE DENPENDENT VARIABLES
	!*****************
    fName = trim(adjustl(dirInputWQM(1))) // trim(adjustl("initial_value.txt") )
    funit =111
    open(funit, file= fName, action='read')
	
    read(funit, *) dummy
    do i =1, nLAIclass
       read(funit, *) dummy, degradN_LAI(i), mineraN_LAI(i), dissolN_LAI(i), init_concIN(i), &
                     init_concON(i),init_fastN(i), init_humusN(i), hnhalf(i)
    end do
    close(funit)

    !*****************
    !READ CROPDATA
    !*****************
    fName = trim(adjustl(dirInputWQM(1))) // trim(adjustl("cropdata.txt") )
    funit = 113
    open(funit, file = fName, action='read' )

    !read header
    read(funit, *) dummy, num_crops
    read(funit, *) dummy
    allocate(cropdata(num_crops) )
	
    do i =1, num_crops
       read(funit, *) cropdata(i)%cropname, cropdata(i)%cropid, cropdata(i)%frtn1, cropdata(i)%frtday1, cropdata(i)%frtdown1, &
          cropdata(i)%frtn2, cropdata(i)%frtday2, cropdata(i)%frtdown2, cropdata(i)%mann1, cropdata(i)%manday1, &
          cropdata(i)%mandown1, cropdata(i)%mann2, cropdata(i)%manday2, cropdata(i)%mandown2, cropdata(i)%manfIN,&			   
          cropdata(i)%frtperiod, cropdata(i)%resn, cropdata(i)%resday, cropdata(i)%resdown, cropdata(i)%resfast,   & 
          cropdata(i)%resperiod,cropdata(i)%up1, cropdata(i)%up2, cropdata(i)%up3, cropdata(i)%uppsoil,cropdata(i)%ploughd, &  
          cropdata(i)%plantd, cropdata(i)%havestd, cropdata(i)%ccrop, cropdata(i)%ccplantd, cropdata(i)%cchavestd

    !applied amount convert units: from kg/ha to kg/km2
    cropdata(i)%frtn1 = cropdata(i)%frtn1 * 100.0_dp 
    cropdata(i)%frtn2 = cropdata(i)%frtn2 * 100.0_dp
    cropdata(i)%mann1 = cropdata(i)%mann1 * 100.0_dp 
    cropdata(i)%mann2 = cropdata(i)%mann2 * 100.0_dp
    cropdata(i)%resn  = cropdata(i)%resn * 100.0_dp
  
    end do  
    close(funit)    
    	
	
    !BASIN DEPENDENT FILES
    !
    !allocate
    if (.not. allocated(nCroptation)) allocate(nCroptation(nBasins))
    if (.not. allocated(rotation) )   allocate(rotation(nBasins) )

    do iBasin =1, nBasins
	
    !*****************
    !READ ROTATION INFORMATION 
    !*****************
    fName = trim(adjustl(dirInputWQM(iBasin) )) // trim(adjustl("rotation_info.txt") )
    funit = 112
    open(funit, file = fName, action='read' )

    !read header
    read(funit, *) dummy, nCroptation(iBasin)
    read(funit, *) dummy
    allocate(rotation(iBasin)%id (nCroptation(iBasin)) )
    allocate(rotation(iBasin)%ncrops (nCroptation(iBasin)) )
    allocate(rotation(iBasin)%crop (nCroptation(iBasin), 10) )
    do i =1, nCroptation(iBasin)
       read(funit, *) rotation(iBasin)%id(i), rotation(iBasin)%ncrops(i), &
          (rotation(iBasin)%crop(i, j),j=1, rotation(iBasin)%ncrops(i)) 
    end do
    close(funit)
	
    !*****************
	!READ SPATIAL DATA OF DISTRIBUTED ROTATION-class
	!*****************	
    funit = 114
    fName = trim(adjustl(dirInputWQM(iBasin))) // trim(adjustl("rotation_class.asc") )
    call read_header_ascii(trim(fName), funit,   &
         level0%nrows(iBasin),     level0%ncols(iBasin), level0%xllcorner(iBasin), &
         level0%yllcorner(iBasin), level0%cellsize(iBasin), level0%nodata_value(iBasin))

    ! check for L0 and L1 scale consistency
    if( resolutionHydrology(iBasin) .LT. level0%cellsize(iBasin)) then
       call message()
       call message('***ERROR: resolutionHydrology (L1) should be smaller than the input data resolution (L0)')
       call message('          check set-up (in mhm.nml) for basin: ', trim(adjustl(num2str(iBasin))),' ...')
       stop
    end if

    !
    ! rotation class + overall mask creation
    fName = trim(adjustl(dirInputWQM(iBasin))) // trim(adjustl("rotation_class.asc") )
    funit = 115   
    call read_spatial_data_ascii(trim(fName), funit, &
         level0%nrows(iBasin),     level0%ncols(iBasin), level0%xllcorner(iBasin),&
         level0%yllcorner(iBasin), level0%cellsize(iBasin), data_i4_2d, mask_2d)

    data_i4_2d = merge(data_i4_2d, nodata_i4, mask_2d )
    call append( L0_cover_rotation, pack(data_i4_2d, mask_2d) )
    end do
  end subroutine wqm_readinputdata
!-----------------------------
 ! ------------------------------------------------------------------

  !     NAME
  !         wqm_variables_initalloc

  !     PURPOSE
  !>        \brief Allocates initial space for global variables of water quality model.

  !>        \details Allocates initial space for global variables of water quality model.
  !>        

  !     CALLING SEQUENCE
  !         

  !     INTENT(IN)
  !>        \param[in] 

  !     INTENT(INOUT)
  !         None

  !     INTENT(OUT)
  !         None

  !     INTENT(IN), OPTIONAL
  !         None

  !     INTENT(INOUT), OPTIONAL
  !         None

  !     INTENT(OUT), OPTIONAL
  !         None

  !     RETURN
  !>        None

  !     RESTRICTIONS
  !         None

  !     EXAMPLE
  !         
  !         

  !     LITERATURE
  !         None

  !     HISTORY
  !>        \author Xiaoqiang Yang
  !>        \date Jun 2016

  subroutine wqm_variables_initalloc()


    use mo_global_variables,    only: basin,  & !basin information
         nSoilHorizons_mHM,        & ! number of soil layers defined in config file 
         nLAIclass, nBasins
!    use mo_init_states,         only: get_basin_info
    use mo_mrm_tools,           only: get_basin_info_mrm
    use mo_append,              only: append
    use mo_wqm_paste,           only: append_3d
    use mo_string_utils,        only: num2str
    use mo_message,             only: message, message_text
    use mo_wqm_global_variables, only: nCroptation, &
         L0_cover_rotation, & !
         nsubstances,  & !
         L1_humusN, L1_fastN, L1_dissolvedIN,              & ! INOUT NS Four different Nitrate pools in each soil layer
         L1_dissolvedON, L1_csoilMoist,    & ! INOUT NS conc. in each soil layer, dim2=soillayers, dim3=substances  
         L1_csealSTW, L1_cunsatSTW, L1_csatSTW,            & ! INOUT NS conc. in each water storage, dim2=substances
         L1_soiltemp, L1_baseflow_avg,  & ! INOUT NS soil temperature, variables for calculating baseflow conc.
         L1_cinfilSoil, L1_cpreEffect, L1_cbaseflow_delta, & ! INOUT NX dim2=soillayers, dim3=substances/dim2=substances  
         L1_crain, L1_cpercolate, L1_crunoffSeal,          & ! INOUT NX dim2=substances
         L1_cfastRunoff, L1_cslowRunoff, L1_cbaseflow,     & ! INOUT NX dim2=substances 
         L1_ctotal_runoff,                                                   & ! INOUT NX dim2=substances 
         L1_rdegradN, L1_rmineralN, L1_rdissolN,                   & ! INOUT NE1 nitrate submodel parameters
         L1_rdeniSoil, L1_gwresidT,  L1_fLAI,                       &     ! INOUT NE1 nitrate submodel parameters  
         L1_frotation, L1_soilUptakeN, L1_soilDenitri,     &
         L1_soilMineralN,L1_soilINfrtmanapp,                  &
         prevstep_sealedStorage,             & ! sealed storage in last step
         prevstep_unsatStorage,              & ! unsaturated storage in last step
         prevstep_satStorage,                & ! saturated storage in last step
         prevstep_soilMoisture,                 & ! soil moisture in last step		 
         prevstep_percol,              & !
         prevstep_baseflow,            & !
         L1_reachtemp, L11_rivertemp,  & !
         L11_riverbox, L11_criverbox, L11_yravg_q,       & !inout 
         L11_concOUT, L11_interload, L11_concTIN,    & !inout dim2=substances                  
         L11_concMod,                            & !inout 
         L11_aquaticDenitri, L11_aquaticAssimil,  & !
         L11_rdeniAqtc,L11_rpprodN,L11_rivert_avg10, &
         L11_rivert_avg20, L11_fLAI

    use mo_mhm_constants,       only: nodata_i4

    implicit none
    !

    !local
    integer(i4)    :: ii   !basin id
    integer(i4)    :: nrows1, ncols1, nrows11, ncols11
    integer(i4)    :: ncells1, ncells11
    real(dp), dimension(:),     allocatable   :: dummy_vector
    real(dp), dimension(:,:),   allocatable   :: dummy_matrix, dummy_matrix_crop
    real(dp), dimension(:,:,:), allocatable   :: dummy_matrix_conc
	

    integer(i4)    :: k
	
  do ii=1, nBasins
    
	!check errors of input rotation types at L0
    do k = basin%L0_iStart(ii), basin%L0_iEnd(ii)

       if ( L0_cover_rotation(k) .eq. nodata_i4  ) then
          message_text = trim(num2str(k,'(I5)'))//','//trim(num2str(ii,'(I5)'))
          call message(' Error: rotation cover has missing value within the valid masked area at cell in basin ', &
               trim(message_text) )
          stop
       end if
    end do
    !allocation space for water quality related variables
    !L1 MODELLING SCALE
    call get_basin_info_mrm(ii, 1, nrows1, ncols1, ncells= ncells1)
    allocate( dummy_vector (ncells1))
    allocate( dummy_matrix (ncells1, nSoilHorizons_mHM))
    allocate( dummy_matrix_crop(ncells1, nCroptation(ii)))
    dummy_vector(:) = 0.0_dp
    dummy_matrix(:,:) = 0.0_dp
    dummy_matrix_crop(:,:) = 0.0_dp    

    ! fraction of crop rotation type
    call append(L1_frotation, dummy_matrix_crop )

    call append(prevstep_sealedStorage, dummy_vector)      ! sealed storage in last step
    call append(prevstep_unsatStorage,dummy_vector)       ! unsaturated storage in last step
    call append(prevstep_satStorage,dummy_vector)        ! saturated storage in last step
    call append(prevstep_percol, dummy_vector)
    call append(prevstep_baseflow, dummy_vector)
    call append(prevstep_soilMoisture, dummy_matrix)          ! soil moisture in last step

    call append(L1_soiltemp, dummy_vector )
    call append(L1_baseflow_avg, dummy_vector )    
    call append(L1_rdegradN, dummy_vector )
    !nitrate submodel parameters
    call append(L1_rmineralN, dummy_vector )
    call append(L1_rdissolN, dummy_vector )
    call append(L1_rdeniSoil, dummy_vector )
    call append(L1_gwresidT, dummy_vector )
    call append(L1_soiltemp, dummy_vector )    

    call append(L1_soilUptakeN, dummy_vector)
    call append(L1_soilDenitri, dummy_vector)
    call append(L1_soilMineralN, dummy_vector)
    call append(L1_soilINfrtmanapp, dummy_vector)
    !store 20day's average air temperature (for instream calcualting)
    call append(L1_reachtemp, dummy_vector)

    ! nitrate pools
    call append(L1_humusN, dummy_matrix )
    call append(L1_fastN, dummy_matrix )
    call append(L1_dissolvedIN, dummy_matrix )
    call append(L1_dissolvedON, dummy_matrix )

    deallocate( dummy_matrix)
    allocate( dummy_matrix (ncells1, nsubstances))
    allocate( dummy_matrix_conc(ncells1, nSoilHorizons_mHM, nsubstances))
    dummy_matrix(:,:) = 0.0_dp
    dummy_matrix_conc(:,:,:) = 0.0_dp
    !three conceptual water storages 
    call append(L1_csealSTW, dummy_matrix )
    call append(L1_cunsatSTW, dummy_matrix )
    call append(L1_csatSTW, dummy_matrix )
    !fluxes
    call append(L1_cbaseflow_delta, dummy_matrix ) ! for baseflow conc. cal.
    call append(L1_cpreEffect, dummy_matrix )
    call append(L1_crain, dummy_matrix )
    call append(L1_cpercolate, dummy_matrix )
    call append(L1_crunoffSeal, dummy_matrix )
    call append(L1_cfastRunoff, dummy_matrix )
    call append(L1_cslowRunoff, dummy_matrix )
    call append(L1_cbaseflow, dummy_matrix )
    call append(L1_ctotal_runoff, dummy_matrix )
    !3D
    call append_3d(L1_csoilMoist, dummy_matrix_conc )
    call append_3d(L1_cinfilSoil, dummy_matrix_conc )

    deallocate( dummy_matrix)
    allocate( dummy_matrix (ncells1, nLAIclass))
    dummy_matrix(:,:) = 0.0_dp
    call append(L1_fLAI, dummy_matrix)
    !**********************
    !LEVEL 11
    !**********************
    call get_basin_info_mrm(ii,11, nrows11, ncols11, ncells = ncells11)
    deallocate(dummy_matrix)
    deallocate(dummy_vector)
	
    allocate( dummy_matrix(ncells11, nLAIclass))
    call append(L11_fLAI, dummy_matrix)
    deallocate(dummy_matrix)
    allocate( dummy_vector(ncells11))
    allocate( dummy_matrix(ncells11, nsubstances))
    dummy_vector(:) = 0.0_dp
    dummy_matrix(:,:) = 0.0_dp
    
    call append(L11_rivertemp, dummy_vector )
    call append(L11_riverbox, dummy_vector )
    call append(L11_yravg_q, dummy_vector )
    call append(L11_aquaticDenitri, dummy_vector)
    call append(L11_aquaticAssimil, dummy_vector)
    call append(L11_rivert_avg10, dummy_vector)
    call append(L11_rivert_avg20, dummy_vector)



    !parameters
    call append(L11_rdeniAqtc, dummy_vector )
    call append(L11_rpprodN, dummy_vector )

    call append(L11_criverbox, dummy_matrix )
    call append(L11_concOUT, dummy_matrix )
    call append(L11_interload, dummy_matrix )
    call append(L11_concTIN, dummy_matrix )
    call append(L11_concMod, dummy_matrix )

    ! free space
    if ( allocated( dummy_vector          ) ) deallocate( dummy_vector          )
    if ( allocated( dummy_matrix          ) ) deallocate( dummy_matrix          )
    if ( allocated( dummy_matrix_conc     ) ) deallocate( dummy_matrix_conc     )
    if ( allocated( dummy_matrix_crop     ) ) deallocate( dummy_matrix_crop     )
   
   end do !basin loop
	
  end subroutine wqm_variables_initalloc
 ! ------------------------------------------------------------------
 ! ------------------------------------------------------------------

  !     NAME
  !         wqm_variables_default_init

  !     PURPOSE
  !>        \brief initialise global variables of water quality model for continuous running (e.g. calibration, sensitivity and uncertainty analysis, ...).

  !>        \details Allocates initial space for global variables of water quality model.
  !>        

  !     CALLING SEQUENCE
  !         

  !     INTENT(IN)
  !>        \param[in] 

  !     INTENT(INOUT)
  !         None

  !     INTENT(OUT)
  !         None

  !     INTENT(IN), OPTIONAL
  !         None

  !     INTENT(INOUT), OPTIONAL
  !         None

  !     INTENT(OUT), OPTIONAL
  !         None

  !     RETURN
  !>        None

  !     RESTRICTIONS
  !         None

  !     EXAMPLE
  !         
  !         

  !     LITERATURE
  !         None

  !     HISTORY
  !>        \author Xiaoqiang Yang
  !>        \date Jun 2016

  subroutine wqm_variables_default_init()

    use mo_wqm_global_variables, only: &

         
         L1_humusN, L1_fastN, L1_dissolvedIN,              & ! INOUT NS Four different Nitrate pools in each soil layer
         L1_dissolvedON, L1_csoilMoist,    & ! L1_initcsm,INOUT NS conc. in each soil layer, dim2=soillayers, dim3=substances  
         L1_csealSTW, L1_cunsatSTW, L1_csatSTW,            & ! INOUT NS conc. in each water storage, dim2=substances
         L1_soiltemp, L1_baseflow_avg,  & ! INOUT NS soil temperature, variables for calculating baseflow conc.
         L1_cinfilSoil, L1_cpreEffect, L1_cbaseflow_delta, & ! INOUT NX dim2=soillayers, dim3=substances/dim2=substances  
         L1_crain, L1_cpercolate, L1_crunoffSeal,          & ! INOUT NX dim2=substances
         L1_cfastRunoff, L1_cslowRunoff, L1_cbaseflow,     & ! INOUT NX dim2=substances 
         L1_ctotal_runoff,                                                   & ! INOUT NX dim2=substances 
         L1_rdegradN, L1_rmineralN, L1_rdissolN,                   & ! INOUT NE1 nitrate submodel parameters
         L1_rdeniSoil, L1_gwresidT,  L1_fLAI,                       &     ! INOUT NE1 nitrate submodel parameters  
         L1_frotation, L1_soilUptakeN, L1_soilDenitri,     &
         L1_soilMineralN,L1_soilINfrtmanapp,               &
         prevstep_sealedStorage,             & ! sealed storage in last step
         prevstep_unsatStorage,              & ! unsaturated storage in last step
         prevstep_satStorage,                & ! saturated storage in last step
         prevstep_soilMoisture,                 & ! soil moisture in last step		 
         prevstep_percol,              & !
         prevstep_baseflow,            & !
         L1_reachtemp, L11_rivertemp,  & !
         L11_riverbox, L11_criverbox, L11_yravg_q,       & !inout 
         L11_concOUT, L11_interload, L11_concTIN,    & !inout dim2=substances                  
         L11_concMod,                            & !inout 
         L11_aquaticDenitri, L11_aquaticAssimil,  & !
         L11_rdeniAqtc,L11_rpprodN,L11_rivert_avg10, &
         L11_rivert_avg20, L11_fLAI

    use mo_mhm_constants,  only: P1_InitStateFluxes
		 
    implicit none
    !

    ! fraction of crop rotation type
    L1_frotation = P1_InitStateFluxes

    prevstep_sealedStorage = P1_InitStateFluxes     ! sealed storage in last step
    prevstep_unsatStorage =P1_InitStateFluxes       ! unsaturated storage in last step
    prevstep_satStorage =P1_InitStateFluxes         ! saturated storage in last step
    prevstep_percol = P1_InitStateFluxes
    prevstep_baseflow = P1_InitStateFluxes
    prevstep_soilMoisture = P1_InitStateFluxes          ! soil moisture in last step

    L1_soiltemp = P1_InitStateFluxes
    L1_baseflow_avg = P1_InitStateFluxes 
    L1_rdegradN = P1_InitStateFluxes
    !nitrate submodel parameters
    L1_rmineralN = P1_InitStateFluxes
    L1_rdissolN = P1_InitStateFluxes
    L1_rdeniSoil =P1_InitStateFluxes
    L1_gwresidT =P1_InitStateFluxes
    L1_soiltemp = P1_InitStateFluxes  

    L1_soilUptakeN = P1_InitStateFluxes
    L1_soilDenitri = P1_InitStateFluxes
    L1_soilMineralN = P1_InitStateFluxes
    L1_soilINfrtmanapp = P1_InitStateFluxes
    !store 20day's average air temperature (for instream calcualting)
    L1_reachtemp = P1_InitStateFluxes

    ! nitrate pools
    L1_humusN = P1_InitStateFluxes
    L1_fastN =P1_InitStateFluxes
    L1_dissolvedIN = P1_InitStateFluxes
    L1_dissolvedON = P1_InitStateFluxes



    !three conceptual water storages 
    L1_csealSTW = P1_InitStateFluxes
    L1_cunsatSTW = P1_InitStateFluxes
    L1_csatSTW = P1_InitStateFluxes
    !fluxes
    L1_cbaseflow_delta = P1_InitStateFluxes! for baseflow conc. cal.
    L1_cpreEffect = P1_InitStateFluxes
    L1_crain = P1_InitStateFluxes
    L1_cpercolate = P1_InitStateFluxes
    L1_crunoffSeal = P1_InitStateFluxes
    L1_cfastRunoff = P1_InitStateFluxes
    L1_cslowRunoff = P1_InitStateFluxes
    L1_cbaseflow = P1_InitStateFluxes
    L1_ctotal_runoff = P1_InitStateFluxes
    !3D
    L1_csoilMoist = P1_InitStateFluxes
    !L1_initcsm = 0.0_dp   !variable to store the initial conc. in soil moisture, currently only for SA to avoid numerical error of conc. of sm
    L1_cinfilSoil =P1_InitStateFluxes


    L1_fLAI = P1_InitStateFluxes
    !**********************
    !LEVEL 11
    !**********************

    L11_fLAI = P1_InitStateFluxes
    L11_rivertemp = P1_InitStateFluxes
    L11_riverbox = P1_InitStateFluxes
    L11_yravg_q = P1_InitStateFluxes
    L11_aquaticDenitri = P1_InitStateFluxes
    L11_aquaticAssimil = P1_InitStateFluxes
    L11_rivert_avg10 = P1_InitStateFluxes
    L11_rivert_avg20 = P1_InitStateFluxes

	
    !parameters
    L11_rdeniAqtc =P1_InitStateFluxes
    L11_rpprodN = P1_InitStateFluxes

    L11_criverbox = P1_InitStateFluxes
    L11_concOUT =P1_InitStateFluxes
    L11_interload = P1_InitStateFluxes
    L11_concTIN = P1_InitStateFluxes
    L11_concMod = P1_InitStateFluxes
	
  end subroutine wqm_variables_default_init
 
END MODULE mo_wqm_read
