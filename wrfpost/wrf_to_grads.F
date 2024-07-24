!  program to create grads file from wrf output (or restart/init) data.
!  current version reads only wrf-netcdf file format

!  May 2004



  program wrf_to_grads

  USE module_wrf_to_grads_util

  implicit none
  integer, parameter :: max_times = 100, max_variables = 100
  character (len=80)    :: data_files(max_times),     &
                            file_for_time(max_times), &
                            times(max_times),         &
                            input_file, grads_file

  integer :: file_time_index(max_times)

  character (len=20)    :: variables3d(max_variables)
  character (len=20)    :: variables2d(max_variables)
  character (len=50)    :: desc3d(max_variables)
  character (len=50)    :: desc2d(max_variables)

  logical :: debug1,debug2, all_times

  integer, external:: iargc
  integer :: input_unit, output_times, number_of_3dvar, number_of_2dvar, &
             input_file_unit, number_of_files
  integer :: length_input, length_grads, num_grads_levels, i, ii
  logical :: output_input_grid, use_lowest_heights
  real, dimension(100)  :: grads_levels
  character (len=1)     :: vertical_type
  character (len=6)     :: case_type
  character (len=2)     :: tmp_debug
  integer :: map

  debug1 = .false. 
  debug2 = .false. 
  input_file_unit = 10
  vertical_type = 'n'

   if (iargc() .lt. 2) then
    write(6,*) ' usage: wrf_to_grads  control_file  grads_output_file  [debug_option]'
    write(6,*) '        debug_options are:      : no debugging'
    write(6,*) '                            -v  : Some debugging' 
    write(6,*) '                            -V  : Lots of debugging'
    write(6,*) '  '
    write(6,*) ' example: wrf_to_grads  control_file  wrfout_jan00_mass  -v'
    stop
  else
    write(6,*) '  '
    write(6,*) '--------------------------------------'
    write(6,*) '        WRF to GrADS converter'
    write(6,*) '--------------------------------------'
    write(6,*) '  '
    call getarg(1,input_file)
    length_input = len_trim(input_file)
    call getarg(2,grads_file)
    length_grads = len_trim(grads_file)
    call getarg(3,tmp_debug)
    if(tmp_debug(1:2) == '-v') then
      debug1 = .true.
      write(6,*) ' Debugging option is set to low'
    endif
    if(tmp_debug(1:2) == '-V') then
      debug1 = .true.
      debug2 = .true.
      write(6,*) ' Debugging option is set to high'
    endif
    if(debug1) write(6,*) ' namelist_input file : ',input_file(1:length_input)
    if(debug1) write(6,*) ' data output file    : ',grads_file(1:length_grads)
    if(debug1) write(6,*) ' '

  endif


!      OPEN name_list 
       OPEN ( unit=input_file_unit,                                              &
              file=input_file(1:length_input),                                   &
              form='formatted',                                                  &
              status='old'  )


!  Get a list of times to process
    if(debug2) write(6,*) ' calling read_time_list '
    call read_time_list     ( input_file_unit, times, max_times, output_times,    &
                              all_times, debug1, debug2  ) 
    if(debug2) write(6,*) '  '


!  Get a list of variables to process
    if(debug2) write(6,*) ' calling read_variable_list '
    call read_variable_list ( input_file_unit,                                    &
                              variables3d, desc3d, number_of_3dvar,               &
                              variables2d, desc2d, number_of_2dvar,               &
                              max_variables, debug1, debug2  )
    if(debug1) write(6,*) '  '

!  Get a list of files to process
    if(debug2) write(6,*) ' calling read_file_list '
    call read_file_list     ( input_file_unit, data_files, max_times,             &
                              number_of_files, debug1, debug2  )
    if(debug1) write(6,*) '  '


!  Now checl to see what to so with the data
    read(input_file_unit, * ) 
    read(input_file_unit, * ) case_type
    write(6,*) ' We are dealing with: ',case_type(1:5), ' case'
    read(input_file_unit, * ) map
    write(6,*) ' Grads MAP is set to : ',map
    if (case_type(1:5) == 'ideal' .and. map == 1) then
      map = 0
      write(6,*) 'RESET map to NO BACKGROUND since we are dealing with and ideal case' 
    endif
    write(6,*) '  '

! Check what to do in the vertical
    read(input_file_unit, * ) num_grads_levels

    if( num_grads_levels == 0) then    !  output  cartesian grid
        write(6,*) ' generic cartesian output grid '
        use_lowest_heights = .false.
        output_input_grid = .true.
        num_grads_levels = 1
    else if ( num_grads_levels < 0) then                
        write(6,*) ' interp to z profile at lowest terrain column '
        use_lowest_heights = .true.          ! use z distribution from 
        output_input_grid = .true.           ! lowest point of terrain
        num_grads_levels = 1
    else
        read(input_file_unit, * ) 
        read(input_file_unit, * ) 
        read(input_file_unit, * ) 
        write(6,*) ' reading in user supplied levels '
        output_input_grid = .false.
        use_lowest_heights = .false. 
        i = 1
        do 
          read(input_file_unit,*,END=777) grads_levels(i)
          write(6,*) '    ',i,grads_levels(i)
          i = i+1
        enddo
 777    continue
        num_grads_levels = i-1
        if ( grads_levels(1) < 100.) then
          write(6,*) ' We are dealing with height levels here'
          vertical_type = 'z'
        else
          write(6,*) ' We are dealing with pressure levels here'
          vertical_type = 'p'
        endif
    endif
    if(debug2)write(6,*) '  '

    if(debug2) write(6,*) ' closing input file '
    close( unit=input_file_unit, status = 'keep')




! next, check each input file for times and produce list of
! times and files for the data

  if(debug2) write(6,*) ' calling create_time_file_list '

  call create_time_file_list( data_files,         &
                              file_for_time,      &
                              file_time_index,    &
                              times,              &
                              max_times,          &
                              all_times,          &
                              output_times,       &
                              number_of_files,    &
                              debug2              )


  if(debug2) write(6,*) ' exited create_time_file_list '

  call create_grads ( grads_file, file_for_time, file_time_index, times,   &
                      output_times, variables3d, number_of_3dvar,          &
                      variables2d, number_of_2dvar,                        &
                      desc3d, desc2d,                                      &
                      output_input_grid,                                   &
                      use_lowest_heights,                                  &
                      grads_levels,                                        &
                      num_grads_levels, case_type, vertical_type,          &
                      map, debug1, debug2 )

    write(6,*) '  '
    write(6,*) '--------------------------------------'
    write(6,*) '           Gracefull STOP             '
    write(6,*) '--------------------------------------'
    write(6,*) '  '

  end program wrf_to_grads

