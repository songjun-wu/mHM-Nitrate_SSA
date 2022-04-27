# model modification for saptial SA  20200821
modify mo_wqm_mpr.f90
modify mhm_driver.f90
replace mhm_parameter.nml

# test
input parameter number = 1730
test nitrate canshuhua?


# run for 3_dm
134480

# run for 1,2,4
26896 = 134480/5
1. 1, 26896
2. 26897, 53792
3. 53793, 80688
4. 80689, 107584
5. 107585, 134480
