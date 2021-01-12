#!/bin/bash

$QFDS -p 4 -d Pressure_Solver poisson2d_4mesh_fft.fds
$QFDS -p 4 -d Pressure_Solver poisson2d_4mesh_fft_tight.fds
$QFDS -p 4 -d Pressure_Solver poisson2d_4mesh_scarc.fds
$QFDS -p 4 -d Pressure_Solver poisson2d_4mesh_scarc_tight.fds
$QFDS -p 4 -d Pressure_Solver poisson2d_4mesh_uscarc.fds
$QFDS -p 4 -d Pressure_Solver poisson2d_4mesh_uglmat.fds

$QFDS -p 16 -d Pressure_Solver poisson2d_16mesh_fft.fds
$QFDS -p 16 -d Pressure_Solver poisson2d_16mesh_fft_tight.fds
$QFDS -p 16 -d Pressure_Solver poisson2d_16mesh_scarc.fds
$QFDS -p 16 -d Pressure_Solver poisson2d_16mesh_scarc_gmg1.fds
$QFDS -p 16 -d Pressure_Solver poisson2d_16mesh_scarc_gmg2.fds
$QFDS -p 16 -d Pressure_Solver poisson2d_16mesh_scarc_gmg2_tight.fds
$QFDS -p 16 -d Pressure_Solver poisson2d_16mesh_scarc_gmg3.fds
$QFDS -p 16 -d Pressure_Solver poisson2d_16mesh_scarc_gmg4.fds
$QFDS -p 16 -d Pressure_Solver poisson2d_16mesh_scarc_gmg5.fds
$QFDS -p 16 -d Pressure_Solver poisson2d_16mesh_scarc_tight.fds
$QFDS -p 16 -d Pressure_Solver poisson2d_16mesh_scarc_twolevel.fds
$QFDS -p 16 -d Pressure_Solver poisson2d_16mesh_scarc_twolevel_tight.fds
$QFDS -p 16 -d Pressure_Solver poisson2d_16mesh_uglmat.fds
$QFDS -p 16 -d Pressure_Solver poisson2d_16mesh_uscarc.fds

$QFDS -p 4 -d NS_Analytical_Solution ns2d_4mesh_16_scarc.fds
$QFDS -p 4 -d NS_Analytical_Solution ns2d_4mesh_16_scarc_nupt1.fds
$QFDS -p 4 -d NS_Analytical_Solution ns2d_4mesh_32_scarc.fds
$QFDS -p 4 -d NS_Analytical_Solution ns2d_4mesh_32_scarc_nupt1.fds
$QFDS -p 4 -d NS_Analytical_Solution ns2d_4mesh_64_scarc.fds
$QFDS -p 4 -d NS_Analytical_Solution ns2d_4mesh_64_scarc_nupt1.fds
$QFDS -p 4 -d NS_Analytical_Solution ns2d_4mesh_8_scarc.fds
$QFDS -p 4 -d NS_Analytical_Solution ns2d_4mesh_8_scarc_nupt1.fds
$QFDS -p 16 -d NS_Analytical_Solution ns2d_16mesh_64_scarc.fds

$QFDS -d Pressure_Solver dancing_eddies_1mesh.fds
$QFDS -p 4 -d Pressure_Solver dancing_eddies_uglmat.fds
$QFDS -p 4 -d Pressure_Solver dancing_eddies_tight.fds
$QFDS -p 4 -d Pressure_Solver dancing_eddies_tight_overlap.fds
$QFDS -p 4 -d Pressure_Solver dancing_eddies_default.fds
$QFDS -p 4 -d Pressure_Solver dancing_eddies_scarc.fds
$QFDS -p 4 -d Pressure_Solver dancing_eddies_scarc_tight.fds
$QFDS -p 4 -d Pressure_Solver dancing_eddies_uscarc.fds

$QFDS -p 10 -d dancing_eddies_10mesh_uglmat.fds
$QFDS -p 10 -d dancing_eddies_10mesh_tight.fds
$QFDS -p 10 -d dancing_eddies_10mesh_default.fds
$QFDS -p 10 -d dancing_eddies_10mesh_scarc.fds
$QFDS -p 10 -d dancing_eddies_10mesh_scarc_gmg.fds
$QFDS -p 10 -d dancing_eddies_10mesh_scarc_tight.fds
$QFDS -p 10 -d dancing_eddies_10mesh_scarc_twolevel.fds
$QFDS -p 10 -d dancing_eddies_10mesh_uscarc.fds

$QFDS -p 8 -d Pressure_Solver duct_flow.fds
$QFDS -p 8 -d Pressure_Solver duct_flow_scarc.fds
$QFDS -p 8 -d Pressure_Solver duct_flow_uglmat.fds
$QFDS -p 8 -d Pressure_Solver duct_flow_uscarc.fds

$QFDS -p 16 -d Scalar_Analytical_Solution shunn3_16mesh_32_scarc.fds
$QFDS -p 16 -d Scalar_Analytical_Solution shunn3_16mesh_64_scarc.fds
$QFDS -p 16 -d Scalar_Analytical_Solution shunn3_16mesh_128_scarc.fds
$QFDS -p 16 -d Scalar_Analytical_Solution shunn3_16mesh_256_scarc.fds
$QFDS -p 16 -d Scalar_Analytical_Solution shunn3_16mesh_512_scarc.fds

$QFDS -p 4 -d Turbulence ribbed_channel_20_uscarc.fds
$QFDS -p 4 -d Turbulence ribbed_channel_40_uscarc.fds
$QFDS -p 4 -d Turbulence ribbed_channel_80_uscarc.fds
$QFDS -p 4 -d Turbulence ribbed_channel_160_uscarc.fds

#$QFDS -d Turbulence deardorff_32_scarc.fds
#$QFDS -d Turbulence deardorff_64_scarc.fds

#$QFDS -p 3 -d Pressure_Effects pressure_rise_scarc.fds

#$QFDS -p 3 -d Pressure_Effects zone_break_slow_4mesh_scarc.fds
#$QFDS -p 3 -d Pressure_Effects zone_break_fast_4mesh_scarc.fds

#$QFDS -p 3 -d Pressure_Effects zone_shape_2mesh_scarc.fds
#$QFDS -p 3 -d Pressure_Effects zone_shape_4mesh_scarc.fds

#$QFDS -p 4 -d HVAC door_crack_scarc.fds

#$QFDS -p 5 -d Flowfields simple_duct_scarc.fds
