#!/bin/bash

./run_scarc.sh 4 Pressure_Solver poisson2d_4mesh_fft.fds
./run_scarc.sh 4 Pressure_Solver poisson2d_4mesh_fft_tight.fds
./run_scarc.sh 4 Pressure_Solver poisson2d_4mesh_scarc.fds
./run_scarc.sh 4 Pressure_Solver poisson2d_4mesh_scarc_tight.fds
./run_scarc.sh 4 Pressure_Solver poisson2d_4mesh_uscarc.fds
./run_scarc.sh 4 Pressure_Solver poisson2d_4mesh_uglmat.fds

./run_scarc.sh 16 Pressure_Solver poisson2d_16mesh_fft.fds
./run_scarc.sh 16 Pressure_Solver poisson2d_16mesh_fft_tight.fds
./run_scarc.sh 16 Pressure_Solver poisson2d_16mesh_scarc.fds
./run_scarc.sh 16 Pressure_Solver poisson2d_16mesh_scarc_gmg1.fds
./run_scarc.sh 16 Pressure_Solver poisson2d_16mesh_scarc_gmg2.fds
./run_scarc.sh 16 Pressure_Solver poisson2d_16mesh_scarc_gmg2_tight.fds
./run_scarc.sh 16 Pressure_Solver poisson2d_16mesh_scarc_gmg3.fds
./run_scarc.sh 16 Pressure_Solver poisson2d_16mesh_scarc_gmg4.fds
./run_scarc.sh 16 Pressure_Solver poisson2d_16mesh_scarc_gmg5.fds
./run_scarc.sh 16 Pressure_Solver poisson2d_16mesh_scarc_tight.fds
./run_scarc.sh 16 Pressure_Solver poisson2d_16mesh_scarc_twolevel.fds
./run_scarc.sh 16 Pressure_Solver poisson2d_16mesh_scarc_twolevel_tight.fds
./run_scarc.sh 16 Pressure_Solver poisson2d_16mesh_uglmat.fds
./run_scarc.sh 16 Pressure_Solver poisson2d_16mesh_uscarc.fds

./run_scarc.sh  4 NS_Analytical_Solution ns2d_4mesh_16_scarc.fds
./run_scarc.sh  4 NS_Analytical_Solution ns2d_4mesh_16_scarc_nupt1.fds
./run_scarc.sh  4 NS_Analytical_Solution ns2d_4mesh_32_scarc.fds
./run_scarc.sh  4 NS_Analytical_Solution ns2d_4mesh_32_scarc_nupt1.fds
./run_scarc.sh  4 NS_Analytical_Solution ns2d_4mesh_64_scarc.fds
./run_scarc.sh  4 NS_Analytical_Solution ns2d_4mesh_64_scarc_nupt1.fds
./run_scarc.sh  4 NS_Analytical_Solution ns2d_4mesh_8_scarc.fds
./run_scarc.sh  4 NS_Analytical_Solution ns2d_4mesh_8_scarc_nupt1.fds
./run_scarc.sh 16 NS_Analytical_Solution ns2d_16mesh_64_scarc.fds

./run_scarc.sh 1 Pressure_Solver dancing_eddies_1mesh.fds
./run_scarc.sh 4 Pressure_Solver dancing_eddies_uglmat.fds
./run_scarc.sh 4 Pressure_Solver dancing_eddies_tight.fds
./run_scarc.sh 4 Pressure_Solver dancing_eddies_tight_overlap.fds
./run_scarc.sh 4 Pressure_Solver dancing_eddies_default.fds
./run_scarc.sh 4 Pressure_Solver dancing_eddies_scarc.fds
./run_scarc.sh 4 Pressure_Solver dancing_eddies_scarc_tight.fds
./run_scarc.sh 4 Pressure_Solver dancing_eddies_uscarc.fds

./run_scarc.sh 10 dancing_eddies_10mesh_uglmat.fds
./run_scarc.sh 10 dancing_eddies_10mesh_tight.fds
./run_scarc.sh 10 dancing_eddies_10mesh_default.fds
./run_scarc.sh 10 dancing_eddies_10mesh_scarc.fds
./run_scarc.sh 10 dancing_eddies_10mesh_scarc_gmg.fds
./run_scarc.sh 10 dancing_eddies_10mesh_scarc_tight.fds
./run_scarc.sh 10 dancing_eddies_10mesh_scarc_twolevel.fds
./run_scarc.sh 10 dancing_eddies_10mesh_uscarc.fds

./run_scarc.sh 8 Pressure_Solver duct_flow.fds
./run_scarc.sh 8 Pressure_Solver duct_flow_scarc.fds
./run_scarc.sh 8 Pressure_Solver duct_flow_uglmat.fds
./run_scarc.sh 8 Pressure_Solver duct_flow_uscarc.fds

./run_scarc.sh 16 Scalar_Analytical_Solution shunn3_16mesh_32_scarc.fds
./run_scarc.sh 16 Scalar_Analytical_Solution shunn3_16mesh_64_scarc.fds
./run_scarc.sh 16 Scalar_Analytical_Solution shunn3_16mesh_128_scarc.fds
./run_scarc.sh 16 Scalar_Analytical_Solution shunn3_16mesh_256_scarc.fds
./run_scarc.sh 16 Scalar_Analytical_Solution shunn3_16mesh_512_scarc.fds

./run_scarc.sh 4 Turbulence ribbed_channel_20_uscarc.fds
./run_scarc.sh 4 Turbulence ribbed_channel_40_uscarc.fds
./run_scarc.sh 4 Turbulence ribbed_channel_80_uscarc.fds
./run_scarc.sh 4 Turbulence ribbed_channel_160_uscarc.fds

#./run_scarc.sh 1 Turbulence deardorff_32_scarc.fds
#./run_scarc.sh 1 Turbulence deardorff_64_scarc.fds

#./run_scarc.sh 3 Pressure_Effects pressure_rise_scarc.fds

#./run_scarc.sh 3 Pressure_Effects zone_break_slow_4mesh_scarc.fds
#./run_scarc.sh 3 Pressure_Effects zone_break_fast_4mesh_scarc.fds

#./run_scarc.sh 3 Pressure_Effects zone_shape_2mesh_scarc.fds
#./run_scarc.sh 3 Pressure_Effects zone_shape_4mesh_scarc.fds

#./run_scarc.sh 4 HVAC door_crack_scarc.fds

#./run_scarc.sh 5 Flowfields simple_duct_scarc.fds
