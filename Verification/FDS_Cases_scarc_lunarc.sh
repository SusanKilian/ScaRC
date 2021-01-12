#!/bin/bash

VERIFICATION=`pwd`
LUNARC="../Lunarc"

#
# NS_Analytical_Solution 
#
cd VERIFICATION/NS_Analytical_Solution

sbatch -J ns2d_4mesh_8_scarc  $LUNARC/avx4.sh ns2d_4mesh_8_scarc.fds
sbatch -J ns2d_4mesh_8_scarc  $LUNARC/avx4.sh ns2d_4mesh_8_scarc_nupt1.fds
sbatch -J ns2d_4mesh_16_scarc $LUNARC/avx4.sh ns2d_4mesh_16_scarc.fds
sbatch -J ns2d_4mesh_16_scarc $LUNARC/avx4.sh ns2d_4mesh_16_scarc_nupt1.fds
sbatch -J ns2d_4mesh_32_scarc $LUNARC/avx4.sh ns2d_4mesh_32_scarc.fds
sbatch -J ns2d_4mesh_32_scarc $LUNARC/avx4.sh ns2d_4mesh_32_scarc_nupt1.fds
sbatch -J ns2d_4mesh_64_scarc $LUNARC/avx4.sh ns2d_4mesh_64_scarc.fds
sbatch -J ns2d_4mesh_64_scarc $LUNARC/avx4.sh ns2d_4mesh_64_scarc_nupt1.fds

#
# Pressure_Effects
#
cd VERIFICATION/Pressure_Effects

sbatch -J pressure_boundary $LUNARC/avx1.sh pressure_boundary.fds
sbatch -J pressure_boundary_scarc $LUNARC/avx10.sh pressure_boundary_scarc.fds

sbatch -J pressure_rise $LUNARC/avx3.sh pressure_rise.fds
sbatch -J pressure_rise_glmat $LUNARC/avx3.sh pressure_rise_glmat.fds
sbatch -J pressure_rise_scarc $LUNARC/avx3.sh pressure_rise_scarc.fds

sbatch -J zone_break_fast_scarc $LUNARC/avx4.sh zone_break_fast_scarc.fds
sbatch -J zone_break_fast $LUNARC/avx1.sh zone_break_fast.fds
sbatch -J zone_break_slow_scarc $LUNARC/avx4.sh zone_break_slow_scarc.fds
sbatch -J zone_break_slow $LUNARC/avx1.sh zone_break_slow.fds

sbatch -J zone_shape $LUNARC/avx2.sh zone_shape.fds
sbatch -J zone_shape_scarc $LUNARC/avx2.sh zone_shape_scarc.fds

sbatch -J zone_shape_2 $LUNARC/avx8.sh zone_shape_2.fds
sbatch -J zone_shape_2_uscarc $LUNARC/avx8.sh zone_shape_2_uscarc.fds

#
# Pressure_Solver 
#
cd VERIFICATION/Pressure_Solver

sbatch -J dancing_eddies_default $LUNARC/avx4.sh dancing_eddies_default.fds
sbatch -J dancing_eddies_scarc $LUNARC/avx4.sh dancing_eddies_scarc.fds
sbatch -J dancing_eddies_scarc_tight $LUNARC/avx4.sh dancing_eddies_scarc_tight.fds
sbatch -J dancing_eddies_tight $LUNARC/avx4.sh dancing_eddies_tight.fds
sbatch -J dancing_eddies_tight_overlap $LUNARC/avx4.sh dancing_eddies_tight_overlap.fds
sbatch -J dancing_eddies_uglmat $LUNARC/avx4.sh dancing_eddies_uglmat.fds
sbatch -J dancing_eddies_uscarc $LUNARC/avx4.sh dancing_eddies_uscarc.fds
sbatch -J dancing_eddies_1mesh $LUNARC/avx1.sh dancing_eddies_1mesh.fds

sbatch -J duct_flow $LUNARC/avx8.sh duct_flow.fds
sbatch -J duct_flow_scarc $LUNARC/avx8.sh duct_flow_scarc.fds
sbatch -J duct_flow_uglmat $LUNARC/avx8.sh duct_flow_uglmat.fds
sbatch -J duct_flow_uscarc $LUNARC/avx8.sh duct_flow_uscarc.fds

sbatch -J hallways $LUNARC/avx5.sh hallways.fds
sbatch -J hallways_scarc $LUNARC/avx5.sh hallways_scarc.fds

sbatch -J poisson2d_fft $LUNARC/avx4_short4.sh poisson2d_fft.fds
sbatch -J poisson2d_scarc $LUNARC/avx4_short4.sh poisson2d_scarc.fds
sbatch -J poisson2d_uglmat $LUNARC/avx4_short4.sh poisson2d_uglmat.fds
sbatch -J poisson2d_uscarc $LUNARC/avx4_short4.sh poisson2d_uscarc.fds
sbatch -J poisson2d_fft_tight $LUNARC/avx4_short4.sh poisson2d_fft_tight.fds
sbatch -J poisson2d_scarc_tight $LUNARC/avx4_short4.sh poisson2d_scarc_tight.fds

sbatch -J poisson3d_fft $LUNARC/avx4.sh poisson3d_fft.fds
sbatch -J poisson3d_glmat $LUNARC/avx4.sh poisson3d_glmat.fds
sbatch -J poisson3d_scarc $LUNARC/avx4.sh poisson3d_scarc.fds
sbatch -J poisson3d_uglmat $LUNARC/avx4.sh poisson3d_uglmat.fds
sbatch -J poisson3d_uscarc $LUNARC/avx4.sh poisson3d_uscarc.fds
sbatch -J tunnel_demo $LUNARC/avx8.sh tunnel_demo.fds
sbatch -J tunnel_demo_glmat $LUNARC/avx8.sh tunnel_demo_glmat.fds
sbatch -J tunnel_demo_scarc $LUNARC/avx8.sh tunnel_demo_scarc.fds

#
# Scalar_Analytical_Solution
#
cd VERIFICATION/Scalar_Analytical_Solutiion

sbatch -J shunn3_16mesh_128_scarc $LUNARC/avx16.sh shunn3_16mesh_128_scarc.fds
sbatch -J shunn3_16mesh_256_scarc $LUNARC/avx16.sh shunn3_16mesh_256_scarc.fds
sbatch -J shunn3_16mesh_32_scarc $LUNARC/avx16.sh shunn3_16mesh_32_scarc.fds
sbatch -J shunn3_16mesh_512_scarc $LUNARC/avx16.sh shunn3_16mesh_512_scarc.fds
sbatch -J shunn3_16mesh_64_scarc $LUNARC/avx16.sh shunn3_16mesh_64_scarc.fds
sbatch -J shunn3_4mesh_128_scarc $LUNARC/avx4.sh shunn3_4mesh_128_scarc.fds
sbatch -J shunn3_4mesh_256_scarc $LUNARC/avx4.sh shunn3_4mesh_256_scarc.fds
sbatch -J shunn3_4mesh_32_scarc $LUNARC/avx4.sh shunn3_4mesh_32_scarc.fds
sbatch -J shunn3_4mesh_512_scarc $LUNARC/avx4.sh shunn3_4mesh_512_scarc.fds
sbatch -J shunn3_4mesh_64_scarc $LUNARC/avx4.sh shunn3_4mesh_64_scarc.fds

#
#Turbulence
#
cd VERIFICATION/Turbulence

sbatch -J ribbed_channel_160_4mesh_uscarc $LUNARC/avx4.sh ribbed_channel_160_4mesh_uscarc.fds
sbatch -J ribbed_channel_20_4mesh_uscarc $LUNARC/avx4.sh ribbed_channel_20_4mesh_uscarc.fds
sbatch -J ribbed_channel_40_4mesh_uscarc $LUNARC/avx4.sh ribbed_channel_40_4mesh_uscarc.fds
sbatch -J ribbed_channel_80_4mesh_uscarc $LUNARC/avx4.sh ribbed_channel_80_4mesh_uscarc.fds
sbatch -J ribbed_channel_20 $LUNARC/avx1.sh ribbed_channel_20.fds
sbatch -J ribbed_channel_40 $LUNARC/avx1.sh ribbed_channel_40.fds
sbatch -J ribbed_channel_80 $LUNARC/avx1.sh ribbed_channel_80.fds
sbatch -J ribbed_channel_160 $LUNARC/avx1.sh ribbed_channel_160.fds

cd VERIFICATION
