rm scrc.f90
echo "Building complete scrc source file"
cat "Modules/scarc_headers.f90" > scrc.f90
cat "Modules/scarc_constants.f90" >> scrc.f90
cat "Modules/scarc_types.f90" >> scrc.f90
cat "Modules/scarc_variables.f90" >> scrc.f90
cat "Modules/scarc_pointers.f90" >> scrc.f90
cat "Modules/scarc_messages.f90" >> scrc.f90
cat "Modules/scarc_errors.f90" >> scrc.f90
cat "Modules/scarc_utilities.f90" >> scrc.f90
cat "Modules/scarc_storage.f90" >> scrc.f90
cat "Modules/scarc_convergence.f90" >> scrc.f90
cat "Modules/scarc_timings.f90" >> scrc.f90
cat "Modules/scarc_stack.f90" >> scrc.f90
cat "Modules/scarc_initialization.f90" >> scrc.f90
cat "Modules/scarc_mpi.f90" >> scrc.f90
echo "#ifdef WITH_MKL" >> scrc.f90
cat "Modules/scarc_mkl.f90" >> scrc.f90
echo "#endif" >> scrc.f90
cat "Modules/scarc_vectors.f90" >> scrc.f90
cat "Modules/scarc_discretization.f90" >> scrc.f90
cat "Modules/scarc_matrices.f90" >> scrc.f90
cat "Modules/scarc_fft.f90" >> scrc.f90
cat "Modules/scarc_gmg.f90" >> scrc.f90
echo "#ifdef WITH_SCARC_AMG" >> scrc.f90
cat "Modules/scarc_amg.f90" >> scrc.f90
echo "#endif" >> scrc.f90
cat "Modules/scarc_mgm.f90" >> scrc.f90
cat "Modules/scarc_methods.f90" >> scrc.f90
cat "Modules/scarc_solvers.f90" >> scrc.f90
echo " ... done"
