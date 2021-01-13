rm scrc.f90
echo "Building complete scrc source file"
cat "scarc_headers.f90" > scrc.f90
cat "scarc_constants.f90" >> scrc.f90
cat "scarc_types.f90" >> scrc.f90
cat "scarc_variables.f90" >> scrc.f90
cat "scarc_pointers.f90" >> scrc.f90
cat "scarc_messages.f90" >> scrc.f90
cat "scarc_errors.f90" >> scrc.f90
cat "scarc_utilities.f90" >> scrc.f90
cat "scarc_storage.f90" >> scrc.f90
cat "scarc_convergence.f90" >> scrc.f90
cat "scarc_timings.f90" >> scrc.f90
cat "scarc_stack.f90" >> scrc.f90
cat "scarc_initialization.f90" >> scrc.f90
cat "scarc_mpi.f90" >> scrc.f90
echo "#ifdef WITH_MKL" >> scrc.f90
cat "scarc_mkl.f90" >> scrc.f90
echo "#endif" >> scrc.f90
cat "scarc_vectors.f90" >> scrc.f90
cat "scarc_discretization.f90" >> scrc.f90
cat "scarc_matrices.f90" >> scrc.f90
cat "scarc_fft.f90" >> scrc.f90
cat "scarc_gmg.f90" >> scrc.f90
echo "#ifdef WITH_SCARC_AMG" >> scrc.f90
cat "scarc_amg.f90" >> scrc.f90
echo "#endif" >> scrc.f90
cat "scarc_mgm.f90" >> scrc.f90
cat "scarc_methods.f90" >> scrc.f90
cat "scarc_solvers.f90" >> scrc.f90
echo " ... done"
