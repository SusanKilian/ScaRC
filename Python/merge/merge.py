import os
import sys

def stripped(fn):
    assert fn.endswith(".f90"), "Can't handle non-f90 file " + fn

    with open(fn) as f:
        lines = f.read().split("\n")

    atDebug = False
    res = ""
    for line in lines:
        if atDebug:
            if line.startswith("#endif"):
                atDebug = False
        elif line.startswith("#ifdef WITH_SCARC_DEBUG"): 
            atDebug = True
        else:
            res += line + "\n"
    assert not atDebug, "Missing closing #endif in " + fn
    return res

files = (
    "scarc_constants",
    "scarc_types",
    "scarc_variables",
    "scarc_pointers",
    "scarc_messages",
    "scarc_errors",
    "scarc_utilities",
    "scarc_storage",
    "scarc_convergence",
    "scarc_timings",
    "scarc_stack",
    "scarc_parser",
    "scarc_mpi",
    "#ifdef WITH_MKL",
    "scarc_mkl",
    "#endif",
    "scarc_vectors",
    "scarc_grids",
    "scarc_matrices",
    "scarc_fft",
    "scarc_gmg",
    "#ifdef WITH_SCARC_AMG",
    "scarc_amg",
    "#endif",
    "scarc_mgm",
    "scarc_methods",
    "scarc_solvers",
)

myself = os.path.dirname(os.path.abspath(__file__))
path, _ = os.path.split(myself)
#path, _ = os.path.split(sys.argv[0])
parent, _ = os.path.split(path)
source = os.path.join(parent, "Source")
target = os.path.join(source, "scrc.f90")
with open(target, "w") as f:
    for file in files:
        if file.startswith("#"): f.write(file+"\n")
        else:
            fn = os.path.join(source, file+".f90")
            f.write(stripped(fn))
