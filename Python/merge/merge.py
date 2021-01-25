import os
import sys
import re

args = [s.upper() for s in sys.argv[1:]]
for arg in args:
    assert arg in [
        "AMG",
        "DEBUG",
        "DEBUG2",
        "POSTPROCESSING",
        "VERBOSE",
        "VERBOSE2"
    ], "Invalid filter argument "+arg
if len(args):
    print ("Filter", args)
    rxString = "#if[^W]*(WITH_SCARC_(%s))?.*" % "|".join(args)
else:
    print ("Filter all #if*SCARC_DEBUG")
    rxString = "#if[^W]*(WITH_SCARC)?.*"

rxIfdef = re.compile(rxString)
def stripped(fn):
    assert fn.endswith(".f90"), "Can't handle non-f90 file " + fn

    with open(fn) as f:
        lines = f.read().split("\n")

    res = ""
    level = 0
    atDebug = 0
    for line in lines:
        write = True
        match = rxIfdef.match(line)

        if match:
            level += 1
            if match.group(1):
                write = False
                if atDebug == 0: atDebug = level

        elif line.startswith("#endif"):
            if level == atDebug:
                atDebug = 0
                write = False
            else:
                write = atDebug == 0
            level -= 1
        else:
            write = atDebug == 0

        if write:
            res += line + "\n"
    assert not atDebug, "Missing closing #endif in " + fn
    return res

files = (
    "scarc_header",
    "scarc_constants",
    "scarc_types",
    "scarc_variables",
    "scarc_pointers",
    "scarc_messages",
    "scarc_troubleshooting",
    "scarc_utilities",
    "scarc_storage",
    "scarc_convergence",
    "scarc_cpu",
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

path, _ = os.path.split(sys.argv[0])
parent, _ = os.path.split(path)
parent, _ = os.path.split(parent)
print  (path, parent)
source = os.path.join(parent, "Source")
target = os.path.join(source, "scrc.f90")
with open(target, "w") as f:
    for file in files:
        if file.startswith("#"): f.write(file+"\n")
        else:
            fn = os.path.join(source, file+".f90")
            f.write(stripped(fn))
