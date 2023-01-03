PROG=convert_netcdf_binary_ww3

FFLAGS = -DST42

F90 = gfortran

#GFORTRAN
F90OPTI = -pg -O1 -ffree-line-length-0 -fbacktrace -Wall
#F90OPTI = -g -O1 -ffree-line-length-0 -fbacktrace 
#F90OPTI = -g -Wall -Wextra -Warray-temporaries -Wconversion -fbacktrace -ffree-line-length-0 -fcheck=all -ffpe-trap=zero,overflow -finit-real=nan



NETCDF_INC = -I$(NETCDF_INCDIR)
NETCDF_LIB = $(NETCDF_FORTRAN_LINK) -lnetcdf


OBJ_S_D = convert_netcdf_binary_ww3.o



FFLAGS += -DWWM_SOLVER

F90FLAGS = ${NETCDF_INC} ${F90OPTI}
LIBS = ${NETCDF_LIB}

$(PROG): $(OBJ_S_D)
	$(F90) -o $(PROG) $(F90FLAGS) ${F90OPTI} $(OBJ_S_D) $(LIBS) 

clean:
	rm -f *.o *.oo *.obj *.ipo *.mod *.map *__genmod.f90 *.ilk *.pdb $(PROG)

.SUFFIXES: $(SUFFIXES) .F90

.F90.o:
	$(F90) $(F90FLAGS) -c $<

