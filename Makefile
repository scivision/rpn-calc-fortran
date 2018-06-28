
rpncalc: reg.o funcs.o calc.f90
	$(FC) -g $^ -o $@

funcs.o: funcs.f90
	$(FC) -g -fmax-errors=3 $^ -c 
	
reg.o: reg.f90
	$(FC) -g $^ -c 
	
clean:
	$(RM) *.o *.mod
