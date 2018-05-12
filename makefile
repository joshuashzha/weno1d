FC = gfortran 
FFLAGS = -O3

sources = Definition.f90 \
          AlphaSplit.f90 \
          BackUp.f90 \
          Boundary1D.f90 \
          Boundary2D.f90 \
          CheckRho.f90 \
          ConstC.f90 \
          ConstD.f90 \
          FindCentralDensity.f90 \
          FindDt.f90 \
          FindEnergy.f90 \
          FindMass.f90 \
          FindPotential.f90 \
          FindPressure.f90 \
          FromRVEToU.f90 \
          FromUToRVE.f90 \
          GetEpsilon.f90 \
          GetRho_EOSPToR.f90 \
          GetRho_EOSRToP.f90 \
          GetRho.f90 \
          GetRho_simple.f90 \
          GetVel.f90 \
          Ini_Der.f90 \
          Initial.f90 \
          Main.f90 \
          Output.f90 \
          PotentialRelax.f90 \
          RungeKutta.f90 \
          Spatial.f90 \
          Update.f90 \
          WENO.f90

objects = $(sources:.f90=.o)
star_weno: $(objects)
	$(FC) $(FFLAGS) -o star_weno $(objects)

$(objects): %.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -rf star_weno
	rm -rf *.o
	rm -rf *.mod

cleanfile:
	rm -rf Star_WENO_*
