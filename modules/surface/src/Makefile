# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

OBJS = NamelistRead.o \
       LevelsType.o   \
       DomainType.o   \
       OptionsType.o  \
       ParametersType.o  \
       ForcingType.o \
       EnergyType.o \
       WaterType.o  \
       WaterModule.o \
       CanopyWaterModule.o \
       SnowWaterModule.o \
       SnowWaterRenew.o \
       SnowLayerChange.o \
       SoilWaterModule.o \
       ForcingModule.o \
       AtmProcessing.o \
       InterceptionModule.o \
       AlbedoModule.o \
       ShortwaveRadiationModule.o \
       ThermalPropertiesModule.o \
       PrecipHeatModule.o \
       EtFluxModule.o \
       SnowSoilTempModule.o \
       EnergyModule.o \
       UtilitiesModule.o

all:	$(OBJS)

.f90.o:
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(*).f90

#
# This command cleans up object (etc) files:
#

clean:
	rm -f *.o *.mod

#
# Dependencies:
#

LevelsType.o: NamelistRead.o 
DomainType.o: NamelistRead.o LevelsType.o 
OptionsType.o: NamelistRead.o 
ParametersType.o: NamelistRead.o 
ForcingType.o: NamelistRead.o
EnergyType.o: NamelistRead.o
WaterType.o: NamelistRead.o
WaterModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
               ForcingType.o WaterType.o SoilWaterModule.o CanopyWaterModule.o SnowWaterModule.o
CanopyWaterModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
                     ForcingType.o WaterType.o 
SnowLayerChange.o: LevelsType.o DomainType.o ParametersType.o EnergyType.o WaterType.o
SnowWaterRenew.o: LevelsType.o DomainType.o ParametersType.o EnergyType.o WaterType.o SnowLayerChange.o
SnowWaterModule.o: LevelsType.o DomainType.o ParametersType.o EnergyType.o WaterType.o \
                   SnowWaterRenew.o SnowLayerChange.o
SoilWaterModule.o: OptionsType.o LevelsType.o ParametersType.o WaterType.o
WaterModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
               ForcingType.o WaterType.o SoilWaterModule.o CanopyWaterModule.o SnowWaterModule.o
ForcingModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
               ForcingType.o WaterType.o AtmProcessing.o
AtmProcessing.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
               ForcingType.o WaterType.o
InterceptionModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
               ForcingType.o WaterType.o
ThermalPropertiesModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
               ForcingType.o WaterType.o
AlbedoModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
               ForcingType.o WaterType.o 
ShortwaveRadiationModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
                ForcingType.o WaterType.o AlbedoModule.o
PrecipHeatModule.o: ParametersType.o EnergyType.o ForcingType.o WaterType.o
EtFluxModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
                ForcingType.o WaterType.o
SnowSoilTempModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
               ForcingType.o WaterType.o
EnergyModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
                ForcingType.o WaterType.o ThermalPropertiesModule.o ShortwaveRadiationModule.o \
                PrecipHeatModule.o EtFluxModule.o SnowSoilTempModule.o
UtilitiesModule.o: DomainType.o ForcingType.o EnergyType.o

