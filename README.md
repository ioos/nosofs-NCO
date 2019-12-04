# nosofs-NCO

v3.2.1

This is a fork of NOAA's National Ocean Service Operational Forecast System obtained from NOAA's PMB website:  https://www.nco.ncep.noaa.gov/pmb/codes/nwprod/ /nosofs.v[VERSION]

*NOAA does not maintain a publicly available source code repository.*

### Directory structure

    .
    ├── ecf               # ecFlow workflow task scripts
    ├── jobs              # "JJOB" scripts; now/forecast and prep launch scripts
    ├── lsf               # Specific task scripts for LSF scheduler
    ├── modulefiles       # Environment modules
    ├── parm              # Used by NCO for data dissemination
    ├── scripts           # Main launch scripts
    ├── sorc              # Model source code and some 3rd party libraries
    │   ├── FVCOM.fd
    │   ├── ROMS.fd
    │   ├── SELFE.fd
    │   └── nos_*.fd      # Programs for preparing forcing data
    ├── ush               # Scripts containing the core logic
    └── README.md

## Getting Started

### Prerequisites
    - Fortran, C, and C++ compilers
    - MPI library
    - NetCDF4
    - HDF5
    - jasper library
    - z library
    - png library
    - Environment module support (recommended)
    
    Required for prep steps:
        - NCEPLibs
        - WGRIB2
        
    Required to run:
    
        Download fixed field files and place them in the 'fix' directory. 
        .
        ├── fix
            ├── shared
            ├── cbofs | ngofs | etc.
   
    Fixed fields can be obtained from NOAA's PMB website:
    https://www.nco.ncep.noaa.gov/pmb/codes/nwprod /nosofs.v[VERSION]

### Building

To build everything:
    
```
cd sorc
./COMPILE.sh
```
    
### Running the tests

## Tested Platforms

## Changes

## License

## Additional Links
    https://confluence.ecmwf.int/display/ECFLOW/ecflow+home
