# nosofs-NCO

This is a fork of NOAA's National Ocean Service Operational Forecast System obtained from the [NCO PMB Website](https://www.nco.ncep.noaa.gov/pmb/codes/nwprod/). Note: NOAA does not maintain a publicly available source code repository.

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
    ├── ush               # Scripts containing the core logic
    └── README.md

## Getting Started

### Prerequisites
    - MPI Library
    - NCEPLibraries

### Building

### Running the tests

## Tested Platforms

## Changes

## License

https://confluence.ecmwf.int/display/ECFLOW/ecflow+home
