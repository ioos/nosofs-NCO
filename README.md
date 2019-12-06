# nosofs-NCO

v3.2.1

This is a fork of NOAA's National Ocean Service Operational Forecast System obtained from NOAA's PMB website:  https://www.nco.ncep.noaa.gov/pmb/codes/nwprod/ nosofs.v[VERSION]

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

Base: https://ioos-cloud-sandbox.s3.amazonaws.com/public/libs/nosofs_base_rpms.gcc.6.5.0.el7.20191011.tgz

All: https://ioos-cloud-sandbox.s3.amazonaws.com/public/libs/nosofs_all_rpms.gcc.6.5.0.el7.20191011.tgz

- Fortran, C, and C++ compilers
- MPI library
- NetCDF4
- HDF5
- jasper library
- z library
- png library
- Environment module support (recommended)
    
Required for prep steps:
- NCEPLibs : https://github.com/NCAR/NCEPlibs
- WGRIB2 : https://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html
        
Required to run:

- Produtils - available on PMB website pmb/codes

Also need fixed fields: 
    
    Download fixed field files and place them in the 'fix' directory. 
    .
    ├── fix
        ├── shared
        ├── cbofs | ngofs | etc.
   
Fixed fields can be obtained from NOAA's PMB website:
https://www.nco.ncep.noaa.gov/pmb/codes/nwprod nosofs.v[VERSION]

Some are also available at https://ioos-cloud-sandbox.s3.amazonaws.com/public/nosofs/fix.
    
### Building

To build:
    
```
cd sorc
./ROMS_COMPILE.sh
./FVCOM_COMPILE.sh

SELFE is untested
```

### Running the tests

CBOFS example:
    
* Obtain ICs from AWS public S3 bucket:  
  https://ioos-cloud-sandbox.s3.amazonaws.com/public/cbofs/ICs.cbofs.2019100100.tgz
* Untar the ICs into /com/nos, cbofs.20191001 directory is in the tar ball.    
* Edit ./jobs/fcstrun.sh to make sure the paths and other parameters are correct for your system.
* Edit /com/nos/cbofs.20191001/nos.cbofs.forecast.20191001.t00z.in so that NtileI x NtileJ == number of CPUs available == NPP in fcstrun.sh
  
Example:
```
mkdir -p /com/nos
cd /com/nos
wget https://ioos-cloud-sandbox.s3.amazonaws.com/public/cbofs/ICs.cbofs.2019100100.tgz
tar -xvf ICs.cbofs.2019100100.tgz
vi ./cbofs.20191001/nos.cbofs.forecast.20191001.t00z.in
cd /save/nosofs-NCO/jobs
vi ./fcstrun.sh
./fcstrun.sh 20191001 00
```
     
### Running the model
    
Nowcast/Forecast
    
* Obtain initial conditions and required meteorological forcing 
    NOAA maintains the past two days of NOS forecasts on the NOMADS server: 
    https://nomads.ncep.noaa.gov/pub/data/nccf/com/nos/prod/

Example CBOFS:
```
cd ./jobs
./getICs.sh 20191204 00
```
* Run the model - follow the procedure in "Running the tests" above.
    
        
## Tested Platforms

    Intel X86-64
        GCC 6.5.0
        IntelMPI 2018,2019,2020
        OpenMPI 3,4
        MPICH 2,3,4
            CentOS7 - AWS EC2 and Docker
            RHEL7   - AWS EC2
            AmazonLinux - AWS EC2
  
## TODO List

Create test scripts that are decoupled from operational scripts and are easier to use.
Improve getICs.sh and other helper scripts.
Put prerequisite libraries RPMS in S3 bucket.
(... and much more)
    
## Gotchas
    
### FVCOM based models hang when using HyperThreads on EC2 instances.
Solution: Either disable HyperThreads or use non-default mpirun bindings.

Example on 48core/96vcpu machine with 2 24 core numa regions:
```
mpirun -bind-to numa:2 -map-by C
```
Depending on the specific system architecture, the bindings needed may be different than the above.

### FVCOM crashes with the nos.XXXX.now/forecast.X.X.in namelist file from NOAA's NOMADS daily forcing data
Example: https://nomads.ncep.noaa.gov/pub/data/nccf/com/nos/prod/ngofs.20191206/nos.ngofs.forecast.20191206.t03z.in
Reason:  The following line is the culprit: ```NC_SUBDOMAIN_FILES = FVCOM,```

Solution: Change the line to: ```NC_SUBDOMAIN_FILES = 'FVCOM',``` (string value must be in quotes)

## Licenses

Various - multiple components are contained herein.

## Additional Links

ecFlow : https://confluence.ecmwf.int/display/ECFLOW/ecflow+home

NOMADS : https://nomads.ncep.noaa.gov/pub/data/nccf/com/nos/prod/
   
