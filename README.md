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

- Fortran, C, and C++ compilers
- MPI library
- NetCDF4
- HDF5
- NCO ProdUtils
- Jasper library
- Z library
- PNG library
- Environment module support (recommended)
- NCEPLibs (required for prep of forcing data)
- WGRIB2 (required for prep of forcing data)

RPMS of the above can be found in the following archives:

https://ioos-cloud-sandbox.s3.amazonaws.com/public/libs/nosofs_base_rpms.gcc.6.5.0.el7.20191011.tgz

https://ioos-cloud-sandbox.s3.amazonaws.com/public/libs/nosofs_all_rpms.gcc.6.5.0.el7.20191011.tgz

Updated RPMs: (NetCDF5, HDF5-IMPI, WGRIB2, Produtil): 

https://ioos-cloud-sandbox.s3.amazonaws.com/public/rpms/netcdf-4.5-1.el7.x86_64.rpm

https://ioos-cloud-sandbox.s3.amazonaws.com/public/rpms/hdf5-impi-1.8.21-1.el7.x86_64.rpm

https://ioos-cloud-sandbox.s3.amazonaws.com/public/rpms/wgrib2-2.0.8-2.el7.x86_64.rpm

https://ioos-cloud-sandbox.s3.amazonaws.com/public/rpms/produtil-1.0.18-2.el7.x86_64.rpm


If not using the RPMS above, other distributions can be found at the following:

Required for prep steps:
- NCEPLibs : https://github.com/NCAR/NCEPlibs
- WGRIB2 : https://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html
        
Required to run:

- Produtils - available on PMB website pmb/codes

Fixed field files are also needed: 
    
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
./SELFE_COMPILE.sh

(SELFE is untested)
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

### NGOFS crashes when using the namelist input file ```nos. ... .in```  from NOAA's NOMADS server.
Example: ngofs.20191206/nos.ngofs.forecast.20191206.t03z.in 

Solution: Two changes are required.
1. Change the following line: ```NC_SUBDOMAIN_FILES = FVCOM,```
   to: ```NC_SUBDOMAIN_FILES = 'FVCOM',``` (string value must be in quotes)
2. Change ``` NESTING_ON        = T,``` to ``` NESTING_ON        = F,```

### NGOFS crashes when writing the ```nos_ngofs_nestnode_*.nc``` files, when ```NCNEST_ON = T```

Some compilers will nullify pointers when they are declared. GFortran does not.

Solution: in ```sorc/FVCOM.fd/FVCOM_source/mod_nesting.F``` in ```SUBROUTINE DUMP_NCNEST_FILE``` add ```NULLIFY(TEMP1)``` after the variable declarations and re-build. This is the proper way to declare and initialize pointer variables.

## Licenses

Various - multiple components are contained herein.

## Additional Links

ecFlow : https://confluence.ecmwf.int/display/ECFLOW/ecflow+home

NOMADS : https://nomads.ncep.noaa.gov/pub/data/nccf/com/nos/prod/
   
