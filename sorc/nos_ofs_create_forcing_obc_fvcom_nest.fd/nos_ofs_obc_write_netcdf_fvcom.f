      subroutine nos_ofs_write_netCDF_obc_fvcom(GRIDFILE,netcdf_file,
     & ncid,imode,time_len,node_len,nele_len,siglay_len,siglev_len,
     & base_date,Itime,Itime2,Times,h,lat,lon,latc,lonc,nv,siglay,
     & siglev,zeta,temp,salinity,u,v,ua,va,partition,globalstr)
      include 'netcdf.inc'
      CHARACTER*120 TEXT,CNAME,netcdf_file,GRIDFILE
      INTEGER LEN,base_date(4),intval(4),CORNER(4),COUNT(4)
      CHARACTER (LEN=10) BIG_BEN(3),CURRENT_TIME*20
      INTEGER DATE_TIME(8)
      character globalstr(9)*120
* error status return
      integer  iret
* netCDF id
      integer  ncid,ncid1
* dimension ids
      integer  time_dim
      integer  node_dim
      integer  siglev_dim
      integer  nele_dim
      integer  three_dim
      integer  siglay_dim
      integer  DateStrLen_dim
* dimension lengths
      integer  time_len
      integer  node_len
      integer  siglev_len
      integer  nele_len
      integer  three_len
      integer  siglay_len
      integer  DateStrLen_len
      parameter (DateStrLen_len = 26)
* variable ids
      integer  Itime_id
      integer  Itime2_id
      integer  h_id
      integer  hyw_id
      integer  iint_id
      integer  lat_id
      integer  latc_id
      integer  lon_id
      integer  lonc_id
      integer  nprocs_id
      integer  nv_id
      integer  partition_id
      integer  salinity_id
      integer  siglay_id
      integer  siglev_id
      integer  temp_id
      integer  time_id
      integer  u_id
      integer  ua_id
      integer  v_id
      integer  va_id
      integer  x_id
      integer  xc_id
      integer  y_id
      integer  yc_id
      integer  zeta_id
      integer  Times_id
      integer  weight_cell_id
      integer  weight_node_id
* data variables
      character*26 Times(time_len)
      integer  Itime(time_len)
      integer  Itime2(time_len)
      integer  IINT(time_len)
      real  time(time_len)
      real  h(node_len)
      real  hyw(node_len,siglev_len,time_len)

      real  lat(node_len)
      real  lon(node_len)
      real  latc(nele_len)
      real  lonc(nele_len)
      integer  nprocs
      integer  nv(nele_len, 3)
      integer  partition(nele_len)
      real  siglay(node_len, siglay_len)
      real  siglev(node_len, siglev_len)
      real  x(node_len)
      real  y(node_len)
      real  xc(nele_len)
      real  yc(nele_len)
      real  zeta(node_len,time_len)
      real  temp(node_len, siglay_len,time_len)
      real  salinity(node_len, siglay_len,time_len)
      real  u(nele_len, siglay_len,time_len)
      real  v(nele_len, siglay_len,time_len)
      real  ua(nele_len,time_len)
      real  va(nele_len,time_len)
      real  weight_node(node_len,time_len)
      real  weight_cell(nele_len,time_len)
CCCCCCCCCCCCC 
      write(*,*)'start writing OBC NetCDF'
      if (imode.eq.1) then           ! Write the file message 
      nprocs=256
      DO I=1,node_len
        x(I)=0.0
        y(I)=0.0
!      DO K=1,siglay_len
!         siglay(I,K)=0.5*(siglev(I,K)+siglev(I,K+1))
!      ENDDO      
      ENDDO      
      DO I=1,nele_len
!        partition(I)=I
        xc(I)=0.0
        yc(I)=0.0
      ENDDO 
* attribute vectors
* enter define mode
      iret = nf_create(trim(netcdf_file), NF_CLOBBER, ncid)
      call check_err(iret)
* define dimensions
      iret = nf_def_dim(ncid, 'time', NF_UNLIMITED, time_dim)
!      iret = nf_def_dim(ncid, 'time', time_len, time_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 'node', node_len, node_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 'siglev',siglev_len , siglev_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 'nele',nele_len , nele_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 'three', 3, three_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 'siglay',siglay_len , siglay_dim)
      call check_err(iret)
      iret = nf_def_dim(ncid, 'DateStrLen', DateStrLen_len,
     1   DateStrLen_dim)
      call check_err(iret)
* define variables
      intval(1) = time_dim
      iret = nf_def_var(ncid, 'Itime', NF_INT, 1, 
     1intval, Itime_id)
      iret = nf_def_var(ncid, 'Itime2', NF_INT, 1,
     1intval, Itime2_id)
      call check_err(iret)
      iret = nf_def_var(ncid, 'time', NF_REAL, 1,intval, time_id)
      call check_err(iret)
      intval(2) = time_dim
      intval(1) = DateStrLen_dim
      iret = nf_def_var(ncid, 'Times', NF_CHAR, 2,intval,Times_id)
      call check_err(iret)

      intval(1) = node_dim
      iret = nf_def_var(ncid, 'h', NF_REAL, 1,intval , h_id)
      call check_err(iret)
      intval(3) = time_dim
      intval(2) = siglev_dim
      intval(1) = node_dim
      iret = nf_def_var(ncid, 'hyw', NF_REAL, 3,intval , hyw_id)
      call check_err(iret)
      
      intval(1) = time_dim
      iret = nf_def_var(ncid, 'iint', NF_INT, 1, intval, iint_id)
      call check_err(iret)
      intval(1) = node_dim
      iret = nf_def_var(ncid, 'lat', NF_REAL, 1, intval, lat_id)
      call check_err(iret)
      intval(1) = nele_dim
      iret = nf_def_var(ncid, 'latc', NF_REAL, 1, intval,latc_id)
      call check_err(iret)
      intval(1) = node_dim
      iret = nf_def_var(ncid, 'lon', NF_REAL, 1, intval, lon_id)
      call check_err(iret)
      intval(1) = nele_dim
      iret = nf_def_var(ncid, 'lonc', NF_REAL,1, intval, lonc_id)
      call check_err(iret)
      iret = nf_def_var(ncid, 'nprocs', NF_INT, 0, 0, nprocs_id)
      call check_err(iret)
      intval(2) = three_dim
      intval(1) = nele_dim
      iret = nf_def_var(ncid, 'nv', NF_INT, 2, intval, nv_id)
      call check_err(iret)
      intval(1) = nele_dim
      iret=nf_def_var(ncid,'partition',NF_INT,1,intval,partition_id)
      call check_err(iret)
      intval(2) = siglay_dim
      intval(1) = node_dim
      iret = nf_def_var(ncid, 'siglay',NF_REAL,2,intval,siglay_id)
      call check_err(iret)
      intval(2) = siglev_dim
      intval(1) = node_dim
      iret = nf_def_var(ncid, 'siglev', NF_REAL,2,intval,siglev_id)
      call check_err(iret)
      intval(3) = time_dim
      intval(2) = siglay_dim
      intval(1) = node_dim
      iret = nf_def_var(ncid, 'temp', NF_REAL,3,intval, temp_id)
      iret=nf_def_var(ncid,'salinity',NF_REAL,3,intval,salinity_id)
      call check_err(iret)
      intval(3) = time_dim
      intval(2) = siglay_dim
      intval(1) = nele_dim
      iret = nf_def_var(ncid, 'u', NF_REAL,3,intval, u_id)
      iret = nf_def_var(ncid, 'v', NF_REAL,3,intval, v_id)
      call check_err(iret)
      intval(2) = time_dim
      intval(1) = nele_dim
      iret = nf_def_var(ncid, 'ua', NF_REAL,2,intval, ua_id)
      iret = nf_def_var(ncid, 'va', NF_REAL,2,intval, va_id)
      call check_err(iret)
      intval(1) = node_dim
      iret = nf_def_var(ncid, 'x', NF_REAL,1,intval, x_id)
      iret = nf_def_var(ncid, 'y', NF_REAL,1,intval, y_id)
      call check_err(iret)
      intval(1) = nele_dim
      iret = nf_def_var(ncid, 'xc', NF_REAL,1,intval, xc_id)
      iret = nf_def_var(ncid, 'yc', NF_REAL,1,intval, yc_id)
      call check_err(iret)
      intval(2) = time_dim
      intval(1) = node_dim
      iret = nf_def_var(ncid, 'zeta', NF_REAL,2,intval,zeta_id)
      call check_err(iret)
      intval(2) = time_dim
      intval(1) = nele_dim
      iret = nf_def_var(ncid, 'weight_cell', NF_REAL,2,intval,
     &  weight_cell_id)
      call check_err(iret)
      intval(2) = time_dim
      intval(1) = node_dim
      iret = nf_def_var(ncid, 'weight_node', NF_REAL,2,intval,
     &  weight_node_id)
      call check_err(iret)
* assign attributes
      WRITE(TEXT,'(a11,I4,3(a1,I2.2),a6)')'days since ',base_date(1),
     & '-',base_date(2),'-',base_date(3),' ',base_date(4),':00:00'
      LEN=LEN_TRIM(TEXT)

      iret = nf_put_att_text(ncid, Itime_id, 'units',
     &       LEN,TRIM(TEXT))
      TEXT='modified julian day(MJD)'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, Itime_id, 'format',
     &       LEN,TRIM(TEXT))
      TEXT='UTC'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, Itime_id, 'time_zone',
     &       LEN,TRIM(TEXT))
      TEXT='mseconds from integer day'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, Itime2_id, 'long_name',
     &       LEN,TRIM(TEXT))
      TEXT='msec since 00:00:00'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, Itime2_id, 'units',
     &       LEN,TRIM(TEXT))
      TEXT='modified julian day(MJD)'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, Itime2_id, 'format',
     &       LEN,TRIM(TEXT))
      TEXT='UTC'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, Itime2_id, 'time_zone',
     &       LEN,TRIM(TEXT))

      TEXT='Calendar Date'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, Times_id, 'long_name',
     &       LEN,TRIM(TEXT))
      TEXT='String: Calendar Time'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, Times_id, 'format',
     &       LEN,TRIM(TEXT))
      TEXT='UTC'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, Times_id, 'time_zone',
     &       LEN,TRIM(TEXT))
      iret = nf_put_att_text(ncid, h_id, 'long_name', 10, 'Bathymetry')
      call check_err(iret)
      iret = nf_put_att_text(ncid, h_id, 'standard_name', 27, 
     1'sea_floor_depth_below_geoid')
      call check_err(iret)
      iret = nf_put_att_text(ncid, h_id, 'units', 1, 'm')
      call check_err(iret)
      iret = nf_put_att_text(ncid, h_id, 'positive', 4, 'down')
      call check_err(iret)
      iret = nf_put_att_text(ncid, h_id, 'grid', 15, 'Bathymetry_Mesh')
      call check_err(iret)
      iret = nf_put_att_text(ncid, h_id, 'coordinates', 7, 'lat lon')
      call check_err(iret)
      iret = nf_put_att_text(ncid, h_id, 'type', 4, 'data')
      call check_err(iret)
      iret = nf_put_att_text(ncid, hyw_id, 'long_name', 30, 
     1'hydro static vertical velocity')
      call check_err(iret)
      iret = nf_put_att_text(ncid, hyw_id, 'units', 3, 'm/s')
      call check_err(iret)
      iret = nf_put_att_text(ncid, hyw_id, 'grid', 10, 'fvcom_grid')
      call check_err(iret)
      iret = nf_put_att_text(ncid, hyw_id, 'coordinates', 7, 'lat lon')
      call check_err(iret)
      iret = nf_put_att_text(ncid, hyw_id, 'type', 4, 'data')
      call check_err(iret)
      iret = nf_put_att_text(ncid, iint_id, 'long_name', 30, 
     1'internal mode iteration number')
      call check_err(iret)
      iret = nf_put_att_text(ncid, lat_id, 'long_name', 14, 
     1'nodal latitude')
      call check_err(iret)
      iret = nf_put_att_text(ncid, lat_id, 'standard_name', 8, 
     1'latitude')
      call check_err(iret)
      iret = nf_put_att_text(ncid, lat_id, 'units', 13, 
     1 'degrees_north')
      call check_err(iret)
      iret = nf_put_att_text(ncid, latc_id, 'long_name', 14, 
     1'zonal latitude')
      call check_err(iret)
      iret = nf_put_att_text(ncid, latc_id, 'standard_name', 8, 
     1'latitude')
      call check_err(iret)
      iret = nf_put_att_text(ncid, latc_id, 'units', 13, 
     1'degrees_north')
      call check_err(iret)
      iret = nf_put_att_text(ncid, lon_id, 'long_name', 15, 
     1'nodal longitude')
      call check_err(iret)
      iret = nf_put_att_text(ncid, lon_id, 'standard_name', 9, 
     1'longitude')
      call check_err(iret)
      iret = nf_put_att_text(ncid, lon_id, 'units', 12, 
     1 'degrees_east')
      call check_err(iret)
      iret = nf_put_att_text(ncid, lonc_id, 'long_name', 15, 
     1'zonal longitude')
      call check_err(iret)
      iret = nf_put_att_text(ncid, lonc_id, 'standard_name', 9, 
     1'longitude')
      call check_err(iret)
      iret = nf_put_att_text(ncid, lonc_id, 'units', 12, 
     1 'degrees_east')
      call check_err(iret)
      iret = nf_put_att_text(ncid, nprocs_id, 'long_name', 20, 
     1'number of processors')
      call check_err(iret)
      iret = nf_put_att_text(ncid, nv_id, 'long_name', 25, 
     1'nodes surrounding element')
      call check_err(iret)
      iret = nf_put_att_text(ncid, partition_id, 'long_name', 9, 
     1'partition')
      call check_err(iret)
      iret = nf_put_att_text(ncid, salinity_id, 'long_name', 8, 
     1'salinity')
      call check_err(iret)
      iret = nf_put_att_text(ncid, salinity_id, 'standard_name', 18, 
     1'sea_water_salinity')
      call check_err(iret)
      iret = nf_put_att_text(ncid, salinity_id, 'units', 4, '1e-3')
      call check_err(iret)
      iret = nf_put_att_text(ncid, salinity_id, 'grid', 10, 
     1'fvcom_grid')
      call check_err(iret)
      iret = nf_put_att_text(ncid, salinity_id, 'coordinates', 7, 
     1'lat lon')
      call check_err(iret)
      iret = nf_put_att_text(ncid, salinity_id, 'type', 4, 'data')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglay_id, 'long_name', 12, 
     1'Sigma Layers')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglay_id, 'standard_name', 30, 
     1'ocean_sigma/general_coordinate')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglay_id, 'positive', 2, 'up')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglay_id, 'valid_min', 2, '-1')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglay_id, 'valid_max', 1, '0')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglay_id, 'formula_terms', 32, 
     1'sigma: siglay eta: zeta depth: h')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglev_id, 'long_name', 12, 
     1'Sigma Levels')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglev_id, 'standard_name', 30, 
     1'ocean_sigma/general_coordinate')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglev_id, 'positive', 2, 'up')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglev_id, 'valid_min', 2, '-1')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglev_id, 'valid_max', 1, '0')
      call check_err(iret)
      iret = nf_put_att_text(ncid, siglev_id, 'formula_terms', 31, 
     1'sigma:siglay eta: zeta depth: h')
      call check_err(iret)
      iret = nf_put_att_text(ncid, temp_id, 'long_name', 11, 
     1'temperature')
      call check_err(iret)
      iret = nf_put_att_text(ncid, temp_id, 'standard_name', 21, 
     1'sea_water_temperature')
      call check_err(iret)
      iret = nf_put_att_text(ncid, temp_id, 'units', 9, 'degrees_C')
      call check_err(iret)
      iret = nf_put_att_text(ncid, temp_id, 'grid', 10, 'fvcom_grid')
      call check_err(iret)
      iret = nf_put_att_text(ncid, temp_id, 'coordinates',7,'lat lon')
      call check_err(iret)
      iret = nf_put_att_text(ncid, temp_id, 'type', 4, 'data')
      call check_err(iret)
      iret = nf_put_att_text(ncid, time_id, 'long_name', 4, 'time')
      call check_err(iret)
      iret = nf_put_att_text(ncid, time_id, 'units', 30, 
     1'days since 1858-11-17 00:00:00')
      call check_err(iret)
      iret = nf_put_att_text(ncid, time_id, 'format', 25, 
     1'modified julian day (MJD)')
      call check_err(iret)
      iret = nf_put_att_text(ncid, time_id, 'time_zone', 3, 'UTC')
      call check_err(iret)
      iret = nf_put_att_text(ncid, u_id, 'long_name', 23, 
     1'Eastward Water Velocity')
      call check_err(iret)
      iret = nf_put_att_text(ncid, u_id, 'units', 10, 'meters s-1')
      call check_err(iret)
      iret = nf_put_att_text(ncid, u_id, 'grid', 10, 'fvcom_grid')
      call check_err(iret)
      iret = nf_put_att_text(ncid, u_id, 'type', 4, 'data')
      call check_err(iret)
      
      iret = nf_put_att_text(ncid, ua_id, 'long_name', 30, 
     1'Vertically Averaged x-velocity')
      call check_err(iret)
      iret = nf_put_att_text(ncid, ua_id, 'units', 10, 'meters s-1')
      call check_err(iret)
      iret = nf_put_att_text(ncid, ua_id, 'grid', 10, 'fvcom_grid')
      call check_err(iret)
      iret = nf_put_att_text(ncid, ua_id, 'type', 4, 'data')
      call check_err(iret)
      iret = nf_put_att_text(ncid, v_id, 'long_name', 24, 
     1'Northward Water Velocity')
      call check_err(iret)
      iret = nf_put_att_text(ncid, v_id, 'units', 10, 'meters s-1')
      call check_err(iret)
      iret = nf_put_att_text(ncid, v_id, 'grid', 10, 'fvcom_grid')
      call check_err(iret)
      iret = nf_put_att_text(ncid, v_id, 'type', 4, 'data')
      call check_err(iret)
      iret = nf_put_att_text(ncid, va_id, 'long_name', 30, 
     1'Vertically Averaged y-velocity')
      call check_err(iret)
      iret = nf_put_att_text(ncid, va_id, 'units', 10, 'meters s-1')
      call check_err(iret)
      iret = nf_put_att_text(ncid, va_id, 'grid', 10, 'fvcom_grid')
      call check_err(iret)
      iret = nf_put_att_text(ncid, va_id, 'type', 4, 'data')
      call check_err(iret)
      iret = nf_put_att_text(ncid, x_id, 'long_name', 18, 
     1'nodal x-coordinate')
      call check_err(iret)
      iret = nf_put_att_text(ncid, x_id, 'units', 6, 'meters')
      call check_err(iret)
      iret = nf_put_att_text(ncid, xc_id, 'long_name', 18, 
     1'zonal x-coordinate')
      call check_err(iret)
      iret = nf_put_att_text(ncid, xc_id, 'units', 6, 'meters')
      call check_err(iret)
      iret = nf_put_att_text(ncid, y_id, 'long_name', 18, 
     1'nodal y-coordinate')
      call check_err(iret)
      iret = nf_put_att_text(ncid, y_id, 'units', 6, 'meters')
      call check_err(iret)
      iret = nf_put_att_text(ncid, yc_id, 'long_name', 18, 
     1'zonal y-coordinate')
      call check_err(iret)
      iret = nf_put_att_text(ncid, yc_id, 'units', 6, 'meters')
      call check_err(iret)
      iret = nf_put_att_text(ncid, zeta_id, 'long_name', 23, 
     1'Water Surface Elevation')
      call check_err(iret)
      iret = nf_put_att_text(ncid, zeta_id, 'units', 6, 'meters')
      call check_err(iret)
      iret = nf_put_att_text(ncid, zeta_id, 'positive', 2, 'up')
      call check_err(iret)
      iret = nf_put_att_text(ncid, zeta_id, 'standard_name', 21, 
     1'sea_surface_elevation')
      call check_err(iret)
      iret = nf_put_att_text(ncid, zeta_id, 'grid', 8, 'SSH_Mesh')
      call check_err(iret)
      iret = nf_put_att_text(ncid, zeta_id, 'coordinates', 7, 
     1 'lat lon')
      call check_err(iret)
      iret = nf_put_att_text(ncid, zeta_id, 'type', 4, 'data')
      call check_err(iret)
!      iret = nf_put_att_text(ncid, Times_id, 'time_zone', 3, 'UTC')
!      call check_err(iret)
      iret = nf_put_att_text(ncid, weight_cell_id, 'long_name', 24, 
     1'Nesting Weights at cells')
      call check_err(iret)
      iret = nf_put_att_text(ncid, weight_cell_id, 'units', 14, 
     1'nondimensional')
      call check_err(iret)
      iret = nf_put_att_text(ncid, weight_cell_id, 'grid', 10, 
     1'fvcom_grid')
      call check_err(iret)
      iret = nf_put_att_text(ncid, weight_cell_id, 'type', 4, 'data')
      call check_err(iret)

      TEXT='Nesting Weights at nodes'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, weight_node_id, 'long_name', LEN, 
     1TRIM(TEXT))
      call check_err(iret)

      TEXT='nondimensional'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, weight_node_id, 'units', LEN, 
     1TRIM(TEXT))
      call check_err(iret)
      TEXT='fvcom_grid'
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, weight_node_id, 'grid', LEN, 
     1TRIM(TEXT))
      call check_err(iret)
      iret = nf_put_att_text(ncid, weight_node_id, 'type', 4, 'data')
      call check_err(iret)

C Global Attributes
      TEXT=trim(globalstr(1))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid,NF_GLOBAL ,'type', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(2))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'title', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(3))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'data_source', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(4))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'Temp_source', 
     &       LEN,TRIM(TEXT))

      TEXT=trim(globalstr(5))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'model_grid_file', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(6))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'output_file', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(7))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'source_code', 
     &       LEN,TRIM(TEXT))
     
      TEXT=trim(globalstr(8))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'history', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(globalstr(9))
      LEN=LEN_TRIM(TEXT)
      iret = nf_put_att_text(ncid, NF_GLOBAL,'reference', 
     &       LEN,TRIM(TEXT))

* leave define mode
      iret = nf_enddef(ncid)
      call check_err(iret)
      print *,'end of define mode' 
* Write record variables
      iret=nf_put_vara_int(ncid,nprocs_id,1,1,nprocs)
      CORNER(1) = 1 
      COUNT(1)=node_len
      iret=nf_put_vara_real(ncid,h_id,CORNER,COUNT,h)
      iret=nf_put_vara_real(ncid,lat_id,CORNER,COUNT,lat)
      iret=nf_put_vara_real(ncid,lon_id,CORNER,COUNT,lon)
      iret=nf_put_vara_real(ncid,x_id,CORNER,COUNT,x)
      iret=nf_put_vara_real(ncid,y_id,CORNER,COUNT,y)
      CORNER(1) = 1 
      COUNT(1)=nele_len
      iret=nf_put_vara_real(ncid,latc_id,CORNER,COUNT,latc)
      iret=nf_put_vara_real(ncid,lonc_id,CORNER,COUNT,lonc)
      iret=nf_put_vara_real(ncid,xc_id,CORNER,COUNT,xc)
      iret=nf_put_vara_real(ncid,yc_id,CORNER,COUNT,yc)
      iret=nf_put_vara_int(ncid,partition_id,CORNER,COUNT,partition)
      CORNER(1) = 1
      CORNER(2) = 1
      COUNT(1)=nele_len
      COUNT(2)=3
      iret=nf_put_vara_int(ncid,nv_id,CORNER,COUNT,nv)
      CORNER(1) = 1
      CORNER(2) = 1
      COUNT(1)=node_len
      COUNT(2)=siglay_len
      iret=nf_put_vara_real(ncid,siglay_id,CORNER,COUNT,siglay)
      CORNER(1) = 1
      CORNER(2) = 1
      COUNT(1)=node_len
      COUNT(2)=siglev_len
      iret=nf_put_vara_real(ncid,siglev_id,CORNER,COUNT,siglev)

      elseif (imode.eq.2) then         ! Write the data into the file 
      iret= nf_inq_dimlen(ncid,time_dim,jtime)
      jtime=jtime+1

      DO I=1,node_len
        DO N=1,time_len
	 weight_node(I,N)=1.0
         DO K=1,siglev_len
           hyw(I,K,N)=0.0
         ENDDO      
        ENDDO      
      ENDDO      
      DO I=1,nele_len
        DO N=1,time_len
	 weight_cell(I,N)=1.0
        ENDDO      
      ENDDO 
      DO N=1,time_len
        iint(N)=jtime
	ITT=Itime2(N)/1000
	time(N)=Itime(N)+ITT/86400.
      ENDDO	


      CORNER(1) = jtime 
      COUNT(1)=time_len
      iret=nf_put_vara_int(ncid,Itime_id,CORNER,COUNT,Itime)
      iret=nf_put_vara_int(ncid,Itime2_id,CORNER,COUNT,Itime2)
      iret=nf_put_vara_real(ncid,time_id,CORNER,COUNT,time)
      iret=nf_put_vara_int(ncid,iint_id,CORNER,COUNT,iint)
      call check_err(iret)
      CORNER(1) = 1
      CORNER(2) = jtime
      COUNT(1)=26
      COUNT(2)=1
      iret=nf_put_vara_text(ncid,Times_id,CORNER,COUNT,Times)
      call check_err(iret)
      CORNER(1) = 1
      CORNER(2) = jtime
      COUNT(1)=node_len
      COUNT(2)=time_len
      iret=nf_put_vara_real(ncid,zeta_id,CORNER,COUNT,zeta)
      iret=nf_put_vara_real(ncid,weight_node_id,CORNER,COUNT,
     1 weight_node)
      CORNER(1) = 1
      CORNER(2) = jtime
      COUNT(1)=nele_len
      COUNT(2)=time_len
      iret=nf_put_vara_real(ncid,ua_id,CORNER,COUNT,ua)
      iret=nf_put_vara_real(ncid,va_id,CORNER,COUNT,va)
      iret=nf_put_vara_real(ncid,weight_cell_id,CORNER,COUNT,
     1 weight_cell)
      CORNER(1) = 1
      CORNER(2) = 1
      CORNER(3) = jtime
      COUNT(1)=node_len
      COUNT(2)=siglay_len
      COUNT(3)=time_len
      iret=nf_put_vara_real(ncid,salinity_id,CORNER,COUNT,salinity)
      iret=nf_put_vara_real(ncid,temp_id,CORNER,COUNT,temp)
      CORNER(1) = 1
      CORNER(2) = 1
      CORNER(3) = jtime
      COUNT(1)=nele_len
      COUNT(2)=siglay_len
      COUNT(3)=time_len
      iret=nf_put_vara_real(ncid,u_id,CORNER,COUNT,u)
      iret=nf_put_vara_real(ncid,v_id,CORNER,COUNT,v)
      CORNER(1) = 1
      CORNER(2) = 1
      CORNER(3) = jtime
      COUNT(1)=node_len
      COUNT(2)=siglev_len
      COUNT(3)=time_len
      iret=nf_put_vara_real(ncid,hyw_id,CORNER,COUNT,hyw)

      elseif (imode.eq.3) then             ! Close the netCDF  
      iret = nf_close(ncid)
      endif
      call check_err(iret)
      RETURN
      end
       
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      subroutine check_err(status)
      integer status
      include 'netcdf.inc'
      if (status .ne. NF_NOERR) then
      print *,'status=',status
      print *, nf_strerror(status)
      stop
      endif
      return
      end
