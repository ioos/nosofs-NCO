!  lf95 write_netCDF_OBC_ROMS.f -I/usr/local/include -L/usr/local/lib -lnetcdf -o write_surface_forcing.x
!      character*80 netcdf_file
!      integer base_date(4)
!      netcdf_file='test_OBC.nc'
!      call write_netCDF_OBC_ROMS(netcdf_file,ncid,1,
!     & 20,45,21,12,13,
!     & 14,11,11,base_date,
!     & 1,0,0,1,1,
!     & 0,0,1,0,
!     & 0,0,0,0,
!     & 0,0,0,0,
!     & 0,0,0,0,
!     & 0,0,0,0,
!     & 0,0,1,0,
!     & 0,0,1,0)

 !     call write_netCDF_OBC_ROMS(netcdf_file,ncid,3,
 !    & 20,45,21,12,13,
 !    & 14,11,11,base_date,
 !    & zeta_time,v2d_time,v3d_time,temp_time,salt_time,
 !   & zeta_west,zeta_east,zeta_south,zeta_north,
 !    & ubar_west,ubar_east,ubar_south,ubar_north,
 !    & vbar_west,vbar_east,vbar_south,vbar_north,
 !    & u_west,u_east,u_south,u_north,
 !    & v_west,v_east,v_south,v_north,
 !    & temp_west,temp_east,temp_south,temp_north,
 !    & salt_west,salt_east,salt_south,salt_north)
 !      end

      subroutine write_netCDF_OBC_ROMS(netcdf_file,ncid,imode,ioxyg,
     & xi_rho_len,eta_rho_len,s_rho_len,zeta_time_len,v2d_time_len,
     & v3d_time_len,temp_time_len,salt_time_len,base_date,
     & zeta_time,v2d_time,v3d_time,temp_time,salt_time,
     & zeta_west,zeta_east,zeta_south,zeta_north,
     & ubar_west,ubar_east,ubar_south,ubar_north,
     & vbar_west,vbar_east,vbar_south,vbar_north,
     & u_west,u_east,u_south,u_north,
     & v_west,v_east,v_south,v_north,
     & temp_west,temp_east,temp_south,temp_north,
     & salt_west,salt_east,salt_south,salt_north,
     & do_west,do_east,do_south,do_north,globalstr)
     
CC     xi_rho = IM in x-direction/longitude
CC     eta_rho = JM in y-dirrection/latitude
      include 'netcdf.inc'
      CHARACTER*80 CNAME,netcdf_file
      CHARACTER*120 TEXT
      INTEGER LEN,base_date(4),intval(4),CORNER(4),COUNT(4)
      CHARACTER (LEN=10) BIG_BEN(3),CURRENT_TIME*20
      INTEGER DATE_TIME(8)
      REAL START_TIME,END_TIME      
!! dimension lengths
      integer  xi_rho_len
      integer  xi_u_len
      integer  xi_v_len
      integer  eta_rho_len
      integer  eta_u_len
      integer  eta_v_len
      integer  s_rho_len
      integer  s_w_len
      integer  zeta_time_len
      integer  v2d_time_len
      integer  v3d_time_len
      integer  temp_time_len
      integer  salt_time_len

!* dimension ids
      integer  xi_rho_dim
      integer  xi_u_dim
      integer  xi_v_dim
      integer  eta_rho_dim
      integer  eta_u_dim
      integer  eta_v_dim
      integer  s_rho_dim
      integer  s_w_dim
      integer  zeta_time_dim
      integer  v2d_time_dim
      integer  v3d_time_dim
      integer  temp_time_dim
      integer  salt_time_dim
!!* variable ids
      integer  zeta_time_id
      integer  v2d_time_id
      integer  v3d_time_id
      integer  temp_time_id
      integer  salt_time_id
      integer  zeta_west_id
      integer  zeta_east_id
      integer  zeta_south_id
      integer  zeta_north_id
      integer  ubar_west_id
      integer  ubar_east_id
      integer  ubar_south_id
      integer  ubar_north_id
      integer  vbar_west_id
      integer  vbar_east_id
      integer  vbar_south_id
      integer  vbar_north_id
      integer  u_west_id
      integer  u_east_id
      integer  u_south_id
      integer  u_north_id
      integer  v_west_id
      integer  v_east_id
      integer  v_south_id
      integer  v_north_id
      integer  temp_west_id
      integer  temp_east_id
      integer  temp_south_id
      integer  temp_north_id
      integer  salt_west_id
      integer  salt_east_id
      integer  salt_south_id
      integer  salt_north_id

C   Add by L. Zheng for the 1-Term DO simulation
      integer  do_west_id
      integer  do_east_id
      integer  do_south_id
      integer  do_north_id
C   Add by L. Zheng for the 1-Term DO simulation

* data variables
      real  zeta_time(zeta_time_len)
      real  v2d_time(v2d_time_len)
      real  v3d_time(v3d_time_len)
      real  temp_time(temp_time_len)
      real  salt_time(salt_time_len)
      real  zeta_west(eta_rho_len, zeta_time_len)
      real  zeta_east(eta_rho_len, zeta_time_len)
      real  zeta_south(xi_rho_len, zeta_time_len)
      real  zeta_north(xi_rho_len, zeta_time_len)
      real  ubar_west(eta_rho_len, v2d_time_len)
      real  ubar_east(eta_rho_len, v2d_time_len)
      real  ubar_south(xi_rho_len-1, v2d_time_len)
      real  ubar_north(xi_rho_len-1, v2d_time_len)
      real  vbar_west(eta_rho_len-1, v2d_time_len)
      real  vbar_east(eta_rho_len-1, v2d_time_len)
      real  vbar_south(xi_rho_len, v2d_time_len)
      real  vbar_north(xi_rho_len, v2d_time_len)
      real  u_west(eta_rho_len, s_rho_len, v3d_time_len)
      real  u_east(eta_rho_len, s_rho_len, v3d_time_len)
      real  u_south(xi_rho_len-1, s_rho_len, v3d_time_len)
      real  u_north(xi_rho_len-1, s_rho_len, v3d_time_len)
      real  v_west(eta_rho_len-1, s_rho_len, v3d_time_len)
      real  v_east(eta_rho_len-1, s_rho_len, v3d_time_len)
      real  v_south(xi_rho_len, s_rho_len, v3d_time_len)
      real  v_north(xi_rho_len, s_rho_len, v3d_time_len)
      real  temp_west(eta_rho_len, s_rho_len, temp_time_len)
      real  temp_east(eta_rho_len, s_rho_len, temp_time_len)
      real  temp_south(xi_rho_len, s_rho_len, temp_time_len)
      real  temp_north(xi_rho_len, s_rho_len, temp_time_len)
      real  salt_west(eta_rho_len, s_rho_len, salt_time_len)
      real  salt_east(eta_rho_len, s_rho_len, salt_time_len)
      real  salt_south(xi_rho_len, s_rho_len, salt_time_len)
      real  salt_north(xi_rho_len, s_rho_len, salt_time_len)
      character globalstr(9)*120

C   Add by L. Zheng for the 1-Term DO simulation
      real  do_west(eta_rho_len, s_rho_len, temp_time_len)
      real  do_east(eta_rho_len, s_rho_len, temp_time_len)
      real  do_south(xi_rho_len, s_rho_len, temp_time_len)
      real  do_north(xi_rho_len, s_rho_len, temp_time_len)
C   Add by L. Zheng for the 1-Term DO simulation

!!* variable ids
      logical  zeta_time_L
      logical  v2d_time_L
      logical  v3d_time_L
      logical  temp_time_L
      logical  salt_time_L
      logical  zeta_west_L
      logical  zeta_east_L
      logical  zeta_south_L
      logical  zeta_north_L
      logical  ubar_west_L
      logical  ubar_east_L
      logical  ubar_south_L
      logical  ubar_north_L
      logical  vbar_west_L
      logical  vbar_east_L
      logical  vbar_south_L
      logical  vbar_north_L
      logical  u_west_L
      logical  u_east_L
      logical  u_south_L
      logical  u_north_L
      logical  v_west_L
      logical  v_east_L
      logical  v_south_L
      logical  v_north_L
      logical  temp_west_L
      logical  temp_east_L
      logical  temp_south_L
      logical  temp_north_L
      logical  salt_west_L
      logical  salt_east_L
      logical  salt_south_L
      logical  salt_north_L

C   Add by L. Zheng for the 1-Term DO simulation
      logical  do_west_L
      logical  do_east_L
      logical  do_south_L
      logical  do_north_L
C   Add by L. Zheng for the 1-Term DO simulation

C save all dimention id and variable id for next call
      save eta_rho_dim,eta_u_dim,eta_v_dim
      save xi_rho_dim,xi_u_dim,xi_v_dim
      save s_rho_dim,s_w_dim,zeta_time_dim,v2d_time_dim,
     &     v3d_time_dim,temp_time_dim,salt_time_dim         
      save one_dim,ocean_time_dim
!! variable ID
      save  zeta_time_id,      zeta_time_L
      save  v2d_time_id,       v2d_time_L
      save  v3d_time_id,       v3d_time_L
      save  temp_time_id,      temp_time_L
      save  salt_time_id,      salt_time_L
      save  zeta_west_id,      zeta_west_L
      save  zeta_east_id,      zeta_east_L
      save  zeta_south_id,     zeta_south_L
      save  zeta_north_id,     zeta_north_L
      save  ubar_west_id,      ubar_west_L
      save  ubar_east_id,      ubar_east_L
      save  ubar_south_id,     ubar_south_L
      save  ubar_north_id,     ubar_north_L
      save  vbar_west_id,      vbar_west_L
      save  vbar_east_id,      vbar_east_L
      save  vbar_south_id,     vbar_south_L
      save  vbar_north_id,     vbar_north_L
      save  u_west_id,	      u_west_L
      save  u_east_id,	      u_east_L
      save  u_south_id,	      u_south_L
      save  u_north_id,	      u_north_L
      save  v_west_id,	      v_west_L
      save  v_east_id,	      v_east_L
      save  v_south_id,	      v_south_L
      save  v_north_id,	      v_north_L
      save  temp_west_id,      temp_west_L
      save  temp_east_id,      temp_east_L
      save  temp_south_id,     temp_south_L
      save  temp_north_id,     temp_north_L
      save  salt_west_id,      salt_west_L
      save  salt_east_id,      salt_east_L
      save  salt_south_id,     salt_south_L
      save  salt_north_id,     salt_north_L

C   Add by L. Zheng for the 1-Term DO simulation
      save  do_west_id,      do_west_L
      save  do_east_id,      do_east_L
      save  do_south_id,     do_south_L
      save  do_north_id,     do_north_L
C   Add by L. Zheng for the 1-Term DO simulation

      xi_u_len=xi_rho_len-1
      xi_v_len=xi_rho_len
      eta_u_len=eta_rho_len
      eta_v_len=eta_rho_len-1
      s_w_len=s_rho_len+1

      if (imode .eq. 1)then 
C Set optional variable flags
      zeta_time_L = .TRUE.
      v2d_time_L = .TRUE.
      v3d_time_L = .TRUE.
      temp_time_L = .TRUE.
      salt_time_L = .TRUE.
      zeta_west_L = .TRUE.
      zeta_east_L = .TRUE.
      zeta_south_L = .TRUE.
      zeta_north_L = .TRUE.
      ubar_west_L = .TRUE.
      ubar_east_L = .TRUE.
      ubar_south_L = .TRUE.
      ubar_north_L = .TRUE.
      vbar_west_L = .TRUE.
      vbar_east_L = .TRUE.
      vbar_south_L = .TRUE.
      vbar_north_L = .TRUE.
      u_west_L = .TRUE.	
      u_east_L = .TRUE.	
      u_south_L = .TRUE.
      u_north_L = .TRUE.
      v_west_L = .TRUE.	
      v_east_L = .TRUE.	
      v_south_L = .TRUE.
      v_north_L = .TRUE.
      temp_west_L = .TRUE.
      temp_east_L = .TRUE.
      temp_south_L = .TRUE.
      temp_north_L = .TRUE.
      salt_west_L = .TRUE.
      salt_east_L = .TRUE.
      salt_south_L = .TRUE.
      salt_north_L = .TRUE.

C   Add by L. Zheng for the 1-Term DO simulation
      if(ioxyg .eq. 1) then
        do_west_L = .TRUE.
        do_east_L = .TRUE.
        do_south_L = .TRUE.
        do_north_L = .TRUE.
      else
        do_west_L = .FALSE.
        do_east_L = .FALSE.
        do_south_L = .FALSE.
        do_north_L = .FALSE.
      endif
C   Add by L. Zheng for the 1-Term DO simulation

      if(zeta_time(1) .le. 0)zeta_time_L = .FALSE.
      if(v2d_time(1) .le. 0) v2d_time_L = .FALSE.
      if(v3d_time(1) .le. 0) v3d_time_L = .FALSE.
      if(temp_time(1) .le. 0) temp_time_L = .FALSE.
      if(salt_time(1) .le. 0) salt_time_L = .FALSE.
      if(zeta_west(1,1) .le. 0) zeta_west_L = .FALSE.
      if(zeta_east(1,1) .le. 0) zeta_east_L = .FALSE.
      if(zeta_south(1,1) .le. 0) zeta_south_L = .FALSE.
      if(zeta_north(1,1) .le. 0) zeta_north_L = .FALSE.
      if(ubar_west(1,1) .le. 0) ubar_west_L = .FALSE.
      if(ubar_east(1,1) .le. 0) ubar_east_L = .FALSE.
      if(ubar_south(1,1) .le. 0) ubar_south_L = .FALSE.
      if(ubar_north(1,1) .le. 0) ubar_north_L = .FALSE.
      if(vbar_west(1,1) .le. 0) vbar_west_L = .FALSE.
      if(vbar_east(1,1) .le. 0) vbar_east_L = .FALSE.
      if(vbar_south(1,1) .le. 0) vbar_south_L = .FALSE.
      if(vbar_north(1,1) .le. 0) vbar_north_L = .FALSE.
      if(u_west(1,1,1) .le. 0) u_west_L = .FALSE. 
      if(u_east(1,1,1) .le. 0) u_east_L = .FALSE. 
      if(u_south(1,1,1) .le. 0) u_south_L = .FALSE.
      if(u_north(1,1,1) .le. 0) u_north_L = .FALSE.
      if(v_west(1,1,1) .le. 0)  v_west_L = .FALSE. 
      if(v_east(1,1,1) .le. 0)  v_east_L = .FALSE. 
      if(v_south(1,1,1) .le. 0) v_south_L = .FALSE.
      if(v_north(1,1,1) .le. 0) v_north_L = .FALSE.
      if(temp_west(1,1,1) .le. 0) temp_west_L = .FALSE.
      if(temp_east(1,1,1) .le. 0) temp_east_L = .FALSE.
      if(temp_south(1,1,1) .le. 0) temp_south_L = .FALSE.
      if(temp_north(1,1,1) .le. 0) temp_north_L = .FALSE.
      if(salt_west(1,1,1) .le. 0) salt_west_L = .FALSE.
      if(salt_east(1,1,1) .le. 0) salt_east_L = .FALSE.
      if(salt_south(1,1,1) .le. 0) salt_south_L = .FALSE.
      if(salt_north(1,1,1) .le. 0) salt_north_L = .FALSE.

C   Add by L. Zheng for the 1-Term DO simulation
      if(do_west(1,1,1) .le. 0) do_west_L = .FALSE.
      if(do_east(1,1,1) .le. 0) do_east_L = .FALSE.
      if(do_south(1,1,1) .le. 0) do_south_L = .FALSE.
      if(do_north(1,1,1) .le. 0) do_north_L = .FALSE.
C   Add by L. Zheng for the 1-Term DO simulation

      CALL DATE_AND_TIME(BIG_BEN(1),BIG_BEN(2),BIG_BEN(3),DATE_TIME)
      WRITE(CURRENT_TIME,1)DATE_TIME(5),':',DATE_TIME(6),
     &DATE_TIME(2),'/',DATE_TIME(3),'/',DATE_TIME(1)
  !    IYY=DATE_TIME(1)
  !    IMON=DATE_TIME(2)
  !    IDD=DATE_TIME(3)
  !    IHH=DATE_TIME(5)
  !    IMIN=DATE_TIME(6)
  !    ISS=DATE_TIME(7)
1     format(I2.2,a1,I2.2,2x,I2.2,a1,i2.2,a1,I4)
!      base_date(1)=2008
!      base_date(2)=1
!      base_date(3)=1
!      base_date(4)=0
!      write(*,*)'netcdf file=',trim(netcdf_file)
        status=nf_create(trim(netcdf_file), NF_CLOBBER, ncid)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'open netcdf file: ',trim(netcdf_file)

c define dimensions
        status=nf_def_dim(ncid,'eta_rho',eta_rho_len,eta_rho_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define eta_rho dimensions: '
        status=nf_def_dim(ncid,'eta_u',eta_u_len, eta_u_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define eta_u dimensions: '
        status=nf_def_dim(ncid,'eta_v',eta_v_len, eta_v_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define eta_v dimensions: '
        status=nf_def_dim(ncid,'xi_rho',xi_rho_len, xi_rho_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define xi_rho dimensions: '
        status=nf_def_dim(ncid,'xi_u',xi_u_len, xi_u_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define xi_u dimensions: '
        status=nf_def_dim(ncid,'xi_v',xi_v_len, xi_v_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define xi_v dimensions: '
        status=nf_def_dim(ncid,'s_rho',s_rho_len, s_rho_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define s_rho dimensions: '
        status=nf_def_dim(ncid,'s_w',s_w_len, s_w_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define s_w dimensions: '
        status=nf_def_dim(ncid,'zeta_time',zeta_time_len,
     &	 zeta_time_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define zeta_time dimensions: '
	if (v2d_time_L)
     &   status=nf_def_dim(ncid,'v2d_time',v2d_time_len,
     &	 v2d_time_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define v2d_time dimensions: '
        if(v3d_time_L)
     &   status=nf_def_dim(ncid,'v3d_time',v3d_time_len,
     &	 v3d_time_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define v3d_time dimensions: '
        if(temp_time_L)
     &   status=nf_def_dim(ncid,'temp_time',temp_time_len,
     &	 temp_time_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define temp_time dimensions: '
        if(salt_time_L)
     &   status=nf_def_dim(ncid,'salt_time',salt_time_len,
     &	 salt_time_dim)
        if (status .ne. NF_NOERR)stop 'netcdf'
        write(6,*)'define salt_time dimensions: '

c define time associated variables
      if(zeta_time_L)then
        status=nf_def_var(ncid,'zeta_time',NF_REAL, 1,
     &  zeta_time_dim, zeta_time_id)
        call check_err(status)
        TEXT='free surface time'
        LEN=LEN_TRIM(TEXT)
        status=nf_put_att_text(ncid, zeta_time_id,'long_name',
     &   LEN,TRIM(TEXT))
        if (status .ne. NF_NOERR)stop 'netcdf'
        WRITE(TEXT,'(a11,I4,3(a1,I2.2),a6)')'days since ',base_date(1),
     & '-',base_date(2),'-',base_date(3),' ',base_date(4),':00:00'
!        WRITE(TEXT,'(a11,I4,3I3)')'days since ',base_date(1),
!     &  base_date(2),base_date(3),base_date(4)
        LEN=LEN_TRIM(TEXT)
        status=nf_put_att_text(ncid, zeta_time_id,'units',
     &   LEN,TRIM(TEXT))
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='zeta time,scalar, series'
        LEN=LEN_TRIM(TEXT)
        status=nf_put_att_text(ncid, zeta_time_id,'field',
     &       LEN,TRIM(TEXT))
!        status=nf_put_att_int(ncid, zeta_time_id,'base_date',NF_INT,
!     &  4,base_date)
        call check_err(status)
      endif

      if(v2d_time_L)then
        status=nf_def_var(ncid,'v2d_time',NF_REAL, 1, 
     &  v2d_time_dim, v2d_time_id)
        call check_err(status)
        TEXT='2D momentum time'
        LEN=LEN_TRIM(TEXT)
        status=nf_put_att_text(ncid, v2d_time_id,'long_name',
     &       LEN,TRIM(TEXT))
        if (status .ne. NF_NOERR)stop 'netcdf'
        WRITE(TEXT,'(a11,I4,3(a1,I2.2),a6)')'days since ',base_date(1),
     & '-',base_date(2),'-',base_date(3),' ',base_date(4),':00:00'
 !       WRITE(TEXT,'(a11,I4,3I3)')'days since ',base_date(1),
 !    &  base_date(2),base_date(3),base_date(4)
        LEN=LEN_TRIM(TEXT)
        status=nf_put_att_text(ncid, v2d_time_id,'units',
     &       LEN,TRIM(TEXT))
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='v2d_time,scalar, series'
        LEN=LEN_TRIM(TEXT)
        status=nf_put_att_text(ncid, v2d_time_id,'field',
     &       LEN,TRIM(TEXT))
!        status=nf_put_att_int(ncid, v2d_time_id,'base_date',NF_INT,
!     &  4,base_date)
        call check_err(status)
      endif
      
      if(v3d_time_L)then  
        status=nf_def_var(ncid,'v3d_time',NF_REAL, 1,
     &  v3d_time_dim, v3d_time_id)
        call check_err(status)
        TEXT='3D momentum time'
        LEN=LEN_TRIM(TEXT)
        status=nf_put_att_text(ncid, v3d_time_id,'long_name',
     &       LEN,TRIM(TEXT))
        if (status .ne. NF_NOERR)stop 'netcdf'
        WRITE(TEXT,'(a11,I4,3(a1,I2.2),a6)')'days since ',base_date(1),
     & '-',base_date(2),'-',base_date(3),' ',base_date(4),':00:00'
 !       WRITE(TEXT,'(a11,I4,3I3)')'days since ',base_date(1),
 !    &  base_date(2),base_date(3),base_date(4)
        LEN=LEN_TRIM(TEXT)
        status=nf_put_att_text(ncid, v3d_time_id,'units',
     &       LEN,TRIM(TEXT))
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='v3d_time,scalar, series'
        LEN=LEN_TRIM(TEXT)
        status=nf_put_att_text(ncid, v3d_time_id,'field',
     &       LEN,TRIM(TEXT))
!        status=nf_put_att_int(ncid, v3d_time_id,'base_date',NF_INT,
!     &  4,base_date)
       endif
 
      if(temp_time_L)then  
        status=nf_def_var(ncid,'temp_time',NF_REAL, 1, 
     &  temp_time_dim, temp_time_id)
        TEXT='open boundary potential temperature time'
        LEN=LEN_TRIM(TEXT)
        status=nf_put_att_text(ncid, temp_time_id,'long_name',
     &       LEN,TRIM(TEXT))
        if (status .ne. NF_NOERR)stop 'netcdf'
        WRITE(TEXT,'(a11,I4,3(a1,I2.2),a6)')'days since ',base_date(1),
     & '-',base_date(2),'-',base_date(3),' ',base_date(4),':00:00'
 !       WRITE(TEXT,'(a11,I4,3I3)')'days since ',base_date(1),
 !    &  base_date(2),base_date(3),base_date(4)
        LEN=LEN_TRIM(TEXT)
        status=nf_put_att_text(ncid, temp_time_id,'units',
     &       LEN,TRIM(TEXT))
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='temp_time,scalar, series'
        LEN=LEN_TRIM(TEXT)
        status=nf_put_att_text(ncid, temp_time_id,'field',
     &       LEN,TRIM(TEXT))
!        status=nf_put_att_int(ncid, temp_time_id,'base_date',NF_INT,
!     &  4,base_date)
      endif

      if(salt_time_L)then  
        status=nf_def_var(ncid,'salt_time',NF_REAL, 1,
     &  salt_time_dim, salt_time_id)
        TEXT='open boundary salinity time'
        LEN=LEN_TRIM(TEXT)
        status=nf_put_att_text(ncid, salt_time_id,'long_name',
     &       LEN,TRIM(TEXT))
        if (status .ne. NF_NOERR)stop 'netcdf'
        WRITE(TEXT,'(a11,I4,3(a1,I2.2),a6)')'days since ',base_date(1),
     & '-',base_date(2),'-',base_date(3),' ',base_date(4),':00:00'
!        WRITE(TEXT,'(a11,I4,3I3)')'days since ',base_date(1),
!     &  base_date(2),base_date(3),base_date(4)
        LEN=LEN_TRIM(TEXT)
        status=nf_put_att_text(ncid, salt_time_id,'units',
     &       LEN,TRIM(TEXT))
        if (status .ne. NF_NOERR)stop 'netcdf'
        TEXT='salt_time,scalar, series'
        LEN=LEN_TRIM(TEXT)
        status=nf_put_att_text(ncid, salt_time_id,'field',
     &       LEN,TRIM(TEXT))
!        status=nf_put_att_int(ncid, salt_time_id,'base_date',NF_INT,
!     &  4,base_date)
       endif
!! define state variables

      intval(2) = zeta_time_dim
      intval(1) = eta_rho_dim
      if(zeta_west_L)THEN
        status=nf_def_var(ncid,'zeta_west',NF_REAL, 2, intval,
     &  zeta_west_id)
        call check_err(status)
        status=nf_put_att_text(ncid, zeta_west_id,'long_name',39,
     1  'free-surface western boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, zeta_west_id,'units',5,'meter')
        call check_err(status)
        status=nf_put_att_text(ncid, zeta_west_id,'field',25,
     1  'zeta_west, scalar, series')
        call check_err(status)
      status=nf_put_att_text(ncid, zeta_west_id,'time',9,'zeta_time')
        call check_err(status)
        status=nf_put_att_real(ncid,zeta_west_id,'_FillValue',NF_REAL,
     &	1,-99999.9)
        call check_err(status)
      endif
      if(zeta_east_L)THEN
        status=nf_def_var(ncid,'zeta_east',NF_REAL, 2, intval,
     &  zeta_east_id)
        call check_err(status)
        status=nf_put_att_text(ncid, zeta_east_id,'long_name',39,
     1  'free-surface eastern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, zeta_east_id,'units',5,'meter')
        call check_err(status)
        status=nf_put_att_text(ncid, zeta_east_id,'field',25,
     1  'zeta_east, scalar, series')
        call check_err(status)
      status=nf_put_att_text(ncid, zeta_east_id,'time',9,'zeta_time')
        call check_err(status)
        status=nf_put_att_real(ncid,zeta_east_id,'_FillValue',NF_REAL,
     &	1,-99999.9)
        call check_err(status)
      endif
      intval(2) = zeta_time_dim
      intval(1) = xi_rho_dim
      if(zeta_south_L)THEN
        status=nf_def_var(ncid,'zeta_south',NF_REAL, 2,intval,
     &  zeta_south_id)
        call check_err(status)
        status=nf_put_att_text(ncid, zeta_south_id,'long_name',40,
     1  'free-surface southern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, zeta_south_id,'units',5,'meter')
        call check_err(status)
        status=nf_put_att_text(ncid, zeta_south_id,'field',26,
     1  'zeta_south, scalar, series')
        call check_err(status)
      status=nf_put_att_text(ncid, zeta_south_id,'time',9,'zeta_time')
        call check_err(status)
        status=nf_put_att_real(ncid,zeta_south_id,'_FillValue',NF_REAL,
     &	1,-99999.9)
        call check_err(status)
      endif
      if(zeta_north_L)THEN
        status=nf_def_var(ncid,'zeta_north',NF_REAL, 2,intval,
     1  zeta_north_id)
        call check_err(status)
        status=nf_put_att_text(ncid, zeta_north_id,'long_name',40,
     1  'free-surface northern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, zeta_north_id,'units',5,'meter')
        call check_err(status)
        status=nf_put_att_text(ncid, zeta_north_id,'field',26,
     1  'zeta_north, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid,zeta_north_id,'time',9,'zeta_time')
        call check_err(status)
        status=nf_put_att_real(ncid,zeta_north_id,'_FillValue',NF_REAL,
     &	1,-99999.9)
        call check_err(status)
      endif
      intval(2) = v2d_time_dim
      intval(1) = eta_u_dim
      if(ubar_west_L)THEN
        status=nf_def_var(ncid,'ubar_west',NF_REAL, 2,intval, 
     1  ubar_west_id)
        call check_err(status)
        status=nf_put_att_text(ncid, ubar_west_id,'long_name',40,
     1  '2D u-momentum western boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, ubar_west_id,'units',14,
     1  'meter second-1')
        call check_err(status)
        status=nf_put_att_text(ncid, ubar_west_id,'field',25,
     1  'ubar_west, scalar, series')
        call check_err(status)
      status=nf_put_att_text(ncid, ubar_west_id,'time',8,'v2d_time')
        call check_err(status)
      endif
      if(ubar_east_L)THEN
        status=nf_def_var(ncid,'ubar_east',NF_REAL, 2,intval, 
     1  ubar_east_id)
        call check_err(status)
        status=nf_put_att_text(ncid, ubar_east_id,'long_name',40,
     1  '2D u-momentum eastern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, ubar_east_id,'units',14,
     1  'meter second-1')
        call check_err(status)
        status=nf_put_att_text(ncid, ubar_east_id,'field',25,
     1  'ubar_east, scalar, series')
        call check_err(status)
      status=nf_put_att_text(ncid, ubar_east_id,'time',8,'v2d_time')
        call check_err(status)
      endif
      intval(2) = v2d_time_dim
      intval(1) = xi_u_dim
      if(ubar_south_L)THEN
        status=nf_def_var(ncid,'ubar_south',NF_REAL, 2,intval,
     1  ubar_south_id)
        call check_err(status)
        status=nf_put_att_text(ncid, ubar_south_id,'long_name',41,
     1  '2D u-momentum southern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, ubar_south_id,'units',14,
     1  'meter second-1')
        call check_err(status)
        status=nf_put_att_text(ncid, ubar_south_id,'field',26,
     1  'ubar_south, scalar, series')
        call check_err(status)
      status=nf_put_att_text(ncid, ubar_south_id,'time',8,'v2d_time')
        call check_err(status)
      endif
      if(ubar_north_L)THEN
        status=nf_def_var(ncid,'ubar_north',NF_REAL, 2,intval,
     1  ubar_north_id)
        call check_err(status)
        status=nf_put_att_text(ncid, ubar_north_id,'long_name',41,
     1  '2D u-momentum northern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, ubar_north_id,'units',14,
     1  'meter second-1')
        call check_err(status)
        status=nf_put_att_text(ncid, ubar_north_id,'field',26,
     1  'ubar_north, scalar, series')
        call check_err(status)
      status=nf_put_att_text(ncid, ubar_north_id,'time',8,'v2d_time')
        call check_err(status)
      endif
      intval(2) = v2d_time_dim
      intval(1) = eta_v_dim
      if(vbar_west_L)THEN
        status=nf_def_var(ncid,'vbar_west',NF_REAL, 2,intval, 
     1  vbar_west_id)
        call check_err(status)
        status=nf_put_att_text(ncid, vbar_west_id,'long_name',40,
     1  '2D v-momentum western boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, vbar_west_id,'units',14,
     1  'meter second-1')
        call check_err(status)
        status=nf_put_att_text(ncid, vbar_west_id,'field',25,
     1  'vbar_west, scalar, series')
        call check_err(status)
      status=nf_put_att_text(ncid, vbar_west_id,'time',8,'v2d_time')
        call check_err(status)
      endif
      if(vbar_east_L)THEN
        status=nf_def_var(ncid,'vbar_east',NF_REAL, 2,intval, 
     1  vbar_east_id)
        call check_err(status)
        status=nf_put_att_text(ncid, vbar_east_id,'long_name',40,
     1  '2D v-momentum eastern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, vbar_east_id,'units',14,
     1  'meter second-1')
        call check_err(status)
        status=nf_put_att_text(ncid, vbar_east_id,'field',25,
     1  'vbar_east, scalar, series')
        call check_err(status)
      status=nf_put_att_text(ncid, vbar_east_id,'time',8,'v2d_time')
        call check_err(status)
      endif
      intval(2) = v2d_time_dim
      intval(1) = xi_v_dim
      if(vbar_south_L)THEN
        status=nf_def_var(ncid,'vbar_south',NF_REAL, 2,intval,
     1  vbar_south_id)
        call check_err(status)
        status=nf_put_att_text(ncid, vbar_south_id,'long_name',41,
     1  '2D v-momentum southern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, vbar_south_id,'units',14,
     1  'meter second-1')
        call check_err(status)
        status=nf_put_att_text(ncid, vbar_south_id,'field',26,
     1  'vbar_south, scalar, series')
        call check_err(status)
      status=nf_put_att_text(ncid, vbar_south_id,'time',8,'v2d_time')
        call check_err(status)
      endif
      if(vbar_north_L)THEN
        status=nf_def_var(ncid,'vbar_north',NF_REAL, 2,intval,
     1  vbar_north_id)
        call check_err(status)
        status=nf_put_att_text(ncid, vbar_north_id,'long_name',41,
     1  '2D v-momentum northern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, vbar_north_id,'units',14,
     1  'meter second-1')
        call check_err(status)
        status=nf_put_att_text(ncid, vbar_north_id,'field',26,
     1  'vbar_north, scalar, series')
        call check_err(status)
      status=nf_put_att_text(ncid, vbar_north_id,'time',8,'v2d_time')
        call check_err(status)
      endif
      intval(3) = v3d_time_dim
      intval(2) = s_rho_dim
      intval(1) = eta_u_dim
      if(u_west_L)THEN
        status=nf_def_var(ncid,'u_west',NF_REAL, 3,intval,
     1  u_west_id)
        call check_err(status)
        status=nf_put_att_text(ncid, u_west_id,'long_name',40,
     1  '3D u-momentum western boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, u_west_id,'units',14,
     1  'meter second-1')
        call check_err(status)
        status=nf_put_att_text(ncid, u_west_id,'field',22,
     1  'u_west, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid, u_west_id,'time',8,'v3d_time')
        call check_err(status)
      endif
      if(u_east_L)THEN
        status=nf_def_var(ncid,'u_east',NF_REAL, 3,intval,
     1  u_east_id)
        call check_err(status)
        status=nf_put_att_text(ncid, u_east_id,'long_name',40,
     1  '3D u-momentum eastern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, u_east_id,'units',14,
     1  'meter second-1')
        call check_err(status)
        status=nf_put_att_text(ncid, u_east_id,'field',22,
     1  'u_east, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid, u_east_id,'time',8,'v3d_time')
        call check_err(status)
      endif
      intval(3) = v3d_time_dim
      intval(2) = s_rho_dim
      intval(1) = xi_u_dim
      if(u_south_L)THEN
        status=nf_def_var(ncid,'u_south',NF_REAL, 3,intval, 
     1  u_south_id)
        call check_err(status)
        status=nf_put_att_text(ncid, u_south_id,'long_name',41,
     1  '3D u-momentum southern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, u_south_id,'units',14,
     1  'meter second-1')
        call check_err(status)
        status=nf_put_att_text(ncid, u_south_id,'field',23,
     1  'u_south, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid, u_south_id,'time',8,'v3d_time')
        call check_err(status)
      endif
      if(u_north_L)THEN
        status=nf_def_var(ncid,'u_north',NF_REAL, 3,intval, 
     1  u_north_id)
        call check_err(status)
        status=nf_put_att_text(ncid, u_north_id,'long_name',41,
     1  '3D u-momentum northern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, u_north_id,'units',14, 
     1  'meter second-1')
        call check_err(status)
        status=nf_put_att_text(ncid, u_north_id,'field',23, 
     1  'u_north, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid, u_north_id,'time',8,'v3d_time')
        call check_err(status)
      endif
      intval(3) = v3d_time_dim
      intval(2) = s_rho_dim
      intval(1) = eta_v_dim
      if(v_west_L)THEN
        status=nf_def_var(ncid,'v_west',NF_REAL, 3,intval, 
     1  v_west_id)
        call check_err(status)
        status=nf_put_att_text(ncid, v_west_id,'long_name',40, 
     1  '3D v-momentum western boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, v_west_id,'units',14, 
     1  'meter second-1')
        call check_err(status)
        status=nf_put_att_text(ncid, v_west_id,'field',22, 
     1  'v_west, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid, v_west_id,'time',8,'v3d_time')
        call check_err(status)
      endif
      if(v_east_L)THEN
        status=nf_def_var(ncid,'v_east',NF_REAL, 3,intval, 
     1  v_east_id)
        call check_err(status)
        status=nf_put_att_text(ncid, v_east_id,'long_name',40, 
     1  '3D v-momentum eastern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, v_east_id,'units',14, 
     1  'meter second-1')
        call check_err(status)
        status=nf_put_att_text(ncid, v_east_id,'field',22, 
     1  'v_east, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid, v_east_id,'time',8,'v3d_time')
        call check_err(status)
      endif
      intval(3) = v3d_time_dim
      intval(2) = s_rho_dim
      intval(1) = xi_v_dim
      if(v_south_L)THEN
        status=nf_def_var(ncid,'v_south',NF_REAL,  3,intval,
     1  v_south_id)
        call check_err(status)
        status=nf_put_att_text(ncid, v_south_id,'long_name',41, 
     1  '3D v-momentum southern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, v_south_id,'units',14, 
     1  'meter second-1')
        call check_err(status)
        status=nf_put_att_text(ncid, v_south_id,'field',23, 
     1  'v_south, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid, v_south_id,'time',8,'v3d_time')
        call check_err(status)
      endif
      if(v_north_L)THEN
        status=nf_def_var(ncid,'v_north',NF_REAL,  3,intval,
     1  v_north_id)
        call check_err(status)
        status=nf_put_att_text(ncid, v_north_id,'long_name',41, 
     1  '3D v-momentum northern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, v_north_id,'units',14, 
     1  'meter second-1')
        call check_err(status)
        status=nf_put_att_text(ncid, v_north_id,'field',23, 
     1  'v_north, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid, v_north_id,'time',8,'v3d_time')
        call check_err(status)
      endif
      intval(3) = temp_time_dim
      intval(2) = s_rho_dim
      intval(1) = eta_rho_dim
      if(temp_west_L)THEN
        status=nf_def_var(ncid,'temp_west',NF_REAL, 3,intval,
     1  temp_west_id)
        call check_err(status)
        status=nf_put_att_text(ncid, temp_west_id,'long_name',48, 
     1  'potential temperature western boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, temp_west_id,'units',7,'Celsius')
        call check_err(status)
        status=nf_put_att_text(ncid, temp_west_id,'field',25, 
     1  'temp_west, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid,temp_west_id,'time',9,'temp_time')
        call check_err(status)
        status=nf_put_att_real(ncid,temp_west_id,'_FillValue',NF_REAL,
     &	1,-99999.9)
        call check_err(status)
      endif
      if(temp_east_L)THEN
        status=nf_def_var(ncid,'temp_east',NF_REAL, 3,intval, 
     1  temp_east_id)
        call check_err(status)
        status=nf_put_att_text(ncid,temp_east_id,'long_name',48, 
     1  'potential temperature eastern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid,temp_east_id,'units',7,'Celsius')
        call check_err(status)
        status=nf_put_att_text(ncid,temp_east_id,'field',25, 
     1  'temp_east, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid,temp_east_id,'time',9,'temp_time')
        call check_err(status)
        status=nf_put_att_real(ncid,temp_east_id,'_FillValue',NF_REAL,
     &	1,-99999.9)
        call check_err(status)
      endif
      intval(3) = temp_time_dim
      intval(2) = s_rho_dim
      intval(1) = xi_rho_dim
      if(temp_south_L)THEN
        status=nf_def_var(ncid,'temp_south',NF_REAL, 3,intval, 
     1  temp_south_id)
        call check_err(status)
        status=nf_put_att_text(ncid, temp_south_id,'long_name',49, 
     1  'potential temperature southern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid,temp_south_id,'units',7,'Celsius')
        call check_err(status)
        status=nf_put_att_text(ncid,temp_south_id,'field',26, 
     1  'temp_south, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid,temp_south_id,'time',9,'temp_time')
        call check_err(status)
        status=nf_put_att_real(ncid,temp_south_id,'_FillValue',NF_REAL,
     &	1,-99999.9)
        call check_err(status)
      endif
      if(temp_north_L)THEN
        status=nf_def_var(ncid,'temp_north',NF_REAL, 3,intval, 
     1  temp_north_id)
        call check_err(status)
        status=nf_put_att_text(ncid, temp_north_id,'long_name',49, 
     1  'potential temperature northern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid,temp_north_id,'units',7,'Celsius')
        call check_err(status)
        status=nf_put_att_text(ncid,temp_north_id,'field',26, 
     1  'temp_north, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid,temp_north_id,'time',9,'temp_time')
        call check_err(status)
        status=nf_put_att_real(ncid,temp_north_id,'_FillValue',NF_REAL,
     &	1,-99999.9)
        call check_err(status)
      endif
      intval(3) = salt_time_dim
      intval(2) = s_rho_dim
      intval(1) = eta_rho_dim
      if(salt_west_L)THEN
        status=nf_def_var(ncid,'salt_west',NF_REAL, 3,intval, 
     1  salt_west_id)
        call check_err(status)
        status=nf_put_att_text(ncid, salt_west_id,'long_name',35, 
     1  'salinity western boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, salt_west_id,'units',3,'PSU')
        call check_err(status)
        status=nf_put_att_text(ncid, salt_west_id,'field',25, 
     1  'salt_west, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid,salt_west_id,'time',9,'salt_time')
        call check_err(status)
        status=nf_put_att_real(ncid,salt_west_id,'_FillValue',NF_REAL,
     &	1,-99999.9)
      endif
      if(salt_east_L)THEN
        status=nf_def_var(ncid,'salt_east',NF_REAL, 3,intval, 
     1  salt_east_id)
        call check_err(status)
        status=nf_put_att_text(ncid, salt_east_id,'long_name',35, 
     1  'salinity eastern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, salt_east_id,'units',3,'PSU')
        call check_err(status)
        status=nf_put_att_text(ncid, salt_east_id,'field',25, 
     1  'salt_east, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid,salt_east_id,'time',9,'salt_time')
        call check_err(status)
        status=nf_put_att_real(ncid,salt_east_id,'_FillValue',NF_REAL,
     &	1,-99999.9)
      endif
      intval(3) = salt_time_dim
      intval(2) = s_rho_dim
      intval(1) = xi_rho_dim
      if(salt_south_L)THEN
        status=nf_def_var(ncid,'salt_south',NF_REAL, 3,intval, 
     1  salt_south_id)
        call check_err(status)
        status=nf_put_att_text(ncid, salt_south_id,'long_name',36, 
     1  'salinity southern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid,salt_south_id,'units',3,'PSU')
        call check_err(status)
        status=nf_put_att_text(ncid,salt_south_id,'field',26, 
     1  'salt_south, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid,salt_south_id,'time',9,'salt_time')
        call check_err(status)
        status=nf_put_att_real(ncid,salt_south_id,'_FillValue',NF_REAL,
     &	1,-99999.9)
      endif
      if(salt_north_L)THEN
        status=nf_def_var(ncid,'salt_north',NF_REAL, 3,intval, 
     1  salt_north_id)
        call check_err(status)
        status=nf_put_att_text(ncid,salt_north_id,'long_name',36,
     1  'salinity northern boundary condition')
        call check_err(status)
        status=nf_put_att_text(ncid, salt_north_id,'units',3,'PSU')
        call check_err(status)
        status=nf_put_att_text(ncid, salt_north_id,'field',26,
     1  'salt_north, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid,salt_north_id,'time',9,'salt_time')
        call check_err(status)
        status=nf_put_att_real(ncid,salt_north_id,'_FillValue',NF_REAL,
     &	1,-99999.9)
      endif

C   Add by L. Zheng for the 1-Term DO simulation
      intval(3) = temp_time_dim
      intval(2) = s_rho_dim
      intval(1) = eta_rho_dim
      if(do_west_L)THEN
        status=nf_def_var(ncid,'oxygen_west',NF_REAL, 3,intval,
     1  do_west_id)
        call check_err(status)
        status=nf_put_att_text(ncid, do_west_id,'long_name',33, 
     1  'dissolved oxygen western boundary')
        call check_err(status)
        status=nf_put_att_text(ncid, do_west_id,'units',7,'mmol/m3')
        call check_err(status)
        status=nf_put_att_text(ncid, do_west_id,'field',27, 
     1  'oxygen_west, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid,do_west_id,'time',9,'temp_time')
        call check_err(status)
        status=nf_put_att_real(ncid,do_west_id,'_FillValue',NF_REAL,
     &	1,-99999.9)
        call check_err(status)
      endif
      if(do_east_L)THEN
        status=nf_def_var(ncid,'oxygen_east',NF_REAL, 3,intval, 
     1  do_east_id)
        call check_err(status)
        status=nf_put_att_text(ncid,do_east_id,'long_name',33, 
     1  'dissloved oxygen eastern boundary')
        call check_err(status)
        status=nf_put_att_text(ncid,do_east_id,'units',7,'mmol/m3')
        call check_err(status)
        status=nf_put_att_text(ncid,do_east_id,'field',27, 
     1  'oxygen_east, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid,do_east_id,'time',9,'temp_time')
        call check_err(status)
        status=nf_put_att_real(ncid,do_east_id,'_FillValue',NF_REAL,
     &	1,-99999.9)
        call check_err(status)
      endif
      intval(3) = temp_time_dim
      intval(2) = s_rho_dim
      intval(1) = xi_rho_dim
      if(do_south_L)THEN
        status=nf_def_var(ncid,'oxygen_south',NF_REAL, 3,intval, 
     1  do_south_id)
        call check_err(status)
        status=nf_put_att_text(ncid, do_south_id,'long_name',34, 
     1  'dissloved oxygen southern boundary')
        call check_err(status)
        status=nf_put_att_text(ncid,do_south_id,'units',7,'mmol/m3')
        call check_err(status)
        status=nf_put_att_text(ncid,do_south_id,'field',28, 
     1  'oxygen_south, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid,do_south_id,'time',9,'temp_time')
        call check_err(status)
        status=nf_put_att_real(ncid,do_south_id,'_FillValue',NF_REAL,
     &	1,-99999.9)
        call check_err(status)
      endif
      if(do_north_L)THEN
        status=nf_def_var(ncid,'oxygen_north',NF_REAL, 3,intval, 
     1  do_north_id)
        call check_err(status)
        status=nf_put_att_text(ncid, do_north_id,'long_name',34, 
     1  'dissloved oxygen northern boundary')
        call check_err(status)
        status=nf_put_att_text(ncid,do_north_id,'units',7,'mmol/m3')
        call check_err(status)
        status=nf_put_att_text(ncid,do_north_id,'field',28, 
     1  'oxygen_north, scalar, series')
        call check_err(status)
        status=nf_put_att_text(ncid,do_north_id,'time',9,'temp_time')
        call check_err(status)
        status=nf_put_att_real(ncid,do_north_id,'_FillValue',NF_REAL,
     &	1,-99999.9)
        call check_err(status)
      endif
C   Add by L. Zheng for the 1-Term DO simulation

!!      status=nf_put_att_real(ncid, lwrad_id, 
 !    & 'missing_value',NF_REAL, 1,-99999.0)

C Global Attributes
      TEXT=trim(adjustL(globalstr(1)))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid,NF_GLOBAL ,'type', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(adjustL(globalstr(2)))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'title', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(adjustL(globalstr(3)))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'WL_source', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(adjustL(globalstr(4)))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'T_S_source', 
     &       LEN,TRIM(TEXT))

      TEXT=trim(adjustL(globalstr(5)))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'data_file', 
     &       LEN,TRIM(TEXT))

      TEXT=trim(adjustL(globalstr(6)))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'interpolation', 
     &       LEN,TRIM(TEXT))
      TEXT=trim(adjustL(globalstr(7)))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'grid_file', 
     &       LEN,TRIM(TEXT))
     
      TEXT=trim(adjustL(globalstr(8)))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'history', 
     &       LEN,TRIM(TEXT))
!      TEXT='Created by Aijun Zhang, OD/CO-OPS/NOS/NOAA'
      TEXT=trim(adjustL(globalstr(9)))
      LEN=LEN_TRIM(TEXT)
      status = nf_put_att_text(ncid, NF_GLOBAL,'reference', 
     &       LEN,TRIM(TEXT))
      status=nf_enddef(ncid)
!!  end of variable definition

      elseif (imode.eq.2) then
c        Inquire of this file what the last itime written was
C        should be equal to the dimension of time
         status= nf_inq_dimlen(ncid,zeta_time_dim,itime)
         itime=itime+1
c scalars
!      CORNER(1) = itime
!      iret=nf_put_var1_real(ncid,frc_time_id,CORNER,frc_time)
C write time associate variables
        if(zeta_time_L)THEN
        CORNER(1) = 1
        COUNT(1)=zeta_time_len
        status=nf_put_vara_real(ncid,zeta_time_id,CORNER,COUNT,
     &	zeta_time)
        if (status .ne. NF_NOERR)stop 'write zeta time'
	endif
        if(v2d_time_L)THEN
        CORNER(1) = 1
        COUNT(1)=v2d_time_len
        status=nf_put_vara_real(ncid,v2d_time_id,CORNER,COUNT,
     &	v2d_time)
        if (status .ne. NF_NOERR)stop 'write v2d time'
	endif
        if(v3d_time_L)THEN
        CORNER(1) = 1
        COUNT(1)=v3d_time_len
        status=nf_put_vara_real(ncid,v3d_time_id,CORNER,COUNT,
     &	v3d_time)
        if (status .ne. NF_NOERR)stop 'write v3d time'
	endif
        if(temp_time_L)THEN
        CORNER(1) = 1
        COUNT(1)=temp_time_len
        status=nf_put_vara_real(ncid,temp_time_id,CORNER,COUNT,
     &	temp_time)
        if (status .ne. NF_NOERR)stop 'write temp_time'
	endif
        if(salt_time_L)THEN
        CORNER(1) = 1
        COUNT(1)=salt_time_len
        status=nf_put_vara_real(ncid,salt_time_id,CORNER,COUNT,
     &	salt_time)
        if (status .ne. NF_NOERR)stop 'write temp_time'
	endif

C    write 2D fields the real array defined in netcdf is 3-D =(x,y,time)
         CORNER(1) = 1
         CORNER(2) = 1
         COUNT(1)=eta_rho_len
         COUNT(2)=zeta_time_len
        if(zeta_west_L)THEN
        status=nf_put_vara_real(ncid,zeta_west_id,CORNER,COUNT,
     &	zeta_west)
        if (status .ne. NF_NOERR)stop 'write zeta_west'
        endif
        if(zeta_east_L)THEN
        status=nf_put_vara_real(ncid,zeta_east_id,CORNER,COUNT,
     &	zeta_east)
        if (status .ne. NF_NOERR)stop 'write zeta_east'
        endif
         CORNER(1) = 1
         CORNER(2) = 1
         COUNT(1)=xi_rho_len
         COUNT(2)=zeta_time_len
        if(zeta_south_L)THEN
        status=nf_put_vara_real(ncid,zeta_south_id,CORNER,COUNT,
     &	zeta_south)
        if (status .ne. NF_NOERR)stop 'write zeta_south'
        endif
        if(zeta_north_L)THEN
        status=nf_put_vara_real(ncid,zeta_north_id,CORNER,COUNT,
     &	zeta_north)
        if (status .ne. NF_NOERR)stop 'write zeta_north'
        endif
         CORNER(1) = 1
         CORNER(2) = 1
         COUNT(1)=eta_rho_len
         COUNT(2)=v2d_time_len
        if(ubar_west_L)THEN
        status=nf_put_vara_real(ncid,ubar_west_id,CORNER,COUNT,
     &	ubar_west)
        if (status .ne. NF_NOERR)stop 'write ubar_west'
        endif
        if(ubar_east_L)THEN
        status=nf_put_vara_real(ncid,ubar_east_id,CORNER,COUNT,
     &	ubar_east)
        if (status .ne. NF_NOERR)stop 'write ubar_east'
        endif
         CORNER(1) = 1
         CORNER(2) = 1
         COUNT(1)=xi_rho_len-1
         COUNT(2)=v2d_time_len
        if(ubar_south_L)THEN
        status=nf_put_vara_real(ncid,ubar_south_id,CORNER,COUNT,
     &	ubar_south)
        if (status .ne. NF_NOERR)stop 'write ubar_south'
        endif
        if(ubar_north_L)THEN
        status=nf_put_vara_real(ncid,ubar_north_id,CORNER,COUNT,
     &	ubar_north)
        if (status .ne. NF_NOERR)stop 'write ubar_north'
        endif

         CORNER(1) = 1
         CORNER(2) = 1
         COUNT(1)=eta_rho_len-1
         COUNT(2)=v2d_time_len
        if(vbar_west_L)THEN
        status=nf_put_vara_real(ncid,vbar_west_id,CORNER,COUNT,
     &	vbar_west)
        if (status .ne. NF_NOERR)stop 'write vbar_west'
        endif
        if(vbar_east_L)THEN
        status=nf_put_vara_real(ncid,vbar_east_id,CORNER,COUNT,
     &	vbar_east)
        if (status .ne. NF_NOERR)stop 'write vbar_east'
        endif
         CORNER(1) = 1
         CORNER(2) = 1
         COUNT(1)=xi_rho_len
         COUNT(2)=v2d_time_len
        if(vbar_south_L)THEN
        status=nf_put_vara_real(ncid,vbar_south_id,CORNER,COUNT,
     &	vbar_south)
        if (status .ne. NF_NOERR)stop 'write vbar_south'
        endif
        if(vbar_north_L)THEN
        status=nf_put_vara_real(ncid,vbar_north_id,CORNER,COUNT,
     &	vbar_north)
        if (status .ne. NF_NOERR)stop 'write vbar_north'
        endif
C    write 3D fields the real array defined in netcdf is 3-D =(x,y,time)
	
	
         CORNER(1) = 1
         CORNER(2) = 1
         CORNER(3) = 1
         COUNT(1)=eta_rho_len
         COUNT(2)=s_rho_len
         COUNT(3)=v3d_time_len
        if(u_west_L)THEN
        status=nf_put_vara_real(ncid,u_west_id,CORNER,COUNT,u_west)
        if (status .ne. NF_NOERR)stop 'write u_west'
        endif
        if(u_east_L)THEN
        status=nf_put_vara_real(ncid,u_east_id,CORNER,COUNT,u_east)
        if (status .ne. NF_NOERR)stop 'write u_east'
        endif
         CORNER(1) = 1
         CORNER(2) = 1
         CORNER(3) = 1
         COUNT(1)=xi_rho_len-1
         COUNT(2)=s_rho_len
         COUNT(3)=v3d_time_len
        if(u_south_L)THEN
        status=nf_put_vara_real(ncid,u_south_id,CORNER,COUNT,u_south)
        if (status .ne. NF_NOERR)stop 'write u_south'
        endif
        if(u_north_L)THEN
        status=nf_put_vara_real(ncid,u_north_id,CORNER,COUNT,u_north)
        if (status .ne. NF_NOERR)stop 'write u_north'
        endif
	
         CORNER(1) = 1
         CORNER(2) = 1
         CORNER(3) = 1
         COUNT(1)=eta_rho_len-1
         COUNT(2)=s_rho_len
         COUNT(3)=v3d_time_len
        if(v_west_L)THEN
        status=nf_put_vara_real(ncid,v_west_id,CORNER,COUNT,v_west)
        if (status .ne. NF_NOERR)stop 'write v_west'
        endif
        if(v_east_L)THEN
        status=nf_put_vara_real(ncid,v_east_id,CORNER,COUNT,v_east)
        if (status .ne. NF_NOERR)stop 'write v_east'
        endif
         CORNER(1) = 1
         CORNER(2) = 1
         CORNER(3) = 1
         COUNT(1)=xi_rho_len
         COUNT(2)=s_rho_len
         COUNT(3)=v3d_time_len
        if(v_south_L)THEN
        status=nf_put_vara_real(ncid,v_south_id,CORNER,COUNT,v_south)
        if (status .ne. NF_NOERR)stop 'write v_south'
        endif
        if(v_north_L)THEN
        status=nf_put_vara_real(ncid,v_north_id,CORNER,COUNT,v_north)
        if (status .ne. NF_NOERR)stop 'write v_north'
        endif
	
         CORNER(1) = 1
         CORNER(2) = 1
         CORNER(3) = 1
         COUNT(1)=eta_rho_len
         COUNT(2)=s_rho_len
         COUNT(3)=temp_time_len
        if(temp_west_L)THEN
        status=nf_put_vara_real(ncid,temp_west_id,CORNER,COUNT,
     &	temp_west)
        if (status .ne. NF_NOERR)stop 'write temp_west'
        endif
        if(temp_east_L)THEN
        status=nf_put_vara_real(ncid,temp_east_id,CORNER,COUNT,
     &	temp_east)
        if (status .ne. NF_NOERR)stop 'write temp_east'
        endif
         CORNER(1) = 1
         CORNER(2) = 1
         CORNER(3) = 1
         COUNT(1)=xi_rho_len
         COUNT(2)=s_rho_len
         COUNT(3)=temp_time_len
        if(temp_south_L)THEN
        status=nf_put_vara_real(ncid,temp_south_id,CORNER,COUNT,
     &	temp_south)
        if (status .ne. NF_NOERR)stop 'write temp_south'
        endif
        if(temp_north_L)THEN
        status=nf_put_vara_real(ncid,temp_north_id,CORNER,COUNT,
     &	temp_north)
        if (status .ne. NF_NOERR)stop 'write temp_north'
        endif

         CORNER(1) = 1
         CORNER(2) = 1
         CORNER(3) = 1
         COUNT(1)=eta_rho_len
         COUNT(2)=s_rho_len
         COUNT(3)=salt_time_len
        if(salt_west_L)THEN
        status=nf_put_vara_real(ncid,salt_west_id,CORNER,COUNT,
     &	salt_west)
        if (status .ne. NF_NOERR)stop 'write salt_west'
        endif
        if(salt_east_L)THEN
        status=nf_put_vara_real(ncid,salt_east_id,CORNER,COUNT,
     &	salt_east)
        if (status .ne. NF_NOERR)stop 'write salt_east'
        endif
         CORNER(1) = 1
         CORNER(2) = 1
         CORNER(3) = 1
         COUNT(1)=xi_rho_len
         COUNT(2)=s_rho_len
         COUNT(3)=salt_time_len
        if(salt_south_L)THEN
        status=nf_put_vara_real(ncid,salt_south_id,CORNER,COUNT,
     &	salt_south)
        if (status .ne. NF_NOERR)stop 'write salt_south'
        endif
        if(salt_north_L)THEN
        status=nf_put_vara_real(ncid,salt_north_id,CORNER,COUNT,
     &	salt_north)
        if (status .ne. NF_NOERR)stop 'write salt_north'
        endif
	
C   Add by L. Zheng for the 1-Term DO simulation
         CORNER(1) = 1
         CORNER(2) = 1
         CORNER(3) = 1
         COUNT(1)=eta_rho_len
         COUNT(2)=s_rho_len
         COUNT(3)=temp_time_len
        if(do_west_L)THEN
        status=nf_put_vara_real(ncid,do_west_id,CORNER,COUNT,
     &	do_west)
        if (status .ne. NF_NOERR)stop 'write do_west'
        endif
        if(do_east_L)THEN
        status=nf_put_vara_real(ncid,do_east_id,CORNER,COUNT,
     &	do_east)
        if (status .ne. NF_NOERR)stop 'write do_east'
        endif
         CORNER(1) = 1
         CORNER(2) = 1
         CORNER(3) = 1
         COUNT(1)=xi_rho_len
         COUNT(2)=s_rho_len
         COUNT(3)=temp_time_len
        if(do_south_L)THEN
        status=nf_put_vara_real(ncid,do_south_id,CORNER,COUNT,
     &	do_south)
        if (status .ne. NF_NOERR)stop 'write do_south'
        endif
        if(do_north_L)THEN
        status=nf_put_vara_real(ncid,do_north_id,CORNER,COUNT,
     &	do_north)
        if (status .ne. NF_NOERR)stop 'write do_north'
        endif
C   Add by L. Zheng for the 1-Term DO simulation

      elseif (imode.eq.3) then
        write(*,*)'The netCDF file is closed'
        status=NF_CLOSE(ncid)
      endif

      return
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
