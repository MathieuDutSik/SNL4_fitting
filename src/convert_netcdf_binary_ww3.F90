PROGRAM CONVERT_NETCDF_BINARY
  USE NETCDF
  IMPLICIT NONE
  CHARACTER*40 FILEI, FILEO
  integer ncid_i
  integer, dimension(nf90_max_var_dims) :: dimids
  integer nsamp, NOPTS
  INTEGER NTH, NK, NSPEC
  INTEGER I, J, nbArg
  INTEGER iret, ierr
  INTEGER varid_a, varid_dw
  INTEGER varid_wnd_mag, varid_wnd_dir, varid_tau_mag, varid_tau_dir
  INTEGER varid_rhoair, varid_seto, varid_as
  INTEGER varid_curr_mag, varid_curr_dir
  INTEGER varid_ice, varid_iceh, varid_icef

  CHARACTER(LEN=40) :: INPNAME
  INTEGER :: FHNDL = 43
  REAL, allocatable :: SPEC(:), SING_ARR(:)
  REAL, ALLOCATABLE :: PTLOC(:,:)
  CHARACTER(LEN=40), ALLOCATABLE :: PTNME(:)
  CHARACTER(LEN=13), PARAMETER :: GRDID = "unused"
  CHARACTER(LEN=31), PARAMETER :: IDSTR = 'WAVEWATCH III POINT OUTPUT FILE'
  CHARACTER(LEN=10), PARAMETER :: VEROPT = '2021-04-06'
  REAL CAO_I, CDO_I, ASO_I, DAIRO_I, DPO_I
  REAL WAO_I, WDO_I, TAUAO_I, TAUDO_I
  REAL ICEO_I, ICEFO_I, ICEHO_I, ZET_SETO_I
  INTEGER II_I, IL_I, IW_I
  INTEGER NDSOP
  NAMELIST /PROC/ FILEI, FILEO
  NAMELIST /GRID/ NTH, NK

  nbArg=command_argument_count()
  if (nbArg .ne. 1) THEN
     STOP 'Number of argument should be 1'
  END IF
  CALL GET_COMMAND_ARGUMENT(1, INPNAME)


  OPEN(FHNDL, FILE = TRIM(INPNAME))
  READ(FHNDL, NML = PROC)
  READ(FHNDL, NML = GRID)
  CLOSE(FHNDL)

  NSPEC = NTH * NK
  NDSOP = 20


  allocate(SPEC(NSPEC), SING_ARR(1))
  !
  iret=NF90_OPEN(TRIM(FILEI), NF90_NOWRITE, ncid_i)
  CALL GENERIC_NETCDF_ERROR_EVAL(1, iret)
  iret=nf90_inq_varid(ncid_i, "A", varid_a)
  CALL GENERIC_NETCDF_ERROR_EVAL(2, iret)
  iret=nf90_inq_varid(ncid_i, "DW", varid_dw)
  CALL GENERIC_NETCDF_ERROR_EVAL(3, iret)
  iret=nf90_inq_varid(ncid_i, "WND_MAG", varid_wnd_mag)
  CALL GENERIC_NETCDF_ERROR_EVAL(4, iret)
  iret=nf90_inq_varid(ncid_i, "WND_DIR", varid_wnd_dir)
  CALL GENERIC_NETCDF_ERROR_EVAL(5, iret)
  iret=nf90_inq_varid(ncid_i, "TAU_MAG", varid_tau_mag)
  CALL GENERIC_NETCDF_ERROR_EVAL(6, iret)
  iret=nf90_inq_varid(ncid_i, "TAU_DIR", varid_tau_dir)
  CALL GENERIC_NETCDF_ERROR_EVAL(7, iret)
  iret=nf90_inq_varid(ncid_i, "RHOAIR", varid_rhoair)
  CALL GENERIC_NETCDF_ERROR_EVAL(8, iret)
  iret=nf90_inq_varid(ncid_i, "SETUP", varid_seto)
  CALL GENERIC_NETCDF_ERROR_EVAL(9, iret)
  iret=nf90_inq_varid(ncid_i, "AS", varid_as)
  CALL GENERIC_NETCDF_ERROR_EVAL(10, iret)
  iret=nf90_inq_varid(ncid_i, "CURR_MAG", varid_curr_mag)
  CALL GENERIC_NETCDF_ERROR_EVAL(11, iret)
  iret=nf90_inq_varid(ncid_i, "CURR_DIR", varid_curr_dir)
  CALL GENERIC_NETCDF_ERROR_EVAL(12, iret)
  iret=nf90_inq_varid(ncid_i, "ICE", varid_ice)
  CALL GENERIC_NETCDF_ERROR_EVAL(13, iret)
  iret=nf90_inq_varid(ncid_i, "ICEH", varid_iceh)
  CALL GENERIC_NETCDF_ERROR_EVAL(14, iret)
  iret=nf90_inq_varid(ncid_i, "ICEF", varid_icef)
  CALL GENERIC_NETCDF_ERROR_EVAL(15, iret)
  iret = NF90_INQUIRE_VARIABLE(ncid_i, varid_a, dimids = dimids)
  CALL GENERIC_NETCDF_ERROR_EVAL(16, iret)
  iret = nf90_inquire_dimension(ncid_i, dimids(3), len = nsamp)
  CALL GENERIC_NETCDF_ERROR_EVAL(17, iret)
  !
  ! now write to it
  !
  OPEN(NDSOP, FILE=FILEO, form='unformatted', IOSTAT=IERR)
  NOPTS = NSAMP
  allocate(PTLOC(2,NSAMP), PTNME(NSAMP))
  WRITE (NDSOP) IDSTR, VEROPT, NK, NTH, NOPTS
  WRITE (NDSOP) ((PTLOC(J,I),J=1,2),I=1,NOPTS), (PTNME(I),I=1,NOPTS)

  DO I=1,NSAMP
     iret = NF90_GET_VAR(ncid_i, varid_a, SPEC,    start = (/ 1, 1, I /), count = (/ NTH, NK, 1 /))
     CALL GENERIC_NETCDF_ERROR_EVAL(18, iret)
     !
     iret = NF90_GET_VAR(ncid_i, varid_dw, SING_ARR, start = (/ I /), count = (/ 1 /))
     CALL GENERIC_NETCDF_ERROR_EVAL(19, iret)
     DPO_I = SING_ARR(1)
     !
     iret = NF90_GET_VAR(ncid_i, varid_wnd_mag, SING_ARR, start = (/ I /), count = (/ 1 /))
     CALL GENERIC_NETCDF_ERROR_EVAL(20, iret)
     WAO_I = SING_ARR(1)
     !
     iret = NF90_GET_VAR(ncid_i, varid_wnd_dir, SING_ARR, start = (/ I /), count = (/ 1 /))
     CALL GENERIC_NETCDF_ERROR_EVAL(21, iret)
     WDO_I = SING_ARR(1)
     !
     iret = NF90_GET_VAR(ncid_i, varid_tau_mag, SING_ARR, start = (/ I /), count = (/ 1 /))
     CALL GENERIC_NETCDF_ERROR_EVAL(22, iret)
     TAUAO_I = SING_ARR(1)
     !
     iret = NF90_GET_VAR(ncid_i, varid_tau_dir, SING_ARR, start = (/ I /), count = (/ 1 /))
     CALL GENERIC_NETCDF_ERROR_EVAL(22, iret)
     TAUDO_I = SING_ARR(1)
     !
     iret = NF90_GET_VAR(ncid_i, varid_rhoair, SING_ARR, start = (/ I /), count = (/ 1 /))
     CALL GENERIC_NETCDF_ERROR_EVAL(23, iret)
     DAIRO_I = SING_ARR(1)
     !
     iret = NF90_GET_VAR(ncid_i, varid_seto, SING_ARR, start = (/ I /), count = (/ 1 /))
     CALL GENERIC_NETCDF_ERROR_EVAL(24, iret)
     ZET_SETO_I = SING_ARR(1)
     !
     iret = NF90_GET_VAR(ncid_i, varid_as, SING_ARR, start = (/ I /), count = (/ 1 /))
     CALL GENERIC_NETCDF_ERROR_EVAL(25, iret)
     ASO_I = SING_ARR(1)
     !
     iret = NF90_GET_VAR(ncid_i, varid_curr_mag, SING_ARR, start = (/ I /), count = (/ 1 /))
     CALL GENERIC_NETCDF_ERROR_EVAL(26, iret)
     CAO_I = SING_ARR(1)
     !
     iret = NF90_GET_VAR(ncid_i, varid_curr_dir, SING_ARR, start = (/ I /), count = (/ 1 /))
     CALL GENERIC_NETCDF_ERROR_EVAL(27, iret)
     CDO_I = SING_ARR(1)
     !
     iret = NF90_GET_VAR(ncid_i, varid_ice, SING_ARR, start = (/ I /), count = (/ 1 /))
     CALL GENERIC_NETCDF_ERROR_EVAL(28, iret)
     ICEO_I = SING_ARR(1)
     !
     iret = NF90_GET_VAR(ncid_i, varid_iceh, SING_ARR, start = (/ I /), count = (/ 1 /))
     CALL GENERIC_NETCDF_ERROR_EVAL(29, iret)
     ICEHO_I = SING_ARR(1)
     !
     iret = NF90_GET_VAR(ncid_i, varid_icef, SING_ARR, start = (/ I /), count = (/ 1 /))
     CALL GENERIC_NETCDF_ERROR_EVAL(30, iret)
     ICEFO_I = SING_ARR(1)
     !
     IW_I = 0
     II_I = 0
     IL_I = 0
     WRITE (NDSOP)                                            &
          IW_I, II_I, IL_I, DPO_I, WAO_I, WDO_I,              &
#ifdef W3_FLX5
          TAUAO_I, TAUDO_I, DAIRO_I,                          &
#endif
#ifdef W3_SETUP
          ZET_SETO_I,                                         &
#endif
          ASO_I, CAO_I, CDO_I, ICEO_I, ICEHO_I,               &
          ICEFO_I, GRDID, (SPEC(J),J=1,NSPEC)
  END DO
  CLOSE(NDSOP)
  !
  ! Now closing
  !
  iret=nf90_close(ncid_i)
  CALL GENERIC_NETCDF_ERROR_EVAL(31, iret)
  !
  deallocate(SPEC, SING_ARR)
  !
CONTAINS
  SUBROUTINE GENERIC_NETCDF_ERROR_EVAL(idx, iret)
    integer, intent(in) :: iret, idx
    character(len=100) :: CHRERR
    IF (iret .NE. nf90_noerr) THEN
       CHRERR = nf90_strerror(iret)
       Print *, "Error at GENERIC_NETCDF_ERROR_EVAL, idx=", idx
       Print *, 'Error Message: ', TRIM(CHRERR)
       STOP 'COMPUTE_S_D failed'
    ENDIF
  END SUBROUTINE GENERIC_NETCDF_ERROR_EVAL

END PROGRAM CONVERT_NETCDF_BINARY
