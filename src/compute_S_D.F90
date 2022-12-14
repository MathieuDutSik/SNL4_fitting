PROGRAM COMPUTE_S_D
  USE NETCDF
  IMPLICIT NONE

  integer opt
  CHARACTER*40 FILEI, FILEO
  integer ncid_i, ncid_o
  integer, dimension(nf90_max_var_dims) :: dimids
  integer nsamp, SNLopt
  INTEGER NTH, NFR, NSPEC
  INTEGER iret, ISTAT
  INTEGER ncid_i, ncid_o
  INTEGER nth_dims, nfr_dims, nsamp_dims
  INTEGER var_id, varid_S, varid_D
  INTEGER ISAMP, NSAMP
  CHARACTER(LEN=40) :: INPNAME
  INTEGER :: FHNDL = 43
  REAL, allocatable(*) :: SPEC, S, D
  NAMELIST /PROC/ FILEI, FILEO, SNLopt
  NAMELIST /GRID/ NTH, NFR

  nbArg=command_argument_count()
  if (nbArg .ne. 1) THEN
     STOP 'Number of argument should be 1'
  END IF
  CALL GET_COMMAND_ARGUMENT(1, INPNAME)


  OPEN(FHNDL, FILE = TRIM(INPNAME))
  READ(FHNDL, NML = PROC)
  READ(FHNDL, NML = GRID)
  CLOSE(FHNDL)

  NSPEC = NTH * NFR

  allocate(SPEC(NSPEC), S(NSPEC), D(NSPEC))
  !
  iret=NF90_OPEN(TRIM(FILEI), NF90_NOWRITE, ncid_i)
  CALL GENERIC_NETCDF_ERROR_EVAL(1, ISTAT)
  iret=nf90_inq_varid(ncid_i, "A", varid_a)
  CALL GENERIC_NETCDF_ERROR_EVAL(2, ISTAT)
  ISTAT = NF90_INQUIRE_VARIABLE(ncid_i, varid, dimids = dimids)
  CALL GENERIC_NETCDF_ERROR_EVAL(2, ISTAT)
  ISTAT = nf90_inquire_dimension(ncid_i, dimids(3), len = nsamp)
  CALL GENERIC_NETCDF_ERROR_EVAL(2, ISTAT)
  !
  ! Create the file
  !
  iret = nf90_create(TRIM(FILEO), NF90_CLOBBER, ncid_o)
  CALL GENERIC_NETCDF_ERROR_EVAL(3, iret)
  iret = nf90_def_dim(ncid_o, 'NTH', NTH, nth_dims)
  CALL GENERIC_NETCDF_ERROR_EVAL(3, iret)
  iret = nf90_def_dim(ncid_o, 'NFR', NFR, nfr_dims)
  CALL GENERIC_NETCDF_ERROR_EVAL(3, iret)
  iret = nf90_def_dim(ncid_o, 'NSAMP', NSAMP, nsamp_dims)
  CALL GENERIC_NETCDF_ERROR_EVAL(3, iret)
  iret=nf90_def_var(ncid, 'S', NF90_REAL, (/ nth_dims, nfr_dims, nsamp_dims /),var_id)
  CALL GENERIC_NETCDF_ERROR_WWM(CallFct, 4, iret)
  iret=nf90_def_var(ncid, 'D', NF90_REAL, (/ nth_dims, nfr_dims, nsamp_dims /),var_id)
  CALL GENERIC_NETCDF_ERROR_WWM(CallFct, 4, iret)
  iret=nf90_close(ncid_o)
  CALL GENERIC_NETCDF_ERROR_EVAL(4, iret)
  !
  ! now write to it
  !
  iret = nf90_open(TRIM(FILEO), NF90_WRITE, ncid_o)
  CALL GENERIC_NETCDF_ERROR_EVAL(3, ISTAT)
  iret=nf90_inq_varid(ncid_i, "S", varid_S)
  CALL GENERIC_NETCDF_ERROR_EVAL(2, ISTAT)
  iret=nf90_inq_varid(ncid_i, "D", varid_D)
  CALL GENERIC_NETCDF_ERROR_EVAL(2, ISTAT)
  DO ISAMP=1,NSAMP
     ISTAT = NF90_GET_VAR(ncid_i, varid_a, A,    start = (/ 1, 1, ISAMP /), count = (/ NTH, NFR, 1 /))
     CALL GENERIC_NETCDF_ERROR_EVAL(3, ISTAT)
     !
     CALL SNL_EVAL(SPEC, WN, CG, DEPTH, WNMEAN, SNLopt, S, D)
     !
     iret = nf90_put_var(ncid_o,varid_s, S,     start = (/ 1, 1, ISAMP /), count = (/ NTH, NFR, 1 /))
     CALL GENERIC_NETCDF_ERROR_EVAL(3, ISTAT)
     iret = nf90_put_var(ncid_o,varid_d, D,     start = (/ 1, 1, ISAMP /), count = (/ NTH, NFR, 1 /))
     CALL GENERIC_NETCDF_ERROR_EVAL(3, ISTAT)
  END DO
  !
  ! Now closing
  !
  iret=nf90_close(ncid_i)
  CALL GENERIC_NETCDF_ERROR_EVAL(3, iret)
  iret=nf90_close(ncid_o)
  CALL GENERIC_NETCDF_ERROR_EVAL(4, iret)
  !
  deallocate(SPEC, S, D)
  !
CONTAINS
  SUBROUTINE GENERIC_NETCDF_ERROR_EVAL(idx, iret)
  integer, intent(in) :: iret, idx
  IF (iret .NE. nf90_noerr) THEN
     CHRERR = nf90_strerror(iret)
     Print *, 'Error Message: ', TRIM(CHRERR)
     STOP 'COMPUTE_S_D failed'
  ENDIF
  END SUBROUTINE GENERIC_NETCDF_ERROR_EVAL


  SUBROUTINE SNL_EVAL(SPEC, CG, WN, DEPTH, WNMEAN, SNLopt, S, D)
  IMPLICIT NONE
  REAL, intent(in) :: SPEC(NSPEC)
  REAL, intent(in) :: CG(NSPEC)
  REAL, intent(in) :: WN(NSPEC)
  REAL, intent(in) :: DEPTH, WNMEAN
  INTEGER, intent(in) :: SNLopt
  REAL, intent(out) :: S(NSPEC), D(NSPEC)
  IF (SNLopt .eq. 1) THEN
     CALL W3SNL1 ( SPEC, CG1, WNMEAN*DEPTH,        S, D )
  END IF
  IF (SNLopt .eq. 2) THEN
     CALL W3SNL2 ( SPEC, CG1, WN1, DEPTH,          S, D )
  END IF
  IF (SNLopt .eq. 3) THEN
     CALL W3SNL3 ( SPEC, CG1, WN1, DEPTH,          S, D )
  END IF
  IF (SNLopt .eq. 4) THEN
        CALL W3SNL4 ( SPEC, CG1, WN1, DEPTH,          S, D )
  END IF
  IF (SNLopt .eq. 5) THEN
     CALL W3SNL5 ( SPEC, CG1, WN1, FMEAN, QI5TSTART,          &
                            U10ABS, U10DIR, JSEA, S, D, QR5KURT)
  END IF

  END SUBROUTINE SNL_EVAL

END PROGRAM COMPUTE_S_D
