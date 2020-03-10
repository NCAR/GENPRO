      SUBROUTINE GTYEAR(YEAR,UNIT)
      INTEGER YEAR, UNIT
C
C  Get YEAR of earliest research or ferry flight; do so by reading the
C  Summary file attached to UNIT.  It is assumed that the Summary file
C  pointer is at record 2 on entry; GTYEAR leaves the Summary file
C  rewound on exit
C
      CHARACTER *2 FLTTYP
      YEAR=100
C    5 READ(UNIT,'(12X,I2,2X,A2)',END=15) IYR,FLTTYP
    5 READ(UNIT,'(8X,I2,2X,I2,2X,A2)',END=15) IMON,IYR,FLTTYP
      IF ((FLTTYP.EQ.'rf'.OR.FLTTYP.EQ.'ff').AND.IYR.LT.YEAR) YEAR=IYR
C  Check for fiscal year change.
      IF (IMON.GT.9) YEAR = YEAR+1
      GOTO 5
   15 REWIND(UNIT)
      RETURN
      END
