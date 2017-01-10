module subs
  use SimModule, only: store_error, ustop
  private
  public :: findcell, urword, same_word
contains
  
subroutine findcell(k,i,j,nlay,nrow,ncol,node)
! ******************************************************************
!   Return layer (k), row (i), and column (j) that correspond to a
!   node number
! ******************************************************************
!
  implicit none
  integer, intent(in) :: ncol, nlay, nrow, node
  integer, intent(inout) :: i, j, k
  integer :: nplay, nodes, n
  nplay=nrow*ncol
  nodes=nplay*nlay
!
  if (node == 0) then
    k = 0
    i = 0
    j = 0
  elseif (node < 0 .or. node > nodes) then
    write(*,'(a,i0)')'NODE value invalid in findcell: ', node
    stop
  else
    k=(node-1)/nplay+1
    n=node-(k-1)*nplay
    i=(n-1)/ncol+1
    j=n-(i-1)*ncol
  endif
!
  return
end subroutine findcell


      SUBROUTINE URWORD(LINE,ICOL,ISTART,ISTOP,NCODE,N,R,IOUT,IN)
!C     ******************************************************************
!C     ROUTINE TO EXTRACT A WORD FROM A LINE OF TEXT, AND OPTIONALLY
!C     CONVERT THE WORD TO A NUMBER.
!C        ISTART AND ISTOP WILL BE RETURNED WITH THE STARTING AND
!C          ENDING CHARACTER POSITIONS OF THE WORD.
!C        THE LAST CHARACTER IN THE LINE IS SET TO BLANK SO THAT IF ANY
!C          PROBLEMS OCCUR WITH FINDING A WORD, ISTART AND ISTOP WILL
!C          POINT TO THIS BLANK CHARACTER.  THUS, A WORD WILL ALWAYS BE
!C          RETURNED UNLESS THERE IS A NUMERIC CONVERSION ERROR.  BE SURE
!C          THAT THE LAST CHARACTER IN LINE IS NOT AN IMPORTANT CHARACTER
!C          BECAUSE IT WILL ALWAYS BE SET TO BLANK.
!C        A WORD STARTS WITH THE FIRST CHARACTER THAT IS NOT A SPACE OR
!C          COMMA, AND ENDS WHEN A SUBSEQUENT CHARACTER THAT IS A SPACE
!C          OR COMMA.  NOTE THAT THESE PARSING RULES DO NOT TREAT TWO
!C          COMMAS SEPARATED BY ONE OR MORE SPACES AS A NULL WORD.
!C        FOR A WORD THAT BEGINS WITH "'", THE WORD STARTS WITH THE
!C          CHARACTER AFTER THE QUOTE AND ENDS WITH THE CHARACTER
!C          PRECEDING A SUBSEQUENT QUOTE.  THUS, A QUOTED WORD CAN
!C          INCLUDE SPACES AND COMMAS.  THE QUOTED WORD CANNOT CONTAIN
!C          A QUOTE CHARACTER.
!C        IF NCODE IS 1, THE WORD IS CONVERTED TO UPPER CASE.
!C        IF NCODE IS 2, THE WORD IS CONVERTED TO AN INTEGER.
!C        IF NCODE IS 3, THE WORD IS CONVERTED TO A REAL NUMBER.
!C        NUMBER CONVERSION ERROR IS WRITTEN TO UNIT IOUT IF IOUT IS
!C          POSITIVE; ERROR IS WRITTEN TO DEFAULT OUTPUT IF IOUT IS 0;
!C          NO ERROR MESSAGE IS WRITTEN IF IOUT IS NEGATIVE.
!C     ******************************************************************
!C
!C        SPECIFICATIONS:
!C     ------------------------------------------------------------------
      double precision,intent(inout) :: r
      CHARACTER*(*) LINE
      CHARACTER*20 STRING
      CHARACTER*30 RW
      CHARACTER*1 TAB
      character(len=200) :: msg
!C     ------------------------------------------------------------------
      TAB=CHAR(9)
!C
!C1------Set last char in LINE to blank and set ISTART and ISTOP to point
!C1------to this blank as a default situation when no word is found.  If
!C1------starting location in LINE is out of bounds, do not look for a
!C1------word.
      LINLEN=LEN(LINE)
      LINE(LINLEN:LINLEN)=' '
      ISTART=LINLEN
      ISTOP=LINLEN
      LINLEN=LINLEN-1
      IF(ICOL.LT.1 .OR. ICOL.GT.LINLEN) GO TO 100
!C
!C2------Find start of word, which is indicated by first character that
!C2------is not a blank, a comma, or a tab.
      DO 10 I=ICOL,LINLEN
      IF(LINE(I:I).NE.' ' .AND. LINE(I:I).NE.',' &
     &    .AND. LINE(I:I).NE.TAB) GO TO 20
10    CONTINUE
      ICOL=LINLEN+1
      GO TO 100
!C
!C3------Found start of word.  Look for end.
!C3A-----When word is quoted, only a quote can terminate it.
20    IF(LINE(I:I).EQ.'''') THEN
         I=I+1
         IF(I.LE.LINLEN) THEN
            DO 25 J=I,LINLEN
            IF(LINE(J:J).EQ.'''') GO TO 40
25          CONTINUE
         END IF
!C
!C3B-----When word is not quoted, space, comma, or tab will terminate.
      ELSE
         DO 30 J=I,LINLEN
         IF(LINE(J:J).EQ.' ' .OR. LINE(J:J).EQ.',' &
     &    .OR. LINE(J:J).EQ.TAB) GO TO 40
30       CONTINUE
      END IF
!C
!C3C-----End of line without finding end of word; set end of word to
!C3C-----end of line.
      J=LINLEN+1
!C
!C4------Found end of word; set J to point to last character in WORD and
!C-------set ICOL to point to location for scanning for another word.
40    ICOL=J+1
      J=J-1
      IF(J.LT.I) GO TO 100
      ISTART=I
      ISTOP=J
!C
!C5------Convert word to upper case and RETURN if NCODE is 1.
      IF(NCODE.EQ.1) THEN
         IDIFF=ICHAR('a')-ICHAR('A')
         DO 50 K=ISTART,ISTOP
            IF(LINE(K:K).GE.'a' .AND. LINE(K:K).LE.'z') &
     &             LINE(K:K)=CHAR(ICHAR(LINE(K:K))-IDIFF)
50       CONTINUE
         RETURN
      END IF
!C
!C6------Convert word to a number if requested.
100   IF(NCODE.EQ.2 .OR. NCODE.EQ.3) THEN
         RW=' '
         L=30-ISTOP+ISTART
         IF(L.LT.1) GO TO 200
         RW(L:30)=LINE(ISTART:ISTOP)
         IF(NCODE.EQ.2) READ(RW,'(I30)',ERR=200) N
         IF(NCODE.EQ.3) READ(RW,'(F30.0)',ERR=200) R
      END IF
      RETURN
!C
!C7------Number conversion error.
200   IF(NCODE.EQ.3) THEN
         STRING= 'A REAL NUMBER'
         L=13
      ELSE
         STRING= 'AN INTEGER'
         L=10
      END IF
!C
!C7A-----If output unit is negative, set last character of string to 'E'.
      IF(IOUT.LT.0) THEN
         N=0
         R=0.
         LINE(LINLEN+1:LINLEN+1)='E'
         RETURN
!C
!C7B-----If output unit is positive; write a message to output unit.
      ELSE IF(IOUT.GT.0) THEN
         IF(IN.GT.0) THEN
            WRITE(IOUT,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
         ELSE
            WRITE(IOUT,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
         END IF
201      FORMAT(1X,/1X,'FILE UNIT ',I4,' : ERROR CONVERTING "',A, &
     &       '" TO ',A,' IN LINE:',/1X,A)
202      FORMAT(1X,/1X,'KEYBOARD INPUT : ERROR CONVERTING "',A, &
     &       '" TO ',A,' IN LINE:',/1X,A)
!C
!C7C-----If output unit is 0; write a message to default output.
      ELSE
         IF(IN.GT.0) THEN
            WRITE(*,201) IN,LINE(ISTART:ISTOP),STRING(1:L),LINE
         ELSE
            WRITE(*,202) LINE(ISTART:ISTOP),STRING(1:L),LINE
         END IF
      END IF
!C
!C7D-----STOP after storing error message.
      call lowcase(string)
      if (in > 0) then
        write(msg,205)in,line(istart:istop),trim(string)
      else
        write(msg,207)line(istart:istop),trim(string)
      endif
205   format('File unit ',I0,': Error converting "',A, &
     &       '" to ',A,' in following line:')
207   format('Keyboard input: Error converting "',A, &
     &       '" to ',A,' in following line:')
      call store_error(msg)
      call store_error(trim(line))
      call ustop()
      !
      END SUBROUTINE URWORD

      subroutine lowcase(word)
!     ******************************************************************
!     Convert a character string to all lower case
!     ******************************************************************
!       specifications:
!     ------------------------------------------------------------------
      implicit none
      ! dummy
      character(len=*) :: word
      ! local
      integer :: idiff, k, l
!
!------compute the difference between lowercase and uppercase.
      l = len(word)
      idiff=ichar('a')-ichar('A')
!
!------loop through the string and convert any uppercase characters.
      do k=1,l
        if(word(k:k).ge.'A' .and. word(k:k).le.'Z') then
          word(k:k)=char(ichar(word(k:k))+idiff)
        endif
      enddo
!
!------return.
      return
      end subroutine lowcase

  logical function same_word(word1, word2)
    ! Perform a case-insensitive comparison of two words
    implicit none
    ! -- dummy variables
    character(len=*), intent(in) :: word1, word2
    ! -- local variables
    character(len=200) :: upword1, upword2
    !
    upword1 = word1
    call upcase(upword1)
    upword2 = word2
    call upcase(upword2)
    same_word = (upword1==upword2)
    return
  end function same_word
  
      SUBROUTINE UPCASE(WORD)
!C     ******************************************************************
!C     CONVERT A CHARACTER STRING TO ALL UPPER CASE
!C     ******************************************************************
!C       SPECIFICATIONS:
!C     ------------------------------------------------------------------
      CHARACTER WORD*(*)
!C
!C1------Compute the difference between lowercase and uppercase.
      L = LEN(WORD)
      IDIFF=ICHAR('a')-ICHAR('A')
!C
!C2------Loop through the string and convert any lowercase characters.
      DO 10 K=1,L
      IF(WORD(K:K).GE.'a' .AND. WORD(K:K).LE.'z') &
     &   WORD(K:K)=CHAR(ICHAR(WORD(K:K))-IDIFF)
10    CONTINUE
!C
!C3------return.
      RETURN
      END SUBROUTINE upcase

end module subs
