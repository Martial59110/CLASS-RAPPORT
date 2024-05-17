      ****************************************************************** 
      *    
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. promo.

      ****************************************************************** 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT
               ASSIGN TO 'input.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-INPUT-STATUS.

           SELECT F-OUTPUT
               ASSIGN TO 'output.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-OUTPUT-STATUS.

      ****************************************************************** 
       DATA DIVISION.
       FILE SECTION.
       FD  F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS 
           RECORDING MODE IS V.

       01  REC-F-INPUT-2         PIC X(02).

       01  REC-STUDENT.
           03 R-S-KEY            PIC 9(02).       
           03 R-S-LASTNAME       PIC X(07).       
           03 R-S-FIRSTNAME      PIC X(06).       
           03 R-S-AGE            PIC 9(02).       

       01  REC-COURSE.
           03 R-C-KEY            PIC 9(02).       
           03 R-C-LABEL          PIC X(21).       
           03 R-C-COEF           PIC X(03).       
           03 R-C-GRADE          PIC X(05).

       FD  F-OUTPUT
           RECORD CONTAINS 2000 CHARACTERS
           RECORDING MODE IS F.
       01  REC-F-OUTPUT        PIC X(2000).

      ******************************************************************
       WORKING-STORAGE SECTION.
       01  F-INPUT-STATUS      PIC X(02) VALUE SPACE.
           88 F-INPUT-STATUS-OK    VALUE '00'.        
           88 F-INPUT-STATUS-EOF   VALUE '10'.

       01  F-OUTPUT-STATUS     PIC X(02) VALUE SPACE.
           88 F-OUTPUT-STATUS-OK    VALUE '00'.        
           88 F-OUTPUT-STATUS-EOF   VALUE '10'.

       01  DATA-STUDENT.
           03 STUDENT-LGTH     PIC 9(03) VALUE 1.
           03 STUDENT 
               OCCURS 7 TIMES.
              
                   05 S-FIRSTNAME  PIC X(20).
                   05 S-LASTNAME   PIC X(20).
                   05 S-AGE        PIC 9(02).

       01  DATA-COURSE.
           03 COURSE-LGTH     PIC 9(03) VALUE 1.
           03 COURSE
               OCCURS 46 TIMES.
              
                   05 C-COEF       PIC 9V99.
                   05 C-LABEL      PIC X(25).

       01  DATA-GRADE.
           03 GRADE-LGTH      PIC 9(03) VALUE 1.
           03 GRADE
               OCCURS 46 TIMES.
           
                    05 G-S-FULLNAME     PIC X(40).
                   05 G-C-LABEL        PIC X(25).
                   05 G-GRADE          PIC 99V99.

       01  WS-BUFFER   PIC X(200) VALUE SPACE.
           88  WS-VALUE-NOT-PRESENT VALUE 'Y'.

       01  WS-PNT.
           03 WS-PNT-GRADE    PIC Z9,99.
           03 WS-PNT-COEF     PIC 9,9.

       01  WS-IDX PIC 99 VALUE 1.
       01  WS-IDX2 PIC 99 VALUE 1.
       01  WS-IDX3 PIC 99 VALUE 1.
        01  NOTE PIC 999V99.
       01  COEFFICIENT PIC 9V99.
        01  MOYENNE PIC 99V99.
       01  MOYENNE-ARRAY PIC 999V99 OCCURS 7 TIMES.
                                 
       01  WS-COUNT PIC 99 VALUE 0.
       01  WS-COUNT2 PIC 99 VALUE 0.
     
       01  LINE1 PIC X(300) VALUE ALL "*" . 
       01  LINE2.
           03  FILLER PIC X VALUE "*".
           03 FILLER PIC X(150) VALUE ALL " ".
           03 FILLER PIC X(30) VALUE "BULLETIN DE NOTES".
           03 FILLER PIC X(118) VALUE ALL " ".
           03 FILLER PIC X VALUE "*".
       01  LINE3.
           03 FILLER PIC X VALUE "*".
           03 FILLER PIC X(53) VALUE ALL " ".
           03 FILLER PIC X VALUE "*".
           03 FILLER PIC X(34) VALUE ALL " ".
           03 FILLER PIC X VALUE "*".
       01  LINE4.
           03 FILLER PIC X VALUE "*".
           03 FILLER PIC X(30) VALUE ALL " ".
           03 FILLER PIC X(10) VALUE "NOMBRE DE ".
           03 VAR PIC X(6) VALUE "ELEVES".
           03 FILLER PIC X VALUE " ".
           03 WS-PNT-NBR PIC Z9.
       01  LINE5.
           03 FILLER PIC X VALUE "*".
           03 FILLER PIC X(14) VALUE ALL " ".
           03 FILLER PIC X(7) VALUE "ELEVES:".
           03 FILLER PIC X(32) VALUE ALL " ".
           03 FILLER PIC X VALUE "*".
           03 FILLER PIC X(7) VALUE ALL " ".
           03 FILLER PIC X(18) VALUE "MOYENNE GENERALE:".
           03 FILLER PIC X(9) VALUE ALL " ".
           03 FILLER PIC X VALUE "*".
           03 FILLER PIC X(12) VALUE ALL " ".
           03 WS-COURSE-DISPLAY PIC X(30) OCCURS 46 TIMES.
                                         
    
       01  LINE6.
           03 FILLER PIC X VALUE "*".
           03 FILLER PIC X(4) VALUE ALL " ".
           03 FULLNAME PIC X(40).
           03 FILLER PIC X(9) VALUE ALL " ".
           03 FILLER PIC X VALUE "*".
           03 FILLER PIC X(14) VALUE ALL " ".
           03 MOY PIC 99,99.
           03 FILLER PIC X(15) VALUE ALL " ".
           03 FILLER PIC X VALUE "*".
           03 FILLER PIC X(14) VALUE ALL " ".
           03 NONO OCCURS 6 TIMES.
               05 BLA.
                   07 FILLER pic X(10) VALUE SPACES.
                   07 BLA-VALUE PIC 99,99.
                   07 FILLER PIC X(13) VALUE SPACES.
                                         
OCESQL*    EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  DBNAME                  PIC  X(30) VALUE 'cobolesque'.
       01  USERNAME                PIC  X(30) VALUE 'cobol'.
       01  PASSWD                  PIC  X(10) VALUE SPACE.

       01  WS-SQL-STUDENT.
           05  SQL-S-ID                 PIC 9(05).
           05  SQL-S-LASTNAME           PIC X(20).
           05  SQL-S-FIRSTNAME          PIC X(20).
           05  SQL-S-AGE                PIC 9(04).
    

       01  WS-SQL-COURSE.
           05  SQL-C-ID                 PIC 9(05).
           05  SQL-C-LABEL              PIC X(07).
           05  SQL-C-COEF               PIC 9V99.
       

       01  WS-SQL-GRADE.
           05  SQL-G-ID-STUDENT            PIC 9(05).
           05  SQL-G-ID-COURSE             PIC 9(05).
           05  SQL-G-ID                    PIC 9(05).
           05  SQL-G-GRADE                 PIC 9(05).
       

OCESQL*    EXEC SQL END DECLARE SECTION END-EXEC.
  
OCESQL*    EXEC SQL INCLUDE SQLCA END-EXEC.
OCESQL     copy "sqlca.cbl".

           
OCESQL*
OCESQL 01  SQ0001.
OCESQL     02  FILLER PIC X(014) VALUE "DISCONNECT ALL".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0002.
OCESQL     02  FILLER PIC X(071) VALUE "INSERT INTO tabstudent (lastna"
OCESQL  &  "me, firstname, age) VALUES ( $1, $2, $3 )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(052) VALUE "INSERT INTO tabcourse(coef, la"
OCESQL  &  "bel) VALUES ( $1, $2 )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0004.
OCESQL     02  FILLER PIC X(041) VALUE "INSERT INTO tabgrade(grade) VA"
OCESQL  &  "LUES ( $1 )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
       PROCEDURE DIVISION.

OCESQL*    EXEC SQL
OCESQL*        CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLConnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE USERNAME
OCESQL          BY VALUE 30
OCESQL          BY REFERENCE PASSWD
OCESQL          BY VALUE 10
OCESQL          BY REFERENCE DBNAME
OCESQL          BY VALUE 30
OCESQL     END-CALL.

       1000-MAIN-START.
           PERFORM 7000-READ-START THRU 7000-READ-END. 


           PERFORM 7100-WRITE-START THRU 7100-WRITE-END.
       1000-MAIN-END.
OCESQL*    EXEC SQL COMMIT WORK END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "COMMIT" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
OCESQL*    EXEC SQL DISCONNECT ALL END-EXEC. 
OCESQL     CALL "OCESQLDisconnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL     END-CALL.
           STOP RUN.
      ****************************************************************** 
       7000-READ-START.
           OPEN INPUT F-INPUT.          

           IF NOT F-INPUT-STATUS-OK
               DISPLAY 'ERROR INPUT FILE'
               GO TO 7000-READ-END
           END-IF.

           PERFORM UNTIL F-INPUT-STATUS-EOF
               READ F-INPUT
               IF F-INPUT-STATUS-EOF
                   GO TO 7000-READ-END
               END-IF
               EVALUATE REC-F-INPUT-2
                   WHEN '01'
                       PERFORM 8010-HANDLE-STUDENT-START 
                           THRU 8010-HANDLE-STUDENT-END
                   WHEN '02'
                       PERFORM 8020-HANDLE-COURSE-START 
                           THRU 8020-HANDLE-COURSE-END
                       PERFORM 8030-HANDLE-GRADE-START
                           THRU 8030-HANDLE-GRADE-END
           END-PERFORM.

       7000-READ-END.
           SET GRADE-LGTH COURSE-LGTH STUDENT-LGTH DOWN BY 1.
           CLOSE F-INPUT.  
      ******************************************************************
       7100-WRITE-START.
           OPEN OUTPUT F-OUTPUT.
           PERFORM 9010-HEADER-START   THRU 9010-HEADER-END.

           PERFORM 9030-BODY-START     THRU 9030-BODY-END.

           PERFORM 9020-FOOTER-START   THRU 9020-FOOTER-END.
       7100-WRITE-END.
           CLOSE F-OUTPUT.
      ******************************************************************  
       8010-HANDLE-STUDENT-START.
           MOVE R-S-FIRSTNAME  TO S-FIRSTNAME(STUDENT-LGTH).
           MOVE R-S-LASTNAME   TO S-LASTNAME(STUDENT-LGTH).
           MOVE R-S-AGE        TO S-AGE(STUDENT-LGTH).
           MOVE R-S-LASTNAME  TO SQL-S-LASTNAME. 
           MOVE R-S-FIRSTNAME  TO SQL-S-FIRSTNAME. 
           MOVE R-S-AGE TO SQL-S-AGE.
       
OCESQL*     EXEC SQL
OCESQL*     INSERT INTO tabstudent (lastname,firstname,age) 
OCESQL*        VALUES (
OCESQL*            :SQL-S-LASTNAME, 
OCESQL*            :SQL-S-FIRSTNAME,
OCESQL*            :SQL-S-AGE
OCESQL*            )
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 20
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-S-LASTNAME
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 20
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-S-FIRSTNAME
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 4
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-S-AGE
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecParams" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0002
OCESQL          BY VALUE 3
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
           SET STUDENT-LGTH UP BY 1.           
       8010-HANDLE-STUDENT-END.
      *****************************************************************s* 
       8020-HANDLE-COURSE-START.
           INITIALIZE WS-BUFFER.
        


               MOVE R-C-COEF   TO C-COEF(COURSE-LGTH).
               MOVE R-C-LABEL  TO C-LABEL(COURSE-LGTH).
               
               
               MOVE C-COEF(COURSE-LGTH) TO SQL-C-COEF .
           
               MOVE C-LABEL(COURSE-LGTH) TO SQL-C-LABEL.
               
OCESQL*         EXEC SQL
OCESQL*     INSERT INTO tabcourse(coef,label) 
OCESQL*        VALUES (
OCESQL*            :SQL-C-COEF, 
OCESQL*            :SQL-C-LABEL
OCESQL*            )
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 3
OCESQL          BY VALUE -2
OCESQL          BY REFERENCE SQL-C-COEF
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 7
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-C-LABEL
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecParams" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0003
OCESQL          BY VALUE 2
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
           
               SET COURSE-LGTH UP BY 1.
          
       8020-HANDLE-COURSE-END.
      ****************************************************************** 
       8030-HANDLE-GRADE-START.
          
           STRING 
               S-FIRSTNAME(STUDENT-LGTH - 1) 
               S-LASTNAME(STUDENT-LGTH - 1) 
               DELIMITED BY SIZE 
           INTO G-S-FULLNAME(GRADE-LGTH).

           MOVE R-C-LABEL TO G-C-LABEL(GRADE-LGTH).
           MOVE R-C-GRADE TO G-GRADE(GRADE-LGTH).
           DISPLAY SQLCODE.
OCESQL*    EXEC SQL
OCESQL*     INSERT INTO tabgrade(grade) 
OCESQL*        VALUES (
OCESQL*            :SQL-G-GRADE
OCESQL*            )
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 5
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-G-GRADE
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecParams" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0004
OCESQL          BY VALUE 1
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
            DISPLAY SQLCODE.
           SET GRADE-LGTH UP BY 1.
       8030-HANDLE-GRADE-END.
      ****************************************************************** 
       9010-HEADER-START.
           INITIALIZE REC-F-OUTPUT.
           MOVE LINE1 TO REC-F-OUTPUT .
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE LINE2 TO REC-F-OUTPUT .
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE LINE1 TO REC-F-OUTPUT .
           WRITE REC-F-OUTPUT.           
       9010-HEADER-END.
      ****************************************************************** 
       9020-FOOTER-START.
           INITIALIZE REC-F-OUTPUT.
           MOVE LINE1 TO REC-F-OUTPUT .
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE STUDENT-LGTH TO WS-PNT-NBR.
           MOVE LINE4 TO REC-F-OUTPUT. 
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE 'NOTES'    TO VAR.
           MOVE GRADE-LGTH TO WS-PNT-NBR.
           MOVE LINE4 TO REC-F-OUTPUT. 
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE 'COURS'     TO VAR.
           MOVE COURSE-LGTH TO WS-PNT-NBR.
           MOVE LINE4 TO REC-F-OUTPUT. 
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE LINE1 TO REC-F-OUTPUT .
           WRITE REC-F-OUTPUT.
       9020-FOOTER-END.
      ****************************************************************** 
       9030-BODY-START.

           INITIALIZE REC-F-OUTPUT.
            MOVE LINE3 TO REC-F-OUTPUT .
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           INITIALIZE WS-IDX.

           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL 
                   WS-IDX  > 6
       
            MOVE G-C-LABEL(WS-IDX) TO WS-COURSE-DISPLAY(WS-IDX)
           
           END-PERFORM.

           MOVE LINE5 TO REC-F-OUTPUT.
           WRITE REC-F-OUTPUT.

      
      *---------------------------------------------------------------- 
           INITIALIZE WS-IDX
           INITIALIZE WS-IDX2
           INITIALIZE WS-IDX3
           INITIALIZE REC-F-OUTPUT.
           MOVE LINE1 TO REC-F-OUTPUT .
           WRITE REC-F-OUTPUT.
           INITIALIZE REC-F-OUTPUT.
           
     
            PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 
            7

        
            INITIALIZE NOTE 
            INITIALIZE COEFFICIENT
             INITIALIZE WS-BUFFER FullNAME

            MOVE STUDENT(WS-IDX) TO FullNAME
        
            PERFORM VARYING WS-IDX3 FROM 1 BY 1 UNTIL WS-IDX3 > 
            GRADE-LGTH
               
               IF FullNAME = G-S-FULLNAME(WS-IDX3)
               DISPLAY FullNAME
                COMPUTE NOTE = NOTE + G-GRADE(WS-IDX3) *
                 C-COEF(WS-IDX3 )
                COMPUTE COEFFICIENT = COEFFICIENT + 
                C-COEF(WS-IDX3 )
                END-IF
               
            END-PERFORM

            COMPUTE MOYENNE = NOTE / COEFFICIENT
            MOVE MOYENNE TO MOY

            ADD 1 TO WS-COUNT
       
            PERFORM VARYING WS-IDX2 FROM 1 BY 1 UNTIL WS-IDX2 = 
            GRADE-LGTH 
           
                MOVE G-GRADE(WS-IDX2 + WS-COUNT2) TO BLA-VALUE(WS-IDX2)
            END-PERFORM
           ADD 6 TO WS-COUNT2
            
            MOVE LINE6 TO REC-F-OUTPUT
            WRITE REC-F-OUTPUT
          
            END-PERFORM.
          
           
           
          
             
             
            
       9030-BODY-END.
      ****************************************************************** 
      ****************************************************************** 
      ****************************************************************** 
