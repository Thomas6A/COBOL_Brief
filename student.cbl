      ******************************************************************
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. student.
       AUTHOR. Thomas Baudrin.

      ******************************************************************
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
      *
      ******************************************************************
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      ******************************************************************
      *
      ******************************************************************
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
      *
      ******************************************************************
       DATA DIVISION.

      ******************************************************************
      *
      ******************************************************************
       FILE SECTION.
       FD  F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS
           RECORDING MODE IS V.

       01  REC-F-INPUT    PIC X(1000).

       01  REC-STUDENT.
           03 R-LASTNAME       PIC X(09).
           03 R-FIRSTNAME      PIC X(09).
           03 R-AGE            PIC 9(02).

       01  REC-COURSE.
           03 R-LABEL          PIC X(23).
           03 R-COEF           PIC x(03).
           03 R-GRADE          PIC X(05).

       FD  F-OUTPUT
           RECORD CONTAINS 250 CHARACTERS
           RECORDING MODE IS F.

       01  REC-F-OUTPUT        PIC X(250).

      ******************************************************************
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  F-INPUT-STATUS      PIC X(02) VALUE SPACE.
           88 F-INPUT-STATUS-OK    VALUE '00'.
           88 F-INPUT-STATUS-EOF   VALUE '10'.

       01  F-OUTPUT-STATUS     PIC X(02) VALUE SPACE.
           88 F-OUTPUT-STATUS-OK    VALUE '00'.
           88 F-OUTPUT-STATUS-EOF   VALUE '10'.

       01  DATA-STUDENT.
           05 STUDENT-LGHT PIC 9(03).
           05 STUDENT OCCURS 1 TO 999 TIMES  
               DEPENDING ON STUDENT-LGHT INDEXED BY S-INDEX.
               10 S-FIRSTNAME  PIC X(12).
               10 S-LASTNAME   PIC X(12).
               10 S-AGE        PIC 9(02).
               10 COURSE-LGHT  PIC 9(03).
               10 S-SOMME-POND     PIC 9(03)V99.
               10 S-SOMME-COEF     PIC 99V9.
               10 S-MOYENNE        PIC 99V99.
               10 COURSE OCCURS 999 times.
                   15 C-LABEL          PIC X(23).
                   15 C-COEF           PIC 9V9.
                   15 C-GRADE          PIC X(05).

       77  S-INDEX-SEARCH              PIC 9(03)           VALUE 0.
       77  C-INDEX                     PIC 9(03)           VALUE 0.
       77  WS-LINE                     PIC X(100).
       77  WS-LINE-LEN                 PIC 9(03).
       77  WS-LABEL-LEN                PIC 9(03).
       77  WS-STUDENT-DUPLICATE        PIC X               VALUE 'F'.
       77  WS-COURSE-DUPLICATE         PIC X               VALUE 'F'.
       77  WS-TEMP-NAME                PIC X(12).

      ******************************************************************
      *
      ******************************************************************
       PROCEDURE DIVISION.

           OPEN INPUT F-INPUT.

           SET F-INPUT-STATUS-OK TO TRUE.

           MOVE 0 TO STUDENT-LGHT.

           PERFORM UNTIL F-INPUT-STATUS-EOF

               READ F-INPUT

                   AT END
                       SET F-INPUT-STATUS-EOF TO TRUE

                   NOT AT END

                       IF REC-F-INPUT(1:2) = '01'

                           MOVE 'F' TO WS-STUDENT-DUPLICATE
                           MOVE REC-F-INPUT(3:) TO WS-LINE
                           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-LINE))
                               TO WS-LINE-LEN
                           MOVE WS-LINE(WS-LINE-LEN - 1:2) TO R-AGE
                           MOVE WS-LINE(1:7) TO R-LASTNAME
                           MOVE WS-LINE(8:WS-LINE-LEN - 9)
                               TO R-FIRSTNAME

                           PERFORM VARYING S-INDEX FROM 1 BY 1 
                               UNTIL S-INDEX > STUDENT-LGHT 
                               OR WS-STUDENT-DUPLICATE = 'T'

                               IF R-AGE = S-AGE(S-INDEX) 
                               AND R-FIRSTNAME = S-FIRSTNAME(S-INDEX)
                               AND R-LASTNAME = S-LASTNAME(S-INDEX) 

                                   MOVE 'T' TO WS-STUDENT-DUPLICATE

                               END-IF

                           END-PERFORM       

                           IF WS-STUDENT-DUPLICATE = 'F'

                               ADD 1 TO STUDENT-LGHT
                               MOVE 0 TO COURSE-LGHT(S-INDEX)
                               MOVE 0 TO C-INDEX

                               MOVE R-FIRSTNAME TO S-FIRSTNAME(S-INDEX)
                               MOVE R-LASTNAME TO S-LASTNAME(S-INDEX)
                               MOVE R-AGE TO S-AGE(S-INDEX)

                           END-IF
                           

                       ELSE IF REC-F-INPUT(1:2) = '02'
                           AND WS-STUDENT-DUPLICATE = 'F'

                           MOVE 'F' TO WS-COURSE-DUPLICATE
                           MOVE REC-F-INPUT(3:) TO WS-LINE
                           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-LINE))
                               TO WS-LINE-LEN

                           IF WS-LINE-LEN >= 29

                                 COMPUTE WS-LABEL-LEN = WS-LINE-LEN - 8
                                 MOVE WS-LINE(WS-LABEL-LEN + 4:5) 
                                   TO R-GRADE

                           ELSE      

                               COMPUTE WS-LABEL-LEN = WS-LINE-LEN - 3
                               MOVE SPACE TO R-GRADE

                           END-IF    

                           MOVE WS-LINE(1:WS-LABEL-LEN) TO R-LABEL
                           MOVE WS-LINE(WS-LABEL-LEN + 1:3) 
                               TO R-COEF

                           PERFORM VARYING C-INDEX FROM 1 BY 1 
                               UNTIL C-INDEX > COURSE-LGHT(S-INDEX) 
                               OR WS-COURSE-DUPLICATE = 'T'

                               IF FUNCTION TRIM(R-LABEL) = 
                               FUNCTION TRIM(C-LABEL(S-INDEX,C-INDEX))

                                   MOVE 'T' TO WS-COURSE-DUPLICATE

                               END-IF

                           END-PERFORM  
                              
                           IF WS-COURSE-DUPLICATE = 'F'
                               ADD 1 TO COURSE-LGHT(S-INDEX)
                               MOVE R-COEF TO C-COEF(S-INDEX,C-INDEX)
                               MOVE R-GRADE TO C-GRADE(S-INDEX,C-INDEX)
                               MOVE R-LABEL TO C-LABEL(S-INDEX,C-INDEX)

                               IF R-GRADE NOT EQUAL SPACE
                                   COMPUTE S-SOMME-POND(S-INDEX) = 
                                       S-SOMME-POND(S-INDEX) + 
                                       (FUNCTION NUMVAL(R-GRADE) * 
                                       FUNCTION NUMVAL(R-COEF))
                                   COMPUTE S-SOMME-COEF(S-INDEX) = 
                                       S-SOMME-COEF(S-INDEX) + 
                                       FUNCTION NUMVAL(R-COEF)
                                   COMPUTE S-MOYENNE(S-INDEX) 
                                       ROUNDED = S-SOMME-POND(S-INDEX)
                                       / S-SOMME-COEF(S-INDEX)  
                                END-IF       

                           END-IF    
               
                       END-IF

               END-READ

           END-PERFORM.

           CLOSE F-INPUT.

           SORT STUDENT DESCENDING S-MOYENNE.                  

           PERFORM VARYING S-INDEX FROM 1 BY 1
               UNTIL S-INDEX > STUDENT-LGHT

               DISPLAY "Nom : " S-LASTNAME(S-INDEX)
               SPACE WITH NO ADVANCING

               DISPLAY "Prénom : " S-FIRSTNAME(S-INDEX)
               SPACE WITH NO ADVANCING

               DISPLAY "Age : " S-AGE(S-INDEX)
               SPACE WITH NO ADVANCING

               DISPLAY "Moyenne : " S-MOYENNE(S-INDEX)
               DISPLAY "Matière : "

               PERFORM VARYING C-INDEX FROM 1 BY 1
                   UNTIL C-INDEX > COURSE-LGHT(S-INDEX)

                   DISPLAY "Nom de la matière : "
                       C-LABEL(S-INDEX, C-INDEX)
                   SPACE WITH NO ADVANCING

                   DISPLAY "Note : " C-GRADE(S-INDEX,C-INDEX)
                   SPACE WITH NO ADVANCING

                   DISPLAY "Coefficient : " C-COEF(S-INDEX,C-INDEX)

               END-PERFORM

           END-PERFORM.

           STOP RUN.
