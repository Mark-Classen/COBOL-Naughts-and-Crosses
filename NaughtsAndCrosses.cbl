       IDENTIFICATION DIVISION.
       PROGRAM-ID. NaughtsAndCrosses.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Board.
           05  Row1     PIC X(3) VALUE "   ".
           05  Row2     PIC X(3) VALUE "   ".
           05  Row3     PIC X(3) VALUE "   ".
       01  Player1       PIC X VALUE 'X'.
       01  Player2       PIC X VALUE 'O'.
       01  Current-Player PIC X.
       01  Internal      PIC 9.
       01  Move-Counter  PIC 9 VALUE 0.
       01  Game-Over     PIC X VALUE 'N'.
       01  Winner        PIC X VALUE ' '.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-BOARD
           MOVE Player1 TO Current-Player
           PERFORM UNTIL Game-Over = 'Y'
               PERFORM DISPLAY-BOARD
               PERFORM GET-MOVE
               IF Move-Counter < 9
                   IF Current-Player = Player1
                       PERFORM UPDATE-BOARD
                       MOVE Player2 TO Current-Player
                   ELSE
                       PERFORM UPDATE-BOARD
                       MOVE Player1 TO Current-Player
                   END-IF
               END-IF
               PERFORM CHECK-WINNER
           END-PERFORM
           PERFORM DISPLAY-BOARD
           IF Winner = ' '
               DISPLAY "Game Over! It's a draw."
           ELSE
               DISPLAY "Game Over! Winner: " Winner
           END-IF
           STOP RUN.

       INITIALIZE-BOARD.
           MOVE "   " TO Row1
           MOVE "   " TO Row2
           MOVE "   " TO Row3
           MOVE 0 TO Move-Counter.

       DISPLAY-BOARD.
           DISPLAY "Current Board:"
           DISPLAY "-----------"
           DISPLAY " " Row1(1:1) "|" Row1(2:1) "|" Row1(3:1)
           DISPLAY "-----------"
           DISPLAY " " Row2(1:1) "|" Row2(2:1) "|" Row2(3:1)
           DISPLAY "-----------"
           DISPLAY " " Row3(1:1) "|" Row3(2:1) "|" Row3(3:1)
           DISPLAY "-----------".

       GET-MOVE.
           DISPLAY "Player " Current-Player ", enter your move (1-9): "
           ACCEPT Internal.

       UPDATE-BOARD.
           EVALUATE Internal
              WHEN 1 IF Row1(1:1) = ' ' MOVE Current-Player TO Row1(1:1)
              WHEN 2 IF Row1(2:1) = ' ' MOVE Current-Player TO Row1(2:1)
              WHEN 3 IF Row1(3:1) = ' ' MOVE Current-Player TO Row1(3:1)
              WHEN 4 IF Row2(1:1) = ' ' MOVE Current-Player TO Row2(1:1)
              WHEN 5 IF Row2(2:1) = ' ' MOVE Current-Player TO Row2(2:1)
              WHEN 6 IF Row2(3:1) = ' ' MOVE Current-Player TO Row2(3:1)
              WHEN 7 IF Row3(1:1) = ' ' MOVE Current-Player TO Row3(1:1)
              WHEN 8 IF Row3(2:1) = ' ' MOVE Current-Player TO Row3(2:1)
              WHEN 9 IF Row3(3:1) = ' ' MOVE Current-Player TO Row3(3:1)
              WHEN OTHER DISPLAY "Invalid move. Try again."
           END-EVALUATE
           ADD 1 TO Move-Counter.

       CHECK-WINNER.
           PERFORM CHECK-ROWS
           PERFORM CHECK-COLUMNS
           PERFORM CHECK-DIAGONALS
           IF Winner = ' ' AND Move-Counter = 9
               MOVE 'Y' TO Game-Over
           END-IF.

       CHECK-ROWS.
           IF (Row1(1:1) = Row1(2:1) AND Row1(2:1) = Row1(3:1)
               AND Row1(1:1) NOT EQUAL TO ' ')
               MOVE Row1(1:1) TO Winner
               MOVE 'Y' TO Game-Over
           ELSE IF (Row2(1:1) = Row2(2:1) AND Row2(2:1) = Row2(3:1) AND
               Row2(1:1) NOT EQUAL TO ' ')
               MOVE Row2(1:1) TO Winner
               MOVE 'Y' TO Game-Over
           ELSE IF (Row3(1:1) = Row3(2:1) AND Row3(2:1) = Row3(3:1) AND
               Row3(1:1) NOT EQUAL TO ' ')
               MOVE Row3(1:1) TO Winner
               MOVE 'Y' TO Game-Over.

       CHECK-COLUMNS.
           IF (Row1(1:1) = Row2(1:1) AND Row2(1:1) = Row3(1:1) AND
               Row1(1:1) NOT EQUAL TO ' ')
               MOVE Row1(1:1) TO Winner
               MOVE 'Y' TO Game-Over
           ELSE IF (Row1(2:1) = Row2(2:1) AND Row2(2:1) = Row3(2:1) AND
               Row1(2:1) NOT EQUAL TO ' ')
               MOVE Row1(2:1) TO Winner
               MOVE 'Y' TO Game-Over
           ELSE IF (Row1(3:1) = Row2(3:1) AND Row2(3:1) = Row3(3:1) AND
               Row1(3:1) NOT EQUAL TO ' ')
               MOVE Row1(3:1) TO Winner
               MOVE 'Y' TO Game-Over.

       CHECK-DIAGONALS.
           IF (Row1(1:1) = Row2(2:1) AND Row2(2:1) = Row3(3:1) AND
               Row1(1:1) NOT EQUAL TO ' ')
               MOVE Row1(1:1) TO Winner
               MOVE 'Y' TO Game-Over
           ELSE IF (Row1(3:1) = Row2(2:1) AND Row2(2:1) = Row3(1:1) AND
               Row1(3:1) NOT EQUAL TO ' ')
               MOVE Row1(3:1) TO Winner
               MOVE 'Y' TO Game-Over.

       END PROGRAM NaughtsAndCrosses.
