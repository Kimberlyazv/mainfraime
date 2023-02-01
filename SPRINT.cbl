      ******************************************************************
      * Author:KIMBERLY AZEVEDO
      * Date:01/2022
      * Purpose: PRIMEIRA SPRINT CAPFLIX
      *******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SPRINT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT USUARIO ASSIGN TO
               'C:\ArqCobol\USUARIO.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY ID-USUARIO
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD USUARIO.
           COPY CADSPRINT.

       WORKING-STORAGE SECTION.
       01 WS-USUARIOS               PIC X(300) VALUE SPACES.
       01 FILLER REDEFINES WS-USUARIOS.
           03 WS-ID-USUARIO         PIC 9(02).
           03 WS-NM-USUARIO         PIC X(100).
           03 WS-EMAIL-USUARIO      PIC X(100).
           03 WS-TEL-USUARIO        PIC 9(12).
           03 WS-PASSWORD-USUARIO   PIC X(8).
       77 WS-FS                     PIC 99.
           88 FS-OK                 VALUE 0.
       77 WS-EOF                    PIC X.
           88 EOF-OK                VALUE 'S' FALSE 'N'.
       77 WS-EXIT                   PIC X.
           88 EXIT-OK               VALUE 'F' FALSE 'N'.

       COPY "CADSPRINT2".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY '*** CADASTRO DE USUARIOS ***'
           SET EXIT-OK              TO FALSE
           PERFORM P100-CADASTRA    THRU P300-FIM UNTIL EXIT-OK
           PERFORM P900-FIM

           .
       P100-CADASTRA.

           SET EOF-OK       TO FALSE
           SET FS-OK        TO TRUE

           INITIALIZE WS-NM-USUARIO

           DISPLAY 'PARA REGISTRAR UM USUARIO, INFORME: '

           DISPLAY 'Informe um numero de identificacao: '
           ACCEPT WS-ID-USUARIO

           DISPLAY 'Um nome para o usuario: '
           ACCEPT WS-NM-USUARIO

           INSPECT FUNCTION REVERSE (WS-NM-USUARIO) TALLYING WS-NOME-1
           FOR LEADING SPACES

           SUBTRACT LENGTH OF WS-NM-USUARIO FROM WS-NOME-1 GIVING
           WS-NOME-2

           INSPECT WS-NM-USUARIO TALLYING WS-NOME-3 FOR ALL
           " " AFTER INITIAL " "

           COMPUTE WS-NOME-4 = (((100 - WS-NOME-3- WS-NOME-2)* -1) + 1)

           IF WS-NOME-4 = 0
           DISPLAY "***************************************"
           DISPLAY "* ERRO - ESCREVA SEU NOME COMPLETO OU *"
           DISPLAY "*          NOME E SOBRENOME            "
           DISPLAY "***************************************"
           DISPLAY "          TENTE NOVAMENTE!"
             GO TO P100-CADASTRA
           ELSE
             GO TO P200-EMAIL.

       P200-EMAIL.

           DISPLAY 'Um email para contato'
           ACCEPT WS-EMAIL-USUARIO

           INSPECT WS-EMAIL-USUARIO TALLYING WS-ARROBA-2 FOR CHARACTERS
            BEFORE INITIAL "@"

            IF WS-ARROBA-2 <= 9
               DISPLAY "***************************************"
               DISPLAY "*ERRO - O EMAIL DEVE CONTER @ (ARROBA)*"
               DISPLAY "*ERRO - O E-MAIL DEVE CONTER NO MINIMO*"
               DISPLAY "*10 CARACTERES ANTES DO @.*************"
               DISPLAY "***************************************"
               DISPLAY "TENTE NOVAMENTE!"
               GO TO P200-EMAIL
            ELSE
               GO TO P300-EMAIL.

       P300-EMAIL.

            INSPECT WS-EMAIL-USUARIO TALLYING WS-ARROBA-3 FOR ALL
            "CAPGEMINI.COM" "BRADESCO.COM" AFTER INITIAL "@"

            IF WS-ARROBA-3 = 0
               DISPLAY "*********************************************"
               DISPLAY "*ERRO - O E-MAIL DEVE PERTENCER AO DOMINIO **"
               DISPLAY "*       CAPGGEMINI.COM OU BRADESCO.COM      *"
               DISPLAY "*********************************************"
               DISPLAY "TENTE NOVAMENTE!"
               GO TO P200-EMAIL
            ELSE
               GO TO P400-TELEFONE.


       P400-TELEFONE.

           INITIALIZE WS-PHONE-1
                      WS-TEL-USUARIO

           DISPLAY 'Um telefone para contato ((DDD)XXXXXXXX):'
           ACCEPT WS-TEL-USUARIO

            MOVE ZEROS              TO WS-PHONE-1
            INSPECT FUNCTION REVERSE(WS-TEL-USUARIO)
                    TALLYING WS-PHONE-1 FOR LEADING ' '


            IF WS-PHONE-1 <= 1
              GO TO P401-TELEFONE
            ELSE
                DISPLAY "******************************************"
                DISPLAY "* ERRO - NUMERO TELEFONICO DEVE CONTER   *"
                DISPLAY "* DDD XXXX XXXX                          *"
                DISPLAY "******************************************"
                DISPLAY "TENTE NOVAMENTE!"
            GO TO P400-TELEFONE.


       P401-TELEFONE.

            INSPECT WS-TEL-USUARIO TALLYING WS-PHONE-2 FOR ALL
            "A" "B" "C" "Ç" "D" "E" "F" "G" "H" "I" "J" "K" "L"
            "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "X" "W" "Y" "Z"
            "a" "b" "c" "ç" "d" "e" "f" "g" "h" "i" "j" "k" "l"
            "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "x" "w" "y" "z"


            IF WS-PHONE-2 >= 1
               DISPLAY "*********************************"
               DISPLAY "* ERRO - CAMPO DESTINADO APENAS *"
               DISPLAY "* A CARACTERES NUMERICOS.       *"
               DISPLAY "*********************************"
               DISPLAY "REPETIR O PROCESSO!"
               GO TO P400-TELEFONE
            ELSE
               CONTINUE.

       P500-PASSWORD.

           DISPLAY 'Digite uma senha: '
           ACCEPT WS-PASSWORD-USUARIO

           MOVE ZEROS              TO WS-PASSWORD-1
            INSPECT FUNCTION REVERSE(WS-PASSWORD-USUARIO)
                    TALLYING WS-PASSWORD-1 FOR LEADING ' '

            COMPUTE WS-PASSWORD-2 = WS-PASSWORD-1 - 9

            IF WS-PASSWORD-2 < 8
               DISPLAY "**************************"
               DISPLAY "* ERRO-SENHA DEVE CONTER *"
               DISPLAY "* NO MINIMO 8 CARACTERES *"
               DISPLAY "**************************"
               DISPLAY "REPETIR O PROCESSO!"
               GO TO P500-PASSWORD
            ELSE
               GO TO P501-PASSWORD.

       P501-PASSWORD.

            INSPECT WS-PASSWORD-USUARIO TALLYING WS-PASSWORD-3 FOR ALL
            "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"

            IF WS-PASSWORD-3 = 0
               DISPLAY "*******************************"
               DISPLAY "* ERRO-SENHA DEVE CONTER PELO *"
               DISPLAY "* MENOS 1 CARACTERE NUMERICO. *"
               DISPLAY "*******************************"
               DISPLAY "REPETIR O PROCESSO!"
               GO TO P500-PASSWORD
            ELSE
               GO TO P502-PASSWORD.

       P502-PASSWORD.

            INSPECT WS-PASSWORD-USUARIO TALLYING WS-PASSWORD-4 FOR ALL
            "A" "B" "C" "Ç" "D" "E" "F" "G" "H" "I" "J" "K" "L"
            "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "X" "W" "Y" "Z"

            IF WS-PASSWORD-4 = 0
               DISPLAY "*******************************"
               DISPLAY "* ERRO-SENHA DEVE CONTER PELO *"
               DISPLAY "* MENOS 1 LETRA MAIUSCULA.    *"
               DISPLAY "*******************************"
               DISPLAY "REPETIR O PROCESSO!"
               GO TO P500-PASSWORD
            ELSE
               GO TO P503-PASSWORD.

       P503-PASSWORD.

            INSPECT WS-PASSWORD-USUARIO TALLYING WS-PASSWORD-5 FOR ALL
            "a" "b" "c" "ç" "d" "e" "f" "g" "h" "i" "j" "k" "l"
            "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "x" "w" "y" "z"

            IF WS-PASSWORD-5 = 0
               DISPLAY "*******************************"
               DISPLAY "* ERRO-SENHA DEVE CONTER PELO *"
               DISPLAY "* MENOS 1 LETRA MINUSCULA.    *"
               DISPLAY "*******************************"
               DISPLAY "REPETIR O PROCESSO!"
               GO TO P500-PASSWORD
            ELSE
               GO TO P504-PASSWORD.

       P504-PASSWORD.

            INSPECT WS-PASSWORD-USUARIO TALLYING WS-PASSWORD-6 FOR ALL
            "+" "-" "[" "]" "*" "~" "_" "@" "#" ":" "?"

            IF WS-PASSWORD-6 = 0
               DISPLAY "**************************************"
               DISPLAY "* ERRO - A SENHA DEVE CONTER PELO    *"
               DISPLAY "* MENOS 1 CARACTERE ESPECIAL.        *"
               DISPLAY "* ( +, -, [, ], *, ~, _, @, #, :, ?) *"
               DISPLAY "**************************************"
               DISPLAY "REPETIR O PROCESSO!"
               GO TO P500-PASSWORD
            ELSE
               CONTINUE


*****************************************************************************************
            DISPLAY "******** RESULTADO DO PROCESSAMENTO ********"
             DISPLAY "NOME  USUARIO : " WS-NM-USUARIO
             DISPLAY "EMAIL USUARIO : " WS-EMAIL-USUARIO
             DISPLAY "SENHA USUARIO : " WS-PASSWORD-USUARIO
             DISPLAY "TELEFONE USUARIO : " "(" WS-DDD ")"
                                           WS-PREFIXO
                                           "-"
                                           WS-SUFIXO

***********************************************************************************************************
           OPEN I-O USUARIO

           IF WS-FS EQUAL 35 THEN
               OPEN OUTPUT USUARIO
           END-IF

           IF FS-OK THEN
               MOVE WS-NM-USUARIO         TO NM-USUARIO
               MOVE WS-EMAIL-USUARIO      TO EMAIL-USUARIO
               MOVE WS-TEL-USUARIO        TO TEL-USUARIO
               MOVE WS-PASSWORD-USUARIO   TO PASSWORD-USUARIO

               WRITE REG-USUARIOS
                     INVALID KEY
                       DISPLAY 'CONTATO JA CADASTRADO!'
                     NOT INVALID KEY
                       DISPLAY 'Contato gravado com sucesso!'
               END-WRITE
           ELSE
               DISPLAY 'ERRO AO ABRIR ARQUIVO DE CONTATOS'
               DISPLAY 'FILE STATUS: ' WS-FS
           END-IF

           CLOSE USUARIO

           DISPLAY
               'TECLE: '
               '<ENTER> para continuar, ou <F> para finalizar.'
           ACCEPT WS-EXIT
           .
       P300-FIM.
       P900-FIM.
            STOP RUN.
       END PROGRAM SPRINT.
