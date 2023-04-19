       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.    CGPRG006.
       AUTHOR.        LUANA - RICARDINO.
       INSTALLATION   FATEC SAO CAETANO.
       DATE-WRITTEN.  03/09/2019.
       DATE-COMPILED. 10/04/2023.
      *--------------------------------*
       ENVIRONMENT DIVISION.
      *====================*
       CONFIGURATION SECTION.
      *---------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           CURRENCY SIGN IS "R$ " WITH PICTURE SYMBOL "$"
           .
       INPUT-OUTPUT SECTION.
      *---------------------*
       DATA DIVISION.
      *=============*
       FILE SECTION.
      *------------*
       WORKING-STORAGE SECTION.
      *-----------------------*
       01  FILLER                 PIC X(35)        VALUE
           '**** INICIO DA WORKING-STORAGE ****'.

      *-----> VARIAVEIS AUXILIARES UTILIZADA NO PROCESSAMENTO
       01  WS-AREA-AUX.
           05  WS-FIM                 PIC X(01).
           05  WS-CTLIDO              PIC 9(02).
           05  WS-MEDIA               PIC 9(02)V99.
           05  AS-DATA                PIC 9(08).
           05  AS-PORCE               PIC 99V9999.
           05  AS-ACD-SP              PIC 9(06).
           05  WS-ACD-SP              PIC Z99.999.
           05  AS-CID-SP              PIC 9(02).
           05  PORCE-SP               PIC 99V9999.
           05  WS-PORCE-SP            PIC ZZ9,99.
           05  MAIOR                  PIC 9(04).
           05  WS-MAIOR               PIC Z.ZZ9.
           05  CIDMA                  PIC 99999.
           05  CIDME                  PIC 99999.
           05  PORCEME                PIC ZZ9,99.
           05  WS-PORCEME             PIC ZZ9,99999.
           05  PORCEA                 PIC ZZ9,99.
           05  CIDMEO                 PIC 9(05).
      *-----> ENTRADA - DADOS VIA SYSIN (NO JCL DE EXECUCAO)
       01  WS-REG-SYSIN.
           05 WS-CIDADE           PIC 9(05).
           05 WS-ESTADO           PIC X(2).
           05 WS-QTD-VEICULOS     PIC 9(07).
           05 WS-BAFOMETRO        PIC X(01).
           05 WS-QTD-ACIDENTES    PIC 9(04).
           05 WS-QTD-OBITOS       PIC 9(04).
       01  WS-REG-SYSOUT.
           05 CID                 PIC 99999.
           05 FILLER              PIC X(01)        VALUE '-'.
           05 UF                  PIC XX.
           05 FILLER              PIC X(04)        VALUE SPACES.
           05 QTVEICS             PIC Z.ZZZ.ZZ9.
           05 FILLER              PIC X(04)        VALUE SPACES.
           05 BAFO                PIC X.
           05 FILLER              PIC X(04)        VALUE SPACES.
           05 QTACIDS             PIC Z.ZZ9.
           05 FILLER              PIC X(04)        VALUE SPACES.
           05 QTOBITOS            PIC Z.ZZ9.
           05 FILLER              PIC X(04)        VALUE SPACES.
           05 PACIDS              PIC ZZ9,99.
           05 FILLER              PIC X(01)        VALUE '%'.
       01  FILLER                 PIC X(35)        VALUE
           '****** FIM DA WORKING-STORAGE *****'.
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL
      *--------------------------------------------------------------*
       000-CGPRG006.

           ACCEPT AS-DATA FROM DATE
           PERFORM 010-INICIAR
           MOVE WS-QTD-ACIDENTES TO MAIOR
           MOVE WS-CIDADE        TO CIDMA
           MOVE WS-CIDADE        TO CIDME
           COMPUTE PORCEME = (WS-QTD-OBITOS / WS-QTD-ACIDENTES) * 100
           MOVE WS-CIDADE        TO CIDMEO
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 040-PROCESSAR-SP
           PERFORM 045-PROCESSAR-MAIOR
           PERFORM 047-PROCESSAR-MENOR
           PERFORM 090-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS
      *--------------------------------------------------------------*
       010-INICIAR.

           DISPLAY "LUANA - RICARDINO"
           DISPLAY "ATIVIDADE 6"
           DISPLAY "ESTATISTICAS - DATA DO CALCULO: " AS-DATA(7:2) "/"
                    AS-DATA(5:2) "/2" AS-DATA(2:3)
           DISPLAY "-------------------------------------------"
           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    LEITURA DADOS DA SYSIN
      *--------------------------------------------------------------*
       025-LER-SYSIN.

           ACCEPT WS-REG-SYSIN  FROM SYSIN

           IF WS-REG-SYSIN = ALL '9'
              MOVE   'S'     TO  WS-FIM
           ELSE
              ADD 1  TO WS-CTLIDO
           END-IF
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DA SYSIN ATE FIM DOS REGISTROS
      *--------------------------------------------------------------*
       030-PROCESSAR.

           COMPUTE AS-PORCE = (WS-QTD-ACIDENTES / WS-QTD-VEICULOS) * 100
           IF WS-ESTADO = 'SP'
              ADD WS-QTD-ACIDENTES TO AS-ACD-SP
              ADD 1 TO AS-CID-SP
              ADD AS-PORCE TO PORCE-SP
           END-IF

           IF MAIOR < WS-QTD-ACIDENTES
              MOVE WS-QTD-ACIDENTES TO MAIOR
              MOVE WS-CIDADE        TO CIDMA
           END-IF

           COMPUTE PORCEA = (WS-QTD-OBITOS / WS-QTD-ACIDENTES) * 100

           IF PORCEA < PORCEME
              MOVE PORCEA        TO PORCEME
              MOVE PORCEME       TO WS-PORCEME
              MOVE WS-CIDADE     TO CIDMEO
           END-IF

           MOVE WS-CIDADE        TO CID
           MOVE WS-ESTADO        TO UF
           MOVE WS-QTD-VEICULOS  TO QTVEICS
           MOVE WS-BAFOMETRO     TO BAFO
           MOVE WS-QTD-ACIDENTES TO QTACIDS
           MOVE WS-QTD-OBITOS    TO QTOBITOS
           MOVE AS-PORCE         TO PACIDS

           DISPLAY WS-REG-SYSOUT

           PERFORM 025-LER-SYSIN
           .
       040-PROCESSAR-SP.
           COMPUTE PORCE-SP = PORCE-SP / AS-CID-SP
           MOVE PORCE-SP    TO WS-PORCE-SP
           MOVE AS-ACD-SP   TO WS-ACD-SP
           DISPLAY "-----------------------------------------"
           DISPLAY "MEDIA DAS PORCENTAGENS DE SP............:"
                    WS-PORCE-SP "%"
           DISPLAY "QTDE. DE ACIDENTES TOTAIS EM SP.........:"
                    WS-ACD-SP
           DISPLAY "QTDE. DE CIDADES DE SP PESQUISADAS......: "
                    AS-CID-SP
           .
       045-PROCESSAR-MAIOR.
           MOVE MAIOR TO WS-MAIOR
           DISPLAY "------------------------------------------"
           DISPLAY "CIDADE COM MAIOR QUANTIDADE DE ACIDENTES: " CIDMA
           DISPLAY "QTDE. DE ACIDENTES DESTA CIDADE.........: "
                    WS-MAIOR
           DISPLAY "QTDE. TOTAL DE CIDADES PESQUISADAS......: "
                    WS-CTLIDO
           .
       047-PROCESSAR-MENOR.
           DISPLAY "------------------------------------------"
           DISPLAY "CIDADE COM MENOR PORCENTAGEM DE OBITOS..: " CIDMEO
           DISPLAY "PORCENTAGEM OBITOS/ACIDENTE DESTA CIDADE:" PORCEME
                   "%"
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS
      *--------------------------------------------------------------*
       090-TERMINAR.

           DISPLAY " *========================================*"
           DISPLAY " *   TOTAIS DE CONTROLE - CGPRG006        *"
           DISPLAY " *----------------------------------------*"
           DISPLAY " * REGISTROS LIDOS    - SYSIN  = " WS-CTLIDO
           DISPLAY " *========================================*"
           DISPLAY " *----------------------------------------*"
           DISPLAY " *      TERMINO NORMAL DO CGPRG006        *"
           DISPLAY " *----------------------------------------*"
           .
      *---------------> FIM DO PROGRAMA CGPRG006 <-------------------*
