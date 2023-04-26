       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.    CGPRG007.
       AUTHOR.        LUANA - RICARDINO.
       INSTALLATION   FATEC SAO CAETANO.
       DATE-WRITTEN.  20/10/2020.
       DATE-COMPILED. 25/04/2023.
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
           05  AS-HORA                PIC 99.99.9999.
           05  AS-PORCE               PIC 99V9999.
           05  AS-ACD-SP              PIC 9(06).
           05  WS-ACD-SP              PIC Z99.999.
           05  AS-CID-SP              PIC 9(02).
           05  AS-PORCE-SP            PIC 99V9999.
           05  WS-PORCE-SP            PIC ZZ9,99.
           05  AS-ACD-RJ              PIC 9(06).
           05  WS-ACD-RJ              PIC Z99.999.
           05  AS-CID-RJ              PIC 9(02).
           05  AS-PORCE-RJ            PIC 99V9999.
           05  WS-PORCE-RJ            PIC ZZ9,99.
           05  AS-ACD-MG              PIC 9(06).
           05  WS-ACD-MG              PIC Z99.999.
           05  AS-CID-MG              PIC 9(02).
           05  AS-PORCE-MG            PIC 99V9999.
           05  WS-PORCE-MG            PIC ZZ9,99.
           05  ACIDMA                 PIC 9(04).
           05  ACIDME                 PIC 9(04).
           05  OBITOME                PIC 9(04).
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
       000-CGPRG007.

           ACCEPT AS-DATA FROM DATE
           ACCEPT AS-HORA FROM TIME
           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 040-PROCESSAR-SP
           PERFORM 041-PROCESSAR-RJ
           PERFORM 042-PROCESSAR-MG
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
           DISPLAY "ATIVIDADE 7"
           DISPLAY "ESTATISTICAS - DATA: " AS-DATA(7:2) "/"
                    AS-DATA(5:2) "/2" AS-DATA(2:3) " - HORA: "
                    AS-HORA
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
              ADD AS-PORCE TO AS-PORCE-SP
           END-IF

           IF WS-ESTADO = 'RJ'
              ADD WS-QTD-ACIDENTES TO AS-ACD-RJ
              ADD 1 TO AS-CID-RJ
              ADD AS-PORCE TO AS-PORCE-RJ
           END-IF

           IF WS-ESTADO = 'MG'
              ADD WS-QTD-ACIDENTES TO AS-ACD-MG
              ADD 1 TO AS-CID-MG
              ADD AS-PORCE TO AS-PORCE-MG
           END-IF

           COMPUTE PORCEA = (WS-QTD-OBITOS / WS-QTD-ACIDENTES) * 100

           IF WS-CTLIDO = 1
              MOVE WS-QTD-ACIDENTES TO ACIDMA
              MOVE WS-CIDADE        TO CIDMA
              MOVE WS-CIDADE        TO CIDME
              MOVE WS-QTD-OBITOS    TO OBITOME
              MOVE PORCEA           TO PORCEME
           END-IF

           IF ACIDMA < WS-QTD-ACIDENTES
              MOVE WS-QTD-ACIDENTES TO ACIDMA
              MOVE WS-CIDADE        TO CIDMA
           END-IF

           IF PORCEA < PORCEME
              MOVE PORCEA           TO PORCEME
              MOVE WS-QTD-OBITOS    TO OBITOME
              MOVE WS-QTD-ACIDENTES TO ACIDME
              MOVE WS-CIDADE        TO CIDME
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
      *--------------------------------------------------------------*
      *    PROCESSAR CIDADES
      *--------------------------------------------------------------*
       040-PROCESSAR-SP.
           COMPUTE AS-PORCE-SP = AS-PORCE-SP / AS-CID-SP
           MOVE AS-PORCE-SP    TO WS-PORCE-SP
           MOVE AS-ACD-SP   TO WS-ACD-SP
           DISPLAY "-----------------------------------------"
           DISPLAY "MEDIA DAS PORCENTAGENS DE SP............:"
                    WS-PORCE-SP "%"
           DISPLAY "QTDE. DE ACIDENTES TOTAIS EM SP.........:"
                    WS-ACD-SP
           DISPLAY "QTDE. DE CIDADES DE SP PESQUISADAS......: "
                    AS-CID-SP
           .
       041-PROCESSAR-RJ.
           COMPUTE AS-PORCE-RJ = AS-PORCE-RJ / AS-CID-RJ
           MOVE AS-PORCE-RJ    TO WS-PORCE-RJ
           MOVE AS-ACD-RJ   TO WS-ACD-RJ
           DISPLAY "-----------------------------------------"
           DISPLAY "MEDIA DAS PORCENTAGENS DE RJ............:"
                    WS-PORCE-RJ "%"
           DISPLAY "QTDE. DE ACIDENTES TOTAIS EM RJ.........:"
                    WS-ACD-RJ
           DISPLAY "QTDE. DE CIDADES DE RJ PESQUISADAS......: "
                    AS-CID-RJ
           .
       042-PROCESSAR-MG.
           COMPUTE AS-PORCE-MG = AS-PORCE-MG / AS-CID-MG
           MOVE AS-PORCE-MG    TO WS-PORCE-MG
           MOVE AS-ACD-MG   TO WS-ACD-MG
           DISPLAY "-----------------------------------------"
           DISPLAY "MEDIA DAS PORCENTAGENS DE MG............:"
                    WS-PORCE-MG "%"
           DISPLAY "QTDE. DE ACIDENTES TOTAIS EM MG.........:"
                    WS-ACD-MG
           DISPLAY "QTDE. DE CIDADES DE MG PESQUISADAS......: "
                    AS-CID-MG
           .
       045-PROCESSAR-MAIOR.
           MOVE ACIDMA TO WS-MAIOR
           DISPLAY "------------------------------------------"
           DISPLAY "CIDADE COM MAIOR QUANTIDADE DE ACIDENTES: " CIDMA
           DISPLAY "QTDE. DE ACIDENTES DESTA CIDADE.........: "
                    WS-MAIOR
           DISPLAY "QTDE. TOTAL DE CIDADES PESQUISADAS......: "
                    WS-CTLIDO
           .
       047-PROCESSAR-MENOR.
           COMPUTE PORCEME = (OBITOME / ACIDME) * 100
           DISPLAY "------------------------------------------"
           DISPLAY "CIDADE COM MENOR PORCENTAGEM DE OBITOS..:  " CIDME
           DISPLAY "PORCENTAGEM OBITOS/ACIDENTE DESTA CIDADE:" PORCEME
                   "%"
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS
      *--------------------------------------------------------------*
       090-TERMINAR.

           DISPLAY " *========================================*"
           DISPLAY " *   TOTAIS DE CONTROLE - CGPRG007        *"
           DISPLAY " *----------------------------------------*"
           DISPLAY " * REGISTROS LIDOS    - SYSIN  = " WS-CTLIDO
           DISPLAY " *========================================*"
           DISPLAY " *----------------------------------------*"
           DISPLAY " *      TERMINO NORMAL DO CGPRG007        *"
           DISPLAY " *----------------------------------------*"
           .
      *---------------> FIM DO PROGRAMA CGPRG007 <-------------------*
