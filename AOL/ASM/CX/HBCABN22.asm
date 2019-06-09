**********************************************************************  00000100
*                                                                    *  00000200
*  PROGRAM ID: HBCABN22                                             *   00000300
*                                                                    *  00000400
*    FUNCTION: THIS IS A CALLED MODULE. IT FORCES A USER ABEND.      *  00000500
*              IT CAN BE INVOKED IN THREE WAYS:                      *  00000600
*                   1. WITH NO PARAMETERS.                           *  00000700
*                   2. WITH 1 PARAMETER (ABEND CODE).                *  00000800
*                      THE ABSOLUTE VALUE OF THE CODE MUST BE        *  00000900
*                      BETWEEN 0000 AND 4095. A NEGATIVE CODE        *  00001000
*                      WILL SUPPRESS THE PRINTING OF THE DUMP.       *  00001100
*                   3. WITH 2 PARAMETERS (ABEND CODE AND MESSAGE).   *  00001200
*                      THE MESSAGE WILL APPEAR WITHIN THE HASP       *  00001300
*                      LOGS AND THE STEP THE PROGRAM WAS FORCED      *  00001400
*                      TO ABEND IN. MESSAGE MUST BE DEFINED IN THE   *  00001500
*                      CALLING PROGRAM AS A FIELD OF 120 CHARACTERS. *  00001600
*                                                                    *  00001700
*                                                                    *  00001800
* CONVENTIONS: R1  - ADDRESS OF AREA CONTAINING PARAMETER ADDRESSES  *  00001900
*              R13 - ADDRESS OF REGISTER SAVE AREA                   *  00002000
*              R14 - RETURN ADDRESS                                  *  00002100
*              R15 - ENTRY POINT ADDRESS                             *  00002200
*                                                                    *  00002300
*   TO INVOKE: CALL 'HBCABN22'.                                      *  00002400
*                  OR                                                *  00002500
*              CALL 'HBCABN22' USING PARM-1.                         *  00002600
*                  OR                                                *  00002700
*              CALL 'HBCABN22' USING PARM-1,PARM-2.                  *  00002800
*                                                                    *  00002900
*    COMMENTS: THIS IS AN IMPROVED VERSION OF HBCDUMP. THE           *  00003000
*              ORIGINAL VERSION WAS TOO RESTRICTIVE IN THAT          *  00003100
*              THE USER HAD NO CONTROL OVER THE ABEND CODE           *  00003200
*              OR WHETHER A DUMP WAS NECESSARY.                      *  00003300
*                                                                    *  00003400
*      AUTHOR: SOFTWARE SERVICES.                                    *  00003500
*                                                                    *  00003600
**********************************************************************  00003700
         SPACE 3                                                        00003800
HBCABN22 CSECT                                                          00003900
HBCABN22 AMODE 31                                                       00004000
HBCABN22 RMODE ANY                                                      00004100
         USING *,15                                                     00004200
         B     ABENDMOD                                                 00004300
IDLEN    DC    AL1(8)                                                   00004400
PGMID    DC    CL8'HBCABN22'      MODULE NAME                           00004500
ASMDATE  DC    CL8'&SYSDATC'                                            00004600
ASMTIME  DC    CL5'&SYSTIME'                                            00004700
         DS    0H                 FORCE ALIGNMENT                       00004800
         SPACE 3                                                        00004900
**********************************************************************  00005000
*        ENTRY LINKAGE CONVENTIONS                                   *  00005100
**********************************************************************  00005200
ABENDMOD EQU   *                                                        00005300
         STM   14,12,12(13)       SAVE REGISTERS                        00005400
         BALR  12,0               ESTABLISH ADDRESSABILITY              00005500
         USING *,12                                                     00005600
         ST    13,SAVE+4                                                00005700
         LA    13,SAVE                                                  00005800
         B     BEGIN                                                    00005900
         SPACE 3                                                        00006000
R15      EQU   15                 EQUATE REGISTERS                      00006100
R14      EQU   14                                                       00006200
R13      EQU   13                                                       00006300
R12      EQU   12                                                       00006400
R11      EQU   11                                                       00006500
R10      EQU   10                                                       00006600
R9       EQU   9                                                        00006700
R8       EQU   8                                                        00006800
R7       EQU   7                                                        00006900
R6       EQU   6                                                        00007000
R5       EQU   5                                                        00007100
R4       EQU   4                                                        00007200
R3       EQU   3                                                        00007300
R2       EQU   2                                                        00007400
R1       EQU   1                                                        00007500
R0       EQU   0                                                        00007600
         SPACE 3                                                        00007700
**********************************************************************  00007800
*        CHECK IF ANY PARAMETERS ARE PASSED                          *  00007900
*            NO - USE DEFAULT U0777                                  *  00008000
**********************************************************************  00008100
BEGIN    EQU   *                                                        00008200
         LTR   R1,R1              TEST FOR PARAMETERS                   00008300
         BNZ   WITHPARM                                                 00008400
         LH    R5,H0777           USE DEFAULT                           00008500
         B     SETABEND                                                 00008600
         SPACE 3                                                        00008700
**********************************************************************  00008800
*        PARAMETERS ARE SUPPLIED                                     *  00008900
*        CHECK SIGN OF ABEND CODE                                    *  00009000
*           SIGN POSITIVE - DUMP                                     *  00009100
*           SIGN NEGATIVE - NO DUMP                                  *  00009200
**********************************************************************  00009300
WITHPARM EQU   *                                                        00009400
         L     R5,0(R1)           LOAD ADDRESS OF PARM-1                00009500
         LH    R5,0(R5)           LOAD PARM                             00009600
         LTR   R5,R5              TEST SIGN                             00009700
         BNM   VALIDATE                                                 00009800
         LPR   R5,R5              MAKE SIGN POSITIVE                    00009900
         NI    OPTION,X'00'       SET DUMP OPTION OFF                   00010000
         SPACE 3                                                        00010100
**********************************************************************  00010200
*        CHECK ABEND CODE GREATER THAN 4095                          *  00010300
*           YES - SET ABEND CODE TO 4095                             *  00010400
**********************************************************************  00010500
VALIDATE EQU   *                                                        00010600
         CH    R5,H4095           IS CODE GREATER THAN 4095             00010700
         BNH   MSGTEST                                                  00010800
         LH    R5,H4095           SET CODE TO 4095                      00010900
         SPACE 3                                                        00011000
**********************************************************************  00011100
*        CHECK IF MESSAGE IS PASSED                                  *  00011200
*          YES - MOVE MESSAGE TO MACRO CONSTANT AREA                 *  00011300
*              - ISSUE MESSAGE                                       *  00011400
**********************************************************************  00011500
MSGTEST  EQU   *                                                        00011600
         TM    0(R1),X'80'        TEST FOR PARM-2                       00011700
         BO    SETABEND                                                 00011800
         L     R6,4(R1)           LOAD ADDRESS OF PARM-2                00011900
         MVC   ABENDINS+8(120),0(R6) MOVE MESSAGE INTO CONSTANT AREA    00012000
ABENDINS EQU   *                                                        00012100
         WTO   '                                                       -00012200
                                                                       -00012300
                        ',ROUTCDE=11                                    00012400
         SPACE 3                                                        00012500
**********************************************************************  00012600
*        ISSUE ABEND                                                 *  00012700
**********************************************************************  00012800
SETABEND EQU   *                                                        00012900
         LR    R1,R5              LOAD R1 WITH ABEND CODE               00013000
         ICM   R1,8,OPTION        INSERT DUMP BIT INTO INSTRUCTION      00013100
         SVC   13                 ISSUE ABEND                           00013200
         SPACE 3                                                        00013300
**********************************************************************  00013400
*        RETURN LINKAGE CONVENTIONS                                  *  00013500
**********************************************************************  00013600
         LM    R14,R12,12(R13)    RESTORE REGISTERS                     00013700
         BR    R14                RETURN                                00013800
         SPACE 3                                                        00013900
**********************************************************************  00014000
*        SAVE AREA AND CONSTANTS                                     *  00014100
**********************************************************************  00014200
SAVE     DS    18F                18 WORDS                              00014300
H0777    DC    H'0777'             2 BYTES                              00014400
H4095    DC    H'4095'             2 BYTES                              00014500
OPTION   DC    X'80'               1 BYTE                               00014600
         END   HBCABN22                                                 00014700
