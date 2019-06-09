*********************************************************************   00001000
*                                                                   *   00002000
*     Department : Systems Programming                              *   00002100
*     Author     : Marvin Yuen                                      *   00002200
*     Date       : Feb - 1992                                       *   00002300
*     Denise testing v8 v1                                          *   00002320
*     Date       : Feb - 1992                                       *   00002300
*     Denise testing v8 v3                                          *   00002320
*     Date       : Feb - 1992                                       *   00002300
*     Denise testing v8 v3                                          *   00002320
*  This program converts input fields to their requested format.    *   00002400
*   Type        Descriptions                                        *   00002500
*     0         Change packed decimal number to packed-no-sign form *   00002600
*                  eg. X'12345F' to X'012345'                       *   00002700
*     1         Change EBCDIC to ASCII form (max 256 bytes)         *   00002800
*     2         Convert Bitmap to byte representation               *   00002900
*                  eg.  C'10100001' to X'A1'                        *   00003000
*     3         Change ASCII to EBCDIC form (max 256 bytes)         *   00003100
*     4         PC  to host binary form ( 2 or 4 btyes)             *   00003200
*               or host binary to PC form ( 2 or 4 btyes)           *   00003300
*                  eg.  X'12345678' to X'78563412'  (4 bytes)       *   00003400
*                       X'1234'     to X'3412'      (2 bytes)       *   00003500
*     5         Convert byte representation to bit map (char. form) *   00003600
*                  eg.  X'A1' to C'10100001'                        *   00003700
*     6         Change packed-no-sign form to packed decimal number *   00003800
*                  EG. X'012345' TO X'12345F'                       *   00003900
*                                                                   *   00004000
*   Calling format and parms:                                       *   00004100
*                                                                   *   00004200
*    CALL 'NEWASM' using                                            *   00004300
*    TYPE1,IN-REC-LGTH1,OUT-REC-LGTH1,IN-FIELD1,OUT-FIELD1          *   00004400
*    TYPE2,IN-REC-LGTH2,OUT-REC-LGTH2,IN-FIELD2,OUT-FIELD2          *   00004500
*      .      .     .      .      .        .         .              *   00004600
*      .      .     .      .      .        .         .              *   00004700
*      .      .     .      .      .        .         .              *   00004800
*    TYPEn,IN-REC-LGTHn,OUT-REC-LGTHn,IN-FIELDn,OUT-FIELDn          *   00004900
*                                                                   *   00005000
*    Parameter specifications:                                      *   00006000
*    name            Length      Type           Cobol form          *   00007000
*    TYPE              1          Alphanumeric    PIC X             *   00008000
*    IN-REC-LGTH       2          Binary          PIC 999 comp      *   00009000
*    OUT-REC-LGTH      2          Binary          PIC 999 comp      *   00009100
*                                                                   *   00009200
*                                                                   *   00009300
*   History:                                                        *   00009400
*   April 8,92    PCTABLE changed to support french characters      *   00009500
*                  ....Marvin Yuen                                  *   00009600
*   Aug. 11,92    Add option 3,4,5. ........Marvin Yuen             *   00009700
*   Aug. 14,92    Add option 6      ........Marvin Yuen             *   00009800
*********************************************************************   00009900
NEWASM  CSECT                                                           00010000
NEWASM  amode 31                                                        00010100
NEWASM  rmode any                                                       00010200
         PRINT OFF                                                      00010000
R0       EQU   0                                                        00010100
R1       EQU   1                                                        00010200
R2       EQU   2                                                        00010300
R3       EQU   3                                                        00010400
R4       EQU   4                                                        00010500
R5       EQU   5                                                        00010600
R6       EQU   6                                                        00010700
R7       EQU   7                                                        00010800
R8       EQU   8                                                        00010900
R9       EQU   9                                                        00011000
R10      EQU   10                                                       00012000
R11      EQU   11                                                       00013000
R12      EQU   12                                                       00014000
R13      EQU   13                                                       00015000
R14      EQU   14                                                       00016000
R15      EQU   15                                                       00017000
         PRINT ON                                                       00018000
NEWASM  CSECT                                                           00019000
         STM    R14,R12,12(R13)                                         00020000
         USING  NEWASM,R12                                              00030000
         LR     R12,R15                                                 00040000
         B      *+20                                                    00050000
         DC     CL8'NEWASM'                                             00060000
         DC     CL8'&SYSDATE'                                           00070000
         LR     R2,R1              R2 HAS THE PARM ADDRESS              00080000
         EJECT                                                          00090000
LOOP000  DS     0H                                                      00100000
         MVC    TYPEADDR(20),0(R2)                                      00110000
         TM     TYPEADDR,B'10000000'    **************************      00120000
         BO     CONVEXIT                *  CHECK ALL PARMS       *      00130000
         TM     INLGADDR,B'10000000'    *  ARE THERE             *      00140000
         BO     CONVEXIT                *   see if we have       *      00150000
         TM     OULGADDR,B'10000000'    *   a complete list      *      00160000
         BO     CONVEXIT                *   of parms             *      00170000
         TM     INRDADDR,B'10000000'    *                        *      00180000
         BO     CONVEXIT                **************************      00190000
         L      R3,TYPEADDR                                             00200000
         MVC    REQSTYPE,0(R3)                                          00210000
         LA    R3,ADDRMAP                                               00220000
*                                                                       00230000
CHKT0000 DS     0H                                                      00240000
         CLC    REQSTYPE,0(R3)                                          00250000
         BE     CHKT0020                                                00260000
         LA     R3,8(R3)                                                00270000
         CLI    0(R3),X'FF'                                             00280000
         BE     CHKTOK00                                                00290000
         B      CHKT0000                                                00300000
*                                                                       00310000
CHKT0020 DS     0H                                                      00320000
         L      R3,4(R3)                                                00330000
         BALR   R6,R3                                                   00340000
         B      CHKTOK00                                                00350000
*                                                                       00360000
CHKTOK00 DS     0H                                                      00370000
         TM     OURDADDR,B'10000000'                                    00380000
         BO     CONVEXIT                                                00390000
         LA     R2,20(R2)                                               00400000
         B      LOOP000                                                 00410000
CONVEXIT DS     0H                                                      00420000
         LM     R14,R12,12(R13)                                         00430000
         XR     R15,R15                                                 00440000
         BR     R14                                                     00450000
         EJECT                                                          00460000
******************************************************************      00470000
*                                                                       00480000
*    CONVERT PACKED DECIMAL TO PACKED-NO-SIGN DECIMAL FORMAT            00490000
*           (TYPE 0)                                                    00500000
******************************************************************      00510000
*                                                                       00520000
CONVPNS  DS     0H                                                      00530000
         ST     R6,SAVEREG                                              00540000
         L      R4,INLGADDR                                             00550000
         LH     R4,0(R4)                                                00560000
         LR     R6,R4          R6 KEEPS THE IN LENGTH                   00570000
         MVC    DOUBLE,NULLS                                            00580000
         L      R5,=F'8'                                                00590000
         SR     R5,R4                                                   00600000
         LA     R5,DOUBLE(R5)                                           00610000
         BCTR   R4,0                                                    00620000
         N      R4,=B'00000000000000000000000000000111'                 00630000
         L      R3,INRDADDR                                             00640000
         EX     R4,MOVECHAR                                             00650000
*                                                                       00660000
         LM     R4,R5,DOUBLE                                            00670000
         SRDA   R4,4(0)                                                 00680000
         N      R4,=B'00001111111111111111111111111111'                 00690000
         STM    R4,R5,DOUBLE                                            00700000
*                                                                       00710000
         L      R4,OULGADDR               PICK UP OUT RECORD LENGTH     00720000
         LH     R4,0(R4)          LOAD OUTPUT LENGTH                    00730000
         LR     R7,R4                                                   00740000
         BCTR   R7,0                                                    00750000
         L      R5,OURDADDR               PICK UP OUT RECORD ADDRESS    00760000
         EX     R7,NULLCHAR               CLEAR OUTPUT AREA             00770000
*                                                                       00780000
         L      R3,=F'8'       *                                        00790000
         SR     R3,R4          *  POSITION 'DOUBLE' START LOCATION      00800000
         LA     R3,DOUBLE(R3)  **********************************       00810000
         EX     R7,MOVECHAR                                             00820000
*                                                                       00830000
CONVPNSX L      R6,SAVEREG                                              00840000
         BR     R6                                                      00850000
         EJECT                                                          00860000
******************************************************************      00870000
*                                                                       00880000
*    CONVERT EBCDIC TO ASCII                                            00890000
*      (TYPE 1)                                                         00900000
******************************************************************      00910000
CONVASC  DS     0H                                                      00920000
         ST     R6,SAVEREG                                              00930000
         L      R3,INRDADDR                                             00940000
         L      R4,INLGADDR                                             00950000
         L      R5,OURDADDR                                             00960000
         LH     R4,0(R4)                                                00970000
         BCTR   R4,0                                                    00980000
         N      R4,=X'000000FF'                                         00990000
         EX     R4,MOVECHAR                                             01000000
         EX     R4,TRANSLAT                                             01010000
CONVASCX L      R6,SAVEREG                                              01020000
         BR     R6                                                      01030000
         EJECT                                                          01040000
******************************************************************      01050000
*                                                                       01060000
*    CONVERT CHAR BIT MAP TO BYTE REPRESENTATION                        01070000
*      (TYPE 2)                                                         01080000
******************************************************************      01090000
CONVBYT  DS     0H                                                      01100000
         ST     R6,SAVEREG                                              01110000
         NI     ONEBYTE,X'00'                                           01120000
         L      R3,INRDADDR                                             01130000
         MVC    DOUBLE(8),0(R3)                                         01140000
         TR     DOUBLE(8),BYTABLE                                       01150000
         NC     DOUBLE(8),=X'8040201008040201'                          01160000
         OC     ONEBYTE,DOUBLE                                          01170000
         OC     ONEBYTE,DOUBLE+1                                        01180000
         OC     ONEBYTE,DOUBLE+2                                        01190000
         OC     ONEBYTE,DOUBLE+3                                        01200000
         OC     ONEBYTE,DOUBLE+4                                        01210000
         OC     ONEBYTE,DOUBLE+5                                        01220000
         OC     ONEBYTE,DOUBLE+6                                        01230000
         OC     ONEBYTE,DOUBLE+7                                        01231000
         L      R3,OURDADDR                                             01232000
         MVC    0(1,R3),ONEBYTE                                         01233000
CONVBYTX L      R6,SAVEREG                                              01234000
         BR     R6                                                      01235000
         EJECT                                                          01236000
******************************************************************      01237000
*                                                                       01238000
*    CONVERT ASCII  TO EBCDIC                                           01239000
*      (TYPE 3)                                                         01240000
******************************************************************      01250000
CONVEBC  DS     0H                                                      01251000
         ST     R6,SAVEREG                                              01252000
         L      R3,INRDADDR                                             01253000
         L      R4,INLGADDR                                             01254000
         L      R5,OURDADDR                                             01255000
         LH     R4,0(R4)                                                01256000
         BCTR   R4,0                                                    01257000
         N      R4,=X'000000FF'                                         01258000
         EX     R4,MOVECHAR                                             01259000
         EX     R4,TRANSEBC                                             01259100
CONVEBCX L      R6,SAVEREG                                              01259200
         BR     R6                                                      01259300
         EJECT                                                          01259400
******************************************************************      01259500
*                                                                       01259600
*    Intel  to  EBCDIC binary form                                      01259700
*      (TYPE 4)                                                         01259800
*                                                                       01259900
******************************************************************      01260000
CONVITL  DS     0H                                                      01260100
         ST     R6,SAVEREG                                              01260200
         L      R3,INRDADDR                                             01260300
         L      R4,INLGADDR                                             01260400
         L      R5,OURDADDR                                             01260500
         LH     R4,0(R4)                                                01260600
         BCTR   R4,0                                                    01260700
         N      R4,=X'000000FF'                                         01260800
         EX     R4,MOVECHAR                                             01260900
         C      R4,=F'3'                                                01261000
         BE     CONVI001      go and do 4 btyes                         01261100
         ICM    R6,3,0(R5)    only 2 bytes here                         01261200
         STCM   R6,2,1(R5)                                              01261300
         STCM   R6,1,0(R5)                                              01261400
         B      CONVITLX                                                01261500
CONVI001 DS     0H                                                      01261600
         ICM    R6,15,0(R5)                                             01261700
         STCM   R6,8,3(R5)                                              01261800
         STCM   R6,4,2(R5)                                              01261900
         STCM   R6,2,1(R5)                                              01262000
         STCM   R6,1,0(R5)                                              01262100
CONVITLX L      R6,SAVEREG                                              01262200
         BR     R6                                                      01262300
         EJECT                                                          01262400
******************************************************************      01262500
*                                                                       01262600
*    CONVERT BYTE REPRESENTATION TO CHAR BIT MAP                        01262700
*      (TYPE 5)                                                         01262800
******************************************************************      01262900
CONVBIT  DS     0H                                                      01263000
         ST     R6,SAVEREG                                              01263100
         L      R3,INRDADDR                                             01263200
         MVC    ONEBYTE,0(R3)                                           01263300
         MVC    DOUBLE(8),=X'8040201008040201'                          01263400
         NC     DOUBLE(1),ONEBYTE                                       01263500
         NC     DOUBLE+1(1),ONEBYTE                                     01263600
         NC     DOUBLE+2(1),ONEBYTE                                     01263700
         NC     DOUBLE+3(1),ONEBYTE                                     01263800
         NC     DOUBLE+4(1),ONEBYTE                                     01263900
         NC     DOUBLE+5(1),ONEBYTE                                     01264000
         NC     DOUBLE+6(1),ONEBYTE                                     01264100
         NC     DOUBLE+7(1),ONEBYTE                                     01264200
         TR     DOUBLE(8),BITABLE                                       01264300
         L      R3,OURDADDR                                             01264400
         MVC    0(8,R3),DOUBLE                                          01264500
CONVBITX L      R6,SAVEREG                                              01264600
         BR     R6                                                      01264700
         EJECT                                                          01264800
******************************************************************      01264900
*                                                                       01265000
*    CONVERT PACKED-NO-SIGN DECIMAL TO PACKED DECIMAL FORMAT            01265100
*           (TYPE 6)                                                    01265200
******************************************************************      01265300
*                                                                       01265400
CONVPDN  DS     0H                                                      01265500
         ST     R6,SAVEREG                                              01265600
         L      R4,INLGADDR                                             01265700
         LH     R4,0(R4)                                                01265800
         LR     R6,R4          R6 KEEPS THE IN LENGTH                   01265900
         MVC    DOUBLE,=X'000000000000000F'                             01266000
         MVC    DOUBLE1,NULLS                                           01266100
*                                                                       01266200
         L      R5,=F'8'                                                01266300
         SR     R5,R4                                                   01266400
         LA     R5,DOUBLE1(R5)                                          01266500
         BCTR   R4,0                                                    01266600
         N      R4,=B'00000000000000000000000000000111'                 01266700
         L      R3,INRDADDR                                             01266800
         EX     R4,MOVECHAR                                             01266900
         MVO    DOUBLE-1(9),DOUBLE1(8)                                  01267000
*                                                                       01267100
*                                                                       01267200
         L      R4,OULGADDR               PICK UP OUT RECORD LENGTH     01267300
         LH     R4,0(R4)          LOAD OUTPUT LENGTH                    01267400
         LR     R7,R4                                                   01267500
         BCTR   R7,0                                                    01267600
         L      R5,OURDADDR               PICK UP OUT RECORD ADDRESS    01267700
         EX     R7,NULLCHAR               CLEAR OUTPUT AREA             01267800
*                                                                       01267900
         L      R3,=F'8'       *                                        01268000
         SR     R3,R4          *  POSITION 'DOUBLE' START LOCATION      01269000
         LA     R3,DOUBLE(R3)  **********************************       01270000
         EX     R7,MOVECHAR                                             01270100
CONVPDNX L      R6,SAVEREG                                              01270200
         BR     R6                                                      01270300
         EJECT                                                          01270400
***********************************************************             01270500
*   THESE INSTRUCTIONS ARE USED IN 'EX'  ABOVE                          01270600
*                                                                       01270700
***********************************************************             01270800
ZAPDBLEI ZAP    DOUBLE(8),0(0,R3)                                       01270900
MOVECHAR MVC    0(0,R5),0(R3)                                           01271000
MOVEPDN  MVO    0(0,R5),0(R3)                                           01271100
NULLCHAR MVC    0(0,R5),NULLS                                           01271200
TRANSEBC TR     0(0,R5),HSTABLE                                         01271300
TRANSLAT TR     0(0,R5),PCTABLE                                         01271400
XORCHAR  XC     0(0,R5),0(R5)                                           01271500
         EJECT                                                          01271600
PCTABLE  DC     X'20202020202020202020202020202020'   00-0F             01271700
         DC     X'20202020202020202020202020202020'   10-1F             01271800
         DC     X'20202020202020202020202020202020'   20-2F             01271900
         DC     X'20202020202020202020202020202020'   30-3F             01272000
         DC     X'20202020202020202020202020202020'   40-4F             01273000
         DC     X'20202020202020202020202020202020'   50-5F             01274000
         DC     X'20202020202020202020202020202020'   60-6F             01275000
         DC     X'20202020202020202020202020202020'   70-7F             01276000
         DC     X'20202020202020202020202020202020'   80-8F             01277000
         DC     X'20202020202020202020202020202020'   90-9F             01278000
         DC     X'20202020202020202020202020202020'   A0-AF             01279000
         DC     X'20202020202020202020202020202020'   B0-BF             01280000
         DC     X'20202020202020202020202020202020'   C0-CF             01290000
         DC     X'20202020202020202020202020202020'   D0-DF             01300000
         DC     X'20202020202020202020202020202020'   E0-EF             01310000
         DC     X'20202020202020202020202020202020'   F0-FF             01320000
         ORG    PCTABLE+X'40'              40-4F                        01330000
         DC     X'20918320852020E187209B2E3C282B7C'                     01340000
         ORG    PCTABLE+X'50'              50-5F                        01350000
         DC     X'268288898A208C8B978D21242A293B5E'                     01360000
         ORG    PCTABLE+X'60'              60-6F                        01370000
         DC     X'2D2F83D685E5208580827C2C255F3E3F'                     01380000
         ORG    PCTABLE+X'70'              70-7F                        01390000
         DC     X'8D9088898A8E8C8B99603A2340273D22'                     01400000
         ORG    PCTABLE+X'80'              80-8F                        01410000
         DC     X'20616263646566676869202020202020'                     01420000
         ORG    PCTABLE+X'90'              90-9F                        01430000
         DC     X'206A6B6C6D6E6F707172202020202020'                     01440000
         ORG    PCTABLE+X'A0'              A0-AF                        01450000
         DC     X'207E737475767778797A202020202020'                     01460000
         ORG    PCTABLE+X'B0'              B0-BF                        01470000
         DC     X'20202020202202020202020020202020'                     01480000
         ORG    PCTABLE+X'C0'              C0-CF                        01490000
         DC     X'7B414243444546474849309395202020'                     01500000
         ORG    PCTABLE+X'D0'              D0-DF                        01510000
         DC     X'7D4A4B4C4D4E4F505152A29681972020'                     01520000
         ORG    PCTABLE+X'E0'              E0-EF                        01530000
         DC     X'5C20535455565758595A309398202020'                     01540000
         ORG    PCTABLE+X'F0'              F0-FF                        01550000
         DC     X'3031323334353637383991969A972020'                     01560000
         LTORG                                                          01570000
         DS     0F                                                      01580000
         EJECT                                                          01590000
HSTABLE  DC     X'40404040404040404040404040404040'                     01600000
         DC     X'40404040404040404040404040404040'                     01610000
         DC     X'40404040404040404040404040404040'                     01620000
         DC     X'40404040404040404040404040404040'                     01630000
         DC     X'40404040404040404040404040404040'                     01640000
         DC     X'40404040404040404040404040404040'                     01650000
         DC     X'40404040404040404040404040404040'                     01660000
         DC     X'40404040404040404040404040404040'                     01670000
         DC     X'40404040404040404040404040404040'                     01680000
         DC     X'40404040404040404040404040404040'                     01690000
         DC     X'40404040404040404040404040404040'                     01700000
         DC     X'40404040404040404040404040404040'                     01710000
         DC     X'40404040404040404040404040404040'                     01720000
         DC     X'40404040404040404040404040404040'                     01730000
         DC     X'40404040404040404040404040404040'                     01740000
         DC     X'40404040404040404040404040404040'                     01750000
         ORG    HSTABLE+X'21'                                           01760000
         DC     C'!'                                                    01770000
         ORG    HSTABLE+X'22'                                           01780000
         DC     C'"'                                                    01790000
         ORG    HSTABLE+X'23'                                           01800000
         DC     C'#'                                                    01810000
         ORG    HSTABLE+X'24'                                           01820000
         DC     C'$'                                                    01830000
         ORG    HSTABLE+X'25'                                           01840000
         DC     C'%'                                                    01850000
         ORG    HSTABLE+X'26'                                           01860000
         DC     C'&&'                                                   01870000
         ORG    HSTABLE+X'27'                                           01880000
         DC     C''''                                                   01890000
         ORG    HSTABLE+X'28'                                           01900000
         DC     C'('                                                    01910000
         ORG    HSTABLE+X'29'                                           01920000
         DC     C')'                                                    01930000
         ORG    HSTABLE+X'2A'                                           01940000
         DC     C'*'                                                    01950000
         ORG    HSTABLE+X'2B'                                           01960000
         DC     C'+'                                                    01970000
         ORG    HSTABLE+X'2C'                                           01980000
         DC     C','                                                    01990000
         ORG    HSTABLE+X'2D'                                           02000000
         DC     C'-'                                                    02010000
         ORG    HSTABLE+X'2E'                                           02020000
         DC     C'.'                                                    02030000
         ORG    HSTABLE+X'2F'                                           02040000
         DC     C'/'                                                    02050000
         ORG    HSTABLE+X'30'                                           02060000
         DC     C'0123456789'                                           02070000
         ORG    HSTABLE+X'3A'                                           02080000
         DC     C':'                                                    02090000
         ORG    HSTABLE+X'3B'                                           02100000
         DC     C';'                                                    02110000
         ORG    HSTABLE+X'3C'                                           02120000
         DC     C'<'                                                    02130000
         ORG    HSTABLE+X'3D'                                           02140000
         DC     C'='                                                    02150000
         ORG    HSTABLE+X'3E'                                           02160000
         DC     C'>'                                                    02170000
         ORG    HSTABLE+X'3F'                                           02180000
         DC     C'?'                                                    02190000
         ORG    HSTABLE+X'40'                                           02200000
         DC     C'@'                                                    02201000
         ORG    HSTABLE+X'41'                                           02202000
         DC     C'ABCDEFGHI'                                            02203000
         ORG    HSTABLE+X'4A'                                           02204000
         DC     C'JKLMNOPQRS'                                           02205000
         ORG    HSTABLE+X'54'                                           02205100
         DC     C'TUVWXYZ'                                              02205200
         ORG    HSTABLE+X'5B'                                           02205300
         DC     X'AD'                                                   02205400
         ORG    HSTABLE+X'5C'                                           02205500
         DC     C'\'                                                    02205600
         ORG    HSTABLE+X'5D'                                           02205700
         DC     X'BD'                                                   02205800
         ORG    HSTABLE+X'5F'                                           02205900
         DC     C'_'                                                    02206000
         ORG    HSTABLE+X'60'                                           02206100
         DC     C''''                                                   02206200
         ORG    HSTABLE+X'61'                                           02206300
         DC     C'abcdefghij'                                           02206400
         ORG    HSTABLE+X'6B'                                           02206500
         DC     C'klmnopqrs'                                            02206600
         ORG    HSTABLE+X'74'                                           02206700
         DC     C'tuvwxyz'                                              02206800
         ORG    HSTABLE+X'7B'                                           02206900
         DC     C'{'                                                    02207000
         ORG    HSTABLE+X'7C'                                           02207100
         DC     C'º'                                                    02207200
         ORG    HSTABLE+X'7D'                                           02207300
         DC     C'}'                                                    02207400
         ORG    HSTABLE+X'7E'                                           02207500
         DC     C'~'                                                    02207600
         ORG    HSTABLE+256                                             02207700
         LTORG                                                          02207800
         DS     0F                                                      02207900
         EJECT                                                          02208000
BITABLE  DC     X'F0F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1'                     02208100
         DC     X'F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1'                     02208200
         DC     X'F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1'                     02208300
         DC     X'F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1'                     02208400
         DC     X'F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1'                     02208500
         DC     X'F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1'                     02208600
         DC     X'F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1'                     02208700
         DC     X'F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1'                     02208800
         DC     X'F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1'                     02208900
         DC     X'F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1'                     02209000
         DC     X'F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1'                     02210000
         DC     X'F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1'                     02220000
         DC     X'F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1'                     02230000
         DC     X'F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1'                     02240000
         DC     X'F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1'                     02250000
         DC     X'F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1'                     02260000
         LTORG                                                          02270000
         EJECT                                                          02280000
BYTABLE  DC     X'00000000000000000000000000000000'                     02290000
         DC     X'00000000000000000000000000000000'                     02300000
         DC     X'00000000000000000000000000000000'                     02310000
         DC     X'00000000000000000000000000000000'                     02320000
         DC     X'00000000000000000000000000000000'                     02330000
         DC     X'00000000000000000000000000000000'                     02340000
         DC     X'00000000000000000000000000000000'                     02350000
         DC     X'00000000000000000000000000000000'                     02360000
         DC     X'00000000000000000000000000000000'                     02370000
         DC     X'00000000000000000000000000000000'                     02371000
         DC     X'00000000000000000000000000000000'                     02372000
         DC     X'00000000000000000000000000000000'                     02373000
         DC     X'00000000000000000000000000000000'                     02374000
         DC     X'00000000000000000000000000000000'                     02375000
         DC     X'00000000000000000000000000000000'                     02376000
         DC     X'00000000000000000000000000000000'                     02377000
         ORG    BYTABLE+C'1'                                            02378000
         DC     X'FF'                                                   02379000
         LTORG                                                          02380000
         DS     0F                                                      02390000
         EJECT                                                          02391000
SAVEREG  DS     F                                                       02392000
         DS     F                                                       02393000
DOUBLE   DS     D                                                       02394000
         DS     F                                                       02395000
DOUBLE1  DS     D                                                       02396000
TYPEADDR DS     F                                                       02397000
INLGADDR DS     F                                                       02398000
OULGADDR DS     F                                                       02399000
INRDADDR DS     F                                                       02399100
OURDADDR DS     F                                                       02399200
ONEBYTE  DS     XL1                                                     02399300
REQSTYPE DS     XL1                                                     02399400
NULLS    DC     XL256'00'                                               02399500
ADDRMAP  DS     0F                                                      02399600
         DC     C'0',A(CONVPNS)                                         02399700
         DC     C'1',A(CONVASC)                                         02399800
         DC     C'2',A(CONVBYT)                                         02399900
         DC     C'3',A(CONVEBC)                                         02400000
         DC     C'4',A(CONVITL)                                         02410000
         DC     C'5',A(CONVBIT)                                         02420000
         DC     C'6',A(CONVPDN)                                         02430000
         DC     X'FF'                                                   02440000
         END                                                            02450000
         EJECT                                                          02460000
