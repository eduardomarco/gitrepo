     *****************************************************************
     ** Copyright (c) 2003 SSA Global Technologies, Inc.
     ******************************************************************
     ** OBJECT ID: ADM500       WRITTEN: 05/12/89   VERSION: 06.2
     ** TEXT:      RPGLE: Inventory Adjustments Selection
     ******************************************************************
     ** MODIFICATIONS:
     **----------------------------------------------------------------
     ** MOD   SCN    DATE   MODIFICATION SUMMARY
     **----------------------------------------------------------------
     ** 9999  99999    9999 PROBLEM: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
     ** JC01        24/11/00 MOTIVO: PARM IMPRESSORA LASER P/ LAB100
     ** em01        20/09/21 MOTIVO: Nao permite Ajuste para filial
     *****************************************************************
     F*  Special Instruction/Notes
     F*  -------------------------
     F*  The variable XATYP returns the adjustment status as follows:
     F*
     F*    'ADJ' -- Adjustment posted successfully.
     F*    'TRN' -- Transfer posted successfully.
     F*    'STS' -- Status Change posted successfully.
     F*    'LOC' -- Adjustment not located.
     F*    'PCK' -- Unable to locate inventory for picking.
     F*    'CAN' -- Posting cancelled.
     F*
     F*****************************************************************
     H/COPY QCPYLESRC,ZYLPSRH
     F**
     FWPASBL06  IF   E           K DISK
     F**
     FADM500FM  CF   E             WORKSTN
     F                                     INFDS(FDSDSP)
     F**
     FWATYBL01  IF   E           K DISK
     F**
     FWINABL01  IF   E           K DISK
     F**
em01 FWAJUFIL   IF   E           K DISK
em01 F**
     D*****************************************************************
     D**
     D** Program Description Array
     D**
     D**
     D*****************************************************************
     D**  Data structures (external/internal) and LDA definitions    **
     D*****************************************************************
     D**
     D** Session Parameters
     D**
     D WSNPB         E DS
     D  @WSNPB                 1    256
     D**
     D WPTPB         E DS
     D**
     D**
     D** Line Number - Multiple Occur. Data Structure
     D WXLNB         E DS                  OCCURS(9999) INZ
     D**
     D FDSDSP          DS
     D  @CSRLC               370    371B 0
     D  @CRSOR               370    371
     D**
     D                 DS
     D  @MSG                   1      7
     D**
     D                 DS
     D  @SECFN                 1     10
     D**
     D                 DS
     D  @HLPSC                 1      8
     D**
     D XLDA           UDS
     D  @SNDTA               769   1024
     D**
     D*****************************************************************
     D**  Constant Definitions                                       **
     D*****************************************************************
     D CCYES           C                   CONST('Y')
     D CCNO            C                   CONST('N')
     I**
     C*****************************************************************
     C**   Keylists, *LIKE field definitions, and *ENTRY parms
     C*****************************************************************
     C**
     C     *DTAARA       DEFINE    *LDA          XLDA
     C     *LIKE         DEFINE    PEPRTT        VPUDTA
     C     *LIKE         DEFINE    PEPRTT        VPPRTR
     C     *LIKE         DEFINE    SEWHS         VPPTRP          - 1
     C     *LIKE         DEFINE    SEWHS         VPWHS
     C     *LIKE         DEFINE    SERFTR        VPEND
     C     *LIKE         DEFINE    XATYP         VPTYP
     C     *LIKE         DEFINE    XACN          VPACN
     C     *LIKE         DEFINE    ATCONT        VPRTN
     C**
     C     TYPKY         KLIST
     C                   KFLD                    XATYP
     C**
     C     TYPKY1        KLIST
     C                   KFLD                    IAATYP
     C**
     C     ADJKY         KLIST
     C                   KFLD                    XACN
     C**
     C**  Key by Source type, Source Control #
     C**
     C     PASKEY        KLIST
     C                   KFLD                    $ADJ
     C                   KFLD                    IAACN
     C**
em01 C     WHSKEY        KLIST
em01 C                   KFLD                    XXWHS
em01 C                   KFLD                    XATYP
em01 C**
     C     *ENTRY        PLIST
     C                   PARM                    VPEND
     C**
     C     PARM01        PLIST
     C                   PARM                    VPTYP
     C                   PARM                    VPACN
     C                   PARM                    VPRTN
     C**
     C*****************************************************************
     C**                 M A I N L I N E
     C*****************************************************************
     C**
     C     VPEND         IFEQ      CCYES
     C                   EXSR      $S001
     C                   RETURN
     C                   ENDIF
     C**
     C                   EXSR      $S000
     C**
     C**  Process first screen
     C**
     C     *INKC         DOWEQ     '0'
     C**
     C     *IN25         DOUEQ     '0'
     C     *IN25         IFEQ      '1'
     C                   READ      ADM50001                               95
     C                   ELSE
     C                   WRITE     ADM50000
     C                   EXFMT     ADM50001
     C                   ENDIF
     C     *IN25         IFEQ      '1'
     C                   MOVEL     'ADM50001'    @HLPSC
     C                   EXSR      $S900
     C                   ENDIF
     C                   ENDDO
     C**
     C     *IN98         CASEQ     '1'           $S999
     C                   ENDCS
     C**
     C**  Continue processing if F3 not pressed
     C**
     C     *INKC         IFEQ      '0'
     C**
     C**  Process command keys and ENTER
     C**
     C     *INKD         CASEQ     '1'           $PROMPT
     C     *INKG         CASEQ     '1'           $S107
     C     *INKR         CASEQ     '1'           $S117
     C     *INKS         CASEQ     '1'           $S109
     C                   CAS                     $S200
     C                   ENDCS
     C**
     C                   ENDIF
     C**
     C                   ENDDO
     C**
     C                   RETURN
     C**
     C*************************************************************************
     C**  $PROMPT -  Prompt key pressed call Prompt Program
     C*************************************************************************
     C     $PROMPT       BEGSR
     C*  PARAMETER LIST FOR PROMPT LOOKUP WINDOW. THE FOLLOWING
     C*  PARAMETER FIELDS ARE DEFINED AS FOLLOWS:
     C*     #SITE  - CURRENT SITE                            (SENT)
     C*     #HOUSE - CURRENT WAREHOUSE                       (SENT)
     C*     @CRSOR - CURSOR POSITION IN FILE INFORMATION
     C*              DATA STRUCTURE. THIS IS USED FOR THE
     C*              DISPLAY ACTIVE OBJECTS LOOKUP WINDOW    (SENT)
     C*     @SELFD - SELECTION FIELD TO BE RETURNED FROM THE
     C*              LOOKUP PROGRAM                          (RETURNED)
     C*     @INPAR - STARTING KEY IF REQUIRED                (SENT)
     C*     @ERRFL - ERROR FLAG SETTING RETURNED FROM THE
     C*              LOOKUP PROGRAM                          (RETURNED)
     C     PRPSRV        PLIST
     C                   PARM                    #SITE             3
     C                   PARM                    #HOUSE            4
     C                   PARM                    @CRSOR
     C                   PARM                    @SELFD           50
     C                   PARM                    @INPAR           32
     C                   PARM                    @ERRFL            1
     C*
     C                   EVAL      #HOUSE = XXWHS
     C* Transaction Type
     C                   EVAL      @SELFD = *BLANKS
     C                   SELECT
     C                   WHEN      ####CF = 'XATYP'
     C                   CALL(E)   'PRM003'      PRPSRV
     C                   IF        @SELFD <> *BLANKS
     C                   MOVEL     @SELFD        XATYP
     C                   ENDIF
     C*
     C                   OTHER
     C                   CALL(E)   'PRM000'
     C                   ENDSL
     C     @CSRLC        DIV       256           ####RW
     C                   MVR                     ####CL
     C                   ENDSR
     C*****************************************************************
     C**  $S000  -  Initialization routine
     C*****************************************************************
     C**
     C     $S000         BEGSR
     C**
     C**  Initialize variables
     C**
     C                   MOVEL     'ADM500'      SFPGMQ
     C                   MOVE      'ADJ'         $ADJ              3
     C                   Z-ADD     0             $ADJL             4 0
     C                   MOVE      '0'           *INKC
     C                   MOVE      '0'           *IN25
     C**
     C**  Bring in session parameters
     C**
     C**
     C**  Fill screen header fields
     C**
     C                   MOVEL     'ADM500'      XXPGMN
     C                   MOVEL     SETRID        XXTRID
     C                   MOVEL     SEOPER        XXUSID
     C                   MOVEL     SESYSN        XXSYSN
     C                   MOVEL     SEINSN        XXINSN
     C                   MOVEL     SEWHS         XXWHS
     C**
     C                   MOVE      SEWHS         VPWHS
     C                   MOVEL     '51 '         VPPTRP
     C                   MOVE      *BLANKS       VPPRTR
     C                   CALL      'STM910C'
     C                   PARM                    VPWHS
     C                   PARM                    VPPTRP
     C                   PARM                    VPPRTR
     C**
     C                   MOVEL     'AD'          VPUDTA
     C**
JC01 C                   MOVE      'N'           WIBM              1
JC01 C**
     C                   ENDSR
     C*****************************************************************
     C**  $S001  -  Exit Cleanup/Unload Programs                     **
     C*****************************************************************
     C     $S001         BEGSR
     C**
     C                   MOVE      '1'           *INLR
     C**
     C                   ENDSR
     C**
     C*****************************************************************
     C**  $S010  -  Send message after returning from called programs
     C*****************************************************************
     C**
     C     $S010         BEGSR
     C**
     C**  Reset variables
     C**
     C                   MOVE      *BLANKS       XATYP
     C                   MOVE      *BLANKS       XDESC
     C                   Z-ADD     0             XACN
     C**
     C                   ENDSR
     C**
     C*****************************************************************
     C**  $S107  -  Process F7 =  EXECUTE
     C*****************************************************************
     C**
     C     $S107         BEGSR
     C**
     C**  Validate transaction type entered
     C**
     C                   EXSR      $S200
     C     *IN98         IFEQ      '0'
     C**
     C                   MOVE      'N'           VPEXIT            1
     C**
     C**  Check content type to determine program to call
     C**
     C     ATCONT        IFEQ      'I'
     C**
     C**  Call Inventory Adjustments processing
     C**
     C                   CALL      'ADM510C'
     C                   PARM                    XATYP
     C                   PARM                    VPEXIT
     C                   ELSE
     C**
     C**  Call Indirect Inventory Adjustments processing
     C**
     C                   CALL      'ADM520C'
     C                   PARM                    XATYP
     C                   PARM                    VPEXIT
     C                   ENDIF
     C**
     C**  Send message about posting results
     C**
     C     VPEXIT        IFEQ      'N'
     C                   EXSR      $S010
     C                   ELSE
     C                   MOVE      '1'           *INKC
     C                   ENDIF
     C**
     C                   ENDIF
     C**
     C                   ENDSR
     C**
     C*****************************************************************
     C**  $S109  -  Process F18  =  REPRINT
     C*****************************************************************
     C**
     C     $S109         BEGSR
     C**
     C**  Validate control number entered
     C**
     C                   EXSR      $S200
     C     *IN98         IFEQ      '0'
     C**
     C**  Determine what kind of labels to reprint
     C**
     C     ATADJC        IFEQ      'A'
     C     IAADJQ        ANDLT     0
     C                   EXSR      $S310
     C                   ELSE
     C     ATADJC        IFEQ      'A'
     C     IAADJQ        ANDGT     0
     C                   EXSR      $S320
     C                   ELSE
     C                   EXSR      $S330
     C                   ENDIF
     C                   ENDIF
     C**
     C**  Send message if labels reprinted successfully
     C**
     C     *IN98         IFEQ      '0'
     C                   MOVE      'PDS2287'     @MSG
     C                   EXSR      $S998
     C                   ENDIF
     C**
     C**  Reset variables
     C**
     C                   MOVE      *BLANKS       XATYP
     C                   MOVE      *BLANKS       XDESC
     C                   Z-ADD     0             XACN
     C**
     C                   ENDIF
     C**
     C                   ENDSR
     C**
     C*****************************************************************
     C**  $S117  -  Process F17  =  Backout
     C*****************************************************************
     C**
     C     $S117         BEGSR
     C**
     C**  Validate control number entered
     C**
     C                   EXSR      $S200
     C**
     C** Warning message to confirm back out.
     C**
     C     *IN98         IFEQ      '0'
     C**
     C                   SETOFF                                       KGKL
     C                   MOVE      'PDS5631'     @MSG
     C                   EXSR      $S998
     C**
     C     *INKG         DOWEQ     '0'
     C     *INKL         ANDEQ     '0'
     C**
     C                   EXFMT     ADM50001
     C**
     C     *INKG         IFEQ      '1'
     C                   EXSR      $S999
     C                   MOVE      *BLANKS       @MSG
     C                   ELSE
     C     *INKL         IFEQ      '1'
     C                   EXSR      $S999
     C                   MOVE      *BLANKS       @MSG
     C                   MOVE      'PDS1030'     @MSG
     C                   EXSR      $S998
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C**
     C                   ENDIF
     C**
     C     *IN98         IFEQ      '0'
     C**
     C**  Call Backout program
     C**
     C                   MOVEL     'ADJ'         VPTYP
     C                   Z-ADD     XACN          VPACN
     C                   MOVEL     '0'           VPRTN
     C                   CALL      'ADM550'      PARM01
     C**
     C**  Send message
     C**
     C                   SELECT
     C     VPRTN         WHENEQ    '1'
     C                   MOVEL(P)  'DSQ3675'     @MSG
     C     VPRTN         WHENEQ    '2'
     C                   MOVEL(P)  'PDS6209'     @MSG
     C                   OTHER
     C                   MOVEL(P)  'DSD1021'     @MSG
     C                   ENDSL
     C                   EXSR      $S998
     C**
     C**  Reset variables
     C**
     C                   MOVE      *BLANKS       XATYP
     C                   MOVE      *BLANKS       XDESC
     C                   Z-ADD     0             XACN
     C**
     C                   ENDIF
     C**
     C                   ENDSR
     C**
     C*****************************************************************
     C**  $S200  -  Process ENTER key
     C*****************************************************************
     C**
     C     $S200         BEGSR
     C**
     C**  Validate screen data entered
     C**
     C                   MOVEA     '00'          *IN(40)
     C                   MOVE      *BLANKS       XDESC
     C**
     C**  Check to see if transaction type OR control number
     C**  was entered but not both
     C**
     C     XATYP         IFNE      *BLANKS
     C     XACN          ANDNE     0
     C                   MOVEA     '11'          *IN(40)
     C                   MOVE      'PDS2299'     @MSG
     C                   EXSR      $S998
     C                   GOTO      S20099
     C                   ENDIF
     C**
     C     XATYP         IFEQ      *BLANKS
     C     XACN          ANDEQ     0
     C                   MOVEA     '11'          *IN(40)
     C                   MOVE      'PDS2288'     @MSG
     C                   EXSR      $S998
     C                   GOTO      S20099
     C                   ENDIF
     C**
     C**  Validate transaction type is entered and valid
     C**
     C     *INKG         IFEQ      '1'
     C     XATYP         ORNE      *BLANKS
     C     *INKR         ANDEQ     '0'
     C     *INKS         ANDEQ     '0'
     C                   EXSR      $S210
     C                   GOTO      S20099
     C                   ENDIF
     C**
     C**  Validate control number is entered and valid
     C**
     C     *INKS         IFEQ      '1'
     C     *INKR         OREQ      '1'
     C     XACN          ORNE      0
     C                   EXSR      $S220
     C                   ENDIF
     C**
     C     S20099        ENDSR
     C**
     C*****************************************************************
     C**  $S210  -  Validate transaction type entered
     C*****************************************************************
     C**
     C     $S210         BEGSR
     C**
     C**  Check that transaction type is entered and valid
     C**
     C     XATYP         IFEQ      *BLANKS
     C                   MOVE      '1'           *IN40
     C                   MOVE      'PDS2301'     @MSG
     C                   EXSR      $S998
     C                   GOTO      S21099
     C                   ENDIF
em01 C*
em01 C     WHSKEY        CHAIN     WAJUFIL                            92
em01 C     *IN92         IFEQ      '0'
em01 C                   MOVE      '1'           *IN40
em01 C                   MOVE      'PDS2302'     @MSG
em01 C                   EXSR      $S998
em01 C                   GOTO      S21099
em01 C                   ENDIF
     C**
     C     TYPKY         CHAIN     WATYBL01                           92
     C
     C**
     C     *IN92         IFEQ      '0'
     C                   MOVE      ATDESC        XDESC
     C**
     C     ATADJC        IFNE      'A'
     C     ATADJC        ANDNE     'S'
     C     ATADJC        ANDNE     'T'
     C                   MOVE      '1'           *IN40
     C                   MOVE      'PDS2298'     @MSG
     C                   EXSR      $S998
     C                   GOTO      S21099
     C                   ENDIF
     C**
     C                   ELSE
     C                   MOVE      '1'           *IN40
     C                   MOVE      'PDS2302'     @MSG
     C                   EXSR      $S998
     C                   GOTO      S21099
     C                   ENDIF
     C**
     C**  Check if user is authorized to transaction type entered
     C**
     C     *INKG         IFEQ      '1'
     C     ATFUNC        ANDNE     *BLANKS
     C                   MOVEL     ATFUNC        @SECFN
     C                   EXSR      $S920
     C                   ENDIF
     C**
     C     S21099        ENDSR
     C**
     C*****************************************************************
     C**  $S220  -  Validate control number entered
     C*****************************************************************
     C**
     C     $S220         BEGSR
     C**
     C**  Check that control number is entered and valid
     C**
     C     XACN          IFEQ      0
     C                   MOVE      '1'           *IN41
     C                   MOVE      'PDS2303'     @MSG
     C                   EXSR      $S998
     C                   GOTO      S22099
     C                   ENDIF
     C**
     C**  Validate the control number if F17(backout) was not pressed
     C**
     C     *INKR         IFEQ      *OFF
     C     ADJKY         CHAIN     WINABL01                           92
     C**
     C     *IN92         IFEQ      '1'
     C                   MOVE      '1'           *IN41
     C                   MOVE      'PDS2304'     @MSG
     C                   EXSR      $S998
     C                   GOTO      S22099
     C                   ENDIF
     C**
     C     TYPKY1        CHAIN     WATYBL01                           92
     C**
     C     ATADJC        IFNE      'A'
     C     ATADJC        ANDNE     'T'
     C     *IN92         OREQ      '1'
     C                   MOVE      'PDS2290'     @MSG
     C                   EXSR      $S998
     C                   GOTO      S22099
     C                   ENDIF
     C                   ENDIF
     C**
     C**  Check authority to Backout
     C**
     C     *INKR         IFEQ      '1'
     C                   MOVEL(P)  'ADM0102C'    @SECFN
     C                   EXSR      $S920
     C                   ENDIF
     C**
     C**  Check authority to reprint labels
     C**
     C     *INKS         IFEQ      '1'
     C     IAOPER        IFEQ      XXUSID
     C                   MOVEL     'ADM0102A'    @SECFN
     C                   EXSR      $S920
     C                   ELSE
     C                   MOVEL     'ADM0102B'    @SECFN
     C                   EXSR      $S920
     C                   ENDIF
     C                   ENDIF
     C**
     C     S22099        ENDSR
     C**
     C*****************************************************************
     C**  $S310  -  Reprint labels for a negative adjustment
     C*****************************************************************
     C**
     C     $S310         BEGSR
     C**
     C**  Check if adjustment uses labels
     C**
     C     IAAISL        IFEQ      *BLANKS
     C     IABAY         ANDEQ     *BLANKS
     C     IALEVL        ANDEQ     *BLANKS
     C     ATPRKL        OREQ      'Y'
     C**
     C     PASKEY        CHAIN     WPASBL06                           90
     C     *IN90         IFEQ      *ON
     C                   MOVE      'PDS5372'     @MSG
     C                   EXSR      $S998
     C                   ELSE
     C**
     C                   CALL      'LAB300'
     C                   PARM                    $ADJ
     C                   PARM                    IAACN
     C**
     C                   ENDIF
     C**
     C                   ELSE
     C**
     C                   MOVE      'PDS2289'     @MSG
     C                   EXSR      $S998
     C**
     C                   ENDIF
     C**
     C                   ENDSR
     C**
     C*****************************************************************
     C**  $S320  -  Reprint labels for a positive adjustment
     C*****************************************************************
     C**
     C     $S320         BEGSR
     C**
     C**  Check if adjustment uses labels
     C**
     C     IAAISL        IFEQ      *BLANKS
     C     IABAY         ANDEQ     *BLANKS
     C     IALEVL        ANDEQ     *BLANKS
     C     ATPRUL        OREQ      'Y'
     C**
     C                   MOVE      IAACN         VPUDTA
     C                   CALL      'LAB100'
     C                   PARM                    $ADJ
     C                   PARM                    IAACN
     C                   PARM                    $ADJL
     C                   PARM                    WSNPB
     C                   PARM                    VPPRTR
     C                   PARM                    VPUDTA
MIG62C*                  PARM                    WXLNB
JC01 C                   PARM                    WIBM
     C**
     C                   ELSE
     C**
     C                   MOVE      'PDS2289'     @MSG
     C                   EXSR      $S998
     C**
     C                   ENDIF
     C**
     C                   ENDSR
     C**
     C*****************************************************************
     C**  $S330  -  Reprint labels for a transfer
     C*****************************************************************
     C**
     C     $S330         BEGSR
     C**
     C**  Check if adjustment uses labels
     C**
     C     IAAISL        IFEQ      *BLANKS
     C     IABAY         ANDEQ     *BLANKS
     C     IALEVL        ANDEQ     *BLANKS
     C                   MOVE      'Y'           ATPRKL
     C                   ENDIF
     C**
     C     IATASL        IFEQ      *BLANKS
     C     IATBAY        ANDEQ     *BLANKS
     C     IATLVL        ANDEQ     *BLANKS
     C                   MOVE      'Y'           ATPRUL
     C                   ENDIF
     C**
     C     ATPRUL        IFEQ      'Y'
     C     ATPRKL        ANDEQ     'Y'
JC01 C     ATADJC        OREQ      'T'
     C                   CALL      'LAB400'
     C                   PARM                    $ADJ
     C                   PARM                    IAACN
     C                   ELSE
     C**
     C                   MOVE      IAACN         VPUDTA
     C     ATPRUL        IFEQ      'Y'
     C                   CALL      'LAB100'
     C                   PARM                    $ADJ
     C                   PARM                    IAACN
     C                   PARM                    $ADJL
     C                   PARM                    WSNPB
     C                   PARM                    VPPRTR
     C                   PARM                    VPUDTA
MIG62C*                  PARM                    WXLNB
JC01 C                   PARM                    WIBM
     C                   ELSE
     C**
     C     ATPRKL        IFEQ      'Y'
     C                   CALL      'LAB300'
     C                   PARM                    $ADJ
     C                   PARM                    IAACN
     C                   ELSE
     C**
     C                   MOVE      'PDS2289'     @MSG
     C                   EXSR      $S998
     C**
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C**
     C                   ENDSR
     C**
     C*****************************************************************
     C**  $S900  -  HELP key processing
     C*****************************************************************
     C**
     C     $S900         BEGSR
     C**
     C                   CALL      'STM900C'
     C                   PARM                    @HLPSC
     C**
     C                   ENDSR
     C**
     C*****************************************************************
     C**  $S920  -  Security checking routine
     C*****************************************************************
     C**
     C     $S920         BEGSR
     C**
     C                   MOVE      *BLANKS       @MSG
     C                   CALL      'STM920'
     C                   PARM                    @SECFN
     C                   PARM                    @MSG
     C**
     C     @MSG          CASNE     *BLANKS       $S998
     C                   ENDCS
     C**
     C                   ENDSR
     C**
     C*****************************************************************
     C**  $S998  -  Write subfile message routine
     C*****************************************************************
     C**
     C     $S998         BEGSR
     C**
     C                   CALL      'STM998C'
     C                   PARM                    @MSG
     C                   PARM                    SFPGMQ
     C**
     C                   MOVE      '1'           *IN98
     C                   WRITE     MSG000C1
     C**
     C                   ENDSR
     C**
     C*****************************************************************
     C**  $S999  -  Clear subfile message routine
     C*****************************************************************
     C**
     C     $S999         BEGSR
     C**
     C                   CALL      'STM999C'
     C                   PARM                    SFPGMQ
     C**
     C                   WRITE     MSG000C1
     C                   MOVE      '0'           *IN98
     C**
     C                   ENDSR
     C*****************************************************************
     C**  *INZSR -  System Program Initialization                    **
     C*****************************************************************
     C     *INZSR        BEGSR
     C**
     C**  Bring in session parameters
     C**
     C                   IN        XLDA
     C                   MOVEL     @SNDTA        @WSNPB
     C**
     C                   ENDSR
     C*****************************************************************
