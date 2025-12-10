

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABDROPS.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE
   INCLUDE('StAMRsz.inc'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR012.INC'),ONCE        !Local module procedure declarations
                       INCLUDE('DICTIONARYEDITOR016.INC'),ONCE        !Req'd for module callout resolution
                       INCLUDE('DICTIONARYEDITOR017.INC'),ONCE        !Req'd for module callout resolution
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! Form Fields
!!! </summary>
UpdateField PROCEDURE (string pParams,string pThread)

ParamsGrp           Group(ParamsGroupType).
OverStr             String(Size(ParamsGrp)),Over(ParamsGrp)
json				JSONClass
st					StringTheory
WinCtrlSt           StringTheory
MyDct				dwrDctParser
xml                 xFilesTree
mt                  MyTableClass
!Drawer              DrawClass
!GlyphPainter        GlyphPainterClass
!FontVerifier        GlyphEnumFontClass
FieldGrp            Group(FieldGroupType).
FldOverStr          String(Size(FieldGrp)),Over(FieldGrp)
Rec                 &GROUP

OptionGrp               Group(OptionGroupType).
CommentGrp              Group(CommentGroupType).

CommentRecsCnt          Long
OptionRecCnt            Long
i                       Long

DctEvent:FetchRecord        Equate(EVENT:User+900)
DctEvent:RefreshData        Equate(EVENT:User+901)
DctEvent:RefreshControls    Equate(EVENT:User+902)

!WindowControl            Queue,Name('WindowControl | RowName()')
!text                       STRING(255),Name('Text | attribute')
!                         End

!WindowControl   		Group,Name('WindowControl') ! | RowName(WindowControl)
!LinesQ					&lineQueueType,Name('Line | queue')	! | RowName(Line)
!                        End
!
!lineQueueType          	Queue,Type,Name('Line | RowName(Line)') ! | RowName(Line)
!text                    STRING(255),Name('Text | attribute')
!                        End

FONT_NAME           CSTRING(32)
hDC                 LONG
hFont               LONG
hOldFont            LONG
Matrix              LIKE(MAT2)
GM                  LIKE(GLYPHMETRICS)
bufSize             LONG
Buffer              &BYTE
pBuffer             LONG        ! pointer returned by ALLOCATE
Character           CSTRING(2)
CharCode            UNSIGNED
res                 LONG
msg                 STRING(200)


FieldPropertiesGroup		Group,Pre(FldProps)
GeneralGrp					 Group,Name('General')
ImageFile                     String(255),Name('ImageFile')
ImageMode                     String(10),Name('ImageMode')
Justification				  String(10),Name('Justification')
Offset                        Long,Name('Offset')
Layout						  String(10),Name('Layout')
UseRichTextFormat             Byte,Name('UseRichTextFormat')
StringValue                   String(100),Name('StringValue')
IsPicture                     Byte,Name('IsPicture')
Picture                       String(10),Name('Picture')
TextCase                      String(20),Name('TextCase')
Text    					  String(100),Name('Text')
TextFont					  String(100),Name('TextFont')
Bold						  Byte,Name('Bold')
GdiCharSet					  Long,Name('GdiCharSet')
Italic					      Byte,Name('Italic')
FontName                      String(100),Name('FontName')
FontSize					  REAL,Name('FontSize')
StrikeOut					  Byte,Name('StrikeOut')
Underline                     Byte,Name('Underline')
CtrlUse						  String(100),Name('CtrlUse')
FalseValue					  String(10),Name('FalseValue')
TrueValue                     String(10),Name('TrueValue')
ThirdStateEnable              Byte,Name('ThirdStateEnable')
ThirdStateValue               String(10),Name('ThirdStateValue')
							 End
ColorGrp					 Group,Name('Color')
BackGround					  Long,Name('BackGround')
SelBackGround                 Long,Name('SelBackGround')
SelForeGround                 Long,Name('SelForeGround')
TextColor					  Long,Name('TextColor')
							 End
DesignGrp					 Group,Name('Design')
SuppressTransparency		  BYTE,Name('SuppressTransparency')
TabIndex					  Long,Name('TabIndex')
							 End
ExtraGrp					 Group,Name('Extra')
Delay                         Long,Name('Delay')
Angle                         Real,Name('Angle')
Boxed                         Byte,Name('Boxed')
DropID						  String(100),Name('DropID')
EntryMode                     String(20),Name('EntryMode')
RangeLower                    Long,Name('RangeLower')
RangeStep                     Long,Name('RangeStep')
RangeUpper                    Long,Name('RangeUpper')
RangeRepeat                   Long,Name('RangeRepeat')
TextCase                      String(20),Name('TextCase')
Icon                          String(100),Name('Icon')
							 End
HelpGrp						 Group,Name('Help')
Alrt                          String(100),Name('Alrt')
Cursor						  String(100),Name('Cursor')
HelpID                        String(100),Name('HelpID')
Key                           String(100),Name('Key')
Message                       String(255),Name('Message')
Tip                           String(25),Name('Tip')
							 End
ModeGrp						 Group,Name('Mode')
Disable						  Byte,Name('Disable')
Hide                          Byte,Name('Hide')
ReadOnly                      Byte,Name('ReadOnly')
Scroll                        Byte,Name('Scroll')
Skip                          Byte,Name('Skip')
Transparent                   Byte,Name('Transparent')
							 End
OptionsGrp                   Group,Name('Options')
Flat                           Byte,Name('Flat')
ReadOnly                       Byte,Name('ReadOnly')
Required                       Byte,Name('Required')
Single                         Byte,Name('Single')
                             End
PositionGrp					 Group,Name('Position')
Height						  Long,Name('Height')
DefaultHeight				  Byte,Name('DefaultHeight')
Width						  Long,Name('Width')
DefaultWidth				  Byte,Name('DefaultWidth')
Xpos						  Long,Name('Xpos')
Ypos                          Long,Name('Ypos')
							 End
ScrollbarsGrp				 Group,Name('Scrollbars')
Horizontal			           Byte,Name('Horizontal')
Vertical                       Byte,Name('Vertical')
                             End
							End


local                   CLASS
PopulateControls        Procedure()
PopulateControlTypesQ   Procedure(<long pTabChoice>)
PopulateCommentsQ       Procedure()
PopulateOptionsQ        Procedure()
PopulateValidityCheck   Procedure()
SaveGlyphBitmapToFile   PROCEDURE(STRING pFileName, *GLYPHMETRICS pGM, LONG pBuffer, LONG bufSize)
SetFieldSize            Procedure()
SetScreenPicture        Procedure()
SetWinCtrlBlob          Procedure(string pPrompt,string pLabel)
                        END
NotifyHandler          CLASS(UltimateNotify)
HandleNotify            PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter),DERIVED
                    END
CurrentTab           STRING(80)                            ! 
ControlTypesQ        QUEUE,PRE(CtrlTypesQ)                 ! 
CtrlType             STRING(6),NAME('CtrlType')            ! 
RowID                LONG                                  ! 
                     END                                   ! 
fonts                QUEUE,PRE(fonts)                      ! 
name                 STRING(64)                            ! 
style                STRING(32)                            ! 
script               STRING(32)                            ! 
weight               LONG                                  ! 
quality              BYTE                                  ! 
pitchandfamily       BYTE                                  ! 
                     END                                   ! 
WindowControl        QUEUE,PRE(WinCtrl),NAME('WindowControl | RowName()') ! 
text                 STRING(255),NAME('Text | attribute')  ! 
                     END                                   ! 
NotifyParam          STRING(1500)                          ! 
OptionsQ             QUEUE,PRE(),NAME('Option')            ! 
Id                   LONG,NAME('Id | Private')             ! 
Property             STRING(100),NAME('Property | attribute') ! 
PropertyType         STRING(5),NAME('PropertyType | attribute') ! 
PropertyValue        STRING(100),NAME('PropertyValue | attribute') ! 
                     END                                   ! 
CommentQ             QUEUE,PRE(CmtQ),NAME('Comment  | RowName(Comment)') ! 
PKGuid               STRING(16),NAME('PKGuid | Private')   ! 
Text                 STRING(255),NAME('Text | attribute')  ! 
Audit                GROUP,PRE(),NAME('Audit')             ! 
CreateUser           STRING(50),NAME('CreateUser | attribute') ! 
CreateDate           STRING(11),NAME('CreateDate | attribute') ! 
CreateTime           STRING(10),NAME('CreateTime | attribute') ! 
CreateVersionNumber  STRING(3),NAME('CreateVersionNumber | attribute') ! 
ModifiedUser         STRING(50),NAME('ModifiedUser | attribute') ! 
ModifiedDate         STRING(11),NAME('ModifiedDate | attribute') ! 
ModifiedTime         STRING(10),NAME('ModifiedTime | attribute') ! 
ModifiedVersionNumber STRING(3),NAME('ModifiedVersionNumber | attribute') ! 
                     END                                   ! 
                     END                                   ! 
ValidityGrp          GROUP,PRE(),NAME('Validity')          ! 
GUID                 STRING(16),NAME('PKGuid | Private')   ! 
ParentGUID           STRING(16),NAME('ParentGUID | Private') ! 
FieldGuid            STRING(16),NAME('Guid | Attribute')   ! 
Check                STRING(20),NAME('Check | Attribute')  ! 
Lookup               STRING(40),NAME('Lookup | Attribute') ! 
Choices              STRING(100),NAME('Choices | Attribute') ! 
Values               STRING(100),NAME('Values | Attribute') ! 
RangeHigh            STRING(20),NAME('RangeHigh | Attribute') ! 
TrueValue            STRING(100),NAME('TrueValue | Attribute') ! 
FalseValue           STRING(100),NAME('FalseValue | Attribute') ! 
                     END                                   ! 
ActionMessage        CSTRING(40)                           ! 
ReportControl        QUEUE,PRE(RepCtrl),NAME('ReportControl | RowNam') ! 
text                 STRING(255),NAME('Text | attribute')  ! 
                     END                                   ! 
!---- Noyantis : Codejock Property Grid Wrapper - Start ----
PropertyGrid6                 CLASS(PropertyGridClass)
Drop                            PROCEDURE(STRING paramDragID, STRING paramDropID),DERIVED
Event                           PROCEDURE(STRING paramEventName, <*SHORT paramReference>, <SIGNED paramOleControl>, <LONG paramCurrentEvent>),DERIVED
EventFunc                       PROCEDURE(*SHORT Reference, SIGNED OleControl, LONG CurrentEvent),DERIVED
EventFuncCommon                 PROCEDURE(*SHORT Reference, SIGNED OleControl, LONG CurrentEvent),DERIVED
Init                            PROCEDURE(),DERIVED
InitComplete                    PROCEDURE(),DERIVED
InitPrepare                     PROCEDURE(SIGNED paramOCXCtrl),DERIVED
InitResize                      PROCEDURE(),DERIVED
InitSecurity                    PROCEDURE(),DERIVED
InitTemplateSettings            PROCEDURE(),DERIVED
Keystroke                       PROCEDURE(UNSIGNED paramKeycode),DERIVED
Kill                            PROCEDURE(),DERIVED
KillComplete                    PROCEDURE(),DERIVED
ParametersReceived              PROCEDURE(<LONG paramSessionID>),DERIVED
ProcessClones                   PROCEDURE(),DERIVED
ProcessMimics                   PROCEDURE(),DERIVED
ProcessShortcutKey              PROCEDURE(UNSIGNED pKeyCode),DERIVED
RefreshContents                 PROCEDURE(<BYTE paramForce>),DERIVED
SyncOCXHeight                   PROCEDURE(<BYTE pForce>),DERIVED
SyncOCXWidth                    PROCEDURE(<BYTE pForce>),DERIVED
TakeEvent                       PROCEDURE(SIGNED paramEvent),DERIVED
TakeNotify                      PROCEDURE(UNSIGNED paramNotifyCode, SIGNED paramThread, LONG paramParameter),DERIVED
TakeSubClassEvent               PROCEDURE(UNSIGNED paramWndHndl, UNSIGNED paramMsg, UNSIGNED paramWParam, LONG paramLParam),DERIVED
TakeTimer                       PROCEDURE(),DERIVED
TakeWindowEvent                 PROCEDURE(*WINDOW paramWindow),DERIVED
CreateCategories                PROCEDURE(),DERIVED
CreateInPlaceButtons            PROCEDURE(),DERIVED
CreateItems                     PROCEDURE(),DERIVED
CreateVerbs                     PROCEDURE(),DERIVED
CatAccepted                     PROCEDURE(STRING paramID, BYTE paramExpanded),DERIVED
InPlaceButtonAccepted           PROCEDURE(STRING paramID),DERIVED
ItemAccepted                    PROCEDURE(STRING paramID, STRING paramValue, STRING paramClarionValue),DERIVED
ItemSelected                    PROCEDURE(STRING paramID, STRING paramValue, STRING paramClarionValue),DERIVED
VerbAccepted                    PROCEDURE(STRING paramID),DERIVED
                              END
PropertyGrid_Ctrl    CSTRING(20)
PropertyGrid_Result  BYTE
!---- Noyantis : Codejock Property Grid Wrapper - End ----
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
DonutHoleWindow     Class(NinjaDonutHoleWindowClass)
Repaint               PROCEDURE(),DERIVED
                    end
FDB4::View:FileDrop  VIEW(DataTypesLkUp)
                       PROJECT(Dat:Type)
                       PROJECT(Dat:GUID)
                     END
Queue:FileDrop       QUEUE                            !
Dat:Type               LIKE(Dat:Type)                 !List box control field - type derived from field
Dat:GUID               LIKE(Dat:GUID)                 !Primary key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
History::Fld:Record  LIKE(Fld:RECORD),THREAD
QuickWindow          WINDOW('Form Fields'),AT(,,448,305),FONT('Segoe UI',10,,FONT:regular,CHARSET:ANSI),RESIZE, |
  CENTER,GRAY,MAX,HLP('UpdateField'),IMM
                       SHEET,AT(2,1,446,304),USE(?CurrentTab),ABOVE
                         TAB('General'),USE(?GeneralTab)
                           PROMPT('Column Name:'),AT(6,23),USE(?Fld:FieldName:Prompt),TRN
                           ENTRY(@s100),AT(70,23,278,10),USE(Fld:FieldName),LEFT(2)
                           PROMPT('External Name:'),AT(6,37),USE(?Fld:ExternalName:Prompt),TRN
                           ENTRY(@s100),AT(70,37,278,10),USE(Fld:ExternalName),LEFT(2)
                           PROMPT('Derived From:'),AT(6,50),USE(?Fld:DerivedFrom:Prompt),TRN
                           ENTRY(@s50),AT(70,50,278,10),USE(Fld:DerivedFrom),LEFT(2)
                           PROMPT('Description:'),AT(6,64),USE(?Fld:Description:Prompt),TRN
                           ENTRY(@s100),AT(70,64,278,10),USE(Fld:Description),LEFT(2)
                           PROMPT('Data Type:'),AT(6,78),USE(?DataTypePrompt),TRN
                           LIST,AT(70,78,101,10),USE(Fld:DataType),LEFT(2),DROP(20),FORMAT('40L(2)|M~Type~@s10@'),FROM(Queue:FileDrop)
                           PROMPT('Base Type:'),AT(6,92),USE(?Fld:BaseType:Prompt),TRN
                           ENTRY(@s20),AT(70,92,84,10),USE(Fld:BaseType),LEFT(2)
                           PROMPT('Reference:'),AT(6,105),USE(?Fld:Reference:Prompt),TRN
                           ENTRY(@s5),AT(70,105,40,10),USE(Fld:Reference),LEFT(2)
                           PROMPT('Field Size:'),AT(6,120),USE(?Fld:FieldSize:Prompt),TRN
                           ENTRY(@s20),AT(70,120,84,10),USE(Fld:FieldSize),LEFT(2),DISABLE
                           PROMPT('Places:'),AT(162,120),USE(?Fld:Places:Prompt),DISABLE,TRN
                           ENTRY(@s3),AT(194,120,40,10),USE(Fld:Places),LEFT(2),DISABLE
                           PROMPT('Dimension:'),AT(6,134,43),USE(?PROMPT1),TRN
                           SPIN(@s3),AT(70,134,25,10),USE(Fld:Dim1),LEFT(2)
                           SPIN(@s3),AT(99,134,25,10),USE(Fld:Dim2),LEFT(2)
                           SPIN(@s3),AT(129,134,25,10),USE(Fld:Dim3),LEFT(2)
                           SPIN(@s3),AT(159,134,25,10),USE(Fld:Dim4),LEFT(2)
                           PROMPT('Row Picture:'),AT(6,148),USE(?Fld:RowPicture:Prompt),TRN
                           ENTRY(@s20),AT(70,148,60,10),USE(Fld:RowPicture),LEFT(2)
                           PROMPT('Screen Picture:'),AT(6,163),USE(?Fld:ScreenPicture:Prompt),TRN
                           ENTRY(@s20),AT(70,162,84,10),USE(Fld:ScreenPicture),LEFT(2)
                           PROMPT('Screen Prompt:'),AT(6,177),USE(?Fld:ScreenPrompt:Prompt),TRN
                           ENTRY(@s100),AT(70,176,278,10),USE(Fld:ScreenPrompt),LEFT(2)
                           PROMPT('Report Heading:'),AT(6,191),USE(?Fld:ReportHeading:Prompt),TRN
                           ENTRY(@s100),AT(70,191,278,10),USE(Fld:ReportHeading),LEFT(2)
                           PROMPT('Freeze:'),AT(6,205,24,10),USE(?Fld:Freeze:Prompt),TRN
                           ENTRY(@s5),AT(70,205,24,10),USE(Fld:Freeze)
                         END
                         TAB('Attributes'),USE(?AttributesTab)
                           OPTION('Case'),AT(7,18,132,55),USE(Fld:Case_),BOXED,TRN
                             RADIO(' Normal'),AT(14,30),USE(?OPTION1:RADIO1),TRN
                             RADIO(' Word Capitalized'),AT(14,42,77,10),USE(?OPTION1:RADIO2),TRN,VALUE('CAPS')
                             RADIO(' Uppercase'),AT(14,54,77,10),USE(?OPTION1:RADIO3),TRN,VALUE('CAPS')
                           END
                           OPTION('Typing Mode'),AT(143,18,79,55),USE(Fld:TypingMode),BOXED,TRN
                             RADIO(' Set Insert'),AT(149,30),USE(?OPTION1:RADIO4),TRN,VALUE('INSERT')
                             RADIO(' Set Overwrite'),AT(149,42,65,10),USE(?OPTION1:RADIO2:2),TRN,VALUE('OVERWRITE')
                             RADIO(' Do Not Reset'),AT(149,54,65,10),USE(?OPTION1:RADIO2:3),TRN,VALUE(' ')
                           END
                           GROUP('Flags'),AT(225,18,101,55),USE(?GROUP1),BOXED,TRN
                             CHECK(' Immediate'),AT(233,30),USE(Fld:Immediate),TRN,VALUE('true','false')
                             CHECK(' Password'),AT(233,42),USE(Fld:Password_),TRN,VALUE('true','false')
                             CHECK(' Read Only'),AT(233,54),USE(Fld:ReadOnly),TRN,VALUE('true','false')
                           END
                           CHECK(' Thread'),AT(7,131),USE(Fld:Thread),TRN,VALUE('true','false')
                           CHECK(' Binary'),AT(98,131),USE(Fld:Binary),TRN,VALUE('true','false')
                           ENTRY(@s20),AT(72,76,84,10),USE(Fld:Justification),LEFT(2)
                           PROMPT('Initial Value:'),AT(7,117),USE(?Fld:InitialValue:Prompt),TRN
                           ENTRY(@s100),AT(72,102,278,10),USE(Fld:Over),LEFT(2)
                           PROMPT('Over:'),AT(7,102),USE(?Fld:Over:Prompt),TRN
                           ENTRY(@s20),AT(72,90,84,10),USE(Fld:Offset),LEFT(2)
                           PROMPT('Offset:'),AT(7,89),USE(?Fld:Offset:Prompt),TRN
                           PROMPT('Justification:'),AT(7,76),USE(?Fld:Justification:Prompt),TRN
                           ENTRY(@s100),AT(72,116,278,10),USE(Fld:InitialValue),LEFT(2)
                         END
                         TAB('Comments'),USE(?CommentsTab)
                           BUTTON,AT(33,20,18,15),USE(?EditCommentBtn),ICON('Edit3.ico'),DISABLE,FLAT
                           BUTTON,AT(12,20,18,15),USE(?AddCommentBtn),ICON('AddNew.ico'),FLAT
                           BUTTON,AT(54,20,18,15),USE(?DeleteCommentBtn),ICON('GarbageClosed.ico'),DISABLE,FLAT
                           LIST,AT(10,40,432,113),USE(?CommentsList),FORMAT('44L(2)M~Create Date~@s11@#5#108L(2)M~' & |
  'Create User~@s50@#8#1020L(2)M~Text~@s255@#2#')
                         END
                         TAB('Options'),USE(?OptionsTab)
                           CHECK(' Do Not Auto-Populate This Column'),AT(7,20),USE(?CHECK1),TRN,VALUE('true','false')
                           CHECK(' Add Extra Vertical Space Before Column Controls on Form Procedures'),AT(7,69),USE(Fld:VerticalSpace), |
  TRN,VALUE('true','false')
                           ENTRY(@s100),AT(57,48,379,10),USE(Fld:FormTab),LEFT(2)
                           PROMPT('Form Tab:'),AT(19,49),USE(?Fld:FormTab:Prompt),TRN
                           BUTTON,AT(29,88,18,15),USE(?EditOptionBtn),ICON('Edit3.ico'),DISABLE,FLAT
                           BUTTON,AT(7,88,18,15),USE(?AddOptionBtn),ICON('AddNew.ico'),FLAT
                           BUTTON,AT(50,88,18,15),USE(?DeleteOptionBtn),ICON('GarbageClosed.ico'),DISABLE,FLAT
                           LIST,AT(7,106,432,101),USE(?OptionsList),FORMAT('97L(2)M~Property~@s100@#2#62L(2)M~Prop' & |
  'erty Type~@s5@#3#400L(2)M~Property Value~@s100@#4#')
                         END
                         TAB('Help'),USE(?HelpTab)
                           ENTRY(@s100),AT(72,20,278,10),USE(Fld:HelpId),LEFT(2)
                           PROMPT('Help Id:'),AT(8,20),USE(?Fld:HelpId:Prompt),TRN
                           PROMPT('Message:'),AT(8,34),USE(?Fld:Message:Prompt),TRN
                           ENTRY(@s100),AT(72,34,278,10),USE(Fld:Message),LEFT(2)
                           ENTRY(@s150),AT(72,48,278,10),USE(Fld:Tooltip),LEFT(2)
                           PROMPT('Tooltip:'),AT(8,48),USE(?Fld:Tooltip:Prompt),TRN
                         END
                         TAB('Validity Checks'),USE(?ValidityChecksTab)
                           OPTION,AT(8,18,431,190),USE(Check),TRN
                             RADIO(' No Checks'),AT(11,23),USE(?CHECK:RADIO1),TRN,VALUE('NOCHECKS')
                             RADIO(' Cannot be Zero or Blank'),AT(11,67),USE(?CHECK:RADIO2),TRN,VALUE('NONZERO')
                             RADIO(' Must be in Numeric Range'),AT(11,77),USE(?CHECK:RADIO3),TRN,VALUE('INRANGE')
                             RADIO(' Must be True or False'),AT(11,104),USE(?CHECK:RADIO4),TRN,VALUE('BOOLEAN')
                             RADIO(' Must be in Table'),AT(11,131),USE(?CHECK:RADIO5),TRN,VALUE('INFILE')
                             RADIO(' Must be in List'),AT(11,143),USE(?CHECK:RADIO6),TRN,VALUE('INLIST')
                           END
                           ENTRY(@s100),AT(86,35,341,10),USE(Choices),LEFT(2)
                           PROMPT('Choices:'),AT(33,36),USE(?Choices:Prompt),TRN
                           PROMPT('Values:'),AT(33,50),USE(?Values:Prompt),TRN
                           ENTRY(@s100),AT(86,50,341,10),USE(Values),LEFT(2)
                           ENTRY(@s20),AT(86,91,136,10),USE(RangeHigh),LEFT(2)
                           PROMPT('Range High:'),AT(33,91),USE(?RangeHigh:Prompt),TRN
                           ENTRY(@s100),AT(86,118,136,10),USE(TrueValue),LEFT(2)
                           PROMPT('True Value:'),AT(36,119),USE(?TrueValue:Prompt),TRN
                           ENTRY(@s100),AT(279,118,136,10),USE(FalseValue),LEFT(2)
                           PROMPT('False Value:'),AT(229,119),USE(?FalseValue:Prompt)
                           ENTRY(@s100),AT(86,156,341,10),USE(Choices,,?Choices:2),LEFT(2)
                           PROMPT('Choices:'),AT(33,156),USE(?Choices:Prompt:2),TRN
                           ENTRY(@s100),AT(86,169,341,10),USE(Values,,?Values:2),LEFT(2)
                           PROMPT('Values:'),AT(33,170),USE(?Values:Prompt:2),TRN
                         END
                         TAB('Controls'),USE(?ControlsTab)
                           ENTRY(@s255),AT(73,21,278,10),USE(Fld:WindowControl),LEFT(2)
                           PROMPT('Window Control:'),AT(9,21),USE(?Fld:WindowControl:Prompt),TRN
                           PROMPT('Report Control:'),AT(9,36),USE(?Fld:ReportControl:Prompt),TRN
                           ENTRY(@s255),AT(73,36,278,10),USE(Fld:ReportControl),LEFT(2)
                           SHEET,AT(9,50,401,81),USE(?ControlsSheet)
                             TAB('Windows'),USE(?ControlsSheetWindowsTab)
                               LIST,AT(15,66,389,60),USE(?LIST1),FORMAT('1020L(2)|M@s255@'),FROM(WindowControl)
                             END
                             TAB('Report'),USE(?ControlsSheetReportTab)
                               LIST,AT(15,66,389,60),USE(?RepCtrlList),FORMAT('1020L(2)M@s255@'),FROM(ReportControl)
                             END
                           END
                           OLE,AT(8,148,402,152),USE(?PropertyGrid)
                           END
                           BUTTON('Button1'),AT(362,26),USE(?BUTTON1)
                           PROMPT('Control Types:'),AT(8,134,53,10),USE(?CtrlTypesPrompt),TRN
                           LIST,AT(65,133,87,10),USE(Fld:CtrlType),DROP(6),FORMAT('24L(2)M@s6@'),FROM(ControlTypesQ)
                         END
                       END
                       BUTTON,AT(397,1,23,12),USE(?OK),LEFT,ICON('Save1.ico'),DISABLE,FLAT,HIDE,MSG('Accept dat' & |
  'a and close the window'),TIP('Accept data and close the window')
                       BUTTON,AT(423,1,23,12),USE(?Cancel:3),LEFT,ICON('Close.ico'),FLAT,HIDE,MSG('Cancel operation'), |
  TIP('Cancel operation')
                       IMAGE,AT(529,66),USE(?Draw),HIDE
                     END

    omit('***',WE::CantCloseNowSetHereDone=1)  !Getting Nested omit compile error, then uncheck the "Check for duplicate CantCloseNowSetHere variable declaration" in the WinEvent local template
WE::CantCloseNowSetHereDone equate(1)
WE::CantCloseNowSetHere     long
    !***
ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
ChangeAction           PROCEDURE(),BYTE,DERIVED
DeleteAction           PROCEDURE(),BYTE,DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
InsertAction           PROCEDURE(),BYTE,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
OnCloseEventCancelled  PROCEDURE(),DERIVED
Open                   PROCEDURE(),DERIVED
Open                   PROCEDURE(*Window pWindow,<*Window pOwner>),DERIVED
PrimeFields            PROCEDURE(),PROC,DERIVED
PrimeUpdate            PROCEDURE(),BYTE,PROC,DERIVED
RestoreField           PROCEDURE(SIGNED Control),DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
SaveOnChangeAction     PROCEDURE(),BYTE,DERIVED
SaveOnInsertAction     PROCEDURE(),BYTE,DERIVED
SetAlerts              PROCEDURE(),DERIVED
SetResponse            PROCEDURE(BYTE Response),DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeCloseEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeCompleted          PROCEDURE(),BYTE,PROC,DERIVED
TakeDisableButton      PROCEDURE(SIGNED Control,BYTE MakeDisable),DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeNewSelection       PROCEDURE(),BYTE,PROC,DERIVED
TakeNotify             PROCEDURE(UNSIGNED NotifyCode,SIGNED Thread,LONG Parameter),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
Update                 PROCEDURE(),DERIVED
                     END

Toolbar              ToolbarClass
mhResize             CLASS(MH::ResizeWindowClass)
Init                   PROCEDURE()
InitialResize          PROCEDURE()                         ! New method added to this class instance
                     END

UpdateField:mhResize:WM CLASS,IMPLEMENTS(MH::ResizeIWindowManager)
                     END
FDB4                 CLASS(FileDropClass)                  ! File drop manager
Q                      &Queue:FileDrop                !Reference to display queue
                     END

! ----- Drawer --------------------------------------------------------------------------
Drawer               Class(DrawClass)
                     End  ! Drawer
! ----- end Drawer -----------------------------------------------------------------------
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
                    END

  CODE
? DEBUGHOOK(DataTypesLkUp:Record)
? DEBUGHOOK(FieldDefaultsLkUp:Record)
? DEBUGHOOK(Fields:Record)
    ParamsGrp           =  pParams
    ThisWindow.Request  =  ParamsGrp.RecordAction
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

    
!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Ask PROCEDURE

  CODE
  if ~DonutHoleWindow.GetIsConnecting()  
  MyDCT.Trace('[UpdateField][ThisWindow.Ask]')
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    ActionMessage = 'Record Will Be Added'
  OF ChangeRecord
    ActionMessage = 'Record Will Be Changed'
  END
  QuickWindow{PROP:Text} = ActionMessage                   ! Display status message in title bar
  End  
  PARENT.Ask


ThisWindow.ChangeAction PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.ChangeAction]')
  ReturnValue = PARENT.ChangeAction()
  RETURN ReturnValue


ThisWindow.DeleteAction PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.DeleteAction]')
  ReturnValue = PARENT.DeleteAction()
  RETURN ReturnValue


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !---- Noyantis : Codejock Property Grid Wrapper - Start ----
  PropertyGrid6.InitHelper(TemplateHelper)
  !---- Noyantis : Codejock Property Grid Wrapper - End ----
  !OverStr = pParams
    !ParamsGrp = pParams
  GlobalErrors.SetProcedureName('UpdateField')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Fld:FieldName:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
    MyDct.Trace('')
    MyDct.Trace('UpdateField')
    MyDct.Trace('   ParamsGrp.Action         = ' & ParamsGrp.Action) 
    MyDct.Trace('   ParamsGrp.RecordAction   = ' & ParamsGrp.RecordAction) 
    MyDct.Trace('   ParamsGrp.DictionaryGuid = ' & ParamsGrp.DictionaryGuid)
    MyDct.Trace('   ParamsGrp.TableGuid      = ' & ParamsGrp.TableGuid)
    MyDct.Trace('   ParamsGrp.GlobalRequest  = ' & ParamsGrp.GlobalRequest) 
    MyDct.Trace('   ParamsGrp.Usage          = ' & ParamsGrp.Usage)
    MyDct.Trace('   ParamsGrp.Order          = ' & ParamsGrp.Order)
    MyDct.Trace('   SELF.Request             = ' & SELF.Request)
    MyDct.Trace('')    
    
    FieldPropertiesGroup.GeneralGrp.Justification        =  'LEFT'
    FieldPropertiesGroup.ColorGrp.Background             =  COLOR:Blue
    FieldPropertiesGroup.DesignGrp.SuppressTransparency  =  FALSE
    FieldPropertiesGroup.PositionGrp.DefaultHeight       =  TRUE
    FieldPropertiesGroup.PositionGrp.DefaultWidth        =  TRUE
    json.Start()
    json.SetTagCase(jF:CaseAsIs)
    json.Save(FieldPropertiesGroup,st,'FieldProperties',True,False,True)
  
    MyDct.Trace('')    
    MyDct.Trace(st.GetValue())    
    MyDct.Trace('')    
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
    NotifyManager.AddProcedure('UpdateField',pThread)
    NotifyHandler.AddNotifyCode(1)
  SELF.HistoryKey = CtrlH
  SELF.AddHistoryFile(Fld:Record,History::Fld:Record)
  SELF.AddHistoryField(?Fld:FieldName,8)
  SELF.AddHistoryField(?Fld:ExternalName,38)
  SELF.AddHistoryField(?Fld:DerivedFrom,10)
  SELF.AddHistoryField(?Fld:Description,11)
  SELF.AddHistoryField(?Fld:DataType,13)
  SELF.AddHistoryField(?Fld:BaseType,12)
  SELF.AddHistoryField(?Fld:Reference,15)
  SELF.AddHistoryField(?Fld:FieldSize,18)
  SELF.AddHistoryField(?Fld:Places,23)
  SELF.AddHistoryField(?Fld:Dim1,19)
  SELF.AddHistoryField(?Fld:Dim2,20)
  SELF.AddHistoryField(?Fld:Dim3,21)
  SELF.AddHistoryField(?Fld:Dim4,22)
  SELF.AddHistoryField(?Fld:RowPicture,14)
  SELF.AddHistoryField(?Fld:ScreenPicture,24)
  SELF.AddHistoryField(?Fld:ScreenPrompt,25)
  SELF.AddHistoryField(?Fld:ReportHeading,26)
  SELF.AddHistoryField(?Fld:Freeze,27)
  SELF.AddHistoryField(?Fld:Case_,30)
  SELF.AddHistoryField(?Fld:TypingMode,31)
  SELF.AddHistoryField(?Fld:Immediate,34)
  SELF.AddHistoryField(?Fld:Password_,36)
  SELF.AddHistoryField(?Fld:ReadOnly,35)
  SELF.AddHistoryField(?Fld:Thread,16)
  SELF.AddHistoryField(?Fld:Binary,17)
  SELF.AddHistoryField(?Fld:Justification,33)
  SELF.AddHistoryField(?Fld:Over,32)
  SELF.AddHistoryField(?Fld:Offset,37)
  SELF.AddHistoryField(?Fld:InitialValue,28)
  SELF.AddHistoryField(?Fld:VerticalSpace,40)
  SELF.AddHistoryField(?Fld:FormTab,29)
  SELF.AddHistoryField(?Fld:HelpId,41)
  SELF.AddHistoryField(?Fld:Message,42)
  SELF.AddHistoryField(?Fld:Tooltip,43)
  SELF.AddHistoryField(?Fld:WindowControl,44)
  SELF.AddHistoryField(?Fld:ReportControl,45)
  SELF.AddHistoryField(?Fld:CtrlType,54)
  SELF.AddUpdateFile(Access:Fields)
  SELF.AddItem(?Cancel:3,RequestCancelled)                 ! Add the cancel control to the window manager
  Relate:DataTypesLkUp.Open()                              ! File DataTypesLkUp used by this procedure, so make sure it's RelationManager is open
  Relate:FieldDefaultsLkUp.Open()                          ! File FieldDefaultsLkUp used by this procedure, so make sure it's RelationManager is open
  Relate:Fields.Open()                                     ! File Fields used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Fields
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.CancelAction = Cancel:Cancel+Cancel:Query         ! Confirm cancel
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  	!Fld:PKGuid = ParamsGrp.FieldGuid 
  	!Access:Fields.Fetch(Fld:PKFldGuidKey)  
    
  
  SELF.Open(QuickWindow)                                   ! Open window
  MyDCT.Trace('[UpdateField][ThisWindow.Init]')
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('UpdateField', THREAD(), QuickWindow)
  !---- Noyantis : Template Helper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.InitAll(TemplateHelper, nysInit_PrepareNoOCXCtrl)
  NYS:DockingPane_EventMgr.MakeTarget       = UPPER('DictionaryEditor.EXE')
  NYS:DockingPane_EventMgr.DisableAtRuntime = FALSE
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  ?CurrentTab{PROP:TabSheetStyle} = TabStyle:BlackAndWhite
  ?ControlsSheet{PROP:TabSheetStyle} = TabStyle:BlackAndWhite
    QuickWindow{PROP:Buffer} = 1    
    QuickWindow{PROP:Buffer} = 1 ! Remove flicker when animating.
    Drawer.Init(?Draw)
    Free(fonts)
    Drawer.EnumFontFamilies()
    loop i = 1 to Records(Drawer.gFontDispQ)
        Get(Drawer.gFontDispQ, i)
        fonts = Drawer.gFontDispQ
        Add(fonts)
    end      
  
    
  !---- Noyantis : Codejock Property Grid Wrapper - Start ----
  PropertyGrid6.InitPrepare(?PropertyGrid{PROP:FEQ})
  !---- Noyantis : Codejock Property Grid Wrapper - End ----
  !---- Noyantis : Codejock Property Grid Wrapper - Start ----
  PropertyGrid6.Init()
  !---- Noyantis : Codejock Property Grid Wrapper - End ----
  Do DefineListboxStyle
  Alert(AltKeyPressed)  ! WinEvent : These keys cause a program to crash on Windows 7 and Windows 10.
  Alert(F10Key)         !
  Alert(CtrlF10)        !
  Alert(ShiftF10)       !
  Alert(CtrlShiftF10)   !
  Alert(AltSpace)       !
  WinAlertMouseZoom()
  WinAlert(WE::WM_QueryEndSession,,Return1+PostUser)
    NotifyHandler.InitWindow()
  IF SELF.Request = ViewRecord                             ! Configure controls for View Only mode
    ?Fld:FieldName{PROP:ReadOnly} = True
    ?Fld:ExternalName{PROP:ReadOnly} = True
    ?Fld:DerivedFrom{PROP:ReadOnly} = True
    ?Fld:Description{PROP:ReadOnly} = True
    DISABLE(?Fld:DataType)
    ?Fld:BaseType{PROP:ReadOnly} = True
    ?Fld:Reference{PROP:ReadOnly} = True
    ?Fld:FieldSize{PROP:ReadOnly} = True
    ?Fld:Places{PROP:ReadOnly} = True
    ?Fld:RowPicture{PROP:ReadOnly} = True
    ?Fld:ScreenPicture{PROP:ReadOnly} = True
    ?Fld:ScreenPrompt{PROP:ReadOnly} = True
    ?Fld:ReportHeading{PROP:ReadOnly} = True
    ?Fld:Freeze{PROP:ReadOnly} = True
    ?Fld:Justification{PROP:ReadOnly} = True
    ?Fld:Over{PROP:ReadOnly} = True
    ?Fld:Offset{PROP:ReadOnly} = True
    ?Fld:InitialValue{PROP:ReadOnly} = True
    DISABLE(?EditCommentBtn)
    DISABLE(?AddCommentBtn)
    DISABLE(?DeleteCommentBtn)
    ?Fld:FormTab{PROP:ReadOnly} = True
    DISABLE(?EditOptionBtn)
    DISABLE(?AddOptionBtn)
    DISABLE(?DeleteOptionBtn)
    ?Fld:HelpId{PROP:ReadOnly} = True
    ?Fld:Message{PROP:ReadOnly} = True
    ?Fld:Tooltip{PROP:ReadOnly} = True
    ?Choices{PROP:ReadOnly} = True
    ?Values{PROP:ReadOnly} = True
    ?RangeHigh{PROP:ReadOnly} = True
    ?TrueValue{PROP:ReadOnly} = True
    ?FalseValue{PROP:ReadOnly} = True
    ?Choices:2{PROP:ReadOnly} = True
    ?Values:2{PROP:ReadOnly} = True
    ?Fld:WindowControl{PROP:ReadOnly} = True
    ?Fld:ReportControl{PROP:ReadOnly} = True
    DISABLE(?BUTTON1)
    DISABLE(?Fld:CtrlType)
  END
  mhResize.Init
  !---- Noyantis : Codejock Property Grid Wrapper - Start ----
  PropertyGrid6.InitResize()
  !---- Noyantis : Codejock Property Grid Wrapper - End ----
  FDB4.Init(?Fld:DataType,Queue:FileDrop.ViewPosition,FDB4::View:FileDrop,Queue:FileDrop,Relate:DataTypesLkUp,ThisWindow)
  FDB4.Q &= Queue:FileDrop
  FDB4.AddSortOrder()
  FDB4.AddField(Dat:Type,FDB4.Q.Dat:Type) !List box control field - type derived from field
  FDB4.AddField(Dat:GUID,FDB4.Q.Dat:GUID) !Primary key field - type derived from field
  FDB4.AddUpdateField(Dat:Type,Fld:DataType)
  ThisWindow.AddItem(FDB4.WindowComponent)
  FDB4.DefaultFill = 0
  !  Post(DctEvent:RefreshData)  
  DonutHoleWindow.SetLogPreamble('UpdateField')
  DonutHoleWindow.init(DonutHoleRegistrator.IDonutHoleRegistration,thread(),QuickWindow)
  SELF.AddItem(DonutHoleWindow.WindowComponent)
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
  !---- Noyantis : Codejock Property Grid Wrapper - Start ----
  PropertyGrid6.InitTemplateSettings()
  PropertyGrid6.InitComplete()
  !---- Noyantis : Codejock Property Grid Wrapper - End ----
    ?CommentsList{PROP:From} = MyDct.CommentsQ
    ?OptionsList{PROP:From}  = MyDct.OptionsQ  
    If ParamsGrp.GlobalRequest <> InsertRecord
        Post(DctEvent:FetchRecord)
    ELSE
        Clear(Fields)
        Fie:TypeName = 'STRING'
        If Access:FieldDefaultsLkUp.Fetch(Fie:TypeNameKey) = Level:Benign
            !Rec                     &= Fields{PROP:Record}
            Fld:PKGuid           =  Glo:st.MakeGuid()
            Fld:ParentGUID       =  ParamsGrp.TableGuid
            Fld:TableGuid        =  ParamsGrp.TableGuid
            Fld:FieldOrder       =  ParamsGrp.Order + 1
            Fld:Guid             =  MyDct.GetWindowsGuid()
            Fld:DataType         =  Fie:DataType
            Fld:FieldSize        =  Fie:Size_
            Fld:ScreenPicture    =  Fie:ScreenPicture
            Fld:FieldPrefix      =  ParamsGrp.TablePrefix
            Fld:IndentBy         =  2
            Fld:NumEndTags       =  1
            st.SetValue(Fie:WindowControl,True)
            st.ToBlob(Fld:WindowControlBlob)
            st.SetValue('<Audit CreateUser="Administrator" CreateDate=" ' & Format(Today(),'@D8') & '" CreateTime="' & Format(Clock(),'@T6') & '" CreateVersionNumber="1" ModifiedUser="Administrator" ModifiedDate=" '& Format(Today(),'@D8') & '" ModifiedTime="'& Format(Clock(),'@T6') & '" ModifiedVersionNumber="1"/>')    
            st.ToBlob(Fld:AuditBlob)
            st.SetValue('<Validity Check="NOCHECKS"/>')
            st.ToBlob(Fld:ValidityBlob)
            ParamsGrp.FieldGuid  =  Fld:Guid
            !Rec = FieldGrp
            Access:Fields.TryInsert()
        End
        MyDct.SetCurrentRecordHash(Fields)
        !Post(DctEvent:FetchRecord)
    End          
  !ParamsGrp.TablePrefix
  
    !local.PopulateControlTypesQ()
  RETURN ReturnValue


ThisWindow.InsertAction PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.InsertAction]')
  ReturnValue = PARENT.InsertAction()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
    SELF.Primary.Me.RestoreBuffer(self.Saved, False)  ! The RestoreBuffer method releases memory allocated by the SaveBuffer,  
                                                        ! value of zero (0 or False) does not update the file's Buffer  
  MyDCT.Trace('[UpdateField][ThisWindow.Kill]')
  Drawer.Kill()
   
  !---- Noyantis : Codejock Property Grid Wrapper - Start ----
  IF PropertyGrid6.KillProcessStarted = FALSE
    PropertyGrid6.Kill()
    PropertyGrid6.KillComplete()
  END
  !---- Noyantis : Codejock Property Grid Wrapper - End ----
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('UpdateField', THREAD())
  !---- Noyantis : Template Helper - End ----
  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:DataTypesLkUp.Close()
    Relate:FieldDefaultsLkUp.Close()
    Relate:Fields.Close()
  END
    NotifyManager.DeleteProcedure('UpdateField',pThread)
  mhResize.Kill
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.Kill()
  NYS:DockingPane_EventMgr.KillComplete()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.OnCloseEventCancelled PROCEDURE

  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.OnCloseEventCancelled]')
  PARENT.OnCloseEventCancelled


ThisWindow.Open PROCEDURE

  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.Open 1]')
  PARENT.Open


ThisWindow.Open PROCEDURE(*Window pWindow,<*Window pOwner>)

  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.Open 2]')
  PARENT.Open(pWindow,pOwner)


ThisWindow.PrimeFields PROCEDURE

  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.PrimeFields]')
  Fld:IndentBy = 2
  Fld:NumEndTags = 1
  !local.SetWinCtrlBlob()
  PARENT.PrimeFields


ThisWindow.PrimeUpdate PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.PrimeUpdate]')
  ReturnValue = PARENT.PrimeUpdate()
  RETURN ReturnValue


ThisWindow.RestoreField PROCEDURE(SIGNED Control)

  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.RestoreField]')
  PARENT.RestoreField(Control)


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.Run]')
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.Run 2]')
  ReturnValue = PARENT.Run(Number,Request)
  RETURN ReturnValue


ThisWindow.SaveOnChangeAction PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.SaveOnChangeAction]')
  ReturnValue = PARENT.SaveOnChangeAction()
  RETURN ReturnValue


ThisWindow.SaveOnInsertAction PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.SaveOnInsertAction]')
  ReturnValue = PARENT.SaveOnInsertAction()
  RETURN ReturnValue


ThisWindow.SetAlerts PROCEDURE

  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.SetAlerts]')
  PARENT.SetAlerts


ThisWindow.SetResponse PROCEDURE(BYTE Response)

  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.SetResponse]')
  PARENT.SetResponse(Response)


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Typeface   STRING(31) !Use CLASTRING in Clarion#
FontSize   SIGNED
FontColor  LONG
FontStyle  LONG
CharSet    BYTE
Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE ACCEPTED()
    OF ?CommentsList
      	Get(MyDct.CommentsQ,Choice(?CommentsList))
      	ParamsGrp.CommentJson = MyDct.CommentsQ.Text
      	If Keycode() = MouseLeft2
      		!MyDct.Trace('UpdateDictionary - Double Clicked ?CommentsList')  
      		SetKeycode(0)  
      		POST(EVENT:Accepted,?EditCommentBtn)
      	End	        
    OF ?OptionsList
      Get(MyDct.OptionsQ,Choice(?OptionsList))    
      ParamsGrp.OptionJson = MyDct.GetOptionJson(MyDct.OptionsQ)
      If Keycode() = MouseLeft2
        !MyDct.Trace('UpdateDictionary - Double Clicked ?OptionsList') 
        SetKeycode(0)  
        POST(EVENT:Accepted,?EditOptionBtn)
      End        
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?Fld:DataType
      !      Case Upper(Clip(Fld:DataType))
      !      Of 'BLOB'
      !      OrOf 'FILE'  
      !      OrOf 'KEY'
      !      OrOf 'REPORT'
      !      OrOf 'VIEW'
      !      OrOf 'WINDOW'
      !        HIDE(?ControlsTab)
      !      ELSE
      !        UNHIDE(?ControlsTab)
      !        local.PopulateControlTypesQ()
      !      End  
    OF ?EditCommentBtn
      ThisWindow.Update()
      ParamsGrp.Action = 'EditComment'
      ParamsGrp.ProcedureToNotify = 'UpdateField'
      ParamsGrp.CommentJson = Clip(MyDct.CommentsQ.Text)
      START(UpdateComment, 25000, OverStr,Thread())
      ThisWindow.Reset        
    OF ?AddCommentBtn
      ThisWindow.Update()
      ParamsGrp.Action = 'InsertComment'
      ParamsGrp.ProcedureToNotify = 'UpdateField'
      START(UpdateComment, 25000, OverStr,Thread())
      ThisWindow.Reset        
    OF ?DeleteCommentBtn
      ThisWindow.Update()
      Case MESSAGE('Are you sure you want to delete the highlighted record?','Confirm Delete',ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:No,MSGMODE:SYSMODAL)
      Of BUTTON:NO
        !RETURN
      Of BUTTON:YES  
        MyDct.DeleteCommentQRec(MyDct.CommentsQ,Fields,'CommentsBlob')  
        ?OK{PROP:Disable} = False    
        Post(DctEvent:RefreshData)
      End      
    OF ?EditOptionBtn
      ThisWindow.Update()
      ParamsGrp.Action = 'EditOption'
      ParamsGrp.ProcedureToNotify = 'UpdateField'
      START(UpdateOption, 25000, OverStr,Thread())
      ThisWindow.Reset         
    OF ?AddOptionBtn
      ThisWindow.Update()
      ParamsGrp.Action = 'InsertOption'
      ParamsGrp.ProcedureToNotify = 'UpdateField'
      START(UpdateOption, 25000, OverStr,Thread())
      ThisWindow.Reset         
    OF ?DeleteOptionBtn
      ThisWindow.Update()
      Case MESSAGE('Are you sure you want to delete the highlighted record?','Confirm Delete',ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:No,MSGMODE:SYSMODAL)
      Of BUTTON:NO
        !RETURN
      Of BUTTON:YES  
        MyDct.DeleteOptionQRec(MyDct.OptionsQ,Fields,'OptionsBlob')  
        ?OK{PROP:Disable} = False      
        Post(DctEvent:RefreshData)
      End      
    OF ?BUTTON1
      ThisWindow.Update()
      IF FONTDIALOGA('Choose Display Font',Typeface,FontSize,FontColor,FontStyle,CharSet,0)
      End
    OF ?Fld:CtrlType
      !Get(ControlTypesQ,Choice(?ListControlTypes))
      !Fld:CtrlType =  ControlTypesQ.CtrlType    
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeCloseEvent PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.TakeCloseEvent]')
  ReturnValue = PARENT.TakeCloseEvent()
  if ReturnValue = Level:Benign AND DonutHoleWindow.GetIsConnected()
    if SELF.Response = RequestCompleted
      !DonutHoleWindow.NotifyHost(not:RecordUpdated, CUS:CustNumber)  
    end!if
    !Access:Relations.RestoreBuffer(self.Saved)
    DonutHoleWindow.NotifyHost(not:closeWindow, thread())
    ReturnValue = Level:Benign
  end!if    
  RETURN ReturnValue


ThisWindow.TakeCompleted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.TakeCompleted]')
  LOOP
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    !If ~Access:Fields.EqualBuffer(self.saved)
    !If MyDct.CheckRecordHash(Fields) = DwrDct:RecordChanged
        !Access:Fields.TryUpdate()
        !self.Saved = Access:Fields.SaveBuffer()
    !End     
  ReturnValue = PARENT.TakeCompleted()
    Case ReturnValue  
    Of Level:Benign
        MyDct.Trace('')
        MyDct.Trace('UpdateField - ThisWindow.TakeCompleted')
        MyDct.Trace('   ParamsGrp.Action = ' & ParamsGrp.Action) 
        MyDct.Trace('   ParamsGrp.DictionaryGuid = ' & ParamsGrp.DictionaryGuid)
        MyDct.Trace('   ParamsGrp.TableGuid = ' & ParamsGrp.TableGuid)
        MyDct.Trace('   ParamsGrp.GlobalRequest = ' & ParamsGrp.GlobalRequest) 
        MyDct.Trace('   ParamsGrp.Usage = ' & ParamsGrp.Usage)
        MyDct.Trace('')    
  
        If Fld:PKGuid = ''
            Rec &= Fields{PROP:Record}
            FieldGrp             =  Rec
            FieldGrp.PKGuid      =  glo:st.MakeGuid()
            FieldGrp.ParentGUID  =  ParamsGrp.TableGuid
            FieldGrp.Guid        =  MyDct.GetWindowsGuid()
            FieldGrp.FieldOrder  =  ParamsGrp.Order + 1 !Records(Fields)+1
            MyDct.InsertField(Fields,FldOverStr)      
        ELSE
            Access:Fields.TryUpdate()
        End
        
        MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateField','RecordSaved',OverStr)
        MyDct.SetCurrentRecordHash(Fields)
        Post(DctEvent:RefreshData)
        ?OK{PROP:Disable} = True
    Of Level:Notify
    Of Level:Fatal
    End    
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeDisableButton PROCEDURE(SIGNED Control,BYTE MakeDisable)

  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.TakeDisableButton]')
  PARENT.TakeDisableButton(Control,MakeDisable)


ThisWindow.TakeEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  mhResize.InitialResize
  ReturnValue = mhResize.TakeEvent()
  IF ReturnValue <> LEVEL:Benign
    RETURN ReturnValue
  END
  LOOP                                                     ! This method receives all events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    Case EVENT()
    Of EVENT:CloseWindow
        MyDct.Trace('UpdateField - EVENT:CloseWindow   <<<<<<<<<<<<<<<<<<---------------------------------------------------------------------------') ! - EqualBuffer = ' & Access:Relations.EqualBuffer(self.Saved))  
    Of event:DonutHoleConnected
        MyDct.Trace('UpdateField - event:DonutHoleConnected')  
        !Post(DctEvent:FetchRecord)
    Of event:DonutHoleUnhide
        MyDct.Trace('UpdateField - event:DonutHoleUnhide - PkGuid = ' & Rel:PKGuid & '   <<<<<<<<<<<<<<<<<<---------------------------------------------------------------------------')  
    Of event:DonutHoleDisconnected
        MyDct.Trace('UpdateField - event:DonutHoleDisconnected')
        !local.SetWinCtrlBlob()
        !If self.Saved  
            !If Not Access:Fields.EqualBuffer(self.Saved)
        !IF self.Saved And NOT SELF.Primary.Me.EqualBuffer(self.Saved) ! did something change?
        !    MyDct.Trace('-------------- Record Changed ----------------------------')
        !    Case MESSAGE('The item has changed. Do you want to save changes?','Dictionary Editor',ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:No,MSGMODE:SYSMODAL)
        !    Of BUTTON:NO
        !    Of BUTTON:YES
        !        Access:Fields.TryUpdate()
        !        !self.Saved = Access:Fields.SaveBuffer()
        !        MyDct.SetCurrentRecordHash(Fields)
        !        MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','RecordSaved',OverStr)
        !    End  
        !END
  
  
  
            !If MyDct.CheckRecordHash(Fields) = DwrDct:RecordChanged
            !    Case MESSAGE('The item has changed. Do you want to save changes?','Dictionary Editor',ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:No,MSGMODE:SYSMODAL)
            !    Of BUTTON:NO
            !    Of BUTTON:YES
            !        Access:Fields.TryUpdate()
            !        !self.Saved = Access:Fields.SaveBuffer()
            !        MyDct.SetCurrentRecordHash(Fields)
            !        MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','RecordSaved',OverStr)
            !    End       
            !End 
            !MyDct.Trace('UpdateField - event:DonutHoleDisconnected - Before RestoreBuffer')
            !Access:Fields.RestoreBuffer(self.Saved)
        !End  
    Of event:DonutHoleHide
        !MyDct.Trace('')
        MyDct.Trace('UpdateField - event:DonutHoleHide   <<<<<<<<<<<<<<<<<<---------------------------------------------------------------------------') 
        !MyDct.Trace('')
  
    Of EVENT:Selected
        !IF NOT SELF.Primary.Me.EqualBuffer(self.Saved) ! did something change?
        If MyDct.CheckRecordHash(Fields) = DwrDct:RecordChanged
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateField','SaveNeeded',OverStr)
            !Glo:SaveBtnFeq{PROP:Disable} = False  
        End
    Of EVENT:Accepted
        MyDct.Trace('UpdateField - EVENT:Accepted   <<<<<<<<<<<<<<<<<<---------------------------------------------------------------------------')
        local.SetScreenPicture()
        !If self.Saved
        !    If Not Access:Fields.EqualBuffer(self.Saved)
        !        ?OK{PROP:Disable} = False 
        !    End 
        !End
        !If ParamsGrp.GlobalRequest = InsertRecord
        !END
        Case Upper(Clip(Fld:CtrlType))
        !Of 'CHECK'
        !OrOf 'IMAGE'
        Of 'ENTRY'
        OrOf 'SPIN'
        OrOf 'STRING'
        OrOf 'TEXT'
            st.SetValue(MyDct.XmlGetWindowControl(Fld:CtrlType,ParamsGrp.TablePrefix,Fld:FieldName,Fld:ScreenPicture))
            st.ToBlob(Fld:WindowControlBlob)
            st.Start()
            !local.PopulateControls()
        ELSE
            st.SetValue('')
            st.SetValue(MyDct.XmlGetWindowControl('ENTRY',ParamsGrp.TablePrefix,Fld:FieldName,Fld:ScreenPicture))
            st.ToBlob(Fld:WindowControlBlob)
            st.Start()
        End
  
        If MyDct.CheckRecordHash(Fields) = DwrDct:RecordChanged
        !IF NOT SELF.Primary.Me.EqualBuffer(self.Saved) ! did something change?
            MyDct.Trace('-------------- Record Changed EVENT:Accepted ----------------------------')
  
  
  
            !st.FromBlob(Fld:WindowControlBlob)
            !If st.FindChars('#PRE#')
            !    st.Replace('#PRE#',clip(Fld:FieldPrefix))
            !End
            !If st.FindChars('#LABEL#')
            !    st.Replace('#LABEL#',clip(Fld:FieldName))
            !End
            !If st.FindChars('#SPLITLABEL#')
            !    st.Replace('#SPLITLABEL#',MyDct.SplitCapitalWords(Fld:FieldName))
            !End
            !st.ToBlob(Fld:WindowControlBlob)
  
  
  
  
        !    Case MESSAGE('The item has changed. Do you want to save changes?','Dictionary Editor',ICON:Question,BUTTON:Yes+BUTTON:No,BUTTON:No,MSGMODE:SYSMODAL)
        !    Of BUTTON:NO
        !    Of BUTTON:YES
                Access:Fields.TryUpdate()
                !self.Saved = Access:Fields.SaveBuffer()
                MyDct.SetCurrentRecordHash(Fields)
                MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateDictionary','SaveNeeded',OverStr)
        !    End  
        END
  
        Post(DctEvent:RefreshData)
  
    Of EVENT:NewSelection   !Post(DctEvent:RefreshControls)
    Of EVENT:GainFocus
        MyDct.Trace('UpdateField - EVENT:GainFocus   <<<<<<<<<<<<<<<<<<---------------------------------------------------------------------------')    
        !Access:Fields.TryUpdate()    
    Of DctEvent:FetchRecord
        !MyDct.Trace('')
        MyDct.Trace('UpdateField - EVENT DctEvent:FetchRecord   <<<<<<<<<<<<<<<<<<---------------------------------------------------------------------------')
        !MyDct.Trace('   ParamsGrp.DictionaryGuid = ' & ParamsGrp.DictionaryGuid)
        !MyDct.Trace('   ParamsGrp.TableGuid = ' & ParamsGrp.TableGuid)
        !MyDct.Trace('   ParamsGrp.FieldGuid = ' & ParamsGrp.FieldGuid)
        !MyDct.Trace('')
        Clear(Fld:Record)
  		Fld:PKGuid = ParamsGrp.FieldGuid
  		If Access:Fields.Fetch(Fld:PKFldGuidKey) = Level:Benign
            !local.SetWinCtrlBlob(Fld:ScreenPrompt,Fld:FieldName)
            !self.Saved = SELF.Primary.Me.SaveBuffer()
            !self.Saved = Access:Fields.SaveBuffer()
            MyDct.SetCurrentRecordHash(Fields)
            Post(DctEvent:RefreshData)
        ELSE
            MyDct.Trace('[UpdateField][ThisWindow.TakeEvent][DctEvent:FetchRecord][Error FETCHing Fields record!' & Error() & ']')
        End
    Of DctEvent:RefreshData
        MyDct.Trace('UpdateField - DctEvent:RefreshData   <<<<<<<<<<<<<<<<<<---------------------------------------------------------------------------')
  
        Fld:ReportControl = Clip(Fld:DataType) & '(' & Clip(Fld:ScreenPicture) & ',USE(' & Clip(ParamsGrp.TablePrefix) & ':' & Clip(Fld:FieldName) & '),' & Clip(Fld:Justification) & '(' & Clip(Fld:Offset) & ')'
  
        st.SetValue('<Validity Check="NOCHECKS"/>')
        st.ToBlob(Fld:ValidityBlob)
        st.Start()
  
        local.PopulateControls()
        local.PopulateCommentsQ()
        local.PopulateOptionsQ()
        local.PopulateValidityCheck()
  
        If Records(MyDct.CommentsQ) <> CommentRecsCnt
            CommentRecsCnt = Records(MyDct.CommentsQ)
            !MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateField','SaveNeeded',OverStr)
        END
        If Records(MyDct.OptionsQ) <> OptionRecCnt
            OptionRecCnt = Records(MyDct.OptionsQ)
            !MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateField','SaveNeeded',OverStr)
        End       
        Post(DctEvent:RefreshControls)
    Of DctEvent:RefreshControls
        MyDct.Trace('UpdateField - EVENT DctEvent:RefreshControls   <<<<<<<<<<<<<<<<<<---------------------------------------------------------------------------')
  
            Case Upper(Clip(Fld:DataType))
            Of 'BLOB'
            OrOf 'FILE'  
            OrOf 'KEY'
            OrOf 'REPORT'
            OrOf 'VIEW'
            OrOf 'WINDOW'
              HIDE(?ControlsTab)
            ELSE
              UNHIDE(?ControlsTab)
              local.PopulateControlTypesQ()
            End   
  
        If Records(MyDct.CommentsQ)
            ?EditCommentBtn{PROP:Disable} = FALSE
            ?DeleteCommentBtn{PROP:Disable} = FALSE
        ELSE
            ?EditCommentBtn{PROP:Disable} = True
            ?DeleteCommentBtn{PROP:Disable} = True
        End
        If Records(MyDct.OptionsQ)
            ?EditOptionBtn{PROP:Disable} = False
            ?DeleteOptionBtn{PROP:Disable} = FALSE
        ELSE
            ?EditOptionBtn{PROP:Disable} = True
            ?DeleteOptionBtn{PROP:Disable} = True
        End
        local.SetFieldSize()
  
        Display(?EditCommentBtn)
        Display(?EditOptionBtn)
        Display(?DeleteOptionBtn)
        Display(?DeleteCommentBtn)
  
        !ControlTypesQ.CtrlType = Fld:CtrlType
        !Get(ControlTypesQ,ControlTypesQ.CtrlType)
        !?ListControlTypes{PROPLIST:MouseDownRow} = ControlTypesQ.RowID
        !Display(?ListControlTypes)
        Post(EVENT:Selected)
    End	 
  ReturnValue = PARENT.TakeEvent()
    !---- Noyantis : Codejock Property Grid Wrapper - Start ----
    PropertyGrid6.TakeEvent(EVENT())
    !---- Noyantis : Codejock Property Grid Wrapper - End ----
    !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
    NYS:DockingPane_EventMgr.TakeEvent(EVENT())
    IF NYS:DockingPane_EventMgr.TakeEventCycleRequired = TRUE THEN CYCLE.
    !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  If event() = event:VisibleOnDesktop !or event() = event:moved
    ds_VisibleOnDesktop()
  end
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeFieldEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all field specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeFieldEvent()
  CASE FIELD()
  OF ?ControlsSheet
    CASE EVENT()
    OF EVENT:TabChanging
      !MyDct.Trace('[UpdateField][ThisWindow.TakeFieldEvent][?ControlsSheet][EVENT:TabChanging]Choice['& Choice(?ControlsSheet) &']')
    ELSE
      MyDct.Trace('[UpdateField][ThisWindow.TakeFieldEvent][?ControlsSheet][Else]Choice['& Choice(?ControlsSheet) &']') 
    local.PopulateControlTypesQ(Choice(?ControlsSheet))   
    END
  END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeNewSelection PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all NewSelection events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeNewSelection()
    CASE FIELD()
    OF ?Fld:DataType
      MyDct.Trace('UpdateField - ?Fld:DataType')
      local.SetFieldSize()
      Fld:ScreenPicture = MyDct.GetDefaultScreenPicture(Fld:DataType)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeNotify PROCEDURE(UNSIGNED NotifyCode,SIGNED Thread,LONG Parameter)

ReturnValue          BYTE,AUTO

  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.TakeNotify]')
  ReturnValue = PARENT.TakeNotify(NotifyCode,Thread,Parameter)
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE EVENT()
    OF EVENT:CloseDown
      if WE::CantCloseNow
        WE::MustClose = 1
        cycle
      else
        self.CancelAction = cancel:cancel
        self.response = requestcancelled
      end
    END
    !---- Noyantis : Codejock Property Grid Wrapper - Start ----
    PropertyGrid6.TakeWindowEvent(QuickWindow)
    !---- Noyantis : Codejock Property Grid Wrapper - End ----
    !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
    NYS:DockingPane_EventMgr.TakeWindowEvent(QuickWindow)
    !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:OpenWindow
        post(event:visibleondesktop)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.Update PROCEDURE

  CODE
  MyDCT.Trace('[UpdateField][ThisWindow.Update]')
  PARENT.Update

!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
local.PopulateControls        Procedure()
xml     xFilesTree
    CODE
    MyDct.Trace('[UpdateField][local.PopulateControls]')
    !If WindowControl.LinesQ &= NULL
    !    WindowControl.LinesQ &= NEW lineQueueType
    !END
    st.FromBlob(Fld:WindowControlBlob)
    MyDct.Trace('')
    MyDct.Trace('local.PopulateControls')
    MyDct.Trace('')
    MyDct.Trace('[local.PopulateControls] From WindowControlBlob ============================================================================================================================')
    MyDct.Trace('')
    MyDct.Trace(st.GetValue())
    !MyDct.Trace('   Records(WindowControl.LinesQ) = ' & Records(WindowControl.LinesQ))
    MyDct.Trace('')

    Case Upper(Clip(Fld:CtrlType))
    Of 'CHECK'
    OrOf 'IMAGE'
        st.Start()
    END

    xml.start()
    xml.SetTagCase(xf:CaseAsIs)
    xml.Load(WindowControl,st,'WindowControl','Line') !,'Line')

    MyDct.Trace('')
    MyDct.Trace('   Records(WindowControl) = ' & Records(WindowControl))
    MyDct.Trace('')

    st.FromBlob(Fld:ReportControlBlob)
    MyDct.Trace('')
    MyDct.Trace(st.GetValue())
    MyDct.Trace('')
    !MyDct.Trace('   Records(WindowControl.LinesQ) = ' & Records(WindowControl.LinesQ))
    MyDct.Trace('')
    xml.start()
    xml.SetTagCase(xf:CaseAsIs)
    xml.Load(ReportControl,st,'ReportControl','Line') !,'Line')
    MyDct.Trace('')
    MyDct.Trace('   Records(ReportControl) = ' & Records(ReportControl))
    MyDct.Trace('')

local.PopulateControlTypesQ   Procedure(<long pTabChoice>)
    CODE    
    MyDct.Trace('[UpdateField][local.PopulateControlTypesQ]')
    Free(ControlTypesQ)

    If Fld:DataType <> 'MEMO'
        ControlTypesQ.CtrlType = 'CHECK'
        ControlTypesQ.RowID = Records(ControlTypesQ)+1
        Add(ControlTypesQ)
    END

    ControlTypesQ.CtrlType = 'IMAGE'
    ControlTypesQ.RowID = Records(ControlTypesQ)+1
    Add(ControlTypesQ)

    If pTabChoice = 1 Or OMITTED(pTabChoice)   ! Window
        ControlTypesQ.CtrlType = 'ENTRY'
        ControlTypesQ.RowID = Records(ControlTypesQ)+1
        Add(ControlTypesQ)
    END

    If pTabChoice = 1 Or OMITTED(pTabChoice)   ! Window
        ControlTypesQ.CtrlType = 'SPIN'
        ControlTypesQ.RowID = Records(ControlTypesQ)+1
        Add(ControlTypesQ)
    End

    ControlTypesQ.CtrlType = 'STRING'
    ControlTypesQ.RowID = Records(ControlTypesQ)+1
    Add(ControlTypesQ)

    ControlTypesQ.CtrlType = 'TEXT'
    ControlTypesQ.RowID = Records(ControlTypesQ)+1
    Add(ControlTypesQ)


local.PopulateCommentsQ       Procedure()
TempSt          StringTheory
LneSt           StringTheory
StartPos        Long
EndPos          Long
xml             xFilesTree
    code
    MyDct.Trace('[UpdateField][local.PopulateCommentsQ]')
    MyDct.PopulateCommentsQ(Fields,'CommentsBlob')
!    !MyDct.Trace('')
!    !MyDct.Trace('UpdateField - local.PopulateCommentsQ - Records(MyDct.CommentsQ) = ' & Records(MyDct.CommentsQ))
!    !MyDct.Trace('')    
!    Free(MyDct.CommentsQ)
!    TempSt.Start()
!    !If Not Status(Fields)
!    !    Open(Fields,42h)
!    !End
!    TempSt.FromBlob(Fld:CommentsBlob)
!    !MyDct.Trace('')
!    !MyDct.Trace(TempSt.GetValue())
!    !MyDct.Trace('')
!    !MyDct.Trace('Length = ' & st.Length())
!    !MyDct.Trace('')
!    StartPos = TempSt.FindChars('<Comment<32>')
!    Loop 
!        If StartPos => TempSt.Length(); Break End
!        LneSt.Start()
!        LneSt.SetValue(MyDct.GetDelimitedStr(TempSt,StartPos,EndPos,'</Comment>',False),True)
!        LneSt.Remove('<13,10>')
!        !MyDct.Trace(StartPos & '<9>' & LneSt.GetValue())
!        Clear(CommentGrp)
!        Clear(MyDct.CommentsQ)
!        If LneSt.ContainsA('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ')
!            xml.Start()
!            xml.SetTagCase(XF:CaseAsIs)
!            xml.Load(CommentGrp,LneSt,'Comment')
!            !MyDct.Trace(CommentGrp.Audit.CreateDate & ' ' & WHERE(CommentGrp,CommentGrp.Audit.CreateDate))
!            MyDct.CommentsQ = CommentGrp
!            Add(MyDct.CommentsQ)
!        End
!    End
!    If Not CommentRecsCnt
!        CommentRecsCnt = Records(MyDct.CommentsQ)
!    END
!    TempSt.Start()
!    !MyDct.Trace('')

local.PopulateOptionsQ          Procedure()
TempSt          StringTheory
LneSt           StringTheory
StartPos        Long
EndPos          Long
xml             xFilesTree
    code
    MyDct.Trace('[UpdateField][local.PopulateOptionsQ]')
    MyDct.PopulateOptionsQ(Fields,'OptionsBlob')
!    !MyDct.Trace('')
!    !MyDct.Trace('UpdateField - local.PopulateCommentsQ - Records(MyDct.OptionsQ) = ' & Records(MyDct.OptionsQ))
!    !MyDct.Trace('')   
!    Free(MyDct.OptionsQ) 
!    !MyDct.Trace('')
!    !MyDct.Trace('UpdateField - local.PopulateMyDct.OptionsQ')
!    TempSt.Start()
!    TempSt.FromBlob(Fld:OptionsBlob)
!    !MyDct.Trace('')
!    !MyDct.Trace(TempSt.GetValue())
!    !MyDct.Trace('')
!    !MyDct.Trace('Length = ' & TempSt.Length())
!    !MyDct.Trace('')
!    StartPos = TempSt.FindChars('<Option<32>')    
!    Loop 
!        If StartPos => TempSt.Length(); Break End
!        LneSt.Start()
!        LneSt.SetValue(MyDct.GetDelimitedStr(TempSt,StartPos,EndPos,'/>',False),True)
!        LneSt.Remove('<13,10>')
!        !MyDct.Trace(StartPos & '<9>' & LneSt.GetValue())
!        Clear(OptionGrp)
!        Clear(MyDct.OptionsQ)
!        If LneSt.ContainsA('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ')
!            xml.Start()
!            xml.SetTagCase(XF:CaseAsIs)
!            xml.Load(OptionGrp,LneSt,'Option')
!            OptionGrp.Id = Records(MyDct.OptionsQ) + 1
!            MyDct.OptionsQ = OptionGrp
!            Add(MyDct.OptionsQ)
!        End
!    End
!    If Not OptionRecCnt
!        OptionRecCnt = Records(MyDct.OptionsQ)
!    End
!    TempSt.Start()
!    !MyDct.Trace('')
!    !st.Start()

local.PopulateValidityCheck     Procedure()
    CODE
    MyDct.Trace('[UpdateField][local.PopulateValidityCheck]')
    st.FromBlob(Fld:ValidityBlob)
    xml.Start()
    xml.SetTagCase(XF:CaseAsIs)
    xml.Load(ValidityGrp,st,'Validity')

local.SaveGlyphBitmapToFile   PROCEDURE(STRING pFileName, *GLYPHMETRICS pGM, LONG pBuffer, LONG bufSize)
!bmh        LIKE(BITMAPFILEHEADER)
!bih        LIKE(BITMAPINFOHEADER)
!Palette    BYTE,DIM(256*4)      ! 256-color grayscale palette (BGRA)
!width      LONG
!height     LONG
!stride     LONG
!imageSize  LONG
!hFile      LONG
!bytesW     LONG
!i          LONG
!FileName   CSTRING(260)
!ok         BOOL
    CODE
    MyDct.Trace('[UpdateField][local.SaveGlyphBitmapToFile]')
!    IF pBuffer = 0 OR bufSize <= 0
!        MyDct.Trace('No glyph data to save')
!        RETURN
!    END
!
!    width  = pGM.gmBlackBoxX
!    height = pGM.gmBlackBoxY
!    IF width <= 0 OR height <= 0
!        MyDct.Trace('Invalid glyph size')
!        RETURN
!    END
!
!    ! Scanline padded to 4 bytes for BMP
!    imageSize = bufSize
!    !bih.biSizeImage = imageSize
!
!
!    stride    = ((width + 3) / 4) * 4  !(width + 3) BAND -4h
!    !imageSize = stride * height
!
!    CLEAR(bmh)
!    CLEAR(bih)
!    CLEAR(Palette)
!
!    ! --- Fill BMP headers ---
!    bmh.bfType    = 4D42h                      ! 'BM'
!    bmh.bfOffBits = SIZE(bmh) + SIZE(bih) + (256*4)
!    bmh.bfSize    = bmh.bfOffBits + imageSize
!
!    bih.biSize        = SIZE(bih)
!    bih.biWidth       = width
!    bih.biHeight      = -height                 ! negative = top-down bitmap
!    bih.biPlanes      = 1
!    bih.biBitCount    = 8                       ! 8bpp indexed
!    bih.biCompression = 0                       ! BI_RGB
!    bih.biSizeImage   = imageSize
!    bih.biClrUsed     = 256
!    bih.biClrImportant= 256
!
!    ! Grayscale palette 0..255
!    LOOP i = 0 TO 255
!        Palette[i*4 + 1] = CHR(i)   ! Blue
!        Palette[i*4 + 2] = CHR(i)   ! Green
!        Palette[i*4 + 3] = CHR(i)   ! Red
!        Palette[i*4 + 4] = 0        ! Reserved
!    END
!
!    FileName = CLIP(pFileName)
!
!    ! --- Create file with Win32 API ---
!    hFile = Dct_CreateFileA(FileName, GENERIC_WRITE, FILE_SHARE_NONE, 0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0)
!    IF hFile = INVALID_HANDLE_VALUE
!        MyDct.Trace('CreateFileA failed for: ' & FileName & ' ' & Clip(FormatMessage(Dct_GetLastError())))
!        RETURN
!    END
!
!    ! Write BITMAPFILEHEADER
!    ok = Dct_WriteFile(hFile, ADDRESS(bmh), SIZE(bmh), bytesW, 0)
!    IF NOT ok
!        MyDct.Trace('WriteFile failed (file header) ' & Clip(FormatMessage(Dct_GetLastError())))
!        !GOTO Cleanup
!    END
!
!    ! Write BITMAPINFOHEADER
!    ok = Dct_WriteFile(hFile, ADDRESS(bih), SIZE(bih), bytesW, 0)
!    IF NOT ok
!        MyDct.Trace('WriteFile failed (info header) ' & Clip(FormatMessage(Dct_GetLastError())))
!        !GOTO Cleanup
!    END
!
!    ! Write palette
!    ok = Dct_WriteFile(hFile, ADDRESS(Palette[1]), 256*4, bytesW, 0)   ![1]
!    IF NOT ok
!        MyDct.Trace('WriteFile failed (palette) ' & Clip(FormatMessage(Dct_GetLastError())))
!        !GOTO Cleanup
!    END
!
!    ! Write raw glyph pixels
!    ok = Dct_WriteFile(hFile, pBuffer, imageSize, bytesW, 0)
!    IF NOT ok
!        MyDct.Trace('WriteFile failed (pixel data) ' & Clip(FormatMessage(Dct_GetLastError())))
!        !GOTO Cleanup
!    END
!
!    MyDct.Trace('Saved BMP: ' & FileName)
!
!    !Cleanup:
!    IF hFile <> INVALID_HANDLE_VALUE
!        MyDct.Trace('<9>Before CloseHandle')
!        Dct_CloseHandle(hFile)
!        MyDct.Trace('<9>After CloseHandle')
!    END
!
!    IF hOldFont
!        Dct_SelectObject(hDC, hOldFont)
!    END
!    IF hFont
!        Dct_DeleteObject(hFont)
!    END
!    IF hDC
!        Dct_ReleaseDC(0, hDC)
!    END 


local.SetFieldSize            Procedure()
    CODE
    MyDct.Trace('[UpdateField][local.SetFieldSize]')
    ?Fld:FieldSize{PROP:Disable} = TRUE
    ?Fld:Places{PROP:Disable} = TRUE
    Case Clip(Fld:DataType)
    Of 'BLOB'
        If Fld:FieldSize = ''
            Fld:FieldSize  =  1
        End
    Of 'BYTE'
        If Fld:FieldSize = ''
            Fld:FieldSize  =  1
        End
    Of 'CSTRING'
        If Fld:FieldSize = ''
            Fld:FieldSize  =  21
        End
        ?Fld:FieldSize{PROP:Disable}  =  FALSE
    Of 'DATE'
        If Fld:FieldSize = ''
            Fld:FieldSize  =  4
        END
    Of 'DECIMAL'
        If Fld:FieldSize = ''
            Fld:FieldSize  =  7
        End
        If Fld:Places = ''
            Fld:Places  =  2
        End
        ?Fld:FieldSize{PROP:Disable}  =  FALSE
        ?Fld:Places{PROP:Disable}     =  FALSE
    Of 'LONG'
        If Fld:FieldSize = ''
            Fld:FieldSize  =  4
        END
    Of 'MEMO'
        If Fld:FieldSize = ''
            Fld:FieldSize  =  1
        End
    Of 'PSTRING'
        If Fld:FieldSize = ''
            Fld:FieldSize  =  21
        End
        ?Fld:FieldSize{PROP:Disable}  =  FALSE
    Of 'REAL'
        If Fld:FieldSize = ''
            Fld:FieldSize  =  8
        End
    Of 'SHORT'
        If Fld:FieldSize = ''
            Fld:FieldSize  =  2
        End
    Of 'SREAL'
        If Fld:FieldSize = ''
            Fld:FieldSize  =  4
        End
    Of 'STRING'
        If Fld:FieldSize = ''
            Fld:FieldSize  =  20
        End
        ?Fld:FieldSize{PROP:Disable}  =  FALSE
    Of 'TIME'
        If Fld:FieldSize = ''
            Fld:FieldSize  =  4
        End
    Of 'ULONG'
        If Fld:FieldSize = ''
            Fld:FieldSize  =  4
        End
    Of 'USHORT'
        If Fld:FieldSize = ''
            Fld:FieldSize  =  2
        End
    End
    self.SetScreenPicture()

local.SetScreenPicture        Procedure()
    code
    MyDct.Trace('[UpdateField][local.SetScreenPicture]')
    RETURN

    Case Clip(Fld:DataType)
    Of 'BLOB'
    Of 'BYTE'
    Of 'CSTRING'
        Fld:ScreenPicture = '@S' & Clip(Fld:FieldSize)
    Of 'DATE'
    Of 'DECIMAL'
    Of 'LONG'
    Of 'MEMO'
    Of 'PSTRING'
    Of 'REAL'
    Of 'SHORT'
    Of 'SREAL'
    Of 'STRING'
        Fld:ScreenPicture = '@S' & Clip(Fld:FieldSize)
    Of 'TIME'
    Of 'ULONG'
    Of 'USHORT'
    End

local.SetWinCtrlBlob          Procedure(string pPrompt,string pLabel)
    CODE
    MyDct.Trace('[UpdateField][local.SetWinCtrlBlob]')
    

!    WindowControl.text = ' PROMPT(<39>' & Clip(pPrompt) & '<39>),USE(?' & Clip(ParamsGrp.TablePrefix) & ':Guid:Prompt)'
!    Add(WindowControl)
!
!    WindowControl.text = ' ENTRY(@s16),USE('& Clip(ParamsGrp.TablePrefix) & ':' & Clip(pLabel) & ')'
!    Add(WindowControl)
!
!    xml.start()
!    xml.SetOmitXMLHeader(True)
!    xml.SetTagCase(xf:CaseAsIs)
!    xml.SetDontSaveBlanks(true)
!    xml.SetDontSaveBlankGroups(true)
!    xml.Save(WindowControl,WinCtrlSt,'WindowControl','Line') ! Save to a StringTheory object
!    WinCtrlSt.ToBlob(Fld:WindowControlBlob)
!---- Noyantis : Codejock Property Grid Wrapper - Start ----
PropertyGrid6.Drop                       PROCEDURE(STRING paramDragID, STRING paramDropID)
  CODE
  PARENT.Drop(paramDragID, paramDropID)
  RETURN

PropertyGrid6.Event                      PROCEDURE(STRING paramEventName, <*SHORT paramReference>, <SIGNED paramOleControl>, <LONG paramCurrentEvent>)
  CODE
  PARENT.Event(paramEventName, paramReference, paramOleControl, paramCurrentEvent)
  RETURN

PropertyGrid6.EventFunc                  PROCEDURE(*SHORT Reference, SIGNED OleControl, LONG CurrentEvent)
  CODE
  PARENT.EventFunc(Reference, OleControl, CurrentEvent)
  RETURN

PropertyGrid6.EventFuncCommon            PROCEDURE(*SHORT Reference, SIGNED OleControl, LONG CurrentEvent)
  CODE
  PARENT.EventFuncCommon(Reference, OleControl, CurrentEvent)
  RETURN

PropertyGrid6.Init                       PROCEDURE()
  CODE
  SELF.MakeTarget = UPPER('DictionaryEditor.EXE')
  PARENT.Init()
  RETURN

PropertyGrid6.InitComplete               PROCEDURE()
  CODE
  PARENT.InitComplete()
  RETURN

PropertyGrid6.InitPrepare                PROCEDURE(SIGNED paramOCXCtrl)
  CODE
  SELF.DisableAtRuntime   = FALSE
  PARENT.InitPrepare(paramOCXCtrl)
  RETURN

PropertyGrid6.InitResize                 PROCEDURE()
  CODE
  PARENT.InitResize()
  IF SELF.DragDropCtrl <> 0
    CLEAR(SELF.mhResize)
    IF SELF.OCXCtrl <> 0 AND mhResize.GetControlRatios(SELF.OCXCtrl, SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height) = Level:Benign
      mhResize.AddControl(SELF.DragDropCtrl, SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height)
    END
  END
  IF SELF.OCXBox       <> 0
    CLEAR(SELF.mhResize)
    IF SELF.OCXCtrl <> 0 AND mhResize.GetControlRatios(SELF.OCXCtrl, SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height) = Level:Benign
      mhResize.AddControl(SELF.OCXBox,       SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height)
    END
  END
  IF SELF.ParentGroup  <> 0
    CLEAR(SELF.mhResize)
    IF SELF.OCXCtrl <> 0 AND mhResize.GetControlRatios(SELF.OCXCtrl, SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height) = Level:Benign
      mhResize.AddControl(SELF.ParentGroup,  SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height)
    END
  END
  IF SELF.ParentRegion <> 0
    CLEAR(SELF.mhResize)
    IF SELF.OCXCtrl <> 0 AND mhResize.GetControlRatios(SELF.OCXCtrl, SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height) = Level:Benign
      mhResize.AddControl(SELF.ParentRegion, SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height)
    END
  END
  IF SELF.ResizeBox    <> 0
    CLEAR(SELF.mhResize)
    IF SELF.OCXCtrl <> 0 AND mhResize.GetControlRatios(SELF.OCXCtrl, SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height) = Level:Benign
      mhResize.AddControl(SELF.ResizeBox,    SELF.mhResize.XPos, SELF.mhResize.YPos, SELF.mhResize.Width, SELF.mhResize.Height)
    END
  END
  CLEAR(SELF.mhResize)
  SELF.mhResize.Active = TRUE
  RETURN

PropertyGrid6.InitSecurity               PROCEDURE()
  CODE
  PARENT.InitSecurity()
  RETURN

PropertyGrid6.InitTemplateSettings       PROCEDURE()
  CODE
  PARENT.InitTemplateSettings()
  SELF.GVisualsLink.LinkActive = TemplateHelper.Settings_GSeries.GVisuals.Link.AutoLink
  SELF.EnterTab                    = 0
  SELF.RequiredItemStrategy        = nysPropertyGrid_ReqImmediately
  SELF.SelectAllContentWhenEditing = 0
  SELF.DataSourceType              = nysPropertyGrid_SourceTpl
  SELF.DefaultCategoryHeight       = -1
  SELF.DefaultItemHeight           = -1
  
  SELF.ClearTrapableEvents()
  SELF.ClearTrapableKeystrokes()
  SELF.AllowMoveableSplitter(1)
  SELF.SetDefaultDateFormat('%d/%m/%Y', '__/__/____')
  SELF.SetDefaultTimeFormat('%h:%m:%s', '__:__:__')
  SELF.SetReadOnlyMode(0)
  SELF.SetSplitterPos('0.50')
  SELF.ShowToolbar(0)
  SELF.ShowHelp(1)
  SELF.HightlightUpdated(0)
  SELF.NavigateItemsTabKey(0)
  SELF.CreateCategories()
  SELF.CreateItems()
  SELF.CreateInPlaceButtons()
  SELF.CreateVerbs()
  SELF.SetSortOrder(0)
  SELF.SetVerbPanelHeight(50)
  RETURN

PropertyGrid6.Keystroke                  PROCEDURE(UNSIGNED paramKeycode)
  CODE
  PARENT.Keystroke(paramKeycode)
  RETURN

PropertyGrid6.Kill                       PROCEDURE()
  CODE
  PARENT.Kill()
  RETURN

PropertyGrid6.KillComplete               PROCEDURE()
  CODE
  PARENT.KillComplete()
  RETURN

PropertyGrid6.ParametersReceived         PROCEDURE(<LONG paramSessionID>)
  CODE
  PARENT.ParametersReceived(paramSessionID)
  RETURN

PropertyGrid6.ProcessClones              PROCEDURE()
  CODE
  PARENT.ProcessClones()
  RETURN

PropertyGrid6.ProcessMimics              PROCEDURE()
  CODE
  PARENT.ProcessMimics()
  RETURN

PropertyGrid6.ProcessShortcutKey         PROCEDURE(UNSIGNED pKeyCode)
  CODE
  PARENT.ProcessShortcutKey(pKeyCode)
  RETURN

PropertyGrid6.RefreshContents            PROCEDURE(<BYTE paramForce>)
  CODE
  PARENT.RefreshContents(paramForce)
  RETURN

PropertyGrid6.SyncOCXHeight              PROCEDURE(<BYTE pForce>)
  CODE
  PARENT.SyncOCXHeight(pForce)
  RETURN

PropertyGrid6.SyncOCXWidth               PROCEDURE(<BYTE pForce>)
  CODE
  PARENT.SyncOCXWidth(pForce)
  RETURN

PropertyGrid6.TakeEvent                  PROCEDURE(SIGNED paramEvent)
  CODE
  PARENT.TakeEvent(paramEvent)
  RETURN

PropertyGrid6.TakeNotify                 PROCEDURE(UNSIGNED paramNotifyCode, SIGNED paramThread, LONG paramParameter)
  CODE
  PARENT.TakeNotify(paramNotifyCode, paramThread, paramParameter)
  RETURN

PropertyGrid6.TakeSubClassEvent          PROCEDURE(UNSIGNED paramWndHndl, UNSIGNED paramMsg, UNSIGNED paramWParam, LONG paramLParam)
  CODE
  PARENT.TakeSubClassEvent(paramWndHndl, paramMsg, paramWParam, paramLParam)
  RETURN

PropertyGrid6.TakeTimer                  PROCEDURE()
  CODE
  PARENT.TakeTimer()
  RETURN

PropertyGrid6.TakeWindowEvent            PROCEDURE(*WINDOW paramWindow)
  CODE
  PARENT.TakeWindowEvent(paramWindow)
  RETURN

PropertyGrid6.CreateCategories           PROCEDURE()
  CODE
  PARENT.CreateCategories()
  SELF.AddCategory('CategoryGeneral', 'General')
  SELF.SetCategoryExpanded('CategoryGeneral', 1)
  SELF.SetCategoryCollection('CategoryGeneral', '', 0)
  SELF.SetCategoryProperty('CategoryGeneral', 'Expandable', 1)
  
  SELF.AddCategory('CategoryColor', 'Color')
  SELF.SetCategoryExpanded('CategoryColor', 1)
  SELF.SetCategoryCollection('CategoryColor', '', 0)
  SELF.SetCategoryProperty('CategoryColor', 'Expandable', 1)
  
  SELF.AddCategory('CategoryDesign', 'Design')
  SELF.SetCategoryExpanded('CategoryDesign', 1)
  SELF.SetCategoryCollection('CategoryDesign', '', 0)
  SELF.SetCategoryProperty('CategoryDesign', 'Expandable', 1)
  
  SELF.AddCategory('CategoryExtra', 'Extra')
  SELF.SetCategoryExpanded('CategoryExtra', 1)
  SELF.SetCategoryCollection('CategoryExtra', '', 0)
  SELF.SetCategoryProperty('CategoryExtra', 'Expandable', 1)
  
  SELF.AddCategory('CategoryHelp', 'Help')
  SELF.SetCategoryExpanded('CategoryHelp', 1)
  SELF.SetCategoryCollection('CategoryHelp', '', 0)
  SELF.SetCategoryProperty('CategoryHelp', 'Expandable', 1)
  
  SELF.AddCategory('CategoryMode', 'Mode')
  SELF.SetCategoryExpanded('CategoryMode', 1)
  SELF.SetCategoryCollection('CategoryMode', '', 0)
  SELF.SetCategoryProperty('CategoryMode', 'Expandable', 1)
  
  SELF.AddCategory('CategoryPosition', 'Position')
  SELF.SetCategoryExpanded('CategoryPosition', 1)
  SELF.SetCategoryCollection('CategoryPosition', '', 0)
  SELF.SetCategoryProperty('CategoryPosition', 'Expandable', 1)
  
  RETURN

PropertyGrid6.CreateInPlaceButtons       PROCEDURE()
  CODE
  PARENT.CreateInPlaceButtons()
  SELF.AddItemInPlaceButton('ItemName', 'FontNameBtnID')
  SELF.SetInPlaceButtonProperty('FontNameBtnID', 'Alignment',  1)
  SELF.SetInPlaceButtonProperty('FontNameBtnID', 'Caption',   'FontNameBtn')
  SELF.SetInPlaceButtonProperty('FontNameBtnID', 'Enabled',    1)
  SELF.SetInPlaceButtonProperty('FontNameBtnID', 'Hyperlink',  0)
  SELF.SetInPlaceButtonProperty('FontNameBtnID', 'ShowAlways', 0)
  SELF.SetInPlaceButtonProperty('FontNameBtnID', 'TabStop',   TRUE)
  
  RETURN

PropertyGrid6.CreateItems                PROCEDURE()
  CODE
  PARENT.CreateItems()
  SELF.AddItem('CategoryGeneral', 'Justification', 'ItemJustification', 0, 'Normal', '')
  
  SELF.AddItem('CategoryGeneral', 'Layout', 'ItemLayout', 0, 'Normal', '')
  
  SELF.AddItem('CategoryGeneral', 'Text', 'ItemText', 0, 'Normal', '')
  
  SELF.AddItem('CategoryGeneral', 'TextFont', 'ItemTextFont', 4, 'Normal', '')
  
  SELF.AddItem('ItemTextFont', 'Bold', 'ItemBold', 2, 'Normal', '')
  
  SELF.AddItem('ItemTextFont', 'GdiCharSet', 'ItemGdiCharSet', 0, 'Normal', '')
  
  SELF.AddItem('ItemTextFont', 'Italic', 'ItemItalic', 2, 'Normal', '')
  
  SELF.AddItem('ItemTextFont', 'Name', 'ItemName', 8, 'Normal', '')
  SELF.SetCtrlProperty('ItemName', '.Flags', xtpGridItemHasComboButton)
  
  SELF.AddItem('ItemTextFont', 'Size', 'ItemSize', 1, 'Normal', '')
  
  SELF.AddItem('ItemTextFont', 'Strikeout', 'ItemStrikeout', 2, 'Normal', '')
  
  SELF.AddItem('ItemTextFont', 'Underline', 'ItemUnderline', 2, 'Normal', '')
  
  SELF.AddItem('CategoryGeneral', 'Use', 'ItemUse', 0, 'Normal', '')
  
  SELF.AddItem('CategoryColor', 'BackGround', 'ItemBackground', 3, 'Normal', '')
  
  SELF.AddItem('CategoryColor', 'TextColor', 'ItemTextColor', 3, 'Normal', '')
  
  SELF.AddItem('CategoryDesign', 'SupressTransparency', 'ItemSupressTransparency', 2, 'Normal', '')
  
  SELF.AddItem('CategoryDesign', 'TabIndex', 'ItemTabIndex', 1, 'Normal', '')
  
  SELF.AddItem('CategoryExtra', 'DropID', 'ItemDropID', 0, 'Normal', '')
  
  SELF.AddItem('CategoryHelp', 'Cursor', 'ItemCursor', 0, 'Normal', '')
  
  SELF.AddItem('CategoryMode', 'Disable', 'ItemDisable', 2, 'Normal', '')
  
  SELF.AddItem('CategoryMode', 'Hide', 'ItemHide', 2, 'Normal', '')
  
  SELF.AddItem('CategoryMode', 'Scroll', 'ItemScroll', 2, 'Normal', '')
  
  SELF.AddItem('CategoryMode', 'Transparent', 'ItemTransparent', 2, 'Normal', '')
  
  SELF.AddItem('CategoryPosition', 'AT', 'ItemAT', 0, 'Normal', '')
  
  SELF.AddItem('ItemAT', 'Height', 'ItemHeight', 0, 'Normal', '')
  
  SELF.AddItem('ItemHeight', 'Default', 'ItemDefaultHeight', 2, 'Normal', '')
  
  SELF.AddItem('ItemHeight', 'Full', 'ItemHeightFull', 2, 'Normal', '')
  
  SELF.AddItem('ItemHeight', 'Value', 'ItemHeightValue', 0, 'Normal', '')
  
  SELF.AddItem('ItemAT', 'Width', 'ItemWidth', 0, 'Normal', '')
  
  SELF.AddItem('ItemWidth', 'Default', 'ItemWidthDefault', 2, 'Normal', '')
  
  SELF.AddItem('ItemWidth', 'Full', 'ItemWidthFull', 2, 'Normal', '')
  
  SELF.AddItem('ItemWidth', 'Value', 'ItemWidthValue', 0, 'Normal', '')
  
    !SELF.AddItemDropOption('ItemName', 'RIDLEY', 1)  
    !MyDct.Trace('')
    !MyDct.Trace('[UpdateField][PropertyGrid6.CreateItems]')
    !MyDct.Trace('')
    Loop i = 1 to Records(fonts)
        Get(fonts,i)
        !MyDct.Trace('<9>' & i & '<9>' & Clip(fonts.name))
        SELF.AddItemDropOption('ItemName', Clip(fonts.name), i)
    End
    !MyDct.Trace('')    
  RETURN

PropertyGrid6.CreateVerbs                PROCEDURE()
  CODE
  PARENT.CreateVerbs()
  RETURN

PropertyGrid6.CatAccepted                PROCEDURE(STRING paramID, BYTE paramExpanded)
  CODE
    MyDct.Trace('CatAccepted ''' & CLIP(paramID) & ''' has been accepted.||It''s paramExpanded is "' & paramExpanded)  !!!, 'Item Accepted', ICON:ASTERISK)
  PARENT.CatAccepted(paramID, paramExpanded)
  RETURN

PropertyGrid6.InPlaceButtonAccepted      PROCEDURE(STRING paramID)
  CODE
  PARENT.InPlaceButtonAccepted(paramID)
  CASE paramID
  OF 'FontNameBtnID'
  END
  RETURN

PropertyGrid6.ItemAccepted               PROCEDURE(STRING paramID, STRING paramValue, STRING paramClarionValue)
FontGrp     LIKE(LOGFONT)
  CODE
  PARENT.ItemAccepted(paramID, paramValue, paramClarionValue)
  !IF Loc:TrapItems = TRUE
    !BEEP(BEEP:SystemAsterisk)
    MyDct.Trace('ItemAccepted ''' & CLIP(paramID) & ''' has been accepted.||It''s value is "' & CLIP(paramValue) & '" It''s Clarion Value is "'& Clip(self.GetItemValue(paramID)) &'"')  !!!, 'Item Accepted', ICON:ASTERISK)
  !END  
  CASE paramID
  OF 'ItemJustification'
    !!!!!!
  OF 'ItemLayout'
  OF 'ItemText'
  OF 'ItemTextFont'
    ParseFontFromLong(paramClarionValue,FontGrp)
  OF 'ItemBold'
  OF 'ItemGdiCharSet'
  OF 'ItemItalic'
  OF 'ItemName'
  OF 'ItemSize'
  OF 'ItemStrikeout'
  OF 'ItemUnderline'
  OF 'ItemUse'
  OF 'ItemBackground'
  OF 'ItemTextColor'
  OF 'ItemSupressTransparency'
  OF 'ItemTabIndex'
  OF 'ItemDropID'
  OF 'ItemCursor'
  OF 'ItemDisable'
  OF 'ItemHide'
  OF 'ItemScroll'
  OF 'ItemTransparent'
  OF 'ItemAT'
  OF 'ItemHeight'
  OF 'ItemDefaultHeight'
  OF 'ItemHeightFull'
  OF 'ItemHeightValue'
  OF 'ItemWidth'
  OF 'ItemWidthDefault'
  OF 'ItemWidthFull'
  OF 'ItemWidthValue'
  END
  RETURN

PropertyGrid6.ItemSelected               PROCEDURE(STRING paramID, STRING paramValue, STRING paramClarionValue)
  CODE
    MyDct.Trace('ItemSelected ''' & CLIP(paramID) & ''' has been accepted.||It''s value is "' & CLIP(paramValue) & '" It''s Clarion Value is "'& Clip(paramClarionValue) &'"')  !!!, 'Item Accepted', ICON:ASTERISK)
  PARENT.ItemSelected(paramID, paramValue, paramClarionValue)
  CASE paramID
  OF 'ItemJustification'
  OF 'ItemLayout'
  OF 'ItemText'
  OF 'ItemTextFont'
  OF 'ItemBold'
  OF 'ItemGdiCharSet'
  OF 'ItemItalic'
  OF 'ItemName'
  OF 'ItemSize'
  OF 'ItemStrikeout'
  OF 'ItemUnderline'
  OF 'ItemUse'
  OF 'ItemBackground'
  OF 'ItemTextColor'
  OF 'ItemSupressTransparency'
  OF 'ItemTabIndex'
  OF 'ItemDropID'
  OF 'ItemCursor'
  OF 'ItemDisable'
  OF 'ItemHide'
  OF 'ItemScroll'
  OF 'ItemTransparent'
  OF 'ItemAT'
  OF 'ItemHeight'
  OF 'ItemDefaultHeight'
  OF 'ItemHeightFull'
  OF 'ItemHeightValue'
  OF 'ItemWidth'
  OF 'ItemWidthDefault'
  OF 'ItemWidthFull'
  OF 'ItemWidthValue'
  END
  RETURN

PropertyGrid6.VerbAccepted               PROCEDURE(STRING paramID)
  CODE
  PARENT.VerbAccepted(paramID)
  RETURN

!---- Noyantis : Codejock Property Grid Wrapper - End ----

mhResize.Init PROCEDURE

  CODE
  SELF.Init(                                               |
      mhResizeFrame,                                       | !Global Host Object
      QuickWindow,                                         | !Window
      UpdateField:mhResize:WM.MH::ResizeIWindowManager,    | !Window Manager
      448,                                                 | !Original Width
      305,                                                 | !Original Height
      1,                                                   | !Resize Width?
      1,                                                   | !Resize Height?
      0,                                                   | !Orig Size is Min?
      0,                                                   | !Border Beyond Min?
      1,                                                   | !Maximize Initially?
      1)                                                     !Do NOT hide+unhide during opening operations?
  SELF.AddControl(?CurrentTab,0,0,1,1)
  SELF.AddControl(?PropertyGrid,0,0,0,1)
  RETURN
  PARENT.Init

mhResize.InitialResize PROCEDURE()

  CODE
  IF NOT SELF.IsInitialResizeDone
    SELF.InitialResize(                                    |
        0,                                                 |  !Restore from INI file?
        1,                                                 |  !Maximize Initially?
        mhHorzPos:Full,                                    |  !Init Horz Pos
        mhVertPos:Full,                                    |  !Init Vert Pos
        0,                                                 |  !Horizontal Shift
        0                                                  )  !Vertical Shift
    IF SELF.IsInitialResizeDone
    END
  END


UpdateField:mhResize:WM.MH::ResizeIWindowManager.Reset PROCEDURE(<BYTE Force>)
  CODE
  ThisWindow.Reset(Force)

NotifyHandler.HandleNotify       PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter)
us    UltimateString
TempSt          StringTheory
XmlSt           StringTheory
    CODE    
    NotifyParam = NotifyManager.GetNotifyParm(NotifyParameter)
	If Size(NotifyParam) > 0
		TempSt.SetValue(NotifyParam)
        !MyDct.Trace('')
        !MyDct.Trace('UpdateField - NotifyHandler.HandleNotify')
		MyDct.Trace(TempSt.GetValue())
        !MyDct.Trace('')
		json.Start()
		json.SetTagCase(jF:CaseAsIs)
		json.Load(ParamsGrp,TempSt)
        If ParamsGrp.Action = ''
            Post(DctEvent:FetchRecord)
        End
        Case ParamsGrp.Action[1:Size(ParamsGrp.Action)]
        OF 'InsertComment'
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateField','SaveNeeded',OverStr)
            MyDct.InsertCommentQRec(MyDct.CommentsQ,Fields,'CommentsBlob',ParamsGrp.CommentJson)
        OF 'EditComment'
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateField','SaveNeeded',OverStr)
            MyDct.UpdateCommentQRec(MyDct.CommentsQ,Fields,'CommentsBlob',ParamsGrp.CommentJson)
        Of 'InsertOption'
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateField','SaveNeeded',OverStr)
            MyDct.InsertOptionQRec(MyDct.OptionsQ,Fields,'OptionsBlob',ParamsGrp.OptionJson)
        Of 'EditOption'
            MyDct.SendNotifyJson(NotifyManager,'Main3','UpdateField','SaveNeeded',OverStr)
            MyDct.UpdateOptionQRec(MyDct.OptionsQ,Fields,'OptionsBlob',ParamsGrp.OptionJson)
        Of 'RecordSaved'
            POST(EVENT:Accepted,?OK)
        END  
        ?OK{PROP:Disable} = False
        Clear(ParamsGrp.Action)
        Display(?CommentsList)
	End
    Post(DctEvent:RefreshData)
DonutHoleWindow.Repaint               PROCEDURE()!,DERIVED
  CODE
  PARENT.Repaint()
