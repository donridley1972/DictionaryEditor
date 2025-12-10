

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE
   INCLUDE('StAMRsz.inc'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR007.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
Main PROCEDURE 

ParamsGrp           Group(ParamsGroupType).
OverStr             String(Size(ParamsGrp)),Over(ParamsGrp)
!MyDct               dwrDctParser
json                JSONClass
st                  StringTheory
!xml                 xFilesTree
NotifyParam          STRING(1000)                          ! 
DctFile              STRING(255)                           ! 
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
DockingPane2                  CLASS(DockingPaneClass)
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
Action                          PROCEDURE(LONG paramAction, STRING paramPaneID, <BYTE pNoCancel>),BYTE,DERIVED
AllChildWindowsOpened           PROCEDURE(),DERIVED
ChildWindowClosed               PROCEDURE(STRING paramPaneID, <LONG paramLevel>, <STRING paramProcName>, <SIGNED paramWndHndl>),DERIVED
ChildWindowLevelChanged         PROCEDURE(STRING paramPaneID, LONG paramLevel),DERIVED
ChildWindowOpened               PROCEDURE(STRING paramPaneID, <LONG paramLevel>, <STRING paramProcName>, <SIGNED paramWndHndl>),DERIVED
CreatePanes                     PROCEDURE(),DERIVED
DetachChildWindows              PROCEDURE(),DERIVED
FileFetchRecord                 PROCEDURE(), BYTE,DERIVED
FileInsertRecord                PROCEDURE(), BYTE,DERIVED
FileLockRecord                  PROCEDURE(),BYTE,PROC,DERIVED
FilePrimeRecord                 PROCEDURE(), BYTE,DERIVED
FileUnlockRecord                PROCEDURE(),BYTE,PROC,DERIVED
FileUpdateRecord                PROCEDURE(), BYTE,DERIVED
MenuButtonAccepted              PROCEDURE(STRING paramPaneID),DERIVED
MenuOptionAccepted              PROCEDURE(STRING paramPaneID, USHORT paramSelection),DERIVED
PaneAccepted                    PROCEDURE(STRING paramID, BYTE paramBringToTop),DERIVED
RestoreLayout                   PROCEDURE(*STRING pLayout),BYTE,DERIVED
RestoreLayoutViaBuiltIn         PROCEDURE(),BYTE,PROC,DERIVED
StartChildWindows               PROCEDURE(<STRING paramPaneID>),DERIVED
SaveLayout                      PROCEDURE(*STRING pLayout),DERIVED
SaveLayoutViaBuiltIn            PROCEDURE(),BYTE,PROC,DERIVED
                              END
DockingPane_Ctrl            CSTRING(20)
DockingPane_LayoutContent   STRING(1)
DockingPane_LayoutRestored  BYTE(FALSE)
DockingPane_Result          BYTE
DockingPane_ThreadNo        BYTE
DockingPane_HiddenGroup     SIGNED
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
Window               WINDOW('Dictionary Editor'),AT(,,577,344),FONT('Segoe UI',10),RESIZE,AUTO,ICON('Dictionary.ico'), |
  GRAY,MAX,SYSTEM,WALLPAPER('Background1.png'),IMM
                       OLE,AT(2,3,573,336),USE(?DockingPane),HIDE
                       END
                     END

FieldsView           View(Fields)
                    Project(Fld:PKGuid)
                    Project(Fld:ParentGUID)
                    Project(Fld:TableGuid)
                    Project(Fld:FieldOrder)
                    Project(Fld:LastInGroup)
                    Project(Fld:Guid)
                    Project(Fld:Ident)
                    Project(Fld:FieldName)
                    Project(Fld:DerivedFrom)
                    Project(Fld:Description)
                    Project(Fld:BaseType)
                    Project(Fld:DataType)
                    Project(Fld:Reference)
                    Project(Fld:Thread)
                    Project(Fld:Binary)
                    Project(Fld:FieldSize)
                    Project(Fld:Places)
                    Project(Fld:ScreenPicture)
                    Project(Fld:ScreenPrompt)
                    Project(Fld:ReportHeading)
                    Project(Fld:InitialValue)
                    Project(Fld:Case_)
                    Project(Fld:Over)
                    Project(Fld:Justification)
                    Project(Fld:ReadOnly)
                    Project(Fld:Offset)
                    Project(Fld:ExternalName)
                    Project(Fld:NoPopulate)
                    Project(Fld:VerticalSpace)
                    Project(Fld:HelpId)
                    Project(Fld:Message)
                    Project(Fld:Tooltip)
                    Project(Fld:WindowControl)
                    Project(Fld:ReportControl)
                    Project(Fld:WindowControlBlob)
                    Project(Fld:AuditBlob)
                    Project(Fld:ValidityBlob)
                    Project(Fld:CommentsBlob)
                    Project(Fld:OptionsBlob)
                    End
KeysView           View(Keys)
                    Project(Key:PKGuid)
                    Project(Key:DictionaryGuid)
                    Project(Key:ParentGUID)
                    Project(Key:TableGuid)
                    Project(Key:KeyGuid)
                    Project(Key:Ident)
                    Project(Key:Order)
                    Project(Key:KeyName)
                    Project(Key:ExternalName)
                    Project(Key:Description)
                    Project(Key:KeyType)
                    Project(Key:Unique)
                    Project(Key:AutoNumber)
                    Project(Key:Primary)
                    Project(Key:Exclude)
                    Project(Key:CaseSensitive)
                    Project(Key:AuditBlob)
                    Project(Key:CommentsBlob)
                    Project(Key:OptionsBlob)
                    Project(Key:ComponentsBlob)
                    End
TriggersView           View(Triggers)
                    Project(Tri:PKGuid)
                    Project(Tri:ParentGUID)
                    Project(Tri:TableGuid)
                    Project(Tri:Guid)
                    Project(Tri:TriggerType)
                    Project(Tri:Audit)
                    Project(Tri:createuser)
                    Project(Tri:createdate)
                    Project(Tri:createtime)
                    Project(Tri:createversionnumber)
                    Project(Tri:modifieduser)
                    Project(Tri:modifieddate)
                    Project(Tri:modifiedtime)
                    Project(Tri:modifiedversionnumber)
                    Project(Tri:CommentsBlob)
                    Project(Tri:Code_)
                    End
RelationsView           View(Relations)
                    Project(Rel:PKGuid)
                    Project(Rel:ParentGUID)
                    Project(Rel:DctxOrder)
                    Project(Rel:Guid)
                    Project(Rel:PrimaryTable)
                    Project(Rel:ForeignTable)
                    Project(Rel:PrimaryKey)
                    Project(Rel:ForeignKey)
                    Project(Rel:Delete)
                    Project(Rel:Update)
					Project(Rel:AuditBlob)
					Project(Rel:MappingsBlob)
                    !Project(Rel:ForeignMapping)
                    !Project(Rel:FMGuid)
                    !Project(Rel:FMField)
                    !Project(Rel:PrimaryMapping)
                    !Project(Rel:PMGuid)
                    !Project(Rel:PMField)
                    End
    omit('***',WE::CantCloseNowSetHereDone=1)  !Getting Nested omit compile error, then uncheck the "Check for duplicate CantCloseNowSetHere variable declaration" in the WinEvent local template
WE::CantCloseNowSetHereDone equate(1)
WE::CantCloseNowSetHere     long
    !***
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
mhResize             CLASS(MH::ResizeWindowClass)
Init                   PROCEDURE()
InitialResize          PROCEDURE()                         ! New method added to this class instance
                     END

Main:mhResize:WM     CLASS,IMPLEMENTS(MH::ResizeIWindowManager)
                     END
! ----- xml --------------------------------------------------------------------------
xml                  Class(xFilesTree)
    ! derived method declarations
ValidateRecord         PROCEDURE (),Long,Proc,Virtual
AfterReflectionParse   PROCEDURE (String pGroupName,*View pView),Virtual
_AddViewNode           PROCEDURE (View pView, *Cstring pGroupName, Long pTableId),Virtual
_AddViewRecordNested   PROCEDURE (View pView, *Cstring pGroupName),Virtual
AddChild               PROCEDURE (Long pComponentType, String pStr, Long pIndex=0),Long,Proc,Virtual
AddNode                PROCEDURE (String pTag,String pValue, Long pIndex, Long pForce=false),*XmlTree,Virtual
AddNode                PROCEDURE (String pTag,Long pIndex=0),*XmlTree,Virtual
                     End  ! xml
! ----- end xml -----------------------------------------------------------------------
! ----- MyDct --------------------------------------------------------------------------
MyDct                Class(dwrDctParser)
                     End  ! MyDct
! ----- end MyDct -----------------------------------------------------------------------

  CODE
? DEBUGHOOK(Aliases:Record)
? DEBUGHOOK(DataTypesLkUp:Record)
? DEBUGHOOK(DctComments:Record)
? DEBUGHOOK(DctVersions:Record)
? DEBUGHOOK(DriversLkUp:Record)
? DEBUGHOOK(Fields:Record)
? DEBUGHOOK(Keys:Record)
? DEBUGHOOK(KitchenSink:Record)
? DEBUGHOOK(Options:Record)
? DEBUGHOOK(Relations:Record)
? DEBUGHOOK(TableTypes:Record)
? DEBUGHOOK(Tables:Record)
? DEBUGHOOK(Triggers:Record)
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  DockingPane2.InitHelper(TemplateHelper)
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  !ParamsGrp.ParentThread = Thread()
  GlobalErrors.SetProcedureName('Main')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = 1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  MyDct.UpdateFilesQueue('Dictionaries',Dictionaries)
  MyDct.UpdateFilesQueue('Tables',Tables)
  MyDct.UpdateFilesQueue('DctVersions',DctVersions)
  MyDct.UpdateFilesQueue('Fields',Fields)
  MyDct.UpdateFilesQueue('Keys',Keys)
  MyDct.UpdateFilesQueue('Relations',Relations)
  MyDct.UpdateFilesQueue('Triggers',Triggers)
  MyDct.UpdateFilesQueue('Aliases',Aliases)
  MyDct.UpdateFilesQueue('DuplicateKeys',DuplicateKeys)
  MyDct.UpdateFilesQueue('DctComments',DctComments)
  MyDct.UpdateFilesQueue('Options',Options)
  Relate:Aliases.SetOpenRelated()
  Relate:Aliases.Open()                                    ! File Aliases used by this procedure, so make sure it's RelationManager is open
  Relate:DataTypesLkUp.Open()                              ! File DataTypesLkUp used by this procedure, so make sure it's RelationManager is open
  Relate:DriversLkUp.Open()                                ! File DriversLkUp used by this procedure, so make sure it's RelationManager is open
  Relate:KitchenSink.Open()                                ! File KitchenSink used by this procedure, so make sure it's RelationManager is open
  Relate:TableTypes.Open()                                 ! File TableTypes used by this procedure, so make sure it's RelationManager is open
  Access:Options.UseFile()                                 ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:DctComments.UseFile()                             ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:DctVersions.UseFile()                             ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Triggers.UseFile()                                ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Relations.UseFile()                               ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Keys.UseFile()                                    ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Fields.UseFile()                                  ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Tables.UseFile()                                  ! File referenced in 'Other Files' so need to inform it's FileManager
  SELF.FilesOpened = True
  
  
  SELF.Open(Window)                                        ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('Main', THREAD(), Window)
  !---- Noyantis : Template Helper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.InitAll(TemplateHelper, nysInit_PrepareNoOCXCtrl)
  NYS:DockingPane_EventMgr.MakeTarget       = UPPER('DictionaryEditor.EXE')
  NYS:DockingPane_EventMgr.DisableAtRuntime = FALSE
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  0{PROP:Buffer} = 1   
  
    If Records(TableTypes) = 0
        TblTypes:PKGuid = Glo:st.MakeGuid()
        TblTypes:Description = 'Pool'
        TblTypes:TypeOrder = 1
        Access:TableTypes.Insert()
        TblTypes:PKGuid = Glo:st.MakeGuid()
        TblTypes:Description = 'Global'
        TblTypes:TypeOrder = 2
        Access:TableTypes.Insert()
        TblTypes:PKGuid = Glo:st.MakeGuid()
        TblTypes:Description = 'Table'
        TblTypes:TypeOrder = 3
        Access:TableTypes.Insert()
    End
  
    If Records(DriversLkUp) = 0
        Dri:GUID = gLO:ST.MakeGuid()
        Dri:Driver = 'ADO'
        Access:DriversLkUp.Insert()
        Dri:GUID = gLO:ST.MakeGuid()
        Dri:Driver = 'ASCII'
        Access:DriversLkUp.Insert()
        Dri:GUID = gLO:ST.MakeGuid()
        Dri:Driver = 'BASIC'
        Access:DriversLkUp.Insert()
        Dri:GUID = gLO:ST.MakeGuid()
        Dri:Driver = 'Btrieve'
        Access:DriversLkUp.Insert()
        Dri:GUID = gLO:ST.MakeGuid()
        Dri:Driver = 'Clarion'
        Access:DriversLkUp.Insert()
        Dri:GUID = gLO:ST.MakeGuid()
        Dri:Driver = 'Clipper'
        Access:DriversLkUp.Insert()
        Dri:GUID = gLO:ST.MakeGuid()
        Dri:Driver = 'dBase3'
        Access:DriversLkUp.Insert()
        Dri:GUID = gLO:ST.MakeGuid()
        Dri:Driver = 'dBase4'
        Access:DriversLkUp.Insert()
        Dri:GUID = gLO:ST.MakeGuid()
        Dri:Driver = 'DOS'
        Access:DriversLkUp.Insert()
        Dri:GUID = gLO:ST.MakeGuid()
        Dri:Driver = 'FoxPro'
        Access:DriversLkUp.Insert()
        Dri:GUID = gLO:ST.MakeGuid()
        Dri:Driver = 'MEMORY'
        Access:DriversLkUp.Insert()
        Dri:GUID = gLO:ST.MakeGuid()
        Dri:Driver = 'MSSQL'
        Access:DriversLkUp.Insert()
        Dri:GUID = gLO:ST.MakeGuid()
        Dri:Driver = 'ODBC'
        Access:DriversLkUp.Insert()
        Dri:GUID = gLO:ST.MakeGuid()
        Dri:Driver = 'Oracle'
        Access:DriversLkUp.Insert()
        Dri:GUID = gLO:ST.MakeGuid()
        Dri:Driver = 'Scalable'
        Access:DriversLkUp.Insert()
        Dri:GUID = gLO:ST.MakeGuid()
        Dri:Driver = 'SQLAnywhere'
        Access:DriversLkUp.Insert()
        Dri:GUID = gLO:ST.MakeGuid()
        Dri:Driver = 'SQLite'
        Access:DriversLkUp.Insert()
        Dri:GUID = gLO:ST.MakeGuid()
        Dri:Driver = 'TOPSPEED'
        Access:DriversLkUp.Insert()
    End
  
    If Records(DataTypesLkUp) = 0
        Dat:GUID = Glo:st.MakeGuid()
        Dat:Type = 'BLOB'
        Access:DataTypesLkUp.Insert()
        Dat:GUID = Glo:st.MakeGuid()
        Dat:Type = 'BYTE'
        Access:DataTypesLkUp.Insert()
        Dat:GUID = Glo:st.MakeGuid()
        Dat:Type = 'CSTRING'
        Access:DataTypesLkUp.Insert()
        Dat:GUID = Glo:st.MakeGuid()
        Dat:Type = 'DATE'
        Access:DataTypesLkUp.Insert()
        Dat:GUID = Glo:st.MakeGuid()
        Dat:Type = 'DECIMAL'
        Access:DataTypesLkUp.Insert()
        Dat:GUID = Glo:st.MakeGuid()
        Dat:Type = 'GROUP'
        Access:DataTypesLkUp.Insert()
        Dat:GUID = Glo:st.MakeGuid()
        Dat:Type = 'LIKE'
        Access:DataTypesLkUp.Insert()
        Dat:GUID = Glo:st.MakeGuid()
        Dat:Type = 'LONG'
        Access:DataTypesLkUp.Insert()
        Dat:GUID = Glo:st.MakeGuid()
        Dat:Type = 'MEMO'
        Access:DataTypesLkUp.Insert()
        Dat:GUID = Glo:st.MakeGuid()
        Dat:Type = 'PICTURE'
        Access:DataTypesLkUp.Insert()
        Dat:GUID = Glo:st.MakeGuid()
        Dat:Type = 'PSTRING'
        Access:DataTypesLkUp.Insert()
        Dat:GUID = Glo:st.MakeGuid()
        Dat:Type = 'REAL'
        Access:DataTypesLkUp.Insert()
        Dat:GUID = Glo:st.MakeGuid()
        Dat:Type = 'SHORT'
        Access:DataTypesLkUp.Insert()
        Dat:GUID = Glo:st.MakeGuid()
        Dat:Type = 'SREAL'
        Access:DataTypesLkUp.Insert()
        Dat:GUID = Glo:st.MakeGuid()
        Dat:Type = 'STRING'
        Access:DataTypesLkUp.Insert()
        Dat:GUID = Glo:st.MakeGuid()
        Dat:Type = 'TIME'
        Access:DataTypesLkUp.Insert()
        Dat:GUID = Glo:st.MakeGuid()
        Dat:Type = 'ULONG'
        Access:DataTypesLkUp.Insert()
        Dat:GUID = Glo:st.MakeGuid()
        Dat:Type = 'USHORT'
        Access:DataTypesLkUp.Insert()
    End
  
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  DockingPane2.InitPrepare(?DockingPane{PROP:FEQ})
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  Do DefineListboxStyle
  Alert(AltKeyPressed)  ! WinEvent : These keys cause a program to crash on Windows 7 and Windows 10.
  Alert(F10Key)         !
  Alert(CtrlF10)        !
  Alert(ShiftF10)       !
  Alert(CtrlShiftF10)   !
  Alert(AltSpace)       !
  WinAlertMouseZoom()
  WinAlert(WE::WM_QueryEndSession,,Return1+PostUser)
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  DockingPane2.Init()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  mhResize.Init
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  DockingPane2.InitResize()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  INIMgr.Fetch('Main',Window)                              ! Restore window settings from non-volatile store
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  DockingPane2.InitTemplateSettings()
  DockingPane2.InitComplete()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  IF DockingPane2.KillProcessStarted = FALSE
    CASE DockingPane2.SaveRestoreLayout.Mode
    OF nysSaveRestore_Manual
      DockingPane2.SaveLayout(DockingPane_LayoutContent)
  
    OF nysSaveRestore_Builtin
      DockingPane2.SaveLayoutViaBuiltIn()
  
    END
  
    DockingPane2.Kill()
    DockingPane2.KillComplete()
  END
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('Main', THREAD())
  !---- Noyantis : Template Helper - End ----
  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Aliases.Close()
    Relate:DataTypesLkUp.Close()
    Relate:DriversLkUp.Close()
    Relate:KitchenSink.Close()
    Relate:TableTypes.Close()
  END
  mhResize.Kill
  IF SELF.Opened
    INIMgr.Update('Main',Window)                           ! Save window data to non-volatile store
  END
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.Kill()
  NYS:DockingPane_EventMgr.KillComplete()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


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
  ReturnValue = PARENT.TakeEvent()
    !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
    DockingPane2.TakeEvent(EVENT())
    !---- Noyantis : Codejock Docking Pane Wrapper - End ----
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
    OF EVENT:CloseWindow
      !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
      DockingPane2.ShuttingDown = nysShutdown_CloseWindow
      IF DockingPane2.CheckCloseWindow() <> TRUE THEN CYCLE.
      !---- Noyantis : Codejock Docking Pane Wrapper - End ----
    END
    !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
    DockingPane2.TakeWindowEvent(Window)
    !---- Noyantis : Codejock Docking Pane Wrapper - End ----
    !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
    NYS:DockingPane_EventMgr.TakeWindowEvent(Window)
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

!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
!---- Noyantis : Codejock Docking Pane Wrapper - End ----

mhResize.Init PROCEDURE

  CODE
  SELF.Init(                                               |
      mhResizeFrame,                                       | !Global Host Object
      Window,                                              | !Window
      Main:mhResize:WM.MH::ResizeIWindowManager,           | !Window Manager
      577,                                                 | !Original Width
      344,                                                 | !Original Height
      1,                                                   | !Resize Width?
      1,                                                   | !Resize Height?
      1,                                                   | !Orig Size is Min?
      0,                                                   | !Border Beyond Min?
      0,                                                   | !Maximize Initially?
      1)                                                     !Do NOT hide+unhide during opening operations?
  SELF.AddControl(?DockingPane,0,0,1,1)
  RETURN
  PARENT.Init

mhResize.InitialResize PROCEDURE()

MH::RestoreFromIni   BYTE,AUTO

  CODE
  IF NOT SELF.IsInitialResizeDone
    MH::RestoreFromIni = CHOOSE(INIMgr.TryFetch('Main','XPos') OR INIMgr.TryFetch('Main','Maximize'))
    SELF.InitialResize(                                    |
        MH::RestoreFromIni,                                |  !Restore from INI file?
        0,                                                 |  !Maximize Initially?
        mhHorzPos:Full,                                    |  !Init Horz Pos
        mhVertPos:Full,                                    |  !Init Vert Pos
        0,                                                 |  !Horizontal Shift
        0                                                  )  !Vertical Shift
    IF SELF.IsInitialResizeDone
    END
  END


Main:mhResize:WM.MH::ResizeIWindowManager.Reset PROCEDURE(<BYTE Force>)
  CODE
  ThisWindow.Reset(Force)

!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
DockingPane2.Drop                        PROCEDURE(STRING paramDragID, STRING paramDropID)
  CODE
  PARENT.Drop(paramDragID, paramDropID)
  RETURN

DockingPane2.Event                       PROCEDURE(STRING paramEventName, <*SHORT paramReference>, <SIGNED paramOleControl>, <LONG paramCurrentEvent>)
  CODE
  PARENT.Event(paramEventName, paramReference, paramOleControl, paramCurrentEvent)
  RETURN

DockingPane2.EventFunc                   PROCEDURE(*SHORT Reference, SIGNED OleControl, LONG CurrentEvent)
  CODE
  PARENT.EventFunc(Reference, OleControl, CurrentEvent)
  RETURN

DockingPane2.EventFuncCommon             PROCEDURE(*SHORT Reference, SIGNED OleControl, LONG CurrentEvent)
  CODE
  PARENT.EventFuncCommon(Reference, OleControl, CurrentEvent)
  RETURN

DockingPane2.Init                        PROCEDURE()
  CODE
  SELF.MakeTarget = UPPER('DictionaryEditor.EXE')
  PARENT.Init()
  RETURN

DockingPane2.InitComplete                PROCEDURE()
  CODE
  PARENT.InitComplete()
  RETURN

DockingPane2.InitPrepare                 PROCEDURE(SIGNED paramOCXCtrl)
  CODE
  !--------------------------------------------------
  !noyantis : Codejock Docking Pane Wrapper - Licence
  !Registered User:      donaldridley2011@gmail.com
  !Registered Company:   
  !Registered Serial No: 25109-984-6728212-51400
  !--------------------------------------------------
  SELF.SaveRestoreLayout.Mode          = TemplateHelper.SaveRestore.Mode
  SELF.SaveRestoreLayout.UserID        = TemplateHelper.SaveRestore.UserID
  SELF.SaveRestoreLayout.ProcedureName = 'Main'
  SELF.SaveRestoreLayout.CtrlInstance  = 2
  SELF.DisableAtRuntime   = FALSE
  SELF.WndMgr            &= DockingPaneWndMgr
  SELF.ParentGroup        = CREATE(0, CREATE:GROUP, ?DockingPane{PROP:PARENT})
  
  PARENT.InitPrepare(paramOCXCtrl)
  RETURN

DockingPane2.InitResize                  PROCEDURE()
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

DockingPane2.InitSecurity                PROCEDURE()
  CODE
  PARENT.InitSecurity()
  RETURN

DockingPane2.InitTemplateSettings        PROCEDURE()
  CODE
  PARENT.InitTemplateSettings()
  SELF.GVisualsLink.LinkActive = TemplateHelper.Settings_GSeries.GVisuals.Link.AutoLink
  SELF.DefaultPaneID           = ''
  SELF.ProcedureName           = 'Main'
  SELF.SaveRestore.AddNewPanes = TemplateHelper.SaveRestore.NewContent
  SELF.SaveRestore.RemovePanes = TemplateHelper.SaveRestore.RemovedContent
  
  SELF.ClearTrapableEvents()
  SELF.ClearTrapableKeystrokes()
  SELF.CreatePanes()
  SELF.ApplyThemeToFloating(0)
  SELF.EnableKeyboardNavigate(xtpPaneKeyboardUnused)
  SELF.HideClientArea(1)
  SELF.LockSplitters(0)
  SELF.SetAvailableShade(1)
  SELF.SetAvailableArrows(1)
  SELF.SetAutoHideDuration(128)
  SELF.SetAutoHideInterval(16)
  SELF.SetAutoHideInactivity(100)
  SELF.SetAutoHideMouseHover(-1)
  SELF.SetBorderMargin(0)
  SELF.SetFloatOpacity(255)
  SELF.SetPropertyValue('Options.ShowCaptionMaximizeButton',   0)
  SELF.SetPropertyValue('Options.ShowFloatingFramePinButton',  0)
  SELF.SetPropertyValue('Options.ShowSizeCursorWhileDragging', 0)
  SELF.SetPropertyValue('Options.StickerStyle',                1)
  SELF.SetPaintManagerProperty('CaptionButtonStyle',           xtpPaneCaptionButtonDefault)
  SELF.SetPaintManagerProperty('CaptionMargin',                5)
  SELF.SetPaintManagerProperty('DrawCaptionIcon',              0)
  SELF.SetPaintManagerProperty('DrawSingleTab',                0)
  SELF.SetPaintManagerProperty('HighlightActiveCaption',       1)
  SELF.SetPaintManagerProperty('ShowCaption',                  1)
  SELF.SetPaintManagerProperty('SplitterSize',                 4)
  SELF.SetTabPaintManagerProperty('Appearance',                xtpTabAppearanceDefault)
  SELF.SetTabPaintManagerProperty('Position',                  xtpTabPositionBottom)
  SELF.SetTabPaintManagerProperty('Layout',                    xtpTabLayoutAutoSize)
  SELF.SetTabPaintManagerProperty('HotTracking',               1)
  SELF.SetTabPaintManagerProperty('ShowTabs',                  1)
  SELF.SetTabPaintManagerProperty('ShowIcons',                 1)
  SELF.SetTabPaintManagerProperty('BoldSelected',              0)
  SELF.SetTabPaintManagerProperty('ClearTypeTextQuality',      0)
  SELF.ShowContentsDragging(0)
  SELF.UseSplitterTracker(1)
  RETURN

DockingPane2.Keystroke                   PROCEDURE(UNSIGNED paramKeycode)
  CODE
  PARENT.Keystroke(paramKeycode)
  RETURN

DockingPane2.Kill                        PROCEDURE()
  CODE
  PARENT.Kill()
  RETURN

DockingPane2.KillComplete                PROCEDURE()
  CODE
  PARENT.KillComplete()
  RETURN

DockingPane2.ParametersReceived          PROCEDURE(<LONG paramSessionID>)
  CODE
  PARENT.ParametersReceived(paramSessionID)
  RETURN

DockingPane2.ProcessClones               PROCEDURE()
  CODE
  PARENT.ProcessClones()
  RETURN

DockingPane2.ProcessMimics               PROCEDURE()
  CODE
  PARENT.ProcessMimics()
  RETURN

DockingPane2.ProcessShortcutKey          PROCEDURE(UNSIGNED pKeyCode)
  CODE
  PARENT.ProcessShortcutKey(pKeyCode)
  RETURN

DockingPane2.RefreshContents             PROCEDURE(<BYTE paramForce>)
  CODE
  PARENT.RefreshContents(paramForce)
  RETURN

DockingPane2.SyncOCXHeight               PROCEDURE(<BYTE pForce>)
  CODE
  PARENT.SyncOCXHeight(pForce)
  RETURN

DockingPane2.SyncOCXWidth                PROCEDURE(<BYTE pForce>)
  CODE
  PARENT.SyncOCXWidth(pForce)
  RETURN

DockingPane2.TakeEvent                   PROCEDURE(SIGNED paramEvent)
  CODE
  PARENT.TakeEvent(paramEvent)
  RETURN

DockingPane2.TakeNotify                  PROCEDURE(UNSIGNED paramNotifyCode, SIGNED paramThread, LONG paramParameter)
  CODE
  PARENT.TakeNotify(paramNotifyCode, paramThread, paramParameter)
  RETURN

DockingPane2.TakeSubClassEvent           PROCEDURE(UNSIGNED paramWndHndl, UNSIGNED paramMsg, UNSIGNED paramWParam, LONG paramLParam)
  CODE
  PARENT.TakeSubClassEvent(paramWndHndl, paramMsg, paramWParam, paramLParam)
  RETURN

DockingPane2.TakeTimer                   PROCEDURE()
  CODE
  PARENT.TakeTimer()
  RETURN

DockingPane2.TakeWindowEvent             PROCEDURE(*WINDOW paramWindow)
  CODE
  PARENT.TakeWindowEvent(paramWindow)
  RETURN

DockingPane2.Action                      PROCEDURE(LONG paramAction, STRING paramPaneID, <BYTE pNoCancel>)
pRetVal    ANY
  CODE
  pRetVal = PARENT.Action(paramAction, paramPaneID, pNoCancel)
  RETURN(pRetVal)

DockingPane2.AllChildWindowsOpened       PROCEDURE()
  CODE
  PARENT.AllChildWindowsOpened()
  RETURN

DockingPane2.ChildWindowClosed           PROCEDURE(STRING paramPaneID, <LONG paramLevel>, <STRING paramProcName>, <SIGNED paramWndHndl>)
  CODE
  PARENT.ChildWindowClosed(paramPaneID, paramLevel, paramProcName, paramWndHndl)
  RETURN

DockingPane2.ChildWindowLevelChanged     PROCEDURE(STRING paramPaneID, LONG paramLevel)
  CODE
  PARENT.ChildWindowLevelChanged(paramPaneID, paramLevel)
  RETURN

DockingPane2.ChildWindowOpened           PROCEDURE(STRING paramPaneID, <LONG paramLevel>, <STRING paramProcName>, <SIGNED paramWndHndl>)
  CODE
  PARENT.ChildWindowOpened(paramPaneID, paramLevel, paramProcName, paramWndHndl)
  RETURN

DockingPane2.CreatePanes                 PROCEDURE()
  CODE
  
  CASE SELF.SaveRestoreLayout.Mode
  OF nysSaveRestore_Manual
    DockingPane_LayoutRestored = SELF.RestoreLayout(DockingPane_LayoutContent)
  
  OF nysSaveRestore_Builtin
    DockingPane_LayoutRestored = SELF.RestoreLayoutViaBuiltIn()
  
  END
  PARENT.CreatePanes()
  IF DockingPane_LayoutRestored = FALSE THEN SELF.RestoreDefaultLayout().
  RETURN

DockingPane2.DetachChildWindows          PROCEDURE()
  CODE
  PARENT.DetachChildWindows()
  RETURN

DockingPane2.FileFetchRecord             PROCEDURE()
pRetVal    ANY
  CODE
  pRetVal = PARENT.FileFetchRecord()
  RETURN(pRetVal)

DockingPane2.FileInsertRecord            PROCEDURE()
pRetVal    ANY
  CODE
  pRetVal = PARENT.FileInsertRecord()
  RETURN(pRetVal)

DockingPane2.FileLockRecord              PROCEDURE()
pRetVal    ANY
  CODE
  pRetVal = PARENT.FileLockRecord()
  RETURN(pRetVal)

DockingPane2.FilePrimeRecord             PROCEDURE()
pRetVal    ANY
  CODE
  pRetVal = PARENT.FilePrimeRecord()
  RETURN(pRetVal)

DockingPane2.FileUnlockRecord            PROCEDURE()
pRetVal    ANY
  CODE
  pRetVal = PARENT.FileUnlockRecord()
  RETURN(pRetVal)

DockingPane2.FileUpdateRecord            PROCEDURE()
pRetVal    ANY
  CODE
  pRetVal = PARENT.FileUpdateRecord()
  RETURN(pRetVal)

DockingPane2.MenuButtonAccepted          PROCEDURE(STRING paramPaneID)
paramSelection  USHORT,AUTO
  CODE
  PARENT.MenuButtonAccepted(paramPaneID)
  IF paramSelection NOT = 0 THEN SELF.MenuOptionAccepted(paramPaneID, paramSelection).
  RETURN

DockingPane2.MenuOptionAccepted          PROCEDURE(STRING paramPaneID, USHORT paramSelection)
  CODE
  Message('MenuButtonAccepted')  
  PARENT.MenuOptionAccepted(paramPaneID, paramSelection)
  RETURN

DockingPane2.PaneAccepted                PROCEDURE(STRING paramID, BYTE paramBringToTop)
  CODE
  PARENT.PaneAccepted(paramID, paramBringToTop)
  RETURN

DockingPane2.RestoreLayout               PROCEDURE(*STRING pLayout)
pRetVal    ANY
  CODE
  pRetVal = PARENT.RestoreLayout(pLayout)
  RETURN(pRetVal)

DockingPane2.RestoreLayoutViaBuiltIn     PROCEDURE()
pRetVal    ANY
  CODE
  pRetVal = PARENT.RestoreLayoutViaBuiltIn()
  RETURN(pRetVal)

DockingPane2.StartChildWindows           PROCEDURE(<STRING paramPaneID>)
  CODE
  PARENT.StartChildWindows(paramPaneID)
  CASE CLIP(paramPaneID)
  OF ''
  
  END
  RETURN

DockingPane2.SaveLayout                  PROCEDURE(*STRING pLayout)
  CODE
  PARENT.SaveLayout(pLayout)
  RETURN

DockingPane2.SaveLayoutViaBuiltIn        PROCEDURE()
pRetVal    ANY
  CODE
  pRetVal = PARENT.SaveLayoutViaBuiltIn()
  RETURN(pRetVal)

!---- Noyantis : Codejock Docking Pane Wrapper - End ----
!----------------------------------------------------
xml.ValidateRecord   PROCEDURE ()
ReturnValue   Long
  CODE
    !MyDct.Trace('xml.ValidateRecord')
  ReturnValue = PARENT.ValidateRecord ()
    Return ReturnValue
!----------------------------------------------------
xml.AfterReflectionParse   PROCEDURE (String pGroupName,*View pView)
  CODE
    !MyDct.Trace('xml.AfterReflectionParse - pGroupName = ' & pGroupName)
  PARENT.AfterReflectionParse (pGroupName,pView)
!----------------------------------------------------
xml._AddViewNode   PROCEDURE (View pView, *Cstring pGroupName, Long pTableId)
  CODE
    !MyDct.Trace('xml._AddViewNode - pGroupName = ' & pGroupName & ' pTableId = ' & pTableId)
  PARENT._AddViewNode (pView,pGroupName,pTableId)
!----------------------------------------------------
xml._AddViewRecordNested   PROCEDURE (View pView, *Cstring pGroupName)
  CODE
    !MyDct.Trace('xml._AddViewRecordNested - pGroupName = ' & pGroupName)
  PARENT._AddViewRecordNested (pView,pGroupName)
!----------------------------------------------------
xml.AddChild   PROCEDURE (Long pComponentType, String pStr, Long pIndex=0)
ReturnValue   Long
  CODE
    !MyDct.Trace('xml.AddChild - pStr = ' & pStr)
  ReturnValue = PARENT.AddChild (pComponentType,pStr,pIndex)
    Return ReturnValue
!----------------------------------------------------
xml.AddNode   PROCEDURE (String pTag,String pValue, Long pIndex, Long pForce=false)
ReturnValue   &XmlTree
  CODE
    !MyDct.Trace('xml.AddNode - pTag = ' & pTag)
  ReturnValue &= PARENT.AddNode (pTag,pValue,pIndex,pForce)
    Return ReturnValue
!----------------------------------------------------
xml.AddNode   PROCEDURE (String pTag,Long pIndex=0)
ReturnValue   &XmlTree
  CODE
    !MyDct.Trace('xml.AddNode - pTag = ' & pTag)
  ReturnValue &= PARENT.AddNode (pTag,pIndex)
    Return ReturnValue
