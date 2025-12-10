

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module


   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

                     MAP
                       INCLUDE('DICTIONARYEDITOR031.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
CreateSqlFields PROCEDURE (string pParams,string pThread)

TurboSQLTable   	FILE,DRIVER('MSSQL','/TURBOSQL=True/LOGONSCREEN=FALSE'), pre(TurboSQL) 
record            	RECORD
                    END
                    END

TempTable   FILE,DRIVER('ODBC','/ODBCCALL=TRUE/VERIFYVIASELECT = TRUE/TURBOSQL=True')   !,OWNER(GLO:dbOwner) !,PRE(Tem),CREATE,BINDABLE,THREAD     !,'/TURBOSQL=True/LOGONSCREEN=FALSE'), pre(TurboSQL) ,/VERIFYVIASELECT = TRUE  /TURBOSQL=True
record            RECORD
f1                  STRING(255),NAME('Field')
f2                  STRING(255),NAME('Field')
f3                  STRING(255),NAME('Field')
f4                  STRING(255),NAME('Field')
f5                  STRING(255),NAME('Field')
f6                  STRING(255),NAME('Field')
f7                  STRING(255),NAME('Field')
f8                  STRING(255),NAME('Field')
f9                  STRING(255),NAME('Field')
f10                 STRING(255),NAME('Field')
f11                 STRING(255),NAME('Field')
f12                 STRING(255),NAME('Field')
f13                 STRING(255),NAME('Field')
f14                 STRING(255),NAME('Field')
f15                 STRING(255),NAME('Field')
f16                 STRING(255),NAME('Field')
f17                 STRING(255),NAME('Field')
f18                 STRING(255),NAME('Field')
f19                 STRING(255),NAME('Field')
f20                 STRING(255),NAME('Field')
f21                 STRING(255),NAME('Field')
f22                 STRING(255),NAME('Field')
f23                 STRING(255),NAME('Field')
f24                 STRING(255),NAME('Field')
f25                 STRING(255),NAME('Field')
f26                 STRING(255),NAME('Field')
f27                 STRING(255),NAME('Field')
f28                 STRING(255),NAME('Field')
f29                 STRING(255),NAME('Field')
f30                 STRING(255),NAME('Field')
f31                 STRING(255),NAME('Field')
f32                 STRING(255),NAME('Field')
f33                 STRING(255),NAME('Field')
f34                 STRING(255),NAME('Field')
f35                 STRING(255),NAME('Field')
f36                 STRING(255),NAME('Field')
f37                 STRING(255),NAME('Field')
f38                 STRING(255),NAME('Field')
f39                 STRING(255),NAME('Field')
f40                 STRING(255),NAME('Field')
f41                 STRING(255),NAME('Field')
f42                 STRING(255),NAME('Field')
f43                 STRING(255),NAME('Field')
f44                 STRING(255),NAME('Field')
f46                 STRING(255),NAME('Field')
f47                 STRING(255),NAME('Field')
f48                 STRING(255),NAME('Field')
                  END
                END

MyDct               dwrDctParser

ParamsGrp           Group(ParamsGroupType).
OverStr             String(Size(ParamsGrp)),Over(ParamsGrp)

FieldsView          View(Fields)
                    End

st              StringTheory
SqlSt           StringTheory
ValSt           StringTheory
InsertIntoSt    StringTheory
ValuesSt        StringTheory
Rec             &GROUP
RecIn           &GROUP
x               Long
y               Long
CurrentField    ANY
ABC                  STRING(20)                            ! 
SQLDatabase          STRING(50)                            ! 
ImportTablePath      STRING(255)                           ! 
ColumnsQ             QUEUE,PRE(Col)                        ! 
RowNum               LONG                                  ! 
ColName              STRING(40)                            ! 
                     END                                   ! 
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
Window               WINDOW('Create SQL Fields'),AT(,,364,240),FONT('Segoe UI',9),AUTO,GRAY,SYSTEM
                       BUTTON('Close'),AT(318,218),USE(?Close)
                       BUTTON('Button1'),AT(11,213),USE(?BUTTON1)
                       LIST,AT(3,49,346,160),USE(?LIST1),VSCROLL,FORMAT('74L(2)M~Row Num~L(1)@n_14@160L(2)M~Co' & |
  'l Name~@s40@'),FROM(ColumnsQ)
                       PROMPT('Import Table Path:'),AT(2,3),USE(?ImportTablePath:Prompt)
                       ENTRY(@s255),AT(3,17,318,10),USE(ImportTablePath),LEFT(2)
                       BUTTON('...'),AT(334,15,12,12),USE(?LookupFile)
                       PROMPT('SQL Database:'),AT(2,31),USE(?SQLDatabase:Prompt)
                       ENTRY(@s50),AT(49,30,177,10),USE(SQLDatabase),LEFT(2)
                     END

    omit('***',WE::CantCloseNowSetHereDone=1)  !Getting Nested omit compile error, then uncheck the "Check for duplicate CantCloseNowSetHere variable declaration" in the WinEvent local template
WE::CantCloseNowSetHereDone equate(1)
WE::CantCloseNowSetHere     long
    !***
ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
FileLookup5          SelectFileClass

  CODE
? DEBUGHOOK(Fields:Record)
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
    OverStr = pParams  
  GlobalErrors.SetProcedureName('CreateSqlFields')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Close
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  ! Restore preserved local variables from non-volatile store
  ImportTablePath = INIMgr.TryFetch('CreateSqlFields_PreservedVars','ImportTablePath')
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:Fields.Open()                                     ! File Fields used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  
    Open(FieldsView)
    FieldsView{PROP:Order} = ''
    FieldsView{PROP:Filter} = '(Fld:ParentGUID = ' & '''' & ParamsGrp.TableGuid & '''' & ')'
    Set(FieldsView)
  
      
  SELF.Open(Window)                                        ! Open window
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.AddActiveWindow('CreateSqlFields', THREAD(), Window)
  !---- Noyantis : Template Helper - End ----
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.InitAll(TemplateHelper, nysInit_PrepareNoOCXCtrl)
  NYS:DockingPane_EventMgr.MakeTarget       = UPPER('DictionaryEditor.EXE')
  NYS:DockingPane_EventMgr.DisableAtRuntime = FALSE
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
  INIMgr.Fetch('CreateSqlFields',Window)                   ! Restore window settings from non-volatile store
  FileLookup5.Init
  FileLookup5.ClearOnCancel = True
  FileLookup5.Flags=BOR(FileLookup5.Flags,FILE:LongName)   ! Allow long filenames
  FileLookup5.SetMask('All Files','*.tps')                 ! Set the file mask
  !---- Noyantis : Template Helper - Start ----
  IF TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey <> 0 THEN ALERT(TemplateHelper.Settings_GSeries.GVisuals.ReplacementText.HotKey).
  !---- Noyantis : Template Helper - End ----
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  !---- Noyantis : Template Helper - Start ----
  TemplateHelper.DeleteActiveWindow('CreateSqlFields', THREAD())
  !---- Noyantis : Template Helper - End ----
  If self.opened Then WinAlert().
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Fields.Close()
  END
  IF SELF.Opened
    INIMgr.Update('CreateSqlFields',Window)                ! Save window data to non-volatile store
  END
  ! Save preserved local variables in non-volatile store
  INIMgr.Update('CreateSqlFields_PreservedVars','ImportTablePath',ImportTablePath)
  !---- Noyantis : Codejock Docking Pane Wrapper - Start ----
  NYS:DockingPane_EventMgr.Kill()
  NYS:DockingPane_EventMgr.KillComplete()
  !---- Noyantis : Codejock Docking Pane Wrapper - End ----
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE ACCEPTED()
    OF ?BUTTON1
      !ALTER TABLE dbo.doc_exa 
      !ADD column_b VARCHAR(20) NULL, column_c INT NULL ;
      
      MyDct.Trace('')
      MyDct.Trace('FieldsView Filter = ' & FieldsView{PROP:Filter})
      MyDct.Trace('')
      !SqlSt.AddLine(Records(SqlSt)+1,'')
      LOOP
        Next(FieldsView)
        If Errorcode(); Break END
        MyDct.Trace(Fld:DataType)
        Case UPPER(Clip(Fld:DataType))
        Of 'STRING'
            SqlSt.AddLine(Records(SqlSt)+1,Clip(Fld:FieldName) & ' VARCHAR(' & Clip(Fld:FieldSize) & ') NULL')
            ColumnsQ.RowNum = Records(ColumnsQ)+1
            ColumnsQ.ColName = Clip(Fld:FieldName)
            Add(ColumnsQ)
        Of 'CSTRING'
            SqlSt.AddLine(Records(SqlSt)+1,Clip(Fld:FieldName) & ' VARCHAR(' & Clip(Fld:FieldSize) & ') NULL')
            ColumnsQ.RowNum = Records(ColumnsQ)+1
            ColumnsQ.ColName = Clip(Fld:FieldName)
            Add(ColumnsQ)
        Of 'BYTE'
            SqlSt.AddLine(Records(SqlSt)+1,Clip(Fld:FieldName) & ' SMALLINT NULL')
            ColumnsQ.RowNum = Records(ColumnsQ)+1
            ColumnsQ.ColName = Clip(Fld:FieldName)
            Add(ColumnsQ)
        Of 'SHORT'
            SqlSt.AddLine(Records(SqlSt)+1,Clip(Fld:FieldName) & ' INT NULL')
            ColumnsQ.RowNum = Records(ColumnsQ)+1
            ColumnsQ.ColName = Clip(Fld:FieldName)
            Add(ColumnsQ)
        Of 'LONG'
            SqlSt.AddLine(Records(SqlSt)+1,Clip(Fld:FieldName) & ' INT NULL')
            ColumnsQ.RowNum = Records(ColumnsQ)+1
            ColumnsQ.ColName = Clip(Fld:FieldName)
            Add(ColumnsQ)
        Of 'ULONG'
            SqlSt.AddLine(Records(SqlSt)+1,Clip(Fld:FieldName) & ' INT NULL')
            ColumnsQ.RowNum = Records(ColumnsQ)+1
            ColumnsQ.ColName = Clip(Fld:FieldName)
            Add(ColumnsQ)
        Of 'SREAL'
            SqlSt.AddLine(Records(SqlSt)+1,Clip(Fld:FieldName) & ' REAL NULL')
            ColumnsQ.RowNum = Records(ColumnsQ)+1
            ColumnsQ.ColName = Clip(Fld:FieldName)
            Add(ColumnsQ)
        Of 'REAL'
            SqlSt.AddLine(Records(SqlSt)+1,Clip(Fld:FieldName) & ' REAL NULL')
            ColumnsQ.RowNum = Records(ColumnsQ)+1
            ColumnsQ.ColName = Clip(Fld:FieldName)
            Add(ColumnsQ)
        Of 'DECIMAL'
            SqlSt.AddLine(Records(SqlSt)+1,Clip(Fld:FieldName) & ' REAL NULL')
            ColumnsQ.RowNum = Records(ColumnsQ)+1
            ColumnsQ.ColName = Clip(Fld:FieldName)
            Add(ColumnsQ)
        Of 'PDECIMAL'
            SqlSt.AddLine(Records(SqlSt)+1,Clip(Fld:FieldName) & ' REAL NULL')
            ColumnsQ.RowNum = Records(ColumnsQ)+1
            ColumnsQ.ColName = Clip(Fld:FieldName)
            Add(ColumnsQ)
        Of 'DATE'
            SqlSt.AddLine(Records(SqlSt)+1,Clip(Fld:FieldName) & ' DATE NULL')
            ColumnsQ.RowNum = Records(ColumnsQ)+1
            ColumnsQ.ColName = Clip(Fld:FieldName)
            Add(ColumnsQ)
        Of 'TIME'
            SqlSt.AddLine(Records(SqlSt)+1,Clip(Fld:FieldName) & ' TIME NULL')
            ColumnsQ.RowNum = Records(ColumnsQ)+1
            ColumnsQ.ColName = Clip(Fld:FieldName)
            Add(ColumnsQ)
        End
      END
      !Message(SqlSt.FileNameOnly(ImportTablePath,False))
      SqlSt.Join(',<13,10>')
      SqlSt.Prepend('CREATE TABLE dbo.' & SqlSt.FileNameOnly(ImportTablePath,False) & ' (<13,10>')
      !SqlSt.Prepend('CREATE TABLE dbo.TestTable (<13,10>')
      SqlSt.Append('<13,10>);')
      MyDct.Trace('')
      MyDct.Trace(SqlSt.GetValue())
      MyDct.Trace('')
      Close(FieldsView)
      
      TurboSQLTable{PROP:Owner} = 'DESKTOP-250R642\SQLEXPRESS,' & Clip(SQLDatabase) & ',PdmanagerSql,OPD@2022!'
      !TurboSQLTable{PROP:Owner} = 'DESKTOP-250R642\SQLEXPRESS,TestTables,PdmanagerSql,OPD@2022!'
      Open(TurboSQLTable)
      MyDct.Trace('Opening TurboSQLTable')
      MyDct.Trace('Break Error - Error[' & Error() & ']<9>FileErrorCode[' & FileErrorcode() & ']<9>FileError[' & FileError() & ']')
      TurboSQLTable{PROP:SQL} = SqlSt.GetValue()
      MyDct.Trace('Break Error - Error[' & Error() & ']<9>FileErrorCode[' & FileErrorcode() & ']<9>FileError[' & FileError() & ']')
      
      
      
      TempTable{PROP:Owner} = 'DRIVER={{TopSpeed ODBC Driver};DBQ=' & SqlSt.PathOnly(ImportTablePath) & '\;Extension=tps;Oem=N;NullEmptyStr=N;NoDot=N;UlongAsDate=N;'
      
      MyDct.Trace(TempTable{PROP:Owner})
      
      Open(TempTable)
      MyDct.Trace('Opening TempTable')
      MyDct.Trace('Break Error - Error[' & Error() & ']<9>FileErrorCode[' & FileErrorcode() & ']<9>FileError[' & FileError() & ']')
      
      
      !      LOOP
      !        Next(FieldsView)
      !        If Errorcode(); Break END
      !        Rec &= Fields{PROP:Record} 
      !        RecIn &= TurboSQLTable{PROP:Record}
      !        RecIn = Rec 
      !        Add(TurboSQLTable)
      !      End
      
      SqlSt.Start()
      Set(TempTable)
      TempTable{PROP:SQL} = 'select * from ' & SqlSt.FileNameOnly(ImportTablePath,False) !st.GetValue()
      !st.Start()
      Loop
        Next(TempTable)
        If Errorcode()
            Break 
        END
        x+=1
        Rec &= TempTable{PROP:Record}  
        RecIn &= TurboSQLTable{PROP:Record}
        !SqlSt.SerializeGroup(Rec,',')
        !MyDct.Trace(TurboSQLTable{PROP:Fields})
        ValuesSt.Start()
        
        Loop x = 1 to TempTable{PROP:Fields}
            CurrentField &= WHAT(Rec,x)
            ValSt.SetValue(CurrentField)
            MyDct.Trace(ValSt.GetValue())
            Get(ColumnsQ,x)
            If Not Errorcode()
                !st.AppendA(ValSt.GetValue(),True,'')
                If ValSt.ContainsA('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')
                    If Not ValSt.FindChar('<0>')
                        !ValSt.Remove('<32>')
                        ValSt.Trim()
                        ValuesSt.AppendA(ValSt.GetValue(),True,',')
                        !st.AppendA(Clip(ColumnsQ.ColName) & ' = <39>' & ValSt.GetValue() & '<39>',True,',<32>')
                    End
                End
                !st.AddLine(Records(st.lines)+1,Clip(ColumnsQ.ColName) & ' = ' & ValSt.GetValue())
            End
            Clear(CurrentField)
        End
        !st.Join(',')
        !RecIn = st.GetValue()
        !Add(TurboSQLTable)
        !MyDct.Trace(st.GetValue())
        !st.Prepend('SET<32>')
        !st.Prepend('UPDATE TestTable<32>') !& SqlSt.FileNameOnly(ImportTablePath,False) & '<32>')
      
        !TurboSQLTable{PROP:SQL} = st.GetValue()
      
        SqlSt.AddLine(Records(SqlSt.lines)+1,ValuesSt.GetValue())
        !RecIn &= TurboSQLTable{PROP:Record}
        !Get(TurboSQLTable,x)
        !RecIn :=: Rec 
        !ADD(TurboSQLTable)
        !Loop y = 1 to TempTable{PROP:Fields}
        !End
      End
      SqlSt.Join('<13,10>')
      MyDct.Trace('')
      MyDct.Trace(SqlSt.GetValue())
      MyDct.Trace('')
      SqlSt.SaveFile('SqlOut.txt')
      Close(TempTable)  
      TempTable{PROP:Disconnect}
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?LookupFile
      ThisWindow.Update()
      ImportTablePath = FileLookup5.Ask(1)
      DISPLAY
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeEvent()
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
    END
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
