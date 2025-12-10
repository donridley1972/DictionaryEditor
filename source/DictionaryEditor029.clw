

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module

                     MAP
                       INCLUDE('DICTIONARYEDITOR029.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
CheckForDuplicateKeys PROCEDURE  (string pParams,string pThread) ! Declare Procedure
ParamsGrp           Group(ParamsGroupType).
OverStr             String(Size(ParamsGrp)),Over(ParamsGrp)
MyDct               dwrDctParser

DupKeysView         View(DuplicateKeys)
                    End

KeysView            View(Keys)
                    End
NotifyHandler          CLASS(UltimateNotify)
HandleNotify            PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter),DERIVED
                    END
NotifyParam          STRING(1500)                          ! 
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----

  CODE
? DEBUGHOOK(DuplicateKeys:Record)
? DEBUGHOOK(Keys:Record)
? DEBUGHOOK(Tables:Record)
    OverStr = pParams

    MyDct.Trace('')
    MyDct.Trace('CheckForDuplicateKeys')
    MyDct.Trace('   ParamsGrp.DictionaryGuid = ' & ParamsGrp.DictionaryGuid)
    MyDct.Trace('   ParamsGrp.TableGuid = ' & ParamsGrp.TableGuid)
    MyDct.Trace('   ParamsGrp.GlobalRequest = ' & ParamsGrp.GlobalRequest) 
    MyDct.Trace('   ParamsGrp.Usage = ' & ParamsGrp.Usage)
    MyDct.Trace('   ParamsGrp.Order = ' & ParamsGrp.Order)
    MyDct.Trace('')   

    Access:DuplicateKeys.UseFile()
    Access:DuplicateKeys.Open()
    Access:Keys.UseFile()
    Access:Keys.Open()
    Open(DupKeysView)
    DupKeysView{PROP:Order} = ''
    DupKeysView{PROP:Filter} = '(DupK:DictionaryGuid = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ')'
    Set(DupKeysView)
    LOOP
        Next(DupKeysView)
        If Errorcode(); Break END
        MyDct.Trace('Deleting ' & Clip(DupK:KeyName))
        Access:DuplicateKeys.DeleteRecord(False)
    END 
    Close(DupKeysView)
    MyDct.Trace('')

    Open(KeysView)
    KeysView{PROP:Order} = ''
    KeysView{PROP:Filter} = '(Key:DictionaryGuid = ' & '''' & ParamsGrp.DictionaryGuid & '''' & ')'
    Set(KeysView)
    LOOP
        Next(KeysView)
        If Errorcode(); Break END
        !MyDct.Trace('CheckForDuplicateKeys - KeyName =  ' & Clip(Key:KeyName))

        MyDct.KeysQ.KeyName = Clip(Key:KeyName)
        Get(MyDct.KeysQ,MyDct.KeysQ.KeyName)
        If Errorcode()
            Add(MyDct.KeysQ)
        ELSE
            MyDct.Trace('CheckForDuplicateKeys - Duplicate ' & Clip(Key:KeyName) & ' found')
            DupK:PKGuid = Glo:st.MakeGuid()
            DupK:DictionaryGuid = ParamsGrp.DictionaryGuid
            DupK:TableGuid = Key:ParentGUID
            DupK:KeyPKGuid = Key:PKGuid
            DupK:KeyGuid = Key:KeyGuid
            DupK:KeyName = Key:KeyName
            Access:DuplicateKeys.Insert()       
        End

    END
    Close(KeysView)

    Access:DuplicateKeys.Close()
    Access:Keys.Close()

    MyDct.Trace('CheckForDuplicateKeys - Done')

    MyDct.SendNotifyJson(NotifyManager,'DuplicateKeysWindow','CheckForDuplicateKeys','DuplicatesChecked',OverStr)
    Return
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
NotifyHandler.HandleNotify       PROCEDURE(UNSIGNED NotifyCode, SIGNED NotifyThread,LONG NotifyParameter)
us    UltimateString
    CODE    
    NotifyParam = NotifyManager.GetNotifyParm(NotifyParameter)
