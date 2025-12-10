

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module

                     MAP
                       INCLUDE('DICTIONARYEDITOR027.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
ResetKeysOrder       PROCEDURE  (string pParams,string pThread) ! Declare Procedure
ParamsGrp           Group(ParamsGroupType).
OverStr             String(Size(ParamsGrp)),Over(ParamsGrp)
MyDct               dwrDctParser
x                   Long

KeysView            View(Keys)
                    End
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----

  CODE
? DEBUGHOOK(Keys:Record)
    OverStr = pParams

    Open(KeysView)
    KeysView{PROP:Order} = 'Key:ParentGUID,Key:KeyOrder'
    KeysView{PROP:Filter} = '(Key:ParentGUID = ' & '''' & ParamsGrp.TableGuid & '''' & ')'
    Set(KeysView)
    LOOP
        Next(KeysView)
        If Errorcode(); Break END
        x+=1
        Key:KeyOrder = x !ABS(Fld:FieldOrder)
        Key:Order = Key:KeyOrder
        MyDct.Trace(Key:KeyName & ' ' & Key:KeyOrder)
        Access:Keys.Update()
    End
    Close(KeysView)
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
