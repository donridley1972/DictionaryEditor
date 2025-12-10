

   MEMBER('DictionaryEditor.clw')                          ! This is a MEMBER module

                     MAP
                       INCLUDE('DICTIONARYEDITOR026.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Source
!!! </summary>
ResetFieldsOrder     PROCEDURE  (string pParams,string pThread) ! Declare Procedure
ParamsGrp           Group(ParamsGroupType).
OverStr             String(Size(ParamsGrp)),Over(ParamsGrp)
MyDct               dwrDctParser
x                   Long
FieldsView          View(Fields)
                    End
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
NYS:DockingPane_EventMgr      CLASS(DockingPaneEventMgrClass)
                              END
!---- Noyantis : Codejock Docking Pane Wrapper - End ----

  CODE
? DEBUGHOOK(Fields:Record)
    OverStr = pParams

    Open(FieldsView)
    FieldsView{PROP:Order} = 'Fld:ParentGUID,Fld:FieldOrder'
    FieldsView{PROP:Filter} = '(Fld:ParentGUID = ' & '''' & ParamsGrp.TableGuid & '''' & ')'
    Set(FieldsView)
    LOOP
        Next(FieldsView)
        If Errorcode(); Break END
        x+=1

        Fld:FieldOrder = x !ABS(Fld:FieldOrder)
        MyDct.Trace(Fld:FieldName & ' ' & Fld:FieldOrder)
        Access:Fields.Update()
    End
    Close(FieldsView)
!---- Noyantis : Codejock Docking Pane Wrapper - Start ----
!---- Noyantis : Codejock Docking Pane Wrapper - End ----
